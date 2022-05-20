--- Some useful transition utilities.

--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--
-- [ MIT license: http://www.opensource.org/licenses/mit-license.php ]
--

-- Standard library imports --
local assert = assert
local pairs = pairs
local rawequal = rawequal
local remove = table.remove
local setmetatable = setmetatable
local yield = coroutine.yield

-- Modules --
local coro_flow = require("solar2d_utils.coro_flow")
local meta = require("tektite_core.table.meta")

-- Solar2D globals --
local transition = transition

-- Cached module references --
local _GetState_

-- Exports --
local M = {}

--
--
--

-- Current state of transition handle, if any --
local HandleState = meta.WeakKeyed()

-- Helper to report to flow operation when a transition has completed
local function DoingTransition (handle)
	return HandleState[handle] == "doing"
end

-- Cache of cancel / complete events --
local EventCache = {}

-- Builds a resettable event to detect cancelled / completed transitions
local function OnEvent ()
	local func, handle, how

	return function(target, arg1, arg2, arg3)
		if rawequal(target, EventCache) then
			func, handle, how = arg1, arg2, arg3
		else
			HandleState[handle] = how

			if func then
				func(target)
			end
		end
	end
end

local function AuxDoAndWait (target, params)
	local event1, func1 = remove(EventCache) or OnEvent(), params.onCancel
	local event2, func2 = remove(EventCache) or OnEvent(), params.onComplete

	-- Install transition-aware handlers into the parameters and launch the transition.
	params.onCancel, params.onComplete = event1, event2

	local handle = transition.to(target, params)

	HandleState[handle] = "doing"

	-- The previous steps may have evicted the user-provided handlers; if so, references to said
	-- handlers are passed along to the overrides, to be called after doing their own work. The
	-- transition handle is also sent, so that it can be flagged once the transition is ended.
	event1(EventCache, func1, handle, "cancelled")
	event2(EventCache, func2, handle, "completed")

	-- Restore the user-provided handlers.
	params.onCancel, params.onComplete = func1, func2

	return handle, event1, event2
end

local function Cleanup (event1, event2)
	-- Remove all transition state and put the events into the cache.
	event1(EventCache, nil)
	event2(EventCache, nil)

	EventCache[#EventCache + 1] = event1
	EventCache[#EventCache + 1] = event2
end

local Pollers = {}

--- DOCME
function M.DoAndPoll (target, params)
	local first, handle, event1, event2
	local poll = remove(Pollers) or function(arg1, arg2, arg3)
		if rawequal(arg1, Pollers) then
			first, handle, event1, event2 = true, AuxDoAndWait(arg2, arg3)
		else
			assert(handle, "Already done polling")

			local state = _GetState_(handle)

			if state == "cancelled" or state == "completed" then
				Cleanup(event1, event2)

				handle, event1, event2 = nil

				return false
			elseif first then
				first = nil
			else
				yield()
			end

			return true
		end
	end

	poll(Pollers, target, params)

	return poll, handle
end

--
--
--

--- Kick off a transition and wait until it finishes.
--
-- This must be called within a coroutine.
-- @param target Object to transition.
-- @ptable params Transition parameters, as per `transition.to`.
-- @callable update Optional update routine, with the transition handle as argument, cf.
-- @{coro_flow.WaitWhile}.
--
-- If the wait is aborted during the update, the transition is cancelled.
function M.DoAndWait (target, params, update, arg)
	local handle, event1, event2 = AuxDoAndWait(target, params)

	-- Wait for the transition to finish, performing any user-provided update.
  coro_flow.SetDoneArg(handle)

	if not coro_flow.WaitWhile(DoingTransition, update, target, arg) then
		transition.cancel(handle)

		-- Yield to accommodate the cancel listener.
		yield()
	end

	Cleanup(event1, event2)

	return HandleState[handle]
end

--
--
--

--- DOCME
function M.GetState (handle)
	return HandleState[handle] or "none"
end

--
--
--

local MT = {
	__index = function(proxy, _)
		return proxy.m_t
	end,

	__newindex = function(proxy, _, t)
		proxy.m_t = t

		proxy.m_func(t, proxy.arg)
	end
}

local Keys = { "delay", "delta", "iterations", "onCancel", "onComplete", "onRepeat", "onStart", "tag", "time", "transition" }

local ParamsCache = {}

--- DOCME
function M.Proxy (func, options, arg)
	local proxy = setmetatable({ m_func = func, m_t = 0, arg = arg }, MT)
  local params = remove(ParamsCache) or {}

	for i = 1, #(options and Keys or "") do
		local k = Keys[i]

		params[k] = options[k]
	end

  params.t = 1

	local handle = transition.to(proxy, params)

	for k in pairs(params) do
    params[k] = nil
	end

  ParamsCache[#ParamsCache + 1] = params

	return handle
end

--
--
--

_GetState_ = M.GetState

return M