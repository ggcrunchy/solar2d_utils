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

-- Solar2D globals --
local transition = transition

-- Cached module references --
local _GetState_

-- Exports --
local M = {}

--
--
--

local EventCache = {}

local EventCookie = EventCache -- arbitrary

local function OnComplete ()
	local func, handle

	return function(target, arg1, arg2)
		if rawequal(target, EventCookie) then
			func, handle = arg1, arg2
		else
      handle._transitionHasCompleted = true -- broken in transition module, so spoof expected result

			if func then
				func(target)
			end
		end
	end
end

local function AuxDoAndWait (target, params)
  -- A transition will run until either being cancelled or completed, firing a callback in
  -- either case. Cancels actually set a flag, but an onComplete handler is needed in that
  -- situation due to a slight bug, i.e. _transitionHasCompleted is never set to true.
	local event, func = remove(EventCache) or OnComplete(), params.onComplete

	params.onComplete = event

  -- Fire the transition and flag it as active.
	local handle = transition.to(target, params)

	handle.m_doing_transition = true

  -- Piggyback any user-provided onComplete on the handler.
	event(EventCookie, func, handle)

	-- Undo any change to the params.
	params.onComplete = func

	return handle, event
end

local function Cleanup (event)
	event(EventCookie, nil)

	EventCache[#EventCache + 1] = event
end

local Pollers = {}

local PollerCookie = Pollers -- arbitrary

local function MakePoller ()
  local first, handle, event

	return function(arg1, arg2, arg3)
		if rawequal(arg1, PollerCookie) then
			first, handle, event = true, AuxDoAndWait(arg2, arg3)

      return handle
		else
			assert(handle, "Already done polling")

			if _GetState_(handle) ~= "doing" then
				Cleanup(event)

				handle, event = nil

				return false
			elseif first then
				first = nil
			else
				yield()
			end

			return true
		end
	end
end

--- DOCME
function M.DoAndPoll (target, params)
	local poll = remove(PollerCookie) or MakePoller()

	return poll, poll(Pollers, target, params)
end

--
--
--

local function DoingTransition (handle)
	return _GetState_(handle) == "doing"
end

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
	local handle, event = AuxDoAndWait(target, params)

	-- Wait for the transition to finish, performing any user-provided update.
  coro_flow.SetDoneArg(handle)

	if not coro_flow.WaitWhile(DoingTransition, update, target, arg) then
		transition.cancel(handle)
	end

	Cleanup(event)

	return _GetState_(handle)
end

--
--
--

--- DOCME
function M.GetState (handle)
  if handle._cancelled then -- q.v. transition library source
    return "cancelled"
  elseif handle._transitionHasCompleted then -- cf. OnComplete()
    return "completed"
  else
    return handle.m_doing_transition and "doing" or "none"
  end
end

--
--
--

local ProxyMT = {
	__index = function(proxy, _)
		return proxy.m_t
	end,

	__newindex = function(proxy, _, t)
		proxy.m_t = t

		proxy.m_func(t, proxy.arg)
	end
}

local Keys = {
  "delay", "delta", "iterations",
  "onCancel", "onComplete", "onPause", "onRepeat", "onResume", "onStart",
  "tag", "time", "transition"
}

local ParamsCache = {}

--- DOCME
function M.Proxy (func, options, arg)
	local proxy = setmetatable({ m_func = func, m_t = 0, arg = arg }, ProxyMT)
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