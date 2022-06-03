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
local remove = table.remove
local setmetatable = setmetatable
local type = type

-- Modules --
local coro_flow = require("solar2d_utils.coro_flow")

-- Solar2D globals --
local Runtime = Runtime
local transition = transition

-- Cached module references --
local _IsActive_
local _IsCancelled_
local _IsCompleted_
local _Wait_

-- Exports --
local M = {}

--
--
--

--- Kick off a transition and wait until it finishes.
--
-- This must be called within a coroutine.
-- @param target As per `transition.to`...
-- @ptable params ...ditto.
-- @callable update As per @{Wait}...
-- @param arg ...ditto.
function M.DoAndWait (target, params, update, arg)
	_Wait_(transition.to(target, params), update, arg)
end

--
--
--

---
-- @tparam TransitionHandle handle
-- @treturn boolean The transition has neither completed nor been cancelled?
function M.IsActive (handle)
  return not (_IsCancelled_(handle) or _IsCompleted_(handle))
end

--
--
--

---
-- @tparam TransitionHandle handle
-- @treturn boolean The transition was cancelled?
function M.IsCancelled (handle)
  return handle._cancelled == true -- q.v. transition library source
end

--
--
--

---
-- @tparam TransitionHandle handle
-- @treturn boolean The transition completed?
function M.IsCompleted (handle)
  if handle._paused then
    return false
  else
    local now = Runtime.getFrameStartTime() -- n.b. will be very slightly earlier than currentTime (q.v. transition library source)...

    return now - handle._timeStart >= handle._duration -- ...so this could register a frame late
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

		proxy.m_update(t, proxy.arg)
	end
}

local Keys = {
  "delay", "delta", "iterations",
  "onCancel", "onComplete", "onPause", "onRepeat", "onResume", "onStart",
  "tag", "time", "transition"
}

local ParamsCache = {}

local function CacheProxyParams (params)
	for k in pairs(params) do
    params[k] = nil
	end

  ParamsCache[#ParamsCache + 1] = params
end

local function PrepareProxyParams (options, n)
  local params = remove(ParamsCache) or {}

  for i = 1, n do
    local k = Keys[i]

    params[k] = options[k]
  end

  params.t = 1

  return params
end

--- Launch a transition that proxies a hidden time value, with periodic updates.
-- @tparam ?|ptable|function options This may be a function, called as `update(t, arg)`,
-- where _t_ is the underlying time, &isin; [0, 1]. Otherwise, as a table it resembles
-- the parameters used by `transition.to`, but the function just described must also be
-- provided under the **update** key.
-- @param[opt] arg Argument to the update routine. This may also be obtained in any
-- listener, e.g. `onRepeat`, as `object.arg`, with _object_ being the listener's argument.
-- @treturn TransitionHandle Transition.
function M.Proxy (options, arg)
  local n, update = 0

  if type(options) == "table" then
    update, n = options.update, #Keys
  else
    update = options
  end

  assert(type(update) == "function", "Non-function update")

  local params = PrepareProxyParams(options, n)
	local proxy = setmetatable({ m_t = 0, m_update = update, arg = arg }, ProxyMT)
	local handle = transition.to(proxy, params)

  CacheProxyParams(params)

	return handle
end

--
--
--

--- Wait until a transition has finished.
--
-- This must be called within a coroutine.
-- @tparam TransitionHandle handle
-- @callable update Optional update routine, with _handle_'s target as argument,
-- cf. @{coro_flow.WaitWhile}.
--
-- If the wait is aborted during the update, the transition is cancelled.
-- @param arg Second argument to @{coro_flow.WaitWhile}.
-- @see DoAndWait
function M.Wait (handle, update, arg)
  coro_flow.SetDoneArg(handle)

	if not coro_flow.WaitWhile(_IsActive_, update, handle._target, arg) then  -- q.v. transition library source
		transition.cancel(handle)
	end
end

--
--
--

_IsActive_ = M.IsActive
_IsCancelled_ = M.IsCancelled
_IsCompleted_ = M.IsCompleted
_Wait_ = M.Wait

return M