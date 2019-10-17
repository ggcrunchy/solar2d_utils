--- Some useful timer utilities.
--
-- A **TimerHandle** refers to the result of a **timer.performWithDelay**.

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
local create = coroutine.create
local error = error
local rawequal = rawequal
local resume = coroutine.resume
local status = coroutine.status
local yield = coroutine.yield

-- Cookies --
local _get_func = {}

-- Corona globals --
local display = display
local timer = timer

-- Cached module references --
local _Wrap_

-- Exports --
local M = {}

--
--
--

--- 
-- @tparam TimerHandle handle
-- @treturn boolean The timer has expired?
function M.HasExpired (handle)
	return handle._expired == true -- q.v. timer library source
end

--- 
-- @tparam TimerHandle handle
-- @treturn boolean The timer is still active, but paused?
function M.IsPaused (handle)
	return not handle._expired and handle._pauseTime ~= nil -- q.v. timer library source
end

--- Spawn a new timer, reusing another timer's listener.
--
-- **N.B.** Since both timers will refer to the same listener, any upvalues and other associated
-- state belonging to said listener are also shared.
-- @uint delay As per **timer.performWithDelay**.
-- @tparam TimerHandle handle Timer to emulate.
-- @int[opt] iterations As per **timer.performWithDelay**. Ignored if _handle_ came from @{Wrap}.
-- @treturn TimerHandle Handle.
function M.PerformWithDelayFromExample (delay, handle, iterations)
	local listener = handle._listener -- q.v. timer library source

	if handle._wrapped then
		return _Wrap_(delay, listener(_get_func))
	else
		return timer.performWithDelay(delay, listener, iterations)
	end
end

local DeferFuncs = {
	activate = function(object)
		object.isBodyActive = true
	end,

	deactivate = function(object)
		object.isBodyActive = false
	end,

	remove = function(object)
		object:removeSelf()
	end,

	stop = function(object)
		object:setLinearVelocity(0, 0)
	end,

	stop_and_deactivate = function(object)
		object.isBodyActive = false

		object:setLinearVelocity(0, 0)
	end
}

--- Perform an action on an object with a 0-frame delay, i.e. soon but not immediately.
--
-- The motivation is that _object_ and / or some associated state might still be in use, for
-- instance until a callback finishes, yet that point in the code is convenient / natural.
-- @callable func Called as `func(object, event)` after the delay, with _event_ as per **timer.performWithDelay**.
--
-- Alternatively, a string corresponding to a built-in behavior:
--
-- * **"activate"**: `object.isBodyActive = true`.
-- * **"deactivate"**: `object.isBodyActive = false`.
-- * **"remove"**: `object:removeSelf()`.
-- * **"stop"**: `object:setLinearVelocity(0, 0)`.
-- * **"stop\_and\_deactivate"**: Combination of **"stop"** and **"deactivate"**.
-- @pobject object Acted-on object. If **removeSelf** has been called on _object_ before
-- the timer goes off, _func_ will never be called.
-- @treturn TimerHandle Handle.
function M.WithObjectDefer (object, func)
	func = DeferFuncs[func] or func

	return timer.performWithDelay(0, function(event)
		if display.isValid(object) then
			func(object, event)
		end
	end)
end

local function DefError (err, _) error(err) end

--- Kick off a coroutine-based timer, allowing _func_ to proceed using @{coroutine.yield}.
--
-- The timer will cancel itself after _func_ completes or an error occurs.
-- @uint delay As per **timer.performWithDelay**.
-- @callable func Timer body, ditto.
-- @callable[opt] err_func On error, called as `err_func(err, coro)`, where _err_ was the error object
-- and _coro_ the offending coroutine. This allows, say, calling `debug.traceback(coro, err)` to gather
-- more debugging information. The default behavior is `error(err)`.
-- @treturn TimerHandle Handle.
function M.Wrap (delay, func, err_func)
	local coro, et = create(func)
	local handle = timer.performWithDelay(delay, function(event)
		if rawequal(event, _get_func) then
			return func -- cf. PerformWithDelayFromExample
		else
			et = et or event -- n.b. relies on leaky abstraction, i.e. new event per timer per frame
			et.count, et.time = event.count, event.time

			local ok, err = resume(coro, event)

			if status(coro) == "dead" then -- errored out or complete?
				timer.cancel(event.source)

				if not ok then -- errored out
					(err_func or DefError)(err, coro)
				end
			end
		end
	end, 0)

	handle._wrapped = true

	return handle
end

--- DOCME
function M.YieldEach (n)
	local count = n

	return function()
		count = count - 1

		if count == 0 then
			count = n

			yield()
		end
	end
end

--- DOCME
function M.YieldOnTimeout (timeout)
	local since

	return function(event, how)
		local now = event.time

		if how == "begin" or not since then
			since = now
		elseif now - since > timeout then
			since = now

			yield()
		end
	end
end

_Wrap_ = M.Wrap

return M