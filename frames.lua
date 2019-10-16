--- This module provides some per-frame logic.

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
local abs = math.abs

-- Corona globals --
local Runtime = Runtime
local system = system

-- Cached module references --
local _GetFrameID_
local _InvalidateID_

-- Exports --
local M = {}

--
--
--

-- Unique frame ID (lazily evaluated) --
local FrameID = 0

-- Frame difference; last frame time --
local Diff, Last = 0, 0

--- Getter.
-- @treturn number Difference in time since last frame.
function M.DiffTime ()
	return Diff
end

--- Getter.
-- @treturn uint Current frame ID.
function M.GetFrameID ()
	if FrameID <= 0 then
		FrameID = 1 - FrameID
	end

	return FrameID
end

--- Getter.
-- @treturn number Time for this frame.
function M.GetFrameTime ()
	return Last
end

-- Dispatcher used to drive "enterFrame" listeners once interception is up and running --
local Events

--- Any future **"enterFrame"** listeners are added to / removed from an internal dispatcher.
-- Each frame the runtime's own event is propagated through this dispatcher, followed by
-- an @{InvalidateID}. (The purpose of intercepting events is to ensure this invalidation.)
--
-- Note that this modifies the **Runtime:addEventListener()** and **Runtime:removeEventListener()**
-- methods in order to reroute **"enterFrame"** messages. If the raw versions are needed, they
-- must be cached beforehand. Typically, this should be called before adding listeners to
-- avoid this need.
--
-- Subsequent calls are no-ops.
function M.InterceptEnterFrameEvents ()
	if not Events then
		-- Add one final "enterFrame" listener to process subsequently added events.
		Events = system.newEventDispatcher()

		Runtime:addEventListener("enterFrame", function(event)
			-- Run "enterFrame" events.
			Events:dispatchEvent(event)

			-- Do post-enterFrame logic.
			_InvalidateID_()
		end)

		--
		local AddEventListener, RemoveEventListener = Runtime.addEventListener, Runtime.removeEventListener

		function Runtime:addEventListener (what, listener)
			if what == "enterFrame" then
				Events:addEventListener(what, listener)
			else
				AddEventListener(self, what, listener)
			end
		end

		function Runtime:removeEventListener (what, listener)
			if what == "enterFrame" then
				Events:removeEventListener(what, listener)
			else
				RemoveEventListener(self, what, listener)
			end
		end
	end
end

--- Invalidate any ID from last frame.
function M.InvalidateID ()
	FrameID = -abs(FrameID)
end

--- Enforces that a function is only called once per frame, e.g. for lazy updates.
-- @callable func One-argument function to be called.
-- @treturn function Wrapper. On the first call in a frame, _func_ is called with the
-- wrapper's argument.
--
-- This wrapper returns **true** on the first call in a frame, **false** otherwise.
function M.OnFirstCallInFrame (func)
	local id

	return function(arg)
		local is_first = id ~= _GetFrameID_()

		if is_first then
			id = FrameID

			func(arg)
		end

		return is_first
	end
end

-- "enterFrame" listener --
Runtime:addEventListener("enterFrame", function(event)
	local now = event.time

	-- Update the time difference.
	Diff, Last = Last and (now - Last) / 1000 or 0, now
end)

-- "system" listener --
Runtime:addEventListener("system", function(event)
	if event.type == "applicationStart" or event.type == "applicationResume" then
--		Last, Diff = system.getTimer(), 0
	elseif event.type == "applicationExit" then
--		SG:Clear()
	end
end)

_GetFrameID_ = M.GetFrameID
_InvalidateID_ = M.InvalidateID

return M