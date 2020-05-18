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

-- Solar2D globals --
local Runtime = Runtime

-- Exports --
local M = {}

--
--
--

local Diff

local UpdateTime

---
-- @treturn number Difference in time since last frame.
function M.DiffTime ()
	UpdateTime()

	return Diff
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
		local frame_id = Runtime.getFrameID()
		local is_first = id ~= frame_id

		if is_first then
			id = frame_id

			func(arg)
		end

		return is_first
	end
end

local Last

UpdateTime = M.OnFirstCallInFrame(function()
	local now = Runtime.getFrameStartTime()

	Diff, Last = Last and (now - Last) / 1000 or 0, now
end)

--[[
Runtime:addEventListener("system", function(event)
	if event.type == "applicationStart" or event.type == "applicationResume" then
--		Diff, Last = 0
	elseif event.type == "applicationExit" then
--		
	end
end)
]]

return M