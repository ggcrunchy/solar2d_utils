--- This module helps capture or save portions of the content area.

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
local max = math.max

-- Modules --
local file = require("corona_utils.file")
local frames = require("corona_utils.frames")
local strings = require("tektite_core.var.strings")

-- Corona globals --
local display = display
local graphics = graphics
local system = system

-- Cached module references --
local _Save_

-- Exports --
local M = {}

-- Default yield function: no-op
local function DefYieldFunc () end

-- Configurable frame --
local SheetFrame = {}

-- Single-frame sheet used to confine capture --
local Sheet = { frames = { SheetFrame } }

--- DOCME
-- @pgroup group
-- @param bounds
-- @ptable[opt] opts
-- @treturn DisplayObject X
function M.CaptureBounds (group, bounds, opts)
	local yfunc = opts and opts.yfunc or DefYieldFunc

	-- Sanity check: if this might be an early frame, attempt to yield.
	if frames.GetFrameID() < 5 then
		yfunc()
	end

	-- Contents are visible: just capture the bounds directly.
	if not (opts and opts.hidden) then
		return display.captureBounds(bounds)
	else
		local gbounds = group.contentBounds

		-- Obscured, but within bounds: just capture the group.
		if gbounds.xMin >= 0 and gbounds.yMin >= 0 and gbounds.xMax <= bounds.xMax and gbounds.yMax <= bounds.yMax then
			return display.capture(group)

		-- Out-of-bounds: make an intermediate image and capture that.
		else
			local w, h = opts and opts.w or display.contentWidth, opts and opts.h or display.contentHeight
			local base_dir = opts and opts.base_dir or system.DocumentsDirectory

			-- Move the group fully on screen, detecting too-large cases.
			local movex, movey = max(0, -gbounds.xMin), max(0, -gbounds.yMin)

			assert(gbounds.xMax + movex <= w, "Frame too wide to capture!")
			assert(gbounds.yMax + movey <= h, "Frame too tall to capture!")
			-- ^^ Okay?

			-- Save the group to an image.
			group.x, group.y = group.x + movex, group.y + movey

			local name = _Save_(group, nil, base_dir)

			yfunc()

			-- Create a one-frame image sheet, with the group positioned over the bounded part of the
			-- content. Using this sheet, load (and return) the just-saved image. Since the image is
			-- only needed temporary, queue it up for subsequent removal.
			SheetFrame.x, SheetFrame.width = movex, bounds.xMax - bounds.xMin
			SheetFrame.y, SheetFrame.height = movey, bounds.yMax - bounds.yMin

			local sheet = graphics.newImageSheet(name, base_dir, Sheet)
			local image = display.newImage(group.parent, sheet, 1)

			file.PutInTrash_Guard(name, image, base_dir)

			return image
		end
	end
end

-- Group save parameters --
local SaveParams = { isFullResolution = true }

--- DOCME
-- @pgroup group
-- @string[opt] name
-- @param[opt=`system.DocumentsDirectory`] base_dir
-- @treturn string _name_.
function M.Save (group, name, base_dir)
	base_dir = base_dir or system.DocumentsDirectory

	-- Generate a new name, if none is provided. As a sanity check, verify no such file exists.
	if not name then
		repeat
			name = strings.AddExtension(strings.NewName(), "png")
		until not file.Exists(name, base_dir)
	end

	-- Save the group as a PNG. Return the filename, which may have been auto-generated.
	SaveParams.filename, SaveParams.baseDir = name, base_dir

	display.save(group, SaveParams)

	return name
end

-- Cache module members.
_Save_ = M.Save

-- Export the module.
return M