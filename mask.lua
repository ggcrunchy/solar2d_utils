--- This module supports dynamic mask generation.

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
local format = string.format
local pairs = pairs

-- Modules --
local file = require("corona_utils.file")
local strings = require("tektite_core.var.strings")

-- Corona globals --
local display = display
local graphics = graphics
local system = system

-- Exports --
local M = {}

-- Add 3 pixels to each side, then add (4 - 1) to round up to next multiple of 4 --
local Rounding = 3 * 2 + 3

-- Helper to get extra padding and report odd counts
local function Extra (n)
	local padding = Rounding - (n + Rounding) % 4
	local odd = padding % 2

	return (padding - odd) / 2, odd
end

--
local function UpperLeftAlign (object, x, y)
	object.anchorX, object.x = 0, x or 0
	object.anchorY, object.y = 0, y or 0
end

--
local function NewRect (group, x, y, w, h, color)
	local rect = display.newRect(group, 0, 0, w, h)

	UpperLeftAlign(rect, x, y)

	if color then
		rect:setFillColor(color)
	end
end

-- Helper for black regions of mask texture
local function BlackRect (group, x, y, w, h)
	NewRect(group, x, y, w, h, 0)
end

--
local function Save (group, name, base_dir)
	display.save(group, { filename = name, baseDir = base_dir, isFullResolution = true })
end

--- Generates a rectangular mask, for use with `graphics.setMask`.
-- @uint w Mask width...
-- @uint h ...and height.
-- @param[opt] name File name to assign to mask; if absent, one will be auto-generated.
-- @param[opt=`system.CachesDirectory`] base_dir Directory where mask is stored.
-- @treturn string Mask file name.
-- @treturn number xscale Scale to apply to mask to fit _w_...
-- @treturn number yscale ...and to fit _h_.
function M.NewMask (w, h, name, base_dir)
	name, base_dir = name and format(name, w, h), base_dir or system.CachesDirectory

	-- If the mask exists, reuse it; otherwise, build it.
	if not file.Exists(name, base_dir) then
		local group = display.newGroup()
		local xpad, ew = Extra(w)
		local ypad, eh = Extra(h)

		BlackRect(group, 0, 0, w + ew + xpad * 2, h + eh + ypad * 2)
		NewRect(group, xpad, ypad, w + ew, h + eh)

		name = name or strings.AddExtension(strings.NewName(), "png")

		Save(group, name, base_dir)

		group:removeSelf()
	end

	return name
end

-- Rounds up to next multiple of 4 (mask dimensions requirement)
local function NextMult4 (x)
	local over = x % 4

	return x + (over > 0 and 4 - over or 0)
end

--- DOCME
function M.NewSheet (opts)
	local fdimx = assert(opts and (opts.dimx or opts.dim), "Missing frame dimension")
	local fdimy = opts.dimy or fdimx

	-- Compute the final height, based on the twin requirements of black borders at least 3
	-- pixels thick and being a multiple of 4.
	local ydim = NextMult4(fdimy + 6)

	-- Compute the offset as the 3 pixels of black border plus any padding needed to satisfy
	-- the height requirement. Bounded captures will be used to grab each frame, since using
	-- several containers and capturing all in one go seems to be flaky on the simulator.
	-- TODO: Capture extra pixel in each direction, to improve filtering? (not perfect with circles...)
	-- ^^^ Then need to start at x = 1... (since black border still needed)
	local mgroup = display.newGroup()
	local stage = display.getCurrentStage()

	BlackRect(stage, 0, 0, fdimx, fdimy)

	local back, x, y = stage[stage.numChildren], 0, (ydim - fdimy) / 2
	local bounds, xscale, yscale = back.contentBounds, opts.w / fdimx, opts.h / fdimy
	local pos, mask = {}

	return function(what, arg1, arg2, arg3)
		-- Set --
		-- arg1: display object
		-- arg2: frame index
		if what == "set" then
			arg1:setMask(mask)

			arg1.maskX = pos[arg2] * xscale
			arg1.maskScaleX = xscale
			arg1.maskScaleY = yscale

		-- Frame --
		-- arg1: func
		-- arg2: frame index
		-- arg3: boolean (is background white?)
		elseif what == "frame" then
			local cgroup, bg = display.newGroup(), arg3 and 1 or 0

			-- Add the left-hand black border.
		--	BlackRect(mgroup, x, y, 3, dim)

			-- Add the background color, i.e. the component of the frame not defined by the shapes.
			back:setFillColor(bg)

			-- Save the frame's left-hand coordinate.
			pos[arg2] = x

			--
			arg1(cgroup, 1 - bg, fdimx, arg2)

			-- Capture the frame and incorporate it into the built-up mask.
			local capture = display.captureBounds(bounds)

			cgroup:removeSelf()
			mgroup:insert(capture)

			UpperLeftAlign(capture, x, y)

			-- Advance past the frame.
			x = x + fdimx

		-- End --
		-- arg1: Filename
		-- arg2: Directory (if absent, CachesDirectory)
		elseif what == "end" then
			back:removeSelf()

			-- Compute the final width and use it to add the other edge borders.
			-- TODO: Recenter the frames?
			local xdim = NextMult4(x)

			BlackRect(mgroup, 0, 0, xdim, y) -- top
			BlackRect(mgroup, 0, y + fdimy, xdim, ydim - (y + fdimy)) -- bottom
			BlackRect(mgroup, x, y, xdim - x, fdimy) -- right

			-- Save the mask (if it was not already generated).
			local base_dir = arg2 or system.CachesDirectory

			if not file.Exists(arg1, base_dir) then
				Save(mgroup, arg1, base_dir)
			end

			display.remove(mgroup)

			mask, mgroup = graphics.newMask(arg1, base_dir)

			-- Correct the mask coordinates to refer to the frame centers, relative to the mask center.
			local correct = (xdim - fdimx) / 2

			for k, v in pairs(pos) do
				pos[k] = correct - v
			end

			-- Figure out scales
			-- Save stuff?
		end
	end
end

--- DOCME
-- @pobject object X
-- @number[opt=object.width] w
-- @number[opt=object.height] h
-- See: https://github.com/Lerg/dynamic-masks/blob/master/main.lua
function M.SetDynamicMask (object, w, h)
	--
	display.setDefault("magTextureFilter", "nearest")

	object:setMask(graphics.newMask("corona_utils/assets/mask.png"))

	display.setDefault("magTextureFilter", "linear")

	--
	w, h = w or object.width, h or object.height

	object.maskScaleX = .5 * w
	object.maskScaleY = .5 * h

	if object._type == "GroupObject" then
		object.maskX = object.x + object.maskScaleX
		object.maskY = object.y + object.maskScaleY
	else
		object.maskX = object.x
		object.maskY = object.y
	end
end

-- PROBATION: Looks like NewMask(), at least, needs anchor fixes? (Make some tests)
-- Also also, sheets more or less assumes deterministic pairs()... which is brittle, to say the least! (needs some metadata)
-- TODO: More robust if the generator does "line feed"s to not overflow the screen

-- Export the module.
return M