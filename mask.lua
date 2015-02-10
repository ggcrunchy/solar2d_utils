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
local max = math.max
local remove = os.remove
local pairs = pairs
local type = type
local unpack = unpack

-- Modules --
local file = require("corona_utils.file")
local strings = require("tektite_core.var.strings")
local var_preds = require("tektite_core.var.predicates")

-- Corona globals --
local display = display
local graphics = graphics
local system = system

-- Corona modules --
local json = require("json")
local sqlite3 = require("sqlite3")

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

-- --
local SaveParams = { isFullResolution = true }

--
local function Save (group, name, base_dir)
	-- Generate a new name, if none is provided. As a sanity check, verify no such file exists.
	if not name then
		repeat
			name = strings.AddExtension(strings.NewName(), "png")
		until not file.Exists(name, base_dir)
	end

	-- Save the group as a PNG. Return the filename, since it may have been auto-generated.
	SaveParams.filename, SaveParams.baseDir = name, base_dir

	display.save(group, SaveParams)

	return name
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
		Save(group, name, base_dir)

		group:removeSelf()
	end

	return name
end

-- --
local CW, CH = display.contentWidth - 6, display.contentHeight - 6

--
local function BoundsMatch (b1, b2)
	return b1.xMin == b2.xMin and b1.yMin == b2.yMin and b1.xMax == b2.xMax and b1.yMax == b2.yMax
end

--
local function CaptureBounds (group, bounds, hidden)
	-- Contents are visible: just capture the bounds directly.
	if not hidden then
		return display.captureBounds(bounds)
	else
		local gbounds = group.bounds

		-- Obscured, but within bounds: just capture the group.
		if BoundsMatch(bounds, gbounds) then
			return display.capture(group)

		-- Out-of-bounds.
		else
			-- Move the group fully on screen, detecting too-large cases.
			local movex, movey = max(0, -gbounds.xMin), max(0, -gbounds.yMin)

			assert(gbounds.xMax + movex <= CW, "Frame too wide to capture!")
			assert(gbounds.yMax + movey <= CH, "Frame too tall to capture!")
			-- ^^ Okay?

			-- Save the group to an image and reload it as an image (in the group's parent).
			group.x, group.y = group.x + movex, group.y + movey

			local name = Save(group, nil, system.TemporaryDirectory)
			local image = display.newImage(group.parent, name, system.TemporaryDirectory, 0, 0, true)

			image.anchorX, image.x = 0, 0
			image.anchorY, image.y = 0, 0

			-- Create a one-frame image sheet encompassing the mask frame. Capture and return that.
			local frame-- = ...
			image:removeSelf()

			return frame, name
		end
	end
end

--
local function ConvertPos (coords, xcorrect, ycorrect)
	local pos = {}

	for i = 1, #coords, 3 do
		pos[coords[i]] = { xcorrect - coords[i + 1], ycorrect - coords[i + 2] }
	end

	return pos
end

--
local function DefYieldFunc () end

-- read sources: table (parent, key); database (table, key, is_open); embedded (chunk type, key)
-- write methods: to table / JSON; to database; to same file
-- remove: if missing read source
-- tEXt:
-- Keyword:        1-79 bytes (character string)
--   Null separator: 1 byte
--   Text:           n bytes (character string)

--
local function IsPosInt (var)
	return var_preds.IsInteger(var) and var > 0
end

-- Rounds up to next multiple of 4 (mask dimensions requirement)
local function NextMult4 (x)
	local over = x % 4

	return x + (over > 0 and 4 - over or 0)
end

-- Compute the final, multiple-of-4 dimension, accounting for the 3-pixel black border
local function FinalDim (n, dim)
	return NextMult4(n * dim + (n + 1) * 3) -- 3 between each frame and per side
end

--
local function WithDatabase (method, arg, filename, func, data)
	local db, tname

	if type(arg) == "table" then
		db, tname = unpack(arg)
	end

	if method == "database" then
		db = sqlite3.open(db)
	end

	tname = tname or "corona_mask_data"

	local res = func(db, tname, filename, data)

	if method == "database" then
		db:close()
	end

	return res
end

--
local function AuxRead (db, tname, filename)
	local data

	for _ in db:urows([[SELECT 1 FROM sql_master_table WHERE type = 'table' AND name = ']] .. tname .. [[';]]) do
		for _, v in db:urows([[SELECT * FROM ]] .. tname .. [[ WHERE m_KEY = ']] .. filename .. [[';]]) do
			data = v
		end
	end

	return data
end

--
local function ReadData (opts, filename)
	local method, arg = opts.method, opts.arg

	-- Read a string out of a database (which may be opened) --
	-- arg: { name / db, table, key }
	if method == "database" or method == "database_handle" then
		WithDatabase(method, arg, filename, AuxRead)

	-- Read from PNG --
	elseif method == "image_metadata" then
		-- FindText(keyword) = "CoronaMaskData"...
		-- arg = ...
	end

	--
	local atype = type(arg)

	if type(arg) == "string" then
		arg = json.decode(arg)
	end

	return type(arg) == "table" -- Is it a table...
		and IsPosInt(#arg.frames) -- ...does it have per-frame data...
		and IsPosInt(arg.dimx) and IsPosInt(arg.fdimy) -- ...and dimensions?
		and arg -- All good: return the data
end

--
local function AuxWrite (db, tname, filename, data)
	db:exec([[
		CREATE TABLE IF NOT EXISTS ]] .. tname .. [[ (m_KEY UNIQUE, m_DATA);
		INSERT OR REPLACE INTO ]] .. tname .. [[ VALUES(]] .. filename .. [[, ]] .. data .. [[);
	]])
end

--
local function WriteData (opts, pos, fdimx, fdimy, filename)
	local data, method = json.encode{ frames = pos, fdimx = fdimx, fdimy = fdimy }, opts.method

	-- Read a string out of a database (which may be opened) --
	-- arg: { name / db, table name (def = "corona_mask_data") } / arg: name / db
	if method == "database" or method == "database_handle" then
		WithDatabase(method, opts.arg, filename, AuxWrite, data)

	-- Read from PNG --
	-- arg: { filename, keyword (def = "CoronaMaskData") }
	elseif method == "image_metadata" then
		-- FindText(keyword) = "CoronaMaskData"...
		-- ^^ Add (or update)

	-- Raw string --
	else
		opts.arg = data
	end
end

--- DOCME
function M.NewSheet (opts)
	local fdimx = assert(opts and (opts.dimx or opts.dim), "Missing frame dimension")
	local fdimy, w, h = opts.dimy or fdimx, assert(opts.w, "Missing width"), assert(opts.h, "Missing height")

	-- ...
	local name, base_dir = assert(opts.name, "Missing filename"), opts.dir or system.CachesDirectory
	local filename = ("__%s_%ix%i__.png"):format(name, w, h)
	local exists = file.Exists(filename, base_dir)
	local data = exists and not opts.recreate and ReadData(opts, filename)
	local MaskSheet, pos, mask, xscale, yscale = {}, {}

	--
	if data then
		pos = ConvertPos(data.indices)
		mask = graphics.newMask(filename, base_dir)
		-- xscale, yscale = data.framew / ...

	--
	else
		-- If a mask file with the same name exists, remove it.
		if exists then
			assert(base_dir ~= system.ResourceDirectory, "Mask sheet is missing data")

			remove(system.pathForFile(filename, base_dir))
		end

		-- Compute the offset as the 3 pixels of black border plus any padding needed to satisfy
		-- the height requirement. Bounded captures will be used to grab each frame, since using
		-- several containers and capturing all in one go seems to be flaky on the simulator.
		-- TODO: Capture extra pixel in each direction, to improve filtering? (not perfect with circles...)
		-- ^^^ Then need to start at x = 1... (since black border still needed)
		local mgroup, stage = display.newGroup(), display.getCurrentStage()

		BlackRect(stage, 0, 0, fdimx, fdimy)

		local back, x, y = stage[stage.numChildren], 0, 0 --(ydim - fdimy) / 2 -- <- ydim WILL be even, check fdimy
		local bounds, yfunc = back.contentBounds, opts.yfunc or DefYieldFunc
		local ncols, nrows, xdim = 0, 0

		--- DOCME
		function MaskSheet:AddFrame (func, index, is_white)
			assert(not mask, "Mask already created")
			assert(y < CH, "No space for new frames")

			local cgroup, bg = display.newGroup(), is_white and 1 or 0

			-- Add the left-hand black border.
		--	BlackRect(mgroup, x, y, 3, dim)

			-- Add the background color, i.e. the component of the frame not defined by the shapes.
			back:setFillColor(bg)

			-- Save the frame's left-hand coordinate.
		--	pos[index] = { x, y }
			pos[#pos + 1] = index
			pos[#pos + 1] = x
			pos[#pos + 1] = y

			--
			func(cgroup, 1 - bg, fdimx, index)

			-- Capture the frame and incorporate it into the built-up mask.
			local capture = display.captureBounds(bounds) -- CaptureBounds(cgroup, bounds, ?)
	-- ^^ If obscured, do long way with sprite sheet?
			cgroup:removeSelf()
			mgroup:insert(capture)

			UpperLeftAlign(capture, x, y)

			-- Advance past the frame.
			x = x + fdimx

			if ncols == 0 then
				nrows = nrows + 1
			end

			if x >= CW then
				xdim = xdim or FinalDim(ncols, fdimx)

				ncols, x, y = 0, 0, y + fdimy
			else
				ncols = ncols + 1
			end

			yfunc()
		end

		--- DOCME
		function MaskSheet:Commit ()
			assert(not mask, "Mask already created")

			back:removeSelf()

			-- Compute the final width and use it to add the other edge borders.
			-- TODO: Recenter the frames?
			xdim = xdim or FinalDim(ncols, fdimx)

			local ydim = FinalDim(nrows, fdimy)--NextMult4(fdimy + 6)

			BlackRect(mgroup, 0, 0, xdim, y) -- top
			BlackRect(mgroup, 0, y + fdimy, xdim, ydim - (y + fdimy)) -- bottom
			BlackRect(mgroup, x, y, xdim - x, fdimy) -- right
-- ^^^ TODO: Rows
			-- Save the mask (if it was not already generated).
	--		if not file.Exists(arg1, base_dir) then
			Save(mgroup, filename, base_dir)
			WriteData(opts, pos, fdimx, fdimy, filename)
			-- ^^^ What else?
	--		end

			mgroup:removeSelf()

			mask, mgroup = graphics.newMask(filename, base_dir)

			--

			-- Correct the mask coordinates to refer to the frame centers, relative to the mask center.
			--[[
			local correct = (xdim - fdimx) / 2

			for k, v in pairs(pos) do
				pos[k] = correct - v
			end]]
			pos = ConvertPos(pos, (xdim - fdimx) / 2, (ydim - fdimy) / 2)
			-- ^^^ How does this change?
			-- Does it account for odd sizes?
--opts.w / fdimx, opts.h / fdimy
			-- Figure out scales
			-- Save stuff?
		end

	-- Add dummy methods if mask already existed.
	else
		local function Fail ()
			assert(false, "Mask already created")
		end

		MaskSheet.AddFrame, MaskSheet.Commit = Fail, Fail
	end

	--- DOCME
	function MaskSheet:IsLoaded ()
		return mask ~= nil
	end

	--- DOCME
	function MaskSheet:Set (object, index)
		object:setMask(assert(mask, "Mask not ready"))

		local x, y = unpack(pos[index])

		object.maskX, object.maskScaleX = x * xscale, xscale
		object.maskY, object.maskScaleY = y * yscale, yscale
		-- ^^ ceil()?
	end

	return MaskSheet
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