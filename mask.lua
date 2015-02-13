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
local floor = math.floor
local max = math.max
local remove = os.remove
-- local pairs = pairs
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
local function NewRect (group, stash, x, y, w, h, color)
	local n, rect = stash and stash.numChildren or 0

	if n > 0 then
		rect = stash[n]

		rect.width, rect.height = w, h

		group:insert(rect)
	else
		rect = display.newRect(group, 0, 0, w, h)
	end

	UpperLeftAlign(rect, x, y)

	if color then
		rect:setFillColor(color)
	end

	return rect
end

-- Helper for black regions of mask texture
local function BlackRect (group, stash, x, y, w, h)
	return NewRect(group, stash, x, y, w, h, 0)
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

		BlackRect(group, nil, 0, 0, w + ew + xpad * 2, h + eh + ypad * 2)
		NewRect(group, nil, xpad, ypad, w + ew, h + eh)
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

-- --
local SheetFrame = {}

-- --
local Sheet = { frames = { SheetFrame } }

--
local function CaptureBounds (group, bounds, hidden)
	-- Contents are visible: just capture the bounds directly.
	if not hidden then
		return display.captureBounds(bounds)
	else
		local gbounds = group.contentBounds

		-- Obscured, but within bounds: just capture the group.
		if BoundsMatch(bounds, gbounds) then
			return display.capture(group)

		-- Out-of-bounds: make an intermediate image and capture that.
		else
			-- Move the group fully on screen, detecting too-large cases.
			local movex, movey = max(0, -gbounds.xMin), max(0, -gbounds.yMin)

			assert(gbounds.xMax + movex <= CW, "Frame too wide to capture!")
			assert(gbounds.yMax + movey <= CH, "Frame too tall to capture!")
			-- ^^ Okay?

			-- Save the group to an image.
			group.x, group.y = group.x + movex, group.y + movey

			local name = Save(group, nil, system.TemporaryDirectory)

			-- Create a one-frame image sheet, where the frame is positioned over the bounded part of
			-- the content. Reload the group as an image (in the group's parent). Capture and return
			-- that. Since the image was temporary, return the name to allow for cleanup.
			SheetFrame.x, SheetFrame.width = movex, bounds.xMax - bounds.xMin
			SheetFrame.y, SheetFrame.height = movey, bounds.yMax - bounds.yMin

			local sheet = graphics.newImageSheet(name, system.TemporaryDirectory, Sheet)

			return display.newImage(group.parent, sheet, 1), name
		end
	end
end

-- Converts an ordered collection of positions into an easier-to-use map
local function ConvertFrames (arr, xcorrect, ycorrect)
	local frames = {}

	for i = 1, #arr, 3 do
		frames[arr[i]] = { xcorrect - arr[i + 1], ycorrect - arr[i + 2] }
	end

	return frames
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
--[[
-- Compute the final, multiple-of-4 dimension, accounting for the 3-pixel black border
local function FinalDim (n, dim)
	return NextMult4(n * dim + (n + 1) * 3) -- 3 between each frame and per side
end
]]
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
		arg = WithDatabase(method, arg, filename, AuxRead)

	-- Read from PNG --
	elseif method == "image_metadata" then
		-- FindText(keyword) = "CoronaMaskData"...
		-- arg = ...
	end

	--
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
local function WriteData (opts, frames, fdimx, fdimy, filename)
	local data, method = json.encode{ frames = frames, fdimx = fdimx, fdimy = fdimy }, opts.method

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
-- @ptable opts
-- @treturn MaskSheet MS
function M.NewSheet (opts)
	local fdimx = assert(opts and (opts.dimx or opts.dim), "Missing frame dimension")
	local fdimy, w, h = opts.dimy or fdimx, assert(opts.w, "Missing width"), assert(opts.h, "Missing height")

	-- ...
	local name, base_dir = assert(opts.name, "Missing filename"), opts.dir or system.CachesDirectory
	local filename = ("__%s_%ix%i__.png"):format(name, w, h)
	local exists = file.Exists(filename, base_dir)
	local data = exists and not opts.recreate and ReadData(opts, filename)
	local MaskSheet, frames, mask, xscale, yscale = {}, {}

	--
	if data then
		frames = ConvertFrames(data.frames)
		mask = graphics.newMask(filename, base_dir)
		-- xscale, yscale = data.framew / ...

		-- Add dummy methods.
		local function Fail ()
			assert(false, "Mask already created")
		end

		MaskSheet.AddFrame, MaskSheet.Commit, MaskSheet.GetRect, MaskSheet.StashRect = Fail, Fail, Fail, Fail

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
		local mgroup, stash, into = display.newGroup(), display.newGroup(), opts.into

		stash.isVisible = false

		if into then
			into:insert(mgroup)
			into:insert(stash)
		end

		--
		local back = BlackRect(into or display.getCurrentStage(), nil, 0, 0, fdimx, fdimy)

--		local x, y = 0, 0 --(ydim - fdimy) / 2 -- <- ydim WILL be even, check fdimy
		local bounds, yfunc = back.contentBounds, opts.yfunc or DefYieldFunc
		local ncols, nrows, hidden--[[, xdim]] = 0, 0, not not opts.hidden
		local x, y, endx = 3, 3

		local DX, DY = fdimx + 3, fdimy + 3
		local NCOLS = floor((CW + 3) / DX)
		local NROWS = floor((CH + 3) / DY)

		--- DOCME
		-- @callable func
		-- @param index
		-- @bool is_white
		-- @callable[opt] after
		function MaskSheet:AddFrame (func, index, is_white, after)
			assert(not mask, "Mask already created")
			assert(nrows < NROWS--[[y < CH]], "No space for new frames")

			--
			local cgroup, bg = display.newGroup(), is_white and 1 or 0

			if into then
				into:insert(cgroup)
			end

			-- Add the left-hand black border.
		--	BlackRect(mgroup, x, y, 3, dim)

			-- Add the background color, i.e. the component of the frame not defined by the shapes.
			back:setFillColor(bg)

			-- Save the frame's left-hand coordinate.
			frames[#frames + 1] = index
			frames[#frames + 1] = x
			frames[#frames + 1] = y

			--
			func(cgroup, 1 - bg, fdimx, fdimy, index)

			-- Capture the frame and incorporate it into the built-up mask.
			local capture = CaptureBounds(cgroup, bounds, hidden)

			if after then
				after(cgroup, index)
			end

			cgroup:removeSelf()
			mgroup:insert(capture)

			UpperLeftAlign(capture, x, y)

			-- Advance past the frame.
		--	x = x + fdimx
--[[
			if ncols == 0 then
				nrows = nrows + 1
			end
]]

			if ncols == NCOLS then--x >= CW then
			--	xdim = xdim or FinalDim(ncols, fdimx)
				endx = endx or x + DX

				ncols, nrows, x, y = 0, nrows + 1, 3, y + DY
				-- Black rect above: y - 3
			else
				ncols, x = ncols + 1, x + DX
			end

			yfunc()
		end

		--- DOCME
		function MaskSheet:Commit ()
			assert(not mask, "Mask already created")

			--
			local XDIM = NextMult4(endx or x)
			local YDIM = NextMult4(y + (ncols > 0 and DY or 0))
--[[
			-- Compute the final width and use it to add the other edge borders.
			-- TODO: Recenter the frames?
			xdim = xdim or FinalDim(ncols, fdimx)

			local ydim = FinalDim(nrows, fdimy)--NextMult4(fdimy + 6)
			-- ^^^ These should just extend
-- Black bar on right, below (also above, if not xdim...)
			BlackRect(mgroup, stash, 0, 0, xdim, y) -- top
			BlackRect(mgroup, stash, 0, y + fdimy, xdim, ydim - (y + fdimy)) -- bottom
			BlackRect(mgroup, stash, x, y, xdim - x, fdimy) -- right
			]]
			local background = BlackRect(mgroup, stash, 0, 0, XDIM, YDIM)

			background:toBack()
-- ^^^ TODO: Rows
-- One big one, put it in back
			-- Save the image and mask data.
			Save(mgroup, filename, base_dir)
			WriteData(opts, frames, fdimx, fdimy, filename)

			-- Clean up temporary resources and create a mask.
			back:removeSelf()
			mgroup:removeSelf()
			stash:removeSelf()

			back, bounds, into, mgroup, stash, yfunc = nil

			mask = graphics.newMask(filename, base_dir)

			--

			-- Correct the mask coordinates to refer to the frame centers, relative to the mask center.
			--[[
			local XHALF, YHALF = XDIM / 2, YDIM / 2
			local xcorr, ycorr = floor(fdimx / 2), floor(fdimy / 2)
			local correct = (xdim - fdimx) / 2 <- Add 1?
			-- xcorrect - arr[i + 1], ycorrect - arr[i + 2]

			for k, v in pairs(pos) do
				pos[k] = correct - v
			end]]
		--	frames = ConvertFrames(frames, XHALF - xcorr, YHALF - ycorr)
			-- ^^^ ????
			-- ^^^ How does this change?
			-- Does it account for odd sizes?
--opts.w / fdimx, opts.h / fdimy
			-- Figure out scales
			-- Save stuff?
		end

		--- DOCME
		-- @pgroup group
		-- @number x
		-- @number y
		-- @number w
		-- @number h
		-- @number[opt] fill
		-- @treturn DisplayObject X
		function MaskSheet:GetRect (group, x, y, w, h, fill)
			assert(not mask, "Mask already created")

			return NewRect(group, stash, x, y, w, h, fill)
		end

		--- DOCME
		-- @pobject rect X
		function MaskSheet:StashRect (rect)
			if stash then
				stash:insert(rect)
			else
				rect:removeSelf()
			end
		end
	end

	--- DOCME
	-- @treturn boolean S###
	function MaskSheet:IsLoaded ()
		return mask ~= nil
	end

	--- DOCME
	-- @pobject object
	-- @param index
	function MaskSheet:Set (object, index)
		object:setMask(assert(mask, "Mask not ready"))

		local x, y = unpack(frames[index])

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