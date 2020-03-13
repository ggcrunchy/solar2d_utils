--- Various geometry soup types and their operations.

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
local setmetatable = setmetatable
local unpack = unpack

-- Unique keys --
local _above = {}
local _above_flag = {}
local _base_offset = {}
local _cur_column = {}
local _index_shape_count = {}
local _indices = {}
local _left_flag = {}
local _lower_left = {}
local _lower_right = {}
local _max_index = {}
local _ncols = {}
local _offset = {}
local _shape = {}
local _upper_left = {}
local _upper_right = {}

-- Cached module references --
local _AddQuadIndices_
local _AddTriangleIndices_

-- Exports --
local M = {}

--
--
--

--- DOCMEMORE
-- Add the indices in quad-sized blocks (0, 2, 1; 1, 2, 3)
function M.AddQuadIndices (indices, i1, i2, i3, i4, base)
	base = base or #indices

	_AddTriangleIndices_(indices, i1, i3, i2, base)
	_AddTriangleIndices_(indices, i2, i3, i4, base + 3)
end

--- DOCME
function M.AddTexturedVertex (uvs, vertices, x, y, w, h)
	uvs[#uvs + 1] = x / w + .5
	uvs[#uvs + 1] = y / h + .5

	vertices[#vertices + 1] = x
	vertices[#vertices + 1] = y
end

--- DOCMEMORE
-- Add the indices as a triangle
function M.AddTriangleIndices (indices, i1, i2, i3, base)
	base = base or #indices

	indices[base + 1] = i1
	indices[base + 2] = i2
	indices[base + 3] = i3
end

local Builder = {}

Builder.__index = Builder

--- Manually clear the "above" flag.
function Builder:ClearAboveFlag ()
	self[_above_flag] = nil
end

--- Manually clear the "left" flag.
function Builder:ClearLeftFlag ()
	self[_left_flag] = nil
end

local function GetIndices (B)
	local indices = B[_indices] or {} -- if first emission, might not have indices

	B[_indices] = indices

	return indices
end

local function NewIndex (B)
	local max_index = B[_max_index] + 1

	B[_max_index] = max_index

	return max_index
end

local function PrepareToAdvance (B, ul_offset, ur_index)
	B:ClearAboveFlag()
	B:ClearLeftFlag()
	B:SetUpperLeft(nil)
	B:SetUpperRight(nil)
	B:SetLowerLeft(nil)
	B:SetLowerRight(nil)

	local col = B[_cur_column]

	if col then
		local above, ncols = B[_above], B[_ncols]

		if ur_index then -- is this a bottom edge?
			ul_offset, above[-col] = -ul_offset, ur_index -- indicate explicit index
		end

		above[col] = ul_offset -- n.b. on last column in first row, stomps "first_row" out

		if col == ncols then
			col = 0 -- increment back to 1
		end

		ul_offset = above[ncols] ~= "first_row" and above[col + 1] -- cf. ResetMethod()

		if ul_offset then
			local indices, ul, ur = B[_indices]

			if ul_offset < 0 then -- explicit index?
				ul, ur = -ul_offset, above[-(col + 1)]
			else
				ul, ur = indices[ul_offset], indices[ul_offset + 1]
			end

			B:SetAboveFlag()
			B:SetUpperLeft(ul)
			B:SetUpperRight(ur)
		end

		B[_cur_column] = col + 1
	end

	return col == 0 and "newline"
end

local function SetNonIndexShape (B, shape)
	assert(B[_shape] ~= "indices", "Index shape in progress")

	B[_index_shape_count], B[_shape] = 0, shape
end

--- DOCME
function Builder:EmitBottomEdge ()
	SetNonIndexShape(self, "bottom_edge")

	local ll = self[_lower_left] or NewIndex(self)
	local lr = self[_lower_right] or NewIndex(self)

	if PrepareToAdvance(self, ll, lr) ~= "newline" then -- explicit indices
		-- TODO: what if has above? should probably set upper-left, but what about left edge flag?
		self:SetLowerLeft(lr)
	end
end

local function AuxEmitIndex (B, index)
	local indices, offset = GetIndices(B), B[_offset]

	indices[offset + 1] = index or NewIndex(B)

	B[_index_shape_count], B[_offset], B[_shape] = B[_index_shape_count] + 1, offset + 1, "indices"
end

local function DegenerateIndex (B)
	local indices, offset = GetIndices(B), B[_offset]

	if offset ~= B[_base_offset] then -- any index available?
		return indices[offset]
	else -- no: make a dummy and refer to it
		local index = NewIndex(B)

		indices[offset + 1], B[_offset] = index, offset + 1

		return index
	end
end

--- DOCME
function Builder:EmitDegenerateIndex ()
	AuxEmitIndex(self, DegenerateIndex(self))
end

local function SetDegenerateIndices (B)
	local index = DegenerateIndex(B)

	B:SetUpperLeft(index)
	B:SetUpperRight(index)
	B:SetLowerLeft(index)
	B:SetUpperRight(index)
end

--- DOCME
function Builder:EmitDegenerateQuad ()
	SetDegenerateIndices(self)

	self:EmitQuad()
end

--- DOCME
function Builder:EmitDegenerateTriangle ()
	SetDegenerateIndices(self)

	self:EmitTriangle()
end

--- DOCME
function Builder:EmitIndex (index)
	AuxEmitIndex(self, index)
end

--- DOCME
function Builder:EmitQuad ()
	SetNonIndexShape(self, "quad")

	local indices, offset = GetIndices(self), self[_offset]
	local ul = self[_upper_left] or NewIndex(self)
	local ur = self[_upper_right] or NewIndex(self)
	local ll = self[_lower_left] or NewIndex(self) -- cf. note in EmitBottomEdge() method
	local lr = self[_lower_right] or NewIndex(self)

	_AddQuadIndices_(indices, ul, ur, ll, lr, offset)

	self[_offset] = offset + 6

	if PrepareToAdvance(self, offset + 5) ~= "newline" then -- offset of lower-left, cf. AddQuadIndices()
		self:SetLeftFlag()
		self:SetUpperLeft(ur) -- n.b. often redundant
		self:SetLowerLeft(lr)
	end
end

--- DOCME
function Builder:EmitTriangle ()
	SetNonIndexShape(self, "tri")

	local above_flag, left_flag, i1, i2, i3 = self[_above_flag], self[_left_flag]
	local indices, offset = GetIndices(self), self[_offset]
	local ul = self[_upper_left]
	local ur = self[_upper_right]
	local ll = self[_lower_left]
	local lr = self[_lower_right]

	if above_flag then
		-- edge above, so triangle will be flush with it in forms:
		--
		-- x--x     x--x
		--  \ | and | /
		--   \|     |/
		--    x     x
		--
		-- the triangle on the right will also be flush with an edge on the left
		i1, i2 = ul, ur

		if left_flag then -- n.b. ll or lr might be nil
			i3 = ll
		else
			i3 = lr
		end
	else
		-- here the corresponding triangles are vertically flipped, but the left-right condition holds
		i1, i2 = lr, ll

		if left_flag then -- cf. note for "above" case
			i3 = ul
		else
			i3 = ur
		end
	end

	i1 = i1 or NewIndex(self)
	i2 = i2 or NewIndex(self) -- cf. note in EmitBottomEdge() method, with i2 and i3 in lieu of ll and lr
	i3 = i3 or NewIndex(self)

	_AddTriangleIndices_(indices, i1, i2, i3, offset)

	self[_offset] = offset + 3

	if PrepareToAdvance(self, not above_flag and offset + 2) ~= "newline" and not left_flag then -- offset of lower-left, in non-"above" case
		self:SetLeftFlag()

		if above_flag then -- i2 = ur, i3 = lr
			self:SetUpperLeft(i2) -- n.b. often redundant
			self:SetLowerLeft(i3)
		else -- i1 = lr, i3 = ur
			self:SetUpperLeft(i3) -- n.b. often redundant
			self:SetLowerLeft(i1)
		end
	end
end

--- DOCME
function Builder:FinishIndexShape ()
	local shape = self[_shape]

	assert(shape == "indices", "Index shape not in progress")

	PrepareToAdvance(self, nil)

	self[_shape] = "none" -- invalidate so other shapes can proceed, but leave count for querying
end

--- DOCME
function Builder:GetLastIndices (how)
	local indices, shape, offset, count = self[_indices], self[_shape], self[_offset]

	if shape == "quad" then
		if how == "raw" then
			count = 6
		else -- cf. AddQuadIndices()
			return indices[offset - 5], unpack(indices, offset - 2, offset)
		end
	elseif shape == "tri" then
		count = 3
	elseif shape == "bottom_edge" then -- cf. PrepareToAdvance() and EmitBottomEdge() method
		local prev = self[_cur_column] - 1
		local above, col = self[_above], prev > 0 and prev or self[_ncols]

		return -above[col], above[-col]
	else
		count = self[_index_shape_count] -- cf. note in FinishIndexShape() method
	end

	return unpack(indices, offset - count + 1, offset)
end

--- DOCME
function Builder:GetLastShape ()
	local shape = self[_shape]

	if shape == "indices" then
		return "indices", self[_index_shape_count]
	else
		return shape
	end
end

--- DOCME
function Builder:GetMaxIndex ()
	return self[_max_index]
end

--- DOCME
function Builder:GetResult ()
	local out, offset = self[_indices], self[_offset]

	self:Reset() -- n.b. offset stays intact

	if offset ~= self[_base_offset] then
		return out, offset
	else
		return out, 0 -- indices table may be present but unwritten
	end
end

--- DOCME
function Builder:Reset (max_index)
	self[_upper_left], self[_upper_right], self[_lower_left], self[_lower_right] = nil
	self[_above_flag], self[_left_flag], self[_indices] = nil
	self[_index_shape_count], self[_max_index] = 0, max_index or 0
	self[_offset], self[_shape] = self[_base_offset], "none"

	local above = self[_above]

	if above then
		self[_cur_column], above[self[_ncols]] = 1, "first_row" -- cf. PrepareToAdvance()
	end
end

--- DOCME
function Builder:SetAboveFlag ()
	self[_above_flag] = true
end

--- DOCME
function Builder:SetIndices (indices)
	assert(self[_offset] == self[_base_offset], "Building in progress")

	self[_indices] = indices
end

--- DOCME
function Builder:SetLeftFlag ()
	self[_left_flag] = true
end

--- DOCME
function Builder:SetLowerLeft (index)
	self[_lower_left] = index
end

--- DOCME
function Builder:SetLowerRight (index)
	self[_lower_right] = index
end

--- DOCME
function Builder:SetMaxIndex (max_index)
	assert(self[_offset] == self[_base_offset], "Building in progress")

	self[_max_index] = max_index or 0
end

--- DOCME
function Builder:SetOffset (offset)
	assert(self[_offset] == self[_base_offset], "Building in progress")

	self[_offset] = offset or 0
end

--- DOCME
function Builder:SetUpperLeft (index)
	self[_upper_left] = index
end

--- DOCME
function Builder:SetUpperRight (index)
	self[_upper_right] = index
end

--- DOCME
function Builder:Skip ()
	PrepareToAdvance(self, nil)

	self[_shape], self[_index_shape_count] = "none", 0
end

local function NewBuilder (ncols)
	local builder = setmetatable({ [_base_offset] = 0 }, Builder)

	if ncols then
		builder[_above], builder[_ncols] = {}, ncols
	end

	builder:Reset()

	return builder
end

--- DOCME
function M.NewLatticeBuilder (ncols)
	assert(ncols and ncols > 0, "Invalid column count")

	return NewBuilder(ncols)
end

--- DOCME
M.NewSequenceBuilder = NewBuilder

_AddQuadIndices_ = M.AddQuadIndices
_AddTriangleIndices_ = M.AddTriangleIndices

return M