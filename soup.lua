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
local floor = math.floor
local setmetatable = setmetatable
local type = type

-- Modules --
local embedded_free_list = require("tektite_core.array.embedded_free_list")

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

local QuadSoup = {}

QuadSoup.__index = QuadSoup

local function PadQuadSoup (QS, n, nused)
	QS.m_free = embedded_free_list.Extend(QS.m_slots, n - nused, QS.m_free)

	local indices, final = QS.m_indices, n * 4 -- final legal index, will come into use if and when stream saturated
-- TODO: not right for grid
	for i = 6 * (nused - 1) + 1, 6 * n do
		indices[i] = final -- make all degenerate
	end
end

--- DOCME
function QuadSoup:GetIndices ()
	local nused, n = #self.m_slots, self.m_capacity

	if nused < n then
		PadQuadSoup(self, n, nused)
	end

	return self.m_indices
end

--- DOCME
-- @treturn ?|uint|nil
function QuadSoup:Insert ()
	local slots, first = self.m_slots
	local index, free = embedded_free_list.GetInsertIndex(slots, self.m_free)

	if index <= self.m_capacity then
		local iminus1, cols, above, to_below = index - 1, self.m_cols

		if cols then
			above, to_below = iminus1 + floor(iminus1 / cols), cols + 1 -- line break on last column
		else
			above, to_below = iminus1 * 4, 2 -- "below" simply next two in line after first pair
		end

		slots[index], first = true, 2 * above + 1 -- two coordinates per vertex

		local below = above + to_below

		_AddQuadIndices_(self.m_indices, above + 1, above + 2, below + 1, below + 2, iminus1 * 6)

		self.m_free = free
	end

	return first
end

local function MakeDegenerateQuad (indices, offset)
	local index = 2 * offset + 1

	_AddQuadIndices_(indices, index, index, index, index, offset * 6 + 1)
end

--- DOCME
-- @uint first
function QuadSoup:Remove (first)
	local qoffset = .125 * (first - 1) -- quad = four corners, two coordinates each
	local slots, removed = self.m_slots, false
-- TODO: not right for grid
	if embedded_free_list.InUse(slots, qoffset + 1) then
		self.m_free = embedded_free_list.RemoveAt(slots, qoffset + 1, self.m_free)

		MakeDegenerateQuad(self.m_indices, qoffset)

		removed = true
	end

	return removed
end

--- DOCME
-- @uint n
-- @string[opt="indexed"] mode
function M.NewQuadSoup (n, mode)
	local qs, indices = { m_capacity = n, m_slots = {} }, {}

	if mode == "vertex" then
		-- "fat" mesh, replicated vertices / uvs
	elseif mode == "deformed_rects" then
		-- list of rects, some replication
		-- uvs?
	else
		if type(mode) == "number" then
			qs.m_cols = mode
		end

		qs.m_indices = indices
	end

	qs.m_mode = mode

	return setmetatable(qs, QuadSoup)
end

local TriangleSoup = {}

--- DOCME
-- @treturn ?|uint|nil
function TriangleSoup:Insert ()
	--
end

--- DOCME
-- @uint index
function TriangleSoup:Remove (first)
	--
end

--- DOCME
function M.NewTriangleSoup (n)
	--
end










-- Standard library imports --
local assert = assert
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

-- Exports --
local MM = {}

--
--
--

local MeshBuilder = {}

MeshBuilder.__index = MeshBuilder

--- DOCME
function MeshBuilder:ClearAboveFlag ()
	self[_above_flag] = nil
end

--- DOCME
function MeshBuilder:ClearLeftFlag ()
	self[_left_flag] = nil
end

local function NewIndex (MB)
	local max_index = MB[_max_index] + 1

	MB[_max_index] = max_index

	return max_index
end

local function AuxEmitIndex (MB, index)
	local indices, offset = MB[_indices] or {}, MB[_offset] -- if first emission, might not have indices

	indices[offset + 1] = index or NewIndex(MB)

	MB[_index_shape_count], MB[_indices], MB[_offset], MB[_shape] = MB[_index_shape_count] + 1, indices, offset + 1, "indices"
end

local function DegenerateIndex (MB)
	local indices, offset = MB[_indices] or {}, MB[_offset] -- if first emission, might not have indices

	if offset ~= MB[_base_offset] then -- any index available?
		return indices[offset]
	else -- no: make a dummy and refer to it
		local index = NewIndex(MB)

		indices[offset + 1], MB[_indices], MB[_offset] = index, indices, offset + 1

		return index
	end
end

--- DOCME
function MeshBuilder:EmitDegenerateIndex ()
	AuxEmitIndex(self, DegenerateIndex(self))
end

local function SetDegenerateIndices (MB)
	local index = DegenerateIndex(MB)

	MB:SetUpperLeft(index)
	MB:SetUpperRight(index)
	MB:SetLowerLeft(index)
	MB:SetUpperRight(index)
end

--- DOCME
function MeshBuilder:EmitDegenerateQuad ()
	SetDegenerateIndices(self)

	self:EmitQuad()
end

--- DOCME
function MeshBuilder:EmitDegenerateTriangle ()
	SetDegenerateIndices(self)

	self:EmitTriangle()
end

--- DOCME
function MeshBuilder:EmitIndex (index)
	AuxEmitIndex(self, index)
end

local function PrepareToAdvance (MB, ul_offset)
	MB:ClearAboveFlag()
	MB:ClearLeftFlag()
	MB:SetUpperLeft(nil)
	MB:SetUpperRight(nil)
	MB:SetLowerLeft(nil)
	MB:SetLowerRight(nil)

	local col = MB[_cur_column]

	if col then
		local above, ncols = MB[_above], MB[_ncols]
		local is_first_row = above[ncols] == "first_row" -- cf. Reset() method; n.b. read before written...

		above[col] = ul_offset	-- ...so that not stomped here, on final column;
								-- setting that one indicates the first row is done;
								-- the offset is left behind for the next row...

		if col == ncols then
			col = 0 -- increment back to 1
		end

		ul_offset = not is_first_row and above[col + 1] -- ...and read back here

		if ul_offset then
			local indices = MB[_indices]

			MB:SetAboveFlag()
			MB:SetUpperLeft(indices[ul_offset])
			MB:SetUpperRight(indices[ul_offset + 1])
		end

		MB[_cur_column] = col + 1
	end

	return col == 0 and "newline"
end

--- DOCME
function MeshBuilder:EmitQuad ()
	assert(self[_shape] ~= "indices", "Index shape in progress")

	local indices, offset = self[_indices] or {}, self[_offset] -- if first emission, might not have indices
	local ul = self[_upper_left] or NewIndex(self)
	local ur = self[_upper_right] or NewIndex(self)
	local ll = self[_lower_left] or NewIndex(self)
	local lr = self[_lower_right] or NewIndex(self)

	_AddQuadIndices_(indices, ul, ur, ll, lr, offset)

	if PrepareToAdvance(self, offset + 5) ~= "newline" then -- offset of lower-left, cf. AddQuadIndices()
		self:SetLeftFlag()
		self:SetUpperLeft(ur) -- n.b. often redundant
		self:SetLowerLeft(lr)
	end

	self[_index_shape_count], self[_indices], self[_offset], self[_shape]  = 0, indices, offset + 6, "quad"
end

--- DOCME
function MeshBuilder:EmitTriangle ()
	assert(self[_shape] ~= "indices", "Index shape in progress")

	local above_flag, indices, left_flag, offset, i1, i2, i3 = self[_above_flag], self[_indices] or {}, self[_left_flag], self[_offset] -- if first emission, might not have indices
	local ul = self[_upper_left]
	local ur = self[_upper_right]
	local ll = self[_lower_left]
	local lr = self[_lower_right]

	if above_flag then
		-- side above, so triangle will be flush with it in forms:
		--
		-- x--x     x--x
		--  \ | and | /
		--   \|     |/
		--    x     x
		--
		-- the triangle on the right will also be flush with a side on the left
		i1, i2 = ul, ur

		if left_flag then -- n.b. ll or lr might be nil, so use "if"
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
	i2 = i2 or NewIndex(self)	-- n.b. this call is sequenced...
	i3 = i3 or NewIndex(self)	-- ...before this one to ensure i2 is placed before i3, so that we can put
								-- i2's offset into the "above" list and recover both

	_AddTriangleIndices_(indices, i1, i2, i3, offset)

	if PrepareToAdvance(self, not above_flag and offset + 2) ~= "newline" and not left_flag then -- offset of lower-left, in non-"above" case
		self:SetLeftFlag()

		if above_flag then -- i2 = ur, i3 = lr
			self:SetUpperLeft(i2) -- n.b. often redundant
			self:SetLowerLeft(i3)
		else -- i2 = ll, i3 = ur
			self:SetUpperLeft(i3) -- n.b. often redundant
			self:SetLowerLeft(i2)
		end
	end

	self[_index_shape_count], self[_indices], self[_offset], self[_shape]  = 0, indices, offset + 3, "tri"
end

--- DOCME
function MeshBuilder:FinishIndexShape ()
	local shape = self[_shape]

	assert(shape == "indices", "Index shape not in progress")

	PrepareToAdvance(self, nil)

	self[_shape] = "none" -- invalidate so other shapes can proceed, but leave count for querying
end

--- DOCME
function MeshBuilder:GetLastIndices (how)
	local indices, shape, offset, count = self[_indices], self[_shape], self[_offset]

	if shape == "quad" then
		if how == "raw" then
			count = 6
		else -- cf. AddQuadIndices()
			return indices[offset - 5], unpack(indices, offset - 2, offset)
		end
	elseif shape == "tri" then
		count = 3
	else
		count = self[_index_shape_count] -- cf. note in FinishIndexShape() method
	end

	return unpack(indices, offset - count + 1, offset)
end

--- DOCME
function MeshBuilder:GetLastShape ()
	local shape = self[_shape]

	if shape == "indices" then
		return "indices", self[_index_shape_count]
	else
		return shape
	end
end

--- DOCME
function MeshBuilder:GetResult ()
	local out, offset = self[_indices], self[_offset]

	self:Reset() -- n.b. offset stays intact

	if offset ~= self[_base_offset] then
		return out, offset
	else
		return out, 0 -- indices table may be present but unwritten
	end
end

--- DOCME
function MeshBuilder:Reset (max_index)
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
function MeshBuilder:SetAboveFlag ()
	self[_above_flag] = true
end

--- DOCME
function MeshBuilder:SetIndices (indices)
	assert(self[_offset] == self[_base_offset], "Building in progress")

	self[_indices] = indices
end

--- DOCME
function MeshBuilder:SetLeftFlag ()
	self[_left_flag] = true
end

--- DOCME
function MeshBuilder:SetLowerLeft (index)
	self[_lower_left] = index
end

--- DOCME
function MeshBuilder:SetLowerRight (index)
	self[_lower_right] = index
end

--- DOCME
function MeshBuilder:SetMaxIndex (max_index)
	assert(self[_offset] == self[_base_offset], "Building in progress")

	self[_max_index] = max_index or 0
end

--- DOCME
function MeshBuilder:SetOffset (offset)
	assert(self[_offset] == self[_base_offset], "Building in progress")

	self[_offset] = offset or 0
end

--- DOCME
function MeshBuilder:SetUpperLeft (index)
	self[_upper_left] = index
end

--- DOCME
function MeshBuilder:SetUpperRight (index)
	self[_upper_right] = index
end

--- DOCME
function MeshBuilder:Skip ()
	PrepareToAdvance(self, nil)

	self[_shape], self[_index_shape_count] = "none", 0
end

local function NewBuilder (ncols)
	local mb = setmetatable({ [_base_offset] = 0 }, MeshBuilder)

	if ncols then
		mb[_above], mb[_ncols] = {}, ncols
	end

	mb:Reset()

	return mb
end

--- DOCME
function MM.NewLatticeBuilder (ncols)
	assert(ncols and ncols > 0, "Invalid column count")

	return NewBuilder(ncols)
end

--- DOCME
MM.NewSequenceBuilder = NewBuilder

M._MM = MM
















































_AddQuadIndices_ = M.AddQuadIndices
_AddTriangleIndices_ = M.AddTriangleIndices

return M