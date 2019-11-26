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
		local iminus1 = index - 1
		local base = iminus1 * 4

		slots[index], first = true, 2 * base + 1 -- two coordinates per vertex

		local bottom = base + (self.m_jump or 2)

		_AddQuadIndices_(self.m_indices, base + 1, base + 2, bottom + 1, bottom + 2, iminus1 * 6)

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
			qs.m_cur_column, qs.m_cols = 1, mode
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

_AddQuadIndices_ = M.AddQuadIndices
_AddTriangleIndices_ = M.AddTriangleIndices

return M