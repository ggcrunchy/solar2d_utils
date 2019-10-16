--- Various mesh-related operations.

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

-- Modules --
local embedded_free_list = require("tektite_core.array.embedded_free_list")

-- Cached module references --
local _AddQuadIndices_
local _AddTriangleIndices_
local _AddVertex_

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

--- DOCMEMORE
-- Add the indices as a triangle
function M.AddTriangleIndices (indices, i1, i2, i3, base)
	base = base or #indices

	indices[base + 1] = i1
	indices[base + 2] = i2
	indices[base + 3] = i3
end

--- DOCME
-- TODO: better name?
function M.AddVertex (uvs, vertices, x, y, w, h)
	uvs[#uvs + 1] = x / w + .5
	uvs[#uvs + 1] = y / h + .5

	vertices[#vertices + 1] = x
	vertices[#vertices + 1] = y
end

local QuadStream = {}

QuadStream.__index = QuadStream

--- DOCME
function QuadStream:GetIndices ()
	return self.m_indices
end

--- DOCME
-- @treturn ?|uint|nil
function QuadStream:Insert ()
	local slots, first = self.m_slots
	local index, free = embedded_free_list.GetInsertIndex(slots, self.m_free)

	if index <= self.m_capacity then
		local iminus1 = index - 1
		local base = iminus1 * 4

		slots[index], first = true, 2 * base + 1 -- two coordinates per vertex

		if self.m_index_mode then
			_AddQuadIndices_(self.m_indices, base + 1, base + 2, base + 3, base + 4, iminus1 * 6)
		end

		self.m_free = free
	end

	return first
end

--- DOCME
-- @uint first
function QuadStream:Remove (first)
	local qoffset = .125 * (first - 1) -- quad = four corners, two coordinates each
	local slots, removed = self.m_slots, false

	if embedded_free_list.InUse(slots, qoffset + 1) then
		self.m_free = embedded_free_list.RemoveAt(slots, qoffset + 1, self.m_free)

		if self.m_index_mode then
			local index = 2 * qoffset + 1

			_AddQuadIndices_(self.m_indices, index, index, index, index, qoffset * 6 + 1) -- make degenerate
		end

		removed = true
	end

	return removed
end

--- DOCME
-- @uint n
-- @string[opt="vertex"] degen
function M.NewQuadStream (n, degen)
	local stream, indices = { m_capacity = n, m_slots = {} }, {}

	if degen == "index" then
		local final = n * 4 -- final legal index, will come into use if and when stream saturated

		for i = 1, 6 * n do
			indices[i] = final -- begin with all degenerate
		end

		stream.m_index_mode = true
	else
		local offset = 0

		for _ = 1, n do
			_AddQuadIndices_(indices, offset + 1, offset + 2, offset + 3, offset + 4)

			offset = offset + 4
		end
	end

	stream.m_indices = indices

	return setmetatable(stream, QuadStream)
end

local TriangleStream = {}

--- DOCME
-- @treturn ?|uint|nil
function TriangleStream:Insert ()
	--
end

--- DOCME
-- @uint index
function TriangleStream:Remove (first)
	--
end

--- DOCME
function M.NewTriangleStream (n)
	--
end

_AddQuadIndices_ = M.AddQuadIndices
_AddTriangleIndices_ = M.AddTriangleIndices
_AddVertex_ = M.AddVertex

return M