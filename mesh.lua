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
local assert = assert

-- Exports --
local M = {}

-- Cached module references --
local _AddQuadIndices_
local _AddTriangleIndices_
local _AddVertex_

--
--
--

--- DOCMEMORE
-- Add the indices in quad-sized blocks (0, 2, 1; 1, 2, 3)
function M.AddQuadIndices (indices, i1, i2, i3, i4)
	_AddTriangleIndices_(indices, i1, i3, i2)
	_AddTriangleIndices_(indices, i2, i3, i4)
end

--- DOCMEMORE
-- Add the indices as a triangle
function M.AddTriangleIndices (indices, i1, i2, i3)
	indices[#indices + 1] = i1
	indices[#indices + 1] = i2
	indices[#indices + 1] = i3
end

--- DOCME
function M.AddVertex (uvs, vertices, x, y, w, h)
	uvs[#uvs + 1] = x / w + .5
	uvs[#uvs + 1] = y / h + .5

	vertices[#vertices + 1] = x
	vertices[#vertices + 1] = y
end

--- DOCME
function M.NewCross ()
	-- TODO!
end

local function LatticeUVsAndVertices (ncols, nrows, w, h, opts)
	local add, uvs, vertices = _AddVertex_, {}, {}
	local x, y, dw, dh = -w / 2, -h / 2, w / ncols, h / nrows
	local ri, stride, context = 1, ncols + 1

	if opts then
		add, stride = opts.add_vertex or add, opts.stride or stride
		context, uvs, vertices = opts.context, opts.uvs, opts.vertices
		ri = (opts.index or ri) - 1
	end

	uvs, vertices = uvs or {}, vertices or {}

	for _ = 1, nrows + 1 do
		local index = ri + 1

		for j = 1, ncols + 1 do
			add(uvs, vertices, x + (j - 1) * dw, y, w, h)

			index = index + 1
		end

		y, ri = y + dh, ri + stride
	end

	return uvs, vertices
end

local function BasicLattice (indices, ncols, nrows, index, stride)
	for _ = 1, nrows do
		for _ = 1, ncols do
			_AddQuadIndices_(indices, index, index + 1, index + stride, index + stride + 1)

			index = index + 1
		end

		index = index + 1
	end
end

local function LatticeWithInterior (indices, ncols, nrows, index, stride)
	local interior = {}

	for _ = 1, ncols do -- first row
		_AddQuadIndices_(indices, index, index + 1, index + stride, index + stride + 1)

		index = index + 1
	end

	for _ = 2, nrows - 1 do
		_AddQuadIndices_(indices, index, index + 1, index + stride, index + stride + 1) -- interior row, but first column

		index = index + 1

		for _ = 1, ncols do -- interior cell
			_AddQuadIndices_(indices, index, index + 1, index + stride, index + stride + 1)

			interior[#interior + 1], index = index, index + 1
		end

		_AddQuadIndices_(indices, index, index + 1, index + stride, index + stride + 1) -- interior row, but last column

		index = index + 1
	end

	for _ = 1, ncols do -- last row
		_AddQuadIndices_(indices, index, index + 1, index + stride, index + stride + 1)

		index = index + 1
	end
end

--- DOCME
function M.NewLattice (ncols, nrows, w, h, opts)
	local indices, op, index = {}, BasicLattice, 1

	if opts then
		local style = opts.style

		if style == "interior" then
			op = LatticeWithInterior
		else
			--
		end

		index = opts.index or index
	end

	local uvs, vertices = LatticeUVsAndVertices(ncols, nrows, w, h, opts)

	op(indices, ncols, nrows, index, ncols + 1)

	return uvs, vertices, indices
end

--- DOCME
function M.NewNub ()
	-- TODO!
end

--- DOCME
function M.NewQuarterArc ()
	-- TODO!
end

--- DOCME
function M.NewQuarterArcInCorner ()
	-- TODO!
end

--- DOCME
function M.NewTJunction ()
	-- TODO!
end

-- Cache module members.
_AddQuadIndices_ = M.AddQuadIndices
_AddTriangleIndices_ = M.AddTriangleIndices
_AddVertex_ = M.AddVertex

-- Export the module.
return M