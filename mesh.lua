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
local abs = math.abs
local assert = assert
local cos = math.cos
local modf = math.modf
local pairs = pairs
local pi = math.pi
local random = math.random
local sin = math.sin
local sqrt = math.sqrt

-- Exports --
local M = {}

-- Cached module references --
local _AddQuadIndices_
local _AddTriangleIndices_
local _AddVertex_
local _NewQuadrantArc_
local _NewQuadrantRing_

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

local function DefBeginRow () end

local DummyMap = M -- has no numerical keys

local function LatticeUVsAndVertices (ncols, nrows, w, h, opts)
	local add, begin_row, index_map, uvs, vertices = _AddVertex_, DefBeginRow, DummyMap
	local x, y, dw, dh = -w / 2, -h / 2, w / ncols, h / nrows
	local ri, stride, context = 1, ncols + 1

	if opts then
		add, begin_row, index_map = opts.add_vertex or add, opts.begin_row or begin_row, opts.index_map or index_map
		context, uvs, vertices = opts.context, opts.uvs, opts.vertices
		ri, stride = (opts.index or ri) - 1, opts.stride or stride
	end

	uvs, vertices = uvs or {}, vertices or {}

	for row = 1, nrows + 1 do
		local index = ri + 1

		begin_row(row, index)

		for j = 1, ncols + 1 do
			if index_map[index] == nil then -- allow false
				add(uvs, vertices, x + (j - 1) * dw, y, w, h)
			end

			index = index + 1
		end

		y, ri = y + dh, ri + stride
	end

	return uvs, vertices
end

local function AddCell (indices, index, stride)
	_AddQuadIndices_(indices, index, index + 1, index + stride, index + stride + 1)

	return index + 1
end

local function ForEach (indices, base, stride, n, index_map)
	for offset = 0, n - 1 do
		AddCell(indices, base + offset, stride, index_map)
	end

	return base + n
end

local function BasicLattice (indices, ncols, nrows, index, stride, index_map)
	for _ = 1, nrows do
		index = ForEach(indices, index, stride, ncols, index_map) + 1
	end
end

local function ForEachDo (indices, base, stride, n, index_map, func, arg)
	for offset = 0, n - 1 do
		local index = base + offset

		AddCell(indices, index, stride, index_map)
		func(arg, index)
	end

	return base + n
end

local function AddToInterior (interior, index)
	interior[#interior + 1] = index
end

local function LatticeWithInterior (indices, ncols, nrows, index, stride, index_map)
	local interior = {}

	index = ForEach(indices, index, stride, ncols, index_map) - 1 -- first row

	for _ = 2, nrows - 1 do
		AddCell(indices, index + 1, stride, index_map) -- interior row, but first column

		index = ForEachDo(indices, index + 2, stride, ncols - 2, index_map, AddToInterior, interior) -- interior cells

		AddCell(indices, index, stride, index_map) -- interior row, but last column
	end

	return ForEach(indices, index, stride, ncols, index_map) -- last row
end

--- DOCME
function M.NewLattice (ncols, nrows, w, h, opts)
	local indices, op, index = {}, BasicLattice, 1

	if opts then
		if opts.has_interior then
			op = LatticeWithInterior
		end

		index = opts.index or index
	end

	local uvs, vertices = LatticeUVsAndVertices(ncols, nrows, w, h, opts)

	op(indices, ncols, nrows, index, ncols + 1, opts and opts.index_map)

	return uvs, vertices, indices
end

local ArcN = 100

local CosSin = (function(n)
	local cs, da, cosa, sina = { 1, 0 }, pi / (2 * n), 1, 0
	local cosda, sinda = cos(da), sin(da)

	-- a = cos(A), b = sin(A), c = cos(dA), d = sin(dA)
	-- P1 = ac, P2 = bd, P3 = (a + b)(c + d)
	local c_plus_d = cosda + sinda

	for _ = 1, n - 1 do
		local P1, P2, P3 = cosa * cosda, sina * sinda, (cosa + sina) * c_plus_d

		cosa, sina = P1 - P2, P3 - P2 - P1
		cs[#cs + 1] = cosa
		cs[#cs + 1] = sina
	end

	cs[#cs + 1] = 0
	cs[#cs + 1] = 1

	return cs
end)(ArcN)

local UVar, VVar

local function Displacement (fx, fy)
	local du = UVar and (2 * random() - 1) * UVar or 0
	local dv = VVar and (2 * random() - 1) * VVar or 0

	return fx * du - fy * dv, fy * du + fx * dv
end

local function DisplaceFromPrevious (ex, ey, prevx, prevy)
	if UVar or VVar then
		local fx, fy = ex - prevx, ey - prevy
		local len_sq = fx^2 + fy^2

		if 1 + len_sq ~= 1 then
			local len = sqrt(len_sq)
			local dx, dy = Displacement(fx / len, fy / len)

			return ex + dx, ey + dy
		end
	end

	return ex, ey
end

local Ox, Oy, Px, Py, Qx, Qy
local PrevRow, CurRow = {}, {}
local Index, RI, VI

local function CheckIndex (index_map, di)
	local mindex = index_map[Index]

	CurRow[RI], RI, Index, VI = mindex or Index, RI + 1, Index + di, VI + 2 * di -- n.b. mindex == false will load Index

	return mindex == nil -- allow false
end

local function Add (uvs, verts, u, v, x, y)
	uvs[VI - 1], uvs[VI] = u, v
	verts[VI - 1], verts[VI] = x, y
end

local function PosOnCurve (pos, vx, vy, wx, wy)
	local offset, t = modf(pos) -- offset in [0, ArcN), frac in [0, 1)
	local csi, s = 2 * offset + 1, 1 - t
	local c1, c2 = CosSin[csi + 0], CosSin[csi + 2]
	local s1, s2 = CosSin[csi + 1], CosSin[csi + 3]
	local ca, sa = s * c1 + t * c2, s * s1 + t * s2

	return Ox + ca * vx + sa * wx, Oy + ca * vy + sa * wy
end

local UA, UB, V1, V2 -- A and B nomenclature as these (potentially) vary with v

local function CircularArc (index, ncurve, uvs, verts, index_map, di) -- in circles, arc length is proportional to angle
	Index, RI, VI = index, 1, 2 * (index - 1)

	if CheckIndex(index_map, di) then -- first point
		Add(uvs, verts, UA, V1, Px, Py)
	end

	local cpos, upos, vpos, dc, du, dv = 0, UA, V1, ArcN / ncurve, (UB - UA) / ncurve, (V2 - V1) / ncurve
	local vx, vy, wx, wy, prevx, prevy = Px - Ox, Py - Oy, Qx - Ox, Qy - Oy, Px, Py

	for _ = 1, ncurve - 1 do -- interior
		cpos, upos, vpos = cpos + dc, upos + du, vpos + dv

		local ex, ey = PosOnCurve(cpos, vx, vy, wx, wy)

		if CheckIndex(index_map, di) then
			Add(uvs, verts, upos, vpos, DisplaceFromPrevious(ex, ey, prevx, prevy))
		end

		prevx, prevy = ex, ey
	end

	if CheckIndex(index_map, di) then -- last point
		Add(uvs, verts, UB, V2, Qx, Qy)
	end
end

local Arc = {}

local function EllipticalArc (index, ncurve, uvs, verts, index_map, di) -- in ellipses, arc length-to-angle is rather complex
	--
end

local function GetMeshStructure (params)
	return params.index_map or DummyMap, params.indices or {}, params.uvs or {}, params.vertices or {}
end

local NubMap, NubParams = {}, {}

local function AuxResolveNubCoreDeltas (inner, outer, name, nradius)
	if inner then
		return inner / outer
	else
		local frac = 1 / (nradius + 1)

		NubParams[name] = outer * frac

		return frac
	end
end

local function ResolveNubCoreDeltas (params, kind)
	local nradius, xinner, xouter, yinner, youter = params.nradius -- inner are optional, outer required

	if params.inner_radius then
		xinner = params.inner_radius
		yinner = xinner
	else
		xinner = params.inner_x_radius
		yinner = params.inner_y_radius
	end

	if params.outer_radius then
		xouter = params.outer_radius
		youter = xouter
	else
		xouter = params.outer_x_radius
		youter = params.outer_y_radius
	end

	local xfrac = AuxResolveNubCoreDeltas(xinner, xouter, "inner_x_radius", nradius)
	local yfrac = AuxResolveNubCoreDeltas(yinner, youter, "inner_y_radius", nradius)

	if kind == "upper_left" or kind == "lower_right" then
		return xfrac, yfrac
	else
		return yfrac, xfrac
	end
end

local function ScrubAndCopy (into, from)
	for k in pairs(into) do
		into[k] = nil
	end

	for k, v in pairs(from) do
		into[k] = v
	end
end

local function NubRing (params, k1, k2, fx, fy, v2)
	v2 = params.v2 or v2

	local index, ncurve, nradius = params.index or 1, params.ncurve, params.nradius
	local u1, u2, v1 = params.u1 or 0, params.u2 or 1, params.v1 or 1 - v2
	local umid, stride = (u1 + u2) / 2, ncurve + 1
	local final = index + (nradius + 1) * stride

	NubParams.index, NubParams.ncurve, NubParams.stride = index, ncurve / 2, stride

	assert(ncurve > 0 and ncurve % 2 == 0, "Curve quantization must be even integer")

	local ufrac, vfrac = ResolveNubCoreDeltas(params, k1)
	local du, vmid = ufrac * abs(u1 - umid), (1 - vfrac) * v2 + vfrac * v1

	NubParams.u1a, NubParams.u1b, NubParams.u2a, NubParams.u2b = u1, umid, umid - du, umid
	NubParams.v1, NubParams.v2a, NubParams.v2b, NubParams.kind = v2, v1, vmid, k1

	local uvs, verts, indices = _NewQuadrantRing_(NubParams)

	index = index + NubParams.ncurve
	NubParams.index, NubParams.indices, NubParams.uvs, NubParams.vertices = index, indices, uvs, verts

	for _ = 1, nradius do
		local vi, dx, dy = 2 * index - 1, Displacement(fx, fy)

		verts[vi], verts[vi + 1], vi = verts[vi] + dx, verts[vi + 1] + dy, vi + 2
		NubMap[index], index = false, index + stride -- false = shadow this index as itself
	end

	final = NubMap[final] or final

	for i = 1, NubParams.ncurve do
		_AddTriangleIndices_(indices, CurRow[i], CurRow[i + 1], final)
	end

	NubParams.u1a, NubParams.u1b, NubParams.u2a, NubParams.u2b = umid, u2, umid, umid + du
	NubParams.v1a, NubParams.v1b, NubParams.v2, NubParams.kind = v1, vmid, v2, k2
	NubParams.v2a, NubParams.v2b = nil

	_NewQuadrantRing_(NubParams)

	for i = 1, NubParams.ncurve do
		_AddTriangleIndices_(indices, CurRow[i], CurRow[i + 1], final)
	end

	if CheckIndex(NubMap, 1) then
		Add(uvs, verts, umid, v2, NubParams.x or 0, NubParams.y or 0)
	end

	return uvs, verts, indices
end

--- DOCME
function M.NewNub (params)
	ScrubAndCopy(NubMap, params.index_map or NubMap) -- if absent, "copy" from self once scrubbed
	ScrubAndCopy(NubParams, params)

	NubParams.index_map = NubMap

	local kind = params.kind

	if kind == "top" then
		return NubRing(params, "upper_left", "upper_right", 1, 0, 0)
	elseif kind == "left" then
		return NubRing(params, "lower_left", "upper_left", 0, -1, 0)
	elseif kind == "right" then
		return NubRing(params, "upper_right", "lower_right", 0, 1, 1)
	else
		return NubRing(params, "lower_right", "lower_left", -1, 0, 1)
	end
end

--- DOCME
function M.NewQuadrantArc (params)
	-- TODO!
end

local function GetRingParams (params)
	local xinner, xouter, yinner, youter

	if params.inner_radius then
		xinner = params.inner_radius
		yinner = xinner
	else
		xinner = params.inner_x_radius
		yinner = params.inner_y_radius
	end

	if params.outer_radius then
		xouter = params.outer_radius
		youter = xouter
	else
		xouter = params.outer_x_radius
		youter = params.outer_y_radius
	end

	assert(xouter > xinner)
	assert(youter > yinner)

	local afunc = (xinner == yinner and xouter == youter) and CircularArc or EllipticalArc

	return afunc, xinner, xouter, yinner, youter
end

local function GetUDomain (params)
	local u1a, u2a = params.u1a or params.u1 or 0, params.u2a or params.u2 or 1 -- n.b. assumed to be symmetric at edges
	local u1b, u2b = params.u1b or params.u1 or 0, params.u2b or params.u2 or 1

	return u1a, u2a, u1b, u2b
end

--- DOCME
function M.NewQuadrantRing (params)
	local kind, afunc, xinner, xouter, yinner, youter = params.kind, GetRingParams(params)
	local v1b, v2b, u1a, u2a, u1b, u2b = params.v1b, params.v2b, GetUDomain(params)

	UA, UB, UVar, VVar = u1a, u1b, params.uvar, params.vvar

	if kind == "upper_left" or kind == "lower_right" then
		V1, V2 = params.v1a or params.v1 or 0, params.v2a or params.v2 or 1
	else
		V1, V2 = params.v1a or params.v1 or 1, params.v2a or params.v2 or 0
	end

	local ncurve, nradius = params.ncurve or 1, params.nradius or 1
	local xthickness, ythickness, dpx, dpy, dqx, dqy = xouter - xinner, youter - yinner

	assert(ncurve >= 1, "Curve quantization must be positive integer")
	assert(nradius >= 0, "Radial quantization must be non-negative integer")

	Ox, Oy = params.x or 0, params.y or 0

	if kind == "upper_left" then
		Px, Py, dpx, dpy = Ox - xouter, Oy, xthickness / nradius, 0
		Qx, Qy, dqx, dqy = Ox, Oy - youter, 0, ythickness / nradius
	elseif kind == "upper_right" then
		Px, Py, dpx, dpy = Ox, Oy - youter, 0, ythickness / nradius
		Qx, Qy, dqx, dqy = Ox + xouter, Oy, -xthickness / nradius, 0
	elseif kind == "lower_right" then
		Px, Py, dpx, dpy = Ox + xouter, Oy, -xthickness / nradius, 0
		Qx, Qy, dqx, dqy = Ox, Oy + youter, 0, -ythickness / nradius
	else
		Px, Py, dpx, dpy = Ox, Oy + youter, 0, -ythickness / nradius
		Qx, Qy, dqx, dqy = Ox - xouter, Oy, xthickness / nradius, 0
	end

	local index, dir, stride = params.index or 1, params.dir or 1, params.stride or ncurve + 1
	local index_map, indices, uvs, verts = GetMeshStructure(params)

	afunc(index, ncurve, uvs, verts, index_map, dir)

	local dua, dub = (u2a - u1a) / nradius, (u2b - u1b) / nradius -- ignored if nradius = 0
	local dv1, dv2 = v1b and (v1b - V1) / nradius or 0, v2b and (v2b - V2) / nradius or 0 -- ditto

	for _ = 1, nradius do
		Px, Py, Qx, Qy, PrevRow, CurRow, index = Px + dpx, Py + dpy, Qx + dqx, Qy + dqy, CurRow, PrevRow, index + stride
		UA, UB, V1, V2 = UA + dua, UB + dub, V1 + dv1, V2 + dv2

		afunc(index, ncurve, uvs, verts, index_map, dir)

		for i = 1, ncurve do
			_AddQuadIndices_(indices, PrevRow[i], PrevRow[i + 1], CurRow[i], CurRow[i + 1])
		end
	end

	return uvs, verts, indices
end

--- DOCME
function M.NewTJunction (params)
	-- TODO!
	-- two NewQuadrantArc()s
	-- one rect (not necessarily .5 of the way, though)
end

-- Cache module members.
_AddQuadIndices_ = M.AddQuadIndices
_AddTriangleIndices_ = M.AddTriangleIndices
_AddVertex_ = M.AddVertex
_NewQuadrantArc_ = M.NewQuadrantArc
_NewQuadrantRing_ = M.NewQuadrantRing

-- Export the module.
return M