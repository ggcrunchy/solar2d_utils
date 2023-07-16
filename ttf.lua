--- Truetype utilities.

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
local min = math.min
local open = io.open
local pairs = pairs
local setmetatable = setmetatable

-- Extension imports --
local hypot = math.hypot

-- Plugins --
local AssetReader = require("plugin.AssetReader")
local libtess2 = require("plugin.libtess2")
local truetype = require("plugin.truetype")
local utf8 = require("plugin.utf8")

-- Solar2D globals --
local display = display
local system = system

-- Exports --
local M = {}

--
--
--

local Tess = libtess2.NewTess()

Tess:SetOption("CONSTRAINED_DELAUNAY_TRIANGULATION", true)

--
--
--

local function AuxAddContourAndGetMin (points, minx, miny)
  for i = 1, #points, 2 do
    minx, miny = min(minx, points[i]), min(miny, points[i + 1])
  end

  Tess:AddContour(points)

  return minx, miny
end

--
--
--

local function MakeAddContour (body)
  return function(arg)
    local points = arg.points

    if points and #points > 0 then
      body(points, arg)

      arg.points = nil
    end
  end
end

--
--
--

local AddContourAndGetMin = MakeAddContour(function(points, arg)
  arg.minx, arg.miny = AuxAddContourAndGetMin(points, arg.minx, arg.miny)
end)

--
--
--

--
--
--

local function AddTriangle (out, elems, verts, base, offset)
  for _ = 1, 3 do
    local index = elems[base]

    out[offset + 1] = verts[index * 2 + 1]
    out[offset + 2] = verts[index * 2 + 2]
    base, offset = base + 1, offset + 2
  end

  return base, offset
end

--
--
--

local function DrawMesh (arg)
  local flattened, base, offset = {}, 1, 0
  local elems, verts = Tess:GetElements(), Tess:GetVertices()

  for _ = 1, Tess:GetElementCount() do
    base, offset = AddTriangle(flattened, elems, verts, base, offset)
  end

  local mesh = display.newMesh{ vertices = flattened }

  mesh.anchorX, mesh.x = 0, arg.minx
  mesh.anchorY, mesh.y = 0, arg.miny

  return mesh
end

--
--
--

local Samples, Length = {}

local function AddSample (j, x, y)
  if j > 1 then
    Length = Length + hypot(x - Samples[j - 3], y - Samples[j - 2])
  else
    Length = 0
  end
  
  Samples[j], Samples[j + 1], Samples[j + 2] = x, y, Length

  return j + 3
end

local function Fit (s)
  local j = 4

  while true do -- s is fraction of full length, so will terminate
    local len2 = Samples[j + 2]

    if s < len2 then
      local x2, y2 = Samples[j], Samples[j + 1]
      local x1, y1, len1 = Samples[j - 3], Samples[j - 2], Samples[j - 1]
      local t = (s - len1) / (len2 - len1)

      return x1 + t * (x2 - x1), y1 + t * (y2 - y1)
    end

    j = j + 3
  end
end

local function GetLength (j)
  return Samples[j - 1]
end

--
--
--

local function AuxCubic ()
  -- TODO!
end

--
--
--

local SampleCount = 30

local function AuxQuadratic (add_point, spacing, prevx, prevy, scale, x, y, cx, cy, arg)
  local j = AddSample(1, prevx, prevy)

  for i = 1, SampleCount do
    local t = i / SampleCount
    local s = 1 - t
    local a, b, c = s * s, 2 * s * t, t * t

    j = AddSample(j, a * prevx + b * cx + c * x, a * prevy + b * cy + c * y)
  end

  local s, len = 0, GetLength(j)

  while s < len do
    local qx, qy = Fit(s)

    add_point(qx * scale, -qy * scale, arg)

    s = s + spacing
  end
end

--
--
--

local function AuxLine (arg, add_point, _, prevx, prevy, scale)
  add_point(prevx * scale, -prevy * scale, arg)
end

--
--
--

local function MakeContourListener (params)
  local add_point, finish, move, start = params.add_point, params.finish, params.move, params.start
  local spacing = params.spacing or 30

  return function(shape, xpos, ypos, scale, arg)
    local prevx, prevy

    if start then
      start(arg)
    end

    for i = 1, #(shape or "") do -- ignore non-shapes such as spaces
      local what, x, y, cx, cy, cx1, cy1 = shape:GetVertex(i)

      if what == "line_to" then
        AuxLine(arg, add_point, spacing, prevx, prevy, scale, x, y)
      elseif what == "curve_to" then
        AuxQuadratic(add_point, spacing, prevx, prevy, scale, x, y, cx, cy, arg)
      elseif what == "cubic_to" then
        AuxCubic(add_point, spacing, prevx, prevy, scale, x, y, cx, cy, cx1, cy1, arg)
      else
        move(x * scale, -y * scale, arg)
      end

      prevx, prevy = x, y
    end

    if finish then
      finish(arg, xpos, ypos)
    end
  end
end

--
--
--

local DoContours = MakeContourListener{
  add_point = function(x, y, arg)
    local points = arg.points

    points[#points + 1] = x
    points[#points + 1] = y
  end,

  finish = function(arg, xpos, ypos)
    arg:try_to_add_contour()

    if Tess:Tesselate("POSITIVE", "POLYGONS", 3) then
      local mesh = arg:draw()

      mesh:translate(xpos, ypos)
      arg.group:insert(mesh)
    end
  end,

  move = function(_, _, arg)
    arg:try_to_add_contour()

    arg.points = arg.points or {}
  end,

  start = function(arg)
    arg.minx, arg.miny = 1 / 0, 1 / 0
  end
}

--
--
--

local LineState = {}

local function BuildLineState (text, font, scale, current)
  local count, cp_prev = 0

  for _, cp_cur in utf8.codes(text) do
    if cp_prev then
        current = current + scale * font:GetCodepointKernAdvance(cp_prev, cp_cur)
    end
  
    LineState[count + 1] = cp_cur
    LineState[count + 2] = current
    count = count + 2

    --
    --
    --

    local advance, _ = font:GetCodepointHMetrics(cp_cur)

    current, cp_prev = current + advance * scale, cp_cur
  end

  return count
end

--
--
--

local function MakeLineStateFunc (func)
  return function(params)
    local font, ypos, listener = params.font, params.baseline, params.listener
    local scale, arg = params.scale or 1, params.arg

    for i = 1, BuildLineState(params.text, font, scale, params.current), 2 do
      func(font, scale, LineState[i], LineState[i + 1], ypos, listener, arg)
    end
  end
end

--
--
--

local AuxShapesLine = MakeLineStateFunc(function(font, scale, cp_cur, xpos, ypos, listener, arg)
  local ok, x0, y0, _ = font:GetCodepointBox(cp_cur)

  if ok then
    listener(font:GetCodepointShape(cp_cur), xpos + scale * x0, ypos + scale * y0, scale, arg)
  end
end)

--
--
--

--- DOCME
function M.ShapesLine (group, text, font, height, opts)
  local arg = { group = group, draw = DrawMesh, try_to_add_contour = AddContourAndGetMin }

  arg.on_item = opts and opts.on_item

  AuxShapesLine{
    text = text, font = font,
    scale = font:ScaleForPixelHeight(height) * (opts and opts.scale or 1),
    current = opts and opts.current or 0, baseline = opts and opts.baseline or 0,
    listener = DoContours, arg = arg
  }
end

--
--
--

local LoadedFonts = setmetatable({}, { mode = "v" })

--
--
--

local Platform = system.getInfo("environment") == "device" and system.getInfo("platform")

--- DOCME
function M.GetFont (filename, dir)
  dir = dir or system.ResourceDirectory

  --
  --
  --

  for info, data in pairs(LoadedFonts) do
    if info.filename == filename and info.dir == dir then
      return data
    end
  end

  --
  --
  --

  local contents

  if Platform == "android" and dir == system.ResourceDirectory then
    contents = AssetReader.Read(filename)
  else
    local file = open(system.pathForFile(filename, dir), "rb")

    if file then
      contents = file:read("*a")

      file:close()
    end
  end

  --
  --
  --

  if contents then
    local info = { contents = contents, filename = filename, dir = dir }
    local font = truetype.InitFont(contents)

    LoadedFonts[info] = font

    return font
  else
    return nil
  end
end

--
--
--

return M