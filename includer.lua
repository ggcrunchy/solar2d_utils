--- MIRMAL!

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
local concat = table.concat
local ipairs = ipairs
local newproxy = newproxy
local pairs = pairs
local sort = table.sort
local type = type

-- Exports --
local M = {}

--
--
--

local function GetRelevantSnippet (list, branch)
	for _, snippet in ipairs(list.snippets) do
		local bvalue = snippet.branch

		if bvalue == branch or bvalue == "any" then
			return snippet
		end
	end
end

local Generation = 0

local function Visit (snippet)
	-- Say we require A, B, and C.
	--
	-- These might have their own dependencies, giving us something like:
	--
	--        x
	--        ^
	--        |
	--   x    C    x
	--   ^    ^    ^
	--   |    |    |
	--   A -> B -> C -> x
	--
	-- C is redundant, since B also requires it. Our list thus becomes: A -> B -> x.

	snippet.revisited = snippet.generation == Generation

	if not snippet.revisited then
		snippet.generation = Generation

		for _, requirement in ipairs(snippet.requirements) do
			Visit(requirement)
		end
	end
end

local function VisitRequirements (requirements, branch)
	Generation = Generation + 1

	for _, list in ipairs(requirements) do
		local tree = GetRelevantSnippet(list, branch)

		if tree then
			Visit(tree)
		end
	end
end

local function AddRequirement (snippet, requirement)
	assert(requirement.integrated) -- the other half of the DAG precondition, cf. MergeRequirements

	local requirements = snippet.requirements

	requirements[#requirements + 1] = requirement
end

local function MergeRequirements (list, requirements)
	for _, snippet in ipairs(list.snippets) do -- walk through kernel(s) or any
		assert(not snippet.integrated)  -- We need to stitch together topologically sorted
										-- code, so cache must remain a DAG.

		local branch = snippet.branch

		VisitRequirements(requirements, branch)

		for j = 1, #requirements do
			local tree = GetRelevantSnippet(requirements[j], branch)

			if tree and not tree.revisited then
				AddRequirement(snippet, tree)
			end
		end
	end
end

local function CheckType (var, what, expected)
	local vtype = type(var)

	if vtype ~= expected then
		assert(false, "Bad " .. what .. ": expected " .. expected .. ", got " .. vtype)
	end
end

local function AddToList (list, code, branch)
	CheckType(code, "code", "string")

	if #code > 0 then
		local snippets = list.snippets

		snippets[#snippets + 1] = { branch = branch, code = code, generation = 0, requirements = {} }
	else
		assert(false, "Empty string supplied for '" .. branch .. "' code")
	end
end

local function GetKernels (list, vert, frag)
	if vert then
		AddToList(list, vert, "vertex")
	else
		assert(frag, "Snippet provided no code")
	end

	if frag then
		AddToList(list, frag, "fragment")
	end
end

local function GetCode (list, params)
	local vert, frag, any = params.vertex, params.fragment, params.any

	if any then
		assert(not (vert or frag), "The 'any' and vertex / fragment options are mutually exclusive")
	
		AddToList(list, any, "any")
	else
		GetKernels(list, vert, frag)
	end
end

local Lists = setmetatable({}, { __mode = "k" })

local function GetRequirement (requirements, id, which)
	local list = Lists[id]

	if not list then
		assert(false, "Requirement '" .. which .. "' not found")
	elseif list.added ~= Generation then -- guard against duplicates
		requirements[#requirements + 1], list.added = list, Generation
	end
end

local function GetRequires (list, requirements, requires)
	Generation = Generation + 1

	if type(requires) == "table" then
		for i, id in ipairs(requires) do
			GetRequirement(requirements, id, i)
		end
	elseif requires ~= nil then
		GetRequirement(requirements, requires, 0)
	end

	MergeRequirements(list, requirements)
end

local function IsNameNew (vname, vtype, requirements)
	for _, rlist in ipairs(requirements) do
		local rtype = rlist.varyings[vname]

		if rtype then
			if rtype ~= vtype then	-- n.b. even if this pair is equal, might disagree with
									-- other requirements; will be detected when the varyings
									-- from them are stitched in
				assert(false, "Varying '" .. vname .. "' already exists but does not have '" .. vtype .. "' type")
			end

			return false
		end
	end

	return true
end

local function StitchInVaryings (into, requirements)
	for _, rlist in ipairs(requirements) do
		for vname, vtype in pairs(rlist.varyings) do
			local cur = into[vname]

			if not cur then
				into[vname] = vtype
			elseif cur ~= vtype then
				assert(false, "Varying '" .. vname .. "' being assigned type '" .. vtype .. "' but already added with '" .. cur .. "' type")
			end
		end
	end
end

local Priority = { mat4 = 1, mat3 = 2, mat2 = 3, vec4 = 4, vec3 = 5, vec2 = 6, float = 7 }

local function GetVaryings (list, requirements, varyings)
	local into = list.varyings

	if varyings then
		CheckType(varyings, "varyings", "table")

		-- n.b. at the moment, only way the list may be this size
		assert(#list.snippets == 2, "Varyings expect fragment and vertex kernels to be present")

		for vname, vtype in pairs(varyings) do
			CheckType(vname, "varying name", "string")
			CheckType(vtype, "varying type", "string")

			if not Priority[vtype] then
				assert(false, "Unknown '" .. vtype .. "' varying type")
			end

			if IsNameNew(vname, vtype, requirements) then
				into[vname] = vtype
			end
		end
	end

	StitchInVaryings(into, requirements)
end

local function NewList ()
	return { snippets = {}, varyings = {} }
end

local AnyParams = {}

--- DOCME
function M.AddSnippet (params)
	local ptype = type(params)

	if ptype == "string" then
		params, AnyParams.any = AnyParams, params
	else
		assert(ptype == "table", "Invalid params")
	end
 
	local list = NewList()

	GetCode(list, params)

	local requirements = {}

	GetRequires(list, requirements, params.requires)
	GetVaryings(list, requirements, params.varyings)

	if ptype == "string" then
		AnyParams.any = nil
	end

	for _, snippet in ipairs(list.snippets) do
		snippet.integrated = true
	end

	local id = newproxy()

	Lists[id] = list

	return id
end

--
--
--

local function Gather (snippet, list)
	if snippet.generation ~= Generation then
		for _, requirement in ipairs(snippet.requirements) do
			Gather(requirement, list)
		end

		list[#list + 1], snippet.generation = snippet.code, Generation
	end
end

local function GetSortedCode (snippet)
	Generation = Generation + 1

	local sorted = {}

	Gather(snippet, sorted)

	return sorted
end

local function GetSortedVaryingStrings (varyings)
	local out

	for vname in pairs(varyings) do
		out = out or {}
		out[#out + 1] = vname
	end

	if out then
		sort(out, function(name1, name2)
			return Priority[varyings[name1]] < Priority[varyings[name2]]
		end)

		for i, name in ipairs(out) do
			out[i] = "varying P_UV " .. varyings[name] .. " " .. name .. ";"
		end

		out[#out + 1] = "\n"
	end

	return out
end

--- DOCME
function M.AugmentKernels (params, out)
	out = out or {}

	local list, requirements = NewList(), {}

	GetKernels(list, params.vertex, params.fragment)
	GetRequires(list, requirements, params.requires)
	GetVaryings(list, requirements, params.varyings)

	-- n.b. all params consumed, now safe to write as out

	local varyings = GetSortedVaryingStrings(list.varyings)

	for _, snippet in ipairs(list.snippets) do
		local code, sorted = {}, GetSortedCode(snippet)

		for i = 1, #(varyings or "") do
			code[#code + 1] = varyings[i]
		end

		for _, elem in ipairs(sorted) do
			code[#code + 1] = elem
		end

		out[snippet.branch] = concat(code)
	end

	return out
end

--
--
--

return M