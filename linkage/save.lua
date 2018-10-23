--- TODO

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
local ipairs = ipairs
local pairs = pairs

-- Modules --
local common = require("s3_editor.Common") -- urgh...
local linkage_utils = require("corona_utils.linkage.utils")
local strings = require("tektite_core.var.strings")

-- Cached module references --
local _SaveValuesIntoEntry_

-- Export --
local M = {}

--
--
--

local function GatherLabel (name, labels)
	local label = common.GetLabel(name)

	if label then
		labels = labels or {}
		labels[name] = label
	end

	return labels
end

--- DOCME
function M.ResolveLinks_Save (level)
	local list = level.links

	if list then
		local new, links, labels = {}, common.GetLinks()
		local tag_db = links:GetTagDatabase()

		for _, rep in ipairs(list) do
			local entry = common.GetValuesFromRep(rep)

			new[#new + 1] = "entry"
			new[#new + 1] = entry.uid

			entry.uid = nil

			for _, sub in tag_db:Sublinks(links:GetTag(rep), "no_templates") do
				new[#new + 1] = "sub"
				new[#new + 1] = sub

				labels = GatherLabel(sub, labels)

				for link in links:Links(rep, sub) do
					local obj, osub = link:GetOtherObject(rep)

					new[#new + 1] = list[obj]
					new[#new + 1] = osub

					labels = GatherLabel(sub, labels)
				end
			end
		end

		level.links, level.labels = new, labels
	end
end

--- DOCME
function M.SaveGroupOfValues (level, what, mod, view)
	local target = {}

	level[what] = { entries = target, version = 1 }

	local values = view:GetValues()

	for k, v in pairs(values) do
		target[k] = _SaveValuesIntoEntry_(level, mod, v, {})
	end
end

local function HasAny (rep)
	local links = common.GetLinks()
	local tag = links:GetTag(rep)

	if tag then
		local f, s, v0, reclaim = links:GetTagDatabase():Sublinks(tag, "no_templates")

		for _, sub in f, s, v0 do
			if links:HasLinks(rep, sub) then
				reclaim()

				return true
			end
		end
	end
end

--- DOCME
function M.SaveValuesIntoEntry (level, mod, values, entry)
	linkage_utils.EnumDefs(mod, values)

	-- Does this values blob have any links? If so, make note of it in the blob itself and
	-- add some tracking information in the links list.
	local rep = common.GetRepFromValues(values)

	if HasAny(rep) then
		local list = level.links or {}

		if not list[rep] then
			values.uid = strings.NewName()

			list[#list + 1] = rep
			list[rep] = #list
		end

		level.links = list
	end

	-- Copy the values into the editor state, alert any listeners, and add defaults as necessary.
	for k, v in pairs(values) do
		entry[k] = v
	end

	linkage_utils.EditorEvent(mod, "save", level, entry, values)

	linkage_utils.AssignDefs(entry)

	entry.positions, entry.instances = common.GetPositions(rep), common.GetInstances(rep, "copy")

	return entry
end

-- ^^^ TODO: what would it take to use this to save / restore via undo?

-- Cache module members.
_SaveValuesIntoEntry_ = M.SaveValuesIntoEntry

-- Export the module.
return M