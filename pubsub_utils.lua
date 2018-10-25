--- Various utilities to facilitate the pub-sub pattern, in particular on the editor side. 

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

-- various link helpers...
-- try to smooth this out!

-- Standard library imports --
local assert = assert
local pairs = pairs
local rawequal = rawequal
local type = type

-- Modules --
local adaptive = require("tektite_core.table.adaptive")

-- Exports --
local M = {}

--
--
--

--- DOCME
function M.AddId (elem, key, id, sub)
	adaptive.Append_Member(elem, key, ComposeId(id, sub))
end

-- Cached PrepLink helper function --
local Helper

local function TryInInstances (elem, other, esub, osub, itls, instance_ids)
	local f, s, ii_key = adaptive.IterArray(instance_ids)

	for _, itl_key in adaptive.IterArray(itls) do
		local instance_to_label = elem[itl_key]
		local label = instance_to_label and instance_to_label[esub]

		_, ii_key = f(s, ii_key)

		if label then
			local list = elem[ii_key] or {}

			_AddId_(list, label, other.uid, osub)

			elem[ii_key] = list

			return true
		end
	end

	return false
end

local function TryInProps (elem, other, esub, osub, props)
	for _, pgroup in pairs(props) do
		if adaptive.InSet(pgroup, esub) then
			_AddId_(elem, esub, other.uid, osub)

			return true
		end
	end

	return false
end

local function TryOutProps (elem, esub, props, pkey)
	local pset, found = elem[pkey], false

	for k, pgroup in pairs(props) do
		if pgroup[esub] then
			pset, found = pset or {}, true
			pset[k] = adaptive.AddToSet(pset[k], esub)

			break
		end
	end

	elem[pkey] = pset

	return found
end

--- DOCME
-- should streamline a bit of this, into imports and exports
function M.PrepLink (elem, other, esub, osub)
	local helper, events, actions, akey, iprops, oprops, pkey, itls, instance_ids

	helper = Helper or function(what, arg1, arg2, arg3, arg4)
		if rawequal(what, ComposeId) then -- arbitrary nonce
			elem, other, esub, osub = arg1, arg2, arg3, arg4
		else
			assert(not Helper, "Link preparation already committed")

			if what == "commit" then
				local found = true

				if events and events[esub] then
					_AddId_(elem, esub, other.uid, osub)
				elseif actions and actions[esub] then
					adaptive.AddToSet_Member(elem, akey, esub)
				else
					found = itls and TryInInstances(elem, other, esub, osub, itls, instance_ids)
					found = found or (oprops and TryOutProps(elem, esub, oprops, pkey))
					found = found or (iprops and TryInProps(elem, other, esub, osub, iprops))
				end

				Helper, events, actions, akey, iprops, oprops, pkey, itls, instance_ids = helper -- re-cached helper

				return found ~= nil
			elseif what == "try_in_instances" then
				assert(type(arg1) == "string", "Expected string key for instance -> label map")
				assert(type(arg2) == "string", "Expected string key for resolved instances map")
				assert(arg1 ~= arg2, "Keys for instance -> label and resolved instances map must differ")

				itls = adaptive.Append(itls, arg1)
				instance_ids = adaptive.Append(instance_ids, arg2)
			else
				assert(type(arg1) == "table", "Expected table as first argument")

				if what == "try_actions" or what == "try_out_properties" then
					assert(arg2 == nil or type(arg2) == "string", "Expected string key or nil as second argument")

					if what == "try_actions" then
						actions, akey = arg1, arg2 or "actions"
					else
						oprops, pkey = arg1, arg2 or "props"
					end
				elseif what == "try_events" then
					events = arg1
				elseif what == "try_in_properties" then
					iprops = arg1
				end
			end
		end
	end

	helper(ComposeId, elem, other, esub, osub) -- see above re. ComposeId

	Helper = nil -- errors might leave helper in inconsistent state, so detach until commitment

	return helper
end

--- DOCME
-- the stuff that follows suggests some restructuring...
function M.PrepLinkHelper (prep_link_base, command)
	local funcs, cfuncs = {}

	local function prep_link_ex (object, other, osub, other_sub, links)
		if not funcs[object.type](object, other, osub, other_sub, links) then
			prep_link_base(object, other, osub, other_sub, links)
		end
	end

	return function(object_type, event, arg1, arg2)
		local prep = funcs[object_type]

		if prep then
			return prep, cfuncs and cfuncs[object_type]
		else
			local func, cleanup, how = event(command, prep_link_base, arg1, arg2)

			if (how or cleanup) == "complete" then -- allow optional cleanup as well
				return func, cleanup ~= "complete" and cleanup
			elseif func then
				funcs[object_type] = func

				if cleanup then
					cfuncs = cfuncs or {}
					cfuncs[object_type] = cleanup
				end

				return prep_link_ex, cleanup
			else
				return prep_link_base
			end
		end
	end, prep_link_ex
end

-- trying to suss out a new format for links
-- basically a name, predicate, and optional adaptive "parent" list (typically 0 or 1 element)
-- uint: parent{ int }
-- int: parent{ number }
-- e.g. OutNumber = pred{ Is(InNumber) }
-- e.g. InNumber = pred{ Is(OutNumber) and HasNoLinks() } (values will usually do this, but not actions?)

-- also should provide a way for In* (values) to also fall back to a field (would seem to cover most cases)
-- could then automate tidying these up
-- also facilities to couple e.g. with actions

-- All of this WILL make versioning harder, of course :)

-- also some of the functions that make up editor events
-- e.g. "super" part (event block, action, etc.); part itself; "super" boilerplate; part follow-up
-- how to compose this w.l.o.g.?

return M