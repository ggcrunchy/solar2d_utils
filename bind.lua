--- Editor- and application-side utilities for inter-object binding.

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
local find = string.find
local format = string.format
local pairs = pairs
local rawequal = rawequal
local rawget = rawget
local remove = table.remove
local setmetatable = setmetatable
local sub = string.sub
local tonumber = tonumber
local type = type

-- Modules --
local adaptive = require("tektite_core.table.adaptive")
local frames = require("corona_utils.frames")
local lazy = require("tektite_core.table.lazy")
local meta = require("tektite_core.table.meta")

-- Corona globals --
local Runtime = Runtime
local system = system

-- Cached module references --
local _AddId_
local _BroadcastBuilder_
local _IterEvents_
local _Reset_
local _Subscribe_

-- Exports --
local M = {}

-- Builds a composite ID out of a target's ID and sublink
local function ComposeId (id, sub)
	return format("%i:%s", id, sub)
end

--- Adds an ID to an element.
--
-- This is a so-called compound ID. Initially, _key_ is assumed to contain **nil**. When
-- the first ID is added, it is stored as is; if further ID's are added, the ID is altered
-- to a compound form.
--
-- Intended for editor-side use, e.g. in a **"prep_link"** handler.
-- @ptable elem Element.
-- @string key Key under which the ID is stored.
-- @int id Target's ID. The stored ID will be a composite of _id_ and _sub_.
-- @string sub Name of target's sublink.
-- @see tektite_core.table.adaptive.Append_Member
function M.AddId (elem, key, id, sub)
	adaptive.Append_Member(elem, key, ComposeId(id, sub))
end

-- --
local Count, Limit = 0

local TryToResetCount = frames.OnFirstCallInFrame(function()
	Count = 0
end)

--- DOCME
function M.AddCalls (n)
	TryToResetCount()

	local count = Count + n

	assert(Limit, "Calls require a limit")
	assert(count <= Limit, "Too many calls")

	Count = count
end

--- DOCME
function M.AtLimit ()
	return Count == Limit
end

-- --
local Tally, LastReported, OnTooManyEvent = 0, 0

local function TryToReport ()
	local now = system.getTimer()

	Tally = Tally + 1

	if LastReported - now >= 3000 then -- throttle the reports
		OnTooManyEvent = OnTooManyEvent or {}
		OnTooManyEvent.name, OnTooManyEvent.tally = "too_many_actions", Tally

		Runtime:dispatchEvent(OnTooManyEvent)

		LastReported, Tally = now, 0
	end	
end

local function AdjustN (n)
	if Limit and Count + n > Limit then
		return Limit - Count
	else
		return n
	end
end

local BoxesStash = {}

local function Box (event)
	local box = remove(BoxesStash)

	if box then
		box(ComposeId, event) -- arbitrary nonce
	else
		function box (arg1, arg2)
			if rawequal(arg1, ComposeId) then -- as per note
				if arg2 == "get" then
					return event
				elseif arg2 == "extract" then
					arg2, event = event

					return arg2
				else
					event = arg2 -- N.B. captured at first, thus only need this on reuse
				end
			elseif arg1 ~= "i" and arg1 ~= "n" then
				if AdjustN(1) == 0 then
					return TryToReport()
				else
					return event()
				end
			end
		end
	end

	return box
end

--- Convenience routine for building a subscribe function that in turn will populate a
-- broadcast-type event sender, i.e. one that may send 0, 1, or multiple events.
-- @treturn function Builder, called as
--    builder(event, object)
-- where _event_ is a published event, cf. @{Publish}.
--
-- When the first event is received, it is assigned to _object_ as is, under the key _what_.
-- If further events are received, a compound event is built up and assigned to the key.
--
-- In the compound case, there are two special ways to call the event:
--
-- * If **"n"** is the first argument, returns the number of component events, _n_.
-- * If **"i"** is the first argument, the second argument, _i_, is interpreted as an index.
-- If _i_ is an integer &isin; [1, _n_], returns compound event #_i_; otherwise, **nil**.
--
-- Otherwise, each component event is called with the compound event's arguments.
--
-- **N.B.** Individually, if called with **"n"** as its first argument, any _event_ is
-- expected to be a no-op.
--
-- It is intended that _builder_ be passed as the _func_ argument to @{Subscribe}, with
-- _object_ passed alongside it as _arg_.
-- @treturn table Weak table associating objects to broadcasters.
function M.BroadcastBuilder ()
	local object_to_broadcaster, list = meta.Weak("k")

	return function(func, object)
		-- If events already exist, add this event to the list. If this
		-- is only the second event thus far, begin constructing the list
		-- (moving the first event into it) and the compound function.
		local curf = object_to_broadcaster[object]

		if curf then
			if not list then
				list, BoxesStash[#BoxesStash + 1] = { curf(ComposeId, "extract") }, curf -- see note in Box()

				object_to_broadcaster[object] = function(arg1, arg2)
					if arg1 == "i" then
						return list[arg2]
					end

					local expected_n = #list
					local n = AdjustN(expected_n)

					if arg1 == "n" then
						return n
					end

					for i = 1, n do
						list[i]()
					end

					if n < expected_n then
						TryToReport()
					end
				end
			end

			list[#list + 1] = func

		-- No events yet: add the first one.
		else
			object_to_broadcaster[object] = Box(func)
		end
	end, object_to_broadcaster
end

--- Variant of @{BroadcastBuilder} that supplies a helper object to facilitate certain
-- common broadcast use cases.
-- @param[opt] name Name of waiting list. If absent, this must be provided by @{broadcast_helper:Subscribe}.
-- @treturn broadcast_helper Object with some useful functions.
function M.BroadcastBuilder_Helper (name)
	local broadcast_helper, builder, object_to_broadcaster = {}, _BroadcastBuilder_()

	--- Calls _object_'s broadcast function, i.e. performs `object_to_broadcaster[object](...)`.
	--
	-- If the function absent, this is a no-op.
	-- @param object Object in which the broadcast function is stored.
	-- @param ... Arguments.
	function broadcast_helper:__call (object, ...)
		local func = object_to_broadcaster[object]

		if func then
			func(...)
		end
	end

	--- Iterates _object_'s broadcast function, i.e. performs `IterEvents(object_to_broadcaster[object])`.
	--
	-- @param object Object in which the broadcast function is stored.
	-- @treturn iterator As per @{IterEvents}.
	function broadcast_helper.Iter (object)
		return _IterEvents_(object_to_broadcaster[object])
	end

	--- Subscribes _object_ to events, i.e. performs `Subscribe(name, id, builder, object)`,
	-- with _builder_ as per @{BroadcastBuilder}.
	-- @param object Object to query for broadcast function.
	-- @tparam ?|string|array|nil id As per @{Subscribe}.
	-- @string? wname If provided, use the named waiting list.
	function broadcast_helper.Subscribe (object, id, wname)
		if name == nil then
			assert(wname ~= nil, "Missing waiting list name")

			name = wname
		end

		_Subscribe_(name, id, builder, object)
	end

	-- Hook up a metatable for __call and supply the helper. Add the key in case the user
	-- does end up needing it (for the auto-generated case, mostly).
	setmetatable(broadcast_helper, broadcast_helper)

	return broadcast_helper
end

-- Event iterator body
local function AuxEvent (event, index)
	if not index then
		if event and (not Limit or Count < Limit) then
			return 0, event(ComposeId, "get") -- see note in Box()
		end
	elseif index > 0 then
		return index - 1, event("i", index)
	end
end

-- --
local Commands = meta.Weak("k")

--- DOCME
function M.GetActionCommands (action)
	return Commands[action]
end

--- Convenience routine for iterating the component events that make up a broadcast (i.e. a
-- compound event built up by some variant of @{BroadcastBuilder}), e.g. when some operation
-- must be performed between each one.
--
-- This accounts for absent, single, and compound events.
--
-- **N.B.** In the single event case, if called with **"n"** as its first argument, _event_
-- is expected to be a no-op.
-- @callable[opt] event The compound event stored mapped to the broadcasting object, cf.
-- @{BroadcastBuilder}. If **nil** (interpreted as said object not being subscribed to the
-- event), this is a no-op.
-- @treturn iterator Supplies index, event.
function M.IterEvents (event)
	return AuxEvent, event, event and event("n")
end

--- Predicate.
-- @tparam int|string id
-- @bool split If composite, return the parts?
-- @treturn boolean _id_ is a composite of a simple ID and a sublink?
-- @treturn ?int Simple ID.
-- @treturn ?string Sublink.
function M.IsCompositeId (id, split)
	local is_composed = false

	if type(id) == "string" then
		local pos = find(id, ":")

		if pos ~= nil then
			if split then
				return true, tonumber(sub(id, 1, pos - 1)), sub(id, pos + 1)
			else
				is_composed = true
			end
		end
	end

	return is_composed
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

--- Provides a helper function to prepare an element for linking.
--
-- This is intended for editor-side use, namely in **"prep_link"** handlers.
--
-- **N.B.** The function is only meant for immediate use; arguments are bound internally when
-- **PrepLink** is called and unbind following a commit.
-- @ptable elem Element.
-- @ptable other Other element.
-- @string esub Name of element's sublink.
-- @string osub Name of other element's sublink.
-- @treturn function Link preparation function.
--
-- Calling code should invoke one or more of the following commands:
--
-- To associate events: `helper("try_events", events)`, with _events_ being a table.
--
-- For actions: `helper("try_actions", actions, akey)`, with _actions_ being a table and
-- _akey_ a string (if absent, **"actions"** is assumed).
--
-- For in-properties: `helper("try_in_properties", in_props)`, with _in\_props_ being a table.
--
-- For out-properties: ``helper("try_out_properties", out_props, pkey)`, with _out\_props_
-- being a table and _pkey_ a string (if absent, **"props"** is assumed).
--
-- For labeled in-instances: `helper("try_in_instances", itl_key, ii_key)`, with _itl\_key_ and
-- _ii\_key_ each being strings.
--
-- After that, the following call should be performed: `ok = helper("commit")`. The relevant
-- tables are searched until either `t[esub]` is non-false, in which case one of the actions
-- described below is taken (_ok_ = **true**), or all options are exhausted (_ok_ = **false**).
--
-- Each scenario asks whether _esub_ is present as a key, ignoring the associated value. In
-- the case of _events_ and _actions_, this is a direct lookup; on the other hand, properties
-- are tables of per-type tables, with these subtables being searched.
--
-- If the key is found among _events_ or _in\_props_, the corresponding event or property is
-- registered in _elem_ via @{AddId}, with the unique ID of _other_ and _osub_ as target ID
-- and sublink name, respectively.
--
-- When found among _actions_ or _out\_props_, _esub_ is added to an adaptive set: `elem[akey]`
-- or `elem[pkey]`, respectively, cf. @{tektite_core.table.adaptive.AddToSet_Member}.
--
-- Should the key be found in `elem[itl_key]`, i.e. a particular instances-to-labels map, the
-- target is registered much like _events_ or _in\_props_: `AddId(elem[ii_key], label, other.uid, osub)`,
-- creating a table under _ii\_key_ if necessary.
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

-- Waiting lists --
local Deferred = lazy.SubTablesOnDemand()

--- Publishes an event. This is intended as a startup process, to provide events to be
-- picked up by event senders.
--
-- Some or all of the subscriptions to this event may have yet to be made, so publishing
-- is deferred until resolution occurs, cf. @{Resolve}.
-- @param name Name of waiting list.
-- @callable event The event to publish.
-- @tparam int|nil id ID of object to which the event belongs, e.g. its target or a dummy singleton.
--
-- If absent, this is a no-op.
--
-- **N.B.** This is not a compound ID.
-- @string sub Sublink name, i.e. which of the object's events is this?
-- @see Subscribe
function M.Publish (name, event, id, sub)
	if id then
		id = ComposeId(id, sub)

		Deferred[name][id] = event
	end
end

--- Empties a waiting list, removing all subscribers and published events.
-- @param name Name of waiting list. 
-- @see Publish, Subscribe
function M.Reset (name)
	Deferred[name] = nil
end

--- Delivers all published events in the given waiting list to their subcribers.
--
-- The waiting list is then reset, cf. @{Reset}.
-- @see Publish, Subscribe
function M.Resolve (name)
	local dt = rawget(Deferred, name)

	for i = 1, #(dt or ""), 3 do
		local id, func, arg = dt[i], dt[i + 1], dt[i + 2]

		func(dt[id], arg)
	end

	_Reset_(name)
end

--- DOCME
function M.SetActionCommands (action, cmds)
	assert(type(action) == "function", "Non-function action")
	assert(cmds == nil or type(cmds) == "function", "Non-function commands")

	Commands[action] = cmds
end

--- DOCME
function M.SetActionLimit (limit)
	assert(type(limit) == "number" and limit > 0, "Invalid limit")

	Limit = limit
end

--- Subscribes to 0 or more events. This is intended as a startup process that associates
-- event senders with the events to be sent.
--
-- Some or all of the events in question may have yet to be published, but this is fine,
-- since delivery is deferred until resolution occurs, cf. @{Resolve}.
-- @param name Name of waiting list.
-- @tparam ?string|array|nil id Compound ID as per @{AddId} or @{LinkActionsAndEvents}, where each
-- component ID corresponds to a published event, cf. @{Publish}.
--
-- If **nil**, this is a no-op.
-- @callable func Called as
--    func(event, arg)
-- on resolution, for each _event_ to which it subscribed.
-- @param arg Argument to _func_. If absent, **false**.
function M.Subscribe (name, id, func, arg)
	arg = arg or false

	local dt = Deferred[name]

	for _, v in adaptive.IterArray(id) do
		dt[#dt + 1] = v
		dt[#dt + 1] = func
		dt[#dt + 1] = arg
	end
end

-- Cache module members.
_AddId_ = M.AddId
_BroadcastBuilder_ = M.BroadcastBuilder
_IterEvents_ = M.IterEvents
_Reset_ = M.Reset
_Subscribe_ = M.Subscribe

-- Export the module.
return M