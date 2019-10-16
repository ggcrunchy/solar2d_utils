--- This module wraps up some useful collision functionality.

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
local pairs = pairs

-- Modules --
local adaptive = require("tektite_core.table.adaptive")
local args = require("iterator_ops.args")
local frames = require("corona_utils.frames")
local powers_of_2 = require("bitwise_ops.powers_of_2")

-- Corona globals --
local display = display
local timer = timer

-- Corona modules --
local physics = require("physics")

-- Cached module references --
local _Implements_Pred_
local _SetVisibility_

-- Exports --
local M = {}

--
--
--

--- Activate or deactivate of a physics object, via a 0-lapse timer.
-- @pobject object Physics object.
-- @bool active Activate? Otherwise, deactivate.
-- @return (0-lapse) timer handle.
function M.Activate (object, active)
	return timer.performWithDelay(0, function()
		object.isBodyActive = active
	end)
end

-- Collision handlers --
local Handlers = {}

--- Defines a collision handler for objects of a given collision type with other objects.
--
-- When two objects collide, each is checked for a handler. Each handler that exists is
-- called as
--    handler(phase, object, other, other_type, contact)
-- where _phase_ is **"began"** or **"ended"**, _object_ is what supplied _handler_, _other_
-- is what collided with _object_, _other_type_ is the collision type of _other_ (may be
-- **nil**), and _contact_ is the physics object passed via the event.
--
-- The collision type of _object_ is not supplied, being implicit in the handler itself. For
-- all practical purposes, _type_ is just a convenient name for _func_; there is no real
-- reason to assign a handler to more than one type.
-- @param type Type of object that will use this handler, as assigned by @{SetType}.
-- @callable func Handler function, or **nil** to remove the handler.
function M.AddHandler (type, func)
	Handlers[type] = func
end

-- --
local InterfacePreds, Interfaces = {}, {}

--- DOCME
function M.AddInterfaces (type, ...)
	for _, v in args.Args(...) do
		adaptive.AddToSet_Member(Interfaces, type, v)
	end
end

--- DOCME
function M.AddInterfaces_Pred (type, ...)
	local preds = InterfacePreds[type] or {}

	for _, what, pred in args.ArgsByN(2, ...) do
		adaptive.AddToSet_Member(Interfaces, type, what)

		preds[what] = pred
	end

	InterfacePreds[type] = preds
end

-- --
local Watching

--- DOCME
function M.Dirty (object)
	if display.isValid(object) then
		local watch = Watching[object] or {}

		for other in pairs(watch) do
			watch[other] = 0
		end

		Watching[object] = watch
	end
end

-- --
local IsHidden, Partners

-- Types used to manage physics interactions --
local Types = {}

--- DOCME
function M.DoIfVisible (object, other, func)
	local object_valid, other_valid = display.isValid(object), display.isValid(other)

	if object_valid and other_valid and object ~= other and not (IsHidden[object] or IsHidden[other]) then
		func(object, other, Types[other], true)
	end
end

--
local function WipeState (event)
	local object = event.target

	if IsHidden then
		IsHidden[object], Partners[object] = nil
	end

	if Watching then
		Watching[object] = nil
	end

	Types[object] = nil
end

--
local function Check (object)
	if IsHidden[object] == nil then
		object:addEventListener("finalize", WipeState)

		IsHidden[object] = false
	end
end

--
local function AddToList (object, other, func)
	Check(object)

	local list = Partners[object] or {}

	Partners[object], list[other] = list, func
end

--
local function InList (object, other)
	local list = Partners[object]

	return list and list[other]
end

--
local function RemoveFromList (object, other)
	local list = Partners[object]

	if list then
		list[other] = nil
	end
end

--- DOCME
function M.DoOrDefer (object, other, phase, func)
	if object ~= other and display.isValid(object) and display.isValid(other) then
		-- Phase "began", objects intact: if at least one of the objects is hidden, defer the
		-- action; otherwise, perform it immediately.
		if phase == "began" then
			--
			AddToList(object, other, func)

			if not InList(other, object) then
				AddToList(other, object, false)
			end

			--
			if not (IsHidden[object] or IsHidden[other]) then
				IsHidden[object] = "immediate"

				_SetVisibility_(object, true)
			end

		-- Phase "ended", objects intact: break pairings.
		else
			RemoveFromList(object, other)
			RemoveFromList(other, object)
		end
	end
end

-- Next mask to allocate --
local Mask = 0x1

-- Lazily named filter flags --
local NamedFlags = setmetatable({}, {
	__index = function(t, k)
		assert(Mask < 0xFFFF, "Limit of 16 named flags")

		t[k], Mask = Mask, 2 * Mask

		return t[k]
	end
})

--- Convenience routine used to build bitmasks for **categoryBits** and **maskBits** filter
-- fields, using friendly names instead of magic numbers.
--
-- When a name is used for the first time, it is mapped to the next available flag. There
-- is a hard limit of 16 names, per the physics API.
--
-- These names are in a separate namespace from collision types, i.e. they need not match,
-- though there is likely to be some crossover in practice, e.g. for clarity.
-- @param ... Names of filter bits to combine. Duplicate and **nil** names are ignored.
-- @treturn uint The boolean union of filter bits, or 0 if none were assigned.
function M.FilterBits (...)
	local bits = 0

	for _, name in args.Args(...) do
		if name ~= nil then
			bits = powers_of_2.Set(bits, NamedFlags[name])
		end
	end

	return bits
end

--- Getter.
-- @param object Object to query.
-- @return Collision type of _object_, or **nil** if absent.
-- @see SetType
function M.GetType (object)
	return Types[object]
end

--- DOCME
function M.Implements (object, what)
	return adaptive.InSet(Interfaces[Types[object]], what)
end

--- DOCME
function M.Implements_Pred (object, what, ...)
	local type = Types[object]
	local implements = adaptive.InSet(Interfaces[type], what)

	if implements then
		local preds = InterfacePreds[type]
		local func = preds and preds[what]

		if func then
			return func(...) and "passed" or "failed"
		else
			return "no_predicate"
		end
	end

	return "does_not_implement"
end

--- DOCME
function M.Implements_PredOrDefFail (object, what, ...)
	local res = _Implements_Pred_(object, what, ...)

	return res ~= "no_predicate" and res or "failed"
end

--- DOCME
function M.Implements_PredOrDefPass (object, what, ...)
	local res = _Implements_Pred_(object, what, ...)

	return res ~= "no_predicate" and res or "passed"
end

--- Predicate.
-- @pobject object Object to poll about visibility.
-- @treturn boolean Is _object_ visible?
-- @see SetVisibility
function M.IsVisible (object)
	return not IsHidden[object]
end

--- Assigns a sensor body to an object.
-- @pobject object Object to make into a sensor.
-- @string body_type Physics body type, or **"dynamic"** by default.
-- @ptable props Optional properties.
function M.MakeSensor (object, body_type, props)
	physics.addBody(object, body_type or "dynamic", props)

	object.isSensor = true
end

--- Attempts to remove an object's body.
-- @param object Object with body to remove.
-- @treturn boolean Was a body removed?
function M.RemoveBody (object)
	return Types[object] ~= nil and physics.removeBody(object)
end

--- Associates a collision type with _object_. This is used to choose _object_'s handler in
-- the event of a collision, and will also be provided (as a convenience) to the other
-- object's handler.
-- @param object Object to type.
-- @param type Type to assign, or **nil** to untype _object_.
-- @see AddHandler, GetType
function M.SetType (object, type)
	Check(object)

	Types[object] = type
end

--- DOCME
function M.SetVisibility (object, show)
	--
	if display.isValid(object) then
		local hide, is_hidden = not show, IsHidden[object]

		--
		IsHidden[object] = hide

		--
		if hide then
			Check(object)

		--
		elseif is_hidden then
			local list1 = Partners[object]

			if list1 then
				local otype, is_immediate = Types[object], is_hidden == "immediate"

				for other, func in pairs(list1) do
					local intact = display.isValid(other)

					if not (intact and IsHidden[other]) then
						if intact then
							if func then
								func(object, other, Types[other], is_immediate)
							end

							--
							local list2 = Partners[other]
							local func2 = list2 and list2[object]

							if func2 then
								func2(other, object, otype, is_immediate)
							end
						end
					end
				end
			end
		end
	end
end

-- Enter Level response
local function EnterLevel ()
	if not IsHidden then
		physics.start()
		physics.setGravity(0, 0)
	end

	IsHidden, Partners, Watching = {}, {}, {}
end

--
local function Watch (o1, o2, value)
	local w1, w2 = Watching[o1], Watching[o2]

	if w1 or w2 then
		local v1, v2

		if w1 then
			w1[o2], v1 = value, w1[o2]
		end

		if w2 then
			w2[o1], v2 = value, w2[o1]
		end
			
		return v1 or v2
	end
end

-- --
local FrameID

--
local function AuxCollision (phase, o1, o2, contact)
	local t1, t2 = Types[o1], Types[o2]
	local h1, h2 = Handlers[t1], Handlers[t2]

	if h1 then
		h1(phase, o1, o2, t2, contact)
	end

	if h2 then
		h2(phase, o2, o1, t1, contact)
	end
end

for k, v in pairs{
	-- Collision --
	collision = function(event)
		local o1, o2, phase = event.object1, event.object2, event.phase

		if phase ~= "began" then
			Watch(o1, o2, nil)
		elseif Watch(o1, o2, FrameID) then
			return
		end

		AuxCollision(phase, o1, o2, event.contact)
	end,

	-- enterFrame --
	enterFrame = function()
		if Watching then
			-- Check all (valid) pairs being watched. If any objects are still colliding
			-- (since they collided again when a new body was added), keep the entries
			-- around but ignore the objects again until one of them is dirtied. If the
			-- entry has gone stale, break the connection.
			for object, w1 in pairs(Watching) do
				for other, frame in pairs(w1) do
					if display.isValid(other) then
						if frame == FrameID then
							Watch(object, other, false)
						elseif frame then
							Watch(object, other, nil)
							AuxCollision("ended", object, other, false)
						end
					else
						w1[other] = nil
					end
				end
			end

			-- Now that all checks have been done against it, update the collision frame ID.
			FrameID = frames.GetFrameID()
		end
	end,

	-- Enter Level --
	enter_level = EnterLevel,

	-- Leave Level --
	leave_level = function()
		physics.stop()

		IsHidden, Partners, Watching = nil
	end,

	-- Reset Level --
	reset_level = EnterLevel
} do
	Runtime:addEventListener(k, v)
end

_Implements_Pred_ = M.Implements_Pred
_SetVisibility_ = M.SetVisibility

return M