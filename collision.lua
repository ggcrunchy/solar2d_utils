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
local visibility = require("solar2d_utils.visibility")

-- Solar2D globals --
local display = display
local Runtime = Runtime

-- Solar2D modules --
local physics = require("physics")

-- Plugins --
local bit = require("plugin.bit")

-- Cached module references --
local _GetType_

-- Exports --
local M = {}

--
--
--

local Deferred = {}

local function Resolve ()
	for i = 1, #Deferred, 3 do
		local object1, object2 = Deferred[i + 1], Deferred[i + 2]

		if display.isValid(object1) and (not object2 or display.isValid(object2)) then
			Deferred[i](object1, object2)
		end
	end

	for i = #Deferred, 1, -1 do
		Deferred[i] = nil
	end

	Runtime:removeEventListener("lateUpdate", Resolve)
end

local function Defer (func, object1, object2)
	local n = #Deferred

	if n == 0 then
		Runtime:addEventListener("lateUpdate", Resolve)
	end

	Deferred[n + 1], Deferred[n + 2], Deferred[n + 3] = func, object1, object2 or false
end

local function MakeDeferred (func)
	return function(object)
		Defer(func, object)
	end
end

--- DOCME
M.ActivateLater = MakeDeferred(function(object)
	object.isBodyActive = true
end)

local Handlers = {}

--- Define a collision handler for objects of a given collision type with other objects.
--
-- When two objects collide, each is checked for a handler. Each handler that exists is
-- called as
--    handler(phase, object, other, contact)
-- where _phase_ is **"began"** or **"ended"**, _object_ is what supplied _handler_, _other_
-- is what collided with _object_, and _contact_ is the physics object passed via the event.
-- @param type Type of object that will use this handler, as assigned by @{SetType}.
-- @callable func Handler function, or **nil** to remove the handler.
function M.AddHandler (type, func)
	Handlers[type] = func
end

--- DOCME
M.DeactivateLater = MakeDeferred(function(object)
	object.isBodyActive = false
end)

--- DOCME
M.DoLater = Defer

local NewMask = 0x1

local NamedMasks = {}

local function AuxGetBitmask (name, new_mask)
	local mask = NamedMasks[name]

	if not mask or mask < 0 then
		assert(new_mask < 0xFFFF, "Limit of 16 named flags")

		mask, new_mask = new_mask, 2 * new_mask
		NamedMasks[name] = -mask -- add provisionally, since we might hit the assert() mid-loop
	end

	return mask, new_mask
end

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
-- TODO: revise this!
function M.GetBitmask (name)
	local mask, new_mask = AuxGetBitmask(name, NewMask)

	if new_mask ~= NewMask then
		NamedMasks[name], NewMask = mask, new_mask
	end

	return mask
end

--- DOCME
function M.GetCombinedBitmask (names)
	local bits, new_mask, mask = 0, NewMask

	for i = 1, #names do
		mask, new_mask = AuxGetBitmask(names[i], new_mask)
		bits = bit.bor(bits, mask)
	end

	-- We can now safely repair any new namings and commit `NewMask`.
	for i = 1, #names do
		local name = names[i]
		local mask = NamedMasks[name]

		if mask < 0 then
			NamedMasks[name] = -mask
		end
	end

	NewMask = new_mask

	return bits
end

local Types = {}

---
-- @param object Object to query.
-- @return Collision type of _object_, or **nil** if absent.
-- @see SetType
function M.GetType (object)
	return display.isValid(object) and Types[object] or nil -- fallthrough if untyped
end

--- Assign a sensor body to an object.
-- @pobject object Object to make into a sensor.
-- @string body_type Physics body type, or **"dynamic"** by default.
-- @ptable props Optional properties.
function M.MakeSensor (object, body_type, props)
	physics.addBody(object, body_type or "dynamic", props)

	object.isSensor = true
end

--- Attempt to remove an object's body.
-- @param object Object with body to remove.
-- @treturn boolean Was a body removed?
function M.RemoveBody (object)
	return _GetType_(object) ~= nil and physics.removeBody(object)
end

--- DOCME
M.StopLater = MakeDeferred(function(object)
	object:setLinearVelocity(0, 0)
end)

--- Associate a collision type with _object_. This is used to choose _object_'s handler in
-- the event of a collision, and will also be provided (as a convenience) to the other
-- object's handler.
-- @param object Object to type.
-- @param type Type to assign, or **nil** to untype _object_.
-- @see AddHandler, GetType
function M.SetType (object, type)
	Types[object] = type
end

local function EnterLevel (event)
	if event.name == "enter_level" then
		physics.start()
		physics.setGravity(0, 0)
	end

	visibility.Start()
end

local function AuxCollision (phase, o1, o2, contact)
	local h1, h2 = Handlers[_GetType_(o1)], Handlers[_GetType_(o2)]

	if h1 then
		h1(phase, o1, o2, contact)
	end

	if h2 then
		h2(phase, o2, o1, contact)
	end
end

local function OnEnded (object, other)
	AuxCollision("ended", object, other, false)
end

local FrameID

for k, v in pairs{
	collision = function(event)
		local o1, o2, phase = event.object1, event.object2, event.phase

		if visibility.OnCollision(o1, o2, phase, FrameID) then
			AuxCollision(phase, o1, o2, event.contact)
		end
	end,

	enterFrame = function(event)
		if visibility.Update(FrameID, OnEnded) then
			FrameID = event.frame
		end
	end,

	enter_level = EnterLevel,

	leave_level = function()
		physics.stop()
		visibility.Stop()
	end,

	reset_level = EnterLevel
} do
	Runtime:addEventListener(k, v)
end

_GetType_ = M.GetType

return M