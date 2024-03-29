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
local pairs = pairs
local remove = table.remove

-- Modules --
local named_bits = require("solar2d_utils.named_bits")

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

local function IsPhysicsObject (object)
  return display.isValid(object) and object.setLinearVelocity ~= nil
end

local function Resolve ()
	for i = 1, #Deferred, 3 do
		local func, object1, object2 = Deferred[i], Deferred[i + 1], Deferred[i + 2]
    local is_valid = func ~= display.remove and IsPhysicsObject or display.isValid

		if is_valid(object1) and (not object2 or is_valid(object2)) then
			func(object1, object2)
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
	object.isAwake, object.isBodyActive = true, true -- wake in case objects already overlap
end)

--
--
--

local Handlers = {}

--- Define a collision handler for objects of a given type.
--
-- When two objects collide, both are checked for a handler. Each handler that exists is
-- called as
--    handler(phase, object, other, contact)
-- where _phase_ is **"began"** or **"ended"**, _object_ is what supplied _handler_, _other_
-- is what collided with _object_, and _contact_ is the **collision** listener's contact.
-- @param type Type of object that will use this handler, as assigned by @{SetType}.
-- @callable func Handler function, or **nil** to remove the handler.
function M.AddHandler (type, func)
	Handlers[type] = func
end

--
--
--

--- DOCME
M.DeactivateLater = MakeDeferred(function(object)
	object.isBodyActive = false
end)

--
--
--

local function PairOK (o1, o2)
  return o1 ~= o2 and display.isValid(o1) and display.isValid(o2)
end

local Hidden = {}

local function IsHidden (object)
  return (Hidden[object] or 0) ~= 0
end

local function NeitherHidden (o1, o2)
  return not (IsHidden(o1) or IsHidden(o2))
end

--- DOCME
function M.DoIfEnabled (object, other, func)
  if PairOK(object, other) and NeitherHidden(object, other) then
		func(object, other, true)
	end
end

--
--
--

--- DOCME
function M.DoIfObjectEnabled (object, other, func)
  if PairOK(object, other) and not IsHidden(object) then
    func(object, other, true)
  end
end

--
--
--

--- DOCME
M.DoLater = Defer

--
--
--

local WipeState

local ConnectionLists = {}

local function GetOrMake (set, object)
  local t = set[object]

  if t then
    return t
  else
    t = {}
    set[object] = t

    return t, true
  end
end

local function EnsureConnectionList (object)
  local list, new = GetOrMake(ConnectionLists, object)

  if new then
    object:addEventListener("finalize", WipeState)
  end

  return list
end

--
--
--

local function AddToPairingsList (list, other, func, show_anyway)
  list[#list + 1] = other
  list[#list + 1] = func
  list[#list + 1] = show_anyway == true -- boolify
end

local Pairings = {}

local function GetPairingsList (object)
  return Pairings[object]
end

local function GetOrMakePairingsList (object)
  local list, new = GetOrMake(Pairings, object)

  if new then
    EnsureConnectionList(object)
  end

  return list
end

local function RemoveFromPairingsList (list, other)
	local n = #(list or "")

  for offset = n - 3, 0, -3 do
    if list[offset + 1] == other then
      local new_n = n - 3

      if offset ~= new_n then -- backfill needed?
        list[offset + 1] = list[n - 2]
        list[offset + 2] = list[n - 1]
        list[offset + 3] = list[n]
      end

      n, list[n - 2], list[n - 1], list[n] = new_n
    end
	end
end

--- DOCME
function M.DoWhenObjectEnabled (object, other, phase, func)
	if PairOK(object, other) then
		if phase == "ended" then
			RemoveFromPairingsList(GetPairingsList(object), other)
			RemoveFromPairingsList(GetPairingsList(other), object)
    else
      AddToPairingsList(GetOrMakePairingsList(object), other, func, true)

      if not IsHidden(object) then
        func(object, other, true)
      end
    end
	end
end

--
--
--

--- DOCME
function M.DoWhenPairEnabled (object, other, phase, func)
	if PairOK(object, other) then
		if phase == "ended" then
			RemoveFromPairingsList(GetPairingsList(object), other)
			RemoveFromPairingsList(GetPairingsList(other), object)
    else
      AddToPairingsList(GetOrMakePairingsList(object), other, func)

      if NeitherHidden(object, other) then
        func(object, other, true)
      end
    end
	end
end

--
--
--

local function GetConnectionList (object)
  return ConnectionLists[object]
end

local function NoOp() end

local function AuxIterPairings (list, first)
  first = first + 3

  local other = list[first]

  if other then
    return first, other, list[first + 1], list[first + 2]
  end
end

local function IterPairings (object)
  local list = GetPairingsList(object)

  if list then
    return AuxIterPairings, list, -2
  else
    return NoOp
  end
end

local function Intact (state)
  return state == true
end

local function AuxEnable (arr, object)
  local n, connections = 0, GetConnectionList(object)

  -- Gather object-first pairings.
  for _, other, func, show_anyway in IterPairings(object) do
    if (show_anyway or not IsHidden(other)) and Intact(connections[other]) then
      n, arr[n + 1], arr[n + 2], arr[n + 3] = n + 3, func, object, other
    end
  end

  -- Gather any reciprocal (other-first) pairings.
  for other, state in pairs(connections) do
    local other_connections = Intact(state) and not IsHidden(other) and GetConnectionList(other)

    if other_connections and Intact(other_connections[object]) then
      for _, oobject, func, show_anyway in IterPairings(other) do
        if oobject == object and not show_anyway then
          n, arr[n + 1], arr[n + 2], arr[n + 3] = n + 3, func, other, object
        end
      end
    end
  end

  return n
end

local function SetHidden (object, hide, mask)
  local cur = Hidden[object] or 0

  if hide then
    EnsureConnectionList(object)

    Hidden[object] = bit.bor(cur, mask)
  elseif IsHidden(object) then
    cur = bit.band(cur, bit.bnot(mask))

    Hidden[object] = cur ~= 0 and cur or nil
  end
end

local function IssueCalls (arr, n)
  for i = 1, n, 3 do
    local func, object, other = arr[i], arr[i + 1], arr[i + 2]
    
    if display.isValid(object) and display.isValid(other) then
      func(object, other, false)
    end
    
    arr[i], arr[i + 1], arr[i + 2] = false, false, false
  end
end

local EnableStack = {}

local function DoEnable (object, show, mask)
	if display.isValid(object) then
    local was_hidden = IsHidden(object)

		SetHidden(object, not show, mask)

		if was_hidden and not IsHidden(object) then
      local arr = remove(EnableStack) or {}
      local n = AuxEnable(arr, object)
    
      IssueCalls(arr, n)

      EnableStack[#EnableStack + 1] = arr
		end
	end
end

local AllOnes = bit.bnot(0)

--- DOCME
function M.Enable (object, show)
  DoEnable(object, show, AllOnes)
end

--
--
--

local CollisionBits

local function ResetCollisionBits ()
  CollisionBits = named_bits.New(16)
end

ResetCollisionBits()

--- Get the name-to-bit collection for **categoryBits** and **maskBits** filters.
--
-- Per the physics API, it has a limit of 16 names.
-- @treturn NamedBits Named bit collection.
function M.GetNamedBits ()
  return CollisionBits
end

--
--
--

local Types = {}

---
-- @param object
-- @return Type of _object_, or **nil** if absent.
-- @see SetType
function M.GetType (object)
  return display.isValid(object) and Types[object] or nil -- fallthrough if untyped
end

--
--
--

--- DOCME
function M.HideBits (object, mask)
  DoEnable(object, false, mask)
end

--
--
--

---
-- @pobject object
-- @treturn boolean Is _object_ visible?
-- @see SetVisibility
function M.IsEnabled (object)
	return display.isValid(object) and not IsHidden(object)
end

--
--
--

--- Assign a sensor body to an object.
-- @pobject object
-- @string[opt] body_type Physics body type, or **"dynamic"** by default.
-- @ptable[opt] props Optional properties.
function M.MakeSensor (object, body_type, props)
  if display.isValid(object) then
    physics.addBody(object, body_type or "dynamic", props)

    object.isSensor = true
  end
end

--
--
--

local function AuxCollision (phase, o1, o2, contact)
	local h1, h2 = Handlers[_GetType_(o1)], Handlers[_GetType_(o2)]

	if h1 then
		h1(phase, o1, o2, contact)
	end

	if h2 then
		h2(phase, o2, o1, contact)
	end
end

local function BreakConnection (o1, o2)
  local c1, c2 = GetConnectionList(o1), GetConnectionList(o2)

  if c1 then
    c1[o2] = nil
  end

  if c2 then
    c2[o1] = nil
  end
end

local function WillBeIntact (state, frame)
  return state == frame or Intact(state)
end

local function MakeConnection (o1, o2, frame)
  -- N.B. getFrameID() has a slight timing bug (it increments after physics)
  -- with a fix, we would subtract 1 from this ID. Strictly speaking the IDs
  -- aren't promised to honor this behavior either, though.

	local c1, c2 = EnsureConnectionList(o1), EnsureConnectionList(o2)
  local fixed = WillBeIntact(c1[o2], frame) and WillBeIntact(c2[o1], frame)

	c1[o2], c2[o1] = true, true

  return fixed
end

local function OnCollision (o1, o2, phase)
  if phase == "ended" then
    BreakConnection(o1, o2)
  else
    return MakeConnection(o1, o2, Runtime.getFrameID())
  end
end

--- DOCME
function M.OnEvent(event)
	local o1, o2, phase = event.object1, event.object2, event.phase

	if OnCollision(o1, o2, phase) ~= true then -- if just fixing a connection, no need to respond
		AuxCollision(phase, o1, o2, event.contact)
	end
end

--
--
--

--- Attempt to remove an object's body.
-- @param object
-- @treturn boolean Was a body removed?
function M.RemoveBody (object)
	local removed = _GetType_(object) ~= nil and physics.removeBody(object)

  if removed then
    local connections, frame = GetConnectionList(object), Runtime.getFrameID() -- may be repaired by next frame

    for other in pairs(connections) do
      connections[other] = frame

      local other_connections = GetConnectionList(other)

      if other_connections and Intact(other_connections[object]) then
        other_connections[object] = frame
      end
    end
  end

  return removed
end

--
--
--

--- DOCME
M.RemoveLater = MakeDeferred(display.remove)

--
--
--

--- DOCME
M.ResetCollisionBits = ResetCollisionBits

--
--
--

--- Associate a type with _object_: this is used to choose _object_'s handler in the
-- event of a collision. It may also be retrieved with @{GetType}; many handlers
-- will want to do so to classify the other object, for instance.
-- @param object
-- @param type Type to assign, or **nil** to untype _object_.
-- @see AddHandler
function M.SetType (object, type)
  if display.isValid(object) then
    if type ~= nil then
      EnsureConnectionList(object)
    end

    Types[object] = type
  end
end

--
--
--

--- DOCME
function M.ShowBits (object, mask)
  DoEnable(object, true, mask)
end

--
--
--

--- DOCME
M.StopLater = MakeDeferred(function(object)
	object:setLinearVelocity(0, 0)
end)

--
--
--

function WipeState (event)
  local object = event.target

  for other in pairs(ConnectionLists) do
    local other_connections = GetConnectionList(other)

    if other_connections then
      other_connections[object] = nil
    end

    RemoveFromPairingsList(GetPairingsList(other), object)
  end

  ConnectionLists[object], Hidden[object], Pairings[object], Types[object] = nil  
end

--
--
--

_GetType_ = M.GetType

return M