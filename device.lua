--- This module wraps up some useful device functionality.

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
local ipairs = ipairs
local pairs = pairs
local tonumber = tonumber

-- Solar2D globals --
local Runtime = Runtime
local system = system

-- Add persistence for configuring?

-- Exports --
local M = {}

--
--
--

--[[
local AxisMappings = {
	OUYA = {
		"left_x", "left_y", "left_trigger", "right_x", "right_y", "right_trigger"
	},
	PS3 = {
		"left_x", "left_y", "right_x", "right_y", [13] = "left_trigger", [14] = "right_trigger"
	}
}
]]

local Axes

local function BindAxisNumbers (device, mapping)
	local axes = {}

	for i, _ in ipairs(device:getAxes()) do
		local amap = mapping[i]

		if amap then
			axes[amap] = i
		end
	end

	Axes[device.descriptor] = axes
end

--
--
--

local AddDevice = {}

--
--
--

local OUYA = "OUYA Game Controller"

local PS3 = "PLAYSTATION(R)3 Controller"

local Joysticks

function AddDevice:joystick (index)
	local name, joy, mapping = self.displayName, { device = self }

	if name:sub(1, #OUYA) == OUYA then
		mapping = Axes.OUYA
	elseif name == PS3 then
		mapping = Axes.PS3
	-- elseif in database, i.e. under `permanentId`... (limit this?)
		--
	end

	--
	if mapping then
		BindAxisNumbers(self, mapping)

		joy.mapping = mapping
	end

	--
	Joysticks[index] = joy
end


--
--
--

local Keyboards

function AddDevice:keyboard (index)
	-- Do we care about particular keyboards?
		-- possible to omit built-ins?
	-- Load key mappings from database?
	-- Include default...
	Keyboards[index] = { device = self }
end

--
--
--

local Mice

function AddDevice:mouse (index)
	-- Do we care about particular mice? (maybe just if they have a wheel...)
	-- Load button mappings?
	Mice[index] = { device = self }
end

--
--
--

--- DOCME
function M.EnumerateDevices ()
	Axes, Joysticks, Keyboards, Mice = {}, {}, {}, {}

	for _, device in ipairs(system.getInputDevices()) do
		local func = AddDevice[device.type]

		if func then
			local index = device.descriptor:match("(%d+)$")

			func(device, tonumber(index))
		end
	end
end

--
--
--

--- DOCME
function M.GetAxisMapping (event)
	local axes, number = Axes[event.device.descriptor], event.axis.number

	if axes then
		for id, name in pairs(axes) do
			if id == number then
				return name
			end
		end
	end

	return "none"
end

-- same for button?

-- ouya, nvidia shield, gamestick
--[[
	local function KeyEvent (event)
		local key_name = event.keyName

		if event.phase == "down" then
			-- button mappings...
			-- button* = (A, B, X, Y) (O, A, U, Y = OUYA; X, Circle, Square, Triangle = PS3)
			-- "up", "down", "left", "right",
			-- "buttonSelect", "buttonStart", "buttonMode" (Power on / off)
			-- *shoulderButton? ("left", "right"; 1, 2)
			-- *joystickButton ("left", "right")
			-- "back", "volumeDown", "volumeUp"

			return true
		end

		return false
	end

	Runtime:addEventListener("key", KeyEvent)
end
]]

--
--
--

-- Adapted from ponywolf's joykey:
do
	local DeadZone = 0.333

	local cache, map = {}, {}

	map["axis1-"] = "left"
	map["axis1+"] = "right"
	map["axis2-"] = "up"
	map["axis2+"] = "down"

	map["axis3-"] = "a"
	map["axis3+"] = "d"
	map["axis4-"] = "w"
	map["axis4+"] = "s"

	local KeyEvent = { name = "key" }

	local function SendKeyEvent (phase, key_name)
		KeyEvent.keyName, KeyEvent.phase = key_name, phase
	
		Runtime:dispatchEvent(KeyEvent)
	end

	-- Capture the axis event
	local function AxisToKey (event)
		local name = "axis" .. (event.axis.number or 1)
		local dtype, plus, minus = event.device.type, map[name .. "+"], map[name .. "-"]

		if plus and (dtype == "gamepad" or dtype == "joystick") then
			local value, key_name, opposite = event.normalizedValue

			if value > 0 then
				key_name, opposite = plus, minus
			else
				key_name, opposite = minus, plus
			end

			if abs(value) > DeadZone then
				if cache[opposite] then
					cache[opposite] = false

					SendKeyEvent("up", opposite)
				end

				if not cache[key_name] then
					cache[key_name] = true

					SendKeyEvent("down", key_name)
				end
			else
				if cache[key_name] then
					cache[key_name] = false

					SendKeyEvent("up", key_name)
				end

				if cache[opposite] then
					cache[opposite] = false

					SendKeyEvent("up", opposite)
				end
			end
		end

		return true
	end

	local IsMapped

	--- DOCME
	function M.MapAxesToKeyEvents (enable)
		if not enable then
			Runtime:removeEventListener("axis", AxisToKey)
		elseif not IsMapped then
			Runtime:addEventListener("axis", AxisToKey)
		end

		IsMapped = not not enable
	end
end

--
--
--

local Actions = {}

--- DOCME
function M.MapButtonsToAction (action, map)
	for ctype, name in pairs(map) do
		local cgroup = Actions[ctype] or {}

		cgroup["button" .. name], Actions[ctype] = action, cgroup
	end
end

--
--
--

local Gamepads = {}

--- DOCME
function M.TranslateButton (event)
	local ctype = Gamepads[event.device]
	local cgroup = Actions[ctype]

	return cgroup and cgroup[event.keyName]
end

--
--
--

local XInput = "XInput Gamepad"

local Devices = {}

local function IdentifyGamepad (device)
	local name = device.displayName

	if name:sub(1, #OUYA) == OUYA then
		Gamepads[device] = "OUYA"
	elseif name == PS3 then
		Gamepads[device] = "PS3"
	elseif name:find(XInput) then
		Gamepads[device] = "Xbox360"
	elseif device.MFiProfile then
		local mfip = device.MFiProfile

		Gamepads[device] = "MFi" .. mfip:sub(1, 1):upper() .. mfip:sub(2)
	else -- TODO: several others, also test different platforms
		Gamepads[device] = "unknown"
	end
end

local OnAndroid = system.getInfo("platform") == "android" and system.getInfo("environment") == "device"

local function AddNewDevice (device)
	Devices[#Devices + 1] = device

	if device.type == "gamepad" then
		IdentifyGamepad(device)
	elseif OnAndroid and device.type == "joystick" then
		if device.displayName == "Microsoft X-Box 360 pad" then -- observed, but this concurs:
		-- https://github.com/libretro/retroarch-joypad-autoconfig/blob/master/android/Microsoft_XBOX_360_Controller_USB.cfg
			Gamepads[device] = "Xbox360"
		end
		-- TODO: need more controllers to see how this all generalizes
	end
end

for _, device in ipairs(system.getInputDevices()) do
	AddNewDevice(device)
end

Runtime:addEventListener("inputDeviceStatus", function(event)
	local ed = event.device

	for _, device in ipairs(Devices) do
		if ed == device then
			ed = nil
			
			break
		end
	end

	if ed then
		AddNewDevice(ed)
	elseif event.connectionStateChanged then
		-- ??? (also, reconfigured...)
	end
end)

--
--
--

return M