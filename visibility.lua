--- This module wraps up some collision visbility state.

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

-- Corona globals --
local display = display

-- Cached module references --
local _Enable_

-- Exports --
local M = {}

--
--
--

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

local IsHidden, Partners

--- DOCME
function M.DoIfEnabled (object, other, func)
	local object_valid, other_valid = display.isValid(object), display.isValid(other)

	if object_valid and other_valid and object ~= other and not (IsHidden[object] or IsHidden[other]) then
		func(object, other, true)
	end
end

local function WipeState (object)
	if IsHidden then
		IsHidden[object], Partners[object] = nil
	end

	if Watching then
		Watching[object] = nil
	end
end

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

				_Enable_(object, true)
			end

		-- Phase "ended", objects intact: break pairings.
		else
			RemoveFromList(object, other)
			RemoveFromList(other, object)
		end
	end
end

---
-- @pobject object Object to poll about visibility.
-- @treturn boolean Is _object_ visible?
-- @see SetVisibility
function M.IsEnabled (object)
	return not IsHidden[object]
end

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

--- DOCME
function M.OnCollision (o1, o2, phase, id)
	if phase ~= "began" then
		Watch(o1, o2, nil)
	elseif Watch(o1, o2, id) then
		return
	end
end

--- DOCME
function M.Enable (object, show)
	if display.isValid(object) then
		local hide, is_hidden = not show, IsHidden[object]

		IsHidden[object] = hide

		--
		if hide then
			Check(object)

		--
		elseif is_hidden then
			local list1 = Partners[object]

			if list1 then
				local is_immediate = is_hidden == "immediate"

				for other, func in pairs(list1) do
					local intact = display.isValid(other)

					if not (intact and IsHidden[other]) then
						if intact then
							if func then
								func(object, other, is_immediate)
							end

							--
							local list2 = Partners[other]
							local func2 = list2 and list2[object]

							if func2 then
								func2(other, object, is_immediate)
							end
						end
					end
				end
			end
		end
	end
end

--- DOCME
function M.Start ()
	IsHidden, Partners, Watching = {}, {}, {}
end

--- DOCME
function M.Stop ()
	IsHidden, Partners, Watching = nil
end

--- DOCME
function M.Update (old_id, on_collision_ended)
	if Watching then
		-- Check all (valid) pairs being watched. If any objects are still colliding
		-- (since they collided again when a new body was added), keep the entries
		-- around but ignore the objects again until one of them is dirtied. If the
		-- entry has gone stale, break the connection.
		for object, w1 in pairs(Watching) do
			for other, id in pairs(w1) do
				if display.isValid(other) then
					if id == old_id then
						Watch(object, other, false)
					elseif id then
						Watch(object, other, nil)
						on_collision_ended(object, other)
					end
				else
					w1[other] = nil
				end
			end
		end
	end

	return Watching ~= nil
end

_Enable_ = M.Enable

return M