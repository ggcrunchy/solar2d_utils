--- This module allows functions to be strung together, with the primary aim of building up
-- potentially complex logic from more manageable parts. 
--
-- In particular, this is tailored toward functions honoring an "event" policy.
--
-- TODO: this last bit might need some work, e.g. there is some resemblance to Solar's event
-- dispatchers, albeit keyed to objects rather than event names

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
local min = math.min
local print = print
local remove = table.remove
local setmetatable = setmetatable
local type = type

-- Modules --
local meta = require("tektite_core.table.meta")

-- Cached module references --
local _MakePerObjectList_

-- Exports --
local M = {}

--
--
--

local Mode = meta.WeakKeyed()

local function Validate (list)
	assert(list == nil or Mode[list], "List function must originate from MakePerObjectList() or Dispatcher:GetAdder()")
end

local AddCookie

local function AuxAdd (list, n)
	if list then
		return list(AddCookie, n)
	else
		return n
	end
end

--- DOCME
function M.Add (list, n)
	Validate(list)

	return AuxAdd(list, n)
end

--
--
--

local CheckCookie

local function AuxCheck (list, n)
	if list then
		return list(CheckCookie, n)
	else
		return 0
	end
end

--- DOCME
function M.Check (list, n)
	Validate(list)

	return AuxCheck(list, n)
end

--
--
--

local ActionCookie, IndexCookie

local function AuxList (list, index)
	if not index then -- simple function?
		local func = list and list(ActionCookie, "get")

		if func then
			return 0, func -- n.b. next iteration will pose as compound function, and fail
		end
	else
		local func = list(IndexCookie, index + 1)

		if func then -- compound function and within bounds?
			return index + 1, func
		end
	end
end

local function AuxIterateList (list)
	return AuxList, list, list and list(IndexCookie)
end

--- DOCME
-- @treturn iterator X
function M.IterateList (list)
	Validate(list)

	return AuxIterateList(list)
end

--
--
--

local BoxesStash = {}

local Specials = {}

local function Box (func, env)
	local box = remove(BoxesStash)

	if box then -- reuse?
		box(ActionCookie, func, env)
	else -- creation
		function box (k, a, b)
			local special = Specials[k]

			if not special then
				if env("add", 1) ~= 0 then
					func()

					return true
				else
					env("report", 0, 1)

					return false, 0
				end
			elseif special == "action" then
				if a == "get" then
					return func
				elseif a == "extract" then
					a, b, func, env = func, env

					return a, b
				else -- reuse case, see above
					func, env = a, b -- n.b. on creation, these were captured instead
				end
			elseif special ~= "index" and a > 0 then -- a: count
				return env(special, 1)
			end
		end
	end

	return box
end

local Environments = meta.WeakKeyed()

local NilKey

local function DefEnv (what, n, expected)
	if what == "add" then
		return n
	elseif what == "count" then
		return 0, 1 / 0
	else
		print("Expected to make " .. expected .. " calls but only performed " .. n)
	end
end

local function MaybeNil (name)
	return name == nil and NilKey or name
end

--
--
--

-- Reuse arbitrary internal objects as nonces.
NilKey = Specials
ActionCookie = BoxesStash
AddCookie = Environments
CheckCookie = DefEnv
IndexCookie = MaybeNil

Specials[ActionCookie] = "action"
Specials[AddCookie] = "add"
Specials[CheckCookie] = "check"
Specials[IndexCookie] = "index"

--
--
--

--- DOCME
-- @param[opt] name
-- @treturn function A
-- @treturn table T
function M.MakePerObjectList (name)
	local env, object_to_list, list = Environments[MaybeNil(name)] or DefEnv, meta.WeakKeyed()

	return function(func, object)
		local underlying_func = object_to_list[object]

		if underlying_func then -- not the very first function?
			if not list then -- second function?
				local func = underlying_func(ActionCookie, "extract") -- n.b. throw away environment

				list, BoxesStash[#BoxesStash + 1] = { func }, underlying_func -- recycle hollowed-out simple function...

				local function CompoundFunc (k, arg) -- ...replace with compound one
					local special = Specials[k]

					if not special then
						local expected_n = #list
						local n = env("add", expected_n)

						for i = 1, n do
							list[i]()
						end

						if n == expected_n then
							return true
						else
							env("report", n, expected_n)

							return false, n
						end
					elseif special == "index" then -- arg: index?
						if arg then
							return list[arg]
						else
							return 0
						end
					else -- arg: count
						return env(special, min(arg, #list))
					end
				end

				object_to_list[object], Mode[CompoundFunc], Mode[underlying_func] = CompoundFunc, "compound"
			end

			list[#list + 1] = func
		else -- first one
			local simple_func = Box(func, env)

			object_to_list[object], Mode[simple_func] = simple_func, "simple"
		end
	end, object_to_list
end

--
--
--

local Dispatcher = {}

Dispatcher.__index = Dispatcher

--
--
--

--- DOCME
-- @param object
-- @uint n
-- @treturn uint X
function Dispatcher:AddForObject (object, n)
	return AuxAdd(self.m_object_to_list[object], n)
end

--
--
--

--- DOCME
-- @param object
-- @uint n
-- @treturn uint X
function Dispatcher:CheckForObject (object, n)
	return AuxCheck(self.m_object_to_list[object], n)
end

--
--
--

--- DOCME
-- @param object
function Dispatcher:DispatchForObject (object)
    local func = self.m_object_to_list[object]

    if func then
        return func()
	else
		return true
    end
end

--
--
--

--- DOCME
-- @treturn function A
function Dispatcher:GetAdder ()
    return self.m_add_to_list
end

--
--
--

--- DOCME
-- @param object
-- @treturn iterator Y
function Dispatcher:IterateFunctionsForObject (object)
    return AuxIterateList(self.m_object_to_list[object])
end

--
--
--

---
-- @param[opt] name
-- @treturn Dispatcher X
function M.NewDispatcher (name)
    local dispatcher = {}

    dispatcher.m_add_to_list, dispatcher.m_object_to_list = _MakePerObjectList_(name)

	return setmetatable(dispatcher, Dispatcher)
end

--
--
--

--- DOCME
function M.SetEnvironment (params)
	assert(type(params) == "table", "Non-table `params`")

	local add, count, limit, report = params.add, params.count, params.limit, params.report

	assert(meta.CanCall(add), "Uncallable `add`")
	assert(meta.CanCall(count), "Uncallable `count`")
	assert(type(limit) == "number" and limit > 0, "Invalid `limit`")
	assert(report == nil or meta.CanCall(report), "Uncallable `report`")

	local function env (what, n, expected)
		if what ~= "report" then
			local ncalls = count()

			assert(type(ncalls) == "number" and ncalls >= 0 and ncalls <= limit, "`count` must return a non-negative number <= `limit`")

			if what == "count" then
				return ncalls, limit
			end

			assert(type(n) == "number" and n >= 0, "Add / check expects a non-negative number")

			if ncalls + n > limit then
				n = limit - ncalls
			end

			if n > 0 and what == "add" then
				add(n)
			end

			return n
		elseif report then
			report(n, expected)
		end
	end

	Environments[MaybeNil(params.name)] = env
end

--
--
--

_MakePerObjectList_ = M.MakePerObjectList

return M