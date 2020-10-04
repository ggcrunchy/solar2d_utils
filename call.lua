--- Various call-related building blocks, in particular for objects typically built up from data. 

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
local pairs = pairs
local print = print
local remove = table.remove
local setmetatable = setmetatable
local type = type

-- Modules --
local meta = require("tektite_core.table.meta")

-- Cached module references --
local _Add_
local _Check_
local _DispatchOrHandleEvent_
local _GetRedirectTarget_
local _IterateList_
local _MakePerObjectList_

-- Exports --
local M = {}

--
--
--

local AddNonce

--- DOCME
function M.Add (list, n)
	if list then
		return list(AddNonce, n)
	else
		return n
	end
end

local CallEvent = {}

--- DOCME
function M.BindNamedArgument (name, arg)
	CallEvent[name] = arg
end

local CheckNonce

--- DOCME
function M.Check (list, n)
	if list then
		return list(CheckNonce, n)
	else
		return 0
	end
end

--- DOCME
function M.DispatchOrHandleEvent (object, event, def)
	local target, result = _GetRedirectTarget_(object)

	if target ~= nil then
		object = target
	end

	if type(object) == "table" then
		event.result, event.target = def, object

		local dispatch_event = object.dispatchEvent

		if dispatch_event then
			dispatch_event(object, event)
		else
			local handler = object[event.name]

			if handler then
				handler(event)
			end
		end

		result, event.result, event.target = event.result
	end

	return result
end

--- DOCME
function M.DispatchOrHandleNamedEvent (name, object, def)
	CallEvent.name = name

	return _DispatchOrHandleEvent_(object, CallEvent, def)
end

local Redirects = meta.WeakKeyed()

--- DOCME
function M.GetRedirectTarget (func)
	return Redirects[func]
end

local ActionNonce, IndexNonce

local function AuxList (list, index)
	if not index then -- simple function?
		local func = list and list(ActionNonce, "get")

		if func then
			return 0, func -- n.b. next iteration will try (and fail) compound branch
		end
	else
		local func = list(IndexNonce, index + 1)

		if func then -- compound function and within bounds?
			return index + 1, func
		end
	end
end

--- DOCME
-- @treturn iterator X
function M.IterateList (list)
	return AuxList, list, list and list(IndexNonce)
end

local BoxesStash = {}

local Specials = {}

local function Box (func, env)
	local box = remove(BoxesStash)

	if box then
		box(ActionNonce, func, env)
	else
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
				else
					func, env = a, b -- N.B. captured at first, thus only need this on reuse
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
		print("Expected to make " .. expected .. " calls but only did " .. n)
	end
end

local function MaybeNil (name)
	if name == nil then
		return NilKey
	else
		return name
	end
end

-- Arbitrarily assign internal objects as nonces.
NilKey = Specials
ActionNonce = CallEvent
AddNonce = Redirects
CheckNonce = DefEnv
IndexNonce = MaybeNil

Specials[ActionNonce] = "action"
Specials[AddNonce] = "add"
Specials[CheckNonce] = "check"
Specials[IndexNonce] = "index"

--- DOCME
-- @param[opt] name
-- @treturn function A
-- @treturn table T
function M.MakePerObjectList (name)
	local env, object_to_list, list = Environments[MaybeNil(name)] or DefEnv, meta.WeakKeyed()

	return function(func, object)
		local curf = object_to_list[object]

		if curf then -- not the very first function?
			if not list then -- second function?
				local func = curf(ActionNonce, "extract") -- throw away environment

				list, BoxesStash[#BoxesStash + 1] = { func }, curf

				object_to_list[object] = function(k, arg)
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
			end

			list[#list + 1] = func
		else -- first one
			object_to_list[object] = Box(func, env)
		end
	end, object_to_list
end

local Dispatcher = {}

Dispatcher.__index = Dispatcher

--- DOCME
-- @param object
-- @uint n
-- @treturn uint X
function Dispatcher:AddForObject (object, n)
	return _Add_(self.m_object_to_list[object], n)
end

--- DOCME
-- @param object
-- @uint n
-- @treturn uint X
function Dispatcher:CheckForObject (object, n)
	return _Check_(self.m_object_to_list[object], n)
end

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

--- DOCME
-- @treturn function A
function Dispatcher:GetAdder ()
    return self.m_add_to_list
end

--- DOCME
-- @param object
-- @treturn iterator Y
function Dispatcher:IterateFunctionsForObject (object)
    return _IterateList_(self.m_object_to_list[object])
end

---
-- @param[opt] name
-- @treturn Dispatcher X
function M.NewDispatcher (name)
    local dispatcher = {}

    dispatcher.m_add_to_list, dispatcher.m_object_to_list = _MakePerObjectList_(name)

	return setmetatable(dispatcher, Dispatcher)
end

--- DOCME
function M.Redirect (func, target)
	assert(not Redirects[func], "Function already redirected")

	Redirects[func] = target
end

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

--- DOCME
function M.UnbindArguments ()
	for k in pairs(CallEvent) do
		CallEvent[k] = nil
	end
end

_Add_ = M.Add
_Check_ = M.Check
_DispatchOrHandleEvent_ = M.DispatchOrHandleEvent
_GetRedirectTarget_ = M.GetRedirectTarget
_IterateList_ = M.IterateList
_MakePerObjectList_ = M.MakePerObjectList

return M