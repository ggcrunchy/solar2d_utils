--- Utilities for so-called "events", i.e. callable objects that honor a certain policy
-- allowing them to be aggregated and linked together.
--
-- TODO: this last bit might need some work

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
local type = type

-- Modules --
local meta = require("tektite_core.table.meta")

-- Cached module references --
local _DispatchOrHandle_
local _GetRedirectTarget_

-- Exports --
local M = {}

--
--
--

local CallEvent = {}

--- DOCME
function M.BindNamedArgument (name, arg)
	CallEvent[name] = arg
end

--
--
--

--- DOCME
function M.DispatchOrHandle (object, event, def)
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

--
--
--

--- DOCME
function M.DispatchOrHandle_Named (name, object, def)
	CallEvent.name = name

	return _DispatchOrHandle_(object, CallEvent, def)
end

--
--
--

local Redirects = meta.WeakKeyed()

--- DOCME
function M.GetRedirectTarget (func)
	return Redirects[func]
end

--- DOCME
function M.Redirect (func, target)
	assert(not Redirects[func], "Function already redirected")

	Redirects[func] = target
end

--
--
--

--- DOCME
function M.UnbindArguments ()
	for k in pairs(CallEvent) do
		CallEvent[k] = nil
	end
end

--
--
--

_DispatchOrHandle_ = M.DispatchOrHandle
_GetRedirectTarget_ = M.GetRedirectTarget

return M