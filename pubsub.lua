--- A list that follows the pub-sub pattern. 

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
local find = string.find
local format = string.format
local pairs = pairs
local sub = string.sub
local tonumber = tonumber
local type = type

-- Modules --
local adaptive = require("tektite_core.table.adaptive")
local meta = require("tektite_core.table.meta")

-- Cached module references --
local _MakeEndpoint_

-- Exports --
local M = {}

--
--
--

-- Check whether a candidate is valid, i.e. it resembles a result of _MakeEndpoint_.
--
-- **N.B.** The current implementation does not rigorously examine the input, merely checking
-- for strings containing a known separator. This is enough to filter out **uint** and **nil**
-- inputs that might be common enough, but not hand-crafted malicious inputs.
-- @param candidate
-- @bool split Should an ID and name be returned if this is a valid endpoint?
-- @return[1] **true**, indicating a valid endpoint.
-- @treturn[1] uint The ID preceding the separator...
-- @treturn[1] string ...and the name following it.
-- @treturn[2] boolean Was _endpoint_ valid?
function M.IsEndpoint (candidate, split)
	local is_endpoint = false

	if type(candidate) == "string" then
		local pos = find(candidate, ":")

		if pos ~= nil then
			if split then
				return true, tonumber(sub(candidate, 1, pos - 1)), sub(candidate, pos + 1)
			else
				is_endpoint = true
			end
		end
	end

	return is_endpoint
end

--- Builds an endpoint from an ID and name.
-- @uint id
-- @string name
-- @treturn string Endpoint.
function M.MakeEndpoint (id, name)
	return format("%i:%s", id, name)
end

local PubSubList = {}

--- DOCME
function PubSubList:Dispatch ()
	for i = 1, #self, 3 do
		local endpoint, func, arg = self[i], self[i + 1], self[i + 2]

		func(self[endpoint], arg)
	end
end

--- DOCME
-- @param payload
-- @tparam ?|uint|nil id
-- @string name
function PubSubList:Publish (payload, id, name)
	if id then
		self[_MakeEndpoint_(id, name)] = payload
	end
end

--- 
-- @tparam ?|string|{string,...}|nil endpoints 0, 1, or multiple publisher endpoints.
-- @callable func When @{PubSubList:Dispatch} is fired, every satis
-- @param[opt=false] arg
function PubSubList:Subscribe (endpoints, func, arg)
	arg = arg or false

	for _, v in adaptive.IterArray(endpoints) do
		self[#self + 1] = v
		self[#self + 1] = func
		self[#self + 1] = arg
	end
end

--- Wipe all subscribers and payloads from the list.
-- @see PubSubList:Publish, PubSubList:Subscribe
function PubSubList:Wipe ()
    for k in pairs(self) do
        self[k] = nil
    end
end

--- DOCME
-- @treturn PubSubList PSL
function M.New ()
    local list = {}

    meta.Augment(list, PubSubList)

    return list
end

_MakeEndpoint_ = M.MakeEndpoint

return M