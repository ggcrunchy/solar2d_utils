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
local _MakeAddress_

-- Exports --
local M = {}

--
--
--

-- DOCME
-- @tparam ?|int|string id
-- @bool split
-- @treturn boolean C
function M.IsAddress (id, split)
	local is_address = false

	if type(id) == "string" then
		local pos = find(id, ":")

		if pos ~= nil then
			if split then
				return true, tonumber(sub(id, 1, pos - 1)), sub(id, pos + 1)
			else
				is_address = true
			end
		end
	end

	return is_address
end

--- Builds an address from an ID and name.
-- @int id
-- @string name
-- @treturn string Composite ID.
function M.MakeAddress (id, name)
	return format("%i:%s", id, name)
end

local PubSubList = {}

--- DOCME
function PubSubList:Dispatch ()
	for i = 1, #self, 3 do
		local address, func, arg = self[i], self[i + 1], self[i + 2]

		func(self[address], arg)
	end
end

--- DOCME
-- @param payload
-- @tparam ?|int|nil id
-- @string name
function PubSubList:Publish (payload, id, name)
	if id then
		id = _MakeAddress_(id, name)
		self[id] = payload
	end
end

--- DOCME
-- @tparam ?|string|{string,...} id
-- @callable func
-- @param arg
function PubSubList:Subscribe (id, func, arg)
	arg = arg or false

	for _, v in adaptive.IterArray(id) do
		self[#self + 1] = v
		self[#self + 1] = func
		self[#self + 1] = arg
	end
end

--- DOCME
function PubSubList:Wipe ()
    for k in pairs(self) do
        self[k] = nil
    end
end

--- DOCME
function M.New ()
    local list = {}

    meta.Augment(list, PubSubList)

    return list
end

_MakeAddress_ = M.MakeAddress

return M