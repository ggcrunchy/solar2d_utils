--- Some utilities for giving names to bits, available as a **NamedBits** type.

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
local ipairs = ipairs
local pairs = pairs
local setmetatable = setmetatable

-- Plugins --
local bit = require("plugin.bit")

-- Exports --
local M = {}

--
--
--

local NamedBits = {}

NamedBits.__index = NamedBits

--
--
--

local function CheckLimit (ok, n)
  if not ok then
    assert(false, "Limit of " .. n .. " named bits")
  end
end

--- Build or retrieve a (one-bit) bitmask from a friendly name.
--
-- If _name_ is new, it is mapped to the next available mask. If the bit limit has
-- already been hit, an error is thrown.
-- @param name
-- @treturn uint Bitmask.
function NamedBits:GetBitmask (name)
  local mask = self.m_masks[name]

  if name ~= nil and not mask then -- new?
    local used, n = self.m_used, self.m_nbits

		CheckLimit(used < n, n)

    mask = bit.lshift(1, used)
    self.m_masks[name], self.m_used = mask, used + 1
  end

	return mask or 0
end

--
--
--

--- Combine multiple named bits together.
--
-- This might introduce new masks, cf. @{GetBitmask}. However, if doing so would exceed the
-- bit limit, an error is thrown and none are actually added.
--
-- Duplicate names are ignored.
-- @array names Names of bits to combine.
-- @treturn uint Boolean union of bitmasks.
function NamedBits:GetCombinedBitmask (names)
  local masks, used = self.m_masks, self.m_used

	for _, name in ipairs(names) do
    if not masks[name] then -- new?
      masks[name], used = -bit.lshift(1, used), used + 1 -- add provisionally
    end
	end

  if used > self.m_nbits then -- went over limit?
    for name, mask in pairs(masks) do
      if mask < 0 then
        masks[name] = nil
      end
    end
    
    CheckLimit(false, self.m_nbits)
  else -- good to go: commit changes
    for name, mask in pairs(masks) do
      if mask < 0 then
        masks[name] = -mask
      end
    end
    
    self.m_used = used
  end

  local bits = 0

	for _, name in ipairs(names) do
		bits = bit.bor(bits, masks[name])
	end

	return bits
end
--
--
--

--- Prepare a named bit collection.
-- @uint n Number of names / masks the collection may hold.
-- @treturn NamedBits Named bit collection.
function M.New (n)
  assert(n > 0, "Maximum must be positive integer")

  local named_bits = { m_masks = {}, m_used = 0, m_nbits = n }

	return setmetatable(named_bits, NamedBits)
end

--
--
--

return M