--- Utility for logic that can be replaced, shadowed, or augmented on demand.

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
local setmetatable = setmetatable

-- Exports --
local M = {}

--
--
--

local CallChain = {}

CallChain.__index = CallChain

--
--
--

local function DefShell (handler, arg) return handler(arg) end

--- Call the most recent handler, if present, as `result, new = shell(handler, arg`).
--
-- Normally, the call will end here and return _result_.
--
-- If _result_ is **"call\_next\_handler"**, the process repeats with the next most recent
-- handler, and so on. If _new_ is non-**nil**, it replaces _arg_ before the next call.
--
-- The default _shell_ simply returns `handler(arg)`.
-- @param arg
-- @tparam[opt] callable get_def_result Fallback when the expected handler is missing. If
-- present, called as `result = get_def_result(arg)`; otherwise, `result` is **nil**.
-- @return Final value of `result`.
-- @see CallChain:SetShell
function CallChain:__call (arg, get_def_result)
    local shell = self.m_shell or DefShell

    for i = #self, 1, -1 do
        local result, new = shell(self[i], arg)

        if result ~= "call_next_handler" then
            return result
        elseif new ~= nil then
            arg = new
        end
    end

    return get_def_result and get_def_result(arg)
end

--
--
--

--- Guard any current handlers in the chain from removal by future @{CallChain:Clear} and
-- @{CallChain:Pop} calls.
function CallChain:Bake ()
    self.m_base = #self
end

--
--
--

--- Remove all handlers from the chain.
-- @bool clear_baked Remove handlers guarded by @{EventStack:Bake}?
function CallChain:Clear (clear_baked)
    for i = #self, (clear_baked and 0 or self.m_base) + 1, -1 do
        self[i] = nil
    end
end

--
--
--

---
-- @treturn callable? Most recently added handler, or **nil** if empty.
-- @see CallChain:Push
function CallChain:GetTop ()
    return self[#self]
end

--
--
--

--- Remove the most recent handler from the chain, if present.
-- @treturn callable? Handler, or **nil** if it was baked or the chain was empty.
-- @see CallChain:Bake, CallChain:Push
function CallChain:Pop ()
    local n, top = #self

    if n > self.m_base then
        top, self[n] = self[n]
    end

    return top
end

--
--
--

--- Add a handler to the chain.
-- @callable handler
function CallChain:Push (handler)
    self[#self + 1] = handler
end

--
--
--

--- Assign a shell behavior, e.g. to ignore results or automate handler chaining.
-- @tparam ?|callable|nil shell New shell, or **nil** to restore the default.
-- @treturn ?|callable|nil Old shell, or **nil** if none was installed.
-- @see CallChain:__call
function CallChain:SetShell (shell)
    local old = self.m_shell

    self.m_shell = shell

    return old
end

--
--
--

---
-- @tstring[opt] name If present, the **__call** logic is also added under this key; this is
-- a convenience for chains as table listeners.
-- @treturn CallChain Empty chain.
-- @see CallChain:__call
function M.New (name)
    local chain = { m_base = 0 }

    if name then
        chain[name] = CallChain.__call
    end

    return setmetatable(chain, CallChain)
end

--
--
--

return M