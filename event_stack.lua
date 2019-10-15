--- Event stack for logic that can be replaced, shadowed, or augmented by higher-priority
-- behavior, e.g. temporarily adding new key input.

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

local function AuxCall (stack, index, shell, event, get_def_result)
    local handler, result = stack[index]

    if handler then
        local new_event

        result, new_event = shell(handler, event)

        if result == "call_next_handler" then
            if new_event ~= nil then
                event = new_event
            end

            return AuxCall(stack, index - 1, shell, event, get_def_result)
        end
    elseif get_def_result then
        result = get_def_result(event)
    end

    return result
end

local function DefShell (handler, event) return handler(event) end

--- Call the topmost event handler on the stack, if present, as `result, new = shell(handler(event)`).
--
-- Normally, the call will finish here and return _result_.
--
-- If _result_ is **"call\_next\_handler"**, the process repeats with the next handler
-- down, and so on. If _new_ is non-**nil**, it replaces _event_ before the next call.
--
-- The default _shell_, will pass _result_ and _new_ through unchanged, but some other
-- behavior may be assigned by @{EventStack:SetShell}.
-- @tparam EventStack stack
-- @param event User-defined event.
-- @tparam[opt=nil] callable get_def_result If the next handler is missing (n.b. this
-- is immediately true with an empty stack), called as `result = get_def_result(event)`. If
-- absent, _result_ will be **nil**.
-- @return Result of the last event handler that was called. **N.B.** In this context,
-- _get\_def\_result_ is interpreted as the bottom handler in the stack.
function M.Call (stack, event, get_def_result)
    return AuxCall(stack, #stack, stack.m_shell or DefShell, event, get_def_result)
end

local EventStack = {}

EventStack.__index = EventStack

--- Guard any handlers currently in the stack from future removals by @{EventStack:Clear}
-- and @{EventStack:Pop}.
function EventStack:Bake ()
    self.m_base = #self
end

--- Remove all event handlers from the stack.
-- @bool clear_baked Remove entries guarded by @{EventStack:Bake}?
function EventStack:Clear (clear_baked)
    for i = #self, (clear_baked and 0 or self.m_base) + 1, -1 do
        self[i] = nil
    end
end

---
-- @treturn callable? Event handler on top of the stack, or **nil** if empty.
function EventStack:GetTop ()
    return self[#self]
end

--- Remove the top event handler from the stack, if present.
-- @treturn callable? Handler that was on top, or **nil** if it was empty or baked.
-- @see EventStack:Bake
function EventStack:Pop ()
    local n, top = #self

    if n > self.m_base then
        top, self[n] = self[n]
    end

    return top
end

--- Push an event handler onto the top of the stack.
-- @callable handler
function EventStack:Push (handler)
    self[#self + 1] = handler
end

--- Set a shell behavior for handlers, e.g. to ignore results or automate next-handler calls.
-- @callable shell
function EventStack:SetShell (shell)
    self.m_shell = shell
end

---
-- @treturn EventStack Empty stack.
function M.New ()
    return setmetatable({ m_base = 0 }, EventStack)
end

return M