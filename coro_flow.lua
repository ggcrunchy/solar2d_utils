--- Coroutine-based control flow operations.

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
local running = coroutine.running
local yield = coroutine.yield

-- Modules --
local meta = require("tektite_core.table.meta")

-- Cached module references --
local _BasicBody_
local _GetIterationLapse_
local _GetIterationTime_
local _TimedBody_

-- Exports --
local M = {}

--
--
--

local DoneArg, DoneArgIndex

local function GetDoneArgs (arg1, arg2, arg3)
  local darg1, darg2, darg3 = arg1, arg2, arg3

  if DoneArgIndex == 1 then
    darg1 = DoneArg
  elseif DoneArgIndex == 2 then
    darg2 = DoneArg
  elseif DoneArgIndex == 3 then
    darg3 = DoneArg
  end

  DoneArg, DoneArgIndex = nil

  return darg1, darg2, darg3
end

local ShouldNegate, YieldValue

--- Body for control flow operations.
--
-- Once invoked, this will spin on a test / update loop until told to terminate. On each
-- iteration, if it did not terminate, it will yield.
-- @tparam ?|callable|nil update Update logic, called as
--    result = update(arg1, arg2, arg3)
-- after _done_. If **nil**, this is a no-op.
--
-- If _result_ is **"done"**, the body will terminate early.
-- @callable done Test, with same signature as _update_, called on each iteration. When
-- _result_ resolves to true (by default, if it is true), the loop terminates.
-- @ptable config Configuration parameters.
--
-- If the **negate_done** field is true, the _result_ from _done_ is negated, i.e. instead
-- of "until test passes" the loop is interpreted as "while test passes", and vice versa.
--
-- If a **yvalue** field is present, this value is yielded after each iteration.
-- @param arg1 Argument #1...
-- @param arg2 ...#2...
-- @param arg3 ...and #3.
-- @treturn boolean Operation completed normally, i.e. not an early out via _update_?
-- TODO: above needs some rework
function M.BasicBody (update, done, arg1, arg2, arg3)
	assert(meta.CanCall(done), "Uncallable done")
	assert(update == nil or meta.CanCall(update), "Uncallable update")

  local darg1, darg2, darg3 = GetDoneArgs(arg1, arg2, arg3)
	local yvalue, notted = YieldValue, not not ShouldNegate -- coerce to boolean: will agree with `not done()` when we pass

	ShouldNegate, YieldValue = nil

	while true do
		if not done(darg1, darg2, darg3) == notted then -- finished? (see note above)
			return true
		elseif update ~= nil and update(arg1, arg2, arg3) == "done" then -- early out?
			return false
		else
			yield(yvalue)
		end
	end
end

--
--
--

local IterationLapse

--- DOCME
function M.GetIterationLapse ()
	return IterationLapse or 0
end

--
--
--

local IterationTime

--- DOCME
function M.GetIterationTime ()
	return IterationTime or 0
end

--
--
--

local Deduct, Lapse

--- DOCME
-- **N.B.** This simply calls the lapse function, if any, assigned via @{SetTimeLapseFuncs},
-- so care should be taken if that carries side effects.
-- @treturn number Lapse
function M.GetLapse ()
	if Lapse then
		return Lapse()
	else
		return 0
	end
end

--
--
--

--- Make a function that can assume different behavior for each coroutine.
-- @treturn function Function which takes a single argument and passes it to the logic
-- registered for the current coroutine, returning any results. If no behavior is assigned,
-- or this is called from outside any coroutine, this is a no-op.
-- @treturn function Setter function, which must be called within a coroutine. The function
-- passed as its argument is assigned as the coroutine's behavior; it may be cleared
-- by passing **nil**.
--
-- It is also possible to pass **"exists"** as argument, which will return **true** if a
-- function is assigned to the current coroutine.
-- TODO: revise this!
function M.MakeLocalStorage ()
	local list = meta.WeakKeyed()

	return function(value)
		local coro = assert(running(), "Called outside a coroutine")

		if value == nil then
			return list[coro]
		else
			list[coro] = value
		end
	end
end

--
--
--

--- DOCME
function M.SetDoneArg (arg, index)
  DoneArg, DoneArgIndex = arg, index
end

--
--
--

--- Assign the time lapse functions used by @{Body_Timed}.
--
-- The lapse function tells us how much time is available **right now** to run a timed body
-- operation. It may be the case that only some of this is needed: a useful abstraction
-- here is a "time bank", where the "balance"&mdash;viz. the entire time slice&mdash;is
-- reported, then the deduct function is told how much to "withdraw".
--
-- In this way, say, a 10-millisecond wait need not consume a 100-millisecond time slice.
-- Indeed, two consecutive 10-millisecond waits could run and still leave more: the lapse
-- would first report 100 milliseconds available, then 90, and so on.
--
-- **N.B.** It is up to the user to decide on the unit of time and employ it consistently.
-- @tparam ?|callable|nil lapse Lapse function to assign, which returns a time lapse as a
-- non-negative number; or **nil** to restore the default (which returns 0).
-- @tparam ?|callable|nil deduct Deduct function to assign, which accepts a non-negative
-- lapse amount and deducts it from the "time bank"; or **nil** to restore the default (a
-- no-op).
function M.SetTimeLapseFuncs (lapse, deduct)
	assert(lapse == nil or meta.CanCall(lapse), "Uncallable lapse function")
	assert(deduct == nil or meta.CanCall(deduct), "Uncallable deduct function")

	Lapse, Deduct = lapse, deduct
end

--
--
--

local UsingTime

--- DOCME
function M.SetUsingTime ()
	UsingTime = true
end

--
--
--

--- DOCME
function M.SetYieldValue (value)
	YieldValue = value
end

--
--
--

local function Clamp (lapse, total)
  assert(lapse >= 0, "Lapse must be non-negative")

  return lapse <= total and lapse or total
end

local function NoDeduct () end

local function NoLapse () return 0 end

--- Timed variant of @{BasicBody}.
--
-- The current time lapse behavior at the time of the call will be used throughout the body,
-- even if @{SetTimeLapseFuncs} is called again before the body has concluded.
--
-- Logically, this body maintains a counter, _time_, which begins at 0. On each iteration,
-- time lapse function is polled for a value, _lapse_, &ge; 0. After test / update, if the
-- body-based operation has not concluded, _time_ will be incremented by _lapse_ (possibly
-- reduced by _update_ and / or _done_), just before the body yields for that iteration.
--
-- On each iteration, the final value of _lapse_ will also be deducted from the "time bank",
-- before yielding or returning.
-- @tparam ?|callable|nil update As per @{BasicBody}, but called as
--    result, true_lapse = update(time_state, arg1, arg2, arg3)
-- If _result_ is **"done"**, _true\_lapse_ is also considered. If present, it indicates how
-- much time actually passed before the update terminated, and will replace the current time
-- lapse (assuming it is a shorter lapse and non-negative).
--
-- _time\_state_ is a table with the following fields:
--
-- * **time**: The current value of _time_.
-- * **lapse**: The current value of _lapse_ (possibly reduced by _done_).
--
-- @callable done Test performed on each iteration, called as
--    is_done[, true_lapse] = done([time_state, ]arg1, arg2, arg3)
-- If _is\_done_ is true, the loop is ready to terminate. In that case, _true\_lapse_ may
-- also be considered, as per _update_; otherwise, the amount is assumed to be 0, i.e. the
-- loop terminated instantly. If _true\_lapse_ &gt; 0, _update_ will still be called, using
-- the narrowed time lapse.
--
-- _time\_state_ is as per _update_, except the **lapse** amount will be the initial value
-- for the current iteration.
--
-- @ptable config As per @{BasicBody}, a **use_time** field is also examined. If this is
-- true, _done_ accepts the _time\_state_ argument and handles _true\_lapse_ on termination.
-- @param arg1 Argument #1...
-- @param arg2 ...#2...
-- @param arg3 ...and #3.
-- @treturn boolean Operation concluded normally?
-- TODO: needs revision
function M.TimedBody (update, done, arg1, arg2, arg3)
	assert(meta.CanCall(done), "Uncallable done")
	assert(update == nil or meta.CanCall(update), "Uncallable update")

  local darg1, darg2, darg3 = GetDoneArgs(arg1, arg2, arg3)
	local time, yvalue, notted = 0, YieldValue, not not ShouldNegate -- cf. note in BasicBody()
	local func, deduct = Lapse or NoLapse, Deduct or NoDeduct

	ShouldNegate, YieldValue = nil

	while true do
		local lapse = func()

		IterationTime, IterationLapse = time, lapse -- make available to GetIteration*

		local dresult, spent_finishing = done(darg1, darg2, darg3)
		local finished = not dresult == notted -- cf. note in BasicBody()

		-- If the loop is ready to terminate, narrow the lapse to the time actually spent.
		-- By default, we assume the iteration ended immediately; however, amounts up to
		-- the current value are allowed (capping anything higher).
    if finished then
      lapse = Clamp(spent_finishing or 0, lapse)
    end

		-- Perform any user-defined update, if any time remains on this iteration. If this
		-- triggers an early out from the loop, we might want to narrow the lapse further.
		local uresult, spent_updating

		if update ~= nil and lapse > 0 then
			IterationTime, IterationLapse = time, lapse -- make available to GetIteration*

			uresult, spent_updating = update(arg1, arg2, arg3)

      if uresult == "done" and spent_updating then
        lapse = Clamp(spent_updating, lapse)
      end
		end

		deduct(lapse)

		if finished or uresult == "done" then
			return uresult ~= "done" -- an early out trumps finishing, even when both conditions hold
		else
      time = time + lapse

      yield(yvalue)
    end
	end
end

--
--
--

local function AuxWait (duration)
	local time = _GetIterationTime_()

	return time + _GetIterationLapse_() >= duration, duration - time
end

--- Wait for some time to pass.
--
-- Built on top of @{TimedBody}.
-- @number duration Time to wait.
-- @tparam ?|callable|nil update Update logic, called as
--    update(time_state, duration, arg)
-- with _time\_state_ as per @{TimedBody}.
--
-- If absent, this is a no-op.
-- @param arg Argument.
-- @treturn boolean The wait completed?
function M.Wait (duration, update, arg)
  DoneArgIndex = nil

	return _TimedBody_(update, AuxWait, duration, arg)
end

--
--
--

local function ChooseBody ()
	local body = UsingTime and _TimedBody_ or _BasicBody_

	UsingTime = nil

	return body
end

local function Index (t, k)
	return t[k]
end

--- Wait until `test(arg1, arg2)` is true.
-- @callable test Test function, with the same signature as _update_. If it returns
-- true, the wait terminates.
-- @tparam ?|callable|nil update Optional update logic, called as
--    update(arg1, arg2)
-- @param arg1 Argument #1...
-- @param arg2 ...and #2.
-- @treturn boolean The test passed?
-- @see SetUsingTime
function M.WaitUntil (test, update, arg1, arg2)
  DoneArgIndex = DoneArg ~= nil and 1

	return ChooseBody()(update, test, arg1, arg2)
end

--
--
--

--- Wait until `object[name]` is true.
-- @param object
-- @param name
-- @callable update Optional update logic, called as
--    update(object, name, arg)
-- @param arg Argument.
-- @treturn boolean The property became true?
-- @see SetUsingTime
function M.WaitUntilPropertyTrue (object, name, update, arg)
  DoneArgIndex = nil

	return ChooseBody()(update, Index, object, name, arg)
end

--
--
--

--- Wait while `test(arg1, arg2)` is true.
-- @callable test Test function, with the same signature as _update_. If it returns
-- false, the wait terminates.
-- @tparam ?|callable|nil update Optional update logic, called as
--    update(arg1, arg2)
-- @param arg1 Argument #1...
-- @param arg2 ...and #2.
-- @treturn boolean The test returned false?
-- @see SetUsingTime
function M.WaitWhile (test, update, arg1, arg2)
	ShouldNegate, DoneArgIndex = true, DoneArg ~= nil and 1

	return ChooseBody()(update, test, arg1, arg2)
end

--
--
--

--- Wait while `object[name]` is true.
--
-- Built on top of @{BasicBody}.
-- @param object
-- @param name
-- @callable update Optional update logic, called as
--    update(object, name, arg)
-- @param arg Argument.
-- @treturn boolean The property became false?
-- @see SetUsingTime
function M.WaitWhilePropertyTrue (object, name, update, arg)
	ShouldNegate, DoneArgIndex = true

	return ChooseBody()(update, Index, object, name, arg)
end

--
--
--

_BasicBody_ = M.BasicBody
_GetIterationLapse_ = M.GetIterationLapse
_GetIterationTime_ = M.GetIterationTime
_TimedBody_ = M.TimedBody

return M