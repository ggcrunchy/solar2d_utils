--- Various audio-related operations.

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
local random = math.random
local rawequal = rawequal
local remove = table.remove
local running = coroutine.running
local setmetatable = setmetatable
local tonumber = tonumber
local type = type

-- Extension imports --
local indexOf = table.indexOf

-- Modules --
local directories = require("s3_utils.directories")

-- Solar2D globals --
local audio = audio
local system = system
local timer = timer

-- Exports --
local M = {}

--
--
--

local IsQuiet

--- Enable or disable audio. When disabled, calls from this module will be no-ops.
-- @bool enable Enable audio logic?
function M.Enable (enable)
	IsQuiet = not enable
end

local function RemoveDeadChannels (channels)
	local n = #channels

	for i = n, 1, -1 do
		if not audio.isChannelActive(channels[i]) then
			channels[i] = channels[n]
			n, channels[n] = n - 1
		end
	end
end

local Opts = {}

local DeferredPlay

local function Backfill3 (t, i, n)
	t[i], t[i + 1], t[i + 2] = t[n - 2], t[n - 1], t[n]
	t[n - 2], t[n - 1], t[n] = nil

	return n - 3
end

local function AuxPlay (group, handles, name)
	local channels = group.m_channels

	--
	RemoveDeadChannels(channels)

	--
	local handle = handles[name]

	if handle == "loading" or running() then
		if not DeferredPlay then
			DeferredPlay = {}

			DeferredPlay.timer = timer.performWithDelay(5, function(event)
				local n = #DeferredPlay

				for i = n - 2, 1, -3 do
					local dgroup, dhandles, dname = DeferredPlay[i], DeferredPlay[i + 1], DeferredPlay[i + 2]
					local handle = dhandles[dname]

					if handle ~= "loading" then
						if handle ~= "removed" then
							AuxPlay(dgroup, dhandles, dname)
						end

						n = Backfill3(DeferredPlay, i, n)
					end
				end

				DeferredPlay.nframes = DeferredPlay.nframes - 1

				if DeferredPlay.nframes == 0 then
					timer.cancel(event.source)

					DeferredPlay = nil
				end
			end, 0)
		end

		DeferredPlay.nframes = (DeferredPlay.nframes or 0) + 10
		DeferredPlay[#DeferredPlay + 1] = group
		DeferredPlay[#DeferredPlay + 1] = handles
		DeferredPlay[#DeferredPlay + 1] = name
	elseif handle and handle ~= "removed" then
		local info = group.m_info[name]

		--
		if info.m_wait then
			local now = system.getTimer()

			if info.m_time and now - info.m_time < info.m_wait then
				return
			else
				info.m_time = now
			end
		end

		--
		local complete = info.m_on_complete

		if not IsQuiet then
			local nloops, opts = info.m_loop_count

			if nloops or complete then
				Opts.loops, Opts.onComplete, opts = nloops, complete, Opts -- TODO: verify on_complete use cases...
			end

			channels[#channels + 1] = audio.play(handle, opts) -- TODO: in theory this could fail due to channel starvation, but some of these might
																-- be evicted after fading out... (just keep counter, I suppose, and dole them out)

		-- If audio is off, but the sound still has on-complete logic, approximate the play
		-- time with a timer, and be ready to handle stops.
		elseif complete then
			local qs, state, nloops = group.m_quiet_state or {}, { complete = complete }, info.m_loop_count or 1

			if nloops > 0 then
				state.timer = timer.performWithDelay(nloops * audio.getDuration(handle), function()
					complete(true)

					state.complete, state.timer = nil
				end)
			end

			qs[#qs + 1], group.m_quiet_state = state, qs
		end
	end
end

local None = {}

-- Plays a bit of audio (unless it has already played too recently)
local function Play (group, handles, name, delay)
	local play_timer

	if rawequal(handles, None) then
		return nil
	elseif delay then
		play_timer = timer.performWithDelay(delay, function()
			AuxPlay(group, handles, name)
		end)
	else
		AuxPlay(group, handles, name)
	end

	return play_timer
end

local function ClearHandles (handles)
	for k, v in pairs(handles) do
		if type(v) ~= "string" then
			audio.dispose(v)
		end

		handles[k] = "removed"
	end
end

local DeferredLoad

local function LoadInfo (handles, info)
	for k, sinfo in pairs(info) do
		local func = sinfo.m_is_streaming and "loadStream" or "loadSound"

		handles[k] = audio[func](sinfo.m_file, sinfo.m_base)
	end
end

-- Groups of related audio, e.g. all sounds related to a type of object --
local Groups = {}

local SoundGroup = {}

SoundGroup.__index = SoundGroup

--- DOCME
function SoundGroup:IsActive ()
	if IsQuiet then
		local qs = self.m_quiet_state

		for i = 1, #(qs or "") do
			if qs[i].complete then
				return true
			end
		end
	else
		local channels = self.m_channels

		for i = 1, #channels do
			if audio.isChannelActive(channels[i]) then
				return true
			end
		end
	end

	return false
end

--- Initializes a group. This must be called before the group is used to play any sounds.
--
-- After loading, this will be a no-op until the level is unloaded.
function SoundGroup:Load ()
	if rawequal(self.m_handles, None) then
		local handles = {}

		if running() then
			if not DeferredLoad then
				DeferredLoad = {}

				DeferredLoad.timer = timer.performWithDelay(5, function(event)
					for i = #DeferredLoad - 1, 1, -2 do
						LoadInfo(DeferredLoad[i], DeferredLoad[i + 1])

						DeferredLoad[i], DeferredLoad[i + 1] = nil
					end

					DeferredLoad.nframes = DeferredLoad.nframes - 1

					if DeferredLoad.nframes == 0 then
						timer.cancel(event.source)

						DeferredLoad = nil
					end
				end, 0)
			end

			for k in pairs(self.m_info) do
				handles[k] = "loading"
			end

			DeferredLoad.nframes = (DeferredLoad.nframes or 0) + 10
			DeferredLoad[#DeferredLoad + 1] = handles
			DeferredLoad[#DeferredLoad + 1] = self.m_info
		else
			LoadInfo(handles, self.m_info)
		end

		self.m_handles = handles
	end
end

--- DOCME
function SoundGroup:PauseAll ()
	if IsQuiet then
		local qs = self.m_quiet_state

		for i = 1, #(qs or "") do
			local state = qs[i]

			if state.timer and not state.is_paused then
				timer.pause(state.timer)

				state.is_paused = true
			end
		end
	else
		for _, channel in ipairs(self.m_channels) do
			if audio.isChannelPlaying(channel) then
				audio.pause(channel)
			end
		end
	end
end

--- Utility.
-- @param name Name of sound to play.
-- @uint[opt] delay Optional delay, in milliseconds, before playing.
-- @treturn TimerHandle A timer that may be cancelled, or **nil** if _delay_ was absent.
function SoundGroup:PlaySound (name, delay)
	return Play(self, assert(self.m_handles, "Sound group not loaded"), name, delay)
end

--- If the group has an array part, plays one of its sounds.
-- @uint[opt] delay Optional delay, in milliseconds, before playing.
-- @treturn TimerHandle A timer that may be cancelled, or **nil** if _delay_ was absent.
function SoundGroup:RandomSound (delay)
	local handles = assert(self.m_handles, "Sound group not loaded")

	return Play(self, handles, random(#handles), delay)
end

--- DOCME
function SoundGroup:Remove ()
	local index = indexOf(Groups, self)

	if index then
		self:StopAll()

		ClearHandles(self.m_handles)

		remove(Groups, index)
	end
end

--- DOCME
function SoundGroup:ResumeAll ()
	if IsQuiet then
		local qs = self.m_quiet_state

		for i = 1, #(qs or "") do
			local state = qs[i]

			if state.timer and state.is_paused then
				timer.resume(state.timer)

				state.is_paused = false
			end
		end
	else
		for _, channel in ipairs(self.m_channels) do
			if audio.isChannelPaused(channel) then
				audio.resume(channel)
			end
		end
	end
end

-- Rewind?

local function StopAllQuiet (group)
	local qs = group.m_quiet_state

	for i = #(qs or ""), 1, -1 do
		local state = qs[i]

		if state.timer then
			timer.cancel(state.timer)
		end

		if state.complete then
			state.complete(false)
		end

		qs[i] = nil
	end
end

--- DOCME
function SoundGroup:StopAll ()
	if IsQuiet then
		StopAllQuiet(self)
	else
		local channels = self.m_channels

		for i = #channels, 1, -1 do
			if audio.isChannelActive(channels[i]) then
				audio.stop(channels[i])
			end

			channels[i] = nil
		end
	end
end

-- Common logic for making a sound group
local function AuxNewSoundGroup (info)
	local group = { m_channels = {}, m_handles = None, m_info = info }

	Groups[#Groups + 1] = group

	return setmetatable(group, SoundGroup)
end

--
local function AddItem (name, sinfo, prefix, base, info)
	local itype = type(sinfo)

	if itype == "string" then
		info[name] = { m_file = prefix .. sinfo, m_base = base }
	else
		assert(itype == "table", "Non-table sound info")

		local nloops

		if sinfo.loops == "forever" then
			nloops = -1
		else
			nloops = tonumber(sinfo.loops)

			assert(nloops ~= 0, "Bad loop count")
		end

		info[name] = {
			m_base = base,
			m_file = prefix .. sinfo.file,
			m_is_streaming = not not sinfo.streaming,
			m_loop_count = nloops,
			m_on_complete = sinfo.on_complete,
			m_wait = sinfo.wait
		}
	end
end

local Reserved = { base = true, module = true, path = true }

local function AddToGroup (info, sounds)
	local base, path = sounds.base, sounds.path

	path = directories.FromModule(sounds.module, directories.GetNamedPath(path) or path)

	for k, v in pairs(sounds) do
		if not Reserved[k] then
			AddItem(k, v, path, base, info)
		end
	end
end

--- Builds a new group of sounds. A group is lazy: unless loaded, it only contains some
-- information about its sounds, since many groups will only be used at certain times.
-- @ptable sounds Name-value pairs of sound information. The names are used to play the
-- sounds.
--
-- If a value is a string, it is assumed to be the filename of the sound to play.
--
-- Otherwise, the value must be a table, and the filename is instead found at the **file**
-- key. Optional members include:
--
-- * **loops**: Number of times to loop, or **"forever"** to loop indefinitely.
-- * **is_streaming**: Boolean: Is this a streaming sound?
-- * **on_complete**: Callable. Called as `on_complete(done)` when the sound has finished
-- playing, with _done_ being true if the sound completed normally.
-- * **wait**: A delay, in milliseconds. Attempts to play the sound again are ignored until
-- this interval has elapsed.
-- @treturn table Sound group object.
function M.NewSoundGroup (sounds)
	local info = {}

	AddToGroup(info, sounds)

	return AuxNewSoundGroup(info)
end

--- DOCME
function M.NewSoundGroup_Multi (arr)
	local info = {}

	for _, sounds in ipairs(arr) do
		AddToGroup(info, sounds)
	end

	return AuxNewSoundGroup(info)
end

-- TODO: Menu audio

local function ClearDeferredItems ()
	if DeferredLoad then
		timer.cancel(DeferredLoad.timer)
	end

	if DeferredPlay then
		timer.cancel(DeferredPlay.timer)
	end

	DeferredPlay, DeferredLoad = nil
end

local TaperFrames = 10

local TaperList

local VolumeOpts = {}

local function TaperOut (group, clear) -- TODO: didn't realize there was an audio.fadeOut()! :P
	local channels, handles = group.m_channels, group.m_handles

	if #channels > 0 then
		TaperList = TaperList or {
			timer = timer.performWithDelay(50, function(event)
				local nlist = #TaperList

				for i = nlist - 2, 1, -3 do
					local channels, handles, frame = TaperList[i], TaperList[i + 1], TaperList[i + 2] - 1
					local n = #channels

					for j = n, 1, -1 do
						local channel, volume = channels[j]

						VolumeOpts.channel = channel

						if audio.isChannelActive(channel) then
							if frame > 0 then
								TaperList[i + 2], volume = frame, frame / TaperFrames
							else
								audio.stop(channel)
							end
						end

						audio.setVolume(volume or 1, VolumeOpts) -- restore to 1 if removing

						if not volume then
							channels[j] = channels[n]
							n, channels[n] = n - 1
						end
					end

					if #channels == 0 then
						nlist = Backfill3(TaperList, i, nlist)

						if handles then
							ClearHandles(handles)
						end
					end
				end

				if nlist == 0 then
					timer.cancel(event.source)

					TaperList = nil
				end
			end, 0)
		}

		TaperList[#TaperList + 1] = channels
		TaperList[#TaperList + 1] = clear and handles or false
		TaperList[#TaperList + 1] = TaperFrames
	elseif clear then
		ClearHandles(handles)
	end
end

-- Leave Level response
local function LeaveLevel ()
	for _, group in ipairs(Groups) do
		if IsQuiet then
			StopAllQuiet(group)
			ClearHandles(group.m_handles)
		else
			TaperOut(group, true)
		end

		group.m_handles = None
	end

	ClearDeferredItems()
end

for k, v in pairs{
	leave_level = LeaveLevel,

	reset_level = function()
		for _, group in ipairs(Groups) do
			if IsQuiet then
				StopAllQuiet(group)
			else
				TaperOut(group)
			end

			for _, sinfo in pairs(group.m_info) do
				sinfo.m_time = nil
			end
		end

		ClearDeferredItems()
	end
} do
	Runtime:addEventListener(k, v)
end

return M