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
local type = type

-- Extension imports --
local indexOf = table.indexOf

-- Modules --
local array_funcs = require("tektite_core.array.funcs")
local file = require("corona_utils.file")

-- Corona globals --
local audio = audio
local system = system
local timer = timer

-- Exports --
local M = {}

-- --
local IsQuiet

--- Enable or disable audio. When disabled, calls from this module will be no-ops.
-- @bool enable Enable audio logic?
function M.Enable (enable)
	IsQuiet = not enable
end

--
local function RemoveDeadChannels (channels)
	local n = #channels

	for i = n, 1, -1 do
		if not audio.isChannelActive(channels[i]) then
			array_funcs.Backfill(channels, i)
		end
	end
end

-- Common play logic
local function AuxPlay (group, handles, name)
	local channels, handle = group.m_channels, handles[name]

	RemoveDeadChannels(channels)

	if handle and not IsQuiet then
		local info = group.m_info[name]

		if info.m_wait then
			local now = system.getTimer()

			if info.m_time and now - info.m_time < info.m_wait then
				return
			else
				info.m_time = now
			end
		end

		channels[#channels + 1] = audio.play(handle)
	end
end

-- Plays a bit of audio (unless it has already played too recently)
local function Play (group, handles, name, delay)
	local play_timer

	if delay then
		play_timer = timer.performWithDelay(delay, function()
			AuxPlay(group, handles, name)
		end)
	else
		AuxPlay(group, handles, name)
	end

	return play_timer
end

--
local function ClearChannels (group)
	local channels = group.m_channels

	for i = #channels, 1, -1 do
		if audio.isChannelActive(channels[i]) then
			audio.stop(channels[i])
		end

		channels[i] = nil
	end
end

--
local function ClearHandles (handles)
	if handles then
		for _, v in pairs(handles) do
			audio.dispose(v)
		end
	end
end

-- Groups of related audio, e.g. all sounds related to a type of object --
local Groups = {}

-- Common logic for making a sound group
local function AuxNewSoundGroup (info)
	-- Streamline the sounds list into a group.
	local SoundGroup = { m_channels = {}, m_info = info }

	--- Initializes a group. This must be called before the group is used to play any sounds.
	--
	-- After loading, this will be a no-op until the level is unloaded.
	function SoundGroup:Load ()
		if not self.m_handles then
			local handles = {}

			for k, sinfo in pairs(self.m_info) do
				handles[k] = audio.loadSound(sinfo.m_file, sinfo.m_base)
			end

			self.m_handles = handles
		end
	end

	--- Utility.
	-- @param name Name of sound to play.
	-- @uint[opt] delay Optional delay, in milliseconds, before playing.
	-- @treturn TimerHandle A timer that may be cancelled, or **nil** if _delay_ was absent.
	function SoundGroup:PlaySound (name, delay)
		return Play(self, assert(self.m_handles, "Sound group not loaded"), name, delay)
	end

	--- DOCME
	function SoundGroup:Remove ()
		local index = indexOf(Groups, self)

		if index then
			ClearChannels(self.m_channels)
			ClearHandles(self.m_handles)

			array_funcs.Backfill(Groups, index)
		end
	end

	--- If the group has an array part, plays one of its sounds.
	-- @uint[opt] delay Optional delay, in milliseconds, before playing.
	-- @treturn TimerHandle A timer that may be cancelled, or **nil** if _delay_ was absent.
	function SoundGroup:RandomSound (delay)
		local handles = assert(self.m_handles, "Sound group not loaded")

		return Play(self, handles, random(#handles), delay)
	end

	Groups[#Groups + 1] = SoundGroup

	return SoundGroup
end

--
local function AddItem (name, sinfo, prefix, base, info)
	local itype = type(sinfo)

	if itype == "string" then
		info[name] = { m_file = prefix .. sinfo, m_base = base }
	else
		assert(itype == "table", "Non-table sound info")

		info[name] = { m_file = prefix .. sinfo.file, m_base = base, m_wait = sinfo.wait }
	end
end

--
local function AddToGroup (info, sounds)
	file.Prefix_WithTableDo(sounds, AddItem, info)
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
-- * **wait**: A delay, in milliseconds. Attempts to play the sound again are ignored until
-- this interval has elapsed.
-- it is ignored.
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

-- Leave Level response
local function LeaveLevel ()
	for _, group in ipairs(Groups) do
		ClearChannels(group.m_channels)
		ClearHandles(group.m_handles)

		group.m_handles = nil
	end
end

-- Listen to events.
for k, v in pairs{
	-- Leave Level --
	leave_level = LeaveLevel,

	-- Leave Menus --
	leave_menus = LeaveLevel,

	-- Reset Level --
	reset_level = function()
		for _, group in ipairs(Groups) do
			ClearChannels(group.m_channels)

			for _, sinfo in pairs(group.m_info) do
				sinfo.time = nil
			end
		end
	end
} do
	Runtime:addEventListener(k, v)
end

-- Export the module.
return M