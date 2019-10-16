--- This module wraps up some useful file functionality.

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
local max = math.max
local open = io.open
local pairs = pairs
local remove = os.remove
local type = type

-- Modules --
local strings = require("tektite_core.var.strings")

-- Corona globals --
local display = display
local system = system
local timer = timer

-- Corona modules --
local lfs = require("lfs")

-- Cached module references --
local _EnumerateFiles_
local _Exists_
local _Prefix_FromTable_

-- Exports --
local M = {}

--
--
--

-- Is this running on the simulator? --
local OnSimulator = system.getInfo("environment") == "simulator"

-- Helper to identify resource directory
local function IsResourceDir (base)
	return not base or base == system.ResourceDirectory
end

-- Helper to deal with paths on simulator
local PathForFile = system.pathForFile

if OnSimulator then
	function PathForFile (name, base)
		if IsResourceDir(base) then
			return system.pathForFile("") .. "/" .. name
		else
			return system.pathForFile(name, base)
		end
	end
end

--- DOCME
function M.AddDirectory (name, base)
	local path = system.pathForFile("", base)

	if lfs.attributes(path .. "/" .. name, "mode") ~= "directory" then -- <- slash needed? (should make consistent?)
		lfs.chdir(path)
		lfs.mkdir(name)
	end
end

--- Chooses a Corona base directory for a file. If the file is found under the "preferred"
-- directory, that one is used; otherwise, **system.ResourceDirectory** is used (**n.b.** the
-- file may or may not exist, in this case).
--
-- This is intended for simulator-side tests. On a device, **system.ResourceDirectory** is
-- always chosen.
-- @string name File path.
-- @param[opt=system.ResourceDirectory] pref_base Preferred directory base.
-- @treturn userdata If chosen, _pref\_base_; otherwise, **system.ResourceDirectory**.
function M.ChooseBaseDirOfFile (name, pref_base)
	if OnSimulator and pref_base and _Exists_(name, pref_base) then
		return pref_base
	else
		return system.ResourceDirectory
	end
end

--- Variant of @{ChooseBaseDirOfFile} which, in the presence of both versions of a file, will
-- prefer the directory base of the more recently modified file.
-- @string name File path.
-- @param[opt=system.ResourceDirectory] pref_base Preferred directory base.
-- @treturn userdata If chosen, _pref\_base_; otherwise, **system.ResourceDirectory**.
function M.ChooseBaseDirOfFile_Newer (name, pref_base)
	local base

	if OnSimulator and pref_base and _Exists_(name, pref_base) then
		base = pref_base

		if _Exists_(name, system.ResourceDirectory) then
			local pfile = system.pathForFile(name, pref_base)
			local rfile = system.pathForFile(name, system.ResourceDirectory)

			if lfs.attributes(pfile, "modification") < lfs.attributes(rfile, "modification") then
				base = system.ResourceDirectory
			end
		end
	end

	return base or system.ResourceDirectory
end

--- DOCME
-- TODO: Still doesn't seem to work...
function M.CopyFile (src_name, src_base, dst_name, dst_base)
	local src_path = PathForFile(src_name, src_base)
	local source = src_path and open(src_path, "rb")

	if source then
		local data = source:read("*a")
		local dst_path = data and PathForFile(dst_name, dst_base or system.DocumentsDirectory)
		local target = dst_path and open(dst_path, "wb")

		if target then
			target:write(data)
			target:close()
		end

		source:close()
	end
end

-- Enumeration choices --
local EnumFiles = {}

-- Helper to enumerate all files
local function EnumAll (enumerate, into, path)
	for name in enumerate(path) do
		into[#into + 1] = name
	end	
end

-- Helper to enumerate files matching extension
function EnumFiles.string (enumerate, into, path, ext)
	for name in enumerate(path) do
		if strings.EndsWith_AnyCase(name, ext) then
			into[#into + 1] = name
		end
	end	
end

-- Helper to enumerate files matching one of several extensions
function EnumFiles.table (enumerate, into, path, exts)
	for name in enumerate(path) do
		for _, ext in ipairs(exts) do
			if strings.EndsWith_AnyCase(name, ext) then
				into[#into + 1] = name

				break
			end
		end
	end	
end

-- Is this running on an Android device? --
local OnAndroid = system.getInfo("platform") == "android" and not OnSimulator

--- Enumerates files in a given directory.
-- @string path Directory path.
-- @ptable[opt] opts Enumeration options. Fields:
--
-- * **exts**: Extensions filter. If this is a string, only files ending in the string are
-- enumerated. If it is an array, only files ending in one of its strings (tried in order)
-- are enumerated. Otherwise, all files are enumerated.
-- * **base**: Directory base. If absent, **system.ResourcesDirectory**.
--
-- @ptable into If provided, files are appended here. Otherwise, a table is provided.
-- @treturn table Enumerated files.
function M.EnumerateFiles (path, opts, into)
	local base, exts = opts and opts.base, opts and opts.exts

	into = into or {}

	-- In the resource directory on Android, try to build a file list from the assets.
	local enumerate, respath

	if OnAndroid and IsResourceDir(base) then
		-- TODO: AssetReader

	-- Otherwise, read the directory if it exists.
	else
		respath = PathForFile(path, base)
		enumerate = respath and lfs.attributes(respath, "mode") == "directory" and lfs.dir
	end

	-- Enumerate the files with the appropriate iterator.
	if enumerate then
		(EnumFiles[type(exts)] or EnumAll)(enumerate, into, respath, exts)
	end

	return into
end

-- ^^^ TODO: Recursion, etc.

--- DOCME
-- @string name
-- @param[opt=system.ResourceDirectory] base Directory base.
-- @treturn boolean Does the file exist?
-- ^^ TODO: Works for directories?
function M.Exists (name, base)
	local path = PathForFile(name, base)
	local file = path and open(path)

	if file then
		file:close()
	end

	return file ~= nil
end

-- Attempts to read the binary contents of a file
local function GetFileContents (name)
	local file, contents = open(name, "rb")

	if file then
		contents = file:read("*a")

		file:close()
	end

	return contents
end

--- DOCME
function M.GetContents (path, base)
	if OnAndroid and IsResourceDir(base) then
		-- TODO: AssetReader
	else
		return GetFileContents(PathForFile(path, base))
	end
end

--- DOCME
-- @function PathForFile
M.PathForFile = PathForFile

--
local function AddSlash (str)
	return strings.EndsWith(str, "/") and str or str .. "/"
end

--
local function FinalPrefix (prefix)
	return #prefix > 0 and AddSlash(prefix) or ""
end

--
local function ModuleToFolder (mod)
	return strings.RemoveLastSubstring(mod or "", "%.", "/")
end

--- DOCME
function M.Prefix_FromModule (mod)
	return FinalPrefix(ModuleToFolder(mod))
end

--- DOCME
function M.Prefix_FromModuleAndPath (mod, path)
	mod = ModuleToFolder(mod)

	if #mod > 0 then
		path = AddSlash(mod) .. path
	end

	return FinalPrefix(path)
end

--- DOCME
function M.Prefix_FromTable (t)
	local base, mod, prefix = t._base, ModuleToFolder(t._here), t._prefix or ""

	if #mod > 0 then
		assert(IsResourceDir(base), "Mixing modules and non-resource asset paths")

		prefix = AddSlash(mod) .. prefix
	end

	return FinalPrefix(prefix), base
end

--- DOCME
function M.Prefix_WithTableDo (t, func, arg)
	local prefix, base = _Prefix_FromTable_(t)

	for k, v in pairs(t) do
		if k ~= "_base" and k ~= "_here" and k ~= "_prefix" then
			func(k, v, prefix, base, arg)
		end
	end
end

-- ^^ TODO: Assumes path is a directory...
-- Should ignore non-troublesome extensions

-- @param[opt=system.ResourceDirectory] base Directory base.

-- Names of files (and associated guard objects, if any) awaiting disposal --
local Trash

-- Helper to try to remove and clean up after disposed-of files
local function TryToRemove (name, object, dir)
	if _Exists_(name, dir) then
		if display.isValid(object) then
			return false
		end

		return remove(PathForFile(name, dir)) ~= nil
	end

	return true
end

-- Helper to queue up files for disposal
local function AuxPutInTrash (name, object, base)
	base = base or system.ResourceDirectory

	assert(OnSimulator or base ~= system.ResourceDirectory, "Attempt to dispose of resource file")

	-- Initialize the state, on the first time or if it went dormant.
	if not Trash then
		Trash = {}

		timer.performWithDelay(200, function(event)
			local total = 0

			-- Try to remove a few files from each base directory.
			for dir, pile in pairs(Trash) do
				local n = #pile

				for i = n, max(n - 20, 1), -2 do
					if TryToRemove(pile[i - 1], pile[i], dir) then
						pile[i - 1], pile[i] = pile[n - 1], pile[n]
						n, pile[n - 1], pile[n] = n - 2
					end
				end

				total = total + n
			end

			-- If no files remain, remove the state until it is needed again.
			if total == 0 then
				Trash = nil

				timer.cancel(event.source)
			end
		end, 0)
	end

	-- Add the file name, plus any guard object, to a disposal list (created, if necessary).
	local pile = Trash[base] or {}

	pile[#pile + 1] = name
	pile[#pile + 1] = object

	Trash[base] = pile
end

--- DOCME
-- @string name
-- @param[opt=system.ResourceDirectory] base
function M.PutInTrash (name, base)
	AuxPutInTrash(name, false, base)
end

--- Variant of @{PutInTrash} which accepts a display object as a "guard".
--
-- The guard is assumed to depend upon the file, e.g. the latter supplied its assets. Thus,
-- until the guard is removed, the trash logic will leave the file alone. The file may cease
-- to exist via external means, of course, in which case its state is evicted.
-- @string name
-- @pobject object
-- @param[opt=system.ResourceDirectory] base
function M.PutInTrash_Guard (name, object, base)
	AuxPutInTrash(name, assert(object, "Missing guard object"), base)
end

--- Launches a timer to watch a file or directory for modifications.
-- @string path File or directory path.
-- @callable func On modification, this is called as `func(path, how)`, where _how_ is one of:
--
-- * **"created"**: File was created (or re-created) once watching was begun.
-- * **"deleted"**: File was deleted once watching was begun.
-- * **"modified"**: File was modified while being watched.
-- @ptable[opt] opts Watch options. Fields:
--
-- * **base**: As per @{EnumerateFiles}.
-- @treturn TimerHandle A timer, which may be cancelled.
function M.WatchForFileModification (path, func, opts)
	local base = opts and opts.base
	local is_res_dir, respath, modtime = IsResourceDir(base)

	if OnSimulator or not is_res_dir then
		return timer.performWithDelay(50, function()
			respath = respath or PathForFile(path, base)

			local now = respath and lfs.attributes(respath, "modification")

			-- Able to find the file: if the modification time has changed since the last query,
			-- alert the watcher (this is skipped on the first iteration). If the file is suddenly
			-- found after being missing, tell the watcher the file was created.
			if now then
				if modtime and now ~= modtime then
					func(path, "modified")
				elseif modtime == false then
					func(path, "created")
				end

				modtime = now

			-- Otherwise, put the file into a missing state. If the file just went missing, tell
			-- the watcher it was deleted.
			else
				if modtime then
					func(path, "deleted")
				end

				modtime = false
			end
		end, 0)
		-- ^^ TODO: Some variety in how this is handled in file vs. directory, especially "modified"
	-- Resource directory is read-only on device, so timer is a no-op.
	else
		return timer.performWithDelay(0, function() end)
	end
end

_EnumerateFiles_ = M.EnumerateFiles
_Exists_ = M.Exists
_Prefix_FromTable_ = M.Prefix_FromTable

return M