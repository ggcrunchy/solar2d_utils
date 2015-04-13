--- Utilities for reading and writing data, with various storage methods.

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
local type = type

-- Modules --
local sqlite_db = require("tektite_core.sqlite_db")

-- Corona modules --
local json = require("json")
local sqlite3 = require("sqlite3")

-- Exports --
local M = {}

--- DOCME
function M.GetData (object)
	return object.m_data
end

-- read sources: table (parent, key); database (table, key, is_open); embedded (chunk type, key)
-- write methods: to table / JSON; to database; to same file
-- remove: if missing read source
-- tEXt:
-- Keyword:        1-79 bytes (character string)
--   Null separator: 1 byte
--   Text:           n bytes (character string)

--
local function WithDatabase (method, args, func, vals)
	local db = args.data

	if method == "database_file" then
		db = sqlite3.open(db)
	end

	local res = func(db, args.table_name, args.key, vals)

	if method == "database_file" then
		db:close()
	end

	return res
end

-- Helper to read data out of a database table, if it exists
local function AuxRead (db, tname, key)
	return sqlite_db.TableExists(db, tname) and sqlite_db.GetOneValueInTable(db, tname, key)
end

--- DOCMEMORE
-- Tries to read file-related data from some source
function M.ReadData (method, data)
	-- Read a string out of a database (which may be opened) --
	-- data: { db = name ("") / db (userdata), table_name = "", key = "" }
	-- arg: filename
	if method == "database_file" or method == "database_handle" then
		data = WithDatabase(method, data, AuxRead)

	-- Read from PNG --
	-- data: { name = "", keyword = "" }
	elseif method == "image_metadata" then
		-- FindText(keyword)
	end

	-- If the data is not already a table, try to convert it to one.
	if type(data) == "string" then
		return json.decode(data)
	else
		return data
	end
end

--
local function AuxWrite (db, tname, filename, data)
	sqlite_db.InsertOrReplace_KeyData(db, tname, filename, data)
end

--- DOCME
function M.WriteData (method, data)
	local vals = json.encode(data.payload)

	-- Write a string into a database (which may be opened) --
	-- data: { db = name ("") / db (userdata), table_name = "", key = "" }
	if method == "database_file" or method == "database_handle" then
		WithDatabase(method, data, AuxWrite, vals)

	-- Write to PNG --
	-- data: { name = "", keyword = "" }
	elseif method == "image_metadata" then
		-- FindText(keyword)
	end

	return vals
end

-- Export the module.
return M