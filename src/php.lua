package.path = '?.lua;src/?.lua'

io = require'io'
inspect = require'inspect'
local tokenizer = require'tokenizer'

local function dump(x)
	io.write(inspect(x) .. '\n')
end

local args = {...}
--[[
local script_file = io.open(args[1], "r")
local script_buf = script_file:read("*a")
local tokenized = tokenizer.script:match(script_buf)
dump(tokenized)
last_idx = 0
for i,j in ipairs(tokenized) do
	last_idx = j[2]
end
if last_idx ~= 1 + script_buf:len() then
	error("tokenizer didn't reach eof")
end
]]
-- --[[
local split = {}
local i0 = 1
local list = args[1]
local listlen = list:len()
for i=1,listlen do
	if list:sub(i,i) == '\n' then
		table.insert(split, list:sub(i0, i - 1))
		i0 = i + 1
	end
	if i == listlen then
		table.insert(split, list:sub(i0, i))
	end
end
for x,arg in ipairs(split) do
	local status, err = pcall(function()
		local script_file = io.open(arg, "r")
		local script_buf = script_file:read("*a")
		local tokenized = tokenizer.script:match(script_buf)
		last_idx = 0
		for i,j in ipairs(tokenized) do
			last_idx = j[2]
		end
		if last_idx ~= 1 + script_buf:len() then
			error("tokenizer didn't reach eof")
		end
	end)
	if not status then
		io.write(string.format("error tokenizing %s: %s\n", arg, err))
	end
end
-- ]]