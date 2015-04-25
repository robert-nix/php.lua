package.path = '?.lua;src/?.lua'

io = require'io'
inspect = require'inspect'
local tokenizer = require'tokenizer'

local function dump(x)
	io.write(inspect(x) .. '\n')
end

local args = {...}
local script_file = io.open(args[1], "r")
local script_buf = script_file:read("*a")
dump(tokenizer.script:match(script_buf))
