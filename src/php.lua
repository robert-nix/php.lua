package.path = package.path .. ';?.lua;src/?.lua'

io = require'io'
inspect = require'inspect'
local tokenizer = require'tokenizer'

function dump(x)
	io.write(inspect(x) .. '\n')
end

local args = {...}
if args[1] ~= 'batch' then
	local context = { file_name = args[2] }
	local script_file = io.open(args[2], "r")
	local script_buf = script_file:read("*a")
	context.file_buf = script_buf
	local tokenized = tokenizer.script:match(script_buf)
	local tree = require("astbuilder").build_ast(context, tokenized)
	dump(tree)
	last_idx = 0
	for i,j in ipairs(tokenized) do
		last_idx = j[2]
	end
	if last_idx ~= 1 + script_buf:len() then
		error("tokenizer didn't reach eof")
	end
else
	local split = {}
	local i0 = 1
	local list = args[2]
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
	for _, arg in ipairs(split) do
		io.write(arg .. '   ')
		io.flush()
		local status, err = pcall(function()
			local context = { file_name = arg }
			local script_file = io.open(arg, "r")
			local script_buf = script_file:read("*a")
			context.file_buf = script_buf
			local tokenized = tokenizer.script:match(script_buf)
			last_idx = 0
			for i,j in ipairs(tokenized) do
				last_idx = j[2]
			end
			if last_idx ~= 1 + script_buf:len() then
				error("tokenizer didn't reach eof")
			end
			local tree = require("astbuilder").build_ast(context, tokenized)
		end)
		io.write('\r')
		if not status then
			io.write(string.format("error tokenizing %s: %s\n", arg, err))
		end
	end
end
