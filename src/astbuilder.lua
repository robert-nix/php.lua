
-- Takes a list of scripts and inserts echo statements where necessary to
-- flatten the scripts into a list of tokens, while also filtering out non-
-- program elements, i.e. whitespace and comments
local function flatten_scripts(scripts)
	local tokens = {}
	local function add_echo(s)
		table.insert(tokens, {-1, -1, keyword = "echo" })
		if s ~= nil and s:len() > 0 then
			table.insert(tokens, {-1, -1,
				literal = { dq_string = {
					prefix = "",
					value = { s }
				}}
			})
			table.insert(tokens, {-1, -1, operator = ";" })
		end
	end
	for _, script in ipairs(scripts) do
		if script.pre_text:len() > 0 then
			add_echo(script.pre_text)
		end
		if script.tag == "<?=" then
			add_echo()
		end
		for _, token in ipairs(script.script) do
			if not token.whitespace and not token.comment then
				table.insert(tokens, token)
			end
		end
		if script.tag == "<?=" then
			table.insert(tokens, {-1, -1, operator = ";" })
		end
		if script.post_text then
			add_echo(script.post_text)
		end
	end
	return tokens
end

-- Turns string literals with variable substitutions and converts them to use
-- the concatenation operator
local function flatten_strings(tokens)
	local result = {}
	local function add_cat(i)
		table.insert(result, {i, i, operator = "."})
	end
	local function add_strval_start(i)
		-- qualified name, non-relative
		table.insert(result, {i, i, qualified = {false, "strval"}})
		table.insert(result, {i, i, operator = "("})
	end
	local function add_strval_final(i)
		table.insert(result, {i, i, operator = ")"})
	end
	local function handle_subst_name(subst)
		add_strval_start(subst[1])
		table.insert(result, {subst[1], subst[2],
			name = {
				variable = subst.name.variable }})
		if subst.name.offset then
			table.insert(result, {subst[1], subst[2], operator = "["})
			if subst.name.offset.name then
				table.insert(result, {subst[1], subst[2],
					literal = { dq_string = subst.name.offset.name }})
			else
				table.insert(result, {subst[1], subst[2],
					literal = subst.name.offset})
			end
			table.insert(result, {subst[1], subst[2], operator = "]"})
		end
		if subst.name.property then
			table.insert(result, {subst[1], subst[2], operator = "->"})
			table.insert(result, {subst[1], subst[2],
				name = subst.name.property})
		end
		add_strval_final(subst[2])
	end
	local function handle_subst_braced(subst)
		add_strval_start(subst[1])
		-- worth noting, the stuff inside the brace expr is much more limited
		-- under Zend... might want to implement that restriction at some point,
		-- but not going to worry about it for now.
		local offs = 0
		for _, t in ipairs(subst.braced.expr) do
			if t[1] ~= nil then
				t[1] = subst[1] + t[1] + offs -- +1 from {
				t[2] = subst[1] + t[2] + offs
			else
				offs = t.variable:len()
				t[1] = subst[1] + 1
				t[2] = subst[1] + 1 + offs
			end
			table.insert(result, t)
		end
		add_strval_final(subst[2])
	end
	local function handle_subst_create_name(subst)
		add_strval_start(subst[1])
		table.insert(result, {subst[1], subst[1], operator = "$"})
		table.insert(result, {subst[1], subst[1], operator = "{"})
		local offs = 1
		for _, t in ipairs(subst.create_name.expr) do
			if t[1] ~= nil then
				t[1] = subst[1] + t[1] + offs -- +2 from ${
				t[2] = subst[1] + t[2] + offs
			else
				offs = t.variable:len()
				t[1] = subst[1] + 2
				t[2] = subst[1] + 1 + offs
			end
			table.insert(result, t)
		end
		table.insert(result, {subst[2], subst[2], operator = "}"})
		add_strval_final(subst[2])
	end
	local function handle_subst(subst)
		if subst.name then
			handle_subst_name(subst)
		elseif subst.braced then
			handle_subst_braced(subst)
		elseif subst.create_name then
			handle_subst_create_name(subst)
		end
	end
	for _, token in ipairs(tokens) do
		if token.literal then
			local literal = token.literal
			local first = true
			if literal.dq_string and
					type(literal.dq_string.value) == "table" then
				local prefix = literal.dq_string.prefix
				local last_idx = token[1] + 1
				for _, part in ipairs(literal.dq_string.value) do
					if type(part) == "table" then
						if not first then
							add_cat(part[1])
						else
							first = false
						end
						last_idx = part[2]
						handle_subst(part)
					else
						if not first then
							add_cat(last_idx)
						else
							first = false
						end
						table.insert(result, {last_idx, last_idx + part:len(),
							literal = { dq_string = {
								prefix = prefix,
								value = part
						}}})
						last_idx = last_idx + part:len()
					end
				end
			elseif literal.hd_string and
					type(literal.hd_string.value) == "table" then
				local last_idx = literal.hd_string[1] + 1
				for _, part in ipairs(literal.hd_string.value) do
					if type(part) == "table" then
						if not first then
							add_cat(part[1])
						else
							first = false
						end
						last_idx = literal.hd_string[1] + part[2]
						handle_subst(part)
					else
						if not first then
							add_cat(last_idx)
						else
							first = false
						end
						table.insert(result, {last_idx, last_idx + part:len(),
							literal = { dq_string = {
								prefix = "",
								value = part
						}}})
						last_idx = last_idx + part:len()
					end
				end
			elseif literal.shell_string and
					type(literal.shell_string.value) == "table" then
				local last_idx = token[1] + 1
				table.insert(result, {token[1], token[1],
					qualified = { false, "shell_exec" }})
				table.insert(result, {token[1], token[1], operator = "("})
				for _, part in ipairs(literal.shell_string.value) do
					if type(part) == "table" then
						if not first then
							add_cat(part[1])
						else
							first = false
						end
						last_idx = part[2]
						handle_subst(part)
					else
						if not first then
							add_cat(last_idx)
						else
							first = false
						end
						table.insert(result, {last_idx, last_idx + part:len(),
							literal = { dq_string = {
								prefix = "",
								value = part
						}}})
						last_idx = last_idx + part:len()
					end
				end
				table.insert(result, {token[1], token[1], operator = ")"})
			else
				table.insert(result, token)
			end
		else
			table.insert(result, token)
		end
	end
	return result
end

-- The AST builder turns output from the tokenizer into a syntax tree.
local function build_ast(scripts)
	tokens = flatten_scripts(scripts)
	tokens = flatten_strings(tokens)
	program = {}

	return tokens
end

return { build_ast = build_ast }
