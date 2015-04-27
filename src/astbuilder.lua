
local ffi = require'ffi'

-- Definitions for C structures; the tokenizer output is converted to this now
-- to save me trouble optimizing later :)
ffi.cdef[[
// an AST node, kinda
// reminder: don't use bitfields, they cause NYIs
typedef struct node {
	int32_t start_line, start_col;
	int32_t final_line, final_col;
	uint16_t kind, subkind;
	union {
		struct name {
			int32_t ref;
			int8_t is_root_ns;
			int8_t is_relative_ns;
			int8_t is_variable;
		} name;
		struct literal {
			int32_t ref;
			int8_t kind;
		} literal;
		struct operator {
			int8_t op;
		} operator;
		struct keyword {
			int8_t word;
		} keyword;
		int32_t data;
	};
	int32_t left;
	int32_t right;
} node;

enum {
	TOK_INVALID,
	TOK_KEYWORD,
	TOK_NAME,
	TOK_OPERATOR,
	TOK_LITERAL
};

enum {
	TLIT_INVALID,
	TLIT_DECIMAL,
	TLIT_OCTAL,
	TLIT_HEX,
	TLIT_BINARY,
	TLIT_FLOAT,
	TLIT_DQSTRING,
	TLIT_SQSTRING,
	TLIT_HDSTRING,
	TLIT_NDSTRING
};

enum {
	TOP_INVALID,
	TOP_SEMICOLON,
	TOP_LEFTBRACE,
	TOP_RIGHTBRACE,
	TOP_LEFTPAREN,
	TOP_RIGHTPAREN,
	TOP_LEFTBRACKET,
	TOP_RIGHTBRACKET,
	TOP_PROPARROW,
	TOP_SCOPERES,
	TOP_POW,
	TOP_INCREMENT,
	TOP_DECREMENT,
	TOP_BNOT,
	TOP_STFU,
	TOP_LNOT,
	TOP_MUL,
	TOP_DIV,
	TOP_MOD,
	TOP_ADD,
	TOP_SUB,
	TOP_CAT,
	TOP_LSHIFT,
	TOP_RSHIFT,
	TOP_LT,
	TOP_LE,
	TOP_GT,
	TOP_GE,
	TOP_EQUAL,
	TOP_INEQUAL,
	TOP_IDENTICAL,
	TOP_UNIDENTICAL,
	TOP_BAND,
	TOP_BXOR,
	TOP_BOR,
	TOP_LAND,
	TOP_LOR,
	TOP_TERN_THEN,
	TOP_TERN_ELSE,
	TOP_ASSIGN,
	TOP_ASSIGN_REF,
	TOP_ASSIGN_ADD,
	TOP_ASSIGN_SUB,
	TOP_ASSIGN_MUL,
	TOP_ASSIGN_POW,
	TOP_ASSIGN_DIV,
	TOP_ASSIGN_CAT,
	TOP_ASSIGN_MOD,
	TOP_ASSIGN_BAND,
	TOP_ASSIGN_BOR,
	TOP_ASSIGN_BXOR,
	TOP_ASSIGN_LSHIFT,
	TOP_ASSIGN_RSHIFT,
	TOP_ASSOC,
	TOP_DOLLAR,
	TOP_COMMA,

	TKW_ABSTRACT,
	TKW_AND,
	TKW_AS,
	TKW_BREAK,
	TKW_CALLABLE,
	TKW_CASE,
	TKW_CATCH,
	TKW_CLASS,
	TKW_CLONE,
	TKW_CONST,
	TKW_CONTINUE,
	TKW_DECLARE,
	TKW_DEFAULT,
	TKW_DO,
	TKW_ECHO,
	TKW_ELSEIF,
	TKW_ENDDECLARE,
	TKW_ENDFOR,
	TKW_ENDFOREACH,
	TKW_ENDIF,
	TKW_ENDSWITCH,
	TKW_ENDWHILE,
	TKW_EXTENDS,
	TKW_FINAL,
	TKW_FINALLY,
	TKW_FOR,
	TKW_FOREACH,
	TKW_FUNCTION,
	TKW_GLOBAL,
	TKW_GOTO,
	TKW_IF,
	TKW_IMPLEMENTS,
	TKW_INCLUDE,
	TKW_INCLUDE_ONCE,
	TKW_INSTANCEOF,
	TKW_INSTEADOF,
	TKW_INTERFACE,
	TKW_NAMESPACE,
	TKW_NEW,
	TKW_OR,
	TKW_PRINT,
	TKW_PRIVATE,
	TKW_PROTECTED,
	TKW_PUBLIC,
	TKW_REQUIRE,
	TKW_REQUIRE_ONCE,
	TKW_RETURN,
	TKW_STATIC,
	TKW_SWITCH,
	TKW_THROW,
	TKW_TRAIT,
	TKW_TRY,
	TKW_USE,
	TKW_VAR,
	TKW_WHILE,
	TKW_XOR,
	TKW_YIELD,
	TKW_TRUE,
	TKW_FALSE,
	TKW_NULL
};
]]

local node_ptr_type = ffi.typeof(ffi.new("node *", nil))

-- Tables for converting strings to enum values
local op_str_to_enum = {
	[";"] = ffi.C.TOP_SEMICOLON,
	["{"] = ffi.C.TOP_LEFTBRACE,
	["}"] = ffi.C.TOP_RIGHTBRACE,
	["("] = ffi.C.TOP_LEFTPAREN,
	[")"] = ffi.C.TOP_RIGHTPAREN,
	["["] = ffi.C.TOP_LEFTBRACKET,
	["]"] = ffi.C.TOP_RIGHTBRACKET,
	["->"] = ffi.C.TOP_PROPARROW,
	["::"] = ffi.C.TOP_SCOPERES,
	["**"] = ffi.C.TOP_POW,
	["++"] = ffi.C.TOP_INCREMENT,
	["--"] = ffi.C.TOP_DECREMENT,
	["~"] = ffi.C.TOP_BNOT,
	["@"] = ffi.C.TOP_STFU,
	["!"] = ffi.C.TOP_LNOT,
	["*"] = ffi.C.TOP_MUL,
	["/"] = ffi.C.TOP_DIV,
	["%"] = ffi.C.TOP_MOD,
	["+"] = ffi.C.TOP_ADD,
	["-"] = ffi.C.TOP_SUB,
	["."] = ffi.C.TOP_CAT,
	["<<"] = ffi.C.TOP_LSHIFT,
	[">>"] = ffi.C.TOP_RSHIFT,
	["<"] = ffi.C.TOP_LT,
	["<="] = ffi.C.TOP_LE,
	[">"] = ffi.C.TOP_GT,
	[">="] = ffi.C.TOP_GE,
	["=="] = ffi.C.TOP_EQUAL,
	["!="] = ffi.C.TOP_INEQUAL,
	["==="] = ffi.C.TOP_IDENTICAL,
	["!=="] = ffi.C.TOP_UNIDENTICAL,
	["&"] = ffi.C.TOP_BAND,
	["^"] = ffi.C.TOP_BXOR,
	["|"] = ffi.C.TOP_BOR,
	["&&"] = ffi.C.TOP_LAND,
	["||"] = ffi.C.TOP_LOR,
	["?"] = ffi.C.TOP_TERN_THEN,
	[":"] = ffi.C.TOP_TERN_ELSE,
	["=" ] = ffi.C.TOP_ASSIGN,
	["=&"] = ffi.C.TOP_ASSIGN_REF,
	["+="] = ffi.C.TOP_ASSIGN_ADD,
	["-="] = ffi.C.TOP_ASSIGN_SUB,
	["*="] = ffi.C.TOP_ASSIGN_MUL,
	["**="] = ffi.C.TOP_ASSIGN_POW,
	["/="] = ffi.C.TOP_ASSIGN_DIV,
	[".="] = ffi.C.TOP_ASSIGN_CAT,
	["%="] = ffi.C.TOP_ASSIGN_MOD,
	["&="] = ffi.C.TOP_ASSIGN_BAND,
	["|="] = ffi.C.TOP_ASSIGN_BOR,
	["^="] = ffi.C.TOP_ASSIGN_BXOR,
	["<<="] = ffi.C.TOP_ASSIGN_LSHIFT,
	[">>="] = ffi.C.TOP_ASSIGN_RSHIFT,
	["=>"] = ffi.C.TOP_ASSOC,
	["$"] = ffi.C.TOP_DOLLAR,
	[","] = ffi.C.TOP_COMMA,
}

local op_enum_to_str = {}
for str, enum in pairs(op_str_to_enum) do
	op_enum_to_str[enum] = str
end

local kw_str_to_enum = {
	["abstract"] = ffi.C.TKW_ABSTRACT,
	["and"] = ffi.C.TKW_AND,
	["as"] = ffi.C.TKW_AS,
	["break"] = ffi.C.TKW_BREAK,
	["callable"] = ffi.C.TKW_CALLABLE,
	["case"] = ffi.C.TKW_CASE,
	["catch"] = ffi.C.TKW_CATCH,
	["class"] = ffi.C.TKW_CLASS,
	["clone"] = ffi.C.TKW_CLONE,
	["const"] = ffi.C.TKW_CONST,
	["continue"] = ffi.C.TKW_CONTINUE,
	["declare"] = ffi.C.TKW_DECLARE,
	["default"] = ffi.C.TKW_DEFAULT,
	["do"] = ffi.C.TKW_DO,
	["echo"] = ffi.C.TKW_ECHO,
	["elseif"] = ffi.C.TKW_ELSEIF,
	["enddeclare"] = ffi.C.TKW_ENDDECLARE,
	["endfor"] = ffi.C.TKW_ENDFOR,
	["endforeach"] = ffi.C.TKW_ENDFOREACH,
	["endif"] = ffi.C.TKW_ENDIF,
	["endswitch"] = ffi.C.TKW_ENDSWITCH,
	["endwhile"] = ffi.C.TKW_ENDWHILE,
	["extends"] = ffi.C.TKW_EXTENDS,
	["final"] = ffi.C.TKW_FINAL,
	["finally"] = ffi.C.TKW_FINALLY,
	["for"] = ffi.C.TKW_FOR,
	["foreach"] = ffi.C.TKW_FOREACH,
	["function"] = ffi.C.TKW_FUNCTION,
	["global"] = ffi.C.TKW_GLOBAL,
	["goto"] = ffi.C.TKW_GOTO,
	["if"] = ffi.C.TKW_IF,
	["implements"] = ffi.C.TKW_IMPLEMENTS,
	["include"] = ffi.C.TKW_INCLUDE,
	["include_once"] = ffi.C.TKW_INCLUDE_ONCE,
	["instanceof"] = ffi.C.TKW_INSTANCEOF,
	["insteadof"] = ffi.C.TKW_INSTEADOF,
	["interface"] = ffi.C.TKW_INTERFACE,
	["namespace"] = ffi.C.TKW_NAMESPACE,
	["new"] = ffi.C.TKW_NEW,
	["or"] = ffi.C.TKW_OR,
	["print"] = ffi.C.TKW_PRINT,
	["private"] = ffi.C.TKW_PRIVATE,
	["protected"] = ffi.C.TKW_PROTECTED,
	["public"] = ffi.C.TKW_PUBLIC,
	["require"] = ffi.C.TKW_REQUIRE,
	["require_once"] = ffi.C.TKW_REQUIRE_ONCE,
	["return"] = ffi.C.TKW_RETURN,
	["static"] = ffi.C.TKW_STATIC,
	["switch"] = ffi.C.TKW_SWITCH,
	["throw"] = ffi.C.TKW_THROW,
	["trait"] = ffi.C.TKW_TRAIT,
	["try"] = ffi.C.TKW_TRY,
	["use"] = ffi.C.TKW_USE,
	["var"] = ffi.C.TKW_VAR,
	["while"] = ffi.C.TKW_WHILE,
	["xor"] = ffi.C.TKW_XOR,
	["yield"] = ffi.C.TKW_YIELD,
	["true"] = ffi.C.TKW_TRUE,
	["false"] = ffi.C.TKW_FALSE,
	["null"] = ffi.C.TKW_NULL,
}

local kw_enum_to_str = {}
for str, enum in pairs(kw_str_to_enum) do
	kw_enum_to_str[enum] = str
end

local lit_enum_to_str = {
	[ffi.C.TLIT_DECIMAL] = "decimal",
	[ffi.C.TLIT_FLOAT] = "float",
	[ffi.C.TLIT_BINARY] = "binary",
	[ffi.C.TLIT_HEX] = "hex",
	[ffi.C.TLIT_OCTAL] = "octal",
	[ffi.C.TLIT_DQSTRING] = "dq_string",
	[ffi.C.TLIT_SQSTRING] = "sq_string",
	[ffi.C.TLIT_HDSTRING] = "hd_string",
	[ffi.C.TLIT_NDSTRING] = "nd_string",
}

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

local function calculate_flat_size(tokens)
	local result = 0
	local function handle_subst(subst)
		if offset == nil then offset = 0 end
		if subst.name then
			result = result + 4
			if subst.name.offset then
				result = result + 3
			end
			if subst.name.property then
				result = result + 2
			end
		elseif subst.braced then
			result = result + 3 + #subst.braced.expr
		elseif subst.create_name then
			result = result + 6 + #subst.create_name.expr
		end
	end
	for _, token in ipairs(tokens) do
		if token.literal then
			local literal = token.literal
			local first = true
			local function handle_cat()
				if not first then
					result = result + 1
				else
					first = false
				end
			end
			if literal.dq_string and
					type(literal.dq_string.value) == "table" then
				for _, part in ipairs(literal.dq_string.value) do
					handle_cat()
					if type(part) == "table" then
						handle_subst(part)
					else
						result = result + 1
					end
				end
			elseif literal.hd_string and
					type(literal.hd_string.value) == "table" then
				for _, part in ipairs(literal.hd_string.value) do
					handle_cat()
					if type(part) == "table" then
						handle_subst(part)
					else
						result = result + 1
					end
				end
			elseif literal.shell_string and
					type(literal.shell_string.value) == "table" then
				result = result + 3
				for _, part in ipairs(literal.shell_string.value) do
					handle_cat()
					if type(part) == "table" then
						handle_subst(part)
					else
						result = result + 1
					end
				end
			else
				result = result + 1
			end
		else
			result = result + 1
		end
	end
	return result
end

local function char_to_linecol(line_lengths, char_idx)
	local line = 1
	while true do
		if char_idx < line_lengths[line] then
			return line, char_idx
		else
			char_idx = char_idx - line_lengths[line]
			line = line + 1
		end
	end
	return -1, -1
end

local function get_constant_ref(ctx, constant)
	if ctx.constant_to_idx[constant] ~= nil then
		return ctx.constant_to_idx[constant]
	end
	local idx = ctx.constant_idx
	ctx.constants[idx] = constant
	ctx.constant_to_idx[constant] = idx
	ctx.constant_idx = idx + 1
	return idx
end

local function node_to_string(ctx, node)
	local result = {}
	if node.kind == ffi.C.TOK_KEYWORD then
		table.insert(result, "keyword: ")
		table.insert(result, kw_enum_to_str[node.keyword.word])
	elseif node.kind == ffi.C.TOK_NAME then
		table.insert(result, "name: ")
		if node.name.is_variable ~= 0 then
			table.insert(result, "$")
		end
		local ns_name = false
		if node.name.is_root_ns ~= 0 then
			table.insert(result, "\\")
			ns_name = true
		end
		if node.name.is_relative_ns ~= 0 then
			table.insert(result, "namespace\\")
			ns_name = true
		end
		if ns_name then
			local qname = ctx.constants[node.name.ref]
			local qparts = {}
			local idx = 1
			for i=1,#qname do
				if qname:sub(i, i) == "\0" then
					table.insert(qparts, qname:sub(idx, i - 1))
					idx = i + 1
				end
			end
			table.insert(qparts, qname:sub(idx, #qname))
			table.insert(result, table.concat(qparts, "\\"))
		else
			table.insert(result, ctx.constants[node.name.ref])
		end
	elseif node.kind == ffi.C.TOK_OPERATOR then
		table.insert(result, "op: ")
		table.insert(result, op_enum_to_str[node.operator.op])
	elseif node.kind == ffi.C.TOK_LITERAL then
		table.insert(result, "literal: ")
		table.insert(result, lit_enum_to_str[node.literal.kind])
		table.insert(result, ": ")
		table.insert(result, tostring(ctx.constants[node.literal.ref]))
	else
		table.insert(result, "<invalid token kind>")
	end
	return table.concat(result)
end

-- Turns string literals with variable substitutions and converts them to use
-- the concatenation operator.  Additionally, this stage converts the table-
-- heavy format output by the tokenizer into a list of unlinked token nodes
local function flatten_strings(ctx, tokens)
	local final_size = calculate_flat_size(tokens)
	io.write('#nodes = ' .. final_size .. '\n')
	io.flush()
	local result = ffi.new("node[" .. final_size .. "]")
	ffi.fill(result, ffi.sizeof("node") * final_size, 0)
	local node_idx = 0
	local function add_token(t)
		local node = result + node_idx
		local start_line, start_col = char_to_linecol(ctx.line_lengths, t[1])
		local final_line, final_col = char_to_linecol(ctx.line_lengths, t[2])
		node.start_line = start_line
		node.start_col = start_col
		node.final_line = final_line
		node.final_col = final_col
		if t.variable ~= nil then
			node.kind = ffi.C.TOK_NAME
			node.name.is_variable = 1;
			node.name.ref = get_constant_ref(ctx, t.variable)
		elseif t.qualified ~= nil then
			node.kind = ffi.C.TOK_NAME
			if t.qualified[1] then
				node.name.is_relative_ns = 1
			else
				node.name.is_root_ns = 1
			end
			local ns_name = {}
			for i = 2, #t.qualified do
				table.insert(ns_name, t.qualified[i])
			end
			node.name.ref = get_constant_ref(ctx, table.concat(ns_name, "\0"))
		elseif t.name ~= nil then
			node.kind = ffi.C.TOK_NAME
			node.name.ref = get_constant_ref(ctx, t.name)
		elseif t.operator ~= nil then
			node.kind = ffi.C.TOK_OPERATOR
			node.operator.op = op_str_to_enum[t.operator]
		elseif t.keyword ~= nil then
			node.kind = ffi.C.TOK_KEYWORD
			node.keyword.word = kw_str_to_enum[t.keyword]
		elseif t.literal ~= nil then
			-- at this stage, prefix information for strings is thrown away
			node.kind = ffi.C.TOK_LITERAL
			if t.literal.decimal ~= nil then
				node.literal.kind = ffi.C.TLIT_DECIMAL
				node.literal.ref = get_constant_ref(ctx, t.literal.decimal)
			elseif t.literal.float ~= nil then
				node.literal.kind = ffi.C.TLIT_FLOAT
				node.literal.ref = get_constant_ref(ctx, t.literal.float)
			elseif t.literal.binary ~= nil then
				node.literal.kind = ffi.C.TLIT_BINARY
				node.literal.ref = get_constant_ref(ctx, t.literal.binary)
			elseif t.literal.hex ~= nil then
				node.literal.kind = ffi.C.TLIT_HEX
				node.literal.ref = get_constant_ref(ctx, t.literal.hex)
			elseif t.literal.octal ~= nil then
				node.literal.kind = ffi.C.TLIT_OCTAL
				node.literal.ref = get_constant_ref(ctx, t.literal.octal)
			elseif t.literal.dq_string ~= nil then
				node.literal.kind = ffi.C.TLIT_DQSTRING
				node.literal.ref = get_constant_ref(ctx,
					t.literal.dq_string.value)
			elseif t.literal.sq_string ~= nil then
				node.literal.kind = ffi.C.TLIT_SQSTRING
				node.literal.ref = get_constant_ref(ctx,
					t.literal.sq_string.value)
			elseif t.literal.hd_string ~= nil then
				node.literal.kind = ffi.C.TLIT_HDSTRING
				node.literal.ref = get_constant_ref(ctx,
					t.literal.hd_string.value)
			elseif t.literal.nd_string ~= nil then
				node.literal.kind = ffi.C.TLIT_NDSTRING
				node.literal.ref = get_constant_ref(ctx,
					t.literal.nd_string.value)
			else
				error("bad literal")
			end
		else
			error("bad token")
		end
		io.write(node_to_string(ctx, node) .. '\n')
		node_idx = node_idx + 1
	end
	local function add_cat(i)
		add_token({i, i, operator = "."})
	end
	local function add_strval_start(i)
		-- qualified name, non-relative
		add_token({i, i, qualified = {false, "strval"}})
		add_token({i, i, operator = "("})
	end
	local function add_strval_final(i)
		add_token({i, i, operator = ")"})
	end
	local function handle_subst_name(subst, offset)
		local start = offset + subst[1]
		local final = offset + subst[2]
		add_strval_start(start)
		add_token({start, final, variable = subst.name.variable })
		if subst.name.offset then
			add_token({start, final, operator = "["})
			if subst.name.offset.name then
				add_token({start, final,
					literal = { dq_string = {
						value = subst.name.offset.name
				}}})
			else
				add_token({start, final,
					literal = subst.name.offset})
			end
			add_token({start, final, operator = "]"})
		end
		if subst.name.property then
			add_token({start, final, operator = "->"})
			add_token({start, final,
				name = subst.name.property})
		end
		add_strval_final(final)
	end
	local function handle_subst_braced(subst, offset)
		local start = offset + subst[1]
		local final = offset + subst[2]
		add_strval_start(start)
		-- worth noting, the stuff inside the brace expr is much more limited
		-- under Zend... might want to implement that restriction at some point,
		-- but not going to worry about it for now.
		local offs = 0
		for _, t in ipairs(subst.braced.expr) do
			if t[1] ~= nil then
				t[1] = start + t[1] + offs -- +1 from {
				t[2] = start + t[2] + offs
			else
				offs = t.variable:len()
				t[1] = start + 1
				t[2] = start + 1 + offs
			end
			add_token(t)
		end
		add_strval_final(final)
	end
	local function handle_subst_create_name(subst, offset)
		local start = offset + subst[1]
		local final = offset + subst[2]
		add_strval_start(start)
		add_token({start, start, operator = "$"})
		add_token({start, start, operator = "{"})
		local offs = 1
		for _, t in ipairs(subst.create_name.expr) do
			if t[1] ~= nil then
				t[1] = start + t[1] + offs -- +2 from ${
				t[2] = start + t[2] + offs
			else
				offs = t.variable:len()
				t[1] = start + 2
				t[2] = start + 1 + offs
			end
			add_token(t)
		end
		add_token({final, final, operator = "}"})
		add_strval_final(final)
	end
	local function handle_subst(subst, offset)
		if offset == nil then offset = 0 end
		if subst.name then
			handle_subst_name(subst, offset)
		elseif subst.braced then
			handle_subst_braced(subst, offset)
		elseif subst.create_name then
			handle_subst_create_name(subst, offset)
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
						add_token({last_idx, last_idx + part:len(),
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
							add_cat(1 + last_idx)
						else
							first = false
						end
						handle_subst(part, literal.hd_string[1] - 1)
						last_idx = literal.hd_string[1] + part[2] - 1
					else
						if not first then
							add_cat(last_idx)
						else
							first = false
						end
						add_token({last_idx, last_idx + part:len(),
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
				add_token({token[1], token[1],
					qualified = { false, "shell_exec" }})
				add_token({token[1], token[1], operator = "("})
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
						add_token({last_idx, last_idx + part:len(),
							literal = { dq_string = {
								prefix = "",
								value = part
						}}})
						last_idx = last_idx + part:len()
					end
				end
				add_token({token[1], token[1], operator = ")"})
			else
				add_token(token)
			end
		else
			add_token(token)
		end
	end
	return result
end

-- The AST builder turns output from the tokenizer into a syntax tree.
local function build_ast(ctx, scripts)
	ctx.constant_idx = 1
	ctx.constant_to_idx = {}
	ctx.constants = {}
	ctx.line_lengths = {}
	local line_idx = 0
	for i = 1, #ctx.file_buf do
		line_idx = line_idx + 1
		if ctx.file_buf:sub(i,i) == '\n' then
			table.insert(ctx.line_lengths, line_idx)
			line_idx = 0
		end
	end
	tokens = flatten_scripts(scripts)
	tokens = flatten_strings(ctx, tokens)
	program = {}

	num_tokens = ffi.sizeof(tokens) / ffi.sizeof("node")
	for i = 0,num_tokens-1 do
		io.write(i .. ' = ' .. node_to_string(ctx, tokens[i]) .. '\n')
	end

	return tokens
end

return { build_ast = build_ast }
