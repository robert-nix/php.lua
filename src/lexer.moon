bit = require "bit"
ffi = require "ffi"

ord = string.byte

ffi.cdef[[
uint64_t strtoull(const char *str, char **str_end, int base);
double strtod(const char *str, char **str_end);

typedef struct lexer {
	const uint8_t *buf;
	int32_t index;
	int32_t buf_len;
	int32_t pos;
	int32_t last_pos;
	int32_t state;
} lexer;

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
		int32_t data[2];
	};
	int32_t left;
	int32_t right;
	int32_t outer_left;
	int32_t outer_right;
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
	TLIT_NDSTRING,
	TLIT_SHELLSTRING
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
	TOP_BSLASH,

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

node_ptr_type = ffi.typeof "node *"
node_type = ffi.typeof "node"

-- Tables for converting strings to enum values
op_str_to_enum =
	";": ffi.C.TOP_SEMICOLON
	"{": ffi.C.TOP_LEFTBRACE
	"}": ffi.C.TOP_RIGHTBRACE
	"(": ffi.C.TOP_LEFTPAREN
	")": ffi.C.TOP_RIGHTPAREN
	"[": ffi.C.TOP_LEFTBRACKET
	"]": ffi.C.TOP_RIGHTBRACKET
	"->": ffi.C.TOP_PROPARROW
	"::": ffi.C.TOP_SCOPERES
	"**": ffi.C.TOP_POW
	"++": ffi.C.TOP_INCREMENT
	"--": ffi.C.TOP_DECREMENT
	"~": ffi.C.TOP_BNOT
	"@": ffi.C.TOP_STFU
	"!": ffi.C.TOP_LNOT
	"*": ffi.C.TOP_MUL
	"/": ffi.C.TOP_DIV
	"%": ffi.C.TOP_MOD
	"+": ffi.C.TOP_ADD
	"-": ffi.C.TOP_SUB
	".": ffi.C.TOP_CAT
	"<<": ffi.C.TOP_LSHIFT
	">>": ffi.C.TOP_RSHIFT
	"<": ffi.C.TOP_LT
	"<=": ffi.C.TOP_LE
	">": ffi.C.TOP_GT
	">=": ffi.C.TOP_GE
	"==": ffi.C.TOP_EQUAL
	"!=": ffi.C.TOP_INEQUAL
	"===": ffi.C.TOP_IDENTICAL
	"!==": ffi.C.TOP_UNIDENTICAL
	"&": ffi.C.TOP_BAND
	"^": ffi.C.TOP_BXOR
	"|": ffi.C.TOP_BOR
	"&&": ffi.C.TOP_LAND
	"||": ffi.C.TOP_LOR
	"?": ffi.C.TOP_TERN_THEN
	":": ffi.C.TOP_TERN_ELSE
	"=": ffi.C.TOP_ASSIGN
	"=&": ffi.C.TOP_ASSIGN_REF
	"+=": ffi.C.TOP_ASSIGN_ADD
	"-=": ffi.C.TOP_ASSIGN_SUB
	"*=": ffi.C.TOP_ASSIGN_MUL
	"**=": ffi.C.TOP_ASSIGN_POW
	"/=": ffi.C.TOP_ASSIGN_DIV
	".=": ffi.C.TOP_ASSIGN_CAT
	"%=": ffi.C.TOP_ASSIGN_MOD
	"&=": ffi.C.TOP_ASSIGN_BAND
	"|=": ffi.C.TOP_ASSIGN_BOR
	"^=": ffi.C.TOP_ASSIGN_BXOR
	"<<=": ffi.C.TOP_ASSIGN_LSHIFT
	">>=": ffi.C.TOP_ASSIGN_RSHIFT
	"=>": ffi.C.TOP_ASSOC
	"$": ffi.C.TOP_DOLLAR
	",": ffi.C.TOP_COMMA
	"\\": ffi.C.TOP_BSLASH

op_enum_to_str = {}
for str, enum in pairs op_str_to_enum
	op_enum_to_str[enum] = str

operators = { -- ordered only by length descending
	"**=", "!==", ">>=", "===", "<<=", "<=>", "++", "+=", "%=", "&&", "&=",
	"**", "*=", ".=", "/=", "!=", "--", "->", "-=", "||", "|=", ">>", ">=",
	"=&", "==", "<=", "<<", "<>", "^=", "=>", "::", "+", "%", "&", "*", "$",
	"}", "{", "]", "[", ")", "(", ".", "/", "?", "!", ":", ";", "-", "|",
	">", "=", "<", "~", "^", "@", ",", "\\"
}

kw_str_to_enum =
	"abstract": ffi.C.TKW_ABSTRACT
	"and": ffi.C.TKW_AND
	"as": ffi.C.TKW_AS
	"break": ffi.C.TKW_BREAK
	"callable": ffi.C.TKW_CALLABLE
	"case": ffi.C.TKW_CASE
	"catch": ffi.C.TKW_CATCH
	"class": ffi.C.TKW_CLASS
	"clone": ffi.C.TKW_CLONE
	"const": ffi.C.TKW_CONST
	"continue": ffi.C.TKW_CONTINUE
	"declare": ffi.C.TKW_DECLARE
	"default": ffi.C.TKW_DEFAULT
	"do": ffi.C.TKW_DO
	"echo": ffi.C.TKW_ECHO
	"elseif": ffi.C.TKW_ELSEIF
	"enddeclare": ffi.C.TKW_ENDDECLARE
	"endfor": ffi.C.TKW_ENDFOR
	"endforeach": ffi.C.TKW_ENDFOREACH
	"endif": ffi.C.TKW_ENDIF
	"endswitch": ffi.C.TKW_ENDSWITCH
	"endwhile": ffi.C.TKW_ENDWHILE
	"extends": ffi.C.TKW_EXTENDS
	"final": ffi.C.TKW_FINAL
	"finally": ffi.C.TKW_FINALLY
	"for": ffi.C.TKW_FOR
	"foreach": ffi.C.TKW_FOREACH
	"function": ffi.C.TKW_FUNCTION
	"global": ffi.C.TKW_GLOBAL
	"goto": ffi.C.TKW_GOTO
	"if": ffi.C.TKW_IF
	"implements": ffi.C.TKW_IMPLEMENTS
	"include": ffi.C.TKW_INCLUDE
	"include_once": ffi.C.TKW_INCLUDE_ONCE
	"instanceof": ffi.C.TKW_INSTANCEOF
	"insteadof": ffi.C.TKW_INSTEADOF
	"interface": ffi.C.TKW_INTERFACE
	"namespace": ffi.C.TKW_NAMESPACE
	"new": ffi.C.TKW_NEW
	"or": ffi.C.TKW_OR
	"print": ffi.C.TKW_PRINT
	"private": ffi.C.TKW_PRIVATE
	"protected": ffi.C.TKW_PROTECTED
	"public": ffi.C.TKW_PUBLIC
	"require": ffi.C.TKW_REQUIRE
	"require_once": ffi.C.TKW_REQUIRE_ONCE
	"return": ffi.C.TKW_RETURN
	"static": ffi.C.TKW_STATIC
	"switch": ffi.C.TKW_SWITCH
	"throw": ffi.C.TKW_THROW
	"trait": ffi.C.TKW_TRAIT
	"try": ffi.C.TKW_TRY
	"use": ffi.C.TKW_USE
	"var": ffi.C.TKW_VAR
	"while": ffi.C.TKW_WHILE
	"xor": ffi.C.TKW_XOR
	"yield": ffi.C.TKW_YIELD
	"true": ffi.C.TKW_TRUE
	"false": ffi.C.TKW_FALSE
	"null": ffi.C.TKW_NULL

kw_enum_to_str = {}
for str, enum in pairs kw_str_to_enum
	kw_enum_to_str[enum] = str

lit_enum_to_str =
	[ffi.C.TLIT_DECIMAL]: "decimal"
	[ffi.C.TLIT_FLOAT]: "float"
	[ffi.C.TLIT_BINARY]: "binary"
	[ffi.C.TLIT_HEX]: "hex"
	[ffi.C.TLIT_OCTAL]: "octal"
	[ffi.C.TLIT_DQSTRING]: "dq_string"
	[ffi.C.TLIT_SQSTRING]: "sq_string"
	[ffi.C.TLIT_HDSTRING]: "hd_string"
	[ffi.C.TLIT_NDSTRING]: "nd_string"
	[ffi.C.TLIT_SHELLSTRING]: "shell_string"


lexer_contexts = {} -- used to store plain lua data associated with the lexer

methods =
	ctx: => lexer_contexts[@index]

	eof: => @pos >= @buf_len

	-- yay constant propagation
	match_string: (str) =>
		len = str\len!
		if @pos + len >= @buf_len
			return false
		for i=1,len
			if @buf[@pos - 1 + i] != ord str, i
				return false
		@pos += len
		return true

	-- assumes str is lowercase
	match_string_i: (str) =>
		len = str\len!
		if @pos + len >= @buf_len
			return false
		for i=1,len
			c = @buf[@pos - 1 + i]
			if c >= 0x41 and c <= 0x5a
				c = bit.bor c, 0x20
			if c != ord str, i
				return false
		@pos += len
		return true

	match_char: (str) =>
		if @pos + 1 >= @buf_len
			return false
		if @buf[@pos] == ord str
			@pos += 1
			return true
		else
			return false

	match_any_char: (str) =>
		if @pos + 1 >= @buf_len
			return false
		for i=1,str\len!
			c = @buf[@pos]
			if c == ord str, i
				@pos += 1
				return c
		return false

	match_range_char: (str) =>
		if @pos + 1 >= @buf_len
			return false
		for i=1,str\len!,2
			c = @buf[@pos]
			if c >= ord(str, i) and c <= ord(str, i + 1)
				@pos += 1
				return c
		return false

	init: =>
		ctx = @ctx!
		ctx.tokens = {}
		ctx.refs = {}
		ctx.constant_idx = 1
		ctx.constant_to_idx = {}
		ctx.constants = {}
		ctx.line_sums = { 0 }
		for i = 1, ctx.str\len!
			-- note: handle macintosh newlines when someone complains
			-- note: handle unicode in source file when someone complains
			if ctx.str\sub(i, i) == '\n'
				table.insert ctx.line_sums, i
		table.insert ctx.line_sums, ctx.str\len!
		return

	char_to_linecol: (char_idx) =>
		char_idx += 1
		ctx = @ctx!
		line_sums = ctx.line_sums
		line = 1
		min = 1
		max = #line_sums
		while min <= max
			mid = min + bit.rshift max - min, 1
			below = line_sums[mid]
			above = line_sums[mid + 1]
			if below <= char_idx
				if above > char_idx
					return mid, char_idx - below
				min = mid + 1
			else
				max = mid - 1
		return -1, -1

	make_ref: (value) =>
		ctx = @ctx!
		if ctx.constant_to_idx[value] ~= nil
			return ctx.constant_to_idx[value]
		idx = ctx.constant_idx
		ctx.constants[idx] = value
		ctx.constant_to_idx[value] = idx
		ctx.constant_idx = idx + 1
		return idx

	make_token: (tok_kind, start, final) =>
		if final == nil
			final = start
		token = ffi.new node_type
		token.start_line, token.start_col = @char_to_linecol start
		token.final_line, token.final_col = @char_to_linecol final
		token.kind = tok_kind
		return token

	push_token: (token) =>
		table.insert @ctx!.tokens, token
		return

	push_keyword: (kw_name, start, final) =>
		token = @make_token ffi.C.TOK_KEYWORD, start, final
		token.keyword.word = kw_str_to_enum[kw_name]
		@push_token token

	push_operator: (op, start, final) =>
		token = @make_token ffi.C.TOK_OPERATOR, start, final
		token.operator.op = op_str_to_enum[op]
		@push_token token

	push_name: (name, start, final, var = false, root = false, rel = false) =>
		token = @make_token ffi.C.TOK_NAME, start, final
		token.name.ref = @make_ref name
		token.name.is_root_ns = root
		token.name.is_relative_ns = rel
		token.name.is_variable = var
		@push_token token

	push_float_literal: (value, start, final) =>
		token = @make_token ffi.C.TOK_LITERAL, start, final
		token.literal.kind = ffi.C.TLIT_FLOAT
		token.literal.ref = @make_ref value
		@push_token token

	push_int_literal: (value, base, start, final) =>
		token = @make_token ffi.C.TOK_LITERAL, start, final
		if base == 2
			token.literal.kind = ffi.C.TLIT_BINARY
		elseif base == 8
			token.literal.kind = ffi.C.TLIT_OCTAL
		elseif base == 10
			token.literal.kind = ffi.C.TLIT_DECIMAL
		else
			token.literal.kind = ffi.C.TLIT_HEX
		token.literal.ref = @make_ref value
		@push_token token

	push_sq_literal: (start, final, value) =>
		token = @make_token ffi.C.TOK_LITERAL, start, final
		token.literal.kind = ffi.C.TLIT_SQSTRING
		if value == nil
			value = @get_string start, final
		token.literal.ref = @make_ref value
		@push_token token

	lex: =>
		while @lex_section!
			line, col = @char_to_linecol @pos
			io.write "section ended at #{line}:#{col}(eof=#{@eof!})\n"

	lex_section: =>
		local begin_echo
		local tag_pos
		done = true
		while not @eof!
			tag_pos = @pos
			if @match_string "<?"
				if @match_string "php"
					begin_echo = false
					done = false
					break
				if @match_char "="
					begin_echo = true
					done = false
					break
			@pos += 1

		if @pos - @last_pos > 0
			@push_keyword "echo", @last_pos
			@push_sq_literal @last_pos, tag_pos
		if done
			return false
		@last_pos = @pos
		line, col = @char_to_linecol @pos
		if begin_echo
			@push_keyword "echo", @last_pos

		end_of_section = false
		while true
			if nil ~= @lex_token!
				break
			@last_pos = @pos
		return true

	lex_token: =>
		-- (( Whitespace and Comments ))
		-- skip whitespace :: special consideration could be made for
		-- newlines here (e.g. detecting mixed newlines) at a minor(?) perf
		-- cost
		if @match_any_char " \f\t\v\r\n"
			@handle_whitespace!
			return
		if @match_string "?>"
			end_of_section = true
			@last_pos = @pos
			return true
		if @match_string "//"
			@handle_line_comment!
			return
		if @match_char "#"
			@handle_line_comment!
			return
		if @match_string "/*"
			@handle_block_comment!
			return
		-- (( Tokens: Keywords and Names ))
		saved_pos = @pos
		name = @read_name!
		if name ~= nil
			-- check for a keyword
			lname = name\lower!
			if kw_str_to_enum[lname] ~= nil
				-- The table access should get CSE'd
				@push_keyword lname, @last_pos, @pos
				return
			@push_name name, @last_pos, @pos
			return
		-- (( Tokens: Variable names ))
		if @match_char "$"
			-- try reading name here, otherwise this $ is an operator
			name = @read_name!
			if name == nil
				-- just push the dollar
				@push_operator "$", @last_pos, @pos
				return
			else
				@push_name name, @last_pos, @pos, true
				return
		-- (( Tokens: Float literals ))
		literal = @read_float!
		if literal == nil
			-- fall back
			@pos = saved_pos
		else
			@push_float_literal literal, @last_pos, @pos
			return
		-- (( Tokens: Integer literals ))
		literal, base = @read_int!
		if literal == nil
			@pos = saved_pos
		else
			@push_int_literal literal, base, @last_pos, @pos
			return
		-- (( Tokens: Nowdoc literals ))
		-- (( Tokens: Heredoc literals ))
		-- (( Tokens: Double-quoted string literals ))
		-- (( Tokens: Shell string literals ))
		-- (( Tokens: Single-quoted string literals ))
		-- ohgod
		-- (( Tokens: Operators ))
		for op in *operators
			if @match_string op
				@push_operator op, @last_pos, @pos
				return
			else
				@pos = saved_pos

		-- todo: proper diagnostics
		line, col = @char_to_linecol @pos
		io.write "parsing stopped early: #{line}:#{col}\n"
		return true

	handle_whitespace: =>
		while @match_any_char " \f\t\v\r\n" do nil
		-- emit whitespace token here, if needed

	handle_line_comment: =>
		while true
			if @match_string "?>"
				@pos -= 2
				break
			if @match_any_char "\r\n"
				@pos -= 1
				break
			@pos += 1
		-- emit line comment here, if needed

	handle_block_comment: =>
		while true
			if @match_string "*/"
				break
			@pos += 1
		-- emit block comment here, if needed

	read_name: =>
		c = @match_range_char "__azAZ\127\255" -- this is why @buf is uint8_t*
		if not c
			return nil
		result = { string.char c }
		while true
			c = @match_range_char "__azAZ09\127\255"
			if not c
				return table.concat result
			table.insert result, string.char c

	get_string: (start, final = @pos) =>
		return @ctx!.str\sub 1 + start, 1 + final

	read_float: =>
		init_pos = @pos
		expect_e = false
		-- 1.23{e+123} / 1e+123
		if not @match_char "."
			if not @match_range_char "09"
				return nil
			while true
				if not @match_range_char "09"
					break
			if @match_char "."
				-- read fract part
				while @match_range_char "09" do nil
			else
				expect_e = true
		-- .23{e+123}
		else
			-- read fract part
			while @match_range_char "09" do nil
		if @match_any_char "eE"
			-- process exp
			@match_any_char "-+"
			if not @match_range_char "09"
				-- todo: diagnostics
				return nil -- can't have fp literal with just "1.23e"
			while true
				if not @match_range_char "09"
					break
			str = @get_string init_pos
			return ffi.C.strtod str, nil
		else
			if expect_e
				return nil
			else
				str = @get_string init_pos
				return ffi.C.strtod str, nil

	read_int: =>
		-- todo: errno
		init_pos = @pos
		if @match_string_i "0x"
			init_pos = @pos
			if not @match_range_char "09afAF"
				return nil -- again, diagnostics; we got this far, should warn
			while @match_range_char "09afAF" do nil
			str = @get_string init_pos
			return ffi.C.strtoull(str, nil, 16), 16
		if @match_string_i "0b"
			init_pos = @pos
			if not @match_any_char "01"
				return nil
			while @match_any_char "01" do nil
			str = @get_string init_pos
			return ffi.C.strtoull(str, nil, 2), 2
		if @match_char "0"
			while @match_range_char "07" do nil
			str = @get_string init_pos
			return ffi.C.strtoull(str, nil, 8), 8
		if @match_range_char "19"
			while @match_range_char "09" do nil
			str = @get_string init_pos
			return ffi.C.strtoull(str, nil, 10), 10
		return nil


-- set up initializer for cdata-based class object
lexer_i = 1
local lexer_t
Lexer = setmetatable {},
	__call: (str) =>
		lexer = ffi.new lexer_t
		with_nul = str .. '\0'
		lexer.index = lexer_i
		lexer_i += 1
		lexer_contexts[lexer.index] =
			str: with_nul
		lexer.buf = with_nul
		lexer.buf_len = str\len!
		lexer.pos = 0
		lexer.last_pos = 0
		lexer.state = 0
		lexer\init!
		return lexer

lexer_t = ffi.metatype "lexer",
	__index: methods
	__gc: => lexer_contexts[@index] = nil

args = {...}
file = io.open args[1], "r"
some_php_buffer = file\read "*a"
test = Lexer some_php_buffer
test\lex!
