bit = require "bit"
ffi = require "ffi"

ord = string.byte

ffi.cdef[[
uint64_t strtoull(const char *str, char **str_end, int base);

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

op_enum_to_str = {}
for str, enum in pairs op_str_to_enum
	op_enum_to_str[enum] = str

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


lexer_contexts = {} -- used to store plain lua data associated with the lexer

methods =
	ctx: => lexer_contexts[@index]

	eof: => @pos >= @buf_len

	is_match: (str) =>
		-- yay constant propagation
		if @pos + str\len! >= @buf_len
			return false
		for i=1,str\len!
			if @buf[@pos - 1 + i] != ord str, i
				return false
		return true

	init: =>
		ctx = @ctx!
		ctx.tokens = {}
		ctx.refs = {}
		ctx.constant_idx = 1
		ctx.constant_to_idx = {}
		ctx.constants = {}
		ctx.line_sums = { 0 }
		for i = 1, ctx.str\len!
			if ctx.str\sub(i, i) == '\n'
				table.insert ctx.line_sums, i
		table.insert ctx.line_sums, ctx.str\len!
		return

	char_to_linecol: (char_idx) =>
		char_idx += 1 -- correct for 0-indexing here...
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
		table.insert(@ctx!.tokens, token)
		return

	push_keyword: (kw_name, start, final) =>
		token = @make_token ffi.C.TOK_KEYWORD, start, final
		token.keyword.word = kw_str_to_enum[kw_name]

	push_sq_literal: (start, final, value) =>
		token = @make_token ffi.C.TOK_LITERAL, start, final
		token.literal.kind = ffi.C.TLIT_SQSTRING
		if value == nil
			value = @ctx!.str\sub 1 + start, 1 + final
		token.literal.ref = @make_ref value

	lex: =>
		local begin_echo
		local tag_pos
		while not @eof!
			tag_pos = @pos
			if @is_match "<?"
				@pos += 2
				if @is_match "php"
					@pos += 3
					begin_echo = false
					break
				if @is_match "="
					@pos += 1
					begin_echo = true
					break
			@pos += 1
		if @pos - @last_pos > 0
			@push_keyword "echo", @last_pos
			@push_sq_literal @last_pos, tag_pos
		@last_pos = @pos
		if begin_echo
			@push_keyword "echo", @last_pos
		-- io.write "done, #{@}\n"
		@last_pos

	lex_input: =>
		io.write "#{@buf_len}\n"


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

some_php_buffer = " <?php echo 'hi'; ?>\n"
test = Lexer some_php_buffer
test\lex!
