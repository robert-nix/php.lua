package.path = '?.lua;src/?.lua;deps/LPegLJ/src/?.lua'

bit = require'bit'
io = require'io'
lpeg = require'lpeglj'
inspect = require'inspect'

local function dump(x)
	io.write(inspect(x) .. '\n')
end

-- Parse:
local grammar = (function()
	local v, r, p, s = lpeg.V, lpeg.R, lpeg.P, lpeg.S
	local C, Cc, Cf, Cg, Ct = lpeg.C, lpeg.Cc, lpeg.Cf, lpeg.Cg, lpeg.Ct

	local function sequence_of(pat)
		return pat^1
	end

	-- Comments:
	local new_line = p"\r\n" + p"\n" + p"\r"
	local input_char = p(1) - new_line
	local input_characters = sequence_of(input_char)
	local single_line_comment =
		p"//" * input_characters^-1 +
		p"#" * input_characters^-1
	local end_comment = p"*/"
	local in_comment = sequence_of(-end_comment * p(1))
	local delimited_comment = p"/*" * in_comment^-1 * end_comment
	local comment = delimited_comment + single_line_comment

	-- Whitespace:
	local whitespace_char = new_line + s" \t"
	local whitespace = sequence_of(whitespace_char)
	-- *w* is used hereafter for when whitespace is allowed but not required:
	local w = whitespace^-1
	-- *hw* for just horizontal whitespace
	local hw = sequence_of(s" \t")^-1

	-- Names:
	local nondigit = r"az" + r"AZ" + p"_"
	local digit = r"09"
	local name_nondigit = nondigit + r"\127\255"
	local name = Cg(C(name_nondigit * (name_nondigit + digit)^0), "name")
	local ns_name = p{"ns_name", ns_name =
		name + name *w* p"\\" *w* v"ns_name"
	}
	local ns_name_as_prefix =
		p"namespace" *w* p"\\" *w* ns_name *w* p"\\" +
		p"namespace" *w* p"\\" +
		p"\\"^-1 *w* ns_name *w* p"\\" +
		p"\\"
	local qualified_name = ns_name_as_prefix *w* name
	local variable_name = Cg(Ct(p"$" * name), "variable")

	-- Keywords:
	local function ci(str)
		-- given str, return a case-insensitive pattern
		local len = str:len()
		local pat = p""
		for i=1,len do
			local c = str:sub(i,i)
			pat = pat * s(c:lower() .. c:upper())
		end
		return pat
	end
	local keywords = {
		"require_once", "include_once", "instanceof", "implements",
		"endforeach", "enddeclare", "protected", "namespace", "interface",
		"insteadof", "endswitch", "function", "endwhile", "continue",
		"callable", "abstract", "require", "private", "include", "foreach",
		"finally", "extends", "default", "declare", "switch", "static",
		"return", "public", "global", "endfor", "elseif", "yield", "while",
		"trait", "throw", "print", "final", "endif", "const", "clone", "class",
		"catch", "break", "goto", "echo", "case", "xor", "var", "use", "try",
		"new", "for", "and", "or", "if", "do", "as",
	}
	local function cat_ci(table)
		-- apply + operator to everything in table:
		local pat = p(ci(table[1]))
		for i=2,#table do
			pat = pat + ci(table[i])
		end
		return pat
	end
	local keyword = Cg(
		C(cat_ci(keywords) * -(name_nondigit + digit)) / function(str)
			return str:lower()
		end, "keyword")

	-- Literals:
	--  - Integer:
	local decimal_literal = r"19" * r"09"^0
	local octal_literal = p"0" * r"07"^0
	local hex_digit = r("09", "af", "AF")
	local hex_literal = (p"0x" + p"0X") * hex_digit^-1
	local binary_literal =  (p"0b" + p"0B") * r"01"^-1
	local integer_literal =
		Cg(binary_literal, "binary") +
		Cg(hex_literal, "hex") +
		Cg(octal_literal, "octal") +
		Cg(decimal_literal, "decimal")

	--  - Floating-point:
	local digit_sequence = sequence_of(digit)
	local exponent_part = s"eE" * s"+-"^-1 * digit_sequence
	local fractional_literal =
		digit_sequence^-1 * p"." * digit_sequence +
		digit_sequence * p"."
	local floating_literal = Cg(
		fractional_literal * exponent_part^-1 +
		digit_sequence * exponent_part
	, "float")

	--  - Single-quoted string:
	local sq_char =
		C(p"\\"^-1 * (p(1) - s"'\\")) +
		p"\\\\" / "\\" +
		p"\\'" / "'"
	local sq_chars = Cf(sequence_of(sq_char), function(a, b) return a .. b end)
	local sq_string_literal = Ct(Cg(p"b"^-1, "prefix") *
		p"'" * Cg(sq_chars^-1, "value") * p"'")

	--  - Double-quoted string:
	local hex_value = C(hex_digit)
	local codepoint_digits = sequence_of(hex_value)
	local dq_unicode_escape = p"\\u{" * Cf(Cc'' *
		Cf(codepoint_digits, function(a, b) return a .. b end), -- cat hex
		function(a, b) -- convert to utf-8
			local code = tonumber(b, 16)
			if code > 0x10ffff then
				error(string.format('Unicode codepoint U+%X too large', code))
			end
			if code < 0x7f then
				return string.char(tonumber(code))
			elseif code < 0x7ff then
				return string.char(
					bit.bor(0xc0, bit.rshift(code, 6)),
					bit.bor(0x80, bit.band(code, 0x3f)))
			elseif code < 0xffff then
				return string.char(
					bit.bor(0xe0, bit.rshift(code, 12)),
					bit.bor(0x80, bit.band(bit.rshift(code, 6), 0x3f)),
					bit.bor(0x80, bit.band(code, 0x3f)))
			elseif code < 0x1fffff then
				return string.char(
					bit.bor(0xf0, bit.rshift(code, 18)),
					bit.bor(0x80, bit.band(bit.rshift(code, 12), 0x3f)),
					bit.bor(0x80, bit.band(bit.rshift(code, 6), 0x3f)),
					bit.bor(0x80, bit.band(code, 0x3f)))
			end -- 5 and 6 byte codes aren't supported
		end) * p"}"

	local dq_hex_escape = (p"\\x" + p"\\X") * Cf(Cc(0) *
		Cf(hex_value * hex_value^-1, function(a, b) -- 1 to 2 digits
			return a .. b end), -- cat hex
		function(a, b) -- convert to char
			return string.char(tonumber(b, 16)) end)

	local dq_octal_escape = p"\\" * Cf(Cc(0) *
		Cf(C(r"07") * C(r"07")^-2, function(a, b) -- 1 to 3 digits
			return a * 8 + b end), -- convert octal to number
		function(a, b) return string.char(b) end) -- convert to char

	local escape_chars = {
		v = "\v",
		t = "\t",
		r = "\r",
		n = "\n",
		f = "\f",
		e = "\27",
		["$"] = "$",
		["\\"] = "\\",
		['"'] = '"'
	}
	local dq_simple_escape = p"\\" * (s"vtrnfe$\\\"" / function(c)
		return escape_chars[c] end)
	local dq_escape =
		dq_unicode_escape +
		dq_hex_escape +
		dq_octal_escape +
		dq_simple_escape

	local dq_char =
		dq_escape +
		C((p(1) - s"\\\"")) +
		p"\\" * (p(1) - (s"Xxvtrnfe$\\\"" + r"07"))
	local dq_chars = Cf(sequence_of(dq_char), function(a, b) return a .. b end)
	local dq_string_literal = Ct(Cg(p"b"^-1, "prefix") *
		p'"' * Cg(dq_chars^-1, "value") * p'"')

	--  - Heredoc string:
	local hd_simple_escape = p"\\" * (s"vtrnfe$\\" / function(c)
		return escape_chars[c] end)
	local hd_escape =
		dq_unicode_escape +
		dq_hex_escape +
		dq_octal_escape +
		hd_simple_escape
	local hd_char =
		hd_escape +
		C((p(1) - p"\\")) +
		p"\\" * (p(1) - (s"Xxvtrnfe$\\" + r"07"))
	local hd_string_literal = lpeg.Cmt(
		p"<<<" *hw* (p'"' * C(name) * p'"' + C(name)),
		function(str, i, m)
			local curr_heredoc = Ct(new_line * 
				Cg(Cf((-p(m) * hd_char)^0, function(a, b) return a .. b end),
					"value") *
				p(m) * p";"^-1 * new_line *
				Cg(lpeg.Cp(), "len"))
			local hd_match = curr_heredoc:match(str:sub(i))
			return i + hd_match.len - 1, hd_match
		end)

	--  - Nowdoc string:
	local nd_string_literal = lpeg.Cmt(
		p"<<<" *hw* p"'" * C(name) * p"'",
		function(str, i, m)
			local curr_nowdoc = Ct(new_line *
				Cg(Cf((-p(m) * hd_char)^0, function(a, b) return a .. b end),
					"value") *
				p(m) * p";"^-1 * new_line *
				Cg(lpeg.Cp(), "len"))
			local nd_match = curr_nowdoc:match(str:sub(i))
			return i + nd_match.len - 1, nd_match
		end)


	local string_literal =
		Cg(nd_string_literal, "nd_string") +
		Cg(hd_string_literal, "hd_string") +
		Cg(dq_string_literal, "dq_string") +
		Cg(sq_string_literal, "sq_string")

	local literal = Cg(Ct(
		floating_literal +
		integer_literal +
		string_literal
	), "literal")

	-- Operators
	local operators = {
		"**=", "!==", ">>=", "===", "<<=", "++", "+=", "%=", "&&", "&=", "**",
		"*=", ".=", "/=", "!=", "--", "->", "-=", "||", "|=", ">>", ">=", "=&",
		"==", "<=", "<<", "^=", "+", "%", "&", "*", "$", "}", "{", "]", "[",
		")", "(", ".", "/", "?", "!", ":", ";", ",", "-", "|", ">", "=", "<",
		"~", "^"
	}
	local function cat(table)
		-- apply + operator to everything in table:
		local pat = p(table[1])
		for i=2,#table do
			pat = pat + table[i]
		end
		return pat
	end
	local operator = Cg(
		C(cat(operators)) / function(str) return str:lower() end,
		"operator")

	-- Input
	local token = Ct(
		variable_name +
		literal +
		operator +
		keyword +
		name
	)
	local input =
		Ct(Cg(whitespace, "whitespace") + Cg(comment, "comment")) + token


	-- Script:
	local start_tag = p"<?php" + p"<?="
	local end_tag = p"?>"
	local text = sequence_of(-start_tag * p(1)) -- anything but start_tag
	local script_section = Ct(
		C(text^-1) * -- text to echo
		C(start_tag) * -- which start tag?
		Ct((-p"?>" * input)^0) * -- expressions to execute
		(p"?>" * -- maybe end tag?
			C(text^-1))^-1 * -- if so, more text to echo
		Cg(lpeg.Cp(), "len")
	)
	local script = Ct(script_section^0)

	return {
		token = token,
		script = script
	}
end)()

local args = {...}
local script_file = io.open(args[1], "r")
local script_buf = script_file:read("*a")
dump(grammar.script:match(script_buf))
