package.path = package.path .. ';deps/LPegLJ/src/?.lua'

bit = require'bit'
lpeg = require'lpeglj'

return (function()
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
	-- *wc* for whitespace or comment
	local wc = (whitespace + comment)^0

	-- Names:
	local nondigit = r"az" + r"AZ" + p"_"
	local digit = r"09"
	local name_nondigit = nondigit + r"\127\255"
	local name = Cg(C(name_nondigit * (name_nondigit + digit)^0), "name")
	local ns_name =
		-- initial bool => relative?
		-- namespace\name{\name*}
		Cc(true) * p"namespace" *wc* p"\\" *wc* C(name) *(wc* p"\\" *wc* C(name))^0 +
		-- name{\name+} or \name
		Cc(true) * C(name) * (wc* p"\\" * C(name))^1 +
		Cc(false) * p"\\" *wc* C(name) * (wc* p"\\" * C(name))^0
	local qualified_name = Cg(Ct(ns_name), "qualified")
	local variable_name =
		Cg(Ct(p"$" * name) / function(a) return a.name end, "variable")

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
	local decimal_literal = (r"19" * r"09"^0) / function(a)
		return tonumber(a, 10) end

	local octal_literal = (p"0" * r"07"^0) / function(a)
		return tonumber(a, 8) end

	local hex_digit = r("09", "af", "AF")
	local hex_literal = (p"0x" + p"0X") * (hex_digit^0) / function(a)
		return tonumber(a, 16) end

	local binary_literal =  (p"0b" + p"0B") * (r"01"^0) / function(a)
		return tonumber(a, 2) end

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
		(fractional_literal * exponent_part^-1 +
		digit_sequence * exponent_part) / function(a) return tonumber(a) end
	, "float")

	--  - String variable substitution:
	local property_in_string = p"->" + Cg(name, "property")
	local offset_in_string =
		Cg(Ct(p"[" * (integer_literal + variable_name + name) * p"]"), "offset")
	local offset_or_property = property_in_string + offset_in_string
	local in_string_var_expr = nil -- assigned after input is defined
	local string_variable = Ct(Cg(lpeg.Cp(), 1) * (Cg(lpeg.Cmt(
		Ct(Cg(lpeg.Cp(), 1) * p"${" * variable_name), function(str, i, m)
			local tail = in_string_var_expr:match(str:sub(i))
			if tail == nil then return nil end
			table.insert(tail.expr, 1, { variable = m.variable })
			local idx = i + tail[2] - 1
			return idx, {
				expr = tail.expr
			}
		end), "create_name") + Cg(Ct(variable_name * offset_or_property^-1),
		"name")) * Cg(lpeg.Cp(), 2))

	local function fold_string_table(a, b) -- a and b may be table or char
		local result = nil
		if type(a) ~= "table" or a.name ~= nil or a.create_name ~= nil then
			result = {a}
		else
			result = a
		end
		if type(b) ~= "table" then
			local last = result[#result]
			if type(last) ~= "table" then
				result[#result] = last .. b
			else
				table.insert(result, b)
			end
		else
			table.insert(result, b)
		end
		return result
	end

	--  - Single-quoted string:
	local sq_char =
		C(p"\\"^-1 * (p(1) - s"'\\")) +
		p"\\\\" / "\\" +
		p"\\'" / "'"
	local function fold_string(a, b) return a .. b end
	local sq_chars = Cf(sequence_of(sq_char), fold_string)
	local sq_string_literal = Ct(Cg(p"b"^-1, "prefix") *
		p"'" * Cg(sq_chars^-1, "value") * p"'")

	--  - Double-quoted string:
	local hex_value = C(hex_digit)
	local codepoint_digits = sequence_of(hex_value)
	local dq_unicode_escape = p"\\u{" * Cf(Cc'' *
		Cf(codepoint_digits, fold_string), -- cat hex
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
		Cf(hex_value * hex_value^-1, fold_string), -- 1 to 2 digits
		function(a, b) -- convert hex to char
			return string.char(tonumber(b, 16)) end)

	local dq_octal_escape = p"\\" * Cf(Cc(0) *
		Cf(C(r"07") * C(r"07")^-2, fold_string), -- 1 to 3 digits
		function(a, b) -- convert octal to char
			return string.char(tonumber(b, 8)) end)

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
		string_variable +
		C(p(1) - s"\\\"") +
		p"\\" * (p(1) - (s"Xxvtrnfe$\\\"" + r"07"))
	local dq_chars = Cf(sequence_of(dq_char), fold_string_table)
	local dq_string_literal = Cf(C(p"b"^-1) *
		p'"' * dq_chars^-1 * p'"', function(a, b)
		return { prefix = a, value = b } end)

	--  - Shell-command string:
	local shell_char =
		dq_escape +
		p"\\" * p"`" * Cc"`" +
		string_variable +
		C(p(1) - s"\\`") +
		C(p"\\" * p(1))
	local shell_chars = Cf(sequence_of(shell_char), fold_string_table)
	local shell_string_literal = (p'`' * shell_chars^-1 * p'`') / function(a)
		return { value = a } end

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
		string_variable +
		C(p(1) - p"\\") +
		C(p"\\" * (p(1) - (s"Xxvtrnfe$\\" + r"07")))
	local hd_string_literal = lpeg.Cmt(
		p"<<<" *hw* Ct((p'"' * name * p'"' + name) * Cg(lpeg.Cp(), "mid")),
		function(str, i, m)
			local hd_id = new_line * p(m.name)
			local curr_heredoc = Ct(new_line * 
				Cf((-hd_id * hd_char)^0, fold_string_table) *
				lpeg.Cp() * hd_id * p";"^-1 * new_line *
				lpeg.Cp()) / function(t)
					return { 1, t[2], value = t[1], len = t[3] } end
			local hd_match = curr_heredoc:match(str:sub(i))
			if hd_match == nil then return nil end
			return i + hd_match.len - 1, {
				m.mid, m.mid + hd_match[2] - 1, value = hd_match.value
			}
		end)

	--  - Nowdoc string:
	local nd_char =
		hd_escape +
		C(p(1) - p"\\") +
		C(p"\\" * (p(1) - (s"Xxvtrnfe$\\" + r"07")))
	local nd_string_literal = lpeg.Cmt(
		p"<<<" *hw* Ct(p"'" * name * p"'" * Cg(lpeg.Cp(), "mid")),
		function(str, i, m)
			local hd_id = new_line * p(m.name)
			local curr_nowdoc = Ct(new_line *
				Cf((-p(hd_id) * nd_char)^0, fold_string) *
				lpeg.Cp() * hd_id * p";"^-1 * new_line *
				lpeg.Cp()) / function(t)
					return { 1, t[2], value = t[1], len = t[3] } end
			local nd_match = curr_nowdoc:match(str:sub(i))
			if nd_match == nil then return nil end
			return i + nd_match.len - 1, {
				m.mid, m.mid + nd_match.len, value = nd_match.value
			}
		end)


	local string_literal =
		Cg(nd_string_literal, "nd_string") +
		Cg(hd_string_literal, "hd_string") +
		Cg(dq_string_literal, "dq_string") +
		Cg(shell_string_literal, "shell_string") +
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
		C(cat(operators)) / function(str) return str:lower() end, "operator")

	-- Input
	local token = Ct(
		Cg(lpeg.Cp(), 1) *
		(qualified_name +
		variable_name +
		literal +
		operator +
		keyword +
		name) *
		Cg(lpeg.Cp(), 2)
	)
	local input = Ct(Cg(lpeg.Cp(), 1) *
		(Cg(whitespace, "whitespace") + Cg(comment, "comment")) *
		Cg(lpeg.Cp(), 2)) + token

	in_string_var_expr = Ct(
		Cg(Ct((-p"}" * input)^0), "expr") * p"}" *
		Cg(lpeg.Cp(), 2))


	-- Script:
	local start_tag = p"<?php" + p"<?="
	local end_tag = p"?>"
	local text = sequence_of(-start_tag * p(1)) -- anything but start_tag
	local script_section = Ct(
		Cg(lpeg.Cp(), 1) *
		Cg(text^-1, "pre_text") * -- text to echo
		Cg(start_tag, "tag") * -- which start tag?
		Cg(Ct((-p"?>" * input)^0), "script") * -- expressions to execute
		(p"?>" * -- maybe end tag?
			Cg(text^-1, "post_text"))^-1 * -- if so, more text to echo
		Cg(lpeg.Cp(), 2)
	)
	local script = Ct(script_section^0)

	return {
		token = token,
		script = script
	}
end)()
