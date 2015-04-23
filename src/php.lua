package.path = '?.lua;src/?.lua;deps/LPegLJ/src/?.lua'

io = require'io'
lpeg = require'lpeglj'
inspect = require'inspect'

local function dump(x)
	io.write(inspect(x) .. '\n')
end

-- Parse:
local grammar = (function()
	lpeg.enableleftrecursion(true)
	local v, r, p, s = lpeg.V, lpeg.R, lpeg.P, lpeg.S
	local C, Cg, Ct = lpeg.C, lpeg.Cg, lpeg.Ct

	-- Comments:
	local new_line = p"\r\n" + p"\n" + p"\r"
	local input_char = p(1) - new_line
	local input_characters = p{"input_characters",
		input_characters = v"input_characters" * input_char + input_char
	}
	local single_line_comment =
		p"//" * input_characters^-1 +
		p"#" * input_characters^-1
	local end_comment = p"*/"
	local delimited_comment = p{"delimited_comment",
		in_comment = v"in_comment" * -end_comment * p(1) + -end_comment * p(1),
		delimited_comment = p"/*" + v"in_comment"^-1 + end_comment
	}

	-- Whitespace:
	local whitespace_char = new_line + p" " + p"\t"
	local whitespace = p{"whitespace", whitespace =
		v"whitespace" * whitespace_char + whitespace_char
	}
	-- *w* is used hereafter for when whitespace is allowed but not required:
	local w = whitespace^-1

	-- Names:
	local nondigit = r"az" + r"AZ" + p"_"
	local digit = r"09"
	local name_nondigit = nondigit + r"\127\255"
	local name = p{"name", name =
		v"name" * name_nondigit +
		v"name" * digit +
		name_nondigit
	}
	local ns_name = p{"ns_name", ns_name =
		v"ns_name" *w* p"\\" *w* name +
		name
	}
	local ns_name_as_prefix =
		p"namespace" *w* p"\\" *w* ns_name *w* p"\\" +
		p"namespace" *w* p"\\" +
		p"\\"^-1 *w* ns_name *w* p"\\" +
		p"\\"
	local qualified_name = ns_name_as_prefix *w* name
	local variable_name = p"$" * name

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
		"abstract", "and", "as", "break", "callable", "case", "catch", "class",
		"clone", "const", "continue", "declare", "default", "do", "echo",
		"elseif", "enddeclare", "endfor", "endforeach", "endif", "endswitch",
		"endwhile", "extends", "final", "finally", "for", "foreach", "function",
		"global", "goto", "if", "implements", "include", "include_once",
		"instanceof", "insteadof", "interface", "namespace", "new", "or",
		"print", "private", "protected", "public", "require", "require_once",
		"return", "static", "switch", "throw", "trait", "try", "use", "var",
		"while", "xor", "yield"
	}
	local keyword = (function(table)
		-- apply + operator to everything in table:
		local pat = p""
		for i=1,#table do
			pat = pat + ci(table[i])
		end
		return pat
	end)(keywords)

	-- Literals:
	local decimal_literal = p{"decimal_literal", decimal_literal =
		v"decimal_literal" * digit + r"19"
	}
	local octal_literal = p{"octal_literal", octal_literal =
		v"octal_literal" * r"07" + p"0"
	}
	local hexadecimal_digit = r("09", "af", "AF")
	local hexadecimal_literal = p{"hexadecimal_literal", hexadecimal_literal =
		v"hexadecimal_literal" * hexadecimal_digit +
		(p"0x" + p"0X") * hexadecimal_digit
	}
	local binary_literal = p{"binary_literal", binary_literal =
		v"binary_literal" * r"01" +
		(p"0b" + p"0B") * r"01"
	}
	local integer_literal =
		Cg(binary_literal, "binary") +
		Cg(hexadecimal_literal, "hex") +
		Cg(octal_literal, "octal") +
		Cg(decimal_literal, "decimal")

	local digit_sequence = p{"digit_sequence", digit_sequence =
		v"digit_sequence" * digit + digit
	}
	local exponent_part = s"eE" * s"+-"^-1 * digit_sequence
	local fractional_literal =
		digit_sequence^-1 * p"." * digit_sequence +
		digit_sequence * p"."
	local floating_literal = Cg(
		fractional_literal * exponent_part^-1 +
		digit_sequence * exponent_part
	, "floating_literal")

	local literal = Cg(Ct(
		floating_literal +
		integer_literal -- + string_literal
	), "literal")

	-- Script:
	local start_tag = p"<?php" + p"<?="
	local end_tag = p"?>"
	local text = p{"text", -- anything but start_tag:
		text = v"text" * -start_tag * p(1) + -start_tag * p(1)
	}
	local script = p{
		"script",
		in_text = v"in_text" * -end_tag * p(1) + -end_tag * p(1),
		script = Ct(v"script" * v"script_section") + v"script_section",
		script_section = Ct(
			C(text^0) * -- text to echo
			C(start_tag) * -- which start tag?
			C(v"in_text"^0) * -- expressions to execute
			(p"?>" * -- maybe end tag?
				C(text^0))^-1 -- if so, more text to echo
		)
	}

	return {
		literal = Ct(literal),
		script = script
	}
end)()

dump(grammar)
dump(grammar.literal:match("01234.1234"))
