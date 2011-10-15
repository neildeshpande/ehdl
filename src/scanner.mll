{ open Parser }

rule token = parse
  [' ' '\t' '\r'] { token lexbuf }
| '\n' 					{ NEWLINE }
| '+' 					{ PLUS }
| '-' 					{ MINUS }
| '*' 					{ TIMES }
| '/' 					{ DIVIDE }
| '%' 					{ MODULO }
| '=' 					{ SET }
| '<'					{ LT }
| '>' 					{ GT }
| "<="					{ LTE }
| ">="					{ GTE }
| "==" 					{ EQ }
| "!=" 					{ NEQ }
| "!" 					{ NOT }
| "||"					{ OR }
| "&&"					{ AND }
| "^" 					{ XOR }
| "<<"					{ SHL }
| ">>" 					{ SHR }
| ',' 					{ COMMA }
| ';'					{ SEMI }
| ':'					{ COLON }
| '.' 					{ DOT }
| '('					{ L_PAREN }
| ')'					{ R_PAREN }
| '['					{ L_BRACKET }
| ']'					{ R_BRACKET }
| '{'					{ L_BRACE }
| '}' 					{ R_BRACE }
| "/*"					{ COM_BEGIN }
| "*/"					{ COM_END }
| "//" 					{ COM_LINE }
| "if" 					{ IF }
| "else" 				{ ELSE }
| "while" 				{ WHILE }
| "for" 				{ FOR }
| "switch" 				{ SWITCH }
| "case"				{ CASE }
| '|' 					{ C_OR }
| "break" 				{ BREAK } (*Not sure it is really necessary. VHDL does not allow to break loops, while case always breaks!*)
| "const" 				{ CONST }
| "unsigned" 				{ UNSIGNED }
| "int" 				{ INT }
| "struct"				{ STRUCT }
| "return" 				{ RET }
| ['0'-'9']+ as lit			{ NUM(int_of_string lit) }
| ['a'-'z'] ['a'-'z' '_' '0'-'9']* as lit  { ID(lit) }
| "POS" 				{ POS }
| "ASYNC" 				{ ASYNC }
| eof 					{ EOF }

