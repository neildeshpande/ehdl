{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n']			{ token lexbuf }

(* Comment *)
| "/*" 					{ comment lexbuf }
| "//"					{ sl_comment lexbuf }
(* Binary Operators *)
| '+' 					{ PLUS }
| '-' 					{ MINUS }
| '*' 					{ TIMES }
| '/' 					{ DIVIDE }
| '%' 					{ MODULO }
| '<'					{ LT }
| '>' 					{ GT }
| "<="					{ LTE }
| ">="					{ GTE }
| "==" 					{ EQ }
| "!=" 					{ NEQ }
| "||"					{ OR }
| "&&"					{ AND }
| "^" 					{ XOR }
| "<<"					{ SHL }
| ">>" 					{ SHR }
(* Unary Operators *)
| "!" 					{ NOT }  
(* Need to hande unary minus as well *)

(* types keywords *)
| "struct"				{ STRUCT }
| "const"				{ CONST }
| "unsigned("['0'-'9']+')' as lit       { UNSIGNED(int_of_string (String.sub lit 9 (String.length lit - 10))) }

| "int("['0'-'9']+')' as lit       	{ INT(int_of_string (String.sub lit 4 (String.length lit - 5))) }
| "enum" 				{ ENUM }
| "array"				{ ARRAY }
| ['0'-'9']+ as lit                     		  { NUM(int_of_string lit) }
| ['a'-'z''A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as lit  { ID(lit) }

| '='					{ ASN }
| ','					{ COMMA }
| ';'					{ SEMI }
| ':'					{ COLON }

| '('					{ LPAREN }
| ')'					{ RPAREN }
| '['					{ LBRACKET }
| ']'					{ RBRACKET }
| '{'					{ LBRACE }
| '}'					{ RBRACE }
(* keywords *)
| "if" 					{ IF }
| "else" 				{ ELSE }
| "while" 				{ WHILE }
| "for" 				{ FOR }
| "switch" 				{ SWITCH }
| "case"				{ CASE }
| '|' 					{ C_OR }
| "return"				{ RETURN }
| "POS"					{ POS }
| "ASYNC"				{ ASYNC }

| eof 					{ EOF }
| _ as char 				{ raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/"					{ token lexbuf }
| _					{ comment lexbuf }

and sl_comment = parse
  '\n'					{ token lexbuf }
| _					{ sl_comment lexbuf }
