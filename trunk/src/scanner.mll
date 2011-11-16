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
| "const"				{ CONST }
| "uint("['0'-'9']+')' as lit	        { UINT(int_of_string (String.sub lit 5 (String.length lit - 6))) }

| "int("['0'-'9']+')' as lit       	{ INT(int_of_string (String.sub lit 4 (String.length lit - 5))) }


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
| "|" 					{ C_OR }
| "POS"					{ POS }
| "ASYNC"				{ ASYNC }
| ['a'-'z''A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as lit  { ID(lit) }
| ['0'-'9']+ as lit                     		  { NUM(int_of_string lit) }

| eof 					{ EOF }
| _ as char 				{ raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/"					{ token lexbuf }
| _					{ comment lexbuf }

and sl_comment = parse
  '\n'					{ token lexbuf }
| _					{ sl_comment lexbuf }

