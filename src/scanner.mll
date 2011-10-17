{ open Parser }

rule token = parse
  [' ' '\t' '\r'] { token lexbuf}


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
| ',' 					{ COMMA }

  
(* Unary Operators *)
| "!" 					{ NOT }  
(* Need to hande unary minus as well *)


| '=' 					{ ASN }
 
  
| '.' 					{ DOT }
  
  
| ';'					{ SEMI }
| ':'					{ COLON }

| '('					{ LPAREN }
| ')'					{ RPAREN }
| '['					{ LBRACKET }
| ']'					{ RBRACKET }
| '{'					{ LBRACE }
| '}' 					{ RBRACE }


  
(* keywords *)
| "if" 					{ IF }
| "else" 				{ ELSE }
| "while" 				{ WHILE }
| "for" 				{ FOR }
| "switch" 				{ SWITCH }
| "case"				{ CASE }
| '|' 					{ C_OR }
| "break" 				{ BREAK } (*Not sure it is really necessary. VHDL does not allow to break loops, while case always breaks!*)
| "const" 				{ CONST }
  
(* It would be nice if the Int constructor could take an integer argument directly from the scanner*)
| "unsigned("['0'-'9']+')' as lit 	{ UNSIGNED(String.sub lit 4 (String.length lit - 5), 0, true) }
| "int("['0'-'9']+')' as lit 	        { INT(String.sub lit 4 (String.length lit - 5), 0, true) }
  
| "struct"				{ STRUCT }
| "return" 				{ RET }
| ['0'-'9']+ as lit			{ NUM(int_of_string lit) }
| ['a'-'z''A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as lit  { ID(lit) }
| "POS" 				{ POS }
| "ASYNC" 				{ ASYNC }
| eof 					{ EOF }

and comment = parse
  "*/"					{ token lexbuf }
| _					{ comment lexbuf }

and sl_comment = parse
  '\n'					{ token lexbuf }
| _					{ sl_comment lexbuf }
