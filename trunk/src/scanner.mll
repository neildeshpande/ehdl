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
| "switch" 				{ SWITCH }
| "case"				{ CASE }
| "default"             		{ DEFAULT }
| "POS"					{ POS }
| "ASYNC"				{ ASYNC }
| "_"|"abs"|"access"|"after"|"alias"|"all"|"and"|"architecture"|"array"|"assert"|"attribute"|"begin"|"block"|"body"|"buffer"|"bus"|"component"|"configuration"|"constant"|"disconnect"|"downto"|"elsif"|"end"|"entity"|"exit"|"file"|"for"|"function"|"generate"|"generic"|"group"|"guarded"|"impure"|"in"|"inertial"|"inout"|"is"|"label"|"library"|"linkage"|"literal"|"loop"|"map"|"mod"|"nand"|"new"|"next"|"nor"|"not"|"null"|"of"|"on"|"open"|"or"|"others"|"out"|"package"|"port"|"postponed"|"procedure"|"process"|"pure"|"range"|"record"|"register"|"reject"|"return"|"rol"|"ror"|"select"|"severity"|"signal"|"shared"|"sla"|"sli"|"sra"|"srl"|"subtype"|"then"|"to"|"transport"|"type"|"unaffected"|"units"|"until"|"use"|"variable"|"wait"|"when"|"with"|"xnor"|"xor" as reserved { raise (Failure("Reserved Keyword " ^  reserved)) }
| ['a'-'z''A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']* as lit  { ID(lit) }
| ['0'-'9']+ as lit                     		  { NUM(int_of_string lit) }

| eof 					{ EOF }
| _ as char 				{ raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/"					{ token lexbuf }
| _					{ comment lexbuf }

and sl_comment = parse
  '\n'					{ token lexbuf }
| _					{ sl_comment lexbuf }

