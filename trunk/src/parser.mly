%{open Ast%}

%token PLUS MINUS TIMES DIVIDE MODULO LT GT LTE GTE EQ NEQ 
%token OR AND XOR SHL SHR NOT
%token IF ELSE WHILE FOR
%token ASN SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA CONST
%token SWITCH CASE DEFAULT C_OR COLON POS ASYNC EOF
%token <int> NUM INT UINT
%token <string> ID

/*Need to check precedence in C!*/
%nonassoc NOELSE
%nonassoc ELSE
%right NOT
%right ASN
%left EQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left OR XOR
%left AND
%left SHL SHR
%nonassoc UMINUS /* Highest precedence for unary minus! */

%start program
%type <Ast.program> program

%%

/* a "program" is a list of "globals" and a list of "function declarators" */
program :
			{ [],[] }
|  program gdecl	{ ($2 :: fst $1), snd $1}
|  program fdecl	{ fst $1, ($2 :: snd $1) }

/* Constant arrays are not useful */
gdecl :
  CONST bdecl SEMI		{ Const ($2, $2.init) }

bdecl :
  async_opt spec ID init_opt	  { { name = $3;
				      size = fst $2;
				      init = $4;
				      unsigned = snd $2;
				      async = $1 } }
async_opt :
		{ false }
| ASYNC 	{ true }

spec :
  INT		{ ($1,false) }
| UINT 		{ ($1, true) }

init_opt :
		{ 0 }
| ASN NUM	{ $2 }

/* a "function declarator" is a "list of output bus", a "list of input bus" and a "body" */
fdecl : 
  out_port_list ID LPAREN port_list RPAREN LBRACE fbody RBRACE
			{ { portout = $1;
			    fname   = $2;
			    portin  = $4;
			    body    = $7 } }
/* Be careful while translating, to check that the user does not override
  the ports with other local variables!!!! Raise an error! */

/* no need for parens if just one output bus */
out_port_list:
   bdecl			{ [$1] }
 | LPAREN port_list RPAREN {$2}  

port_list :
  port_rlist		{ List.rev($1) }

/* VHDL ports cannot be custom type */
port_rlist :
  bdecl			{ [$1] }
| port_list COMMA bdecl	{ $3 :: $1 }


/* the "function body" is the list of "local variables" and a list of "statements"  */
fbody :
			{ [], [] }
| fbody local		{ ($2 :: fst $1), snd $1 }
| fbody stmt            { fst $1, ($2 :: snd $1) }

local :
  vdecl SEMI		{ $1 }

vdecl :
  bdecl			{ Bdecl($1) }  
| adecl			{ Adecl(fst $1, snd $1) }

adecl :
  async_opt spec ID LBRACKET NUM RBRACKET init_opt { ( { name = $3;
					    	         size = fst $2;
					    	         init = $7;
					    	         unsigned = snd $2;
					    	         async = $1 }, $5 ) }

stmt :
  LBRACE stmt_list RBRACE			{ Block(List.rev $2) }
| expr SEMI              			{ Expr($1) }
| POS LPAREN expr RPAREN SEMI			{ Pos($3) } 
| IF LPAREN expr RPAREN stmt %prec NOELSE 	{ If($3, $5, Block([]))}
| IF LPAREN expr RPAREN stmt ELSE stmt    	{ If($3,$5,$7) }
/*| IF LPAREN expr RPAREN stmt ELSE stmt    { If(($3, $5)::$6) }*/ 
| FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt  { For($3,$5,$7,$9) }
| WHILE LPAREN expr RPAREN stmt			{ While($3, $5) }
| SWITCH LPAREN expr RPAREN LBRACE case_stmt case_list RBRACE{ Switch($3,$6::(List.rev $7)) } 
/*Ocaml code outside the parser should handle
   the dependency of Switch - Case statements */

stmt_list :
{ [] }
| stmt_list stmt	{ $2 :: $1 }

expr :
  NUM				{ Num($1) }
| ID				{ Id($1) }
| ID LBRACKET expr RBRACKET	{ Barray($1, $3) }
| ID LPAREN NUM COLON NUM RPAREN { Subbus($1, $3, $5) }
| MINUS expr %prec UMINUS	{ Unop(Umin, $2) }
| NOT expr %prec NOT		{ Unop(Not, $2) }
| expr PLUS expr		{ Binop($1, Add, $3) }
| expr MINUS expr		{ Binop($1, Sub, $3) }
| expr TIMES expr		{ Binop($1, Mul, $3) }
| expr DIVIDE expr		{ Binop($1, Div, $3) }
| expr MODULO expr		{ Binop($1, Mod, $3) }
| expr LT expr			{ Binop($1, Lt, $3) }
| expr GT expr			{ Binop($1, Gt, $3) }
| expr LTE expr			{ Binop($1, Lte, $3) }
| expr GTE expr			{ Binop($1, Gte, $3) }
| expr EQ expr			{ Binop($1, Eq, $3) }
| expr NEQ expr			{ Binop($1, Neq, $3) }
| expr OR expr			{ Binop($1, Or, $3) }
| expr AND expr			{ Binop($1, And, $3) }
| expr XOR expr			{ Binop($1, Xor, $3) }
| expr SHL expr			{ Binop($1, Shl, $3) } /*check that $3 is an integer!*/
| expr SHR expr			{ Binop($1, Shr, $3) }
| ID ASN expr			{ Basn($1, $3) }
| ID LBRACKET expr RBRACKET ASN expr { Aasn($1, $3, $6) }
| ID LPAREN actuals_list RPAREN	{ Call($1, $3) }

case_list :
/* need to explicitly state the type, does not get inferred otherwise 
because of the stmt_list definition*/ 
 { [] : (expr * stmt) list} 
| case_list case_stmt  { $2 :: $1 }

case_stmt : 
  CASE expr COLON stmt_list {($2,Block($4)) } 
| DEFAULT COLON stmt_list {(Noexpr,Block($3))}

/*
elseif_lst : 
{ [] } 
| elsif_lst elsif_stmt { $2 :: $1 }    
| elsif_lst else_stmt  { $2 :: $1 }  

elsif_stmt: 
  ELSEIF LPAREN expr RAPERN stmt {($3,$5)} 

else_stmt: 
  ELSE stmt {(Noexpr,$2)}    
*/
  
actuals_list :
  actuals_rlist			{ List.rev $1 }

actuals_rlist :
  expr				{ [$1] }
| actuals_list COMMA expr	{ $3 :: $1 }

