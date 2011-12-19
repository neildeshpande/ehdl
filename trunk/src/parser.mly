%{open Ast%}

%token PLUS MINUS TIMES LT GT LTE GTE EQ NEQ 
%token OR AND XOR SHL SHR NOT
%token IF ELSE WHILE FOR
%token ASN SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA CONST
%token SWITCH CASE DEFAULT COLON POS ASYNC EOF
%token <int> NUM INT
%token <string> ID

/*Need to check precedence in C!*/
%nonassoc NOELSE
%nonassoc ELSE
%right ASN
%left EQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left OR XOR
%left AND
%left SHL SHR
%right NOT
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
				      size = $2;
				      init = $4;
				      async = $1;
                      isAssigned = Array.make $2 false} }
async_opt :
		{ false }
| ASYNC 	{ true }

spec :
  INT		{ $1 }

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
				{ raise (Failure("Empty output port list"))   }
 | bdecl			{ [$1] }
 | LPAREN port_list RPAREN {$2}  

port_list :
			{ [] }
 | port_rlist		{ List.rev($1) }

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
					    	         size = $2;
					    	         init = $7;
					    	         async = $1; 
                                     isAssigned = Array.make ($5) false}, $5 ) } /* setting the bitfield to the size of the array and not the size of the bus*/

stmt :
  LBRACE stmt_list RBRACE			{ Block(List.rev $2) }
| asn_expr SEMI              			{ Expr($1) }
| POS LPAREN other_expr RPAREN SEMI			{ Pos($3) } 
| IF LPAREN other_expr RPAREN stmt %prec NOELSE 	{ If($3, $5, Block([]))}
| IF LPAREN other_expr RPAREN stmt ELSE stmt    	{ If($3,$5,$7) }
| WHILE LPAREN other_expr RPAREN stmt			{ While($3, $5) }
| SWITCH LPAREN other_expr RPAREN LBRACE case_stmt case_list RBRACE{ Switch($3,$6::(List.rev $7)) }
/* Enforcing at least one case_stmt in the parser so no need to do it later */ 
| LPAREN actuals_list RPAREN ASN ID LPAREN actuals_list RPAREN SEMI	{ Call($5, (List.rev $2), (List.rev $7)) }


stmt_list :
{ [] }
| stmt_list stmt	{ $2 :: $1 }


other_expr :
  NUM						{ Num($1) }
| ID						{ Id($1) }
| ID LBRACKET other_expr RBRACKET		{ Barray($1, $3) }
| ID LPAREN NUM COLON NUM RPAREN		{ Subbus($1, $3, $5) }
| ID LPAREN NUM RPAREN				{ Subbus($1, $3, $3) }
| MINUS other_expr %prec UMINUS			{ Unop(Umin, $2) }
| NOT other_expr %prec NOT			{ Unop(Not, $2) }
| other_expr PLUS other_expr			{ Binop($1, Add, $3) }
| other_expr MINUS other_expr			{ Binop($1, Sub, $3) }
| other_expr TIMES other_expr			{ Binop($1, Mul, $3) }
| other_expr LT other_expr			{ Binop($1, Lt, $3) }
| other_expr GT other_expr			{ Binop($1, Gt, $3) }
| other_expr LTE other_expr			{ Binop($1, Lte, $3) }
| other_expr GTE other_expr			{ Binop($1, Gte, $3) }
| other_expr EQ other_expr			{ Binop($1, Eq, $3) }
| other_expr NEQ other_expr			{ Binop($1, Neq, $3) }
| other_expr OR other_expr			{ Binop($1, Or, $3) }
| other_expr AND other_expr			{ Binop($1, And, $3) }
| other_expr XOR other_expr			{ Binop($1, Xor, $3) }
| other_expr SHL other_expr			{ Binop($1, Shl, $3) }
| other_expr SHR other_expr			{ Binop($1, Shr, $3) }
| LPAREN other_expr RPAREN			{ $2 }

/*No multiple assignments within the same line! a = b = c + 1 is not permitted*/
asn_expr :
| ID ASN other_expr			{ Basn($1, $3) }
| ID LBRACKET other_expr RBRACKET ASN other_expr { Aasn($1, $3, $6) }
| ID LPAREN NUM COLON NUM RPAREN ASN other_expr {Subasn($1, $3, $5, $8)}
| ID LPAREN NUM RPAREN ASN other_expr		{Subasn($1, $3, $3, $6)}

case_list :
 { [] : (expr * stmt) list} 
| case_list case_stmt  { $2 :: $1 }

case_stmt : 
  CASE other_expr COLON stmt_list {($2,Block($4)) } 
| DEFAULT COLON stmt_list {(Noexpr,Block($3))}


/* ehdl.ml checks whether expressions are used*/
actuals_list :
  actuals_rlist			{ List.rev $1 }

actuals_rlist :
  other_expr				{ [$1] } 
| actuals_list COMMA other_expr		{ $3 :: $1 }
