%{open Ast%}

%token PLUS MINUS TIMES DIVIDE MODULO LT GT LTE GTE EQ NEQ 
%token OR AND XOR SHL SHR NOT
%token IF ELSE WHILE FOR
%token ASN SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA RETURN CONST STRUCT ENUM ARRAY
%token SWITCH CASE C_OR COLON POS ASYNC EOF
%token <int> NUM INT UNSIGNED
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


/* a "global" could be either a "constant" or a "struct" */
gdecl :
  CONST formal_decl ASN NUM SEMI	{ Const ($2, $4) }
| ID ASN LBRACE vdecl_opt RBRACE	{ Struct($1, $4) }


/* a "function declarator" is a "output type", a "list of input bus" and a "body" */
fdecl : 
  f_type ID LPAREN input_opt RPAREN LBRACE fbody RBRACE
			{ { ftype   = $1;
			    fname   = $2;
			    formals = $4;
			    body    = $7 } }

/* an "function type" is the ID of a bus group (struct) or int(size) or an unsigned(size) */
f_type:
   INT			{ ("int("^(string_of_int $1)^")") }
|  UNSIGNED		{ ("unsigned("^(string_of_int $1)^")") }
|  ID			{ $1 }

/* "input_opt" is the ordered "list of input bus" for a function */
input_opt :
   			{ [] }
| formal_list		{ List.rev $1 }

/* "forml_list" is the reversed "list of input bus" for a function */
formal_list :
   formal_decl				{ [$1] }
|  formal_list COMMA formal_decl	{ $3 :: $1 }

formal_decl : 
  formal_bus_decl	{ $1 }
| ASYNC	formal_bus_decl	{ { name = $2.name;
			    size = $2.size;
			    init = $2.init;
			    unsigned = $2.unsigned;
			    async = true } }

/* a "formal" is a "bus" either with an integer value or an unsigned value */
formal_bus_decl : 
  INT ID ASN NUM	{ { name = $2;
			    size = $1;
			    init = $4;
			    unsigned = false;
			    async = false } }
| UNSIGNED ID ASN NUM	{ { name = $2;
			    size = $1;
			    init = $4;
			    unsigned = true;
			    async = false } }

/* the "function body" is the list of "local variables" and a list of "statements" 
   variables of type Struct are declared implicitly and assigned through function call only.
   Force to match the return type! We must get the bus list from the struct*/
fbody :
                        { [], [] }
| fbody vdecl		{ ($2 :: fst $1), snd $1 }
| fbody stmt            { fst $1, ($2 :: snd $1) }


/* "vdecl_opt" is the ordered list of "variable declarators" */
vdecl_opt :
 vdecl_list		{ List.rev $1 }

/* "vdecl_list" is the reversed list of "variable declarators" */
vdecl_list :
  			{ [] }
| vdecl_list vdecl	{ $2 :: $1 }


vdecl :
  bus_decl			{ $1 }
| ASYNC bus_decl		{ { name = $2.name;
				    size = $2.size;
				    init = $2.init;
				    unsigned = $2.unsigned;
				    async = true } }

/* Must always assign a variable with a constant in the same line of declaration for reset! */
/* a "variable" is a bus either with an integer value or an unsigned value */
bus_decl :
  INT ID ASN NUM SEMI		{ { name = $2;
				    size = $1;
				    init = $4;
				    unsigned = false;
				    async = false; } }
| UNSIGNED ID ASN NUM SEMI	{ { name = $2;
				    size = $1;
				    init = $4;
				    unsigned = true;
				    async = false } } 

stmt_list :
				{ [] }
| stmt_list stmt		{ $2 :: $1 }

stmt :
 expr SEMI              			{ Expr($1) }
| RETURN expr SEMI      			{ Return($2) }
| POS LPAREN expr RPAREN SEMI			{ Pos($3) }
| LBRACE stmt_list RBRACE			{ Block(List.rev $2) }
| IF LPAREN expr RPAREN stmt %prec NOELSE 	{ If($3, $5, Block([]))}
| IF LPAREN expr RPAREN stmt ELSE stmt    	{ If($3,$5,$7) }
| FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt  { For($3,$5,$7,$9) }
| WHILE LPAREN expr RPAREN stmt			{ While($3, $5) }
| SWITCH LPAREN expr RPAREN LBRACE stmt RBRACE	{ Switch($3, $6) }
| CASE expr COLON stmt				{ Case($2, $4) } /*Ocaml code outside the parser should handle
								   the dependency of Switch - Case statements */

/* Be careful while translating, to check that the user does not override
  the outputs with other local variables!!!! Raise an error! */

expr :
  ID				{ Id($1) }
| NUM				{ Num($1) }
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
| ID ASN expr			{ Assign($1, $3) }
| ID LPAREN actuals_opt RPAREN	{ Call($1, $3) }
| ENUM ID ASN actuals_list	{ Enum($2, $4) } /* This expression changes the environment! */
| ARRAY ID LBRACKET NUM RBRACKET formal_decl { Bus_array($2, $6, $4) } /* This expression changes the environment! */

/* When calling a function: List.map (fun b -> 
				     {name = Instance_ID^"."^b.name; size=b.size;
				     init=b.init; unsigned=b.unsigned}) struct_bus_list */
/* STRUCT_ID ID = FUNC_ID(INPUT_LIST) -> rename outputs ID.(b.name) */

/* "actuals_opt" is the ordered list of arguments for a function call */
actuals_opt :
				{ [] }
| actuals_list 			{ List.rev $1 }

/* "actual_list" is the reversed list of arguments for a function call */
/* this list is also used for enum type declaration (See expression) */
actuals_list :
  ID				{ [$1] }	/*Should we allow also expressions?*/
| actuals_list COMMA ID		{ $3 :: $1 }
