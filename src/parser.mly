{open Ast }

%token PLUS MINUS TIMES DIVIDE MODULO LT GT LTE GTE EQ NEQ 
%token OR AND XOR SHL SHR COMMA NOT ASN DOT
%token SEMI COLON LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token IF ELSE WHILE FOR SWITCH CASE C_OR BREAK CONST INT UNSIGNED
%token STRUCT RET POS ASYNC EOF
%token <int> NUM
%token <string> ID

(*Need to check precedence in C!*)
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

%start program
%type <Ast.program> program

%%


