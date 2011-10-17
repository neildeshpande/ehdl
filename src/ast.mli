type operator = Add | Sub | Mul | Div | Mod| Lt | Gt | Lte | Gte | Eq | Neq | Or | And | Xor | Shl | Shr | Comma
  
type bus = {size : int; value : int; undefined : bool}
type types = Int of bus | Unsign of bus
  
(* Anonymous structs are not supported.
   Structs are broken down into their components by the parser
   e.g.: struct foo{
               int(4) bar;
               int(10) baz;
              } myS;
   
   will be broken by the parser into two ints foo.bar and foo.baz
   int
   *)

type param = {t : types; p : string}
type paramList = Params of param list

type expr = (* Expressions *)
      Literal of int (* 42 *)
    | Id of string (* foo *)
    | Binop of expr * operator * expr (* a + b *)
    | Assign of string * expr (* foo = 42 *)
    | Call of string * expr list (* foo(1, 25 *)
    | Noexpr (* for (;;) *)

type labelstmt = 
      Case of int * stmt
    | Default of stmt

type stmt = (* Statements *)
      Block of stmt list (* { ... } *)
    | Expr of expr (* foo = bar + 3; *)
    | Return of expr (* return 42; *)
    | If of expr * stmt * stmt (* if (foo == 42) {} else {} *)
    | For of expr * expr * expr * stmt (* for (i=0;i<10;i=i+1) { ... } *)
    | While of expr * stmt (* while (i<10) { i = i + 1 } *)
    | Switch of expr * (labelstmt list)
    
  
type arg = Arg of expr
type args = Args of arg list
  
type func_decl = {
	fname : string; (* Name of the function *)
	formals : string list; (* Formal argument names *)
	locals : string list; (* Locally defined variables *)
	body : stmt list;
}
  
  
(* Every variable is an int. We assume that the OCaml int can hold all our ints *)

type program = string list * func_decl list (* global vars, funcs *)
