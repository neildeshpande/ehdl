type operator = Add | Sub | Mul | Lt | Gt | Lte | Gte | Eq | Neq | Or | And | Xor | Shl | Shr | Not | Umin

type bus = {name : string; size : int; init : int; async : bool; isAssigned : bool array}

type gdecl =
  Const of bus * int (* bus * constant value *)	

type locals =
  Bdecl of bus
| Adecl of bus * int (* bus * array length *)

type expr =
  Num of int
| Id of string				(* Bus name *)
| Barray of string * expr		(* Array reference *)
| Subbus of string * int * int		(* Subbus reference *)
| Unop of operator * expr		(* Unary operations *)
| Binop of expr * operator * expr	(* Binary operations *)
| Basn of string * expr			(* bus name * value *)
| Aasn of string * expr * expr		(* Array name * array index * value *)
| Subasn of string * int * int * expr	(* Bus name * bit range * value *)
| Noexpr


type stmt =
  Block of stmt list
| Expr of expr
| Pos of expr		(*Insert a rule that avoids having Pos inside if else!*)
| If of expr * stmt * stmt 
| While of expr * stmt
| Switch of expr *  (expr * stmt) list 	(* switch (expr) {...} *)
| Call of string * (expr list) * (expr list) 

type fbody =  locals list * stmt list

type fdecl = {
  portout : bus list;
  fname   : string;
  portin  : bus list;
  body    : fbody;
}

type program = gdecl list * fdecl list
