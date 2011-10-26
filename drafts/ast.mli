type operator = Add | Sub | Mul | Div | Mod| Lt | Gt | Lte | Gte | Eq | Neq | Or | And | Xor | Shl | Shr | Not | Umin

type bus = {name : string; size : int; init : int; unsigned : bool; async : bool}

type gdecl =
  Const of bus * int (* bus * constant value *)	
| Struct of string * bus list (*struct ID * bus list of the struct *)


type expr =
  Num of int
| Id of string			(* bus or struct name *)
| Binop of expr * operator * expr
| Unop of operator * expr
| Assign of string * expr	(* bus name * Value *)
| Call of string * string list	(* function name * arguments id *)
| Enum of string * string list  (* enum ID * enum lables *)
				(* translate enum into 	type ID_t is label list;
							signal ID : ID_t; *)
				(* the encoding is made automatically by the synth. *)
| Bus_array of string * bus * int (* array ID * bus format * array length *)
				  (* translate array into	type ID_t is array (0 to array_length-1) of std_logic_vector(bus_size-1 downto 0);
								signal ID : ID_t := (others => (others => '0'));*)

(*Both enum and array expressions must add variables to the environment. Is it possible?
Is it better to make more complicated the variable declaration rule? *)

type stmt =
  Block of stmt list
| Expr of expr
| Return of expr
| Pos of expr		(*Insert a rule that avoids having Pos inside if else!*)
| If of expr * stmt * stmt 
| For of expr * expr * expr * stmt 
| While of expr * stmt
| Switch of expr * stmt	(* switch (ID) {...} *)
| Case of expr * stmt

type fbody =  bus list * stmt list

type func_decl = {
  ftype : string;	(* struct ID or int(size) or unsigned(size) *)
  fname : string;
  formals : bus list;
  body : fbody;
}

type program = gdecl list * func_decl list
