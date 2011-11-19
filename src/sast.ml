open Ast

module StringMap = Map.Make(String);;

let function_table = StringMap.empty

type types = 
  	Bus 
  | Array 
  | Const
  | Function

let string_of_sast_type (t : types) =
  match t with
  	  Bus -> "Bus"
    | Array -> "Array"
    | Const -> "Const"
    | Function -> "Function"  

(* Covers both buses and array, out of bounds exceptions should done at run time *)
type symbol_table = {
  parent : symbol_table option;
  variables : (Ast.bus * int) list
                    }
  
type expr_detail =
  IntConst of int
| Id of string
| Call of fdecl * expression list
| BinOp of expr_detail * Ast.operator * expr_detail
| Basn of Ast.bus * expr_detail
| Aasn of Ast.bus * int * expr_detail * expr_detail
| UnOp of Ast.operator * expr_detail
| Subbus of Ast.bus * int * int
| Barray of Ast.bus * int * expr_detail
(* | Const of Ast.bus * int *)
  
and expression = expr_detail * types
  
  
let rec find_variable (scope : symbol_table) name =
	try
		List.find ( 
    				fun ( v, _ ) -> v.name = name 
    				) scope.variables
	with Not_found ->
		match scope.parent with
			  Some(parent) -> find_variable parent name
			| _ -> raise Not_found

type translation_environment = {
	return_type : types; (* Function’s return type *)
	scope : symbol_table; (* symbol table for vars *)
}

(* Check type compatibility of e1 and e2 for the given op *)
(* Raise error if incompatible else return unit *)
let check_types e1 op e2 = ()
let check_basn vbus e1 = () 
let check_aasn vbus e1 e2 = ()
let check_call actuals env func_decl = ()
let check_subbus vbus x y = ()
let check_array_dereference  varray size e1 = ()  
  
exception Error of string  
  
let rec expr env = function
(* An integer constant: convert and return Int type *)
	Ast.Num(v) -> IntConst(v), Const
(* An identifier: verify it is in scope and return its type *)
  | Ast.Id(vname) ->
    	let vbus, _ = 
       		try
    			find_variable env.scope vname (* locate a variable by name *)
    		with Not_found ->
    			raise (Error("undeclared identifier " ^ vname))
    	in Id(vbus.name), Bus
  | Ast.Binop(e1, op, e2) ->
    	let e1 = expr env e1 (* Check left and right children *)
		and e2 = expr env e2 in
    	check_types e1 op e2;
		BinOp(fst e1, op, fst e2), Bus (* Success: result is bus *)
  | Ast.Basn(vname, e1) ->
		let e1 = expr env e1
  		and vbus, _ = find_variable env.scope vname
    	in
    	check_basn vbus e1;
    	Basn(vbus, fst e1), Bus
  | Ast.Aasn(vname, e1, e2) ->
		let e1 = expr env e1
  		and e2 = expr env e2
  		and vbus, size = find_variable env.scope vname
    	in
    	check_aasn vbus e1 e2;
    	Aasn(vbus, size, fst e1, fst e2), Bus
  | Ast.Call(fname, expr_list) ->
    	let func_decl = 
       		try StringMap.find fname function_table
         with Not_found -> raise (Failure ("undefined function " ^ fname))
     	in let expr_detail_list = []
         in let _ = List.fold_left (
           fun (env : translation_environment) (actual : Ast.expr) ->  
             let e1 = expr env actual
             in ignore(e1::expr_detail_list); env) env expr_list
            in Call(func_decl, expr_detail_list), Function
  | Ast.Unop(op, e1) ->
    	let (e1, t1) = expr env e1
     in UnOp(op, e1), t1
  | Ast.Subbus(vname, x, y) ->
    	let vbus, _ = find_variable env.scope vname
     	in check_subbus vbus x y;
    	Subbus(vbus, x, y), Bus
  | Ast.Barray(vname, e1) ->
    	let (e1, t1) = expr env e1
     and varray, size = find_variable env.scope vname
     in check_array_dereference varray size e1;
    Barray(varray, size, e1), Array
    
  (*
  | Ast.Const(cbus, cvalue) ->
    	check_bus_width cbus int;
    	ignore(cbus::env.scope.variables);
    	Const(cbus, cvalue), Const
  *)  
    	
  