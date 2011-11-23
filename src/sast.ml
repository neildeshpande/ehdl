open Ast

module StringMap = Map.Make(String);;

let function_table = StringMap.empty

exception Error of string  

type local_t = 
    In_port
  | Out_port
  | Int_signal
  
type types = 
    Bus 
  | Array 
  | Const
  | Function
  | Void

(* Covers both buses and array, out of bounds exceptions should done at run time *)
type symbol_table = {
  parent : symbol_table option;
  variables : (Ast.bus * int * types * local_t) list
                    }

type translation_environment = {
	scope : symbol_table; (* symbol table for vars *)
}

type function_decl = {	pout : Ast.bus list;
			fid  : string;
			pin  : Ast.bus list;
			floc : translation_environment;
			fbod : s_stmt list;
		     }

and expr_detail =
  Num of int
| Id of string
| Barray of Ast.bus * int * expr_detail
| Subbus of Ast.bus * int * int
| Unop of Ast.operator * expr_detail
| Binop of expr_detail * Ast.operator * expr_detail
| Basn of Ast.bus * expr_detail
| Aasn of Ast.bus * int * expr_detail * expr_detail
| Call of function_decl * expression list
| Noexpr
  
and expression = expr_detail * types
  
and s_stmt = 
    Block of symbol_table * (s_stmt list)
  | Expr of expression
  | If of expr_detail * s_stmt * s_stmt
  | For of expr_detail * expr_detail * expr_detail * s_stmt 
  | While of expr_detail * s_stmt
  | Pos of expr_detail
  | Switch of expr_detail * ((expr_detail * s_stmt)	list)
    
    
let string_of_sast_type (t : types) =
  match t with
      Bus -> "Bus"
    | Array -> "Array"
    | Const -> "Const"
    | Function -> "Function"  
    | Void -> "Void"


(* Find variable in scope *)
let rec find_variable (scope : symbol_table) name =
	try
		List.find ( 
    				fun ( v , _, _, _ ) -> v.name = name 
    				) scope.variables
	with Not_found ->
		match scope.parent with
			  Some(parent) -> find_variable parent name
			| _ -> raise (Error("Variable " ^ name ^ " is undeclared"))


(* Add local to Symbol Table *)
let check_and_add_local (vbus, x, t, lt) (env : translation_environment) =
  let var = (vbus, x, t, lt) in
(* Un-comment to print the list of locals name *)
  (*let _ = print_endline vbus.name in*)
  if List.exists (fun (varbus, _, _, _) -> varbus.name = vbus.name) env.scope.variables
  then raise (Error("Multiple declarations for " ^ vbus.name))
  else let new_scope = { parent = env.scope.parent;
		         variables = var :: env.scope.variables; }
  in let new_env = { scope = new_scope;}
       in  new_env


(* Check type compatibility of e1 and e2 for the given op *)
(* Raise error if incompatible else return unit *)
let check_types e1 op e2 = ()
let check_basn vbus e1 = () 
let check_aasn vbus e1 e2 = ()
let check_call actuals env func_decl = ()
let check_subbus vbus x y = ()
let check_array_dereference  varray size e1 = ()
let check_conditional e1 t1 = ()
let check_pos_expr e1 = ()
let check_switchable e1 t1 = ()
let check_function_params fd expr_detail_list = ()


(*Check expressions *)
let rec chk_expr env = function
(* An integer constant: convert and return Int type *)
	Ast.Num(v) -> Num(v), Const
(* An identifier: verify it is in scope and return its type *)
  | Ast.Id(vname) ->
    	let vbus, _, _, _ = 
       		try
    			find_variable env.scope vname (* locate a variable by name *)
    		with Not_found ->
    			raise (Error("undeclared identifier " ^ vname))
    	in Id(vbus.name), Bus
  | Ast.Binop(e1, op, e2) ->
    	let e1 = chk_expr env e1 (* Check left and right children *)
		and e2 = chk_expr env e2 in
    	check_types e1 op e2;
		Binop(fst e1, op, fst e2), Bus (* Success: result is bus *)
  | Ast.Basn(vname, e1) ->
		let e1 = chk_expr env e1
  and vbus, _, _, _ = find_variable env.scope vname
    	in
    	check_basn vbus e1;
    	Basn(vbus, fst e1), Bus
  | Ast.Aasn(vname, e1, e2) ->
		let e1 = chk_expr env e1
  		and e2 = chk_expr env e2
  		and vbus, size, _, _ = find_variable env.scope vname
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
             let e1 = chk_expr env actual
             in ignore(e1::expr_detail_list); env) env expr_list (*WRONG!!!!*)
            in let expr_dlist = List.rev expr_detail_list
               in let _	= check_function_params func_decl expr_dlist
            in Call(func_decl, List.rev expr_detail_list), Function
  | Ast.Unop(op, e1) ->
    	let (e1, t1) = chk_expr env e1
     in Unop(op, e1), t1
  | Ast.Subbus(vname, x, y) ->
    	let vbus, _, _, _ = find_variable env.scope vname
     	in check_subbus vbus x y;
    	Subbus(vbus, x, y), Bus
  | Ast.Barray(vname, e1) ->
    	let (e1, t1) = chk_expr env e1
     and varray, size, _, _ = find_variable env.scope vname
     in check_array_dereference varray size e1;
    Barray(varray, size, e1), Array
  | Ast.Noexpr -> Noexpr, Void


(*Check Statements*)
let rec chk_stmt env = function
    Ast.Expr(e) -> Expr(chk_expr env e)
  | Ast.If(e1, s1, s2) ->
    	let e1, t1 = chk_expr env e1
     in check_conditional e1 t1;
    If(e1, chk_stmt env s1, chk_stmt env s2)
  | Ast.For(e1, e2, e3, s1) ->
    	let e1, t1 = chk_expr env e1
     	and e2, t2 = chk_expr env e2
     	and e3, t3 = chk_expr env e3
      in check_conditional e1 t1;
    For(e1, e2, e3, chk_stmt env s1)
  | Ast.While(e1, s1) ->
    	let e1, t1 = chk_expr env e1
     	in check_conditional e1 t1;
    	While(e1, chk_stmt env s1)
  | Ast.Pos(e1) ->
    	let e1, t1 = chk_expr env e1
     	in check_pos_expr e1;
    	Pos(e1)
  | Ast.Block(slist) ->
    	(* New scopes: parent is the existing scope, start out empty *)
		let new_scope = { parent = Some(env.scope); variables = [] }
	    in
        (* New environment: same, but with new symbol tables *)
        let new_env = { scope = new_scope}
        in let run_chk_stmt (env : translation_environment) (actual : Ast.stmt) =
	  let s1 = chk_stmt env actual
	    in s1
	in let new_stmt_list = 
	let rec stmt_helper l = function
	    [] -> List.rev l
	  | hd::tl -> let new_l = ( run_chk_stmt new_env hd )::l
		in stmt_helper new_l tl
	in stmt_helper [] slist
(* Uncomment to check if Blocks are parsed *)
	in let _ = print_endline "parsed a Block"
	in Block(new_scope,new_stmt_list)
  | Ast.Switch(e, caselist) ->
    	let e, t1 = chk_expr env e
     	in let _ = check_switchable e t1
	in let chk_case_list (env : translation_environment) ( (e1, s1) : (Ast.expr * Ast.stmt) ) =
            let e1, _ = chk_expr env e1
            in let s1 = chk_stmt env s1
	    in (e1, s1)
	in let rec clist_helper l = function
	   [] -> List.rev l
	 | hd::tl -> let new_l = (chk_case_list env hd) :: l
			in clist_helper new_l tl
	in let clist = clist_helper [] caselist	
(* Uncomment to check if Switch is parsed *)
	in let _ = print_endline "parsed a Switch"
	in Switch(e, clist)

(* Function translation Ast -> Sast. Build Symbol table; parse statements*)
let check_func (env : translation_environment) (portin : (Ast.bus list)) (portout : (Ast.bus list)) (body : Ast.fbody) =
  let pin_env = List.fold_left (
    fun (pin_env : translation_environment) (actual : Ast.bus) ->
      check_and_add_local (actual, 0, Bus, In_port) pin_env
				) env portin
  in
  let pout_env = List.fold_left (
    fun (pout_env : translation_environment) (actual : Ast.bus) ->
      check_and_add_local (actual, 0, Bus, Out_port) pout_env
				) pin_env portout
  in
  let (locals_list, stmts) = body
  in let full_env = List.fold_left (
    fun (env : translation_environment) (actual : Ast.locals) ->
      match actual with
      	  Bdecl(vbus) -> check_and_add_local (vbus, 0, Bus, Int_signal) env
        | Adecl(vbus, size) -> check_and_add_local (vbus, size, Array, Int_signal) env
                            ) pout_env locals_list	
	in let run_chk_stmt (env : translation_environment) (actual : Ast.stmt) =
		let s1 = chk_stmt env actual
		in s1	
	in let new_stmt_list = 
	let rec stmt_helper l = function
		  [] -> List.rev l
		| hd::tl -> let new_l = ( run_chk_stmt full_env hd )::l
			in stmt_helper new_l tl
	  in stmt_helper [] stmts
	in (full_env, new_stmt_list)
  

(* Function table *)
let func (env : translation_environment) (astfn : Ast.fdecl) =
  let func_scope = { parent = Some(env.scope); variables = [] }
  in let func_env = {scope = func_scope }
    in let (chk_floc, chk_fbod) = check_func func_env astfn.portin astfn.portout astfn.body
     in let fobj = {	pout = astfn.portout;
		    	fid  = astfn.fname;
			pin  = astfn.portin;
			floc = chk_floc;
			fbod = chk_fbod; }           
        in let function_table = StringMap.add astfn.fname fobj function_table
	  in function_table


(* Program transaltion Ast -> Sast *)
let prog ((constlist : Ast.gdecl list), (funclist : Ast.fdecl list)) = 
  let clist = List.map (
    fun (gdecl : Ast.gdecl)-> 
      let Ast.Const(vbus, value) = gdecl
        in
      let _ = check_basn vbus value
      in (vbus, value, Const, Int_signal)
                          ) constlist
(* Un-comment to print the list of constants name *)
(*in let name_list = List.map (fun (sgnl,_,_,_) -> sgnl.name) clist
in let _ = List.iter print_endline name_list*)
     in let global_scope = { parent = None; variables = List.rev clist}
        in let global_env = { scope = global_scope }

	   in let rec create_map mymap = function
					  [] -> mymap 
  					| hd::tl -> let mymap = func global_env (hd : Ast.fdecl)
							in create_map mymap tl
	   in let ftable = create_map function_table funclist
		in global_env, ftable


