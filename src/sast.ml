open Ast

module StringMap = Map.Make(String);;

let function_table = StringMap.empty

exception Error of string  
  
  
type types = 
    Bus 
  | Array 
  | Const
  | Function
  | Void

(* Covers both buses and array, out of bounds exceptions should done at run time *)
type symbol_table = {
  parent : symbol_table option;
  variables : (Ast.bus * int * types) list
                    }

type translation_environment = {
	scope : symbol_table; (* symbol table for vars *)
}

type function_decl = {	pout : Ast.bus list;
			fid  : string;
			pin  : Ast.bus list;
			floc : translation_environment;
			fbod : stmt list;
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
  
and stmt = 
    Block of symbol_table * (stmt list)
  | Expr of expression
  | If of expr_detail * stmt * stmt
  | For of expr_detail * expr_detail * expr_detail * stmt 
  | While of expr_detail * stmt
  | Pos of expr_detail
  | Switch of expr_detail * ((expr_detail * stmt)	list)
    
    
let string_of_sast_type (t : types) =
  match t with
      Bus -> "Bus"
    | Array -> "Array"
    | Const -> "Const"
    | Function -> "Function"  
    | Void -> "Void"
    
let rec find_variable (scope : symbol_table) name =
	try
		List.find ( 
    				fun ( v , _, _ ) -> v.name = name 
    				) scope.variables
	with Not_found ->
		match scope.parent with
			  Some(parent) -> find_variable parent name
			| _ -> raise Not_found

let check_and_add_local (vbus, x, t) (env : translation_environment) =
  let var = (vbus, x, t) in
  if List.exists (fun (varbus, _, _) -> varbus.name = vbus.name) env.scope.variables
  then raise (Error("Multiple declarations for " ^ vbus.name))
  else let _ = var :: env.scope.variables
       in env

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
  
let rec expr env = function
(* An integer constant: convert and return Int type *)
	Ast.Num(v) -> Num(v), Const
(* An identifier: verify it is in scope and return its type *)
  | Ast.Id(vname) ->
    	let vbus, _, _ = 
       		try
    			find_variable env.scope vname (* locate a variable by name *)
    		with Not_found ->
    			raise (Error("undeclared identifier " ^ vname))
    	in Id(vbus.name), Bus
  | Ast.Binop(e1, op, e2) ->
    	let e1 = expr env e1 (* Check left and right children *)
		and e2 = expr env e2 in
    	check_types e1 op e2;
		Binop(fst e1, op, fst e2), Bus (* Success: result is bus *)
  | Ast.Basn(vname, e1) ->
		let e1 = expr env e1
  and vbus, _, _ = find_variable env.scope vname
    	in
    	check_basn vbus e1;
    	Basn(vbus, fst e1), Bus
  | Ast.Aasn(vname, e1, e2) ->
		let e1 = expr env e1
  		and e2 = expr env e2
  		and vbus, size, _ = find_variable env.scope vname
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
            in let expr_dlist = List.rev expr_detail_list
               in let _	= check_function_params func_decl expr_dlist
            in Call(func_decl, List.rev expr_detail_list), Function
  | Ast.Unop(op, e1) ->
    	let (e1, t1) = expr env e1
     in Unop(op, e1), t1
  | Ast.Subbus(vname, x, y) ->
    	let vbus, _, _ = find_variable env.scope vname
     	in check_subbus vbus x y;
    	Subbus(vbus, x, y), Bus
  | Ast.Barray(vname, e1) ->
    	let (e1, t1) = expr env e1
     and varray, size, _ = find_variable env.scope vname
     in check_array_dereference varray size e1;
    Barray(varray, size, e1), Array
  | Ast.Noexpr -> Noexpr, Void
  	
let rec chk_stmt env = function
    Ast.Expr(e) -> Expr(expr env e)
  | Ast.If(e1, s1, s2) ->
    	let e1, t1 = expr env e1
     in check_conditional e1 t1;
    If(e1, chk_stmt env s1, chk_stmt env s2)
  | Ast.For(e1, e2, e3, s1) ->
    	let e1, t1 = expr env e1
     	and e2, t2 = expr env e2
     	and e3, t3 = expr env e3
      in check_conditional e1 t1;
    For(e1, e2, e3, chk_stmt env s1)
  | Ast.While(e1, s1) ->
    	let e1, t1 = expr env e1
     	in check_conditional e1 t1;
    	While(e1, chk_stmt env s1)
  | Ast.Pos(e1) ->
    	let e1, t1 = expr env e1
     	in check_pos_expr e1;
    	Pos(e1)
  | Ast.Block(slist) ->
    	(* New scopes: parent is the existing scope, start out empty *)
		let new_scope = { parent = Some(env.scope); variables = [] }
	    in
        (* New environment: same, but with new symbol tables *)
        let new_env = { scope = new_scope}
        in  
     	let stmt_list = []
         in let _ = List.fold_left (
           fun (env : translation_environment) (actual : Ast.stmt) ->  
             let s1 = chk_stmt env actual
             in ignore(s1::stmt_list); new_env) new_env slist
            in Block(new_scope, List.rev stmt_list)
  | Ast.Switch(e, caselist) ->
    	let e, t1 = expr env e
     	in check_switchable e t1;
        let clist = []
        in let _ = List.fold_left (
          fun (env : translation_environment) ( (e1, s1) : (Ast.expr * Ast.stmt) ) ->
            let e1, _ = expr env e1 in
            let s1 = chk_stmt env s1 in
            let _ = (e1, s1)::clist in
            env) env clist
           in Switch(e, List.rev clist)


let check_func (env : translation_environment) (body : Ast.fbody) =
  let (locals_list, stmts) = body
  in let env = List.fold_left (
    fun (env : translation_environment) (actual : Ast.locals) ->
      match actual with
      	  Bdecl(vbus) -> check_and_add_local (vbus, 0, Bus) env
        | Adecl(vbus, size) -> check_and_add_local (vbus, size, Array) env
                            ) env locals_list
     in let stmt_list = []
        in let _ = List.fold_left (
       		fun (env : translation_environment) (actual : Ast.stmt) ->
         		let _ = (chk_stmt env actual) :: stmt_list
           			in env
                                    ) env stmt_list
           in List.rev stmt_list


  
  
let func (env : translation_environment) (astfn : Ast.fdecl) =
  let func_scope = { parent = Some(env.scope); variables = [] }
  in let func_env = {scope = func_scope }
     in let fobj = {	pout = (astfn.portout);
		    	fid  = (astfn.fname);
			pin  = (astfn.portin);
			floc = func_env;
			fbod = check_func func_env (astfn.body); }           
        in function_table = StringMap.add astfn.fname fobj function_table
  
let prog ((constlist : Ast.gdecl list), funclist) = 
  let clist = List.map (
    fun (gdecl : Ast.gdecl)-> 
      let Ast.Const(vbus, value) = gdecl
        in
      let _ = check_basn vbus value
      in (vbus, value, Const)
                          ) constlist
     in let global_scope = { parent = None; variables = List.rev clist}
        in let global_env = { scope = global_scope }
	   in let _ = List.map (
	     fun (astfn : Ast.fdecl) -> (func global_env astfn)
				   ) funclist
	       in global_env, function_table
           (*in let flist = List.map (
             fun (astfn : Ast.fdecl) -> (func global_env astfn)
                                   ) funclist
              in global_env, flist*)
