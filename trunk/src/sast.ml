open Ast

module StringMap = Map.Make(String);;

(*Auxiliary functions*)
(*USE THIS FUNCTION FOR TYPE CHECKING WHEN NEEDED!*)
let bit_required x = 
let log2 y = int_of_float ( ((log (float_of_int y)) /. (log 2.)) )
in ((log2 x) + 1)

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
  variables : (Ast.bus * int * types * local_t * bool) list
                    }

type translation_environment = {
	scope : symbol_table; (* symbol table for vars *)
}

type function_decl = {	pout : Ast.bus list;
	 fid  : string;
	 pin  : Ast.bus list;
	 floc : translation_environment;
     	 fcalls : function_decl list; (* list of other functions that are called by this function *)  
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
| Subasn of Ast.bus * int * int * expr_detail
| Noexpr

(* expression * type retuned * size *)
(* To return the size of expr is redundant, but helpful for type checking!!*)
(* The field returns the size of the bus, even with arr ays, because
   the size of the array is already stored in the symbol_table *)
and expression = expr_detail * types * int 

and s_stmt = 
    Block of s_stmt list
  | Expr of expression
  | If of expr_detail * s_stmt * s_stmt
  | For of expr_detail * expr_detail * expr_detail * s_stmt 
  | While of expr_detail * s_stmt
  | Pos of expr_detail
  | Switch of expr_detail * ((expr_detail * s_stmt)	list)
  | Call of function_decl * (expr_detail list) * (expr_detail list) 
    
    
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
    				fun ( v , _, _, _, _ ) -> v.name = name 
    				) scope.variables
	with Not_found ->
		match scope.parent with
			  Some(parent) -> find_variable parent name
			| _ -> raise (Error("Variable " ^ name ^ " is undeclared"))


(* Add local to Symbol Table *)
let check_and_add_local (vbus, x, t, lt, dr) (env : translation_environment) =
  let var = (vbus, x, t, lt, dr) in
(* Un-comment to print the list of locals name *)
  (*let _ = print_endline vbus.name in*)
  if List.exists (fun (varbus, _, _, _, _) -> varbus.name = vbus.name) env.scope.variables
  then raise (Error("Multiple declarations for " ^ vbus.name))
  else let new_scope = { parent = env.scope.parent;
		         variables = var :: env.scope.variables; }
  in let new_env = { scope = new_scope;}
       in  new_env


(* Check type compatibility of e1 and e2 for the given op *)
(* Raise error if incompatible else return unit *)
(*!!! WHILE WRITING THESE FUNCTIONS, CHECK THE LAST FIELD OF THE VARIABLES:
      IF TRUE RAISE "Variable <vname> has more than one driver"       !!!*)
let check_types e1 op e2 = 32 (*Must return also the size of the output!!!*)
let check_basn vbus e1 = ()
let check_subasn vbus x y e1 = ()
let check_aasn vbus e1 e2 = ()
let check_call env out_actuals in_actuals func_decl = ()
let check_subbus vbus x y = ()
let check_array_dereference  varray size e1 s1 = ()
let check_conditional e1 t1 = ()
let check_pos_expr e1 = ()
let check_switchable e1 t1 = ()
let check_function_params fd expr_detail_list = ()


(*Check expressions *)
let rec chk_expr function_table env = function
(* An integer constant: convert and return Int type *)
	Ast.Num(v) ->
	let min_size = bit_required v in
	Num(v), Const, min_size (*If assigned to the bus vbus, check that vbus.size >= min_size!!*)
(* An identifier: verify it is in scope and return its type *)
  | Ast.Id(vname) ->
    	let vbus, _, vtype, _, _ = 
       		try
    			find_variable env.scope vname (* locate a variable by name *)
    		with Not_found ->
    			raise (Error("undeclared identifier " ^ vname))
    	in Id(vbus.name), vtype, vbus.size (*Be careful!!! An Id could be a constant, a bus or an array!*)
  | Ast.Binop(e1, op, e2) ->
    	let e1 = chk_expr function_table env e1 (* Check left and right children *)
		and e2 = chk_expr function_table env e2
    	in let output_size = check_types e1 op e2 in
	let (e1,_,_) = e1 and (e2,_,_) = e2
	in Binop(e1, op, e2), Bus, output_size (* Success: result is bus *)
  | Ast.Basn(vname, e1) ->
		let e1 = chk_expr function_table env e1
  and vbus, _, _, _, _ = find_variable env.scope vname
    	in let _ = check_basn vbus e1
	in let (e1, _, _) = e1
	in Basn(vbus, e1), Bus, vbus.size
  | Ast.Subasn(vname, x, y, e1) ->
	let e1 = chk_expr function_table env e1
  and vbus, _, _, _, _ = find_variable env.scope vname
	in let _ = check_subasn vbus x y e1
	in let (e1, _, _) = e1
	in Subasn(vbus, x, y, e1), Bus, (abs(x-y) +1);
  | Ast.Aasn(vname, e1, e2) ->
		let e1 = chk_expr function_table env e1
  		and e2 = chk_expr function_table env e2
  		and vbus, size, _, _, _ = find_variable env.scope vname
    	in let _ = check_aasn vbus e1 e2
	in let (e1, _, _) = e1 and (e2, _, _) = e2
    	in Aasn(vbus, size, e1, e2), Bus, vbus.size
  (* NEED TO CHECK OUTPUT PORTS MATCH WITH LOCALS ASSIGNMENT!!!*)
  | Ast.Unop(op, e1) ->
    	let (e1, t1, s1)= chk_expr function_table env e1
     in Unop(op, e1), t1, s1
  | Ast.Subbus(vname, x, y) ->
    	let vbus, _, _, _, _ = find_variable env.scope vname
     	in check_subbus vbus x y;
    	Subbus(vbus, x, y), Bus, (abs(x-y) +1)
  | Ast.Barray(vname, e1) ->
    	let (e1, t1, s1) = chk_expr function_table env e1
     and varray, size, vtype, _, _ = find_variable env.scope vname
     in check_array_dereference varray size e1 s1;
    Barray(varray, size, e1), vtype, varray.size (*Be careful!!! A reference to array a[i] returns always a varray type!*)
  | Ast.Noexpr -> Noexpr, Void, 0


(*Check Statements*)
let rec chk_stmt function_table env = function
    Ast.Expr(e) -> Expr(chk_expr function_table env e)
  | Ast.If(e1, s1, s2) ->
    	let e1, t1, _ = chk_expr function_table env e1
     in check_conditional e1 t1;
    If(e1, chk_stmt function_table env s1, chk_stmt function_table env s2)
  | Ast.For(e1, e2, e3, s1) ->
    	let e1, t1, _= chk_expr function_table env e1
     	and e2, t2, _= chk_expr function_table env e2
     	and e3, t3, _= chk_expr function_table env e3
      in check_conditional e1 t1;
    For(e1, e2, e3, chk_stmt function_table env s1)
  | Ast.While(e1, s1) ->
    	let e1, t1, _= chk_expr function_table env e1
     	in check_conditional e1 t1;
    	While(e1, chk_stmt function_table env s1)
  | Ast.Pos(e1) ->
    	let e1, t1, _= chk_expr function_table env e1
     	in check_pos_expr e1;
    	Pos(e1)
  | Ast.Block(slist) ->
    	(*(* New scopes: parent is the existing scope, start out empty *)
		let new_scope = { parent = Some(env.scope); variables = [] }
	    in
        (* New environment: same, but with new symbol tables *)
        let new_env = { scope = new_scope}
        in *)
	let run_chk_stmt (env : translation_environment) (actual : Ast.stmt) =
	  let s1 = chk_stmt function_table env actual
	    in s1
	in let new_stmt_list = 
	let rec stmt_helper l = function
	    [] -> List.rev l
	  | hd::tl -> let new_l = ( run_chk_stmt env hd )::l
		in stmt_helper new_l tl
	in stmt_helper [] slist
(* Un-comment to check if Blocks are parsed *)
	(*in let _ = print_endline "parsed a Block"*)
	in Block(new_stmt_list)
  | Ast.Switch(e, caselist) ->
    	let e, t1, _ = chk_expr function_table env e
     	in let _ = check_switchable e t1
	in let chk_case_list (env : translation_environment) ( (e1, s1) : (Ast.expr * Ast.stmt) ) =
            let e1, _, _ = chk_expr function_table env e1
            in let s1 = chk_stmt function_table env s1
	    in (e1, s1)
	in let rec clist_helper l = function
	   [] -> List.rev l
	 | hd::tl -> let new_l = (chk_case_list env hd) :: l
			in clist_helper new_l tl
	in let clist = clist_helper [] caselist	
(* Un-comment to check if Switch is parsed *)
	(*in let _ = print_endline "parsed a Switch"*)
	in Switch(e, clist)
	
   | Ast.Call(fname, out_list, in_list ) ->	
    	let func_decl = 
       		try StringMap.find fname function_table
         with Not_found -> raise (Failure ("undefined function " ^ fname))
    in let _ = check_call env out_list in_list func_decl     
    in let inlist = List.fold_left 
    ( fun l x -> let e1, _, _ =  chk_expr function_table env x in e1::l ) [] in_list 
    in let outlist = List.fold_left 
    ( fun l x -> let e1, _, _ =  chk_expr function_table env x in e1::l ) [] out_list
(* Un-comment to check if Function Call is parsed *)
	(*in let _ = print_endline "Function Call parsed"*)
	in Call(func_decl, outlist, inlist)


(* Function translation Ast -> Sast. Build Symbol table; parse statements*)
let check_func (env : translation_environment) (portin : (Ast.bus list)) (portout : (Ast.bus list)) (body : Ast.fbody) function_table =
  let pin_env = List.fold_left (
    fun (pin_env : translation_environment) (actual : Ast.bus) ->
      check_and_add_local (actual, 0, Bus, In_port, true) pin_env
				) env portin
  in
  let pout_env = List.fold_left (
    fun (pout_env : translation_environment) (actual : Ast.bus) ->
      check_and_add_local (actual, 0, Bus, Out_port, false) pout_env
				) pin_env portout
  in
  let (locals_list, stmts) = body
  in let full_env = List.fold_left (
    fun (env : translation_environment) (actual : Ast.locals) ->
      match actual with
      	  Bdecl(vbus) -> check_and_add_local (vbus, 0, Bus, Int_signal, false) env
        | Adecl(vbus, size) -> check_and_add_local (vbus, size, Array, Int_signal, false) env
                            ) pout_env locals_list	
	in let run_chk_stmt (env : translation_environment) (stmt_lst, call_lst) (actual : Ast.stmt) =
	let s1 = chk_stmt function_table env actual
	in let call_lst = (match actual with 
	        Ast.Call(fname, _, _ ) -> let f_decl = 
       		               try StringMap.find fname function_table with Not_found -> raise (Failure ("undefined function " ^ fname))
	                       in f_decl::call_lst 
	       | x -> call_lst (* do nothing *) ) 
	    in (s1::stmt_lst,call_lst)
	
	in let (new_stmt_list,call_lst) = 
	List.fold_left (run_chk_stmt full_env) ([],[]) stmts  
	
	(*
	let rec stmt_helper l cl = function
		  [] -> List.rev l
		| hd::tl -> let new_l = ( run_chk_stmt full_env cl hd )::l
			in stmt_helper new_l tl
	  in stmt_helper [] [] stmts *)
	in (full_env, call_lst, new_stmt_list)
  

(* Function table *)
let func (env : translation_environment) (astfn : Ast.fdecl) tmp_ftable =
  let func_scope = { parent = Some(env.scope); variables = [] }
  in let func_env = {scope = func_scope }
    in let (chk_floc, chk_calls, chk_fbod) = check_func func_env astfn.portin astfn.portout astfn.body tmp_ftable
     in let fobj = {	pout = astfn.portout;
		    	fid  = astfn.fname;
			    pin  = astfn.portin;
			    floc  = chk_floc;
			    fcalls = chk_calls; 
			    fbod = chk_fbod; }           
        in let new_ftable = StringMap.add astfn.fname fobj tmp_ftable
(* Un-comment to check if functions are added to the Function Table*)
	in let _ = print_endline ("Added "^astfn.fname)
	  in new_ftable


(* Program transaltion Ast -> Sast *)
let prog ((constlist : Ast.gdecl list), (funclist : Ast.fdecl list)) = 
  let clist = List.map (
    fun (gdecl : Ast.gdecl)-> 
      let Ast.Const(vbus, value) = gdecl
        in
      let _ = check_basn vbus value
      in (vbus, value, Const, Int_signal, true)
                          ) (List.rev constlist)
(* Un-comment to print the list of constants name *)
(*in let name_list = List.map (fun (sgnl,_,_,_) -> sgnl.name) clist
in let _ = List.iter print_endline name_list*)
     in let global_scope = { parent = None; variables = List.rev clist}
        in let global_env = { scope = global_scope }

	   in let rec create_map mymap = function
					  [] -> mymap 
  					| hd::tl -> let new_mymap = func global_env (hd : Ast.fdecl) mymap
							in create_map new_mymap tl
	   in let ftable = create_map StringMap.empty (List.rev funclist)
		in global_env, ftable


