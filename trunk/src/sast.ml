open Ast

module StringMap = Map.Make(String);;

(*Auxiliary functions*)
(*USE THIS FUNCTION FOR TYPE CHECKING WHEN NEEDED!*)
let bit_required x = 
let s = if x < 0 then 1 else 0
in let x = abs x
in let log2 y = int_of_float ( ((log (float_of_int y)) /. (log 2.)) )
in let res = ((log2 x) + 1 + s)
in print_endline (string_of_int res); res

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
  variables : (Ast.bus * int * types * local_t * bool) list;
  isIf : bool array;
  isWhile : bool array
                    }

type translation_environment = {
	scope : symbol_table; (* symbol table for vars *)
}

type function_decl = {
  	 pout : Ast.bus list;
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
  let _ = print_endline ("Adding local " ^ vbus.name ^ " " ^ string_of_int x ^ " " ^ string_of_bool dr) in
  if bit_required vbus.init > vbus.size then raise (Error("Initial value does not fit bus size: "^vbus.name)) else
  if dr then ( if (x = 0) 
  				then for i = 0 to vbus.size-1 do vbus.isAssigned.(i) <- true done
      		   else for i = 0 to x-1 do vbus.isAssigned.(i) <- true done)
  else if (x = 0)
  		then (for i = 0 to vbus.size-1 do vbus.isAssigned.(i) <- false;  done)
   else for i = 0 to x-1 do vbus.isAssigned.(i) <- false done;
     
  let var = (vbus, x, t, lt, dr) in
(* Un-comment to print the list of locals name *)
  (*let _ = print_endline vbus.name in*)
  if List.exists (fun (varbus, _, _, _, _) -> varbus.name = vbus.name) env.scope.variables
  then raise (Error("Multiple declarations for " ^ vbus.name))
  else let new_scope = { parent = env.scope.parent;
		         variables = var :: env.scope.variables; isIf = env.scope.isIf; isWhile = env.scope.isWhile}
  in let new_env = { scope = new_scope;}
       in  new_env


let check_operand_type type_1  =
  match type_1 with
    Bus | Array| Const -> true
               | _ -> raise(Error("Operand types should be bus or array or const"))
                 

let check_types e1 op e2 =
  let (detail_1, type_1, size_1) = e1
  in let (detail_2, type_2, size_2) = e2
     in let _ = check_operand_type type_1
        in let _ = check_operand_type type_2
          in
  			match op with
    			Or | And | Xor -> if size_1 != size_2 then raise(Error("Operand size mismatch in logical operation "))
       								else size_1
                | Mul                               -> size_1 + size_2
                | Lt | Gt | Lte | Gte | Eq | Neq    -> 1
                | Shl | Shr                         -> if type_1 = Const then raise(Error("Bit Shift operators cant be used on Constants"))
                                           		else size_1
                | Add|Sub	                    -> Pervasives.max size_1 size_2
                | _                                 -> raise(Error("Unary operators passed to binop"))

let check_switchable e1 t1 = 
  check_operand_type t1

 
let check_array_dereference varray size e1 t1 s1 = 
  match e1 with
  | Noexpr -> raise(Error("Array index undefined: "^varray.name))
  | Num(v) -> if v > (size - 1) then raise(Error("Array index out of bound "^varray.name)) else()
  | _ -> if s1 > bit_required(size)
    		then print_endline ("Warning: possible array index out of bound: "^varray.name) else()
  
let check_basn env vbus e1 =
 let _ = print_endline ("Checking variable "^vbus.name)
  in let (detail, t, size) = e1
   in match t with
     Bus -> if size = vbus.size
             then for i = 0 to vbus.size-1 do 			(* print_endline ("Checking bit " ^ string_of_int i); *)
       								if (vbus.isAssigned.(i) && not(env.scope.isWhile.(0)))
     								then raise (Error("Variable "^vbus.name^" has more than one driver"))
     								else vbus.isAssigned.(i) <- true done
     
    	    else raise (Error("Bus size mismatch for "^vbus.name))
   | Const -> for i = 0 to vbus.size-1 do if (vbus.isAssigned.(i) && not(env.scope.isWhile.(0)))
     					  then raise (Error("Variable "^vbus.name^" has more than one driver"))
     					  else vbus.isAssigned.(i) <- true done;
         
         			(match detail with
         				  Num(v) -> if (bit_required v) > vbus.size then raise(Error("size mismatch "^vbus.name))
             					    else()
					| Id(s) -> let b,_,_,_,_ = find_variable env.scope s in
						     if b.size != vbus.size then raise(Error("size mismatch "^vbus.name))
						     else()
					| Subbus(b,strt,stop) -> if (abs (strt - stop)) != ((vbus.size)-1)
								 then raise (Error("Size mismatch "^vbus.name))
								 else ()
                    			| _ -> raise (Error("Illegal bus assignment: "^vbus.name))   )
   | Array -> for i = 0 to vbus.size-1 do if (vbus.isAssigned.(i) && not(env.scope.isWhile.(0)))
     					  then raise (Error("Variable "^vbus.name^" has more than one driver"))
     					  else vbus.isAssigned.(i) <- true done;
				(match detail with
					Barray(b,_,_) -> if b.size != vbus.size then raise(Error("size mismatch "^vbus.name))
							else()
					| _ ->	raise (Error("Expected variable of type bus or const "^vbus.name)) )
   | _ -> raise (Error("Expected variable of type bus or const "^vbus.name))



let check_aasn env vbus size e1 e2 = 
  let(detail_e1,t_e1, size_e1) = e1
  in match t_e1 with
      Const -> let (ed2,t_e2, size_e2) = e2
       			in (match t_e2 with
				  Const -> (match ed2 with
						  Num(v) -> if size_e2 > vbus.size
       				 			then raise(Error("Bus size mismatch for "^vbus.name)) else ()	
						| Unop(uop,ued) -> (match uop with
									  Umin -> (match ued with
											  Num(v) -> if size_e2 > vbus.size
					       				 			then raise(Error("Bus size mismatch for "^vbus.name)) else ()	
											| _ -> if size_e2 != vbus.size
       				 								then raise(Error("Bus size mismatch for "^vbus.name)) else ()	)
									| _ -> 	if size_e2 != vbus.size
       				 						then raise(Error("Bus size mismatch for "^vbus.name)) else ()	)
						| _ -> if size_e2 != vbus.size
       				 			then raise(Error("Bus size mismatch for "^vbus.name)) else ()	)
				| _ ->	if size_e2 != vbus.size
       				 	then raise(Error("Bus size mismatch for "^vbus.name)) else ()	);		  
    			(match detail_e1 with
         
         			Num(v) -> if v > (size-1) then raise(Error("Array index out of bound "^vbus.name)) 
            					else() ;
         					  if (vbus.isAssigned.(v) && not(env.scope.isWhile.(0)))
            					then raise (Error("Array index has more than one driver "^vbus.name))
          					  else vbus.isAssigned.(v) <- true
             			| Id(s) -> let b,_,_,_,_ = find_variable env.scope s in  
					     if b.init > (size-1) then raise(Error("Array index out of bound "^vbus.name)) 
            					else if (vbus.isAssigned.(b.init) && not(env.scope.isWhile.(0)))
            					then raise (Error("Array index has more than one driver "^vbus.name))
          					  else vbus.isAssigned.(b.init) <- true
				| Subbus(b,x,y) -> let rec addone p = (function
							  0 -> p + 1
							| n -> let newp = p + (int_of_float ( 2. ** (float_of_int n) ))
								in addone newp (n-1)	)
						   in let subn number strt stop = let r = abs (strt-stop)
							in let ones = addone 0 r in (number land ones)
						   in let init = subn b.init x y
						   in if init > (size-1) then raise(Error("Array index out of bound "^vbus.name)) 
            						else if (vbus.isAssigned.(init) && not(env.scope.isWhile.(0)))
            						then raise (Error("Array index has more than one driver "^vbus.name))
          					  	else vbus.isAssigned.(init) <- true
				| _ -> raise (Error("Array index is an expression of constants: "^vbus.name))  )
      | Bus -> if(size_e1 > bit_required size)
               	then print_endline ("Warning: possible array index out of bound: "^vbus.name) else();
               for i = 0 to size-1 do if (vbus.isAssigned.(i) && not(env.scope.isWhile.(0)))
      										then raise (Error("Array index has more than one driver "^vbus.name))
                					else vbus.isAssigned.(i) <- true done ; 
        		let (ed2,t_e2,size_e2) = e2
			in (match t_e2 with
				  Const -> (match ed2 with
						  Num(v) -> if size_e2 > vbus.size
       				 			then raise(Error("Bus size mismatch for "^vbus.name)) else ()
						| _ -> if size_e2 != vbus.size
       				 			then raise(Error("Bus size mismatch for "^vbus.name)) else ()	)
				| _ ->	if size_e2 != vbus.size
       				 	then raise(Error("Bus size mismatch for "^vbus.name)) else ()	)                  			
      | _ -> raise (Error("Array index should be const or bus "^vbus.name)) 
        
 

let check_subbus vbus x y =
  let (x,y) = if x < y then (x,y) else (y,x)
  in if x >= 0 && y <= vbus.size && x <= y then ()
  else raise (Error("Incorrect subbus dereference for "^vbus.name))

let check_subasn env vbus x y e1 = 
  let (x,y) = if x < y then (x,y) else (y,x)
  in let (detail, t, size) = e1
  in match t with 
    Bus -> let _ = check_subbus vbus x y
           in if size = y-x+1
           then for i = x to y do if (vbus.isAssigned.(i) && not(env.scope.isWhile.(0)))
           						then raise (Error("Variable "^vbus.name^" has more than one driver"))
                 				else vbus.isAssigned.(i) <- true done
           else raise (Error("Size of expression is different from subbus width for "^vbus.name))
    | Const -> (let _ = check_subbus vbus x y
                in let _ = match detail with
                 			Num(v) -> if(bit_required v) > y-x+1
                 				  then raise (Error("Size of expression is bigger than subbus width for "^vbus.name))
                     		  		  else ()
					| Id(s) ->  let b,_,_,_,_ = find_variable env.scope s in
							if b.size != y-x+1
							then raise (Error("Size of expression is different from subbus width for "^vbus.name))
							else ()
					| Subbus(_,strt,stop) -> if (abs (strt - stop)) != y-x
								 then raise (Error("Size of expression is different from subbus width for "^vbus.name))
								 else ()
	                    		| _ -> raise (Error("Const expressions are not supported"))
                      in for i = x to y do if (vbus.isAssigned.(i) && not(env.scope.isWhile.(0)))
           				   then raise (Error("Variable "^vbus.name^" has more than one driver"))
                 			   else vbus.isAssigned.(i) <- true done)
      | Array -> (let _ = check_subbus vbus x y
           	in if size = y-x+1
           	then for i = x to y do if (vbus.isAssigned.(i) && not(env.scope.isWhile.(0)))
           						then raise (Error("Variable "^vbus.name^" has more than one driver"))
                 				else vbus.isAssigned.(i) <- true done
           	else raise (Error("Size of expression is different from subbus width for "^vbus.name))	);
		(match detail with
			Barray(b,_,_) -> ()
			| _ ->	raise (Error("Expected variable of type bus or const "^vbus.name)) )
      | _ -> raise (Error("Expected variable of type bus "^vbus.name))
      

let pred (b,_,_,_,_) (b',_,_,_,_) =
  		let _ =
                for i=0 to ((Array.length b.isAssigned) - 1) do
                        b.isAssigned.(i) <- b'.isAssigned.(i) || b.isAssigned.(i)
                done
        in true      
        
let check_function_outvars env e vbus2 =
  let (ed, t,sz) = e
  in match t with
    Bus -> (
           match ed with
           Id(vname) -> (
                    let vbus1, _, vtype, _, _ = 
                                       		try
                                    			find_variable env.scope vname (* locate a variable by name *)
                                    		with Not_found ->
                                    			raise (Error("undeclared identifier " ^ vname))
                    in
        				if vbus1.size = vbus2.size then 
								let _ = (for i = 0 to vbus1.size-1 
             								do if (vbus1.isAssigned.(i) && not(env.scope.isWhile.(0)))
                   								then raise(Error("Bus "^vbus1.name^" has more than one driver"))
                         					   else vbus1.isAssigned.(i) <- true done) in true
  						else raise(Error("Function output variable width mismatch "^vbus1.name^" "^vbus2.name))
                    )
      		| Subbus(vbus,x,y) -> (
			      let (x,y) = if x < y then (x,y) else (y,x)
                              in if(vbus2.size = y-x+1)
                              then (let _ = (for i = x to y
                                            do if (vbus.isAssigned.(i) && not(env.scope.isWhile.(0)))
                                            then raise (Error("Variable "^vbus.name^" has more than one driver"))
                                            else (vbus.isAssigned.(i) <- true) done) in true)
                              else raise (Error("Size mismatch in function output assignment "^vbus.name))
                            )
        	|_ -> (raise(Error("Expected bus or subbus")))   )
         | Array -> ( match ed with
            			Barray(vbus, sz, exd) ->
                           	if vbus.size != vbus2.size
                             		then raise (Error("Size mismatch in function output assignment "^vbus.name))
                            	  else let _ =
					( match exd with
						  Num(idx) -> (if (vbus.isAssigned.(idx) && not(env.scope.isWhile.(0)))
                                        			then raise (Error("variable "^vbus.name^" has more than one driver"))
                                        			else vbus.isAssigned.(idx) <- true )
						| Id(s) -> let gl_env = (match env.scope.parent with
									  Some(g) -> g
									| _ -> raise (Error("No Global Environment")) )
							      in
							     (try let b,_,_,_,_ = find_variable gl_env s
								in (if (vbus.isAssigned.(b.init) && not(env.scope.isWhile.(0)))
                                        			then raise (Error("variable "^vbus.name^" has more than one driver"))
                                        			else vbus.isAssigned.(b.init) <- true )
							     with Error(_) ->  raise (Error("Function input and output ports must be static: " ^vbus.name)) )
                           			| _ -> raise (Error("Function input and output ports must be static: " ^vbus.name))  )
						in true
           			|_ -> raise(Error("Expected type of variable Barray "))        )   
   | _ -> raise (Error("function assignment must be to a bus or an array")) 
    
     
let check_function_invars env e vbus2 =
  let (ed, t, sz) = e 
   in match t with
     Bus -> (
           match ed with
           Id(vname) -> (
                    let vbus1, _, vtype, _, _ = 
                                       		try find_variable env.scope vname (* locate a variable by name *)
                                    		with Not_found -> raise (Error("undeclared identifier " ^ vname))
                    in if vbus1.size != vbus2.size then raise(Error("Function input variable width mismatch "^vbus1.name^" "^vbus2.name))
  			else true	 )
      		| Subbus(vbus,x,y) -> (
			      let (x,y) = if x < y then (x,y) else (y,x)
                              in if(vbus2.size != y-x+1)
                              then raise (Error("Size mismatch in function output assignment "^vbus.name))
                              else true		)
        	|_ -> (raise(Error("Expected bus or subbus: ")))   )
   | Array -> ( match ed with
            			Barray(vbus, sz, exd) ->
                           	if vbus.size != vbus2.size
                             		then raise (Error("Size mismatch in function output assignment "^vbus.name))
                            	  else let _ =
					( match exd with
						  Num(idx) -> ()
						| Id(s) -> let gl_env = (match env.scope.parent with
									  Some(g) -> g
									| _ -> raise (Error("No Global Environment")) )
							      in
							     (try let _ =find_variable gl_env s
								in ()
							     with Error(_) ->  raise (Error("Function input and output ports must be static: " ^vbus.name)) )
                           			| _ -> raise (Error("Function input and output ports must be static: " ^vbus.name))  )
						in true
           			|_ -> raise(Error("Expected type of variable Barray "))        )  
   | Const -> (match ed with
                 			Num(v) -> if(bit_required v) > vbus2.size
                 				  then raise (Error("Size of expression is bigger than subbus width for "^vbus2.name))
                     		  		  else true
					| Id(s) ->  let b,_,_,_,_ = find_variable env.scope s in
							if b.size != vbus2.size
							then raise (Error("Size of expression is different from subbus width: "^s))
							else true
					| Subbus(vbus,strt,stop) -> if ((abs (strt - stop))+1) != vbus2.size
								 then raise (Error("Size of expression is different from subbus width: "^vbus.name))
								 else true
	                    		| _ -> raise (Error("Const expressions in function call"))	)
   | _ -> raise (Error("function input must be to a bus a const or an array reference")) 

  
let check_call env out_actuals in_actuals fd =
  let _ = print_endline ("Checking function call: "^fd.fid)
    in
  let _ =( try List.for_all2 (check_function_outvars env) out_actuals fd.pout
	with (Invalid_argument(_)) -> raise (Error("Port mismatch for function call: "^fd.fid)) )
  in let _ = ( try List.for_all2 (check_function_invars env) in_actuals fd.pin
 		with (Invalid_argument(_)) -> raise (Error("Port mismatch for function call: "^fd.fid))	)
     in ()
  
  
(* Check expressions *)
(* This returns expr_detail * types * int *)
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
	let (e1,t1,_) = e1 and (e2,t2,_) = e2
	in let t = if ((t1 == Const) && (t2 == Const)) then Const else Bus
	in Binop(e1, op, e2), t, output_size (* Success: result is bus *)
  | Ast.Basn(vname, e1) ->
    	let _ = print_endline ("Checking bus assignment for "^vname) in
		let e1 = chk_expr function_table env e1
  and vbus, _, _, _, _ = find_variable env.scope vname
  in let _ = check_basn env vbus e1
	in let (e1, _, _) = e1
	in Basn(vbus, e1), Bus, vbus.size
  | Ast.Subasn(vname, x, y, e1) ->
	let e1 = chk_expr function_table env e1
  and vbus, _, _, _, _ = find_variable env.scope vname
  in let _ = check_subasn env vbus x y e1
	in let (e1, _, _) = e1
	in Subasn(vbus, x, y, e1), Bus, (abs(x-y) +1);
  | Ast.Aasn(vname, e1, e2) ->
		let e1 = chk_expr function_table env e1
  		and e2 = chk_expr function_table env e2
  		and vbus, size, _, _, _ = find_variable env.scope vname
    in let _ = check_aasn env vbus size e1 e2
	in let (e1, _, _) = e1 and (e2, _, _) = e2
    	in Aasn(vbus, size, e1, e2), Bus, vbus.size
  (* NEED TO CHECK OUTPUT PORTS MATCH WITH LOCALS ASSIGNMENT!!!*)
  | Ast.Unop(op, e1) ->
    	let (e1, t1, s1)= chk_expr function_table env e1
     in Unop(op, e1), t1, s1
  | Ast.Subbus(vname, x, y) ->
    	let _ = print_endline ("Check Sub bus asn for: "^vname^" with params: "
       					^ (string_of_int x) ^ " " ^ (string_of_int y)) in 
    	let vbus, _, _, _, _ = find_variable env.scope vname
     	in check_subbus vbus x y;
    	Subbus(vbus, x, y), Bus, (abs(x-y) +1)
  | Ast.Barray(vname, e1) ->
    	let (e1, t1, s1) = chk_expr function_table env e1
     and varray, size, vtype, _, _ = find_variable env.scope vname
     in check_array_dereference varray size e1 t1 s1;
    Barray(varray, size, e1), vtype, varray.size 
  | Ast.Noexpr -> Noexpr, Void, 0
    
(*Check Statements*)
let rec chk_stmt function_table env = function
    Ast.Expr(e) -> Expr(chk_expr function_table env e)
  | Ast.If(e1, s1, s2) ->
    	let e1, t1, _ = chk_expr function_table env e1
     in 
    let temp = { scope =
                 { env.scope with
                        variables = List.map (
                                fun (b, s, t, l, f) ->
                                        ( { b with isAssigned =
                                                Array.copy b.isAssigned },
                                         s, t, l, f )
                                ) env.scope.variables
                }
        }
    in let stmt_1 = chk_stmt function_table temp s1
       in
    let stmt_2 = chk_stmt function_table env s2
    in let _ = List.for_all2 pred (env.scope.variables) (temp.scope.variables)
      in If(e1,  stmt_1, stmt_2)
  | Ast.While(e1, s1) ->
    	let _ = env.scope.isWhile.(0) <- true in
    	let e1, t1, _= chk_expr function_table env e1
     in (* check_conditional e1 t1;*)
    	let statement = chk_stmt function_table env s1
     in let _ = env.scope.isWhile.(0) <- false in
    	While(e1, statement)
  | Ast.Pos(e1) ->
    	let e1, t1, _= chk_expr function_table env e1
     in (* check_pos_expr e1;*)
    	Pos(e1)
  | Ast.Block(slist) ->
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
            let e1, t1, _ = chk_expr function_table env e1 in 
           		let _ = if not(t1 = Const || t1 = Void) (* Void represents the default case *)
             then raise(Error("Case constants must be CONSTANTS. Received case expression of type: " ^ string_of_sast_type t1)) 
             			else ()
            	in let s1 = chk_stmt function_table env s1
	    	in (e1, s1)
    	in let rec clist_helper l = function
                      	   [] -> List.rev l
                      	 | hd::tl ->
                          		let temp = { scope =
                                       			{ env.scope with
                                              		variables = List.map (
                                                      		fun (b, s, t, l, f) ->
                                                              	( { b with isAssigned =
                                                                      Array.copy b.isAssigned },
                                                               	s, t, l, f )
                                                      		) env.scope.variables
                                      			}
                              	}
          						in  let new_l = ((chk_case_list temp hd),temp) :: l
    							in clist_helper new_l tl
    in let nlist = clist_helper [] caselist
       in let clist = List.map (fun ((x,y),z) -> let _ = (List.for_all2 pred env.scope.variables z.scope.variables) in (x,y)) nlist
(* Un-comment to check if Switch is parsed *)
	(*in let _ = print_endline "parsed a Switch"*)
	in Switch(e, clist)
	
  | Ast.Call(fname, out_list, in_list ) ->	
    	let _ = print_endline ("Checking function call: "^fname) in
    	let _ = List.iter (fun x -> match x with  
       					Ast.Id(v) -> print_endline ("Assigning to: "^v)
                       	| _ -> print_endline "This better be an array deref or a subbus") out_list in
    	let _ = List.iter (fun x -> match x with  
       					Ast.Id(v) -> print_endline ("Call param: "^v)
                       	| _ -> print_endline "Check expr passed as call param:") in_list in
    	let func_decl = 
       		try StringMap.find fname function_table
         with Not_found -> raise (Failure ("undefined function " ^ fname))
    in let inlist = List.fold_left 
    ( fun l x -> let e1 =  chk_expr function_table env x in e1::l ) [] in_list 
    in let outlist = List.fold_left 
    ( fun l x -> let e1 =  chk_expr function_table env x in e1::l ) [] out_list
       in let _ = print_endline "Just before checking function call"
       in let _ = check_call env outlist inlist func_decl
          in let outlist = List.fold_left (fun l x -> let (e1, _, _) = x in e1::l) [] outlist
             in let inlist = List.fold_left (fun l x -> let (e1, _, _) = x in e1::l) [] inlist
              (* Uncomment to check if Function Call is parsed *)
              	(* in let _ = print_endline "Function Call parsed" *)
              		in Call(func_decl, outlist, inlist)


(* Function translation Ast -> Sast. Build Symbol table; parse statements*)
let check_func (env : translation_environment) (portin : (Ast.bus list)) (portout : (Ast.bus list)) (body : Ast.fbody) function_table =
  let _ = print_endline "Checking fucntion... " in
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
    				in let call_lst =
      					(match actual with 
	        					Ast.Call(fname, _, _ ) -> 
             						let f_decl = 
       		               					try StringMap.find fname function_table
                               				with Not_found -> raise (Failure ("undefined function " ^ fname))
	                       			in f_decl::call_lst 
	       						| x -> call_lst (* do nothing *) ) 
           		in (s1::stmt_lst, call_lst)
	
	in let (new_stmt_list,call_lst) = 
	List.fold_left (run_chk_stmt full_env) ([],[]) (List.rev stmts)  
	in (full_env, List.rev call_lst, List.rev new_stmt_list)
  

(* Function table *)
let func (env : translation_environment) (astfn : Ast.fdecl) tmp_ftable =
  let func_scope = { parent = Some(env.scope); variables = []; isIf = Array.make 2 false; isWhile = Array.make 1 false }
  in let func_env = {scope = func_scope }
    in let (chk_floc, chk_calls, chk_fbod) = check_func func_env astfn.portin astfn.portout astfn.body tmp_ftable
     in let fobj = {	pout = astfn.portout;
		    	fid  = astfn.fname;
			    pin  = astfn.portin;
			    floc  = chk_floc;
			    fcalls = chk_calls; 
			    fbod = chk_fbod; }           
        in let new_ftable = StringMap.add astfn.fname fobj tmp_ftable
	in let _ = print_endline ("Added function "^astfn.fname)
	  in new_ftable


(* Program transaltion Ast -> Sast *)
let prog ((constlist : Ast.gdecl list), (funclist : Ast.fdecl list)) =
  let _ = print_endline "Starting prog..." in
  let clist = List.map (
    fun (gdecl : Ast.gdecl)-> 
      let Ast.Const(vbus, value) = gdecl
      in
      let dummy_env = {scope = { parent = None; variables = []; isIf = Array.make 2 false; isWhile = Array.make 1 false}} (*Workaround for while/if multiple assignment *)
        in
      let _ = check_basn dummy_env vbus (Num(value), Const, bit_required value)
      in (vbus, value, Const, Int_signal, true)
                          ) (List.rev constlist)
  in let global_scope = { parent = None; variables = List.rev clist; isIf = Array.make 2 false; isWhile = Array.make 1 false}
        in let global_env = { scope = global_scope }

	   in let rec create_map mymap = function
					  [] -> mymap 
  					| hd::tl -> let new_mymap = func global_env (hd : Ast.fdecl) mymap
							in create_map new_mymap tl
	   in let ftable = create_map StringMap.empty (List.rev funclist)
		in global_env, ftable
