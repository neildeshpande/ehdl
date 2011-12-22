open Ast
open Sast
open Clocktab

module CompMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

exception ErrorS of string 

type s_env = {
    sens_list : string list;  
           } (* can add more stuff to this, list of locals for example, so POS can update it *)     	
    
    
(* insert non-duplicate fcalls, should really do operator overloading and use a generic uniq function *) 
let rec insert_call l c =
	try let _ = List.find ( fun ( s ) -> s.fid = c.fid ) l in l  
	with Not_found-> c::l
(* returns a list whose vals are unique *) 	
let uniq_calls clist = 
    List.fold_left (fun l c -> insert_call l c) [] clist      
    
(* Utility function for producing a delimeter separated string from a list*)    
let rec delim_sprt delim p = match List.length p with 
       0 -> "" | 
       x -> let head = (List.hd p)
	      (* don't want to put in a delim after the last element, hence the if check *) 	 
	     in if ( x = 1 ) then head else  (head ^ delim ^ (delim_sprt delim (List.tl p)) )   

let rec port_descr_list p inOrOut (ports: Ast.bus list) = match ports with
       [] -> p
       | hd::tl -> let typedescr = 
		     (match hd.size with 
			0 -> raise (Failure ("bus size cannot be zero " ))
			(* not doing std_logic because then we have to convert 1 to '1' *)
		  | x -> " std_logic_vector(" ^ string_of_int(hd.size-1) ^ " downto 0)" ) 
		   in  let s = "\t" ^ hd.name ^ " : "  ^ inOrOut ^ typedescr 
		       in port_descr_list  (s::p) inOrOut tl	     
	     
	     
let port_gen cname cobj = 
    let inportlist = port_descr_list [] "in " cobj.pin
	in let portList =  port_descr_list inportlist "out" cobj.pout
	  in (delim_sprt ";\n" (List.rev portList))
	     
	    
(*Auxiliary function: adds conv_std_logic_vector *)
let num_to_slv v size =
   try let _ = int_of_string v
	in "ieee.std_logic_arith.conv_std_logic_vector("^v^","^(string_of_int size)^")"
   with Failure(s) -> v	

(*Auxiliary function: adds conv_integer *)
let to_int v =
   try let _ = int_of_string v
	in v
   with Failure(s) -> "conv_integer("^v^")"
	     

let translate (genv, ftable) =
let create_component cname cobj components =
(*cloc is the local symbol table, required while translating statements and expressions*)
let (cloc, cname) = (cobj.floc,cobj.fid)
  in let libraries = "\nlibrary ieee;\n" ^ 
    "use ieee.std_logic_1164.all;\n" ^
    "use ieee.std_logic_signed.all;\n\n\n"

   in let entity cname cobj = (* entity *) 
	    let s = port_gen cname cobj     
	     in "entity " ^ cname ^ "  is \n\nport (\n" ^
		"\tclk : in std_logic;\n\trst : in std_logic;\n" ^ s ^ ");\n\nend " ^ cname ^ ";\n\n"



(***************While loop processing******************)

in let translate_while (wcond : Sast.expr_detail) (wblock : Sast.s_stmt list) curr_asn_map (curr_cc : int) (curr_fc : int)= 

(* Evaluate expressions *) 
let rec weval e env asn_map cc = match e with
    Num(i) -> string_of_int i,string_of_int i, env, asn_map 
    | Id(i) -> (i ^ "_r" ^ (string_of_int (cc+1))),(i ^ "_r" ^ (string_of_int (cc))), env, asn_map
    | Barray(bs, _, e1) -> let v1,v2 = match e1 with
      			  Num(i) -> (string_of_int i), (string_of_int i)
    			| x -> let i1,i2, _, _ = weval x env asn_map cc
				in ("ieee.std_logic_unsigned.conv_integer(" ^ i1 ^ ")"), ("ieee.std_logic_unsigned.conv_integer(" ^ i2 ^ ")")
		in (bs.name^"_r"^(string_of_int (cc+1))) ^ "(" ^ v1 ^ ")", (bs.name^"_r"^(string_of_int (cc))) ^ "(" ^ v2 ^ ")", env, asn_map 
		(* Using "a" rather than "a(i)" in the sensitivity list, which is fine, because the list must be static *)  
    | Subbus(bs, strt, stop) -> let range = 
		    if strt < stop then "(" ^ (string_of_int stop) ^ " downto " ^ (string_of_int strt) ^ ")" else
					"(" ^ (string_of_int strt) ^ " downto " ^ (string_of_int stop) ^ ")"
		in (bs.name ^ "_r" ^ (string_of_int (cc+1))) ^ range, (bs.name ^ "_r" ^ (string_of_int (cc))) ^ range, env, asn_map
    | Unop(op,e1) -> let v11,v12, _, _ = weval e1 env asn_map cc in 
    ( match op with 
      Umin -> "(- " ^ v11 ^ ")","(- " ^ v12 ^ ")", env, asn_map
    | Not -> "(not " ^ v11 ^ ")","(not " ^ v12 ^ ")", env, asn_map
    | x -> raise (Failure ("ERROR: Invalid Unary Operator ")) ) 
    | Binop(e1,op,e2) -> 
     let v11,v12,_, _ = weval e1 env asn_map cc in let v21, v22, _, _ = weval e2 env asn_map cc
	in let shift_v21 = to_int v21 in let shift_v22 = to_int v22
     in (match op with 
	 Add  -> "(("^v11^")" ^ " + " ^ "("^v21^"))","(("^v12^")" ^ " + " ^ "("^v22^"))", env, asn_map
       | Sub  -> "(("^v11^")" ^ " - " ^ "("^v21^"))","(("^v12^")" ^ " - " ^ "("^v22^"))" , env, asn_map
       | Mul  -> "(("^v11^")" ^ " * " ^ "("^v21^"))","(("^v12^")" ^ " * " ^ "("^v22^"))"  , env, asn_map
       | Lt   -> "(("^v11^")" ^ " < " ^ "("^v21^"))","(("^v12^")" ^ " < " ^ "("^v22^"))", env, asn_map
       | Gt   -> "(("^v11^")" ^ " > " ^ "("^v21^"))","(("^v12^")" ^ " > " ^ "("^v22^"))", env, asn_map
       | Lte  -> "(("^v11^")" ^ " <= " ^ "("^v21^"))","(("^v12^")" ^ " <= " ^ "("^v22^"))", env, asn_map
       | Gte  -> "(("^v11^")" ^ " >= " ^ "("^v21^"))","(("^v12^")" ^ " >= " ^ "("^v22^"))", env, asn_map
       | Eq   -> "(("^v11^")" ^ " = " ^ "("^v21^"))","(("^v12^")" ^ " = " ^ "("^v22^"))", env, asn_map
       | Neq  -> "(("^v11^")" ^ " /= " ^ "("^v21^"))","(("^v12^")" ^ " /= " ^ "("^v22^"))", env, asn_map
       | Or   -> "(("^v11^")" ^ " or " ^ "("^v21^"))","(("^v12^")" ^ " or " ^ "("^v22^"))", env, asn_map
       | And  -> "(("^v11^")" ^ " and " ^ "("^v21^"))","(("^v12^")" ^ " and " ^ "("^v22^"))", env, asn_map
       | Xor  -> "(("^v11^")" ^ " xor  " ^ "("^v21^"))","(("^v12^")" ^ " xor  " ^ "("^v22^"))", env, asn_map
       | Shl  -> "(to_stdlogicvector( to_bitvector("^v11^")" ^ " sll " ^ "("^shift_v21^") ))","(to_stdlogicvector( to_bitvector("^v12^")" ^ " sll " ^ "("^shift_v22^") ))", env, asn_map
       | Shr  -> "(to_stdlogicvector( to_bitvector("^v11^")" ^ " srl " ^ "("^shift_v21^") ))","(to_stdlogicvector( to_bitvector("^v12^")" ^ " srl " ^ "("^shift_v22^") ))", env, asn_map
       | x    -> raise (Failure ("ERROR: Invalid Binary Operator ")) )
   | Basn(i, e1) -> let asn_map = update_asn (Basn(i,Id(i.name))) asn_map
		in let v1, v2, _, _ = weval e1 env asn_map cc
		in  let slv_v1 = num_to_slv v1 i.size
		in  let slv_v2 = num_to_slv v2 i.size
		  in ("\t\t" ^ i.name ^ "_r" ^ (string_of_int (cc+1)) ^ " <= " ^ slv_v1 ^ ";\n" ),
		     ("\t\t" ^ i.name ^ "_r" ^ (string_of_int (cc+1)) ^ " <= " ^ slv_v2 ^ ";\n" ), env, asn_map
   | Subasn(i, strt, stop, e1) -> let asn_map = update_asn (Subasn(i, strt, stop, Id(i.name))) asn_map
		in let v1, v2, _, _ = weval e1 env asn_map cc
		in let slv_v1 = num_to_slv v1 ((abs (strt - stop)+1))
		in let slv_v2 = num_to_slv v2 ((abs (strt - stop)+1))
		  in let range = 
		   if strt < stop then "(" ^ (string_of_int stop) ^ " downto " ^ (string_of_int strt) ^ ")" else
			"(" ^ (string_of_int strt) ^ " downto " ^ (string_of_int stop) ^ ")"
		in ("\t\t" ^ i.name ^ "_r" ^ (string_of_int (cc+1)) ^ range ^ " <= " ^ slv_v1 ^ ";\n" ),
		   ("\t\t" ^ i.name ^ "_r" ^ (string_of_int (cc+1)) ^ range ^ " <= " ^ slv_v2 ^ ";\n" ), env, asn_map
   | Aasn(bs,sz,e1,e2) -> let v11,v12, _, asn_map = match e1 with
      			  Num(i) -> let am = update_asn (Aasn(bs,i,Id("constant"),Id("constant"))) asn_map
					in (string_of_int i),(string_of_int i), env, am
			| Id(i) -> let bus_from_var var = let (bus, _,_,_,_) = var in bus
				   in (try let bs_i = bus_from_var (find_variable genv.scope i)
					in let am = update_asn (Aasn(bs, bs_i.init, Id("constant"), Id("constant"))) asn_map
					in ("ieee.std_logic_unsigned.conv_integer(" ^ i ^ "_r" ^ string_of_int (cc+1) ^ ")"),
					   ("ieee.std_logic_unsigned.conv_integer(" ^ i ^ "_r" ^ string_of_int cc ^ ")"), env, am
				    with Error(_) ->  let am = update_asn (Aasn(bs,sz,e1,Id(bs.name))) asn_map
							in let i1,i2, _, _ = weval e1 env am cc
							in ("ieee.std_logic_unsigned.conv_integer(" ^ i1 ^ ")"),
							   ("ieee.std_logic_unsigned.conv_integer(" ^ i2 ^ ")"), env, am )
    			| x -> let am = update_asn (Aasn(bs,sz,x,Id(bs.name))) asn_map
				in let i1,i2, _, _ = weval x env am cc
				in ("ieee.std_logic_unsigned.conv_integer(" ^ i1 ^ ")"),
				   ("ieee.std_logic_unsigned.conv_integer(" ^ i2 ^ ")"), env, am
             	    in let v21,v22, _, _ = weval e2 env asn_map cc
		     in  let slv_v21 = num_to_slv v21 bs.size
		     in  let slv_v22 = num_to_slv v22 bs.size
		     in ("\t\t" ^ bs.name ^ "_r" ^ (string_of_int (cc+1)) ^ "(" ^ v11 ^ ") " ^ " <= " ^ slv_v21 ^ ";\n" ),
			("\t\t" ^ bs.name ^ "_r" ^ (string_of_int (cc+1)) ^ "(" ^ v12 ^ ") " ^ " <= " ^ slv_v22 ^ ";\n" ), env, asn_map
   | x ->  raise (Failure ("Illegal expression in the body of the While statement" ))

(*Evaluate conditional expressions*)
in let rec wcondeval e env asn_map cc= match e with
    Num(i) -> (string_of_int i)^" /= 0 ",(string_of_int i)^" /= 0 ", env, asn_map 
    | Id(i) -> (i ^ "_r" ^ (string_of_int (cc+1)) ^ " /= 0 "),(i ^ "_r" ^ (string_of_int cc) ^ " /= 0 "), {sens_list = (i^"_r"^string_of_int cc)::env.sens_list;}, asn_map 
    | Barray(bs, _, e1) -> let v1,v2 = match e1 with
      			  Num(i) -> (string_of_int i),(string_of_int i)
    			| x -> let i1,i2, _, _ = weval x env asn_map cc
				in ("ieee.std_logic_unsigned.conv_integer(" ^ i1 ^ ")"), ("ieee.std_logic_unsigned.conv_integer(" ^ i2 ^ ")")
		in (bs.name^"_r"^(string_of_int (cc+1))) ^ "(" ^ v1 ^ ")" ^ " /= 0 ", (bs.name^"_r"^(string_of_int cc)) ^ "(" ^ v1 ^ ")" ^ " /= 0 ", env, asn_map 
    | Subbus(bs, strt, stop) -> let range = 
		   if strt < stop then "(" ^ (string_of_int stop) ^ " downto " ^ (string_of_int strt) ^ ")" else
			"(" ^ (string_of_int strt) ^ " downto " ^ (string_of_int stop) ^ ")"
			in (bs.name ^ "_r" ^ (string_of_int (cc+1))) ^ range ^ " /= 0 ", (bs.name ^ "_r" ^ (string_of_int cc)) ^ range ^ " /= 0 ", env, asn_map
    | Unop(op,e1) -> 
    ( match op with 
      Umin -> let v1,v2,_, _ = weval e1 env asn_map cc in "(- " ^ v1 ^ ")"  ^ " /= 0 ", "(- " ^ v2 ^ ")"  ^ " /= 0 ", env, asn_map
    | Not -> let v1,v2,_ , _ = wcondeval e1 env asn_map cc in "(not " ^ v1 ^ ")", "(not " ^ v2 ^ ")", env, asn_map	
    | x -> raise (Failure ("ERROR: Invalid Unary Operator ")) ) 
    | Binop(e1,op,e2) -> 
     let v11,v12, _, _ = weval e1 env asn_map cc in let v21,v22,_ , _ = weval e2 env asn_map cc
	in let shift_v21 = to_int v21 in let shift_v22 = to_int v22
     in (match op with 
	 Add  -> "(("^v11^")" ^ " + " ^ "("^v21^"))" ^ " /= 0 ","(("^v12^")" ^ " + " ^ "("^v22^"))" ^ " /= 0 ", env, asn_map
       | Sub  -> "(("^v11^")" ^ " - " ^ "("^v21^"))" ^ " /= 0 ","(("^v12^")" ^ " - " ^ "("^v22^"))" ^ " /= 0 ", env, asn_map
       | Mul  -> "(("^v11^")" ^ " * " ^ "("^v21^"))" ^ " /= 0 ","(("^v12^")" ^ " * " ^ "("^v22^"))" ^ " /= 0 ", env, asn_map
       | Lt   -> "(("^v11^")" ^ " < " ^ "("^v21^"))","(("^v12^")" ^ " < " ^ "("^v22^"))", env, asn_map
       | Gt   -> "(("^v11^")" ^ " > " ^ "("^v21^"))","(("^v12^")" ^ " > " ^ "("^v22^"))", env, asn_map
       | Lte  -> "(("^v11^")" ^ " <= " ^ "("^v21^"))","(("^v12^")" ^ " <= " ^ "("^v22^"))", env, asn_map
       | Gte  -> "(("^v11^")" ^ " >= " ^ "("^v21^"))","(("^v12^")" ^ " >= " ^ "("^v22^"))", env, asn_map
       | Eq   -> "(("^v11^")" ^ " = " ^ "("^v21^"))","(("^v12^")" ^ " = " ^ "("^v22^"))", env, asn_map
       | Neq  -> "(("^v11^")" ^ " /= " ^ "("^v21^"))","(("^v12^")" ^ " /= " ^ "("^v22^"))", env, asn_map
       | Xor  -> "(("^v11^")" ^ " xor  " ^ "("^v21^"))" ^ " /= 0 ","(("^v12^")" ^ " xor  " ^ "("^v22^"))" ^ " /= 0 ", env, asn_map
       | Shl  -> "(to_stdlogicvector( to_bitvector("^v11^")" ^ " sll " ^ "("^shift_v21^") ))" ^ " /= 0 ","(to_stdlogicvector( to_bitvector("^v12^")" ^ " sll " ^ "("^shift_v22^") ))" ^ " /= 0 ", env, asn_map
       | Shr  -> "(to_stdlogicvector( to_bitvector("^v11^")" ^ " srl " ^ "("^shift_v21^") ))" ^ " /= 0 ","(to_stdlogicvector( to_bitvector("^v12^")" ^ " srl " ^ "("^shift_v22^") ))" ^ " /= 0 ", env, asn_map
       | Or   -> let v11,v12, _, _ = wcondeval e1 env asn_map cc in let v21,v22, _, _ = wcondeval e2 env asn_map cc
			in "(("^v11^")" ^ " or " ^ "("^v21^"))","(("^v12^")" ^ " or " ^ "("^v22^"))", env, asn_map
       | And  -> let v11,v12, _, _ = wcondeval e1 env asn_map cc in let v21,v22, _, _ = wcondeval e2 env asn_map cc
			in "(("^v11^")" ^ " and " ^ "("^v21^"))","(("^v12^")" ^ " and " ^ "("^v22^"))", env, asn_map
       | x    -> raise (Failure ("ERROR: Invalid Binary Operator ")) )
   | x ->  raise (Failure ("Illegal conditional expression" ))


 (* translate_wstmt *)
 in let rec translate_wstmt (env,str1,str2,asn_map,cc) stmt = 
    (  match stmt with  
	  Block(stmts)  -> List.fold_left translate_wstmt (env,str1,str2,asn_map,cc) (List.rev stmts) 
	| Expr(ex) -> let (e, ex_t, ex_s) = ex
	    in let s1,s2,_,asn_map = weval e env asn_map cc in (env, (str1 ^ s1), (str2 ^ s2), asn_map, cc)   
	| If(e,if_stmt,else_stmt) -> 
			(*Check there is no POS*)
			let _ = (match if_stmt with
					Block(sl) -> let chk_if_pos = (function
							  Pos(_) -> raise (Error("Pos inside if statement"))
							| _ -> "" )
			     			 in let _ = (List.map chk_if_pos sl) in ""
					| Pos(_) -> raise (Error("Pos inside if statement"))
			   		| _ -> "" )
			in let _ = (match else_stmt with
					Block(sl) -> let chk_if_pos = (function
							  Pos(_) -> raise (Error("Pos inside if statement"))
							| _ -> "" )
			     			 in let _ = (List.map chk_if_pos sl) in ""
					| Pos(_) -> raise (Error("Pos inside if statement"))
			   		| _ -> "" )	
		in let s1,s2,_,_ = wcondeval e env asn_map cc
	    in let _,if_block1,if_block2,asn_map,_ = translate_wstmt (env,"","",asn_map,cc) if_stmt
	    in let _,else_block1,else_block2,asn_map,_ = translate_wstmt (env,"","",asn_map,cc) else_stmt
	    in (env, (str1^"\t\tif (" ^ s1 ^ ") then \n" ^ if_block1 ^ "\n\t\telse\n" ^ else_block1 ^ "\t\tend if;\n"),
		     (str2^"\t\tif (" ^ s2 ^ ") then \n" ^ if_block2 ^ "\n\t\telse\n" ^ else_block2 ^ "\t\tend if;\n"), asn_map,cc)
	| Switch ( e, c_list ) -> 
	    ( match c_list with 
	      [] -> env,"","",asn_map,cc
	     |hd::tl ->       
	      let (e1, stmt) = hd
			(*Check there is no POS*)
			in let _ = (match stmt with
					Block(sl) -> let chk_if_pos = (function
							  Pos(_) -> raise (Error("Pos inside switch statement"))
							| _ -> "" )
			     			 in let _ = (List.map chk_if_pos sl) in ""
					| Pos(_) -> raise (Error("Pos inside switch statement"))
			   		| _ -> "" )
           in let s11,s12,_,_ = weval e env asn_map cc in let s21,s22,_,_ = weval e1 env asn_map cc  
           in let s31 = "\t\tif (" ^ s11 ^ " = " ^ s21 ^ ") then \n"
	   in let s32 = "\t\tif (" ^ s12 ^ " = " ^ s22 ^ ") then \n"  
           in let _,if_block1,if_block2,asn_map,_ = translate_wstmt (env,"","",asn_map,cc) stmt
		   in let _,s51,s52,asn_map,_ = List.fold_left (translate_case (s11,s12)) (env,"","",asn_map,cc) tl 	
		   in (env, (str1^s31 ^ if_block1 ^ s51 ^ "\t\tend if;\n"),(str2^s32 ^ if_block2 ^ s52 ^ "\t\tend if;\n"),asn_map,cc ) )
	| x -> 	raise (Failure ("Illegal statement within the body of the While statement" )) )
    and translate_case (left1,left2) (env,s1,s2,asn_map,cc) (e,stmt) = 
			(*Check there is no POS*)
			let _ = (match stmt with
					Block(sl) -> let chk_if_pos = (function
							  Pos(_) -> raise (Error("Pos inside switch statement"))
							| _ -> "" )
			     			 in let _ = (List.map chk_if_pos sl) in ""
					| Pos(_) -> raise (Error("Pos inside switch statement"))
			   		| _ -> "" )
      in ( match e with 
     (* SAST needs to check there is atmost one dafault and no duplicate 
     case expressions *) 
          Noexpr->   translate_wstmt (env,s1 ^ "\t\telse \n", s2 ^ "\t\telse \n",asn_map,cc) stmt   
        | x     ->    let right1,right2,_,asn_map = weval e env asn_map cc
         in translate_wstmt (env,s1 ^ "\t\telsif (" ^ left1 ^ " = " ^ right1 ^ ") then \n",
			         s2 ^ "\t\telsif (" ^ left2 ^ " = " ^ right2 ^ ") then \n",asn_map,cc ) stmt  
         )
(* end of translate_wstmt *)


(*Translating While loop*)
in let rec build_while wstr str1 str2 asn_map prev_asn_map cc = function
	  [] -> wstr, str1, str2, asn_map, prev_asn_map, cc
	| (Pos(en))::tl -> (let sen1, sen2, _, _ = wcondeval en {sens_list=[]} asn_map cc
			(*While condition: always check the output of the loop*)
	               in let wcs1, wcs2,_,_ = wcondeval wcond {sens_list=[]} Im.empty cc
		       in let (_,async) = get_asn curr_asn_map
		       in let sync = get_nc_asn curr_asn_map asn_map
		       in let (psync,_) = get_asn prev_asn_map
		       in let (wsync,wasync) = get_asn asn_map
			(*No Asynchronous Variables can be assigned within a While loop or multiple assignments will occure!*)
			in let _ = match wasync with
				[] -> ""
			       | _ -> raise (Error("Asynchronous variables assigned in the body of a While statement"))
			(*Even inside the While statement Variables can be assigned only once!*)
			in let chk_asn (asn : Sast.expr_detail) = try let _  = (List.find (fun a -> a = asn) psync) in 
									raise (Error("Multiple variable assignment within a While statement"))
									with Not_found -> ""
			in let _ = List.map chk_asn wsync
			(*Find common sync assignments between curr_asn_map and while asn_map*)
			in let print_ccp1 (ap) = (function
			| Basn(x,_) -> ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ " <= " ^ x.name ^ "_r" ^ (string_of_int cc) ^ ";\n"
			| Aasn(x,i,e1,_) -> (match e1 with (*Either e1 is a constant or the whole array is assigned!*)
						Id("constant") -> ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ "("
								  ^ string_of_int i ^ ")" ^ " <= " ^ x.name ^ "_r"
								  ^ (string_of_int cc) ^ "(" ^ string_of_int i ^ ")" ^ ";\n"
						| e -> ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ " <= " ^ x.name ^ "_r"
						          ^ (string_of_int cc) ^ ";\n"		)
			| Subasn(x,strt,stop,_) -> let range = 
		  			 if strt < stop then "(" ^ (string_of_int stop) ^ " downto " ^ (string_of_int strt) ^ ")" else
							     "(" ^ (string_of_int strt) ^ " downto " ^ (string_of_int stop) ^ ")"
				in ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ range ^ " <= " ^ x.name ^ "_r" ^ (string_of_int cc) ^ range ^ ";\n"
			| x -> raise (Error("not an assignment"))	)
			in let print_reset (ap) = (function
			| Basn(x,_) -> ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ " <= ieee.std_logic_arith.conv_std_logic_vector("
					  ^ string_of_int (x.init) ^ "," ^ string_of_int (x.size) ^ ");\n"
			| Aasn(x,i,e1,_) -> (match e1 with (*Either e1 is a constant or the whole array is assigned!*)
						Id("constant") -> ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ "("
								  ^ string_of_int i ^ ")" ^ " <= ieee.std_logic_arith.conv_std_logic_vector("
					  			  ^ string_of_int (x.init) ^ "," ^ string_of_int (x.size) ^ ");\n"
						| e -> ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ " <= (others => ieee.std_logic_arith.conv_std_logic_vector("
					  			  ^ string_of_int (x.init) ^ "," ^ string_of_int (x.size) ^ "));\n"		)
			| Subasn(x,strt,stop,_) -> let range = 
		  			 if strt < stop then "(" ^ (string_of_int stop) ^ " downto " ^ (string_of_int strt) ^ ")" else
							     "(" ^ (string_of_int strt) ^ " downto " ^ (string_of_int stop) ^ ")"
				in ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ range ^ " <= ieee.std_logic_arith.conv_std_logic_vector("
					  ^ string_of_int (x.init) ^ "," ^ string_of_int ((abs (strt-stop))+1) ^ ");\n"
			| x -> raise (Error("not an assignment"))	)
			
			in let nw = (List.fold_left print_ccp1 ("--Pos--\n") async) ^ (List.fold_left print_ccp1 ("") sync)
			in let ywreset = (List.fold_left print_reset ("") wsync) ^ (List.fold_left print_reset ("") psync)
			in let ywpr = List.fold_left print_ccp1 ("") psync
			in let ywyr1 = str1
			in let ywyr2 = (List.fold_left print_ccp1 ("") wsync) (*use str2 for the other version*)

			in let seqp = "process(clk,rst)\nbegin\nif rst = '0' then\n"
			in let posedge = "elsif clk'event and clk = '1' then\n"
			in let swc_if = "if " ^ wcs1 ^ "then\n"
			in let sen1 = "if " ^ sen1 ^ " then\n"
			in let swc_else = "end if;\nelse\n" (*use "end if;\nelsif " ^ wcs2 ^ " then\n" for the other version*)
			in let sen2 = "if " ^ sen2 ^ " then\n"
			in let endp = "end if;\nend if;\nend if;\nend process;\n\n"
			in let wstr = wstr^(nw^seqp^ywreset^posedge^swc_if^sen1^ywpr^ywyr1^swc_else^sen2^ywpr^ywyr2^endp)
			in let str1 = "" in let str2 = ""
			in let prev_asn_map = 
				let up pam asn = update_asn asn pam
				in List.fold_left up prev_asn_map wsync
			in let asn_map = Im.empty
			in let cc = cc+1 
			in build_while wstr str1 str2 asn_map prev_asn_map cc tl )
	| hd::tl -> let _,str1,str2,asn_map,cc = translate_wstmt ({sens_list=[]},str1,str2, asn_map ,cc) hd
		in build_while wstr str1 str2 asn_map prev_asn_map cc tl

in let wstr, str1, str2, asn_map, prev_asn_map, curr_cc = build_while "" "" "" Im.empty Im.empty curr_cc wblock

in let (psync,_) = get_asn prev_asn_map
in let (sync,_) = get_asn asn_map
	in let tmp_asn_map =
		let up pam asn = update_asn asn pam
		in List.fold_left up curr_asn_map psync
	in let curr_asn_map = 
		let up pam asn = update_asn asn pam
		in List.fold_left up tmp_asn_map sync
in {sens_list=[]},wstr,curr_asn_map,curr_cc

(***************End of while loop processing******************)


(* Evaluate expressions *) 
 in let rec eval e env asn_map cc= match e with
    Num(i) -> string_of_int i, env, asn_map 
    | Id(i) -> (try let _ = (find_variable genv.scope i) (*no constants in the sensitivity list!*)
			in (i ^ "_r" ^ (string_of_int cc)), env, asn_map
		with (Error(_)) -> (i ^ "_r" ^ (string_of_int cc)), {sens_list = (i^"_r"^string_of_int cc)::env.sens_list;}, asn_map )
    | Barray(bs, _, e1) -> let v1, env = match e1 with
      			  Num(i) -> (string_of_int i), env
    			| x -> let i, env, _ = eval x env asn_map cc
				in ("ieee.std_logic_unsigned.conv_integer(" ^ i ^ ")"), env
		in (bs.name^"_r"^(string_of_int cc)) ^ "(" ^ v1 ^ ")", {sens_list = (bs.name^"_r"^string_of_int cc)::env.sens_list;}, asn_map 
		(* Using "a" rather than "a(i)" in the sensitivity list, which is fine, because the list must be static *)  
    | Subbus(bs, strt, stop) -> let range = 
		   if strt < stop then "(" ^ (string_of_int stop) ^ " downto " ^ (string_of_int strt) ^ ")" else
			"(" ^ (string_of_int strt) ^ " downto " ^ (string_of_int stop) ^ ")"
		in (try let _ = (find_variable genv.scope bs.name) 
			in (bs.name ^ "_r" ^ (string_of_int cc)) ^ range, env, asn_map
		    with (Error(_)) -> (bs.name ^ "_r" ^ (string_of_int cc)) ^ range, {sens_list =  (bs.name^"_r"^string_of_int cc)::env.sens_list;}, asn_map )
    | Unop(op,e1) -> let v1, env, _ = eval e1 env asn_map cc in 
    ( match op with 
      Umin -> "(- " ^ v1 ^ ")"
    | Not -> "(not " ^ v1 ^ ")"
    | x -> raise (Failure ("ERROR: Invalid Unary Operator ")) ), env, asn_map 
    | Binop(e1,op,e2) -> 
     let v1, env, _ = eval e1 env asn_map cc in let v2, env, _ = eval e2 env asn_map cc
	in let shift_v2 = to_int v2
     in (match op with 
	 Add  -> "(("^v1^")" ^ " + " ^ "("^v2^"))"
       | Sub  -> "(("^v1^")" ^ " - " ^ "("^v2^"))" 
       | Mul  -> "(("^v1^")" ^ " * " ^ "("^v2^"))"  
       | Lt   -> "(("^v1^")" ^ " < " ^ "("^v2^"))"
       | Gt   -> "(("^v1^")" ^ " > " ^ "("^v2^"))"
       | Lte  -> "(("^v1^")" ^ " <= " ^ "("^v2^"))"
       | Gte  -> "(("^v1^")" ^ " >= " ^ "("^v2^"))"
       | Eq   -> "(("^v1^")" ^ " = " ^ "("^v2^"))"
       | Neq  -> "(("^v1^")" ^ " /= " ^ "("^v2^"))"
       | Or   -> "(("^v1^")" ^ " or " ^ "("^v2^"))"
       | And  -> "(("^v1^")" ^ " and " ^ "("^v2^"))"
       | Xor  -> "(("^v1^")" ^ " xor  " ^ "("^v2^"))"
       | Shl  -> "(to_stdlogicvector( to_bitvector("^v1^")" ^ " sll " ^ "("^shift_v2^") ))"
       | Shr  -> "(to_stdlogicvector( to_bitvector("^v1^")" ^ " srl " ^ "("^shift_v2^") ))"
       | x    -> raise (Failure ("ERROR: Invalid Binary Operator ")) ), env, asn_map
   | Basn(i, e1) -> let asn_map = update_asn (Basn(i,Id(i.name))) asn_map
		in let v1, env, _ = eval e1 env asn_map cc
		in  let slv_v1 = num_to_slv v1 i.size
		  in ("\t\t" ^ i.name ^ "_r" ^ (string_of_int cc) ^ " <= " ^ slv_v1 ^ ";\n" ) , env, asn_map
   | Subasn(i, strt, stop, e1) -> let asn_map = update_asn (Subasn(i, strt, stop, Id(i.name))) asn_map
		in let v1, env, _ = eval e1 env asn_map cc
		in let slv_v1 = num_to_slv v1 ((abs (strt - stop)+1))
		  in let range = 
		   if strt < stop then "(" ^ (string_of_int stop) ^ " downto " ^ (string_of_int strt) ^ ")" else
			"(" ^ (string_of_int strt) ^ " downto " ^ (string_of_int stop) ^ ")"
		in ("\t\t" ^ i.name ^ "_r" ^ (string_of_int cc) ^ range ^ " <= " ^ slv_v1 ^ ";\n" ) , env, asn_map
   | Aasn(bs,sz,e1,e2) -> let v1, env, asn_map = match e1 with
      			  Num(i) -> let am = update_asn (Aasn(bs,i,Id("constant"),Id("constant"))) asn_map
					in (string_of_int i), env, am
			| Id(i) -> let bus_from_var var = let (bus, _,_,_,_) = var in bus
				   in (try let bs_i = bus_from_var (find_variable genv.scope i)
					in let am = update_asn (Aasn(bs, bs_i.init, Id("constant"), Id("constant"))) asn_map
					in ("ieee.std_logic_unsigned.conv_integer(" ^ i ^ "_r" ^ string_of_int cc ^ ")"), env, am
				    with Error(_) ->  let am = update_asn (Aasn(bs,sz,e1,Id(bs.name))) asn_map
							in let i, env, _ = eval e1 env am cc
							in ("ieee.std_logic_unsigned.conv_integer(" ^ i ^ ")"), env, am )
    			| x -> let am = update_asn (Aasn(bs,sz,x,Id(bs.name))) asn_map
				in let i, env, _ = eval x env am cc
				in ("ieee.std_logic_unsigned.conv_integer(" ^ i ^ ")"), env, am
             in let v2, env, _ = eval e2 env asn_map cc
		     in  let slv_v2 = num_to_slv v2 bs.size
		     in ("\t\t" ^ bs.name ^ "_r" ^ (string_of_int cc) ^ "(" ^ v1 ^ ") " ^ " <= " ^ slv_v2 ^ ";\n" ), env, asn_map
   | x ->  raise (Failure ("Expression not supported yet " ))


(*Evaluate conditional expressions*)
in let rec condeval e env asn_map cc= match e with
    Num(i) -> (string_of_int i)^" /= 0 ", env, asn_map 
    | Id(i) -> (try let _ = (find_variable genv.scope i) (*no constants in the sensitivity list!*)
			in (i ^ "_r" ^ (string_of_int cc) ^ " /= 0 "), env, asn_map
		with (Error(_)) -> (i ^ "_r" ^ (string_of_int cc) ^ " /= 0 "), {sens_list = (i^"_r"^string_of_int cc)::env.sens_list;}, asn_map )
    | Barray(bs, _, e1) -> let v1, env = match e1 with
      			  Num(i) -> (string_of_int i), env
    			| x -> let i, env, _ = eval x env asn_map cc
				in ("ieee.std_logic_unsigned.conv_integer(" ^ i ^ ")"), env
		in (bs.name^"_r"^(string_of_int cc)) ^ "(" ^ v1 ^ ")" ^ " /= 0 ", {sens_list = (bs.name^"_r"^string_of_int cc)::env.sens_list;}, asn_map 
		(* Using "a" rather than "a(i)" in the sensitivity list, which is fine, because the list must be static *)  
    | Subbus(bs, strt, stop) -> let range = 
		   if strt < stop then "(" ^ (string_of_int stop) ^ " downto " ^ (string_of_int strt) ^ ")" else
			"(" ^ (string_of_int strt) ^ " downto " ^ (string_of_int stop) ^ ")"
		in (try let _ = (find_variable genv.scope bs.name) 
			in (bs.name ^ "_r" ^ (string_of_int cc)) ^ range ^ " /= 0 ", env, asn_map
		    with (Error(_)) -> (bs.name ^ "_r" ^ (string_of_int cc)) ^ range ^ " /= 0 ", {sens_list =  (bs.name^"_r"^string_of_int cc)::env.sens_list;}, asn_map )
    | Unop(op,e1) ->  
    ( match op with 
      Umin -> let v1, env, _ = eval e1 env asn_map cc in "(- " ^ v1 ^ ")"  ^ " /= 0 "
    | Not -> let v1, env, _ = condeval e1 env asn_map cc in "(not " ^ v1 ^ ")"		
    | x -> raise (Failure ("ERROR: Invalid Unary Operator ")) ), env, asn_map 
    | Binop(e1,op,e2) -> 
     let v1, env, _ = eval e1 env asn_map cc in let v2, env, _ = eval e2 env asn_map cc
	in let shift_v2 = to_int v2
     in (match op with 
	 Add  -> "(("^v1^")" ^ " + " ^ "("^v2^"))" ^ " /= 0 "
       | Sub  -> "(("^v1^")" ^ " - " ^ "("^v2^"))" ^ " /= 0 "
       | Mul  -> "(("^v1^")" ^ " * " ^ "("^v2^"))" ^ " /= 0 "
       | Lt   -> "(("^v1^")" ^ " < " ^ "("^v2^"))"
       | Gt   -> "(("^v1^")" ^ " > " ^ "("^v2^"))"
       | Lte  -> "(("^v1^")" ^ " <= " ^ "("^v2^"))"
       | Gte  -> "(("^v1^")" ^ " >= " ^ "("^v2^"))"
       | Eq   -> "(("^v1^")" ^ " = " ^ "("^v2^"))"
       | Neq  -> "(("^v1^")" ^ " /= " ^ "("^v2^"))"
       | Xor  -> "(("^v1^")" ^ " xor  " ^ "("^v2^"))" ^ " /= 0 "
       | Shl  -> "(to_stdlogicvector( to_bitvector("^v1^")" ^ " sll " ^ "("^shift_v2^") ))" ^ " /= 0 "
       | Shr  -> "(to_stdlogicvector( to_bitvector("^v1^")" ^ " srl " ^ "("^shift_v2^") ))" ^ " /= 0 "
       | Or   -> let v1, env, _ = condeval e1 env asn_map cc in let v2, env, _ = condeval e2 env asn_map cc
			in "(("^v1^")" ^ " or " ^ "("^v2^"))"
       | And  -> let v1, env, _ = condeval e1 env asn_map cc in let v2, env, _ = condeval e2 env asn_map cc
			in "(("^v1^")" ^ " and " ^ "("^v2^"))"
       | x    -> raise (Failure ("ERROR: Invalid Binary Operator ")) ), env, asn_map
   | x ->  raise (Failure ("Illegal conditional expression" ))


 (* translate_Stmt *)
 in let rec translate_stmt (env,str,asn_map,cc) stmt = 
    (  match stmt with  
	  Block(stmts)  -> List.fold_left translate_stmt (env,str,asn_map,cc) (List.rev stmts) 
	| Expr(ex) -> let (e, ex_t, ex_s) = ex
	    in let s,env,asn_map = eval e env asn_map cc in (env, (str ^ s), asn_map, cc)   
	| If(e,if_stmt,else_stmt) -> 
			(*Check there is no POS*)
			let _ = (match if_stmt with
					Block(sl) -> let chk_if_pos = (function
							  Pos(_) -> raise (Error("Pos inside if statement"))
							| _ -> "" )
			     			 in let _ = (List.map chk_if_pos sl) in ""
					| Pos(_) -> raise (Error("Pos inside if statement"))
			   		| _ -> "" )
			in let _ = (match else_stmt with
					Block(sl) -> let chk_if_pos = (function
							  Pos(_) -> raise (Error("Pos inside if statement"))
							| _ -> "" )
			     			 in let _ = (List.map chk_if_pos sl) in ""
					| Pos(_) -> raise (Error("Pos inside if statement"))
			   		| _ -> "" )
		in let s,env,_ = condeval e env asn_map cc
	    in let env,if_block,asn_map,_ = translate_stmt (env,"",asn_map,cc) if_stmt
	    in let env,else_block,asn_map,_ = translate_stmt (env,"",asn_map,cc) else_stmt
	    in (env, (str^"\t\tif (" ^ s ^ ") then \n" ^ if_block 
	    (* the tabbing needs to be done programmatically, not manually. 
	    I am assuming SAST will tell us the nesting depth *)    
	    ^ "\n\t\telse\n" ^ else_block ^ "\t\tend if;\n"), asn_map,cc)
	| Switch ( e, c_list ) -> 
	    ( match c_list with 
	      [] -> env,"",asn_map,cc
	     |hd::tl ->       
	     (*let s,env,asn_map = eval e env asn_map 
           in *)let (e1, stmt) = hd
			(*Check there is no POS*)
			in let _ = (match stmt with
					Block(sl) -> let chk_if_pos = (function
							  Pos(_) -> raise (Error("Pos inside switch statement"))
							| _ -> "" )
			     			 in let _ = (List.map chk_if_pos sl) in ""
					| Pos(_) -> raise (Error("Pos inside switch statement"))
			   		| _ -> "" )
           in let s1,env,_ = eval e env asn_map cc in let s2,env,_ = eval e1 env asn_map cc  
           in let s3 = "\t\tif (" ^ s1 ^ " = " ^ s2 ^ ") then \n"  
           in let env,if_block,asn_map,_ = translate_stmt (env,"",asn_map,cc) stmt
		   in let env,s5,asn_map,_ = List.fold_left (translate_case s1) (env,"",asn_map,cc) tl 	
		   in (env, (str^s3 ^ if_block ^ s5 ^ "\t\tend if;\n"),asn_map,cc ) )
	| While(e1,s1) -> let curr_fc,sl = (match s1 with
					Block(sl) -> let inc_fc curr_fc = (function
							Pos(_) -> curr_fc + 1
							| _ -> curr_fc )
			     			 in (List.fold_left inc_fc cc sl), sl
			   		| _ -> raise (Error("While statement requires a block containing at least one POS and another statement")))
			in let rsl = List.rev sl
			in let rsl, curr_fc = match List.hd rsl with
					  Pos(en) -> rsl, curr_fc
					| _ -> (print_endline "Warning: inferred Pos(1) at the end of the while loop"); (Pos(Num(1)))::rsl, curr_fc+1
			in let sl = List.rev rsl
			in translate_while e1 sl asn_map cc (curr_fc-1)
	| Pos(en) -> let sen,_,_ = condeval en env asn_map cc
		       in let (sync,async) = get_asn asn_map
			in let print_ccp1 (ap) = (function
			  Basn(x,_) -> ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ " <= " ^ x.name ^ "_r" ^ (string_of_int cc) ^ ";\n"
			| Aasn(x,i,e1,_) -> (match e1 with (*Either e1 is a constant or the whole array is assigned!*)
						Id("constant") -> ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ "("
								  ^ string_of_int i ^ ")" ^ " <= " ^ x.name ^ "_r"
								  ^ (string_of_int cc) ^ "(" ^ string_of_int i ^ ")" ^ ";\n"
						| e -> ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ " <= " ^ x.name ^ "_r"
						          ^ (string_of_int cc) ^ ";\n"		)
			| Subasn(x,strt,stop,_) -> let range = 
		  			 if strt < stop then "(" ^ (string_of_int stop) ^ " downto " ^ (string_of_int strt) ^ ")" else
							     "(" ^ (string_of_int strt) ^ " downto " ^ (string_of_int stop) ^ ")"
				in ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ range ^ " <= " ^ x.name ^ "_r" ^ (string_of_int cc) ^ range ^ ";\n"
			| x -> raise (Error("not an assignment"))	)
			in let print_reset (ap) = (function
			  Basn(x,_) -> ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ " <= ieee.std_logic_arith.conv_std_logic_vector("
					  ^ string_of_int (x.init) ^ "," ^ string_of_int (x.size) ^ ");\n"
			| Aasn(x,i,e1,_) -> (match e1 with (*Either e1 is a constant or the whole array is assigned!*)
						Id("constant") -> ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ "("
								  ^ string_of_int i ^ ")" ^ " <= ieee.std_logic_arith.conv_std_logic_vector("
					  			  ^ string_of_int (x.init) ^ "," ^ string_of_int (x.size) ^ ");\n"
						| e -> ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ " <= (others => ieee.std_logic_arith.conv_std_logic_vector("
					  			  ^ string_of_int (x.init) ^ "," ^ string_of_int (x.size) ^ "));\n"		)
			| Subasn(x,strt,stop,_) -> let range = 
		  			 if strt < stop then "(" ^ (string_of_int stop) ^ " downto " ^ (string_of_int strt) ^ ")" else
							     "(" ^ (string_of_int strt) ^ " downto " ^ (string_of_int stop) ^ ")"
				in ap ^ x.name ^ "_r" ^ (string_of_int (cc+1)) ^ range ^ " <= ieee.std_logic_arith.conv_std_logic_vector("
					  ^ string_of_int (x.init) ^ "," ^ string_of_int ((abs (strt-stop))+1) ^ ");\n"
			| x -> raise (Error("not an assignment"))	)
			
			in let nr = List.fold_left print_ccp1 ("--Pos--\n") async
			in let reset = List.fold_left print_reset ("") sync
			in let yr = List.fold_left print_ccp1 ("") sync
			in let seqp = "process(clk,rst)\nbegin\nif rst = '0' then\n"
			in let posedge = "elsif clk'event and clk = '1' then\n"
			in let sen = "if " ^ sen ^ " then\n"
			in let endp = "end if;\nend if;\nend process;\n\n"
			in env,(nr^seqp^reset^posedge^sen^yr^endp),asn_map,(cc+1)

	| Call(fdecl, out_list, in_list ) ->
	   (* start of f *) 
	   let f (s,l,am) b =
		let bus_from_var var = let (bus, _,_,_,_) = var in bus
		(*using the field "size" in Barray(_,size,_) to identify which bus in the vector is assigned*)
		in let actual_barray am bs =  function
			  Num(i) -> let am = update_asn (Aasn(bs, i, Id("constant"), Id("constant"))) am
					in (string_of_int i), am
			| Id(i) -> (try let bs_i = bus_from_var (find_variable genv.scope i)
					in let am = update_asn (Aasn(bs, bs_i.init, Id("constant"), Id("constant"))) am
					in ("ieee.std_logic_unsigned.conv_integer(" ^ i ^ "_r" ^ (string_of_int cc) ^ ")"), am
			   	   with Error(_) -> raise (Failure("Function Call to " ^ fdecl.fid ^ ": actual "^ bs.name ^ " is not static"))  )
    			| x -> raise (Failure("Function Call to " ^ fdecl.fid ^ ": illegal actual assignment"))
 	    in
	    let s1, asn_map = (match (List.hd l) with
	     Num(v) -> let s = num_to_slv (string_of_int v) b.size in s,am
	   |  Id(i) -> let bs_i = bus_from_var (find_variable cloc.scope i)
			in let am = update_asn (Basn(bs_i, Id("port map"))) am (*I don't care about the expr_detail in the assignment*)
			in i ^ "_r" ^ (string_of_int cc), am
	   | Barray(bs, _, e1) -> let v1,am = actual_barray am bs e1
				in bs.name ^ "_r" ^ (string_of_int cc) ^ "(" ^ v1 ^ ")", am
	   | Subbus(bs, strt, stop) -> let range = 
		   if strt < stop then "(" ^ (string_of_int stop) ^ " downto " ^ (string_of_int strt) ^ ")" else
			"(" ^ (string_of_int strt) ^ " downto " ^ (string_of_int stop) ^ ")"
			in let am = update_asn (Subasn(bs, strt, stop, Id("port map"))) am
			in bs.name ^ "_r" ^ (string_of_int cc) ^ range, am
	   | x ->  raise (Failure ("Function Call to " ^ fdecl.fid ^ ": In/Output port mapping must use pre-existing variables " ))   ) 
	   in  s^",\n\t\t"^b.name^" => " ^ s1 , (List.tl l) , asn_map   (* end of f *) 
	   
	   (* When a function uses the same component multiple times, it needs to use unique labels to describe the 
	   separate instantiations. One way to do this is to append a string that is a function of the head of the 
	   output_list. The output_list is guranteed to be non-empty, SAST should also gurantee that the same output 
	   variable does not get used in two different calls as outputs. *)
	   in let array_label = function
		Num(i) -> string_of_int i
	      | Id(i)  -> i
	      | x -> raise (Failure("Function Call to " ^ fdecl.fid ^ ": illegal actual assignment"))
	   in let label = match (List.hd out_list) with
	      Id(i) -> i 
	    | Barray(bs, _, e1) ->  (bs.name) ^ (array_label e1)
	    | Subbus(bs, strt, stop) -> bs.name ^ "_" ^ (string_of_int strt) ^ "_" ^ (string_of_int stop)
	    | x->  raise (Failure ("In/Output port mapping must use pre-existing variables " ))
	   in  let s = str ^ fdecl.fid ^ "_" ^ label ^ " : " ^ fdecl.fid ^ " port map (\n\t\tclk => clk,\n \t\trst => rst" 
	   	   
	    in let s,_,_ = List.fold_left f (s,(List.rev in_list),asn_map) fdecl.pin
	    in let s,_,asn_map = List.fold_left f (s,(List.rev out_list),asn_map) fdecl.pout 
	    in ({sens_list=env.sens_list;}, s ^ ");\n\n",asn_map,cc) )
    and translate_case left (env,s,asn_map,cc) (e,stmt) = 
			(*Check there is no POS*)
			let _ = (match stmt with
					Block(sl) -> let chk_if_pos = (function
							  Pos(_) -> raise (Error("Pos inside switch statement"))
							| _ -> "" )
			     			 in let _ = (List.map chk_if_pos sl) in ""
					| Pos(_) -> raise (Error("Pos inside switch statement"))
			   		| _ -> "" )
      in ( match e with 
     (* SAST needs to check there is atmost one dafault and no duplicate 
     case expressions *) 
          Noexpr->   translate_stmt (env,s ^ "\t\telse \n",asn_map,cc) stmt   
        | x     ->    let right,env,asn_map = eval e env asn_map cc
         in translate_stmt (env,s ^ "\t\telsif (" ^ left ^ " = " ^ right ^ ") then \n",asn_map,cc ) stmt  
         )
(* end of translate_stmt *)          
         

    in let print_process prev (env,s) =  
     let l = uniq env.sens_list in 
     ( match l with   [] -> prev ^ s (* Don't make this a process if nothing in the sensitivity list, affects Call() and consts *)  
                    | x  -> let ss = delim_sprt ", " l
	                in prev ^ "\n\tprocess (" ^ ss ^ ")\n\tbegin\n" ^ s ^ "\n\tend process;\n\n" )	  
   
    in let body cobj asn_map=
    let empty_env = {sens_list=[];}
		in let rec hsa l asn_map cc= function
		   [] -> (List.rev l), asn_map, cc
		 | hd::tl -> let (ts_env, ts_str,new_asn_map, new_cc) = (translate_stmt (empty_env,"",asn_map,cc) hd)
			      in let new_l = (ts_env,ts_str)::l
				in hsa new_l new_asn_map new_cc tl
		in let (stmt_attr, full_asn_map, fc) = hsa [] asn_map 0 cobj.fbod
(*un-comment the three lines below to print out the list of assigned variables*)
	(*in let _ = print_endline ("function "^cname^":")
	in let _ = print_endline ("final clock :"^(string_of_int fc))
	in let _ = print_asn_map full_asn_map*)
	in let s = List.fold_left print_process "" stmt_attr
	in s, fc

   in let arch cname cobj = (*arch *)
    (* Add input ports to assignment map *)
      let pin_asn = List.map (fun b -> Basn(b,Id(b.name))) cobj.pin
	in let asn_map =
	  let rec ha0 asn0 = function
	     [] -> asn0;
	   | hd::tl -> let new_asn0 = update_asn hd asn0
			in ha0 new_asn0 tl
	   in ha0 Im.empty pin_asn

(* body takes Function objetc, assignment map at clock 0 and returns the body string and the final clock*) 
   in let behavior, fc = body cobj asn_map
(* need to print out the locals before begin *)

    (* print out component list *) 
    in let comp_decl s fdecl = 
	    let s1 = port_gen fdecl.fid fdecl     
	     in s ^ "component " ^ fdecl.fid ^  "\nport (\n" ^
		"\tclk : in std_logic;\n\trst : in std_logic;\n" ^ s1 ^ ");\nend component;\n\n" 
	(* if same component is used twice, we just print them once, hence the call to uniq_calls *) 	 
    in let cl_s = List.fold_left comp_decl "" (uniq_calls cobj.fcalls)


	in let rec print_bus bs ss = function
	   0 -> "signal " ^ bs.name ^ "_r0" ^ ss ^ " : std_logic_vector(" ^ (string_of_int (bs.size-1))
	   	^ " downto 0) := ieee.std_logic_arith.conv_std_logic_vector(" ^ string_of_int (bs.init)
	   	^ "," ^ string_of_int (bs.size) ^ ");\n"
	 | x -> print_bus bs (ss ^ ", " ^ bs.name ^ "_r" ^ string_of_int x) (x-1)
	in let rec print_const bs ss = function
	   0 -> "constant " ^ bs.name ^ "_r0" ^ ss ^ " : std_logic_vector(" ^ (string_of_int (bs.size-1))
	   	^ " downto 0) := ieee.std_logic_arith.conv_std_logic_vector(" ^ string_of_int (bs.init)
	   	^ "," ^ string_of_int (bs.size) ^ ");\n"
	 | x -> print_const bs (ss ^ ", " ^ bs.name ^ "_r" ^ string_of_int x) (x-1)
	in let rec print_array bs ss = function
	   0 -> "signal " ^ bs.name ^ "_r0" ^ ss ^ " : " ^ bs.name
		^ "_type := (others => ieee.std_logic_arith.conv_std_logic_vector("
		^ string_of_int (bs.init) ^ "," ^ string_of_int (bs.size) ^ "));\n"
	 | x -> print_array bs (ss ^ ", " ^ bs.name ^ "_r" ^ string_of_int x) (x-1)

	in let print_signals ss var =
	  let (bs, sz, tp, _, _) = var
	  in let ps = (match tp with
	     Bus -> ss ^ (print_bus bs "" fc)
	   | Const -> ss ^ (print_const bs "" fc)
		(*!!PREVENT THE USER TO CALL AN ARRAY <ID>_TYPE. Maybe we want to remove '_' from ID regular expression*)
	   | Array -> let s_type = "type " ^ bs.name ^ "_type is array (0 to " ^ string_of_int (sz-1) ^ ") of std_logic_vector("
		         ^ string_of_int (bs.size-1) ^ " downto 0);\n"
			in ss ^ s_type ^ (print_array bs "" fc)
	   | x -> raise (Failure("There's something wrong with the type symbol table!"))
		)
		in ps
	
	in let const_list = (genv.scope).variables
	in let bus_list = ((cobj.floc).scope).variables
	 in let c_sgnls = List.fold_left print_signals "" const_list
	 in let b_sgnls = List.fold_left print_signals "" bus_list
	 in let sgnls = c_sgnls^b_sgnls
	
	in let print_inasn ss ibus = ss ^ ibus.name ^ "_r0 <= " ^ ibus.name ^ ";\n"
	in let print_outasn ss obus = ss ^ obus.name ^ " <= " ^ obus.name ^ "_r" ^ (string_of_int fc) ^ ";\n"
	in let inasn = List.fold_left print_inasn "" (cobj.pin)
	in let outasn = List.fold_left print_outasn "" (cobj.pout)

    in "architecture e_" ^ cname ^ " of  " ^ cname ^ " is \n\n" ^ cl_s ^"\n\n"^ sgnls ^"\n\nbegin\n"
      ^ inasn ^ outasn ^ "\n" ^ behavior
      ^ "\n\nend e_" ^ cname ^ ";\n\n"

  in let s = libraries ^ (entity cname cobj) ^ (arch cname cobj)
  in CompMap.add cname s components 
  in let components = StringMap.fold create_component ftable CompMap.empty
  in components 

let print_programs outfile components =  
  let s = CompMap.fold ( fun k d s -> s ^ d ) components ""   
    in let out_channel = open_out outfile
     in output_string out_channel s  


let _ =
let usage = "Usage : ehdl [-o outfile] infile1 infile2 ...\n" in 
let _ = if Array.length Sys.argv < 2 then raise (ErrorS ("input files not provided!" ^  usage) ) in 
let j = if Sys.argv.(1) = "-o" then 3 else 1 in 
let outfile =    
    (if Sys.argv.(1) = "-o" then 
        if ( Array.length Sys.argv < 4 ) then raise (ErrorS ("-o should follow an outfile and one or more infiles! \n" ^  usage ^ "\n")  )
        else Sys.argv.(2)
     else "main.vhd") in 
let temp_file = "temp.txt" in      
let temp_out = open_out temp_file in   
let _ =   
for i = j to Array.length Sys.argv - 1 do
    let in_channel =
    try 
         open_in Sys.argv.(i) 
    with e -> raise(ErrorS ("Failed to open " ^ Sys.argv.(i)))
    in
    let rec readfile infile =
        try let line = input_line infile in line ^ "\n" ^ (readfile infile)
        with End_of_file -> "" in
        output_string temp_out ((readfile in_channel) ^ "\n\n") 
done in 
let _ = close_out temp_out in 
let in_channel = open_in temp_file in
let lexbuf = Lexing.from_channel in_channel in
let program = Parser.program Scanner.token lexbuf in
let _  = print_programs outfile ( translate (prog (fst program , snd program) ) ) in
Printf.printf "Find the output in %s ...\n" outfile 
