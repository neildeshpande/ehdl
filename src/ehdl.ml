open Ast
open Sast

module CompMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)


type s_env = {
    sens_list : string list;  
           } (* can add more stuff to this, list of locals for example, so POS can update it *) 


(* insert non-duplicates *) 
let rec insert_uniq l name =
	try let _ = List.find ( fun ( s ) -> s = name ) l in l  
	with Not_found-> name::l
(* returns a list whose vals are unique *) 	
let uniq lst = 
    List.fold_left (fun l s -> insert_uniq l s) [] lst    	
    
    
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
	  in delim_sprt ";\n" (List.rev portList)       
	     
	     
	    
(*Auxiliary function: adds conv_std_logic_vector *)
let num_to_slv v size =
   try let _ = int_of_string v
	in "ieee.std_logic_arith.conv_std_logic_vector("^v^","^(string_of_int size)^")"
   with Failure(s) -> v	     
	     

let translate (genv, ftable) =
let create_component cname cobj components = 
  let libraries = "\nlibrary ieee;\n" ^ 
    "use ieee.std_logic_1164.all;\n" ^
    "use ieee.std_logic_signed.all;\n\n\n"

   in let entity cname cobj = (* entity *) 
	    let s = port_gen cname cobj     
	     in "entity " ^ cname ^ "  is \n\nport (\n" ^
		"\tclk : in std_logic;\n\trst : in std_logic;\n" ^ s ^ ");\n\nend " ^ cname ^ ";\n\n"   
	     
   
(* Evaluate expressions *) 
 in let rec eval e env = match e with
    Num(i) -> string_of_int i, env 
    | Id(i) -> i, {sens_list = i::env.sens_list;} (* the list is keeping track of variables for the sensitivity list*) 
    | Barray(bs, idx, _) -> bs.name ^ "[" ^ (string_of_int idx) ^ "]" (* will need to consider if we let the programmar use expr at all. Right now it is using the size as index, which is inaccurate *), {sens_list = bs.name::env.sens_list;} (* right now using "a" rather than "a[i]" in the sensitivity list *)  
    | Subbus(bs, strt, stop) -> let range = 
		   if strt < stop then "(" ^ (string_of_int strt) ^ " to " ^ (string_of_int stop) ^ ")" else
			"(" ^ (string_of_int strt) ^ " downto " ^ (string_of_int stop) ^ ")"
		in bs.name ^ range, 
	{sens_list =  bs.name::env.sens_list;}      
    | Unop(op,e1) -> let v1, env = eval e1 env in 
    ( match op with 
      Umin -> "- " ^ v1  
    | Not -> "not " ^ v1
    | x -> raise (Failure ("ERROR: Invalid Unary Operator ")) ), env 
    | Binop(e1,op,e2) -> 
     let v1, env = eval e1 env  in let v2, env = eval e2 env 
     in (match op with 
	 Add  -> v1 ^ " + " ^ v2
       | Sub  -> v1 ^ " - " ^ v2 
       | Mul  -> v1 ^ " * " ^ v2  
       | Div  -> v1 ^ " / " ^ v2   
       | Mod  -> v1 ^ "  mod " ^ v2 
       | Lt   -> v1 ^ " < " ^ v2
       | Gt   -> v1 ^ " > " ^ v2
       | Lte  -> v1 ^ " <= " ^ v2
       | Gte  -> v1 ^ " >= " ^ v2
       | Eq   -> v1 ^ " = " ^ v2
       | Neq  -> v1 ^ " /= " ^ v2
       | Or   -> v1 ^ " or " ^ v2
       | And  -> v1 ^ " and " ^ v2
       | Xor  -> v1 ^ " xor  " ^ v2
       | Shl  -> v1 ^ " sll " ^ v2 (*check that v2 is an integer,here or parser ? *)
       | Shr  -> v1 ^ " srl " ^ v2 
       | x    -> raise (Failure ("ERROR: Invalid Binary Operator "))), env
   | Basn(i, e1) -> let v1, env = eval e1 env
		in  let slv_v1 = num_to_slv v1 i.size
		  in ("\t\t" ^ i.name ^ " <= " ^ slv_v1 ^ ";\n" ) , env
   | Subasn(i, strt, stop, e1) -> let v1, env = eval e1 env
		in let slv_v1 = num_to_slv v1 ((abs (strt - stop)+1))
		  in let range = 
		   if strt < stop then "(" ^ (string_of_int strt) ^ " to " ^ (string_of_int stop) ^ ")" else
			"(" ^ (string_of_int strt) ^ " downto " ^ (string_of_int stop) ^ ")"
		in ("\t\t" ^ i.name ^ range ^ " <= " ^ slv_v1 ^ ";\n" ) , env
   | Aasn(i,sz,e1,e2) -> let v1, env = eval e1 env (* may want to restrict what e1 can be, I say just make num or const *)
             in let v2, env = eval e2 env 
		     in  let slv_v2 = num_to_slv v2 i.size
		     in ("\t\t" ^ i.name ^ "[" ^ v1 ^ "] " ^ " <= " ^ slv_v2 ^ ";\n" ) , env    		  
   | x ->  raise (Failure ("Expression not supported yet " ))

 (* translate_Stmt *)
 in let rec translate_stmt (env,str) stmt = 
    (  match stmt with  
	  Block(stmts)  -> List.fold_left translate_stmt (env,str) (List.rev stmts) 
	| Expr(ex) -> let (e, ex_t, ex_s) = ex
	    in let s,env = eval e env  in (env, (str ^ s))   
	| If(e,if_stmt,else_stmt) -> 
	    let s,env = eval e env 
	    in let env,if_block = translate_stmt(env,"") if_stmt
	    in let env,else_block = translate_stmt(env,"") else_stmt
	    in env, ("\t\tif (" ^ s ^ ") then \n" ^ if_block 
	    (* the tabbing needs to be done programmatically, not manually. 
	    I am assuming SAST will tell us the nesting depth *)    
	    ^ "\n\t\telse\n" ^ else_block ^ "\t\tend if;\n") 
	| Switch ( e, c_list ) -> 
	    ( match c_list with 
	      [] -> env,""
	     |hd::tl ->       
	     let s,env = eval e env 
           in let (e1, stmt) = hd
           in let s1,env = eval e env in let s2,env = eval e1 env    
           in let s3 = "\t\tif (" ^ s1 ^ " = " ^ s2 ^ ") then \n"  
           in let env,if_block = translate_stmt (env,"") stmt
		   in let env,s5 = List.fold_left (translate_case s1) (env,"") tl 	
		   in env, (s3 ^ if_block ^ s5 ^ "\t\tend if;\n") ) 		      
	| Pos(s2) -> raise (Failure ("Pos not supported yet " ))    
	| Call(fdecl, out_list, in_list ) ->
	   (* start of f *) 
	   let f (s,l) b = 
	    let s1 = (match (List.hd l) with Id(i) -> i 
	   | Barray(bs, idx, _) -> bs.name ^ "[" ^ (string_of_int idx) ^ "]"(* TODO *)
	   | Subbus(bs, strt, stop) -> bs.name ^ "(" ^ (string_of_int stop) ^ " downto " ^ (string_of_int strt) ^ ")"
	   | x ->  raise (Failure ("In/Output port mapping must use pre-existing variables " ))   ) 
	   in  s^",\n\t\t"^b.name^" => " ^ s1 , List.tl l   (* end of f *) 
	   
	   (* When a function uses the same component multiple times, it needs to use unique labels to describe the 
	   separate instantiations. One way to do this is to append a string that is a function of the head of the 
	   output_list. The output_list is guranteed to be non-empty, SAST should also gurantee that the same output 
	   variable does not get used in two different calls as outputs. *) 
	   in let label = (match (List.hd out_list) with Id(i) -> i 
	    |  Barray(bs, idx, _) ->  bs.name (*TODO*)  
	    | Subbus(bs, strt, stop) -> bs.name ^ "_" ^ (string_of_int strt) ^ "_" ^ (string_of_int stop)
	    | x->  raise (Failure ("In/Output port mapping must use pre-existing variables " )) ) 
	   in  let s = str ^ fdecl.fid ^ "_" ^ label ^ " : " ^ fdecl.fid ^ " port map (\n\t\tclk => clk,\n \t\trst => rst" 
	   	   
	    in let s,_ = List.fold_left f (s,in_list) fdecl.pin
	    in let s,_ = List.fold_left f (s,out_list) fdecl.pout 
	    in {sens_list=env.sens_list;}, s ^ ");\n"     
	| x -> 	raise (Failure ("Statement not supported yet " )) )
    and translate_case left (env,s) (e,stmt) = 
      ( match e with 
     (* SAST needs to check there is atmost one dafault and no duplicate 
     case expressions *) 
          Noexpr->   translate_stmt (env,s ^ "\t\telse \n") stmt   
        | x     ->    let right,env = eval e env 
         in translate_stmt (env,s ^ "\t\telsif (" ^ left ^ " = " ^ right ^ ") then \n" ) stmt  
         )
(* end of translate_stmt *)          
         

    in let print_process prev (env,s) =  
     let l = uniq env.sens_list in 
     ( match l with   [] -> prev ^ s (* Don't make this a process if nothing in the sensitivity list, affects Call() and consts *)  
                    | x  -> let ss = delim_sprt ", " l
	                in prev ^ "\n\tprocess (" ^ ss ^ ")\n\tbegin\n" ^ s ^ "\n\tend process;\n" )	  
   
    in let body cobj = 
    let empty_env = {sens_list=[];} 
	in let stmt_attr = List.map (translate_stmt (empty_env,"")) (cobj.fbod : Sast.s_stmt list)
	in let s = List.fold_left print_process "" stmt_attr
	in s

   in let arch cname cobj = (*arch *)
(* need to evaluate the body before printing out the locals, because pos implies new signals! *)
    let behavior = body cobj 
(* need to print out the locals nefore begin *)

    (* print out component list *) 
    in let comp_decl s fdecl = 
	    let s1 = port_gen fdecl.fid fdecl     
	     in s ^ "component " ^ fdecl.fid ^  "\nport (\n" ^
		"\tclk : in std_logic;\n\trst : in std_logic;\n" ^ s1 ^ ");\nend component;\n\n" 
	(* if same component is used twice, we just print them once, hence the call to uniq_calls *) 	 
    in let cl_s = List.fold_left comp_decl "" (uniq_calls cobj.fcalls)    
    in "architecture e_" ^ cname ^ " of  " ^ cname ^ " is \n\n" ^ cl_s ^ "\n\nbegin\n"
      ^ behavior
      ^ "\n\nend e_" ^ cname ^ ";\n\n"

  in let s = libraries ^ (entity cname cobj) ^ (arch cname cobj)
  in CompMap.add cname s components 
  in let components = StringMap.fold create_component ftable CompMap.empty
  in components 

let print_programs filename components =
  (* need to iterate over all components, not just main, 
     maybe components should be a list *)
  (* let s = CompMap.find "main" components *) 
  let s = CompMap.fold ( fun k d s -> s ^ d ) components ""   
  (*in let out_channel = open_out (filename ^ ".vhd" ) *) 
    in let out_channel = open_out "main.vhd"
     in output_string out_channel s  


let _ =
let in_channel = open_in Sys.argv.(1) in
let lexbuf = Lexing.from_channel in_channel in
let program = Parser.program Scanner.token lexbuf in
print_programs Sys.argv.(1) ( translate (prog (fst program , snd program) ) ) 
