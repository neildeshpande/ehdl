open Ast
open Sast

module CompMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

let translate (genv, ftable) =
let create_component cname cobj components = 
  let libraries = "\nlibrary ieee;\n" ^ 
    "use ieee.std_logic_1164.all;\n" ^
    "use ieee.std_logic_arith.all;\n" ^
    "use ieee.std_logic_signed.all;\n\n\n"

  (* Utility function for producing a delimeter separated string from a list*) 
  in let rec delim_sprt delim p = match List.length p with 
       0 -> "" | 
       x -> let head = (List.hd p)
	      (* don't want to put in a delim after the last element, hence the if check *) 	 
	     in if ( x = 1 ) then head else  (head ^ delim ^ (delim_sprt delim (List.tl p)) ) 	   
   
   (* helper for building the vhdl style port list string *) 		 
     in let rec port_descr_list p inOrOut (ports: Ast.bus list) = match ports with
       [] -> p
       | hd::tl -> let typedescr = 
		     (match hd.size with 
			0 -> raise (Failure ("bus size cannot be zero " ))
			(* not doing std_logic because then we have to convert 1 to '1' *)
		  | x -> " std_logic_vector(" ^ string_of_int(hd.size-1) ^ " downto 0)" ) 
		   in  let s = "\t" ^ hd.name ^ " : "  ^ inOrOut ^ typedescr 
		       in port_descr_list  (s::p) inOrOut tl
   

   in let entity cname cobj = (* entity *) 
	let inportlist = port_descr_list [] "in " cobj.pin
	in let portList =  port_descr_list inportlist "out" cobj.pout
	  in let s = delim_sprt ";\n" (List.rev portList)       
	     in "entity " ^ cname ^ "  is \n\nport (\n" ^
		"\tclk : in std_logic;\n\trst : in std_logic;\n" ^ s ^ ");\n\nend main;\n\n"   
	     
(*  (* Evaluate expressions *) 
 in let rec eval e l = match e with
    Num(i) -> string_of_int i, l 
    | Id(i) -> i, i::l (* the list is keeping track of variables for the sensitivity list*) 
    | Unop(op,e1) -> let v1, l = eval e1 l in 
    ( match op with 
      Umin -> "- " ^ v1  
    | Not -> "not " ^ v1
    | x -> raise (Failure ("Unary operator not supported ")) ), l 
    | Binop(e1,op,e2) -> 
     let v1, l = eval e1 l  in let v2, l = eval e2 l 
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
       | x    -> raise (Failure ("The operator is not a binary operator "))), l
   | Basn(i, e1) -> let v1, l = eval e1 l in  (i ^ " <= " ^ v1 ^ ";\n" ) , l
   | Call(i,e1) ->  raise (Failure ("Call not supported yet " ))
   
   | x ->  raise (Failure ("Expression not supported yet " ))

 (* statements *)
 in let rec translate_stmt (l,str) stmt = 
    (  match stmt with  
	  Block(stmts)  -> List.fold_left translate_stmt (l,str) (List.rev stmts) 
	| Expr(e) -> let s,l = eval e l  in (l, (str ^ s))   
	| If(e,if_stmt,else_stmt) -> 
	    let s,l = eval e l 
	    in let l,if_block = translate_stmt(l,"") if_stmt
	    in let l,else_block = translate_stmt(l,"") else_stmt
	    in l, ("if (" ^ s ^ ") then \n\t\t\t" ^ if_block 
	    (* the tabbing needs to be done programmatically, not manually. 
	    I am assuming SAST will tell us the nesting depth *)    
	    ^ "\n\t\telse\n\t\t\t" ^ else_block ^ "\n\t\tend if;") 
	| Switch ( e, c_list ) -> 
	    ( match c_list with 
	      [] -> l,""
	     |hd::tl ->       
	     let s,l = eval e l 
           in let (e1, stmt) = hd
           in let s1,l = eval e l in let s2,l = eval e1 l    
           in let s3 = "if (" ^ s1 ^ " = " ^ s2 ^ ") then \n\t\t\t"  
           in let l,if_block = translate_stmt (l,"") stmt
		   in let l,s5 = List.fold_left (translate_case s1) (l,"") tl 	
		   in l, (s3 ^ if_block ^ s5 ^ "\n\t\tend if;") ) 		      
	| Pos(s2) -> raise (Failure ("Pos not supported yet " ))     
	| x -> 	raise (Failure ("Statement not supported yet " )) )
    and translate_case left (l,s) (e,stmt) = 
      ( match e with 
     (* SAST needs to check there is atmost one dafault and no duplicate 
     case expressions *) 
          Noexpr->   translate_stmt (l,s ^ "\t\telse \n\t\t\t") stmt   
        | x     ->    let right,l = eval e l 
         in translate_stmt (l,s ^ "\t\telsif (" ^ left ^ " = " ^ right ^ ") then \n\t\t\t" ) stmt  
         )

    in let print_process prev (l,s) =  
	 let ss = delim_sprt ", " (List.rev l)
	 in prev ^ "\n\tprocess (" ^ ss ^ ")\n\tbegin\n\t\t" ^ s ^ "\n\tend process;\n"	  
   
    in let body cobj = 
       (* need to print out the locals here *) 
	let stmt_attr = List.map (translate_stmt ([],"")) (snd cobj.fbod)
	in  List.fold_left print_process "" stmt_attr

   in let arch cname cobj = (*arch *)
      "architecture e_" ^ cname ^ " of  " ^ cname ^ " is \n\nbegin\n"
      ^ body cobj 
      ^ "\n\nend e_" ^ cname ^ ";\n\n" *)

  in let s = libraries ^ (entity cname cobj) (*^ (arch cname cobj)  *)
  in CompMap.add cname s components 
  in let components = StringMap.fold create_component ftable CompMap.empty
  in components 

let print_programs filename components =
  (* need to iterate over all components, not just main, 
     maybe components should be a list *)
  let s = CompMap.find "main" components 
  (* Not essential right now: need to take in a file "adder.ehdl" and spit out "adder.vhdl"*)
  (*in let out_channel = open_out (filename ^ ".vhdl" ) *) 
    in let out_channel = open_out "main.vhdl"
     in output_string out_channel s  


let _ =
let in_channel = open_in Sys.argv.(1) in
let lexbuf = Lexing.from_channel in_channel in
let program = Parser.program Scanner.token lexbuf in
print_programs Sys.argv.(1) ( translate (prog (fst program , snd program) ) ) 
