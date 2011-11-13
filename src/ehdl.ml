open Ast 

module StringMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

let translate (global_consts, funcs) =
let create_component components fdecl  = 
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
   in let rec port_descr_list p inOrOut ports = match ports with     
       [] -> p
       | hd::tl -> let s = "\t" ^ hd.name  
	      ^ " : "  ^ inOrOut ^ " std_logic_vector(" ^ string_of_int(hd.size-1) ^ " downto 0)"
	      in port_descr_list  (s::p) inOrOut tl
		   (* need to check for size 0 and 1*)       

   in let entity  fdecl = (* entity *)
	let inportlist = port_descr_list [] "in" fdecl.portin 
	in let portList =  port_descr_list inportlist "out" fdecl.portout
	  in let s = delim_sprt ";\n" (List.rev portList)       
	     in "entity " ^ fdecl.fname ^ "  is \n\nport (\n" ^ s ^ ");\n\nend main;\n\n"   
	     
  (* Evaluate expressions *) 
 in let rec eval e l = match e with
    Id(i) -> i, i::l (* the list is keeping track of variables for the sensitivity list*) 
   | Binop(e1,op,e2) -> 
     let v1, l = eval e1 l  in let v2, l = eval e2 l in  
     let m = (match op with 
	 Add -> v1 ^ " + " ^ v2
       | x ->  raise (Failure ("Binop not supported yet " ))) in m, l
   | Basn(i, e1) -> let v1, l = eval e1 l in  (i ^ " <= " ^ v1 ^ ";" ) , l
   | x ->  raise (Failure ("Expression not supported yet " ))
      
 (* statements *)
 in let rec translate_stmt (l,str) stmt = 
      match stmt with  
	  Block(stmts)  -> List.fold_left translate_stmt (l,str) stmts 
	| Expr(e) -> let s,l = eval e l  in (l, (str ^ s))   
	| x -> 	raise (Failure ("Statement not supported yet " )) 

  (* translates the body of a function *) 
  in let body fdecl = 
       (* need to print out the locals here *) 
       let (l,s) = List.fold_left translate_stmt ([],"") (snd fdecl.body)
       in let ss = delim_sprt ", " (List.rev l)
       in "\tprocess (" ^ ss ^ ")\n\tbegin\n\t\t" ^ s ^ "\n\tend process;"   	    

   in let arch fdecl = (*arch *)
      "architecture e_" ^ fdecl.fname ^ " of  " ^ fdecl.fname ^ " is \n\nbegin\n\n"
      ^ body fdecl 
      ^ "\n\nend e_" ^ fdecl.fname ^ ";\n\n" 

  in let s = libraries ^ (entity fdecl) ^ (arch fdecl)  
  in StringMap.add fdecl.fname s components 
  in let components = List.fold_left  create_component  StringMap.empty funcs
  in components 

let print_programs components = 
  let s = StringMap.find "main" components 
  in let out_channel = open_out "main.vhdl" in  (* need to generalize this later*)
  output_string out_channel s  


let _ =
let in_channel = open_in Sys.argv.(1) in
let lexbuf = Lexing.from_channel in_channel in
let program = Parser.program Scanner.token lexbuf in
print_programs ( translate program ) 
