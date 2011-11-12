open Ast 

module FileMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

let translate (global_consts, funcs) =
let create_component components fdecl  = 
  let libraries = "library ieee;\n" ^ 
    "use ieee.std_logic_1164.all;\n" ^
    "use ieee.std_logic_arith.all;\n" ^
    "use ieee.std_logic_signed.all;\n\n\n"
  in 
  let entity  fdecl = 
       let ports inOut p = 
	    List.fold_left (fun  str bus -> str ^ "\t" ^ bus.name  
	      ^ " :  " ^ inOut ^ " std_logic_vector(" ^ string_of_int(bus.size-1) ^ " downto 0);\n" )  
	      "" p
	      (* need to check for size 0*)       
       in "entity " ^ fdecl.fname ^ "  is \n\nport (\n" 
           ^ (ports "in" fdecl.portin) ^ (ports "out" fdecl.portout) ^ "\t);\nend main;"   
  in 
  let s = libraries ^ (entity fdecl )
  in FileMap.add fdecl.fname s components 
  in let components = List.fold_left  create_component  FileMap.empty funcs
  in components 

let print_programs components = 
  let s = FileMap.find "main" components 
  in let out_channel = open_out "main.vhdl" in  
  output_string out_channel s  


let _ =
let in_channel = open_in Sys.argv.(1) in
let lexbuf = Lexing.from_channel in_channel in
let program = Parser.program Scanner.token lexbuf in
print_programs ( translate program ) 
