open Ast

type symbol_table = {
  parent : symbol_table option;
  variables : bus list
}
  
  
let rec find_variable (scope : symbol_table) name =
	try
		List.find (fun (b : Ast.bus) -> b.name = name) scope.variables
	with Not_found ->
		match scope.parent with
			  Some(parent) -> find_variable parent name
			| _ -> raise Not_found

type translation_environment = {
	return_type : Ast.types; (* Function’s return type *)
	scope : symbol_table; (* symbol table for vars *)
}
