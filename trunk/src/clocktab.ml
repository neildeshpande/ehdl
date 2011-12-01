open Ast
open Sast

module Im = Map.Make(struct
  type t = Sast.expr_detail
  let compare x y = Pervasives.compare x y
end)

module Sm = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

(*Call this function when an assignment occurs*)
(*takes clock cycle #, assignment, current assignment map and updates it*)
let update_asn (asn_expr : Sast.expr_detail) (cc : int) asn_map = Im.add asn_expr cc asn_map

let asn_to_string k cc s = 
	let s = s^ ""
	in let _ = match k with
	Basn(x,_) -> print_endline (x.name^" -> clock cycle: "^(string_of_int cc))
	| Aasn(x,i,_,_) -> print_endline ((x.name)^" "^(string_of_int i)^" -> clock cycle: "^string_of_int cc)
	| Subasn(x,a,b,_) -> print_endline ((x.name)^" range "^(string_of_int a)^":"^(string_of_int b)^" -> clock cycle: "^string_of_int cc)
	| x -> raise (Error("not an assignment"))
	in s

let print_asn_map asn_map = Im.fold asn_to_string asn_map ""

(*Call this function when a reference to id or array occures*)
(*takes ehdl variable name, current swap map and returns the current vhdl signal name*)
let get_current_name vname swap_map = Sm.find vname swap_map

(*Call this function when a pos statement is processed*)
(*takes ehdl variable name, current clock cycle, current swap map and updates it*)
let update_swap vname (cc : int) swap_map = Sm.add vname (vname^"_r"^(string_of_int cc)) swap_map
