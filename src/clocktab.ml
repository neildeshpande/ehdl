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

(*Debug: check assignments - this is a kind of int. representation*)
let asn_to_string k cc s = 
	let s = s^ ""
	in let _ = match k with
	Basn(x,_) -> print_endline (x.name^" -> clock cycle: "^(string_of_int cc))
	| Aasn(x,i,_,_) -> print_endline ((x.name)^" "^(string_of_int i)^" -> clock cycle: "^string_of_int cc)
	| Subasn(x,a,b,_) -> print_endline ((x.name)^" range "^(string_of_int a)^":"^(string_of_int b)^" -> clock cycle: "^string_of_int cc)
	| x -> raise (Error("not an assignment"))
	in s

let print_asn_map asn_map = Im.fold asn_to_string asn_map ""

(*CHECK ASYNC!*)
let asn_map_to_list k _ (sync,async) = match k with
	  Basn(x,e1) -> if x.async then (sync,(Basn(x,e1))::async) else ((Basn(x,e1))::sync,async)
	| Aasn(x,sz,e1,e2) -> if x.async then (sync,(Aasn(x,sz,e1,e2))::async) else ((Aasn(x,sz,e1,e2))::sync,async)
	| Subasn(x,a,b,e1) -> if x.async then (sync,(Subasn(x,a,b,e1))::async) else ((Subasn(x,a,b,e1))::sync,async)
	| x -> raise (Error("not an assignment"))
let get_asn asn_map = Im.fold asn_map_to_list asn_map ([],[])








