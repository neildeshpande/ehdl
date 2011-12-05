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
let update_asn (asn_expr : Sast.expr_detail) asn_map =
	let vname = match asn_expr with
	  Basn(x,_) -> x.name
	| Aasn(x,_,_,_) -> x.name
	| Subasn(x,_,_,_) -> x.name
	| x -> raise (Error("not an assignment"))
	in Im.add asn_expr vname asn_map

(*Debug: check assignments - this is a kind of int. representation*)
let asn_to_string k _ s = 
	let s = s^ ""
	in let _ = match k with
	Basn(x,_) -> print_endline (x.name^" -> assigned")
	| Aasn(x,i,_,_) -> print_endline ((x.name)^" "^(string_of_int i)^" -> assigned")
	| Subasn(x,a,b,_) -> print_endline ((x.name)^" range "^(string_of_int a)^":"^(string_of_int b)^" -> assigned")
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


(*Auxiliary functions*)
(* insert non-duplicates *) 
let rec insert_uniq l name =
	try let _ = List.find ( fun ( s ) -> s = name ) l in l  
	with Not_found-> name::l
(* returns a list whose vals are unique *) 	
let uniq lst = 
    List.fold_left (fun l s -> insert_uniq l s) [] lst


let range_list bl (a,b) = if a >= b then (a,b)::bl else (b,a)::bl

let others_range_list (a0,b0) l = 
	let (a0,b0) = if a0 >= b0 then (a0, b0) else (b0,a0)
	in let l = List.rev ( List.sort (fun (c1,_) (c2,_) -> Pervasives.compare c1 c2) l )
	in let add_range r (c1,c2) = if( (a0 > c1) && (b0 > c1) )then (a0,b0)::r else
				   if( (a0 > c1) && (b0 >= c2) )then (a0,(c1+1))::r else
				   if( (a0 > c1) && (b0 < c2) )then (a0,(c1+1))::(((c2-1),b0)::r) else
				   if( (a0 >= c2) && (b0 < c2) )then  ((c2-1),b0)::r else
				     (a0,b0)::r 
	in let tmp = List.fold_left add_range ([]) l
	in let res = uniq tmp in res

(*Get all assignments to vname*)
let get_asn_to_vname asn v (vname,l) = vname, (if v = vname then asn::l else l)

(*Get assignments from map1 not reported in map2*)
let get_nc_asn map1 map2 = 
	let list_nc_asn asn vname l =
	   if Im.mem asn map2 then l else
	   let _, tmp_asn_l = Im.fold get_asn_to_vname map2 (vname,[])
	   in let nca ncal = function
		  [] -> asn::ncal
		| hd::tl -> (match hd with
				  Basn(x,e1) -> ncal (*if bus -> single assignment!*)
				(*| Aasn(x,sz,e1,e2) ->*)
				| Subasn(x,a,b,e1) -> (match asn with
							Subasn(_,a0,b0,_) ->
							let bitl = range_list [] (a,b)
							in let rec fill_bitl bl = (function
								  [] -> bl
								| hd1::tl1 -> (match hd1 with 
										Subasn(_,a1,b1,_) -> fill_bitl (range_list bl (a1,b1)) tl1
										| _ -> raise (Error("Wrong list in get_nc_asn"))	)	)
							  in let bitl = fill_bitl bitl tl
							in let subasn_list = 
							 let subasn_from_range l (a2,b2) = (Subasn(x,a2,b2,e1))::l
							 in let orl = others_range_list (a0,b0) bitl
							 in List.fold_left subasn_from_range ncal orl
							in subasn_list
							| _ -> [asn] )
				| _ -> raise (Error("not an assignment"))	)
		in nca l tmp_asn_l
	in let nc_list = Im.fold list_nc_asn map1 []
	in nc_list
