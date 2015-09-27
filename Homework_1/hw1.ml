(** returns true iff item 'a' is found in list 'b' **)
let rec is_a_in_b a b = match b with
	| [] -> false
	| h::t -> if h = a then true else is_a_in_b a t ;;

(** returns true iff list 'a' is a subset of list 'b' **)
let rec subset a b = match a with
	| [] -> true
	| h::t -> if is_a_in_b h b then subset t b else false ;;


(** returns true iff the represented sets are equal **)
let equal_sets a b =
	if (subset a b) = (subset b a) then true else false ;;

(** returns a list representing the union of 'a' and 'b'. avoids adding duplicates from a to b. **)
let rec set_union a b = match a with
	| [] -> b
	| h::t -> if is_a_in_b h b then (set_union t b) else set_union t (b @ [h]) ;;


(** returns a list representing the intersection of 'a' and 'b' **)
let set_intersection a b = 
	let rec aux first inter = match first with
		| [] -> inter
		| h::t -> if (is_a_in_b h b) then (aux t (inter @ [h])) else (aux t inter)
	in aux a [] ;;


(** returns a list representing a-b, the set of all members of a that are not 
also members of b. **)
let set_diff a b = 
	let rec aux first diff = match first with
		| [] -> diff
		| h::t -> if (is_a_in_b h b) then (aux t diff) else (aux t (diff @[h]))
	in aux a [] ;;