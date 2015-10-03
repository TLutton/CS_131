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
	if (subset a b) && (subset b a) then true else false ;;

(** returns a list representing the union of 'a' and 'b'. avoids adding 
	duplicates from a to b. **)
let rec set_union a b = match a with
	| [] -> b
	| h::t -> if is_a_in_b h b then (set_union t b) 
			else set_union t (b @ [h]) ;;

(** returns a list representing the intersection of 'a' and 'b' **)
let set_intersection a b = 
	let rec aux first inter = match first with
		| [] -> inter
		| h::t -> if (is_a_in_b h b) 
					then (aux t (inter @ [h])) 
				else (aux t inter)
	in aux a [] ;;

(** returns a list representing a-b, the set of all members of a that are not 
also members of b. **)
let set_diff a b = 
	let rec aux first diff = match first with
		| [] -> diff
		| h::t -> if (is_a_in_b h b) then (aux t diff) else (aux t (diff @[h]))
	in aux a [] ;;

(** returns the computed fixed point for 'f' w.r.t. 'x'

	@Param eq : the equality predicate for f's domain. i.e. (=), the builtin 
	equality predicate

	If there is no computed fixed point, the function's behavior is undefined 
	per specification. **)
let computed_fixed_point eq f x =  
	let rec aux y z = 
		if (eq y z) then y else aux z (f z)
	in aux x (f x);;

(** returns the computed periodic point for f with period p w.r.t. x

	@Param eq : the equality predicate for f's domain **)
let rec computed_periodic_point eq f p x = 
	let rec aux q y = 
		if q = p then (if (eq y x) then x 
						else computed_periodic_point eq f p (f x) )
	else aux (q+1) (f y)
in aux 0 x ;;


(** Filter Blind Alleys Shenanigans **)

(** Algorithm:

	1.	Find all N's that evaluate directly to T's. Add those N's to a 'safe' 
		list
	2.	Find all N's that indirectly evaluate to T's. 
			A. Look for N's that evaluate to N's already in 'safe' list
			B. Will need to loop several times removing N's that were already
				placed into 'safe' list s
	3.  Remove all rules that either start with or point to nonterminals not in 
		the 'safe' list. 
**)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal  ;;

(** This function takes in a symbol and returns true if the symbol is a 
	terminal. false otherwise *)
let is_terminal x = match x with
	| T _ -> true
	| _ -> false ;;

(** This function takes in a list of nonterminals that currently lead either 
	directly or indirectly to terminals and a list symbols to evaluate. 

	Returns true if all nonterminals in symbols in 2nd input list are found in 
	l.

*)
let rec is_each_symbol_ind_terminal l = function
	| [] -> true
	| h::t -> if ( (is_terminal h) || (is_a_in_b (match h with N x -> x) l) )
				then (is_each_symbol_ind_terminal l t)
				else false ;;

(** This function takes in a list of nonterminals to o 
	and a list of (nonterminal * symbol list) list to match. 

	Returns a list of nonterminals that can indirectly reach a terminal. 

Goal is that every nonterminal that is NOT in the return list constitutes a 
blind alley 

Thought: This function should be called until N's are no longer added *)
let rec get_Ns_with_ind_terminals safe = function 
	| [] -> safe
	| h::t -> if ( is_each_symbol_ind_terminal safe (snd h) )
				then ( if (is_a_in_b (fst h) safe)
						then (get_Ns_with_ind_terminals safe t)
						else (get_Ns_with_ind_terminals (safe @ [(fst h)]) t))
				else (get_Ns_with_ind_terminals safe t) ;;

(** ^^^ Should do something about adding duplicates to the list ^^^  **)



(** Function that loops get_Ns_with_ind_terminals and stops when safe is no 
	longer being modified 

	@Parameter: safe = safe list, match/function list = list of symbol lists, *)

let rec loop s l = match l with
	| [] -> s
	| h::t -> ( loop (get_Ns_with_ind_terminals s [h]) t );;

(** Function calls itself @Param: safe and (loop safe llist) are not equal
	This means that there were Ns found that can reach Ts either directly
	or indirectly. We should keep looping until no more Ns can reach Ts  *)
let rec call_get_Ns safe l = match (loop safe l) with
	| h::t -> if (equal_sets (h::t) safe) 
				then safe 
			else (call_get_Ns (h::t) l) 
	| [] -> if (equal_sets safe [])
				then safe
			else (call_get_Ns [] l);;

(** Get the set of rules that lead to terminals. Then remove all items from the 
	grammar that aren't nonterminals in this list.

	@Param g = rhs of grammar 
	@Param s = list of nonterminals that are NOT blind alleys
	@Param r = list that will be returned as rhs of grammar w/ no blind alleys

	what to check LHS and RHS to make sure that all Ns are in the safe list. 

*)
let rec shrink_grammar g s r = match g with
	| [] -> r
	| h::t -> if (is_a_in_b (fst h) s) && is_each_symbol_ind_terminal s (snd h)
				then ( shrink_grammar t s (r @ [h]) )
				else ( shrink_grammar t s r)
	;;

(** returns a copy of the grammar g with all blind-alley rules removed while 
	preserving the original order of rules found in g.  

	Create a list of nonterminals that are NOT blind alley Ns. Then look 
	through snd g and find all instances of Ns not in the list. 
**)
let filter_blind_alleys g = 
	( (fst g), 
		( shrink_grammar (snd g) (call_get_Ns [] (snd g)) []) )
;;
	

	





