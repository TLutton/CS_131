
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal  ;;



(* convert_grammar returns a Homework-2 style grammar which is converted
	from a homework-1 style grammar @Param: gram1.  

	For homework-2, a grammar is defined as:
		* a start symbol (a nonterminal)
		* and a production function  *)

let rec cg_aux n = function
	| [] -> []
	| (h_n, h_rhs)::t -> if (h_n = n) then h_rhs::(cg_aux n t)
							else (cg_aux n t) ;;
							
let convert_grammar gram1 = ( (fst gram1),
								function n -> (cg_aux n (snd gram1) ) ) ;;

(* and matcher that takes in evaluation rule, full list of rules, acceptor, 
	deriv, and frag *)
let rec and_matcher eval rules accept deriv frag = match eval with
	| [] -> (accept deriv frag)
	| h::t -> if ( frag = [] ) then None (* frag is empty but rule is not filled *)
		else ( match frag with | h_frag::t_frag -> 
				( match h with 
					| T sym -> 
						if sym = h_frag
						then ( and_matcher t rules accept deriv t_frag ) (* match rest *)
						else None 
					(* recursively start or matching again on nonterminal *)
					| N sym -> or_matcher sym rules (and_matcher t rules accept) deriv frag (rules sym) 
				) 
			)
(* or matcher that takes the start symbol, list of rules, acceptor, deriv,
	fragment, and a rule for evaluation *)
and or_matcher start rules acceptor deriv frag = function
	| [] -> None 
	| h::t -> (
		(* try to match based on left most rule. If it fails, try next rule. If success, return *)
		match (and_matcher h rules acceptor (deriv@[(start, h)]) frag) with
			| None -> (or_matcher start rules acceptor deriv frag t)
			| retval -> retval
		) ;;


(* parse_prefix takes a grammar, acceptor, and fragment to parse and returns a parse tree *)
let parse_prefix gram acceptor frag = 
	(or_matcher (fst gram) (snd gram) acceptor [] frag ((snd gram) (fst gram)) );;