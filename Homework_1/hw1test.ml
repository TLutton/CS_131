(* These test cases were used in augment of the given sample test cases *)

(* subset test cases *)
let subset_test0 = subset [] [1;2;3]
let subset_test1 = not (subset [1;2;3] []) 
let subset_test2 = subset [1;1;1;1;2;3] [1;2;3]

(* equal_sets test cases *)
let equal_sets_test0 = equal_sets [] []
let equal_sets_test1 = equal_sets [1;2;3] [3;2;1;3]
let equal_sets_test2 = not (equal_sets [] [1])
let equal_sets_test3 = not (equal_sets [1] [2])

(* set_union test cases *)
let set_union_test0 = equal_sets (set_union [] []) []
let set_union_test1 = equal_sets (set_union [1;2;3] []) [1;2;3]
let set_union_test2 = equal_sets (set_union [1;2;3] [3;4;5]) [1;2;3;4;5]
let set_union_test3 = equal_sets (set_union [1;1;1;1] [2;2;2;2]) [1;2]

(* set_intersection test cases *)
let set_intersection_test0 = equal_sets (set_intersection [] [1;2;3]) []
let set_intersection_test1 = equal_sets (set_intersection [1;4] [1;2;3]) [1]
let set_intersection_test2 = equal_sets (set_intersection [1;2;3] [3;4]) [3]
let set_intersection_test3 = 
	equal_sets (set_intersection [1;2;3] [1;2;3]) [1;2;3]

(* set_diff test cases *)
let set_diff_test0 = equal_sets (set_diff [1;2;3] [1;2;3;4;5]) []
let set_diff_test1 = equal_sets (set_diff [1;2;3;4;5] [1;2;3]) [4;5]
let set_diff_test2 = equal_sets (set_diff [1;2;3] []) [1;2;3]
let set_diff_test3 = equal_sets (set_diff [] []) []

(* computed_fixed_point test cases *)
let computed_fixed_point_test0 = 
	computed_fixed_point (=) (fun x -> x / 5) 250 = 0 
let computed_fixed_point_test1 = 
	computed_fixed_point (fun x y -> abs_float (x -. y) < 0.5) 
	(fun x -> x /. 5.) 250. = 0.4 

(* computed_periodic_point test cases *)
let computed_periodic_point_test0 = 
	computed_periodic_point (=) (fun x -> lnot x) 2 0 = 0

(* filter_blind_alleys test cases *)
type my_nonterminals = | Tom | Sam | Sha | Kyl | Kit

let filter_blind_alleys_test0 = 
	filter_blind_alleys (Tom, 
	[Tom, [N Tom];
	Tom, [N Sam];
	Sam, [N Sam];
	Kyl, [T"hi"];
	Tom, [N Kyl];
	Tom, [N Kit];
	Kit, [N Sha];
	Sha, [N Kit]
	]) = 
	(Tom, 
		[Tom, [N Tom];
		Kyl, [T"hi"];
		Tom, [N Kyl]]) 