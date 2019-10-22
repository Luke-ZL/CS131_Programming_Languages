open List;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
  ;;

(*task 1 subset*)
let rec subset a b = match a with
  | [] -> true
  | e::a2 -> if (mem e b) && (subset a2 b) then true
             else false
  ;;

(*task 2 equal_sets*)
let rec equal_sets a b =
  if (subset a b) && (subset b a) then true
  else false
  ;;

(*task 3 set_union*)
let rec set_union a b = match a with
  | [] -> b
  | e::a2 -> if (mem e b) then (set_union a2 b)
             else e::(set_union a2 b)
  ;;

(*task 4 set_intersection*)
let rec set_intersection a b = match a with
  | [] -> []
  | e::a2 -> if (mem e b) then e::(set_intersection a2 b)
             else (set_intersection a2 b)
  ;;

(*task 5 set_diff*)
let rec set_diff a b = match a with
  | [] -> []
  | e::a2 -> if (mem e b) then (set_diff a2 b)
             else e::(set_diff a2 b)
  ;;

(*task 6 computed_fixed_point*)
let rec computed_fixed_point eq f x  =
  let x2 = (f x) in
  if (eq x2 x) then x
  else (computed_fixed_point eq f x2) 
  ;;

(*task 7*)
(*find if a single element is a nonterminal*)
let check_non a = match a with
  | (N sym) -> true
  | (T sym) -> false
  ;;

(*filter out all terminals in the rules*)
let rec filter_list l = match l with
  | [] -> []
  | e::rest -> if (check_non e) then e::(filter_list rest)
               else (filter_list rest)
  ;;

let rec filter_rules rules = match rules with
  | [] -> []
  | e::rest -> if ((filter_list (snd e)) = []) then (filter_rules rest)
               else ((fst e),(filter_list (snd e)))::(filter_rules rest)
  ;;

(*get all nonterminals in a list*)
let rec get_all_non l = match l with
  | [] -> []
  | e::rest -> (set_union  [e] (get_all_non rest))
  ;;

(*find all new rhs nonterminals*)
let rec find_rhs_new non rules = match rules with
  | [] -> []
  | e::rest -> if ((N (fst e)) = non) then (set_union (get_all_non (snd e)) (find_rhs_new non rest))
               else (find_rhs_new non rest)
  ;;

(*derive new possible left-hand-side nonterminals*)
let rec derive_new known_nonterminals new_nonterminals rules  = match new_nonterminals with
  | [] -> []
  | e::rest -> (set_diff (set_union (find_rhs_new e rules) (derive_new known_nonterminals rest rules)) known_nonterminals)
  ;;

(*get a list of reachable nonterminals*)
let rec reachable_non_terminals known_nonterminals new_nonterminals rules =
  let new_non = (derive_new known_nonterminals new_nonterminals rules) in
  if (new_non = []) then (set_union known_nonterminals new_nonterminals)
  else (reachable_non_terminals (set_union known_nonterminals new_nonterminals) new_non rules)
  ;;

(*with a list of reachable nonterminals, get all the reachable rules*)
let rec get_rules l rules = match rules with
  | [] -> []
  | e::rest -> if (mem (N (fst e)) l) then e::(get_rules l rest)
               else (get_rules l rest)

(*actual task 7 function*)
let filter_reachable g =
  let l_non = reachable_non_terminals [] [(N (fst g))] (filter_rules (snd g)) in
  ((fst g),(get_rules l_non (snd g)))
  ;;
