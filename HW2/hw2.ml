open List

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(*task 1 convert_grammar, with a helper function find_rules that find all the rules that associate with a non-terminal*)
let rec find_rules non_terminal = function
  | [] -> []
  | e::rest -> if (fst e) = non_terminal then (snd e)::(find_rules non_terminal rest)
               else (find_rules non_terminal rest)

let convert_grammar gram1 =
((fst gram1), fun non_terminal -> find_rules non_terminal (snd gram1))

(*task 2 parse_tree_leaves, with a helper function parse_tree_leaves that parse all sub trees in a node*) 
let rec parse_tree_list = function
  | [] -> []
  | e::rest -> (parse_tree_leaves e)@(parse_tree_list rest)

and parse_tree_leaves tree = match tree with
  | Node (sym, list) -> (parse_tree_list list)
  | Leaf num -> [num] 

(*task 3 and 4 public code*)
(*inspired by hint code given in the spec*)
let append_matchers matcher1 matcher2 accept frag used = matcher1 (fun frag1 -> matcher2 accept frag1) frag used

let rec match_rules non_terminal rule_fun rule_list accept frag used = match rule_list with
  | [] -> None  (*base casem, no matching*)
  | e::rest -> (match (match_individual rule_fun e accept frag (used@[(non_terminal, e)])) with
  (*see if the current rule can derive, if yes, the rule is useful, add it to the list of derivation*)
    | None -> (match_rules non_terminal rule_fun rest accept frag used) (*if fail, move on to the next rule*)
    | Some s -> Some s) (*if yes, return immediately*)

and match_individual rule_fun sym_list accept frag used = match sym_list with
  | [] -> (accept frag used) (*base case, check if accepted by acceptor*)
  | e::rest -> (match frag with
    | [] -> None (*exceed the frag length, not possible, reject immediately*)
    | h::t -> (match e with
      | T sym -> if sym = h then (match_individual rule_fun rest accept t used)
      (*if it is a terminal, then match the rest of the symbols if the terminal is the first of the frag*)
                            else None          
      (*if the terminal does not match, reject*)
      | N sym -> (append_matchers (match_rules sym rule_fun (rule_fun sym)) (match_individual rule_fun rest) accept frag used)))
      (*if it is a non-terminal, use the idea of appended matcher given in hint, use the second mathcer as an acceptor to recursively match the prefix*)

(*task 3 make_matcher*)
(*we do not care about the rules/derivation, so we use a wrapper to get rid of the used rules*)
let accept_wrapper accept frag used = match (accept frag) with
  | None -> None
  | Some s -> Some s
					      
let make_matcher gram accept frag = (match_rules (fst gram) (snd gram) ((snd gram) (fst gram)) (accept_wrapper accept) frag [])

(*task 4 make_parser*)
(*Use an empty acceptor to get the derivation of the whole fragment*)
let accept_empty frag used = match frag with
   | _::_ -> None
   | x -> Some used

let make_used_rules gram frag = (match_rules (fst gram) (snd gram) ((snd gram) (fst gram)) accept_empty frag [])

(*for the next part, I write the make_parser using a mutual recursive function, which parse the used rules given by make_used_rules, the order of the used rules can be seen as an alternative form of parse tree required by us*)

(*the first function deal with subtrees with the same depth, the return is a tuple since we need both the parse tree and keeping track of the rules we have consumed*)
let rec make_parallel sym_list used_list = match sym_list with
  | [] -> (used_list, []) (*base case, finish parsing a subtree*)
  | h::t -> (let (rul, subt) = (make_subtree h used_list) in (*get the subtree returned by the second function*)
    (let (rul_re, lis_re) = (make_parallel t rul) in (*append it to the rest of the subtrees with the same depth*)
      (rul_re, subt::lis_re)))

and make_subtree symb used_list = match symb with
  | N sym -> (match used_list with
    | [] -> ([], Node (sym, [])) (*Not possible, for exhaustive matching purpose, this is because the used_list is another form of parse tree verified by the previous functions*)
    | h::t -> (match (make_parallel (snd h) t) with
      | (rul, lis) -> (rul, Node (sym, lis)))) (*parse the subtrees in the next depth level*)
  | T sym -> (used_list, Leaf sym)(*return the subtree which is a leaf*)

(*actual make_parser function, just a wrapper for the previous chunks of functions*)
let make_parser gram frag= match (make_used_rules gram frag) with
  | None -> None
  | Some [] -> None
  | Some ls -> Some (hd (snd (make_parallel [N (fst (hd ls))] ls)))
