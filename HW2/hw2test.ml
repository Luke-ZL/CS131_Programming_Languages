type giant_nonterminals =
| A | B | C | D 

let giant_grammar =
B,
[A, [T"ZZZ"];
A, [T "woof"];
D, [T"khrgh"];
D, [T"aooogah!"];
C, [N D; N C;T "RUA"];
C, [N B];
C, [T "HI"];
B, [N A];
B, [N B; T","; N C; N D]]

let accept_z = function
 | ["ZZZ"] -> Some ["ZZZ"]
 | _ -> None


let parse_t = (make_parser (convert_grammar giant_grammar) ["ZZZ"; "," ; "khrgh"; "ZZZ"; "RUA"; "aooogah!"])

let get_some = function
  | None -> (Node (A, []))
  | Some s -> s			   

let make_matcher_test = (make_matcher (convert_grammar giant_grammar) accept_z ["ZZZ"; "," ; "khrgh"; "ZZZ"; "RUA"; "aooogah!"; "ZZZ"]) = Some ["ZZZ"] 

let make_parser_test = (parse_tree_leaves (get_some parse_t)) = ["ZZZ"; "," ; "khrgh"; "ZZZ"; "RUA"; "aooogah!"]
