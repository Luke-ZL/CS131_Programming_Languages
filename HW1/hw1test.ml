let subset_test0 = subset [1] [1;2;3]
let subset_test1 = subset [1;3;1] [1;2;3]
let subset_test2 = not (subset [4;4;5] [4;1;3])

let equal_sets_test0 = equal_sets [1;3;1] [3;1;3]
let equal_sets_test1 = not (equal_sets [1;1;14] [3])

let set_union_test0 = equal_sets (set_union [1] [1;2;3]) [1;2;3]
let set_union_test1 = equal_sets (set_union [3;3;3] [1;2;3]) [1;2;3]
let set_union_test2 = equal_sets (set_union [1] [1]) [1]

let set_intersection_test0 =
  equal_sets (set_intersection [2] [1;2;3]) [2]
let set_intersection_test1 =
  equal_sets (set_intersection [2;1;3] [1;2;3]) [1;2;3]
let set_intersection_test2 =
  equal_sets (set_intersection [1;2;3;4] [3;1;2]) [3;2;1]

let set_diff_test0 = equal_sets (set_diff [1] [1;4;3;1]) []
let set_diff_test1 = equal_sets (set_diff [4;3;1;1;3] [1;3;3;4;4]) []
let set_diff_test2 = equal_sets (set_diff [4;3;1] []) [3;4;1]
let set_diff_test3 = equal_sets (set_diff [4;3;3;2] [4;3;1]) [2]

let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x - x * x * x) 1 = 0


type awksub_nonterminals =
  | A | B | BB | D | E

let awksub_rules =
   [BB, [T"("; N BB; T")"];
    BB, [N D];
    BB, [N B; N E; N A];
    BB, [N B];
    BB, [N BB; N D];
    BB, [N A;N A];
    A, [];
    B, [N D];
    D, [T"--"];
    D, [T"+"];
    D, [T"-"];
    E, [T"0"];
    E, [T"1"];
    E, [T"2"];
    E, [T"3"];
    E, [T"4"];
    E, [T"5"];
    E, [T"6"];
    E, [T"7"];
    E, [T"8"];
    E, [T"9"]]

let awksub_grammar = A, awksub_rules

let awksub_test0 =
  filter_reachable awksub_grammar = (A, [A,[]])

let awksub_test1 =
  filter_reachable (BB, List.tl awksub_rules) = (BB, List.tl awksub_rules)

let awksub_test2 =
  filter_reachable (B, awksub_rules) = (B, [B, [N D];D, [T"--"];D, [T"+"];D, [T"-"]])

