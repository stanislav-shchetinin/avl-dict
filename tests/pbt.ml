open Avl_dict
open QCheck

let pair_gen =
  Gen.(map (fun (k, v) -> (k, v)) (pair (int_bound 100) (int_bound 100)))

let pair_list_gen =
  Gen.list_size (Gen.int_bound 20) pair_gen

let avl_tree_gen =
  Gen.map
    (fun pairs -> List.fold_left (fun tree (k, v) -> insert k v tree) empty pairs)
    pair_list_gen

(* Arbitrary *)
let avl_tree_arbitrary = make ~print:(fun _ -> "<avl_tree>") avl_tree_gen

(* Neutral element property *)
let neutral_element_test =
  Test.make
    ~name:"Neutral element property"
    avl_tree_arbitrary
    (fun tree -> equals tree (merge empty tree) && equals tree (merge tree empty))

(* Associativity property *)
let associativity_test =
  Test.make
    ~name:"Associativity property"
    Gen.(triple avl_tree_gen avl_tree_gen avl_tree_gen |> make)
    (fun (a, b, c) ->
      equals (merge a (merge b c)) (merge (merge a b) c))

let () =
  QCheck_base_runner.run_tests_main
    [neutral_element_test; associativity_test]
