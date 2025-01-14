open Avl_dict
open QCheck

(* Генератор случайных пар *)
let pair_gen =
  Gen.(map (fun (k, v) -> (k, v)) (pair (int_bound 100) (int_bound 100)))

(* Генератор списка пар *)
let pair_list_gen =
  Gen.list_size (Gen.int_bound 20) pair_gen

(* Генератор AVL-дерева *)
let avl_tree_gen =
  Gen.map
    (fun pairs -> List.fold_left (fun tree (k, v) -> insert k v tree) empty pairs)
    pair_list_gen

(* Преобразование генератора в arbitrary *)
let avl_tree_arbitrary = make ~print:(fun _ -> "<avl_tree>") avl_tree_gen

(* Тест нейтрального элемента *)
let neutral_element_test =
  Test.make
    ~name:"Neutral element property"
    avl_tree_arbitrary
    (fun tree -> equals tree (merge empty tree) && equals tree (merge tree empty))

(* Тест ассоциативности *)
let associativity_test =
  Test.make
    ~name:"Associativity property"
    Gen.(triple avl_tree_gen avl_tree_gen avl_tree_gen |> make)
    (fun (a, b, c) ->
      equals (merge a (merge b c)) (merge (merge a b) c))

(* Запуск тестов *)
let () =
  QCheck_base_runner.run_tests_main
    [neutral_element_test; associativity_test]
