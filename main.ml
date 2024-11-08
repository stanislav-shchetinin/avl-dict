(* main.ml *)

open Avl_dict
open Test
open Avl_dict_ut
open Pbt
open Format

(* Preparation *)
let data = [ (1, "1"); (5, "5"); (3, "3"); (42, "42") ]
let tree = List.fold_left (fun tree (k, v) -> insert k v tree) empty data

(* Find tests *)
let () = find_test 1 tree data
let () = find_test 2 tree data
let () = find_test 5 tree data

(* Insert tests *)
let () = insert_test 10 "10" tree
let () = insert_test 1 "3" tree
let () = insert_test 1 "1" tree

(* Erase tests *)
let() = erase_test 1 tree
let() = erase_test 2 tree
let() = erase_test 42 tree

(* Monoid *)
let() = neutral_element_test

let() = printf "%d" 5
