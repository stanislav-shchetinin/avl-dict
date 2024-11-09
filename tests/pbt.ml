open Avl_dict

let () = Random.self_init ()

let random_pair () =
  (Random.int 100, Random.int 100)

let random_pair_list n =
  List.init n (fun _ -> random_pair ())

let random_tree =
  List.fold_left (fun tree (k, v) -> insert k v tree) empty (random_pair_list (Random.int 20))

let check_neutral_element tree = 
  equals tree (merge empty tree) && equals tree (merge tree empty)

let neutral_element_test = 
  assert (check_neutral_element random_tree)

let associativity_test = 
  let a = random_tree in
  let b = random_tree in
  let c = random_tree in
  assert (
    equals (merge a (merge b c)) (merge (merge a b) c)
  )

(* Monoid tests *)
let () = neutral_element_test
let () = associativity_test

let() = Printf.printf "Monoid tests: OK\n"
