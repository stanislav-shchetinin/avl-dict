open Avl_dict

let () = Random.self_init ()

let random_pair () =
  (Random.int 100, Random.int 100)

let random_pair_list n =
  List.init n (fun _ -> random_pair ())

let random_tree n =
  List.fold_left (fun tree (k, v) -> insert k v tree) empty (random_pair_list n)

let check_neutral_element tree = 
  equals tree (merge empty tree) && equals tree (merge tree empty)

let neutral_element_test = 
  assert (check_neutral_element (random_tree 15))

let associativity_test = 
  let a = random_tree 10 in
  let b = random_tree 10 in
  let c = random_tree 10 in
  assert (
    (* equals a b = false *)
    equals (merge a (merge b c)) (merge (merge a b) c)
  )
