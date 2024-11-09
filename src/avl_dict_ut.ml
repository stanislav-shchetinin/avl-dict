open Avl_dict

let find_test key tree data =
  if List.exists (fun (k, _) -> k = key) data then
    match find key tree with
    | None -> assert false
    | Some v' -> assert (List.exists (fun (k, v) -> k = key && v = v') data)
  else
    assert(find key tree = None)

let insert_test key value tree =
  let new_tree = insert key value tree in
  match find key new_tree with
  | None -> assert false
  | Some v -> assert (v = value)  

let erase_test key tree = 
  let new_tree = erase key tree in
  assert (find key new_tree = None)

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

let() = Printf.printf "AVL tree unit tests: OK\n"
