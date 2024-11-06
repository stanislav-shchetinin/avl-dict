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
 
    