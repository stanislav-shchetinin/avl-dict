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

let filter_test predicate tree data =
  let filtered_tree = filter predicate tree in
  let filtered_data = List.filter (fun (_, v) -> predicate v) data in
  List.iter (fun (k, v) ->
    match find k filtered_tree with
    | None -> assert false
    | Some v' -> assert (v = v')
  ) filtered_data;
  let rec check_all_pass tree =
    match to_list tree with
    | [] -> true
    | (_, v) :: rest -> predicate v && check_all_pass (List.fold_left (fun acc (k, v) -> insert k v acc) empty rest)
  in
  assert (check_all_pass filtered_tree)

let map_test f tree data =
  let mapped_tree = map f tree in
  let mapped_data = List.map (fun (k, v) -> (k, f v)) data in
  List.iter (fun (k, v) ->
    match find k mapped_tree with
    | None -> assert false
    | Some v' -> assert (v = v')
  ) mapped_data;
  assert (List.length (to_list mapped_tree) = List.length data)

let foldl_test f init tree data =
  let result = foldl f init tree in
  let expected = List.fold_left (fun acc (_, v) -> f acc v) init data in
  assert (result = expected)

let foldr_test f init tree data =
  let result = foldr f init tree in
  let expected = List.fold_right (fun (_, v) acc -> f v acc) data init in
  assert (result = expected)

(* Preparation *)
let data = [ (1, "1"); (3, "5"); (5, "3"); (42, "42") ]
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

(* Filter tests *)
let () = filter_test (fun v -> String.length v > 3) tree data
let () = filter_test (fun v -> v = "two") tree data
let () = filter_test (fun _ -> true) tree data
let () = filter_test (fun _ -> false) tree data

(* Map tests *)
let () = map_test String.uppercase_ascii tree data
let () = map_test (fun v -> v ^ "!") tree data
let () = map_test (fun _ -> "constant") tree data
let () = map_test (fun v -> String.sub v 0 1) tree data

(* Foldl tests *)
let () = 
  foldl_test 
    (fun acc v -> acc ^ v ^ " ") 
    "" 
    tree 
    data

let () = 
  foldl_test 
    (fun acc v -> acc + String.length v) 
    0 
    tree 
    data

(* Foldr tests *)
let () = 
  foldr_test 
    (fun v acc -> acc ^ v ^ " ") 
    "" 
    tree 
    data

let () = 
  foldr_test 
    (fun v acc -> acc + String.length v) 
    0 
    tree 
    data

let() = Printf.printf "AVL tree unit tests: OK\n"
