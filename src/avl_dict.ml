(* avl_dict.ml *)

type ('k, 'v) t =
  | Empty
  | Node of {
      key : 'k;
      value : 'v;
      left : ('k, 'v) t;
      right : ('k, 'v) t;
      height : int;
    }

let empty = Empty

let height = function
  | Empty -> 0
  | Node {height; _} -> height

let balance_factor = function
  | Empty -> 0
  | Node {left; right; _} -> height left - height right

let create_node key value left right = 
  Node {key; value; left; right; height = 1 + max (height left) (height right) }

let rotate_left = function
  | Node { key; value; left; right = Node { key = k1; value = v1; left = l1; right = r1; _}; _} ->
    create_node k1 v1 (create_node key value left l1) r1
  | node -> node

let rotate_right = function
  | Node { key; value; left = Node{ key = k1; value = v1; left = l1; right = r1; _}; right; _ } ->
    create_node k1 v1 l1 (create_node key value r1 right)
  | node -> node

let balance = function
  | Empty -> Empty
  | Node {key; value; left; right; _} as node -> 
    let bf = balance_factor node in
    if bf = -2 then
      if balance_factor right <= 0 then rotate_left node
      else create_node key value left (rotate_right right) |> rotate_left
    else if bf = 2 then
      if balance_factor left <= 0 then rotate_right node
      else create_node key value (rotate_left left) right |> rotate_right
    else node

let rec find key = function
  | Empty -> None
  | Node {key = k; value; left; right; _} ->
    if key < k then find key left
    else if key > k then find key right
    else Some value

let rec insert key value = function
  | Empty -> create_node key value Empty Empty
  | Node {key = k; value = v; left; right; _} ->
    if key < k then 
      create_node k v (insert key value left) right |> balance
    else if key > k then
      create_node k v left (insert key value right) |> balance
    else create_node key value left right

let rec min_node_kv = function
  | Empty -> failwith "Tree is empty"
  | Node {key; value; left = Empty; _} -> (key, value)
  | Node {left; _} -> min_node_kv left

let rec erase key = function
  | Empty -> Empty
  | Node {key = k; value = v; left; right; _} ->
    if key < k then 
      create_node k v (erase key left) right |> balance
    else if key > k then
      create_node k v left (erase key right) |> balance
    else 
      match left, right with
      | Empty, Empty -> Empty
      | _, Empty -> left
      | Empty, _ -> right
      | _, _ ->
        let k, v = min_node_kv right in
        create_node k v left (erase k right) |> balance
            
let is_empty = function
  | Empty -> true
  | _ -> false

let rec to_list = function
  | Empty -> []
  | Node {key; value; left; right; _} ->
    to_list left @ [(key, value)] @ to_list right

let rec filter (f: 'v -> bool) = function
  | Empty -> Empty
  | Node {key; value; left; right; _} as node -> 
    if f value then create_node key value (filter f left) (filter f right) |> balance
    else filter f (erase key node)

let rec map (f: 'v -> 'v) = function
  | Empty -> Empty
  | Node {key; value; left; right; _} -> create_node key (f value) (map f left) (map f right) 

let rec foldl f acc tree =
  match tree with
  | Empty -> acc
  | Node { value; left; right; _ } ->
      let acc = foldl f acc left in
      let acc = f acc value in
      foldl f acc right

let rec foldr f acc tree =
  match tree with
  | Empty -> acc
  | Node { value; left; right; _ } ->
      let acc = foldr f acc right in
      let acc = f value acc in
      foldr f acc left

let merge node1 node2 =
  let rec insert_all tree = function
    | [] -> tree
    | (key, value) :: rest -> insert_all (insert key value tree) rest
  in
  insert_all node2 (to_list node1)
    
let equals node1 node2 =
  to_list node1 = to_list node2
