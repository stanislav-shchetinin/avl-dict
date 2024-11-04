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
  | Node {key; value; left;  right; _} as node -> 
    let bf = balance_factor node in
    if bf = -2 then
      if balance_factor right <= 0 then rotate_left node
      else create_node key value left (rotate_right right) |> rotate_left
    else if bf = 2 then
      if balance_factor left <= 0 then rotate_right node
      else create_node key value (rotate_left left) right |> rotate_right
    else node

let find key = function
  | Empty -> Empty
  | Node {k = key; left; right; _} as node ->
    if k < key then find key left
    else if k > key find key right
    else node

let insert key value = function
  | Empty -> create_node key value Empty Empty
  | Node {k = key; v = value; left; right; _} as node ->
    if k < key then 
      create_node k v (insert key value left) right |> balance
    else if k > key 
      create_node k v left (insert key value right) |> balance
    else create_node key value left right

let min_node = function
  | Empty -> Empty
  | Node {left; _} as node ->
    match left with
      | Empty -> node
      | _ -> min_node left

let erase key = function
  | Empty -> Empty
  | Node {k = key; v = value; left; right; _} as node ->
    if k < key then 
      create_node k v (erase key value left) right |> balance
    else if k > key 
      create_node k v left (erase key value right) |> balance
    else 
      match node with
      | Node {Empty; Empty; _} -> Empty
      | Node {left; Empty; _} -> left
      | Node {Empty; right; _} -> right
      | Node {left; right; _} -> 
        let er_node = min_node right in
          match er_node with
          | Node {k = key; v = value; l = left; r = right; _} ->
            create_node k v left (erase k right)

let is_empty = function
  | Empty -> true
  | _ -> false

let to_list = function
  | Empty -> []
  | Node {key; value; left; right; _} -> to_list left @ [(key, value)] @ to_list right

let filter (f: ('k, 'v) t -> bool) node =
  if f node then
    match node with
    | Empty -> f Empty
    | Node {key; value; left; right; _} -> create_node key value (f left) (f right)
  else 
    f (erase node)

let map (f: 'v -> 'v) node = function
  | Empty -> Empty
  | Node {key; value; left; right; _} -> create_node key (f value) (map left) (map right) 

