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
  | Node { height; _ } -> height

let create_node key value left right =
  let height = 1 + max (height left) (height right) in
  Node { key; value; left; right; height }

let balance_factor = function
  | Empty -> 0
  | Node { left; right; _ } -> height left - height right

let rotate_right = function
  | Node { key; value; left = Node { key = lk; value = lv; left = ll; right = lr; _ }; right; _ } ->
      create_node lk lv ll (create_node key value lr right)
  | node -> node

let rotate_left = function
  | Node { key; value; right = Node { key = rk; value = rv; left = rl; right = rr; _ }; left; _ } ->
      create_node rk rv (create_node key value left rl) rr
  | node -> node

let balance node =
  match node with
  | Empty -> Empty
  | Node { key; value; left; right; _ } as n ->
      let bf = balance_factor n in
      if bf > 1 then
        if balance_factor left >= 0 then rotate_right n
        else create_node key value (rotate_left left) right |> rotate_right
      else if bf < -1 then
        if balance_factor right <= 0 then rotate_left n
        else create_node key value left (rotate_right right) |> rotate_left
      else n

let rec insert key value = function
  | Empty -> create_node key value Empty Empty
  | Node { key = k; value = v; left; right; _ } ->
      if key < k then
        balance (create_node k v (insert key value left) right)
      else if key > k then
        balance (create_node k v left (insert key value right))
      else
        create_node key value left right

let rec find key = function
  | Empty -> None
  | Node { key = k; value = v; left; right; _ } ->
      if key < k then find key left
      else if key > k then find key right
      else Some v

let is_empty = function
  | Empty -> true
  | _ -> false

let rec to_list = function
  | Empty -> []
  | Node { key; value; left; right; _ } -> to_list left @ [(key, value)] @ to_list right
