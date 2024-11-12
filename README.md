## Лабораторная работа №2
- Студент: ``` Щетинин Станислав ```
- ИСУ: ``` 367658 ```
- Вариант: ``` AVL Tree Dict ```

## Пример использования
```main.ml```:  
``` ocaml
let dict =  empty |>
            insert 3 "three" |>
            insert 1 "one" |>
            insert 2 "two" in
  
  match find 2 dict with
  | Some value -> Printf.printf "Found: %s\n" value
  | None -> Printf.printf "Not found\n";
  
  let elements = to_list dict in
  List.iter (fun (k, v) -> 
    Printf.printf "%d: %s\n" k v) elements
```
Результат:

```
Found: two
```

## Интерфейс
```avl_dict.mli```: 
``` ocaml
type ('k, 'v) t

val empty       : ('k, 'v) t

val insert      : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
val erase       : 'k -> ('k, 'v) t -> ('k, 'v) t
val find        : 'k -> ('k, 'v) t -> 'v option

val height      : ('k, 'v) t -> int
val is_empty    : ('k, 'v) t -> bool
val to_list     : ('k, 'v) t -> ('k * 'v) list

val filter      : ('v -> bool) -> ('k, 'v) t -> ('k, 'v) t
val map         : ('v -> 'v) -> ('k, 'v) t -> ('k, 'v) t
val foldl       : (('k, 'v) t -> ('k, 'v) t -> ('k, 'v) t) -> ('k, 'v) t -> ('k, 'v) t
val foldr       : (('k, 'v) t -> ('k, 'v) t -> ('k, 'v) t) -> ('k, 'v) t -> ('k, 'v) t

val merge       : ('k, 'v) t -> ('k, 'v) t -> ('k, 'v) t
val equals      : ('k, 'v) t -> ('k, 'v) t -> bool
```

## Реализация
Реализация основных функций:

``` find ```:  
``` ocaml
let rec find key = function
  | Empty -> None
  | Node {key = k; value; left; right; _} ->
    if k < key then find key left
    else if k > key then find key right
    else Some value
```

``` insert ```:
``` ocaml
let rec insert key value = function
  | Empty -> create_node key value Empty Empty
  | Node {key = k; value = v; left; right; _} ->
    if k < key then 
      create_node k v (insert key value left) right |> balance
    else if k > key then
      create_node k v left (insert key value right) |> balance
    else create_node key value left right
```

``` erase ```:
``` ocaml
let rec erase key = function
  | Empty -> Empty
  | Node {key = k; value = v; left; right; _} ->
    if k < key then 
      create_node k v (erase key left) right |> balance
    else if k > key then
      create_node k v left (erase key right) |> balance
    else 
      match left, right with
      | Empty, Empty -> Empty
      | _, Empty -> left
      | Empty, _ -> right
      | _, _ ->
        let k, v = min_node_kv right in
        create_node k v left (erase k right) |> balance
```

## Моноид

Нейтральный элемент:
``` ocaml
let check_neutral_element tree = 
  equals tree (merge empty tree) && equals tree (merge tree empty)

let neutral_element_test = 
  assert (check_neutral_element random_tree)
```

Ассоциативность:

``` ocaml
let associativity_test = 
  let a = random_tree in
  let b = random_tree in
  let c = random_tree in
  assert (
    equals (merge a (merge b c)) (merge (merge a b) c)
  )
```

## Вывод
В результате выполнения лабораторной работы было реализовано avl дерево в функциональной парадигме в виде неизменяемой структуры данных, а также были проверены свойства моноида данной структуры.
