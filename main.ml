(* main.ml *)

open Avl_dict

let () =
  let dict = empty |> insert 3 "three" |> insert 1 "one" |> insert 2 "two" in
  
  match find 2 dict with
  | Some value -> Printf.printf "Found: %s\n" value
  | None -> Printf.printf "Not found\n";
  
  let elements = to_list dict in
  List.iter (fun (k, v) -> Printf.printf "%d: %s\n" k v) elements
