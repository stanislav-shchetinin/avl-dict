(* avl_dict.mli *)
type ('k, 'v) t

val empty : ('k, 'v) t
val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
val erase : 'k -> ('k, 'v) t -> ('k, 'v) t
val find : 'k -> ('k, 'v) t -> 'v option
val height : ('k, 'v) t -> int
val is_empty : ('k, 'v) t -> bool
val to_list : ('k, 'v) t -> ('k * 'v) list
val filter : (f: ('k, 'v) t -> bool) -> ('k, 'v) t -> ('k, 'v) t
val map : (f: 'v -> 'v) -> ('k, 'v) t -> ('k, 'v) t

