(* avl_dict.mli *)
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
val equalse     : ('k, 'v) t -> ('k, 'v) t -> bool
