
type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c
type filename = string
type 'a stack = 'a list


val cat :      filename -> string list
val exclude : ('a -> bool) -> 'a list -> 'a list
val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val i_to_s : int -> string
exception Impossible
val join : string (* sep *) -> string list -> string
val map_filter : ('a -> 'b option) -> 'a list -> 'b list
val null : 'a list -> bool
val pr : string -> unit
val profile_code : string -> (unit -> 'a) -> 'a
val pr2 : string -> unit
val pr2_gen: 'a -> unit
val pr2_once : string -> unit
val push : 'a -> 'a stack ref -> unit
val spf : ('a, unit, string) format -> 'a
val take_safe : int -> 'a list -> 'a list
exception Todo
val with_open_infile : filename -> (in_channel -> 'a) -> 'a

val (=|=) : int    -> int    -> bool
val (=~) : string -> string -> bool







