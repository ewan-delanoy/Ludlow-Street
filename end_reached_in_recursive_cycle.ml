(*

#use"end_reached_in_recursive_cycle.ml";;

*)

type t=
   Reached of int (* there may be several exit doors *)
  |Not_reached;;
