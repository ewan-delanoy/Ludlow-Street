(*

#use"dircopy_diff.ml";;

*)

type t={
   recently_deleted : string list;
   recently_changed : string list;
   recently_created : string list;
};;

let recently_deleted x=x.recently_deleted;;
let recently_created x=x.recently_created;;
let recently_changed x=x.recently_changed;;

let veil a b c={
   recently_deleted =Recently_deleted.to_string_list a;
   recently_changed =Recently_changed.to_string_list b;
   recently_created =Recently_created.to_string_list c;
};;

