(*

#use"small_array.ml";;

*)

type 'a t={
   mutable current_size : int;
   container : ('a option) array;
};;


let max_size=2000;;

exception Index_out_of_bounds of int*int;;

let get x k=
    if (k<1)||(k>x.current_size)
    then raise(Index_out_of_bounds(k,x.current_size))
    else Option.unpack(Array.get x.container (k-1));;

let set x k y=
      if (k<1)||(k>x.current_size)
      then raise(Index_out_of_bounds(k,x.current_size))
      else Array.set x.container (k-1) (Some y);;
      
let of_array arr=
  let n=Array.length arr in
  {
     current_size = n;
     container = Array.init max_size(
       fun k->
         if k<n
         then Some(Array.get arr k)
         else None
     )
  };;

let to_array x=
    Array.init x.current_size (fun k->
      Option.unpack(Array.get x.container k)
    );;

let of_list l=of_array(Array.of_list l);;
let to_list x=Array.to_list (to_array x);;

exception Overflow;;

let push_right x u=
  let c=x.current_size in
  if c>=max_size
  then raise(Overflow)
  else
  (
    x.current_size<-(c+1); 
    Array.set x.container c (Some u)
  );;

exception Bad_indices_in_reposition of int*int*int;;

let reposition_just_after_fst_idx
  x i j=
    let c=x.current_size in
    if (i>=j)||(i<1)||(i>c)||(j<1)||(j>c)
    then raise(Bad_indices_in_reposition(i,j,c)) 
    else
    let temp_copy=Array.copy x.container in
    for k=1 to c
    do
      let new_k=Ennig.reposition_just_after_fst_idx i j k in
    Array.set x.container (k-1) (Array.get temp_copy (new_k-1))
    done;;
   











         