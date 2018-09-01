(*

#use"small_array.ml";;

*)

type 'a t={
   mutable current_size : int;
   container : ('a option) array;
};;



let max_size=2000;;

exception Index_out_of_bounds of int*int;;

let size x=x.current_size;;

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



exception Bad_indices_in_reposition of int*int*int;;

let reposition_by_putting_snd_immediately_after_fst
  x i j=
    let c=x.current_size in
    if (i>=j)||(i<1)||(i>c)||(j<1)||(j>c)
    then raise(Bad_indices_in_reposition(i,j,c)) 
    else
    let temp_copy=Array.copy x.container in
    for k=i+1 to j
    do
      let new_k=Ennig.reposition_by_putting_snd_immediately_after_fst i j k in
      if new_k<>k
      then Array.set x.container (k-1) (Array.get temp_copy (new_k-1))
    done;;
   
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
    
let push_immediately_after_idx x u i=
  let c=x.current_size in 
  (
    push_right x u;
    reposition_by_putting_snd_immediately_after_fst x i (c+1)  
  );;

let copy x=of_array(to_array x);;

let copy_from x y=
  (
    x.current_size <- y.current_size;
    for j=0 to (max_size-1) 
    do
      Array.set x.container j (Array.get y.container j)
    done  
  );;


exception Element_not_found;;
 
let leftmost_index_of_in y x=
    let c=x.current_size in
    let rec tempf=(
      fun k->if k>c then raise(Element_not_found) else
             if Array.get x.container (k-1)=Some(y)
             then k
             else tempf(k+1)
    ) in
    tempf 1;;

let indices_of_property_of_in f x=
    let c=x.current_size in
    let accu=ref[] in 
    for k=1 to c 
    do
    let item =  Option.unpack (Array.get x.container (k-1)) in
       if f item
       then accu:=k::(!accu)
    done;
    List.rev(!accu);;


exception Property_not_found;;

let leftmost_index_of_property_in f x=
    let c=x.current_size in
    let rec tempf=(
      fun k->if k>c then raise(Property_not_found) else
             if f(Option.unpack(Array.get x.container (k-1)))
             then k
             else tempf(k+1)
    ) in
    tempf 1;;

let seek f x=
  let c=x.current_size in
  let rec tempf=(
    fun k->if k>c then None else
          let item =  
            Option.unpack (Array.get 
             x.container (k-1)) in
          if f item
           then Some(item)
           else tempf(k+1)
  ) in
  tempf 1;;

let filter_and_unpack f x=
    let c=x.current_size in
    let accu=ref[] in 
    for k=1 to c do
    let item =  Option.unpack (Array.get x.container (k-1)) in
      (match f item with
      Some(result)->accu:=result::(!accu);
      |None->()
    ) 
    done;
    List.rev(!accu);;

let image f x=
      let c=x.current_size in
      let accu=ref[] in 
      for k=1 to c do
      let item =  Option.unpack (Array.get x.container (k-1)) in
      accu:=(f item)::(!accu);
      done;
      List.rev(!accu);;
  


exception Remove_item_exn of int*int;;

let remove_item_at_index x k=
  let c=x.current_size in
  if (k<1)||(k>c) then raise(Remove_item_exn(k,c)) else
  (
    x.current_size<-(c-1); 
    for j=k to c 
    do
      Array.set x.container (j-1) (Array.get x.container j)
    done  
  );;

let apply_transformation_on_interval x f i j=
    (
      for k=(i-1) to (j-1) do
        let old_val=Option.unpack(Array.get x.container k) in
        let new_val=f(old_val) in
        Array.set x.container k (Some new_val)
      done  
    );;

let apply_transformation_on_all x f=
  apply_transformation_on_interval x f 1 (x.current_size);;

let apply_transformation_on_rightmost_interval x f i=
  apply_transformation_on_interval x f i (x.current_size);;

 
let industrial_separator=Industrial_separator.small_array;;

(*

Do not use those archiving functions on nested small arrays !

*)

let archive old_archiver x=
   let temp1=Ennig.doyle (
       fun k->Nonblank.make(old_archiver(get x k))
   ) 1 x.current_size in
   String.concat industrial_separator temp1;;
   
let unarchive old_unarchiver s=
    let temp1=Str.split (Str.regexp_string industrial_separator) s in
    let temp2=Image.image (fun x->old_unarchiver (Nonblank.decode x)) temp1 in 
    of_list temp2;;   
   





         