(*

#use"double_augmented.ml";;

*)

type modifier=R | U;;

type sign=Plus |Minus;;

type vector=
   Zero
  |V of (modifier list)* int;;

type cell=Cell of int*int;;

type state={
   table_for_r:(vector*(sign*vector)) list;
   table_for_u:(vector*(sign*vector)) list;
   memberships:(vector*cell) list;
};;

let default_r=function
    Zero->(Plus,Zero)
    |V(l,k)->
      match l with
      []->(Plus,V([R],k))
      |a::b->if a=R
             then (Plus,Zero)
             else (Plus,V(R::l,k));;

let default_u=function
    Zero->(Plus,Zero)
    |V(l,k)->
      match l with
      []->(Plus,V([U],k))
      |a::b->if a=U
             then (Plus,Zero)
             else (Plus,V(U::l,k));;

let actual_r state v=
    match Option.find_and_stop (
      fun (v1,rv1)->if v1=v then Some(rv1) else None
    ) state.table_for_r with
    None->default_r v
    |Some(rv)->rv;;

let actual_u state v=
      match Option.find_and_stop (
        fun (v1,rv1)->if v1=v then Some(rv1) else None
      ) state.table_for_u with
      None->default_u v
      |Some(rv)->rv;;    

let do_modification state modif v=
   match modif with
   R->actual_r state v
   |U->actual_u state v;;

let is_not_a_root_vector=function
    Zero->true
   |V(l,k)->l<>[];;             

let add_membership state (x,y)={
  table_for_r=state.table_for_r;
  table_for_u=state.table_for_u;
  memberships=(x,y)::state.memberships;
};;


let vector_counter=ref(0);;

let new_counter_value ()=
    let j=(!vector_counter)+1 in
    let _=(vector_counter:=j) in
    j;;

let initial_state={
    table_for_r=[];
    table_for_u=[];
    memberships=[V([],new_counter_value()),Cell(0,0)];
};;


let expand (cell,modif) state=
   let temp1=Option.filter_and_unpack 
   (fun (x,y)->if y=cell then Some(x) else None) state.memberships in
   let (state1,temp2)=(
       (* we add a root vector if there is none already present *)
       if List.for_all is_not_a_root_vector temp1
       then let new_v=V([],new_counter_value()) in 
           (add_membership state (new_v,cell),temp1@[new_v])
       else (state,temp1)
   ) in
   let temp3=Image.image(fun 
     v->(v,do_modification state1 modif v)
   ) temp2 in
   temp3;;
   

expand (Cell(0,0),R) initial_state;;
   

