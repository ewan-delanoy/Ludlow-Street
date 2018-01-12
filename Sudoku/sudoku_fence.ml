(*

#use"Sudoku/sudoku_fence.ml";;

*)

type t=F of int list;;


let get (F l) (i,j)=List.nth l (j-1+9*(i-1));;

let set (F l) (i,j) v=
     let checked_v=(
         if (1<=v)&&(v<=9) then v else 0
     ) in
     let arr=Array.of_list l in
     let _=(Array.set arr (j-1+9*(i-1)) checked_v ) in
     F(Array.to_list arr);;
    
let initial =F (Ennig.doyle (fun _->0) 1 81);;

