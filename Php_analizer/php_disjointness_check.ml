(*

#use"Php_analizer/php_disjointness_check.ml";;

Most of  the "head_tail" lists used here will be of the form
[(HI(i1),x1);(HI(i2),x2);...;(HI(in),xn)] where i1<i2<...<in.

*)

type holder_index=HI of int;;
type head_tail=holder_index*Php_constructible_recognizer.t;;

type t=
   {
     dead  : ((Php_projected_token_set.t list)*holder_index) list;
     alive : ((Php_projected_token_set.t list)*(head_tail list)) list;
   }
;;

module Private=struct
   
let start l=
     let temp1=Ennig.index_everything(l) in
     let temp2=Image.image (fun (j,x)->(HI(j),x)) temp1 in
     {
         dead=[];
         alive=[[],temp2];
     };;

(*
We  assume that the argument of individual_push has the form 
[(HI(i1),x1);(HI(i2),x2);...;(HI(in),xn)] where i1<i2<...<in.
This allows us to deduce that if xi is Chain[], then this pair is already dead.

*)

let individual_push (l:head_tail list)=
    let (temp7,not_immediately_dead)=
        List.partition (
          fun (idx,x)->not(Php_constructible_recognizer.equals_empty_word_acceptor x)
        ) l in
    let first_dead=Image.image (fun pair->([],fst pair)) temp7 in
    let temp1=List.flatten(Image.image (fun (idx,x)->
        let ttemp2=snd(Php_constructible_recognizer.head_tail_decomposition x) in
        Image.image (fun (a,peurrest)->((idx,a),peurrest) ) ttemp2
    ) not_immediately_dead) in
    let temp2=Image.image fst temp1 in
    let temp3=Php_projected_token_set.generated_algebra (Image.image snd temp2) in
    let temp4=Image.image(
        fun (generated,generators)->
         let ttemp5=Option.filter_and_unpack (fun ((idx,ptokset),peurrest)->
         if Tidel.elfenn ptokset generators
         then Some((idx,peurrest))
         else None
         ) temp1 in
         let ttemp9=Option.filter_and_unpack (
           fun (idx,_)->
            let tttemp10=Option.filter_and_unpack (fun (idx2,peurrest2)->
            if idx2=idx then Some(peurrest2) else None
            ) ttemp5 in
            if tttemp10=[]
            then None
            else Some((idx,Php_constructible_recognizer.disjunction tttemp10):head_tail)
         ) not_immediately_dead in
        (generated,ttemp9)
      ) temp3 in
    let (temp6,living_ones)=List.partition (fun (x,l)->List.length l=1) temp4 in
    let second_dead=Image.image (fun (x,l)->([x],fst(List.hd l))) temp6 in  
    (first_dead@second_dead,living_ones);;


let translated_individual_push (first_ptoks,ll)=
      let (dead_ones,living_ones)=individual_push ll in
      {
         dead=Image.image (fun (x,l)->(first_ptoks@x,l)) dead_ones;
         alive=Image.image (fun (x,l)->(first_ptoks@[x],l)) living_ones;
      } ;;

let pusher x=
     let temp1=Image.image  translated_individual_push x.alive in
     let new_dead=List.flatten(Image.image (fun y->y.dead) temp1)
     and still_alive=List.flatten(Image.image (fun y->y.alive) temp1) in
     {
         dead=(x.dead)@new_dead;
         alive=still_alive;
     };;

let max_head_size_for_check=15;;

let ref_for_errors=ref[];;

let check l=
     let rec tempf=(fun (walker,j)->
        if walker.alive=[]
        then Some(walker.dead)
        else   if j>=max_head_size_for_check
               then (ref_for_errors:=[l,walker.alive];None)
               else tempf(pusher walker,j+1) 
     ) in
     tempf(start l,0);;
end;;

let check=Private.check;;


     
    




