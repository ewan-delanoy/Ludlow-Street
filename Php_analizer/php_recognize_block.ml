(*

#use"Php_analizer/php_recognize_block.ml";;

*)



let main
   f (left_blocker,right_blocker) depth tok_l=
   let rec tempf=(
   fun (graet,j,da_ober)->
     if da_ober=[]
     then None
     else 
     let (a,peurrest)=Listennou.ht da_ober in 
     let lxm=fst(a) in
     if lxm=left_blocker
     then tempf(List.cons a graet,j+1,peurrest)
     else
     if lxm=right_blocker
     then if j=1
          then Some((List.rev graet,snd(snd(a)),peurrest),a)
          else tempf(List.cons  a graet,j-1,peurrest)
     else 
       if f lxm
       then tempf(List.cons  a graet,j,peurrest)
       else None
   ) in
   tempf([],depth,tok_l);;

