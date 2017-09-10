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
     let (a,peurrest)=Php_positioned_token_list.ht da_ober in 
     let lxm=fst(a) in
     if lxm=left_blocker
     then tempf(Php_positioned_token_list.cons a graet,j+1,peurrest)
     else
     if lxm=right_blocker
     then if j=1
          then Some((Php_positioned_token_list.rev graet,snd(snd(a)),peurrest),a)
          else tempf(Php_positioned_token_list.cons  a graet,j-1,peurrest)
     else 
       if f lxm
       then tempf(Php_positioned_token_list.cons  a graet,j,peurrest)
       else None
   ) in
   tempf(Php_positioned_token_list.empty,depth,tok_l);;

