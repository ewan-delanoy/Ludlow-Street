(*

#use"Php_analizer/php_recognize_block.ml";;

*)



let main
   f (left_blocker,right_blocker) depth tok_l=
   let rec tempf=(
   fun (graet,j,da_ober)->
     if Positioned_php_token_list.is_empty da_ober
     then None
     else 
     let (a,peurrest)=Positioned_php_token_list.ht da_ober in 
     let lxm=Php_positioned_token.fst(a) in
     if lxm=left_blocker
     then tempf(Positioned_php_token_list.cons a graet,j+1,peurrest)
     else
     if lxm=right_blocker
     then if j=1
          then Some((Positioned_php_token_list.rev graet,snd(Php_positioned_token.snd(a)),peurrest),a)
          else tempf(Positioned_php_token_list.cons  a graet,j-1,peurrest)
     else 
       if f lxm
       then tempf(Positioned_php_token_list.cons  a graet,j,peurrest)
       else None
   ) in
   tempf(Positioned_php_token_list.empty,depth,tok_l);;

