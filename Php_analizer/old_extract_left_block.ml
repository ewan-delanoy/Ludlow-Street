(*

#use"Php_analizer/extract_left_block.ml";;

*)



let main
   f (left_blocker,right_blocker) tok_l=
   let rec tempf=(
   fun (graet,j,da_ober)->match
    da_ober with
     []->None
    |a::peurrest->
       let lxm=Positioned_php_token.fst(a) in
       if lxm=left_blocker
       then tempf(a::graet,j+1,peurrest)
       else
       if lxm=right_blocker
       then if j=1
            then Some((List.rev graet,snd(Positioned_php_token.snd(a)),peurrest),a)
            else tempf(a::graet,j-1,peurrest)
       else 
       if f lxm
       then tempf(a::graet,j,peurrest)
       else None
   ) in
   tempf([],1,tok_l);;

