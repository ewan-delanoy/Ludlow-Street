
(* 


#use"Makefile_makers/check_genealogy.ml";;

Checks the "all_ancestors" record against the "direct_fathers"
record.

*)

let local_recomputation l fd=
     let temp1=Modulesystem_data.direct_fathers fd in
     let temp2=Image.image(fun hm2->
       let fd2=Option.find_it_really(
          fun x->Modulesystem_data.name x=hm2
       ) l in
       Tidel.diforchan(Modulesystem_data.all_ancestors fd2)
     ) temp1 in
     let temp3=(Tidel.diforchan temp1)::temp2 in
     Option.filter_and_unpack(fun fd3->
       let hm3=Modulesystem_data.name fd3 in
       if List.exists(Tidel.elfenn hm3) temp3  
       then Some(hm3)
       else None
     ) l;;




let check_genealogy ts=
  let fs=Target_system.modulesystem ts in
  let data=Modulesystem.all_filedata fs in
  let recompute_ancestors=local_recomputation data in
  let tester=(fun fd->
    let a=recompute_ancestors fd
    and b=Modulesystem_data.all_ancestors fd in
    let za=Tidel.safe_set a
    and zb=Tidel.safe_set b in
    let d1=Tidel.forget_order(Tidel.lemel za zb)
    and d2=Tidel.forget_order(Tidel.lemel zb za)
    in
    if (d1,d2)=([],[])
    then None
    else Some(Modulesystem_data.name fd,d1,d2)
  )
  in
  Option.find_and_stop tester
  data;;
  
  





 