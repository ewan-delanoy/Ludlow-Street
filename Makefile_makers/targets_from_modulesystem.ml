(*

#use"Makefile_makers/isidore_targets_from_modulesystem.ml";;

*)

let from_modulesystem_data dt=
  let hm=Modulesystem_data.name dt 
  and (mlp,mlip,mllp,mlyp)=Modulesystem_data.presences dt in
  let temp1=[
                mllp,Ocaml_target.ml_from_mll hm;
                mlyp,Ocaml_target.ml_from_mly hm;
           mlp||mlip,Ocaml_target.cmi hm;
     	   mlp||mlip,Ocaml_target.cmo hm;
           mlp||mlip,Ocaml_target.cma hm;
           mlp||mlip,Ocaml_target.cmx hm;
                 mlp,Ocaml_target.executable hm;
  ] in
  Option.filter_and_unpack (fun x->if fst x then Some(snd x) else None) temp1;;
  
let from_modulesystem fs=
  let l_dt=Modulesystem.all_filedata fs in
  let temp1=Image.image from_modulesystem_data l_dt 
  and temp2=Image.image Modulesystem_data.name l_dt in
  let temp3=List.flatten temp1 
  and mtn=Modulesystem.main_toplevel_name fs in
  temp3@[Ocaml_target.toplevel mtn temp2]
  ;;


 
 
