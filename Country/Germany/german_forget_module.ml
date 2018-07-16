(*

#use"Country/Germany/german_forget_module.ml";;

*)

    
exception ModuleWithDependencies of 
	Half_dressed_module.t*(Half_dressed_module.t list);;
exception Non_registered_module of Half_dressed_module.t;;

let on_targets root_dir (mdata,dirs,tgts) hm=
  match Modify_md_list.find_module_registration mdata hm with
   None->raise(Non_registered_module(hm))
  |Some(dt)->
   let bel=Modify_md_list.below mdata hm in
    if bel=[]
    then let (answer,short_paths)=German_unregister_module.on_targets root_dir (mdata,tgts) hm in
         let sfn=Half_dressed_module.to_shortened_string hm in
         let _=Image.image
         (fun edg->
          let cmd="rm -f _build/"^sfn^edg in
          Unix_command.uc(cmd))
         [".cm*";".d.cm*";".caml_debuggable"] in
         let temp1=Image.image (fun t->
            Absolute_path.of_string(Root_directory.join root_dir t)
         ) short_paths in
         let _=Image.image (German_forget_unregistered_file.forget root_dir) temp1 in
         (answer,short_paths)
    else raise(ModuleWithDependencies(hm,bel));;
