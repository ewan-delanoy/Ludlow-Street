(*

#use"Country/Germany/german_forget_module.ml";;

*)

    
exception ModuleWithDependencies of 
	Half_dressed_module.t*(Naked_module_t.t list);;
exception Non_registered_module of Naked_module_t.t;;

let on_targets root_dir (wmdata,dirs,tgts) hm=
  let nm=Half_dressed_module.naked_module hm in
  match Modify_md_list.seek_module_index  wmdata nm with
   None->raise(Non_registered_module(nm))
  |Some(dt)->
   let bel=Modify_md_list.below wmdata hm in
    if bel=[]
    then let (answer,short_paths)=German_unregister_module.on_targets root_dir 
                    (wmdata,tgts) hm in
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
