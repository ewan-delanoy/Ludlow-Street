(*

#use"Country/Germany/german_forget_module.ml";;

*)

    
exception ModuleWithDependencies of 
	Half_dressed_module.t*(Half_dressed_module.t list);;
exception Non_registered_module of Half_dressed_module.t;;

let on_targets (mdata,dirs,tgts) hm=
  match German_data.find_module_registration mdata hm with
   None->raise(Non_registered_module(hm))
  |Some(dt)->
   let bel=German_data.below mdata hm in
    if bel=[]
    then let s_hm=Half_dressed_module.to_string hm in
         let fn=(Directory_name.to_string(German_constant.root))^s_hm in
         let _=Image.image
         (fun edg->Shell_command.do_and_notice_failure("rm -f "^fn^edg^"*"))
         [".cm";".d.cm";".caml_debuggable"] in
         let edgs=Modulesystem_data.registered_endings dt in
         let paths=Image.image (fun edg->Mlx_filename.to_path(Mlx_filename.join hm edg)) edgs in
         let (mdata2,dirs2,tgts2)=German_unregister_module.on_targets (mdata,tgts) hm in
         ((mdata2,dirs2,tgts2),Image.image German_forget_unregistered_file.on_recently_deleted_files paths)
    else raise(ModuleWithDependencies(hm,bel));;


