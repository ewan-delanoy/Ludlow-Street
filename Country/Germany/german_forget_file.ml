(*

#use"Country/Germany/german_forget_file.ml";;

*)

    
exception FileWithDependencies of 
	Mlx_ended_absolute_path.t*(Half_dressed_module.t list);;


let on_targets root_dir triple ap=
  let (mdata,dirs,tgts)=triple in
  let hm=Half_dressed_module.of_path_and_root ap root_dir 
  and mlx=Mlx_ended_absolute_path.of_path_and_root ap root_dir  in
  match Modify_md_list.find_module_registration mdata hm with
   None->triple
  |Some(_)->
   let bel=Modify_md_list.below mdata (Mlx_ended_absolute_path.half_dressed_core mlx) in
    if bel=[]
    then let s_hm=Half_dressed_module.uprooted_version hm in
         let fn=(Root_directory.connectable_to_subpath(root_dir))^s_hm in
         let _=Image.image
         (fun edg->Unix_command.uc("rm -f "^fn^edg^"*"))
         [".cm";".d.cm";".caml_debuggable"] in
         German_unregister_mlx_file.on_targets root_dir (mdata,tgts) mlx
    else raise(FileWithDependencies(mlx,bel));;
