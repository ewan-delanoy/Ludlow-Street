
(* 


#use"Country/Germany/german_unregister_mlx_file.ml";;


*)


 
    
 let on_targets (old_mdata,old_tgts) mlx=
  let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
  let new_mdata=Modify_md_list.unregister_mlx_file_on_monitored_modules old_mdata mlx in
  let new_dirs=Modify_md_list.compute_subdirectories_list new_mdata
  and new_tgts=List.filter (fun tgt->
   	match Ocaml_target.main_module tgt with
   	None->false |Some(hm2)->hm2<>hm
  ) old_tgts in
  let (new_mdata2,new_tgts2,_)=Alaskan_make_ocaml_target.feydeau
  German_constant.root new_mdata new_tgts  in
  (new_mdata2,new_dirs,new_tgts2);;   
  
