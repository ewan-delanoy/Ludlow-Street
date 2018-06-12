
(* 


#use"Country/Germany/german_unregister_mlx_file.ml";;


*)


 
    
 let on_targets (old_mdata,old_tgts) mlx=
  let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
  let new_mdata=Md_list.unregister_mlx_file_on_monitored_modules old_mdata mlx in
  let new_dirs=Md_list.compute_subdirectories_list new_mdata
  and new_tgts=List.filter (fun tgt->
   	match Ocaml_target.main_module tgt with
   	None->false |Some(hm2)->hm2<>hm
  ) old_tgts in
  let default_top=(Md_list.default_toplevel German_constant.main_toplevel_name  new_mdata) in
  let (new_mdata2,new_tgts2,_)=
    snd(Alaskan_make_ocaml_target.make
      German_constant.root
     (new_mdata,new_tgts,[]) default_top) in
  (new_mdata2,new_dirs,new_tgts2);;   
  
