
(* 

#use"Country/Alaska/alaskan_register_mlx_file.ml";;


*)



  
let on_targets (old_mdata,old_dirs,old_tgts) mlx=
  let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
  let new_dir=Half_dressed_module.subdirectory hm in
 let new_mdata=Md_list.register_mlx_file_on_monitored_modules old_mdata mlx in
 let new_dirs=
 (if List.mem new_dir old_dirs then old_dirs else old_dirs@[new_dir] )
 and new_tgts=
 (*
       The only outdated targets are the targets 
       corresponding to an identical module
       (for example when a mll or mly is added to
       an already registered ml) 
        *)
       List.filter (
        fun tgt->match Ocaml_target.main_module tgt with
                 None->true
                 |Some(hm2)->hm2<>hm
       ) old_tgts
  in
  (new_mdata,new_dirs,new_tgts);; 
 

