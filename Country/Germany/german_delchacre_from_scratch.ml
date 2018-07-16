(*

#use"Country/Germany/german_delchacre_from_scratch.ml";;

*)

let dfs (source_dir,dir_for_backup) mdata=
   let temp1=Modify_md_list.all_mlx_paths mdata in
   let temp3=temp1 in
   let temp4=Image.image (fun ap->Root_directory.cut_beginning 
    source_dir (Absolute_path.to_string ap)) temp3 in
  Prepare_dircopy_update.compute_diff
     (source_dir,temp4) dir_for_backup;;
 






   
   
  