(*

#use"Country/Germany/german_delchacre_from_scratch.ml";;

*)

let dfs mdata=
   let temp1=Modify_md_list.all_mlx_paths mdata in
   let temp3=temp1 in
   let temp4=Image.image (fun ap->Root_directory.cut_beginning 
    (German_constant.root) (Absolute_path.to_string ap)) temp3 in
  let destination_dir=German_constant.dir_for_backup in
  Prepare_dircopy_update.compute_diff
     (German_constant.root,temp4) destination_dir;;
 






   
   
  