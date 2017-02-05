(*

#use"Country/Germany/german_backup_target_system.ml";;

*)



let ref_for_backup=ref((None:Prepare_dircopy_update.diff option));;

let prepare_backup ()=
   if ((!ref_for_backup)<>None)
   then Option.unpack(!ref_for_backup)
   else
   let temp1=German_data.all_mlx_paths (German_wrapper.data()) in
   let temp2=German_wrapper.outside_files () in
   let temp3=temp1@temp2 in
   let n1=String.length(Directory_name.to_string(German_constant.root)) in
   let temp4=Image.image (fun ap->Cull_string.cobeginning n1
  (Absolute_path.to_string ap)) temp3 in
  let destination_dir=Directory_name.of_string German_constant.location_for_backup in
  let answer=Prepare_dircopy_update.compute_diff
     (German_constant.root,temp4) destination_dir in
  let _=(ref_for_backup:=Some(answer)) in
  answer;;

let github_after_backup=ref(true);;

let commands_for_backup ()=
   let temp1=prepare_backup () in
   if Prepare_dircopy_update.diff_is_empty temp1
   then ([],[])
   else 
   let destination_dir=Directory_name.of_string German_constant.location_for_backup in
   let s_destination=Directory_name.to_string destination_dir in
   let created_ones=Prepare_dircopy_update.recently_created temp1 in
   let temp2=Option.filter_and_unpack
   (fun fn->
     if String.contains fn '/'
     then let dn=Father_and_son.father fn '/' in
          Some("mkdir -p "^s_destination^dn)
     else None 
    )
   created_ones in
   let temp3=Ordered.forget_order(Ordered_string.diforchan temp2) in
   let s_source=Directory_name.to_string German_constant.root in
   let temp4=Image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^(Father_and_son.father fn '/')
   ) created_ones in
   let changed_ones=Prepare_dircopy_update.recently_changed temp1 in
   let temp5=Image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^fn
   ) changed_ones in
   let temp6=Image.image(
      fun fn->
      "git add "^fn
   ) (created_ones@changed_ones) in
   let temp7=Image.image(
      fun fn->
      "git rm "^fn
   ) (Prepare_dircopy_update.recently_deleted temp1) in
   (temp3@temp4@temp5,temp6@temp7);;

let backup msg=
  let destination_dir=Directory_name.of_string German_constant.location_for_backup 
  and (nongit_cmds,git_cmds)=commands_for_backup() in
  let s_destination=Directory_name.to_string destination_dir in
  let _=Image.image Shell_command.do_and_notice_failure nongit_cmds in
  let _=(
  if (!github_after_backup)
  then let cwd=Sys.getcwd() in
       let _=Sys.chdir s_destination in
       let _=Image.image Shell_command.do_and_notice_failure git_cmds in
       let _=Image.image Shell_command.do_and_notice_failure
       [
         "git commit -m \""^msg^"\"";
         "git push"
       ] in
       Sys.chdir cwd
  else ()
  ) in
  (ref_for_backup:=None);;

  
  









   
   
  