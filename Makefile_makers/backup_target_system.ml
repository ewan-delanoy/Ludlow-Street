(*

#use"Makefile_makers/backup_target_system.ml";;

*)

let github_after_backup=ref(true);;

let backup ts destination_dir msg=
  let old_root=Target_system.root ts in
  let s_old_root=Directory_name.to_string old_root in
  let s_destination=Directory_name.to_string destination_dir in
  let main_copier=(fun ap->
    let fn=Directory_name.cut_beginning old_root (Absolute_path.to_string ap) in
    let new_file=s_destination^"/"^fn in
    let cmd="cp "^s_old_root^"/"^fn^" "^new_file  in
    if not(Sys.file_exists new_file)
    then Shell_command.do_and_notice_failure cmd
    else 
    let new_ap=Absolute_path.of_string new_file in
    if Io.read_whole_file(ap)<>Io.read_whole_file(new_ap)
    then Shell_command.do_and_notice_failure cmd
    else ()
  )  in
  let temp1=(Target_system.directories ts)@(Target_system.outside_directories ts) in
  let _=Image.image (fun sdir->
    let s_sdir=Subdirectory.to_string sdir in
    let cmd="mkdir -p "^s_destination^"/"^s_sdir in
    Shell_command.do_and_notice_failure cmd
  ) temp1 in
  let temp2=(Target_system.inside_files ts)@(Target_system.outside_files ts) in
  let _=Image.image main_copier temp2 in 
  let _=(
  if (!github_after_backup)
  then let cwd=Sys.getcwd() in
       let _=Sys.chdir s_destination in
       let _=Image.image 
       (fun s->Shell_command.do_and_notice_failure(
        "rm -rf "^s
       )) 
       (Target_system.recently_deleted ts)
       in
       let _=Image.image Shell_command.do_and_notice_failure
       [
         "git add *";
         "git commit -m \""^msg^"\"";
         "git push"
       ] in
       Sys.chdir cwd
  else ()
  ) in
  Modify_target_system.forget_recent_deletions ts;;

  
  









   
   
  