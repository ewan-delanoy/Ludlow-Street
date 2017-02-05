(*

#use"Makefile_makers/implement_modulesystem.ml";;

*)

type directory_to_be=string;;

let print_message s=(print_string (s^"\n");flush stdout);;

let implement fs (root_to_be:directory_to_be)=
  let s_old_root=Directory_name.to_string(Modulesystem.root fs) in
  let _=print_message "Creating new root directory ..." in
  let cmd1="mkdir -p "^root_to_be in
  let _=Sys.command cmd1 in
  let _=print_message "New root directory exists now." in
  let new_root_dir=Directory_name.of_string root_to_be in
  let s_new_root=Directory_name.to_string new_root_dir in
  let simple_copy=(fun fn->
    let cmd="cp "^s_old_root^"/"^fn^" "^s_new_root^"/"^fn  in
    Sys.command cmd
  )  in
  let careful_copy=(fun fn->
     let _=simple_copy fn in
     let new_path=Absolute_path.of_string(s_new_root^"/"^fn) in
     Replace_inside.replace_inside_file
        (s_old_root,s_new_root) new_path
  ) in
  let temp1=Modulesystem.all_mlx_files fs in
  let temp2=Image.image Mlx_filename.to_string temp1 in
  let temp3="Remembered"::"Forgotten"::(Modulesystem.local_directories fs) in
  let _=print_message "Copying files ..." in
  let _=Image.image (fun sdir->
    let cmd="mkdir -p "^s_new_root^"/"^sdir in
    Sys.command cmd
  ) temp3 in
  let _=Explicit.image simple_copy temp2 in
  let _=List.iter careful_copy 
   [".ocamlinit";"Remembered/aztec.ml";
    "my_loadings.ml";"my_pervasives.ml";
    "Global_variables/current_root_directory.ml"] in
  let _=print_message "Files copied." in
  ();;









   
   
  