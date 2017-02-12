(*

#use"find_suitable_ending.ml";;

*)

(*

Note that the order in Ocaml_ending.correspondances is important


*)

exception No_suitable_location of Directory_name.t*(Subdirectory.t list)*string;;

let find_file_location dir l_subdir x=
  let new_x=String.uncapitalize x in
  let x_list=(if  new_x=x
    then [x]
    else [x;new_x]) in
  let s_dir=Directory_name.to_string(dir) in
  let original_endings=Ocaml_ending.all_string_endings in
  let endings=(
     if List.exists (fun edg->Substring.ends_with x edg) original_endings
     then [""]
     else original_endings
  ) in
  let temp1=Cartesian.tproduct(l_subdir) x_list endings in
  let tempf=(fun (sd,y,edg)->
  let s1=s_dir^(Subdirectory.to_string sd)^y^edg in
  if Sys.file_exists s1
  then Some(Absolute_path.of_string s1)
  else None
  ) in
  let opt=Option.find_and_stop tempf temp1 in
  if opt=None
  then raise(No_suitable_location(dir ,l_subdir,x))
  else  Option.unpack(opt);;