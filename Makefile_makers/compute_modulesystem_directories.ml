(*

#use"Makefile_makers/compute_modulesystem_directories.ml";;

*)


let individual_directory hm=
       let s_hm=Half_dressed_module.to_string hm in
       let s_dir=Father_and_son.father s_hm '/' in
       Subdirectory.of_string s_dir;;

let compute_modulesystem_directories fs=
  let temp1=Modulesystem.all_filedata fs in
  let temp2=Image.image (
    fun dt->
       let hm=Modulesystem_data.name dt in
       individual_directory hm
  ) temp1 in
  let temp3=Tidel.diforchan(temp2) in
  Ordered.forget_order temp3;;
 
