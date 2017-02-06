(*

#use"mlx_filename.ml";;

*)

type t=
  ML of string*Directory_name.t
 |MLI of string*Directory_name.t
 |MLL of string*Directory_name.t
 |MLY of string*Directory_name.t;;

exception Unknown_ending of string;;
exception Unpointed_filename of string;;

exception Inexistent_filename of string;;

let unveil=function
   ML(s,dir)->(s^".ml",dir)
  |MLI(s,dir)->(s^".mli",dir)
  |MLL(s,dir)->(s^".mll",dir)
  |MLY(s,dir)->(s^".mly",dir);;
  
let short_path mlx=fst(unveil mlx);;  

let of_string_and_root s dir= 
  if not(String.contains s '.') then raise(Unpointed_filename(s)) else
  let (core,ending)=Father_and_son.father_and_son s '.' in
  let s_dir=Directory_name.to_string dir in
  if (not(Sys.file_exists(s_dir^s)))
  then raise(Inexistent_filename(s_dir^s))
  else
  if ending="ml"  then ML  (core,dir) else
  if ending="mli" then MLI (core,dir) else
  if ending="mll" then MLL (core,dir) else
  if ending="mly" then MLY (core,dir) else
  raise(Unknown_ending(s));;

let try_from_string_and_root s dir=
  try (Some(of_string_and_root s dir)) with _->None;;

let to_string=function
   ML(s,dir)->(s^".ml")
  |MLI(s,dir)->(s^".mli")
  |MLL(s,dir)->(s^".mll")
  |MLY(s,dir)->(s^".mly");;

let root=function
   ML(s,dir)->dir
  |MLI(s,dir)->dir
  |MLL(s,dir)->dir
  |MLY(s,dir)->dir;;

exception FileOutsideDirectory of Absolute_path.t*Directory_name.t;;


let of_path_and_root ap dir=
    if (not(Path_is_in_directory.path_is_in_directory ap dir))
    then raise(FileOutsideDirectory(ap,dir))
    else 
    let s_dir=Directory_name.to_string dir in
    let n_dir=String.length s_dir in
    let subpath=Cull_string.cobeginning n_dir (Absolute_path.to_string ap) in
    of_string_and_root subpath dir;;    

let try_from_path_and_root ap dir=
    try (Some(of_path_and_root ap dir)) with _->None;;

let decompose=function
   ML(s,dir)->(Half_dressed_module.of_string_and_root s dir,Ocaml_ending.Ml)
  |MLI(s,dir)->(Half_dressed_module.of_string_and_root s dir,Ocaml_ending.Mli)
  |MLL(s,dir)->(Half_dressed_module.of_string_and_root s dir,Ocaml_ending.Mll)
  |MLY(s,dir)->(Half_dressed_module.of_string_and_root s dir,Ocaml_ending.Mly);;

let half_dressed_core mlx=fst(decompose mlx);;
let ending mlx=snd(decompose mlx);;

let to_path mlx=
  let (hm,edg)=decompose mlx in
  let dir=root mlx in
  let s_hm=Half_dressed_module.to_string hm 
  and s_dir=Directory_name.to_string dir in
  Absolute_path.of_string( s_dir^s_hm^(Ocaml_ending.to_string edg) );;

let join hs ending=
  let (s,dir)=Half_dressed_module.unveil hs in
  Ocaml_ending.pattern_matching
  (ML (s,dir)) (MLI (s,dir)) (MLL (s,dir)) (MLY (s,dir)) ending;;
  
exception Failed_File_Renaming of t*string;;  
  
(*

notice that the ending is preserved below. If the user
unwittingly puts a wrong ending, this will have no effect.

*)  
  
let do_file_renaming mlx new_name=
  let core=Father_and_son.invasive_father (No_slashes.to_string new_name) '.' in
  let checked_name=No_slashes.of_string(core^(Ocaml_ending.to_string(ending mlx))) in
  let ap=to_path mlx in
  let new_ap=Rename_file.rename ap checked_name in
  of_path_and_root new_ap (root mlx);;   
  
let do_file_displacing mlx new_subdir=
  let s_new_subdir=Subdirectory.to_string new_subdir
  and dir=root mlx in
  let s_dir=Directory_name.to_string dir in
  let new_dir=Directory_name.of_string(s_dir^s_new_subdir) in
  let ap=to_path mlx in
  let new_ap=Relocate_file.relocate ap new_dir in
  of_path_and_root new_ap (root mlx);;  
  
  
let is_optional x=Half_dressed_module.is_optional(half_dressed_core x);;  
let is_archived x=Half_dressed_module.is_archived(half_dressed_core x);;  

let complete_ls dir=
  let temp1=Directory_name.to_string dir in
  let temp2=More_unix.quick_beheaded_complete_ls temp1 in
  let temp3=Option.filter_and_unpack(
     fun s->try_from_string_and_root s dir
  ) temp2 in
  List.filter (fun mlx->not(is_archived mlx)) temp3;;

let to_absolute_path mlx=
 let (s,dir)=unveil mlx in
 let s_dir=Directory_name.to_string dir in
 Absolute_path.of_string(s_dir^s);;   


let ocaml_name w=
  let (s,dir)=unveil w in
  "Mlx_file"^"name"^".of_string_and_index("^
  (Strung.enclose s)^
  ")("^(Directory_name.to_string dir)^")";;    

let industrial_separator1=Industrial_separator.new_separator ();;  
 


let prepare_archive=function
   ML(s,dir)->["ml";s;Directory_name.to_string dir]
  |MLI(s,dir)->["mli";s;Directory_name.to_string dir]
  |MLL(s,dir)->["mll";s;Directory_name.to_string dir]
  |MLY(s,dir)->["mly";s;Directory_name.to_string dir];;

  
exception Unrecognized_constructor of string;;   
  
let archive x=String.concat industrial_separator1 (prepare_archive x);;
 

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator1) s in
   let c=List.hd l1 and s=List.nth l1 1 and dir=Directory_name.of_string(List.nth l1 2) in
   if c="ml" then    ML(s,dir) else
   if c="mli" then  MLI(s,dir) else
   if c="mll" then  MLL(s,dir) else
   if c="mly" then  MLY(s,dir) else
   raise(Unrecognized_constructor(c));;
