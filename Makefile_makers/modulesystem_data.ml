
(* 

Gathers all (ml/mli/mll/mly) corresponding to the same module.

#use"Makefile_makers/modulesystem_data.ml";;

*)


 

type t={
    name : Half_dressed_module.t;
    ml_present  : bool;
    mli_present : bool;
    mll_present : bool;
    mly_present : bool;
    ml_modification_time : float;
    mli_modification_time : float;
    mll_modification_time : float;
    mly_modification_time : float;
    needed_libraries : Ocaml_library.t list;
    direct_fathers : Half_dressed_module.t list;
    all_ancestors : Half_dressed_module.t list;
    needed_directories : Subdirectory.t list;
    inactivity_count : int
};;
   
   

let name x=x.name;;
let ml_present x=x.ml_present;;
let mli_present x=x.mli_present;;
let mll_present x=x.mll_present;;
let mly_present x=x.mly_present;;
let presences x=(x.ml_present,x.mli_present,x.mll_present,x.mly_present);;
let ml_modification_time x=x.ml_modification_time;;
let mli_modification_time x=x.mli_modification_time;;
let mll_modification_time x=x.mll_modification_time;;
let mly_modification_time x=x.mly_modification_time;;
let needed_libraries x=x.needed_libraries;;
let direct_fathers x=x.direct_fathers;;
let all_ancestors x=x.all_ancestors;;
let needed_directories x=x.needed_directories;;
let inactivity_count x=x.inactivity_count;;

let modification_time x edg=
  Ocaml_ending.pattern_matching
  (ml_modification_time x)
  (mli_modification_time x)
  (mll_modification_time x)
  (mly_modification_time x) edg;;

let modification_times x=
  (
   x.ml_modification_time,
   x.mli_modification_time,
   x.mll_modification_time,
   x.mly_modification_time
  );;

let make (nam,mlp,mlip,mllp,mlyp,mlmt,mlimt,mllmt,mlymt,libned,dirfath,allanc,dirned,inac)=
  {
    name=nam;
    ml_present=mlp;
    mli_present=mlip;
    mll_present=mllp;
    mly_present=mlyp;
    ml_modification_time=mlmt;
    mli_modification_time=mlimt;
    mll_modification_time=mllmt;
    mly_modification_time=mlymt;
    needed_libraries=libned;
    direct_fathers=dirfath;
    all_ancestors=allanc;
    needed_directories=dirned;
    inactivity_count=inac

};;

let compact_make (dir,nam,mlp,mlip,mllp,mlyp,mlmt,mlimt,mllmt,mlymt,libned,dirfath,allanc,dirned,inac)=
  make (Half_dressed_module.of_string_and_root nam dir,
  		mlp,mlip,mllp,mlyp,
  		mlmt,mlimt,mllmt,mlymt,
  		Image.image Ocaml_library.of_string libned,
  		Image.image (fun s->Half_dressed_module.of_string_and_root s dir) dirfath,
  		Image.image (fun s->Half_dressed_module.of_string_and_root s dir) allanc,
  		Image.image Subdirectory.of_string dirned,inac);;

let make_ml_present x=
 {
    name=x.name;
    ml_present=true;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count

};;

let make_mli_present x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=true;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count


};;

let make_mll_present x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=true;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count


};;


let make_mly_present x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=true;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count


};;




let make_ml_absent x=
 {
    name=x.name;
    ml_present=false;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count


};;
  
let make_mli_absent x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=false;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count


};;

let make_mll_absent x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=false;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count


};;


let make_mly_absent x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=false;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count


};;  
  
let check_presence ending dt=
   Ocaml_ending.pattern_matching 
    dt.ml_present
    dt.mli_present
    dt.mll_present
    dt.mly_present
    ending;; 

let make_presence ending dt=
   Ocaml_ending.pattern_matching 
    (make_ml_present dt)
    (make_mli_present dt)
    (make_mll_present dt)
    (make_mly_present dt)
    ending;;  
  
let make_absence ending dt=
   Ocaml_ending.pattern_matching 
    (make_ml_absent dt)
    (make_mli_absent dt)
    (make_mll_absent dt)
    (make_mly_absent dt)
    ending;;    

let acolytes dt=
  let name=dt.name in
  Option.filter_and_unpack (fun 
    edg->
       if check_presence edg dt 
       then Some(Mlx_filename.join name edg)
       else None
  ) Ocaml_ending.all_endings;;
  

let registered_endings dt=
  List.filter (fun edg->
    check_presence edg dt 
  ) Ocaml_ending.all_endings;;

let compute_modification_times hm=
  let dir=Half_dressed_module.root_directory hm in
  Ocaml_ending.exhaustive_uple (fun edg->
    let mlx=Mlx_filename.join hm edg in
    let file=(Directory_name.to_string dir)^(Mlx_filename.to_string mlx) in
    if not(Sys.file_exists file) then 0. else
    let st=Unix.stat file in
    st.Unix.st_mtime 
  );;

let rename1 new_name x=
   let (ml_mt,mli_mt,mly_mt,mll_mt)=compute_modification_times new_name in
   {
    name=new_name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=ml_mt;
    mli_modification_time=mli_mt;
    mll_modification_time=mll_mt;
    mly_modification_time=mly_mt;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count


   };;
   
let rename (old_name,new_name) x=
  if x.name=old_name
  then rename1 new_name x
  else if not(List.mem old_name (x.all_ancestors)) 
       then x
       else 
       let renamer=(fun t->if t=old_name then new_name else t) in
       let renamed_fathers=Image.image renamer x.direct_fathers
       and renamed_ancestors=Image.image renamer x.all_ancestors in
       let renamed_directories=Option.filter_and_unpack(
         fun mlx->
         let s=Half_dressed_module.to_string mlx in
         if String.contains s '/' 
         then Some(Subdirectory.of_string(Father_and_son.father s '/') )
         else None
       ) renamed_ancestors in
       {
    		name=x.name;
    		ml_present=x.ml_present;
   			mli_present=x.mli_present;
    		mll_present=x.mll_present;
    		mly_present=x.mly_present;
    		ml_modification_time=x.ml_modification_time;
    		mli_modification_time=x.mli_modification_time;
    		mll_modification_time=x.mll_modification_time;
    		mly_modification_time=x.mly_modification_time;
    		needed_libraries=x.needed_libraries;
    		direct_fathers=renamed_fathers;
    		all_ancestors=renamed_ancestors;
    		needed_directories=renamed_directories;
    		inactivity_count=x.inactivity_count

	   };;
       
let update_anclibdir changer l_data x=
   if not(List.mem changer.name (x.all_ancestors))
   then x
   else 
   let (anc,llib,dir)=(changer.all_ancestors,changer.needed_libraries,changer.needed_directories) in
   let new_ancestors=Option.filter_and_unpack(
     fun fd->
       let hm=name fd in
       if (List.mem hm x.all_ancestors)||(List.mem hm anc)
       then Some(hm)
       else None
   ) l_data in
   let new_lib=List.filter (
      fun lib->(List.mem lib llib)||(List.mem lib x.needed_libraries)
   ) Ocaml_library.all_libraries in
   let temp1=Option.filter_and_unpack(
     fun hm->
       let s_hm=Half_dressed_module.to_string hm in
       let s_dir=Father_and_son.father s_hm '/' in
       if s_dir="" then None else
       Some(Subdirectory.of_string s_dir)
   )  new_ancestors in
  let new_dir=Ordered.forget_order(Tidel.diforchan temp1) in
   {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=new_lib;
    direct_fathers=x.direct_fathers;
    all_ancestors=new_ancestors;
    needed_directories=new_dir;
    inactivity_count=x.inactivity_count


   };;       
       
let is_optional x=Half_dressed_module.is_optional(x.name);;
let is_not_optional x=not(is_optional x);;

let is_executable x=Half_dressed_module.is_executable(x.name);;
let is_not_executable x=not(is_executable x);;

let compute_needed_directories l_md=
  let temp1=Image.image(
     fun md->Tidel.safe_set(md.needed_directories)
  ) l_md in
  let temp2=Tidel.big_teuzin temp1 in
  Ordered.forget_order temp2;;
  
let compute_needed_libraries l_md=
  List.filter 
  (
   fun lib->
   List.exists(fun md->List.mem lib md.needed_libraries)
     l_md
  ) 
  Ocaml_library.all_libraries;;  
  
let outdated_acolytes dt=
  let hm=dt.name in
  let (n_ml,n_mli,n_mll,n_mly)=compute_modification_times hm in
  let temp1=[
    Ocaml_ending.mll,dt.mll_modification_time,n_mll;
    Ocaml_ending.mly,dt.mly_modification_time,n_mly;
    Ocaml_ending.ml ,dt.ml_modification_time,n_ml  ;
    Ocaml_ending.mli,dt.mli_modification_time,n_mli;
  ] in
  Option.filter_and_unpack (
    fun (edg,x,y)->
      if x<>y
      then Some(Mlx_filename.join hm edg)
      else None
  ) temp1;;
 
let is_outdated  dt=((outdated_acolytes  dt)<>[]);;


let force_ml_modification_time x new_val=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=new_val;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count


};;


let force_mli_modification_time x new_val=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=new_val;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count


};;

let force_mll_modification_time x new_val=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=new_val;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count


};;

let force_mly_modification_time x new_val=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=new_val;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count


};;

let force_modification_time x edg new_val=
  Ocaml_ending.pattern_matching
   (force_ml_modification_time x new_val) 
   (force_mli_modification_time x new_val) 
   (force_mll_modification_time x new_val) 
   (force_mly_modification_time x new_val) 
   edg;;

let increment_inactivity_count x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=(x.inactivity_count+1)
};;

let reset_inactivity_count x=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
    inactivity_count=0
};;

let fix_ancestors_and_libs_and_dirs x anc=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=compute_needed_libraries(x::anc);
    direct_fathers=x.direct_fathers;
    all_ancestors=Image.image (fun md->md.name) anc;
    needed_directories=compute_needed_directories(x::anc);
    inactivity_count=x.inactivity_count
};;

let fix_ancestors x anc=
 {
    name=x.name;
    ml_present=x.ml_present;
    mli_present=x.mli_present;
    mll_present=x.mll_present;
    mly_present=x.mly_present;
    ml_modification_time=x.ml_modification_time;
    mli_modification_time=x.mli_modification_time;
    mll_modification_time=x.mll_modification_time;
    mly_modification_time=x.mly_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=anc;
    needed_directories=x.needed_directories;
    inactivity_count=x.inactivity_count
};;




let needed_dirs_and_libs is_optimized dt=
   let extension=(if is_optimized then ".cmxa" else ".cma") in
   let dirs=String.concat(" ")
    (Image.image(fun y->let z=Subdirectory.to_string(y) in
     if z="" then "" else "-I "^z )
    dt.needed_directories)
	and libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    dt.needed_libraries) in
    String.concat " " ["";dirs;libs;""];;

let needed_dirs_and_libs_for_several is_optimized l_dt=
   let extension=(if is_optimized then ".cmxa" else ".cma") in
   let pre_dirs1=Image.image (fun dt->Tidel.diforchan(dt.needed_directories)) l_dt in
   let pre_dirs2=Ordered.forget_order (Tidel.big_teuzin pre_dirs1) in
   let dirs=String.concat(" ")
    (Image.image(fun y->let z=Subdirectory.to_string(y) in 
    if z="" then "" else "-I "^z )
    pre_dirs2) in
   let pre_libs1=Image.image (fun dt->Tidel.diforchan(dt.needed_libraries)) l_dt in
   let pre_libs2=Ordered.forget_order (Tidel.big_teuzin pre_libs1) in 
   let libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    pre_libs2) in
    String.concat " " ["";dirs;libs;""];;

let principal_mlx x=
   if x.mll_present then Mlx_filename.join x.name Ocaml_ending.mll else
   if x.mly_present then Mlx_filename.join x.name Ocaml_ending.mly else
   if x.ml_present then Mlx_filename.join x.name Ocaml_ending.ml else
   Mlx_filename.join x.name Ocaml_ending.mli;;
   
let principal_path x=Mlx_filename.to_path (principal_mlx x);;   

let unprefixed_compact_ocaml_name x=
   let enc=Strung.enclose in
   let (s,dir)=Half_dressed_module.unveil x.name in
  "("^
  (Directory_name.ocaml_name dir)^","^
  (enc(s))^","^
  (string_of_bool x.ml_present)^","^
  (string_of_bool x.mli_present)^","^
  (string_of_bool x.mll_present)^","^
  (string_of_bool x.mly_present)^","^
  (string_of_float x.ml_modification_time)^","^
  (string_of_float x.mli_modification_time)^","^
  (string_of_float x.mll_modification_time)^","^
  (string_of_float x.mly_modification_time)^","^
  "["^(String.concat ";" (Image.image (fun w->enc(Ocaml_library.to_string w)) x.needed_libraries))^"],"^
  "["^(String.concat ";" (Image.image (fun w->enc(Half_dressed_module.to_string w)) x.direct_fathers))^"],"^
  "["^(String.concat ";" (Image.image (fun w->enc(Half_dressed_module.to_string w)) x.all_ancestors))^"],"^
  "["^(String.concat ";" (Image.image (fun w->enc(Subdirectory.to_string w)) x.needed_directories))^"]"^
  ")";;    
  
let compact_ocaml_name x=
  "New_modulesystem"^"_data.compact_make"^
  (unprefixed_compact_ocaml_name x);;
     
  
let ocaml_name x=
   let (s,dir)=Half_dressed_module.unveil x.name in
  "New_modulesystem"^"_data.make("^
  (Directory_name.ocaml_name dir)^","^
  (Strung.enclose s)^","^
  (string_of_bool x.mli_present)^","^
  (string_of_bool x.mli_present)^","^
  (string_of_bool x.mll_present)^","^
  (string_of_bool x.mly_present)^","^
  (string_of_float x.ml_modification_time)^","^
  (string_of_float x.mli_modification_time)^","^
  (string_of_float x.mll_modification_time)^","^
  (string_of_float x.mly_modification_time)^","^
  "["^(String.concat ";" (Image.image Ocaml_library.ocaml_name x.needed_libraries))^"],"^
  "["^(String.concat ";" (Image.image Half_dressed_module.ocaml_name x.direct_fathers))^"],"^
  "["^(String.concat ";" (Image.image Half_dressed_module.ocaml_name x.all_ancestors))^"],"^
  "["^(String.concat ";" (Image.image Subdirectory.ocaml_name x.needed_directories))^"]"^
  ")";;  

let directories_from_list l=
  let temp2=Image.image (
    fun dt->
       let hm=name dt in
       Half_dressed_module.subdirectory hm
  ) l in
  let temp3=Tidel.diforchan(temp2) in
  Ordered.forget_order temp3;;

  
let industrial_separator1=Industrial_separator.new_separator ();;  
let industrial_separator2=Industrial_separator.new_separator ();;    

  
  
let archive x=
   String.concat industrial_separator1
   [
     Half_dressed_module.archive x.name;
     string_of_bool x.ml_present;
     string_of_bool x.mli_present;
     string_of_bool x.mll_present;
     string_of_bool x.mly_present;
     string_of_float x.ml_modification_time;
     string_of_float x.mli_modification_time;
     string_of_float x.mll_modification_time;
     string_of_float x.mly_modification_time;
     Nonblank.make(String.concat industrial_separator2 (Image.image Ocaml_library.to_string x.needed_libraries));
     Nonblank.make(String.concat industrial_separator2 (Image.image Half_dressed_module.to_string x.direct_fathers));
     Nonblank.make(String.concat industrial_separator2 (Image.image Half_dressed_module.to_string x.all_ancestors));
     Nonblank.make(String.concat industrial_separator2 (Image.image Subdirectory.unveil x.needed_directories));
     string_of_int(x.inactivity_count)
   ];;

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator1) s in
   let v1=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1  9))
   and v2=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1 10))
   and v3=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1 11))
   and v4=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1 12)) in
   let hm=Half_dressed_module.unarchive(List.hd l1) in
   let dir=Half_dressed_module.root_directory hm in
{
    name = hm;
    ml_present  = bool_of_string(List.nth l1 1);
    mli_present = bool_of_string(List.nth l1 2);
    mll_present = bool_of_string(List.nth l1 3);
    mly_present = bool_of_string(List.nth l1 4);
    ml_modification_time = float_of_string(List.nth l1 5);
    mli_modification_time = float_of_string(List.nth l1 6);
    mll_modification_time = float_of_string(List.nth l1 7);
    mly_modification_time = float_of_string(List.nth l1 8);
    needed_libraries =Image.image Ocaml_library.of_string v1;
    direct_fathers = Image.image (fun s->Half_dressed_module.of_string_and_root s dir) v2;
    all_ancestors = Image.image (fun s->Half_dressed_module.of_string_and_root s dir) v3;
    needed_directories = Image.image Subdirectory.of_string v4;
    inactivity_count = int_of_string(List.nth l1 13);
};;
     
