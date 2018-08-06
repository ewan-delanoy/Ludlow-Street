
(* 

Gathers all (ml/mli/mll/mly) corresponding to the same module.

#use"Makefile_makers/modulesystem_data.ml";;

*)


 

type t={
    name : Half_dressed_module.t;
    principal_ending : Ocaml_ending.t;
    mli_registered : bool;
    principal_modification_time : float;
    mli_modification_time : float;
    needed_libraries : Ocaml_library.t list;
    direct_fathers : Half_dressed_module.t list;
    all_ancestors : Half_dressed_module.t list;
    needed_directories : Subdirectory_t.t list;
};;
   
   

let name x=x.name;;
let principal_ending x=x.principal_ending;;
let mli_registered x=x.mli_registered;;
let principal_modification_time x=x.principal_modification_time;;
let mli_modification_time x=x.mli_modification_time;;
let needed_libraries x=x.needed_libraries;;
let direct_fathers x=x.direct_fathers;;
let all_ancestors x=x.all_ancestors;;
let needed_directories x=x.needed_directories;;


let modification_time x edg=
    if edg=principal_ending x then x.principal_modification_time else
    if edg=Ocaml_ending.Mli then x.mli_modification_time else 0. ;;

let ml_modification_time x=modification_time x Ocaml_ending.Ml;;    
let mll_modification_time x=modification_time x Ocaml_ending.Mll;;
let mly_modification_time x=modification_time x Ocaml_ending.Mly;;


let ml_registered x=(x.principal_ending = Ocaml_ending.Ml);;
let mll_registered x=(x.principal_ending = Ocaml_ending.Mll);;
let mly_registered x=(x.principal_ending = Ocaml_ending.Mly);;

let presences x=
  (ml_registered x,x.mli_registered,mll_registered x,mly_registered x);;




let modification_times x=
  (
   ml_modification_time x,
   mli_modification_time x,
   mll_modification_time x,
   mly_modification_time x
  );;

let make (nam,prend,mlip,prmt,mlimt,libned,dirfath,allanc,dirned)=
  {
    name=nam;
    principal_ending=prend;
    mli_registered=mlip;
    principal_modification_time=prmt;
    mli_modification_time=mlimt;
    needed_libraries=libned;
    direct_fathers=dirfath;
    all_ancestors=allanc;
    needed_directories=dirned;

};;

let compact_make (dir,namp,prend,mlip,prmt,mlimt,libned,dirfath,allanc,dirned)=
  make (Half_dressed_module.of_string_and_root namp dir,
      Ocaml_ending.of_string prend,
      mlip,
  		prmt,mlimt,
  		Image.image Ocaml_library.of_string libned,
  		Image.image (fun s->Half_dressed_module.of_string_and_root s dir) dirfath,
  		Image.image (fun s->Half_dressed_module.of_string_and_root s dir) allanc,
  		Image.image Subdirectory.of_string dirned);;
  
let check_registration ending dt=match ending with
   Ocaml_ending.Ml->ml_registered dt
  |Ocaml_ending.Mli->dt.mli_registered
  |Ocaml_ending.Mll->mll_registered dt
  |Ocaml_ending.Mly->mly_registered dt;;  
  
let make_registration ending x=
   {
    name=x.name;
    principal_ending=if ending=Ocaml_ending.mli then x.principal_ending else ending;
    mli_registered=if ending=Ocaml_ending.mli then true else x.mli_registered;
    principal_modification_time=x.principal_modification_time;
    mli_modification_time=x.mli_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;
   };;

let make_unregistration ending x=
    {
     name=x.name;
     principal_ending=if ending=Ocaml_ending.mli then x.principal_ending else Ocaml_ending.mli;
     mli_registered=if ending=Ocaml_ending.mli then false else x.mli_registered;
     principal_modification_time=x.principal_modification_time;
     mli_modification_time=x.mli_modification_time;
     needed_libraries=x.needed_libraries;
     direct_fathers=x.direct_fathers;
     all_ancestors=x.all_ancestors;
     needed_directories=x.needed_directories;
    };;   

  

let acolytes dt=
  let name=dt.name in
  Option.filter_and_unpack (fun 
    edg->
       if check_registration edg dt 
       then Some(Mlx_ended_absolute_path.join name edg)
       else None
  ) Ocaml_ending.all_endings;;
  

let registered_endings dt=
  List.filter (fun edg->
    check_registration edg dt 
  ) Ocaml_ending.all_endings;;

let short_paths dt=Image.image Mlx_ended_absolute_path.short_path (acolytes dt);;
  

let compute_modification_times hm=
  let dir=Half_dressed_module.bundle_main_dir hm in
  Ocaml_ending.exhaustive_uple (fun edg->
    let mlx=Mlx_ended_absolute_path.join hm edg in
    let file=(Root_directory.connectable_to_subpath dir)^(Mlx_ended_absolute_path.to_string mlx) in
    if not(Sys.file_exists file) then 0. else
    let st=Unix.stat file in
    st.Unix.st_mtime 
  );;

let associated_modification_time  (ml_mt,mli_mt,mly_mt,mll_mt) edg=match edg with
 Ocaml_ending.Ml->ml_mt
|Ocaml_ending.Mli->mli_mt
|Ocaml_ending.Mll->mll_mt
|Ocaml_ending.Mly->mly_mt;;  

let rename1 new_name x=
   let (ml_mt,mli_mt,mly_mt,mll_mt)=compute_modification_times new_name in
   let pr_mt=associated_modification_time (ml_mt,mli_mt,mly_mt,mll_mt) (principal_ending x) in
   {
    name=new_name;
    principal_ending=x.principal_ending;
    mli_registered=x.mli_registered;
    principal_modification_time=pr_mt;
    mli_modification_time=mli_mt;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

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
         let s=Half_dressed_module.uprooted_version mlx in
         if String.contains s '/' 
         then Some(Subdirectory.of_string(Father_and_son.father s '/') )
         else None
       ) renamed_ancestors in
       {
        name=x.name;
        principal_ending=x.principal_ending;
   			mli_registered=x.mli_registered;
    		principal_modification_time=x.principal_modification_time;
    		mli_modification_time=x.mli_modification_time;
    		needed_libraries=x.needed_libraries;
    		direct_fathers=renamed_fathers;
    		all_ancestors=renamed_ancestors;
    		needed_directories=renamed_directories;
	   };;
       
let tool_in_update_anclibdir (new_anc,new_lib,new_dir) x=
  {
    name=x.name;
    principal_ending=x.principal_ending;
    mli_registered=x.mli_registered;
    principal_modification_time=x.principal_modification_time;
    mli_modification_time=x.mli_modification_time;
    needed_libraries=new_lib;
    direct_fathers=x.direct_fathers;
    all_ancestors=new_anc;
    needed_directories=new_dir;
   };;       


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
    Ocaml_ending.mll,mll_modification_time dt,n_mll;
    Ocaml_ending.mly,mly_modification_time dt,n_mly;
    Ocaml_ending.ml ,ml_modification_time dt,n_ml  ;
    Ocaml_ending.mli,mli_modification_time dt,n_mli;
  ] in
  Option.filter_and_unpack (
    fun (edg,x,y)->
      if x<>y
      then Some(Mlx_ended_absolute_path.join hm edg)
      else None
  ) temp1;;
 
let is_outdated  dt=((outdated_acolytes  dt)<>[]);;


let force_principal_modification_time x new_val=
  let pr_end=principal_ending x in
  let new_mli_val=(if pr_end=Ocaml_ending.Mli then new_val else x.mli_modification_time) in
  {
    name=x.name;
    principal_ending=x.principal_ending;
    mli_registered=x.mli_registered;
    principal_modification_time=new_val;
    mli_modification_time=new_mli_val;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;


let force_mli_modification_time x new_val=
   let pr_end=principal_ending x in
   let new_pr_val=(if pr_end=Ocaml_ending.Mli then new_val else x.principal_modification_time) in
 {
    name=x.name;
    principal_ending=x.principal_ending;
    mli_registered=x.mli_registered;
    principal_modification_time=new_pr_val;
    mli_modification_time=new_val;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=x.all_ancestors;
    needed_directories=x.needed_directories;

};;


let force_modification_time x edg new_val=
if edg=principal_ending x then force_principal_modification_time x new_val else
if edg=Ocaml_ending.Mli then force_mli_modification_time  x new_val else x;;


let fix_ancestors_and_libs_and_dirs x anc=
 {
    name=x.name;
    principal_ending=x.principal_ending;
    mli_registered=x.mli_registered;
    principal_modification_time=x.principal_modification_time;
    mli_modification_time=x.mli_modification_time;
    needed_libraries=compute_needed_libraries(x::anc);
    direct_fathers=x.direct_fathers;
    all_ancestors=Image.image (fun md->md.name) anc;
    needed_directories=compute_needed_directories(x::anc);
};;

let fix_ancestors x anc=
 {
    name=x.name;
    principal_ending=x.principal_ending;
    mli_registered=x.mli_registered;
    principal_modification_time=x.principal_modification_time;
    mli_modification_time=x.mli_modification_time;
    needed_libraries=x.needed_libraries;
    direct_fathers=x.direct_fathers;
    all_ancestors=anc;
    needed_directories=x.needed_directories;
};;




let needed_dirs_and_libs is_optimized dt=
   let extension=(if is_optimized then ".cmxa" else ".cma") in
   let s_root=Root_directory.connectable_to_subpath(
        Half_dressed_module.bundle_main_dir(name dt)
   ) in
   let dirs=
   "-I "^s_root^"_build"
  and libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    dt.needed_libraries) in
    String.concat " " ["";dirs;libs;""];;

let needed_dirs_and_libs_for_several is_optimized l_dt=
   let extension=(if is_optimized then ".cmxa" else ".cma") in
   let pre_dirs1=Image.image (fun dt->Tidel.diforchan(dt.needed_directories)) l_dt in
   let pre_dirs2=Ordered.forget_order (Tidel.big_teuzin pre_dirs1) in
   let dirs=String.concat(" ")
    (Image.image(fun y->let z=Subdirectory.connectable_to_subpath(y) in 
    if z="" then "" else "-I "^z )
    pre_dirs2) in
   let pre_libs1=Image.image (fun dt->Tidel.diforchan(dt.needed_libraries)) l_dt in
   let pre_libs2=Ordered.forget_order (Tidel.big_teuzin pre_libs1) in 
   let libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    pre_libs2) in
    String.concat " " ["";dirs;libs;""];;



let principal_mlx x=
  Mlx_ended_absolute_path.join x.name (principal_ending x);;
   
let principal_path x=Mlx_ended_absolute_path.to_path (principal_mlx x);;  



let ml_path x=Mlx_ended_absolute_path.to_path (Mlx_ended_absolute_path.join x.name Ocaml_ending.ml);;   

let rename_endsubdirectory (old_subdir,new_subdirname) x=
    let ren=Half_dressed_module.rename_endsubdirectory (old_subdir,new_subdirname) 
    and ren_sub=Subdirectory.rename_endsubdirectory (old_subdir,new_subdirname) in
{                                                                 
      name = ren (x.name);
      principal_ending=x.principal_ending;
      mli_registered = x.mli_registered;
      principal_modification_time =  x.principal_modification_time;
      mli_modification_time = x.mli_modification_time;
      needed_libraries = x.needed_libraries;
      direct_fathers = Image.image ren x.direct_fathers;
      all_ancestors = Image.image ren x.all_ancestors;
      needed_directories = Image.image ren_sub x.needed_directories;
};;

let directories_from_list l=
  let temp2=Image.image (
    fun dt->
       let hm=name dt in
       Half_dressed_module.subdirectory hm
  ) l in
  let temp3=Tidel.diforchan(temp2) in
  Ordered.forget_order temp3;;

  
let industrial_separator1=Industrial_separator.modulesystem_data1;;  
let industrial_separator2=Industrial_separator.modulesystem_data2;;    


let archive x=
   String.concat industrial_separator1
   [
     Half_dressed_module.archive x.name;
     Ocaml_ending.to_string x.principal_ending;
     string_of_bool x.mli_registered;
     string_of_float x.principal_modification_time;
     string_of_float x.mli_modification_time;
     Nonblank.make(String.concat industrial_separator2 (Image.image Ocaml_library.to_string x.needed_libraries));
     Nonblank.make(String.concat industrial_separator2 (Image.image Half_dressed_module.uprooted_version x.direct_fathers));
     Nonblank.make(String.concat industrial_separator2 (Image.image Half_dressed_module.uprooted_version x.all_ancestors));
     Nonblank.make(String.concat industrial_separator2 (Image.image Subdirectory.without_trailing_slash x.needed_directories));
   ];;

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator1) s in
   let nd=(function k->Nonblank.decode(List.nth l1 k)) in
   let v1=Str.split (Str.regexp_string industrial_separator2) (nd 5)
   and v2=Str.split (Str.regexp_string industrial_separator2) (nd 6)
   and v3=Str.split (Str.regexp_string industrial_separator2) (nd 7)
   and v4=Str.split (Str.regexp_string industrial_separator2) (nd 8) in
   let hm=Half_dressed_module.unarchive(List.hd l1) in
   let dir=Half_dressed_module.bundle_main_dir hm in
{
    name = hm;
    principal_ending = Ocaml_ending.of_string(List.nth l1 1);
    mli_registered  = bool_of_string(List.nth l1 2);
    principal_modification_time = float_of_string(List.nth l1 3);
    mli_modification_time = float_of_string(List.nth l1 4);
    needed_libraries =Image.image Ocaml_library.of_string v1;
    direct_fathers = Image.image (fun s->Half_dressed_module.of_string_and_root s dir) v2;
    all_ancestors = Image.image (fun s->Half_dressed_module.of_string_and_root s dir) v3;
    needed_directories = Image.image Subdirectory.of_string v4;
};;
     
