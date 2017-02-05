
(* 

#use"Makefile_makers/modulesystem.ml";;


The only allowed combinations are .mll .ml or .mly .ml or .mli .ml.


*)




type t={
  root : Directory_name.t;
  data : Modulesystem_data.t list;
  main_toplevel_name:string;
};;


let make (dir,l,mtn)={root=dir;data=l;main_toplevel_name=mtn;};;


let from_root_and_toplevel dir opt=
  let mtn=(match opt with
   None->"ocaml_toplevel"
   |Some(s)->s
  ) in
  make (dir,[],mtn);;

let filedata_selector ending x=List.filter 
   (Modulesystem_data.check_presence ending) x.data;;

let root x=x.root;; 
let all_filedata x=x.data;;
let main_toplevel_name x=x.main_toplevel_name;;
let ml_filedata = filedata_selector Ocaml_ending.ml;;
let mli_filedata = filedata_selector Ocaml_ending.mli;;
let mll_filedata = filedata_selector Ocaml_ending.mll;;
let mly_filedata = filedata_selector Ocaml_ending.mly;;     
   
   

let all_mlx_files x=
  List.flatten
  (Image.image Modulesystem_data.acolytes x.data);; 
  
let all_mlx_paths x=Image.image Mlx_filename.to_absolute_path (all_mlx_files x);;  
  
let local_directories fs=
  let temp1=all_mlx_files fs in
  let temp5=Image.image Mlx_filename.to_string temp1 in
  let temp2=Image.image (fun s->Father_and_son.father s '/') temp5 in
  let temp3=Ordered_string.diforchan temp2 in
  let temp4=Ordered.forget_order temp3 in
  temp4;;

let find_module_registration fs hm=
  Option.find_it(fun a->Modulesystem_data.name a=hm) fs.data;;   
   
 let see_if_file_is_registered fs mlx=
    let hm=Mlx_filename.half_dressed_core mlx
    and edg=Mlx_filename.ending mlx in  
    match Option.find_it (fun a->Modulesystem_data.name a=hm) (fs.data) with
    None->false
    |Some(dt)->Modulesystem_data.check_presence edg dt;;
 
let check_presences fs hm=
    match Option.find_it (fun a->Modulesystem_data.name a=hm) (fs.data) with
    None->Ocaml_ending.exhaustive_uple (fun _->false)
    |Some(dt)->Ocaml_ending.exhaustive_uple 
     (fun edg->Modulesystem_data.check_presence edg dt);;
 
 let acolytes fs hm=
    match Option.find_it (fun a->Modulesystem_data.name a=hm) 
      (fs.data) with
     None->[]
    |Some(dt)->Modulesystem_data.acolytes dt;;    
   
 
 let outdated_files fs=
   let temp1=Image.image Modulesystem_data.outdated_acolytes fs.data in
   List.flatten temp1;;
 

   
 let descendants fs names=
    let temp1=List.filter(
      fun dt->List.exists (fun t->List.mem t names) 
        (Modulesystem_data.all_ancestors dt)
    )(fs.data) in
    temp1;;
    
let optionality_partition l=
  let (before,core,after)=Three_parts.select_center_element 
    Modulesystem_data.is_optional l in
  (before,Option.add_perhaps core after);;    

exception Deletability_issue of Mlx_filename.t;;

 let is_deletable fs mlxfile=
   let hm=Mlx_filename.half_dressed_core mlxfile in
   if (descendants fs [hm])<>[]
   then false
   else let edg=Mlx_filename.ending mlxfile in
        if List.mem edg [Ocaml_ending.ml;Ocaml_ending.mli]
        then true
        else 
        if List.mem edg [Ocaml_ending.mll;Ocaml_ending.mly]
        then let opt=Option.find_it (fun a->Modulesystem_data.name a=hm) fs.data in
             (
               match opt with
               None->true
               |Some(dt)->not(Modulesystem_data.ml_present dt)
             )
        else raise(Deletability_issue(mlxfile));; 
 
 let unregistered_mlx_files fs=
   let temp1=Mlx_filename.complete_ls (root fs) in
   List.filter (fun mlx->
     not(see_if_file_is_registered fs mlx)
   ) temp1;;
 
 let system_size x=List.length(x.data);;
 
exception  Non_registered_module of Half_dressed_module.t;;
 
let above fs hm=
   let files=all_filedata fs in
   match Option.find_it(fun dt->Modulesystem_data.name dt=hm) files with
    None->raise(Non_registered_module(hm))
   |Some(dt)->Modulesystem_data.all_ancestors dt;;
   
let below fs hm=
   let files=all_filedata fs in
   Option.filter_and_unpack(fun dt->
   if List.mem hm (Modulesystem_data.all_ancestors dt)
   then Some(Modulesystem_data.name dt)
   else None) files;;   
 
let directly_below fs hm=
   let files=all_filedata fs in
   Option.filter_and_unpack(fun dt->
   if List.mem hm (Modulesystem_data.direct_fathers dt)
   then Some(Modulesystem_data.name dt)
   else None) files;;    
 
let inactivity_report fs=
  let temp1=List.filter Modulesystem_data.is_not_optional fs.data in 
  let (_,m_min)=Min.minimize_it Modulesystem_data.inactivity_count temp1
  and (m_max,l_max)=Max.maximize_it_with_care Modulesystem_data.inactivity_count temp1 in
  let temp2=Image.image Modulesystem_data.name l_max in
  let temp3=List.filter (fun hm->directly_below fs hm=[]) temp2 in
  (m_max-m_min,(fun ()->temp3));;

let files_containing_string fs old_name=
   let temp1=all_mlx_paths fs in
   List.filter (fun ap->Substring.is_a_substring_of 
     old_name (Io.read_whole_file ap)) temp1;;
 
 let sliced_ocaml_name fs=
   let temp1=Image.image (fun dt->
     (Modulesystem_data.unprefixed_compact_ocaml_name dt)^";"
   ) fs.data in
   
   Sliced_string.of_string_list(
   [
     "Module"^"system.make(";
     "";
     "\t"^(Directory_name.ocaml_name(root  fs))^",";
     "";
     "\tImage.image Modulesystem_data.compact_make [";
     ""
    ]
    @temp1@
    [
     "";
     "],";
     "\""^(fs.main_toplevel_name)^"\")"
    ]
    );;
 
 let ocaml_name fs=Sliced_string.print(sliced_ocaml_name fs);;
 
 
let industrial_separator1=Industrial_separator.new_separator ();;  
let industrial_separator2=Industrial_separator.new_separator ();;    

  
let archive x=
   String.concat industrial_separator1
   [
     Directory_name.to_string(x.root);
     Nonblank.make(String.concat industrial_separator2 (Image.image Modulesystem_data.archive x.data));
     x.main_toplevel_name
   ];;

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator1) s in
   let v1=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1  1)) in
   let new_data=Image.image Modulesystem_data.unarchive v1 in 
{
    root = Directory_name.of_string(List.nth l1 0);
    data = new_data;
    main_toplevel_name =List.nth l1  2
};;
     

  
 
 
 
