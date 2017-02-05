(*

#use"Makefile_makers/target_system.ml";;

The up_to_date_targets record only stores non-mlx targets.
A non-mlx target is up to date when it has been produced
from the latest mlx targets.

*)

type t={
   modulesystem : Modulesystem.t;
   directories : Subdirectory.t list;
   up_to_date_targets : Ocaml_target.t list;
   outside_directories : Subdirectory.t list;
   outside_files : Absolute_path.t list;
   recently_deleted : string list;
   printer_equipped_types : Half_dressed_module.t list;
};;

let make filesys l_dirs l_targs o_dirs o_files r_deleted pe_types={
   modulesystem=filesys;
   directories=l_dirs;
   up_to_date_targets=l_targs;
   outside_directories=o_dirs;
   outside_files=o_files;
   recently_deleted=r_deleted;
   printer_equipped_types=pe_types;
};;

let modulesystem ts=ts.modulesystem;;
let directories ts=ts.directories;;
let up_to_date_targets ts=ts.up_to_date_targets;;
let outside_directories ts=ts.outside_directories;;
let outside_files ts=ts.outside_files;;
let recently_deleted ts=ts.recently_deleted;;
let printer_equipped_types ts=ts.printer_equipped_types;;

let root ts=Modulesystem.root (ts.modulesystem);;
let main_toplevel_name ts=Modulesystem.main_toplevel_name (ts.modulesystem);;
let find_module_registration ts mlx=Modulesystem.find_module_registration (ts.modulesystem) mlx;;
let all_mlx_files ts=Modulesystem.all_mlx_files (ts.modulesystem);;
let inside_files ts=Image.image Mlx_filename.to_path (all_mlx_files ts);;

let from_modulesystem fs=
  let l_dirs=Compute_modulesystem_directories.compute_modulesystem_directories fs in
  make fs l_dirs [] [] [] [] [];;
 

let from_root_and_toplevel dir opt=
  from_modulesystem(Modulesystem.from_root_and_toplevel dir opt);; 

 
let unregistered_mlx_files ts=Modulesystem.unregistered_mlx_files ts.modulesystem;; 
 

 
let all_modules ts=
  let temp1=Modulesystem.all_filedata ts.modulesystem in
  let temp2=Image.image (fun dt->
     Half_dressed_module.to_string(Modulesystem_data.name dt)
  ) temp1 in
  temp2;;
 
let system_size x=Modulesystem.system_size(x.modulesystem);; 
 
let industrial_separator1=Industrial_separator.new_separator ();;  
let industrial_separator2=Industrial_separator.new_separator ();;    

  
  
let archive x=
   let temp1=Image.image (fun w->Nonblank.make(Subdirectory.unveil w)) x.directories 
   and temp2=Image.image (fun w->Nonblank.make(Subdirectory.unveil w)) x.outside_directories 
   and temp3=Image.image (fun w->Absolute_path.to_string w) x.outside_files 
   and temp4=Image.image (fun w->Nonblank.make w) x.recently_deleted 
   and temp5=Image.image Half_dressed_module.archive x.printer_equipped_types in
   String.concat industrial_separator1
   [
     Modulesystem.archive x.modulesystem;
     Nonblank.make(String.concat industrial_separator2 temp1);
     Nonblank.make(String.concat industrial_separator2 (Image.image Ocaml_target.archive x.up_to_date_targets));
     Nonblank.make(String.concat industrial_separator2 temp2);
     Nonblank.make(String.concat industrial_separator2 temp3);
     Nonblank.make(String.concat industrial_separator2 temp4);
     Nonblank.make(String.concat industrial_separator2 temp5);
   ];;

let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator1) s in
   let v1=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1  1))
   and v2=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1  2)) 
   and v3=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1  3)) 
   and v4=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1  4)) 
   and v5=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1  5))   
   and v6=Str.split (Str.regexp_string industrial_separator2) (Nonblank.decode(List.nth l1  6))
   in
   let new_modulesystem=Modulesystem.unarchive(List.nth l1 0) in
   let new_directories=Image.image (fun v->Subdirectory.of_string(Nonblank.decode v)) v1 in
   let new_targets=Image.image Ocaml_target.unarchive v2 in
   let new_odirectories=Image.image (fun v->Subdirectory.of_string(Nonblank.decode v)) v3 in
   let new_ofiles=Image.image Absolute_path.of_string v4 in
   let new_dfiles=Image.image Nonblank.decode v5 in
   let new_pe_types=Image.image Half_dressed_module.unarchive v6 in
   
{
    modulesystem = new_modulesystem;
    directories = new_directories;
    up_to_date_targets = new_targets;
    outside_directories= new_odirectories;
    outside_files= new_ofiles;
    recently_deleted= new_dfiles;
    printer_equipped_types=new_pe_types;
};;




    
   

 
 