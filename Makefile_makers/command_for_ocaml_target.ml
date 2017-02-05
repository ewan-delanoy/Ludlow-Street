(*

#use"Makefile_makers/command_for_ocaml_target.ml";;

*)


exception Command_called_on_nodep of Mlx_filename.t;;
exception Unregistered_cmo  of Half_dressed_module.t;;
exception Unregistered_dcmo of Half_dressed_module.t;;
exception Unregistered_cmi  of Half_dressed_module.t;;
exception Unregistered_cma  of Half_dressed_module.t;;
exception Unregistered_cmx  of Half_dressed_module.t;;
exception Unregistered_ml_from_mll of Half_dressed_module.t;;
exception Unregistered_ml_from_mly of Half_dressed_module.t;;
exception Unregistered_executable of Half_dressed_module.t;;
exception Unregistered_debuggable of Half_dressed_module.t;;
exception Unregistered_modules_in_toplevel of string*(Half_dressed_module.t list);;  

let ingr=Ingredients_for_ocaml_target.ingredients_for_ocaml_target;;

let cmx_manager=function
 Ocaml_target.CMX(hm2)->
    let s_hm2=Half_dressed_module.to_string hm2 in
    Some(s_hm2^".cmx")
 |_->None;;

let dcmo_manager=function
 Ocaml_target.DCMO(hm2)->
    let s_hm2=Half_dressed_module.to_string hm2 in
    Some(s_hm2^".d.cmo")
 |_->None;;

let command_for_nodep mlx=[];;

let command_for_ml_from_mll fs hm=
          let s_hm=Half_dressed_module.to_string hm in
          let s=
          "ocamllex "^
          " -o "^s_hm^".ml"^
          	 " "^s_hm^".mll" in
          [Shell_command.usual s];; 
 
let command_for_ml_from_mly fs hm=
          let s_hm=Half_dressed_module.to_string hm in
          let s=
          "ocamlyacc "^s_hm^".mly" in
          [Shell_command.usual s];;  

let command_for_cmi fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cmi(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let ending=(
          if Modulesystem_data.mli_present dt
          then ".mli"
          else ".ml"
          ) in
          let s1=
          "ocamlc "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -c "^s_hm^ending in
          let s_dir=Directory_name.to_string(Modulesystem.root fs) in
          let full_mli=s_dir^s_hm^".mli" in
          if (not(Modulesystem_data.mli_present dt))
             &&(Sys.file_exists(full_mli))
          then  let dummy_mli=s_dir^"uvueaoqhkt.mli" in
                let s2="mv "^full_mli^" "^dummy_mli
                and s3="mv "^dummy_mli^" "^full_mli in
                [Shell_command.usual s2;
                 Shell_command.usual s1;
                 Shell_command.usual s3]
          else   [Shell_command.usual s1];;

let command_for_cmo fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cmo(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s=
          "ocamlc "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".cmo"^
          " -c "^s_hm^".ml" in
          [Shell_command.usual s];;

let command_for_dcmo fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_dcmo(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s=
          "ocamlc -g "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".d.cmo"^
          " -c "^s_hm^".ml" in
          [Shell_command.usual s];;

          
let command_for_cma fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cma(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s=
          "ocamlopt -a "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".cma"^
          " -c "^s_hm^".ml" in
          [Shell_command.usual s];;
 
let command_for_cmx fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cmx(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let s=
          "ocamlopt "^(Modulesystem_data.needed_dirs_and_libs true dt)^
          " -o "^s_hm^".cma"^
          " -c "^s_hm^".ml" in
          [Shell_command.usual s];; 
 

let command_for_executable fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_executable(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let temp1=ingr fs (Ocaml_target.EXECUTABLE(hm)) in
          let temp2=Option.filter_and_unpack cmx_manager temp1 in
          let s=
          "ocamlopt "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^s_hm^".caml_executable"^
          (String.concat " " temp2) in
          [Shell_command.usual s];; 
  
let command_for_debuggable fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_debuggable(hm)) else 
          let dt=Option.unpack opt in
          let s_hm=Half_dressed_module.to_string hm in
          let short_s_hm=Father_and_son.son s_hm '/' in
          let temp1=ingr fs (Ocaml_target.DEBUGGABLE(hm)) in
          let temp2=Option.filter_and_unpack dcmo_manager temp1 in
          let s=
          "ocamlc -g "^(Modulesystem_data.needed_dirs_and_libs false dt)^
          " -o "^short_s_hm^".ocaml_debuggable "^
          (String.concat " " temp2) in
          [Shell_command.usual s];; 
  
let command_for_toplevel fs name l=
          let temp1=Image.image (fun hm->(hm,Modulesystem.find_module_registration fs hm)) l  in
          let temp2=List.filter (fun x->snd(x)=None) temp1 in
          if temp2<>[]
          then let temp3=Image.image fst temp2 in
               raise(Unregistered_modules_in_toplevel(name,temp3))
          else
          let l_dt=Image.image (fun (_,y)->Option.unpack y) temp1 in
          let temp4=Image.image (fun fd->
             let hm=Modulesystem_data.name fd in
             let s_hm=(Half_dressed_module.to_string hm) in
             if Modulesystem_data.ml_present fd 
             then s_hm^".cmo"
             else " "
          ) l_dt in 
          let s_lhm=String.concat " " temp4 in
          let s=
          "ocamlmktop "^(Modulesystem_data.needed_dirs_and_libs_for_several false l_dt)^
          " -o "^name^" "^
          "  "^s_lhm^" " in
          [Shell_command.usual s];;   
 
let command_for_ocaml_target fs tgt=
  Ocaml_target.pattern_matching
  	command_for_nodep
  	(command_for_ml_from_mll  fs)
    (command_for_ml_from_mly  fs)
  	(command_for_cmi fs)
    (command_for_cmo fs)
    (command_for_dcmo fs)
    (command_for_cma fs)
    (command_for_cmx fs)
    (command_for_executable  fs)
    (command_for_debuggable  fs)
    (command_for_toplevel fs)
    tgt;;
  
  
 let command_for_ocaml_target_in_dir fs tgt=
   let fs_dir=Modulesystem.root fs in
   Shell_command.take_care_of_root_directory fs_dir 
     (command_for_ocaml_target fs tgt);; 
          

 
