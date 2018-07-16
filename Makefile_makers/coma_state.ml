
(* 

#use"Makefile_makers/coma_state.ml";;

*)


let get_root x=x.Coma_state_t.root;;
let get_backup_dir x=x.Coma_state_t.dir_for_backup;;
   
let get_data x=x.Coma_state_t.data;;
let get_directories x=x.Coma_state_t.directories;;
let get_targets x=x.Coma_state_t.targets;;
let get_preq_types x=x.Coma_state_t.printer_equipped_types;;

let set_data x y=x.Coma_state_t.data<- y;;
let set_directories x y=x.Coma_state_t.directories<- y;;
let set_targets x y=x.Coma_state_t.targets<- y;;
let set_preq_types x y=x.Coma_state_t.printer_equipped_types<- y;;

let empty_one x y={
     Coma_state_t.root =x;
     dir_for_backup =y;
     data        =[];
     directories =[];
     targets     =[];
     printer_equipped_types =[];
};;

let uple_form x=
   (get_data x,
    get_directories x,
    get_targets x,
    get_preq_types x);;


let recompile x=
     let (old_mdata,old_tgts)=
       (x.Coma_state_t.data,x.Coma_state_t.targets) in
     let ((new_mdata,hms_to_be_updated),short_paths)=
        Modify_md_list.recompile_on_monitored_modules false old_mdata in
     if hms_to_be_updated=[] then (false,[]) else
     let new_dirs=Modify_md_list.compute_subdirectories_list new_mdata 
     and new_tgts1=Ocaml_target.still_up_to_date_targets hms_to_be_updated old_tgts in
     let checker=Ocaml_target.test_target_existence German_constant.root in
     let new_tgts=List.filter checker new_tgts1 in
     let (new_mdata2,new_tgts2,rejected_ones2)=
       Alaskan_make_ocaml_target.feydeau
       German_constant.root new_mdata new_tgts  in
      let new_preqt=Image.image(
        fun (hm,_)->(hm,not(List.mem hm rejected_ones2))
      )  (x.Coma_state_t.printer_equipped_types) in   
     let _=(
        x.Coma_state_t.data <- new_mdata2;
        x.Coma_state_t.directories <- new_dirs;
        x.Coma_state_t.targets <- new_tgts2;
        x.Coma_state_t.printer_equipped_types <- new_preqt;
     )  in
    (true,short_paths);;       

let add_printer_equipped_type x hm=
   set_preq_types x ((get_preq_types x)@[hm]);;

let remove_printer_equipped_type x hm=
    set_preq_types x (List.filter (fun hm2->hm2<>hm) (get_preq_types x));;

let remove_debuggables x=
  let new_tgts=List.filter Ocaml_target.is_not_a_debuggable
         (get_targets x)  in 
  set_targets x new_tgts;;
    
let backup x diff opt=
  Alaskan_backup_target_system.backup 
  (get_backup_dir x) diff opt;;

let forget_file x ap=
    let (new_mdata,new_dirs,new_tgts)= 
     German_forget_file.on_targets 
       (get_data x,get_directories x,get_targets x) ap in  
        (
          set_data x new_mdata;
          set_directories x new_dirs;
          set_targets x new_tgts;
        );;         
   
let forget_module x hm=
    let ((new_mdata,new_dirs,new_tgts),short_paths)= 
      German_forget_module.on_targets
      (get_data x,get_directories x,get_targets x) hm in
      let _=(
          set_data x new_mdata;
          set_directories x new_dirs;
          set_targets x new_tgts;
      ) in
      short_paths;;          

let initialize x=
        let s_ap=Root_directory.join German_constant.root  Coma_constant.name_for_targetfile in
        let ap=Absolute_path.of_string s_ap in
        let the_archive=Io.read_whole_file ap in
        let 
        (
         mdata,
         directories,
         targets,
         pe_types
        )=Alaskan_save_all.read_all the_archive in
        (
       set_data x mdata;
       set_directories x directories;
       set_targets x targets;
       set_preq_types x pe_types;
       );;      

let refresh x=
      let (new_mdata,new_tgts,new_ptypes)=
        Alaskan_create_target_system.from_main_directory 
             German_constant.root
         in 
        let new_dirs=Modify_md_list.compute_subdirectories_list new_mdata in
        let new_diff=German_delchacre_from_scratch.dfs (get_backup_dir x) new_mdata in
        let _=
        (
          set_data x new_mdata;
          set_directories x new_dirs;
          set_targets x new_tgts;
          set_preq_types x new_ptypes;
         ) in
         new_diff;; 


let register_mlx_file x mlx=
          let (new_mdata,new_dirs,new_tgts)= 
          Alaskan_register_mlx_file.on_targets 
           (get_data x,get_directories x,get_targets x) mlx in
         let (new_mdata2,new_tgts2,_)=Alaskan_make_ocaml_target.feydeau
               German_constant.root new_mdata new_tgts  in     
                
             (
              set_data x new_mdata2;
              set_directories x new_dirs;
              set_targets x new_tgts2;   
              ) ;;             



let relocate_module x old_name new_subdir=
  let ((new_mdata,new_tgts),(old_files,new_files))=
    German_relocate_module.on_targets 
       (get_data x,get_targets x) 
       old_name new_subdir in
     (
      set_data x new_mdata;
      set_targets x new_tgts; 
     );;    

let rename_directory x (old_subdir,new_subdirname)=
      let _=Rename_endsubdirectory.in_unix_world 
       German_constant.root (old_subdir,new_subdirname) in
      let pair=(old_subdir,new_subdirname) in
      let new_mdata=Modify_md_list.rename_directory_on_data pair 
         (get_data x)
      and new_dirs=German_rename_directory.on_subdirectories pair 
        (get_directories x)
      and new_tgts=German_rename_directory.on_up_to_date_targets pair 
        (get_targets x)
      and new_peqt=German_rename_directory.on_printer_equipped_types pair 
        (get_preq_types x)
      in
         (
          set_data x new_mdata;
          set_directories x new_dirs;
          set_targets x new_tgts;
          set_preq_types x new_peqt;  
         );;   
      
let rename_module x old_name new_name=
      let ((new_mdata,new_tgts),(old_files,new_files))=
        German_rename_module.on_targets 
          (get_data x,get_targets x) old_name new_name in  
         (
          set_data x new_mdata;
          set_targets x new_tgts;
         );;   


let reposition_module x hm (l_before,l_after)=
    let new_mdata=
        Modify_md_list.reposition_module (get_data x) hm (l_before,l_after) in
    set_data x new_mdata;;               
                    
  

let start_debugging x=
          let (bowl,(new_mdata,new_tgts,_))=
            German_start_debugging.sd 
            (get_data x,get_targets x)  in
          if bowl
          then (
            set_data x new_mdata;
            set_targets x new_tgts 
               )
          else ();;

          


let unregister_mlx_file x mlx=
        let (new_mdata,new_dirs,new_tgts)= 
          German_unregister_mlx_file.on_targets
          (get_data x,get_targets x) mlx in
          (
              set_data x new_mdata;
              set_directories x new_dirs;
              set_targets x new_tgts;
          ) ;;  


let unregister_module x hm=
        let ((new_mdata,new_dirs,new_tgts),short_paths)= 
         German_unregister_module.on_targets 
           (get_data x,get_targets x) hm in
            (
              set_data x new_mdata;
              set_directories x new_dirs;
              set_targets x new_tgts;
            );;        


               