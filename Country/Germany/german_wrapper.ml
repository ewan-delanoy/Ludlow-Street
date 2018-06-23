(*

#use"Country/Germany/german_wrapper.ml";;


*)

module Private=struct

let data_ref=ref(Md_list.empty_one);;
let directories_ref=ref([]:Subdirectory_t.t list);;
let up_to_date_targets_ref=ref([]:Ocaml_target.t list);;
let printer_equipped_types_ref=ref([]:(Half_dressed_module.t*bool) list);;

let whole ()=(
	(!data_ref),
	(!directories_ref),
	(!up_to_date_targets_ref),
	(!printer_equipped_types_ref)
);;

let save_all ()=Alaskan_save_all.write_all 
  (German_constant.root, 
    Coma_constant.name_for_makefile,
    Coma_constant.name_for_targetfile,
    Coma_constant.name_for_loadingsfile,
    Coma_constant.name_for_printersfile
  )
  (
	  whole()
  );;



let recompile ()=
        match Alaskan_recompile.on_targets 
               German_constant.root
               false (!data_ref,!up_to_date_targets_ref) with
         None->(false,[])
        |Some((new_mdata,new_dirs,new_tgts,rejected_ones),short_paths)->
            let new_preqt=Image.image(
              fun (hm,_)->(hm,not(List.mem hm rejected_ones))
            )  (!printer_equipped_types_ref) in     
            let _=(
              data_ref:=new_mdata;
              directories_ref:=new_dirs;
              up_to_date_targets_ref:=new_tgts;
              printer_equipped_types_ref:=new_preqt;
              save_all();
            ) in
            (true,short_paths);;       


end;;



let data ()=(!Private.data_ref);;

let declare_printer_equipped_type hm=
  (
  (Private.printer_equipped_types_ref:=
  (!Private.printer_equipped_types_ref)@[hm]);
  Private.save_all ()
  );;



let directories ()=(!Private.directories_ref);;


 
 
let end_debugging ()=
    let _= Alaskan_remove_debuggables.rd 
                German_constant.root (!Private.data_ref) in
                
    let new_tgts=List.filter Ocaml_target.is_not_a_debuggable
         (!Private.up_to_date_targets_ref)  in 
       (
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
       );;   
   
let forget_unregistered_file ap=
    let _=Private.recompile() in
    let _=German_forget_unregistered_file.forget ap in
    Private.save_all();;       
       
let forget_file ap=
   let _=Private.recompile() in
   let (new_mdata,new_dirs,new_tgts)= 
    German_forget_file.on_targets 
      (data(),directories(),(!Private.up_to_date_targets_ref)) ap in  
       (
         Private.data_ref:=new_mdata;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
       );;         

       
let forget_module hm=
        let _=Private.recompile() in
        let ((new_mdata,new_dirs,new_tgts),short_paths)= 
         German_forget_module.on_targets
           (data(),directories(),(!Private.up_to_date_targets_ref)) hm in    
        let _=(
              Private.data_ref:=new_mdata;
              Private.directories_ref:=new_dirs;
              Private.up_to_date_targets_ref:=new_tgts;
              Private.save_all();
            ) in
        short_paths;;     

 let initialize ()=
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
	Private.data_ref:= mdata;
	Private.directories_ref:= directories;
	Private.up_to_date_targets_ref:= targets;
	Private.printer_equipped_types_ref:= pe_types;
  );;


let printer_equipped_types ()=(!Private.printer_equipped_types_ref);;

let recompile=Private.recompile;;  




let refresh ()=
  
    let (new_mdata,new_tgts,new_ptypes)=
    Alaskan_create_target_system.from_main_directory 
         German_constant.root
     in 
    let new_dirs=Md_list.compute_subdirectories_list new_mdata in
    let new_diff=German_delchacre_from_scratch.dfs new_mdata in
    let _=
    (
          Private.data_ref:=new_mdata;
      Private.directories_ref:=new_dirs;
      Private.up_to_date_targets_ref:=new_tgts;
      Private.printer_equipped_types_ref:=new_ptypes;
          Private.save_all();
     ) in
     new_diff;;  



    
let register_mlx_file mlx=
   let _=Private.recompile() in
   let (new_mdata,new_dirs,new_tgts)= 
   Alaskan_register_mlx_file.on_targets 
    (data(),directories(),(!Private.up_to_date_targets_ref)) 
        mlx in
  let (new_mdata2,new_tgts2,_)=Alaskan_make_ocaml_target.feydeau
        German_constant.root new_mdata new_tgts  in     
 	      
      (
         Private.data_ref:=new_mdata2;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts2;
         Private.save_all();
       ) ;;     
 
    
 let relocate_module old_name new_subdir=
    let _=Private.recompile() in
    let ((new_mdata,new_tgts),(old_files,new_files))=
      German_relocate_module.on_targets (data(),(!Private.up_to_date_targets_ref)) 
         old_name new_subdir in
       (
         Private.data_ref:=new_mdata;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
         
       );;       
 
 let rename_directory (old_subdir,new_subdirname)=
    let _=Private.recompile() in
    let _=Rename_endsubdirectory.in_unix_world German_constant.root (old_subdir,new_subdirname) in
    let pair=(old_subdir,new_subdirname) in
    let new_data=Md_list.rename_directory_on_data pair (!Private.data_ref)
    and new_dirs=German_rename_directory.on_subdirectories pair (!Private.directories_ref)
    and new_tgts=German_rename_directory.on_up_to_date_targets pair (!Private.up_to_date_targets_ref)
    and new_peqt=German_rename_directory.on_printer_equipped_types pair (!Private.printer_equipped_types_ref)
    in
       (
         Private.data_ref:=new_data;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.printer_equipped_types_ref:=new_peqt;
         Private.save_all();
       );;   
    
 let rename_module old_name new_name=
    let _=Private.recompile() in
    let ((new_mdata,new_tgts),(old_files,new_files))=
      German_rename_module.on_targets (data(),(!Private.up_to_date_targets_ref)) old_name new_name in  
       (
         Private.data_ref:=new_mdata;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
       );;   
   
let reposition_module hm (l_before,l_after)=
    let _=Private.recompile() in
    let new_mdata=
      Md_list.reposition_module (data()) hm (l_before,l_after) in
       (
         Private.data_ref:=new_mdata;
         Private.save_all();
       );;      
  
 let start_debugging ()=
    let _=Private.recompile() in
    let (bowl,(new_mdata,new_tgts,_))=
      German_start_debugging.sd (data(),(!Private.up_to_date_targets_ref))  in
    if (not(bowl)) 
    then ()
    else  
       (
         Private.data_ref:=new_mdata;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
       );;   
    

    
 let save_all=Private.save_all;;   
    
 let undeclare_printer_equipped_type hm=
  (Private.printer_equipped_types_ref:=
  List.filter (fun x->x<>hm) (!Private.printer_equipped_types_ref);
  Private.save_all ());;   
    
 let unregister_mlx_file mlx=
   let _=Private.recompile() in
   let (new_mdata,new_dirs,new_tgts)= 
    German_unregister_mlx_file.on_targets (data(),(!Private.up_to_date_targets_ref)) mlx in
       (
         Private.data_ref:=new_mdata;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
       );;  

let unregister_module hm=
   let _=Private.recompile() in
   let ((new_mdata,new_dirs,new_tgts),short_paths)= 
    German_unregister_module.on_targets (data(),(!Private.up_to_date_targets_ref)) hm in
       (
         Private.data_ref:=new_mdata;
         Private.directories_ref:=new_dirs;
         Private.up_to_date_targets_ref:=new_tgts;
         Private.save_all();
       );;        
   

   
let up_to_date_targets ()=(!Private.up_to_date_targets_ref);;   
   

let view_definition s=
  let opt=Md_list.find_value_definition (!(Private.data_ref)) s in
  if opt=None then () else
  let itm=Option.unpack opt in
  let text="\n\n"^(Ocaml_gsyntax_item.whole itm)^"\n\n" in
  (print_string text;flush stdout);;   

   
let whole=Private.whole;;
 
 