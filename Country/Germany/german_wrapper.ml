(*

#use"Country/Germany/german_wrapper.ml";;


*)

module Private=struct

let main_ref=Coma_state.empty_one;;

let whole ()=Coma_state.uple_form main_ref;;

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



let recompile ()=Coma_state.recompile main_ref;;   


end;;


let recompile_softly ()= let _=Private.recompile() in ();;
let data ()=Coma_state.get_data (Private.main_ref);;
let directories ()=Coma_state.get_directories (Private.main_ref);;

let declare_printer_equipped_type hm=
  (
    Coma_state.add_printer_equipped_type Private.main_ref hm; 
    Private.save_all ()
  );;
 
 
let end_debugging ()=
  let sbuild=(Root_directory.connectable_to_subpath German_constant.root)
     ^"_build/" in
  let _=Unix_command.uc("rm -f "^sbuild^"*.d.cm*"^" "^
     sbuild^"*.ocaml_debuggable")  in
  (   
     Coma_state.remove_debuggables  Private.main_ref;  
  );;   
   
let forget_unregistered_file ap= 
    let _=Private.recompile() in
    let _=German_forget_unregistered_file.forget ap in
    Private.save_all();;       
       
let forget_file ap=
  (
    recompile_softly();
    Coma_state.forget_file Private.main_ref ap;
    Private.save_all();
  );;   

       
let forget_module hm=
        let _=Private.recompile() in
        let short_paths=
          Coma_state.forget_module Private.main_ref hm in    
        let _=Private.save_all() in
        short_paths;;     

let initialize ()=
    Coma_state.initialize Private.main_ref ;; 


let printer_equipped_types ()=
    Coma_state.get_preq_types Private.main_ref;;

let recompile=Private.recompile;;  




let refresh ()=
   let new_diff=Coma_state.refresh Private.main_ref in
   let _=Private.save_all() in
   new_diff;;

let register_mlx_file mlx=
    (recompile_softly();
     Coma_state.register_mlx_file Private.main_ref mlx;
     Private.save_all();
    );;  
 
let relocate_module old_name new_subdir=
      (recompile_softly();
       Coma_state.relocate_module Private.main_ref old_name new_subdir;
       Private.save_all();
      );;          
    


let rename_directory (old_subdir,new_subdirname)=
  (
          recompile_softly();
          Coma_state.rename_directory Private.main_ref (old_subdir,new_subdirname);
          Private.save_all();
  );;   
        

let rename_module old_name new_name=
    (
            recompile_softly();
            Coma_state.rename_module Private.main_ref old_name new_name;
            Private.save_all();
    );;   
                   


let reposition_module hm (l_before,l_after)=
  (
    recompile_softly();
    Coma_state.reposition_module Private.main_ref hm (l_before,l_after);
    Private.save_all();
 );;   
  
let start_debugging ()=
  (
    recompile_softly();
    Coma_state.start_debugging Private.main_ref;
    Private.save_all();
 );;   
    

    
let save_all=Private.save_all;;   
    
    
let undeclare_printer_equipped_type hm=
    (
      Coma_state.remove_printer_equipped_type Private.main_ref hm; 
      Private.save_all ()
    );;
   

let unregister_module hm=
    (recompile_softly();
     Coma_state.unregister_module Private.main_ref hm;
     Private.save_all();
    );;    


let unregister_mlx_file mlx=
   (recompile_softly();
    Coma_state.unregister_mlx_file Private.main_ref mlx;
    Private.save_all();
   );;        
   
   
let up_to_date_targets ()=Coma_state.get_targets Private.main_ref;;   
   

let view_definition s=
  let opt=Modify_md_list.find_value_definition (Coma_state.get_data Private.main_ref) s in
  if opt=None then () else
  let itm=Option.unpack opt in
  let text="\n\n"^(Ocaml_gsyntax_item.whole itm)^"\n\n" in
  (print_string text;flush stdout);;   

   
let whole=Private.whole;;
 
 