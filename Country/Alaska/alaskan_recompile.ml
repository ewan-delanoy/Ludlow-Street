
(* 

#use"Country/Alaska/alaskan_recompile.ml";;


*)



    
  let on_targets (dir,main_top_name) tolerate_cycles (old_mdata,old_tgts)=
      let ((new_mdata,hms_to_be_updated),short_paths)=
        Md_list.recompile_on_monitored_modules tolerate_cycles old_mdata in
    if hms_to_be_updated=[] then None else
    let new_dirs=Md_list.compute_subdirectories_list new_mdata 
     and new_tgts1=Ocaml_target.still_up_to_date_targets hms_to_be_updated old_tgts in
     let checker=Ocaml_target.test_target_existence dir in
     let new_tgts=List.filter checker new_tgts1 in
     let default_top=(Md_list.default_toplevel main_top_name new_mdata) in
     let (new_mdata2,new_tgts2,rejected_ones2)=
       snd(Alaskan_make_ocaml_target.make 
        dir
       (new_mdata,new_tgts,[]) default_top) in
      Some((new_mdata2,new_dirs,new_tgts2,rejected_ones2),short_paths);;   
  
  
  
  
  
  