(*

#use"Ocaml_analysis/read_ocaml_files_without_expanding_inclusions.ml";;

Same as read_ocaml_files except that :
Module inclusions are not expanded.

*)

module Private=struct
  
     
  let first_pusher_for_modulename_prepension
     walker_state x=
     let (graet,current_full_scope,current_names)=walker_state in
    match x.Ocaml_gsyntax_item.category with
      Ocaml_gsyntax_category.Value                                                                          
    | Ocaml_gsyntax_category.Type
    | Ocaml_gsyntax_category.Exception->
            let new_x=Ocaml_gsyntax_item.prepend_prefix current_full_scope x in
            (new_x::graet,current_full_scope,current_names)
    | Ocaml_gsyntax_category.Module_inclusion->
            let temp=Ocaml_gsyntax_item.prepend_prefix current_full_scope x in
            let new_x=Ocaml_gsyntax_item.make_name_coincide_with_content temp in
            (new_x::graet,current_full_scope,current_names)
    | Ocaml_gsyntax_category.Module_opener->
            let new_name=x.Ocaml_gsyntax_item.name in
            let new_names=current_names@[new_name] in
            let new_full_scope=String.concat "." new_names in
            (graet,new_full_scope,new_names)
    | Ocaml_gsyntax_category.Module_closer->
            let new_names=List.rev(List.tl(List.rev(current_names))) in
            let new_full_scope=String.concat "." new_names in
            (graet,new_full_scope,new_names);;
  
  exception Pusher23_exn;;
  
  let pusher_for_modulename_prepension (walker_state,da_ober)=
     match da_ober with
     []->raise(Pusher23_exn)
     |x::peurrest->(first_pusher_for_modulename_prepension 
     walker_state x,peurrest);;    
  
           
  let rec iterator_for_modulename_prepension (walker_state,da_ober)=
     if da_ober=[] 
     then let  (graet,_,_)=walker_state in List.rev graet
     else iterator_for_modulename_prepension(
       pusher_for_modulename_prepension (walker_state,da_ober));; 
  
  
  let prepend_modulenames data_before (current_module,l)=
      iterator_for_modulename_prepension 
        ((data_before,current_module,Strung.split '.' current_module),l);;
  
  end;;
  
  exception Reading_error of Absolute_path.t * string;;
  
  let rofwei l_hm=
     let temp1=Image.image( fun hm->
     let mlx=Mlx_ended_absolute_path.join hm Ocaml_ending.ml in
     let ap=Mlx_ended_absolute_path.to_path mlx in
     let s_ap=Absolute_path.to_string ap
     and text=Io.read_whole_file ap in
     let unpointed=Father_and_son.father s_ap '.' in
     let module_name=String.capitalize_ascii (Father_and_son.son unpointed '/') in
     try (module_name,Pre_read_ocaml_files.pre_read text)  with
     Pre_read_ocaml_files.Pre_read_exn(t)->raise(Reading_error(ap,t)) 
     ) l_hm in 
     List.fold_left Private.prepend_modulenames [] temp1;;
     
     
  