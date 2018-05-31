(*

#use"Ocaml_analysis/read_ocaml_files_without_expanding_inclusions.ml";;

Same as read_ocaml_files except that :
Module inclusions are not expanded.

*)

module Private=struct
     
let pusher_for_modulename_prepension
    (end_reached,(graet,current_full_scope,current_names,da_ober))=
    match da_ober with
    []->(true,(graet,current_full_scope,current_names,da_ober))
    |x::peurrest->
    match x.Ocaml_gsyntax_item.category with
      Ocaml_gsyntax_category.Value                                                                          
    | Ocaml_gsyntax_category.Type
    | Ocaml_gsyntax_category.Exception->
            let new_x=Ocaml_gsyntax_item.prepend_prefix current_full_scope x in
            (false,(new_x::graet,current_full_scope,current_names,peurrest))
    | Ocaml_gsyntax_category.Module_inclusion->
            let temp=Ocaml_gsyntax_item.prepend_prefix current_full_scope x in
            let new_x=Ocaml_gsyntax_item.make_name_coincide_with_content temp in
            (false,(new_x::graet,current_full_scope,current_names,peurrest))
    | Ocaml_gsyntax_category.Module_opener->
            let new_name=x.Ocaml_gsyntax_item.name in
            let new_names=current_names@[new_name] in
            let new_full_scope=String.concat "." new_names in
            (false,(graet,new_full_scope,new_names,peurrest))
    | Ocaml_gsyntax_category.Module_closer->
            let new_names=List.rev(List.tl(List.rev(current_names))) in
            let new_full_scope=String.concat "." new_names in
            (false,(graet,new_full_scope,new_names,peurrest));;
           
  let rec iterator_for_modulename_prepension walker_state=
     if fst(walker_state) 
     then let  (graet,_,_,_)=snd(walker_state) in List.rev graet
     else iterator_for_modulename_prepension(pusher_for_modulename_prepension walker_state);; 
  
  end;;
  
  exception Reading_error of Absolute_path.t * string;;
  
  let rofwei hm=
     let mlx=Mlx_ended_absolute_path.join hm Ocaml_ending.ml in
     let ap=Mlx_ended_absolute_path.to_path mlx in
     let text=Io.read_whole_file ap in
     let temp1=(
     try Pre_read_ocaml_files.pre_read text  with
     Pre_read_ocaml_files.Pre_read_exn(t)->raise(Reading_error(ap,t)) 
     ) in 
     Private.iterator_for_modulename_prepension (false,([],"",[],temp1));;
     
     
  