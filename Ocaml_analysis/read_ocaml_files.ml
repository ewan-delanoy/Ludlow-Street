(*

#use"Ocaml_analysis/read_ocaml_files.ml";;

*)

module Private=struct
  

  
  let module_inclusion_in_pusher    
     (graet,current_full_scope,current_names) x=
      let included_module=x.Ocaml_gsyntax_item.name in
          let full_scope=current_full_scope^"."^included_module in
          let maybe_included_items=List.filter(
             fun y->let nm_y=y.Ocaml_gsyntax_item.name in
             (Substring.begins_with nm_y full_scope)
             ||
             (Substring.begins_with nm_y included_module)  
          ) graet in 
          (* local redifinition has priority over an outside definition *)
          let chosen_scope=(if
            List.exists(fun y->
              y.Ocaml_gsyntax_item.name=included_module
            ) maybe_included_items
            then included_module
            else full_scope
          ) in
           let included_items=List.filter(
             fun y->y.Ocaml_gsyntax_item.name=chosen_scope
           ) maybe_included_items in
           let renamed_included_items=Image.image 
           (Ocaml_gsyntax_item.include_in_new_scope full_scope )
           included_items in
           (List.rev_append renamed_included_items graet,current_full_scope,current_names);;
     
  let first_pusher_for_modulename_prepension_and_inclusion_expansion  
     walker_state x=
     let (graet,current_full_scope,current_names)=walker_state in
    match x.Ocaml_gsyntax_item.category with
      Ocaml_gsyntax_category.Value                                                                          
    | Ocaml_gsyntax_category.Type
    | Ocaml_gsyntax_category.Exception->
            let new_x=Ocaml_gsyntax_item.prepend_prefix current_full_scope x in
            (new_x::graet,current_full_scope,current_names)
    | Ocaml_gsyntax_category.Module_opener->
            let new_name=x.Ocaml_gsyntax_item.name in
            let new_names=current_names@[new_name] in
            let new_full_scope=String.concat "." new_names in
            (graet,new_full_scope,new_names)
    | Ocaml_gsyntax_category.Module_closer->
            let new_names=List.rev(List.tl(List.rev(current_names))) in
            let new_full_scope=String.concat "." new_names in
            (graet,new_full_scope,new_names)
    | Ocaml_gsyntax_category.Module_inclusion->
           module_inclusion_in_pusher (graet,current_full_scope,current_names) x;;
  
  exception Pusher23_exn;;
  
  let pusher_for_modulename_prepension_and_inclusion_expansion (walker_state,da_ober)=
     match da_ober with
     []->raise(Pusher23_exn)
     |x::peurrest->(first_pusher_for_modulename_prepension_and_inclusion_expansion 
     walker_state x,peurrest);;    
  
           
  let rec iterator_for_modulename_prepension_and_inclusion_expansion (walker_state,da_ober)=
     if da_ober=[] 
     then let  (graet,_,_)=walker_state in List.rev graet
     else iterator_for_modulename_prepension_and_inclusion_expansion(
       pusher_for_modulename_prepension_and_inclusion_expansion (walker_state,da_ober));; 
  
  
  let prepend_modulenames_and_expand_inclusions data_before (current_module,l)=
      iterator_for_modulename_prepension_and_inclusion_expansion 
        ((data_before,current_module,Strung.split '.' current_module),l);;
  
  end;;
  
  exception Reading_error of Absolute_path.t * string;;
  
  let read_ocaml_files l_ap=
     let temp1=Image.image( fun ap->
     let s_ap=Absolute_path.to_string ap
     and text=Io.read_whole_file ap in
     let unpointed=Father_and_son.father s_ap '.' in
     let module_name=String.capitalize_ascii (Father_and_son.son unpointed '/') in
     try (module_name,Pre_read_ocaml_files.pre_read text)  with
     Pre_read_ocaml_files.Pre_read_exn(t)->raise(Reading_error(ap,t)) 
     ) l_ap in 
     List.fold_left Private.prepend_modulenames_and_expand_inclusions [] temp1;;
     
     
  (*
  
  let g1=German_wrapper.data();;
  let g2=List.filter Modulesystem_data.ml_present g1;;
  let g3=List.flatten (image Modulesystem_data.acolytes g2);;
  let g4=List.filter (fun mlx->snd(Mlx_filename.decompose mlx)=Ocaml_ending.ml) g3;;
  let g5=image Mlx_filename.to_absolute_path g4;;
  
  let g6=read3 g5;;
  
  
  let g6=image (fun ap->let s=Io.read_whole_file ap in
    (-(String.length s),(ap,s))
  ) g5 ;;
  let g7=image snd (ofo(Tidel2.diforchan g6));;
  let g8=Explicit.image (fun (ap,s)->(ap,read2 s)) g7;;
  let g9=Explicit.image (fun (ap,l)->
    from_level2_to_level3 ([],"Moody") l
  ) g8;;
  
  *)
  
    
  (*  
  
  let s1="let jiving=234  ;;";;
  describe_value_item s1 (1,String.length s1);;
  
  let s2="type ('a,'b) sister=('a list)*'b*string;;";;
  describe_type_item s2 (1,String.length s2);;
  
  let s3="type sister=(int list)*float*string;;";;
  describe_type_item s3 (1,String.length s3);;
  
  let s4="exception Foobar of string*int;;";;
  describe_exception_item s4 (1,String.length s4);;
  
  let s5="exception Foobar;;";;
  describe_exception_item s5 (1,String.length s5);;
  
  let s6="module  Foobar=struct";;
  describe_module_opener_item s6 (1,String.length s6);;
  
  let s7="end\n;;";;
  describe_module_opener_item s7 (1,String.length s7);;
  
  let s8="include Leap\n;;";;
  describe_module_inclusion_item s8 (1,String.length s8);;
     
  *)   
     
       
    