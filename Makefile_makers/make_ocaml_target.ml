(*

#use"Makefile_makers/make_ocaml_target.ml";;

*)



let cmd_for_tgt=Command_for_ocaml_target.command_for_ocaml_target_in_dir;;
let ingr_for_tgt=Ingredients_for_ocaml_target.ingredients_for_ocaml_target;;
let ingr_for_top=Ingredients_for_ocaml_target.marked_ingredients_for_unprepared_toplevel;;

let is_up_to_date ts tgt=
  if Ocaml_target.is_a_debuggable tgt
  then false
  else 
  let fs=Target_system.modulesystem ts in
  if Ocaml_target.test_path (Modulesystem.root fs) tgt
  then List.mem tgt (Target_system.up_to_date_targets ts)
  else false;;

let unit_make (bowl,ts) tgt=
  if (not bowl)
  then (bowl,ts)
  else
  if is_up_to_date ts tgt
  then (true,ts)
  else 
  let fs=Target_system.modulesystem ts  in
  let temp1=Image.image Shell_command.announce_and_do (cmd_for_tgt fs tgt) in 
  if List.for_all (fun bowl->bowl) temp1
  then let opt_tgt=(if Ocaml_target.is_a_debuggable tgt then None else (Some tgt)) in
       let ts2=Modify_target_system.add_target_perhaps opt_tgt ts in
        match Ocaml_target.ml_from_lex_or_yacc_data tgt with
       None->(true,ts2)
       |Some(mlx)->
                   let ts3=Modify_target_system.force_modification_time_update ts2 mlx in
                   (true,ts3)        
  else (false,ts);;

let make_nontoplevel ts tgt=
  let l=ingr_for_tgt (Target_system.modulesystem ts) tgt in
  List.fold_left unit_make  (true,ts) l;;
  
exception Ending_for_toplevel_pusher;;  



let rec pusher_for_toplevel (successful_ones,to_be_treated,ts)=
  match to_be_treated with
  []->raise(Ending_for_toplevel_pusher)
  |(tgt,is_an_ending_or_not)::others->
  let (bowl2,ts2)=unit_make (true,ts) tgt in
  if bowl2
  then let new_successful_ones=(
         if is_an_ending_or_not=Is_an_ending_or_not.Yes
         then let hm=Option.unpack(Ocaml_target.main_module tgt) in
              (*
                Note that the cmi and cmo give the same hm
              *)
              if List.mem hm successful_ones
              then successful_ones
              else hm::successful_ones
         else successful_ones
       ) in
       (new_successful_ones,others,ts2)
  else let hm=Option.unpack(Ocaml_target.main_module tgt) in
  	   let remains=List.filter
       (fun (tgt,_)->
         not(Ingredients_for_ocaml_target.module_dependency_for_ocaml_target 
         (Target_system.modulesystem ts) [hm] tgt)
       ) to_be_treated in
       (successful_ones,remains,ts2);; 

let rec iterator_for_toplevel (successful_ones,to_be_treated,ts)=
  match to_be_treated with
  []->(List.rev successful_ones,ts)
  |_->iterator_for_toplevel(pusher_for_toplevel(successful_ones,to_be_treated,ts));;

  
 let make_toplevel ts name l=
    let fs=Target_system.modulesystem ts in
    let temp1=ingr_for_top fs name l in
    let (successful_ones,ts2)=iterator_for_toplevel([],temp1,ts) in
    let new_toplevel=Ocaml_target.toplevel name successful_ones  in
    unit_make (true,ts2) new_toplevel;;
 
let make ts tgt=
  match Ocaml_target.toplevel_data tgt with
  None->make_nontoplevel ts tgt
  |Some(name,l)->make_toplevel ts name l;; 
 
