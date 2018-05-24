(*

#use"Country/Alaska/alaskan_make_ocaml_target.ml";;

*)



let cmd_for_tgt=Alaskan_command_for_ocaml_target.command_for_ocaml_target;;

let ingr_for_tgt =Alaskan_ingredients_for_ocaml_target.ingredients_for_ocaml_target;;
let ingr_for_top =Alaskan_ingredients_for_ocaml_target.marked_ingredients_for_unprepared_toplevel;;


let is_up_to_date dir tgts tgt=
  if Ocaml_target.is_a_debuggable tgt
  then false
  else 
  if Ocaml_target.test_target_existence dir tgt
  then List.mem tgt tgts
  else false;;

let unit_make dir (bowl,(mdata,tgts,rejected_ones)) tgt=
  if (not bowl)
  then (bowl,(mdata,tgts,rejected_ones))
  else
  if is_up_to_date dir tgts tgt
  then (true,(mdata,tgts,rejected_ones))
  else 
  let temp1=Image.image Unix_command.uc (cmd_for_tgt dir mdata tgt) in 
  if List.for_all (fun i->i=0) temp1
  then let tgts2=(
        if Ocaml_target.is_a_debuggable tgt 
        then tgts
        else tgt::tgts
       )  in
        match Ocaml_target.ml_from_lex_or_yacc_data tgt with
       None->(true,(mdata,tgts2,rejected_ones))
       |Some(mlx)->
                   let mdata2=Alaskan_force_modification_time.update dir mdata mlx in
                   (true,(mdata2,tgts2,rejected_ones))        
  else let rejected_ones2=(
        match Ocaml_target.main_module tgt with
        None->rejected_ones
        |Some(hm)->hm::rejected_ones
        )  in
       (false,(mdata,tgts,rejected_ones2));;

let make_nontoplevel dir (mdata,tgts,rejected_ones) tgt=
  let l=ingr_for_tgt mdata tgt in
  List.fold_left (unit_make dir)  (true,(mdata,tgts,rejected_ones)) l;;
  
exception Ending_for_toplevel_pusher;;  



let pusher_for_toplevel dir (successful_ones,to_be_treated,ts)=
  match to_be_treated with
  []->raise(Ending_for_toplevel_pusher)
  |(tgt,is_an_ending_or_not)::others->
  let (bowl2,ts2)=unit_make dir (true,ts) tgt in
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
       let root=Half_dressed_module.bundle_main_dir hm in
       let s_root=Directory_name.connectable_to_subpath root in
       let (mdata,_,_)=ts 
       and (mdata2,tgts2,rejected_ones2)=ts2 in
  	   let (rejects,remains)=List.partition
       (fun (tgtt,_)->
         Alaskan_ingredients_for_ocaml_target.module_dependency_for_ocaml_target 
         mdata [hm] tgtt
       ) others in
       let _=Image.image (
         fun (tgtt,_)->
         if Ocaml_target.has_dependencies tgtt
         then let s_ap=s_root^"_build/"^
                  (Ocaml_target.to_shortened_string tgtt) in
              let _=Unix_command.uc("rm -f "^s_ap) in
              ()
       ) ((tgt,is_an_ending_or_not)::rejects) in
       let newly_rejected_ones=Option.filter_and_unpack 
       (fun (tgt,_)->Ocaml_target.main_module tgt) rejects in
       let rejected_ones2=Listennou.nonredundant_version(
         List.rev_append newly_rejected_ones rejected_ones2
       ) in
       (successful_ones,remains,(mdata2,tgts2,rejected_ones2));; 

let rec  iterator_for_toplevel dir (successful_ones,to_be_treated,ts)=
  match to_be_treated with
  []->(List.rev successful_ones,ts)
  |_->iterator_for_toplevel dir (pusher_for_toplevel dir (successful_ones,to_be_treated,ts));;

  
let make_toplevel dir ts name l=
    let (mdata,_,_)=ts in
    let temp1=ingr_for_top mdata name l in
    let (successful_ones,ts2)=iterator_for_toplevel dir ([],temp1,ts) in
    let new_toplevel=Ocaml_target.toplevel name successful_ones  in
    unit_make dir (true,ts2) new_toplevel;;
 
let make dir ts tgt=
  match Ocaml_target.toplevel_data tgt with
  None->make_nontoplevel dir ts tgt
  |Some(name,l)->make_toplevel dir ts name l;; 
 
