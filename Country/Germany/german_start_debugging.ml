(*

#use"Country/Germany/german_start_debugging.ml";;

*)



let sd (mdata,tgts)=
    let _=Alaskan_remove_debuggables.rd German_constant.root mdata in
    let dbg=Coma_constant.name_for_debugged_module in
	let dir=German_constant.root in
	let rdir=Modify_md_list.compute_subdirectories_list mdata in
	let ap=Find_suitable_ending.find_file_location dir rdir 
	     (dbg^".ml") in
	let hm=Half_dressed_module.of_path_and_root ap dir in
	let mdata2=Modify_md_list.recompute_module_info mdata hm in
	let tgt=Ocaml_target.debuggable hm in
	let answer=Alaskan_make_ocaml_target.make German_constant.root
	(mdata2,tgts,[]) tgt in
	let msg=(
	  if (fst answer)
	  then "\n\n Now, start \n\nocamldebug _build/"^dbg^".ocaml_debuggable\n\nin another terminal\n\n"
	  else "\n\n Something went wrong, see above. \n\n"
	) in
	let _=(
	  print_string msg;
	  flush stdout
	) in
	answer;;   
   

