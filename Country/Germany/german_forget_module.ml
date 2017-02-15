(*

#use"Country/Germany/german_forget_module.ml";;

*)

    
exception ModuleWithDependencies of 
	Half_dressed_module.t*(Half_dressed_module.t list);;
exception Non_registered_module of Half_dressed_module.t;;

let on_targets (mdata,dirs,tgts) hm=
  match Alaskan_data.find_module_registration mdata hm with
   None->raise(Non_registered_module(hm))
  |Some(dt)->
   let bel=German_data.below mdata hm in
    if bel=[]
    then let s_hm=Half_dressed_module.to_string hm in
         let fn=(Directory_name.to_string(German_constant.root))^s_hm in
         let _=Image.image
         (fun edg->Shell_command.do_and_notice_failure("rm -f "^fn^edg))
         [".ml";".mll";".mli";".mly";".cm*";".d.cm*";".caml_debuggable"] in
         German_unregister_module.on_targets (mdata,tgts) hm
    else raise(ModuleWithDependencies(hm,bel));;
