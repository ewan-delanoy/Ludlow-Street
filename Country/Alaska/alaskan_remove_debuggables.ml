(*

#use"Country/Alaska/alaskan_remove_debuggables.ml";;

*)



let rd dir mdata=
   let sbuild=(Root_directory.connectable_to_subpath dir)^"_build/" in
   Unix_command.uc("rm -f "^sbuild^"*.d.cm*"^" "^sbuild^"*.ocaml_debuggable");;
   
  

