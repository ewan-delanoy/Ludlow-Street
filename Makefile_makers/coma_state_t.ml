
(* 

#use"Makefile_makers/coma_state_t.ml";;

*)


type t={
     root : Root_directory_t.t;
     dir_for_backup : Root_directory_t.t;
     mutable data        : Modulesystem_data.t list;
     mutable directories : Subdirectory_t.t list;
     mutable targets     : Ocaml_target.t list;
     mutable printer_equipped_types : (Half_dressed_module.t*bool) list;
};;


   

