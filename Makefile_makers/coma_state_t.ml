
(* 

#use"Makefile_makers/coma_state_t.ml";;

*)


type t={
     root : Root_directory_t.t;
     dir_for_backup : Root_directory_t.t;
     (* mutable data        : Md_list_t.t; *)
     modules : Naked_module_t.t Small_array.t ;
     subdir_for_module : Subdirectory_t.t Small_array.t ;
     principal_ending_for_module : Ocaml_ending.t Small_array.t ;
     mli_presence_for_module : bool Small_array.t ;
     principal_mt_for_module : string Small_array.t ;
     mli_mt_for_module : string Small_array.t ;
     needed_libs_for_module : Ocaml_library.t list Small_array.t ;
     direct_fathers_for_module : Naked_module_t.t list Small_array.t;
     ancestors_for_module : Naked_module_t.t list Small_array.t ; 
     needed_dirs_for_module : Subdirectory_t.t list Small_array.t;
     executability_for_module : bool Small_array.t ;
     product_up_to_date_for_module : bool Small_array.t;
     mutable directories : Subdirectory_t.t list;
     mutable targets     : Ocaml_target.t list;
     mutable printer_equipped_types : (Half_dressed_module.t*bool) list;
};;


   

