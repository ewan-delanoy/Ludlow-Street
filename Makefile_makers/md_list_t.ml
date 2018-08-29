
(* 

#use"Makefile_makers/md_list_t.ml";;

*)


type t={
   root : Root_directory_t.t ;
   modules : Naked_module_t.t Small_array.t ;
   subdir_for_module : Subdirectory_t.t Small_array.t ;
   principal_ending_for_module : Ocaml_ending.t Small_array.t ;
   mli_presence_for_module : bool Small_array.t ;
   principal_mt_for_module : string Small_array.t ;
   mli_mt_for_module : string Small_array.t ;
   needed_libs_for_module : Ocaml_library.t list Small_array.t ;
   direct_fathers_for_module : Naked_module_t.t list Small_array.t;
   ancestors_for_module : Naked_module_t.t list Small_array.t ; 
   needed_dirs_for_module : Subdirectory_t.t list Small_array.t
};;

