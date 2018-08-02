(*

#use"Makefile_makers/temporary_converter.ml";;

*)

type t=(
Root_directory_t.t * Naked_module_t.t Small_array.t *
Subdirectory_t.t Small_array.t * Acolyte_repartition_t.t Small_array.t *
bool Small_array.t *
float Small_array.t * float Small_array.t * 
Ocaml_library.t list Small_array.t *
Naked_module_t.t list Small_array.t *
Naked_module_t.t list Small_array.t * 
Subdirectory_t.t list Small_array.t);;

module Private=struct

  
let hm_recomputation (u:t) nm=
  let (
    main_root,  
    all_modules,
    to_subdirectory,
    to_repartition,
    to_interface_is_adjusted,
    to_modification_time,
    to_mli_modification_time,
    to_needed_libraries,
    to_direct_fathers,
    to_all_ancestors,
    to_needed_directories
  ) =u in
  let idx=Small_array.leftmost_index_of_in nm all_modules in
  let n_subdirectory=Small_array.get to_subdirectory idx in
  {
      Half_dressed_module.bundle_main_dir = Root_directory.without_trailing_slash main_root;
      subdirectory = Subdirectory.without_trailing_slash n_subdirectory;
      naked_module = Naked_module.to_string nm;
    } ;;

let list_of_modules (u:t)=
      let (
          main_root,  
          all_modules,
          to_subdirectory,
          to_repartition,
          to_interface_is_adjusted,
          to_modification_time,
          to_mli_modification_time,
          to_needed_libraries,
          to_direct_fathers,
          to_all_ancestors,
          to_needed_directories
      ) =u in
      Small_array.to_list all_modules;;

let individual_recomputation (u:t) nm=
    let (
        main_root,  
        all_modules,
        to_subdirectory,
        to_repartition,
        to_interface_is_adjusted,
        to_modification_time,
        to_mli_modification_time,
        to_needed_libraries,
        to_direct_fathers,
        to_all_ancestors,
        to_needed_directories
    ) =u in
    let idx=Small_array.leftmost_index_of_in nm all_modules in
    let 
    n_ending=Small_array.get to_repartition idx and
    n_modification_time=Small_array.get to_modification_time idx and
    n_mli_modification_time=Small_array.get to_mli_modification_time idx and
    n_needed_libraries=Small_array.get to_needed_libraries idx and
    n_direct_fathers=Small_array.get to_direct_fathers idx and
    n_all_ancestors=Small_array.get to_all_ancestors idx and
    n_needed_directories=Small_array.get to_needed_directories idx in
    let hm=hm_recomputation u nm in
    let c_direct_fathers=Image.image (hm_recomputation u) n_direct_fathers 
    and c_all_ancestors=Image.image (hm_recomputation u) n_all_ancestors
    in

    {                                                                 
      Modulesystem_data.name = hm;
      acolyte_repartition = n_ending;
      mli_present = Acolyte_repartition.test_mli_presence n_ending;
      principal_modification_time =  n_modification_time;
      mli_modification_time = n_mli_modification_time;


      needed_libraries = n_needed_libraries;
      direct_fathers = c_direct_fathers;
      all_ancestors = c_all_ancestors;
      needed_directories = n_needed_directories;
     };;
end;;

let to_md_list (u:t)=
   let list_of_modules=Private.list_of_modules u in
   Image.image(
      Private.individual_recomputation u
   ) list_of_modules;;

let of_md_list l=
    let temp1=Image.image Modulesystem_data.name l in
    let main_root=
        Half_dressed_module.bundle_main_dir(List.hd temp1) in
    let l_all_modules=
      Image.image Half_dressed_module.naked_module temp1 
    and l_to_subdirectory=
      Image.image Half_dressed_module.subdirectory  temp1 
    and l_to_repartition=
      Image.image Modulesystem_data.acolyte_repartition l
    and l_to_interface_is_adjusted=
      Image.image Modulesystem_data.mli_present l  
    and l_to_modification_time=
      Image.image (fun md->
      Modulesystem_data.modification_time md 
        (Modulesystem_data.principal_ending md)
      ) l 
    and l_to_mli_modification_time=
      Image.image (fun md->
      Modulesystem_data.mli_modification_time md 
      ) l   
    and l_to_needed_libraries=
      Image.image Modulesystem_data.needed_libraries l
    and l_to_direct_fathers=
      Image.image (fun md->
        let temp2=Modulesystem_data.direct_fathers md in
        Image.image Half_dressed_module.naked_module temp2
      ) l 
    and l_to_all_ancestors=
      Image.image (fun md->
        let temp2=Modulesystem_data.all_ancestors md in
        Image.image Half_dressed_module.naked_module temp2
      ) l 
    and l_to_needed_directories=
      Image.image Modulesystem_data.needed_directories l  in
    let all_modules=Small_array.of_list l_all_modules 
    and to_subdirectory=Small_array.of_list l_to_subdirectory
    and to_repartition=Small_array.of_list l_to_repartition
    and to_interface_is_adjusted=Small_array.of_list l_to_interface_is_adjusted
    and to_modification_time=Small_array.of_list l_to_modification_time
    and to_mli_modification_time=Small_array.of_list l_to_mli_modification_time
    and to_needed_libraries=Small_array.of_list l_to_needed_libraries 
    and to_direct_fathers=Small_array.of_list l_to_direct_fathers
    and to_all_ancestors=Small_array.of_list l_to_all_ancestors
    and to_needed_directories=Small_array.of_list l_to_needed_directories
    in 
    ((
    main_root,  
    all_modules,
    to_subdirectory,
    to_repartition,
    to_interface_is_adjusted,
    to_modification_time,
    to_mli_modification_time,
    to_needed_libraries,
    to_direct_fathers,
    to_all_ancestors,
    to_needed_directories
   ) :t);;

(*

let z1=German_wrapper.data();;
let z2=of_md_list z1;;
let z3=to_md_list z2;;
let z4=of_md_list z3;;
let check=(z4=z2);;




*)


