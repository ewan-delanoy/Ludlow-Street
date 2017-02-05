
(* 


#use"Makefile_makers/isidore_compare_two_modulesystem_data.ml";;

Here we compare two modulesystem elements corresponding to the same
module name.

*)

 type t = {
      name : Half_dressed_module.t;
      ml_presences : bool*bool;
      mli_presences : bool*bool;
      mll_presences : bool*bool;
      mly_presences : bool*bool;
      ml_modification_time_has_changed : bool;
      mli_modification_time_has_changed : bool;
      mll_modification_time_has_changed : bool;
      mly_modification_time_has_changed : bool;
      changed_needed_libraries : (Ocaml_library.t list)*(Ocaml_library.t list);
      changed_direct_fathers : (Half_dressed_module.t list)*(Half_dressed_module.t list);
      changed_all_ancestors : (Half_dressed_module.t list)*(Half_dressed_module.t list);
      changed_needed_directories : (Subdirectory.t list)*(Subdirectory.t list);
    };;

module FD=Modulesystem_data;;

exception Different_Names of FD.t*FD.t;;

let sd=Simplify_without_orderings.symmetric_decomposition;;

let compare dt1 dt2=
    if (FD.name dt1)<>(FD.name dt2)
    then raise(Different_Names(dt1,dt2))
    else
    {
	  name=FD.name dt1;
      ml_presences=(FD.ml_present dt1,FD.ml_present dt2);
      mli_presences=(FD.mli_present dt1,FD.mli_present dt2);
      mll_presences=(FD.mll_present dt1,FD.mll_present dt2);
      mly_presences=(FD.mly_present dt1,FD.mly_present dt2);
      ml_modification_time_has_changed=((FD.ml_modification_time dt1)<>(FD.ml_modification_time dt2));
      mli_modification_time_has_changed=((FD.mli_modification_time dt1)<>(FD.mli_modification_time dt2));
      mll_modification_time_has_changed=((FD.mll_modification_time dt1)<>(FD.mll_modification_time dt2));
      mly_modification_time_has_changed=((FD.mly_modification_time dt1)<>(FD.mly_modification_time dt2));
      changed_needed_libraries=sd (FD.needed_libraries dt1) (FD.needed_libraries dt2);
      changed_direct_fathers=sd (FD.direct_fathers dt1) (FD.direct_fathers dt2) ;
      changed_all_ancestors=sd (FD.all_ancestors dt1) (FD.all_ancestors dt2);
      changed_needed_directories=sd (FD.needed_directories dt1) (FD.needed_directories dt2);

};;

let uncurried_compare (dt1,dt2)=compare dt1 dt2;;

let tab=String.make 0 ' ';;

let mlx_contribution s=function
   (true,false)->tab^" "^s^" disappeared "
   |(false,true)->tab^" "^s^" appeared "
   |_->"";;
   
let mlx_mt_contribution s (b1,b2) b=if(b1&&b2&&b) then tab^s^" changed" else "";; 


let display_list printer l=
  if l=[] then "" else (String.concat "," (Image.image printer l));;
  
let display_prefixed_list pref printer l=
  if l=[] then "" else pref^(String.concat "," (Image.image printer l));;  
  
let display_list_pair printer (l1,l2)=
  let old_s1=display_list printer l1
  and old_s2=display_list printer l2 in
  let s1=(if old_s1="" then "" else old_s1^" out")
  and s2=(if old_s2="" then "" else old_s2^" in")
  in
  if (s1<>"")&&(s2<>"") then s1^" | "^s2 else s1^s2;;

let display x=
  let temp1=[
   
      mlx_contribution "Ml" x.ml_presences;
      mlx_contribution "Mli" x.mli_presences;
      mlx_contribution "Mll" x.mll_presences;
      mlx_contribution "Mly" x.mly_presences;
      mlx_mt_contribution "Ml"   x.ml_presences  x.ml_modification_time_has_changed;
      mlx_mt_contribution "Mli" x.mli_presences x.mli_modification_time_has_changed;
      mlx_mt_contribution "Mll" x.mll_presences x.mll_modification_time_has_changed;
      mlx_mt_contribution "Mly" x.mly_presences x.mly_modification_time_has_changed;
      display_list_pair  	   Ocaml_library.to_string x.changed_needed_libraries;
      display_prefixed_list " has abandoned direct use of : " Half_dressed_module.to_string (fst(x.changed_direct_fathers));
      display_prefixed_list " now uses directly : " Half_dressed_module.to_string (snd(x.changed_direct_fathers));
      display_prefixed_list " has stopped using : " Half_dressed_module.to_string (fst(x.changed_all_ancestors));
      display_prefixed_list " now uses : " Half_dressed_module.to_string (snd(x.changed_all_ancestors));
      (*
      display_list_pair  Half_dressed_module.to_string x.changed_direct_fathers;
      display_list_pair  Half_dressed_module.to_string x.changed_all_ancestors;
      *)
      display_list_pair  Subdirectory.to_string x.changed_needed_directories; 
  
  ] in
  let temp2=List.filter (fun s->s<>"") temp1 in
  if temp2=[] then "" else
  let s_name=Half_dressed_module.to_string x.name in
  let temp3=Image.image (fun s->if String.contains s ':' then s_name^s else s_name^" : "^s) temp2 in
  String.concat "\n" temp3;;
  
let display_list l=
  let temp1=Image.image display l in
  let temp2=List.filter (fun s->s<>"") temp1 in
  if temp2=[] then "" else
  String.concat "\n" temp2;;   
    
   
   
