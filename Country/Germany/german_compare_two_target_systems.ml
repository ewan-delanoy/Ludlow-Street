
(* 

#use"Country/Germany/german_compare_two_target_systems.ml";;

*)

 type t = {
      file_differences : German_compare_two_modulesystems.t;
      personal_targets : (Ocaml_target.t list)*(Ocaml_target.t list);
      personal_directories : (Subdirectory.t list)*(Subdirectory.t list);
};;


let consider_targets adjective l=
 if l=[] then "" else 
 adjective^" targets : "^(String.concat "," (Image.image Ocaml_target.to_string l));;


let consider_directories adjective l=
 if l=[] then "" else 
 adjective^" directories : "^(String.concat "," (Image.image Subdirectory.to_string l));;

let deal_with_targets l1 l2=
  let ttg1=Tidel.safe_set(l1)
  and ttg2=Tidel.safe_set(l2) in
  let temp1=Tidel.lemel ttg1 ttg2
  and temp2=Tidel.lemel ttg2 ttg1 in
  let temp3=List.filter (fun tgt->Tidel.elfenn tgt temp1) l1
  and temp4=List.filter (fun tgt->Tidel.elfenn tgt temp2) l2 in
  (temp3,temp4);;
 

  

let compare (mdata1,dirs1,tgts1) (mdata2,dirs2,tgts2)=
  {
     file_differences=German_compare_two_modulesystems.compare mdata1 mdata2;
      personal_targets=deal_with_targets tgts1 tgts2;
      personal_directories=
      	Simplify_without_orderings.symmetric_decomposition
      	dirs1 dirs2;
  };;
 
let obsolete_targets_will_be_displayed=ref false;; 

let display x=
  let temp1=[

      (
       if (!obsolete_targets_will_be_displayed)
       then consider_targets "Obsolete" (fst(x.personal_targets))
       else ""
      );
      consider_targets "Newly created" (snd(x.personal_targets));
      consider_directories "Obsolete" (fst(x.personal_directories));
      consider_directories "Newly created" (snd(x.personal_directories));
      German_compare_two_modulesystems.display x.file_differences
  
  ] in
  let temp2=List.filter (fun s->s<>"") temp1 in
  if temp2=[] then "" else
  String.concat "\n\n" (temp2@["\n"]);;
  
   