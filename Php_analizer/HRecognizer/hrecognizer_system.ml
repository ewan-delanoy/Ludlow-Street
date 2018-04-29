(*

#use"Php_analizer/HRecognizer/hrecognizer_system.ml";;

Manages a modifiable  set of inter-related recognizers.

Rules : avoidables are always constant Atomic_hrecognizer.t objects, or concatenation of such.

*)

type t= {
    avoidables : (Avoider_label.t * ((string*(string list)) list)) list;
    atoms : (string*Atomic_hrecognizer.t) list;
    unlabelled : Abstractified_nonatomic_hrecognizer.t list;
    labelled : Abstractified_nonatomic_hrecognizer.t list;
};;

let empty_one={
    avoidables = [];
    atoms = [];
    unlabelled = [];
    labelled = [];
};;

let name_is_used x nahme=
      (List.exists (fun (_,l)->List.exists (fun (n,_)->n=nahme) l) x.avoidables)
      ||
      (List.exists (fun (n,_)->n=nahme) x.atoms)
      ||
      (List.exists (fun rcgzr->Abstractified_nonatomic_hrecognizer.name rcgzr=nahme) x.unlabelled)
      ||
      (List.exists (fun rcgzr->Abstractified_nonatomic_hrecognizer.name rcgzr=nahme) x.labelled);; 

exception Check_that_name_is_used_exn of string;;      

let check_that_name_is_used x nahme=
     if name_is_used x nahme then () else raise(Check_that_name_is_used_exn(nahme));;
     
exception Check_that_name_is_not_used_exn of string;;      

let check_that_name_is_not_used x nahme=
      if not(name_is_used x nahme) then () else raise(Check_that_name_is_not_used_exn(nahme));;

let add_avoidable_item x avdbl nahme parts=
    let _=(check_that_name_is_not_used x nahme) in
    let new_avoidables=(
      if List.for_all (fun (lbl,l)->lbl<>avdbl) x.avoidables 
      then  x.avoidables@[avdbl,[nahme,parts]] 
      else     
    Image.image (
      fun (lbl,l)->
        let new_l=(if lbl=avdbl then l@[nahme,parts] else l) in
        (lbl,new_l)
    ) x.avoidables) in 
    {
        avoidables = new_avoidables;
        atoms = x.atoms;
        unlabelled = x.unlabelled;
        labelled = x.labelled;
   };;

let add_atom x (nahme,atm)=
    let _=(check_that_name_is_not_used x nahme) in
    {
        avoidables = x.avoidables;
        atoms = (x.atoms)@[nahme,atm];
        unlabelled = x.unlabelled;
        labelled = x.labelled;
    };;

let add_unlabelled x ulab=
    let nahme=Abstractified_nonatomic_hrecognizer.name ulab 
    and support=Abstractified_nonatomic_hrecognizer.support ulab  in
    let _=(check_that_name_is_not_used x nahme;List.iter (check_that_name_is_used x) support) in
    {
        avoidables = x.avoidables;
        atoms = x.atoms;
        unlabelled = x.unlabelled@[ulab];
        labelled = x.labelled;
    };;

let add_labelled x lab=
    let nahme=Abstractified_nonatomic_hrecognizer.name lab 
    and support=Abstractified_nonatomic_hrecognizer.support lab  in
    let _=(check_that_name_is_not_used x nahme;List.iter (check_that_name_is_used x) support) in
    {
        avoidables = x.avoidables;
        atoms = x.atoms;
        unlabelled = x.unlabelled;
        labelled = x.labelled@[lab];
    };;








