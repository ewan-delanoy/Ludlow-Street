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
    recognizers : Nonatomic_hrecognizer.t list;
};;

let empty_one={
    avoidables = [];
    atoms = [];
    unlabelled = [];
    labelled = [];
    recognizers = [];
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
    let new_recgzr=Nonatomic_hrecognizer.chain nahme (
        Image.image (fun s->
        Nonatomic_hrecognizer.leaf s (Atomic_hrecognizer.constant s)
        ) parts
    ) in
    {
        avoidables = new_avoidables;
        atoms = x.atoms;
        unlabelled = x.unlabelled;
        labelled = x.labelled;
        recognizers = x.recognizers@[new_recgzr];
   };;

let add_atom x (nahme,atm)=
    let _=(check_that_name_is_not_used x nahme) in
    let new_recgzr=
        Nonatomic_hrecognizer.leaf nahme atm in
    {
        avoidables = x.avoidables;
        atoms = (x.atoms)@[nahme,atm];
        unlabelled = x.unlabelled;
        labelled = x.labelled;
        recognizers = x.recognizers@[new_recgzr];
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
        recognizers = [];
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
        recognizers = [];
    };;








