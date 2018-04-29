(*

#use"Php_analizer/HRecognizer/hrecognizer_system.ml";;

Manages a modifiable  set of inter-related recognizers.

*)

type t= {
    keywords : (string*(string list)) list;
    atoms : (string*Atomic_hrecognizer.t) list;
    unlabelled : Abstractified_nonatomic_hrecognizer.t list;
    labelled : Abstractified_nonatomic_hrecognizer.t list;
};;

let empty_one={
    keywords = [];
    atoms = [];
    unlabelled = [];
    labelled = [];
};;

let name_is_used x nahme=
      (List.exists (fun (n,_)->n=nahme) x.keywords)
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

let add_keyword x (s,l)=
    let _=(check_that_name_is_not_used x s;List.iter (check_that_name_is_used x) l) in
    {
        keywords = x.keywords@[s,l];
        atoms = x.atoms;
        unlabelled = x.unlabelled;
        labelled = x.labelled;
   };;

let add_atom x (nahme,atm)=
    let _=(check_that_name_is_not_used x nahme) in
    {
        keywords = x.keywords;
        atoms = (x.atoms)@[nahme,atm];
        unlabelled = x.unlabelled;
        labelled = x.labelled;
    };;

let add_unlabelled x ulab=
    let nahme=Abstractified_nonatomic_hrecognizer.name ulab 
    and support=Abstractified_nonatomic_hrecognizer.support ulab  in
    let _=(check_that_name_is_not_used x nahme;List.iter (check_that_name_is_used x) support) in
    {
        keywords = x.keywords;
        atoms = x.atoms;
        unlabelled = x.unlabelled@[ulab];
        labelled = x.labelled;
    };;

let add_labelled x lab=
    let nahme=Abstractified_nonatomic_hrecognizer.name lab 
    and support=Abstractified_nonatomic_hrecognizer.support lab  in
    let _=(check_that_name_is_not_used x nahme;List.iter (check_that_name_is_used x) support) in
    {
        keywords = x.keywords;
        atoms = x.atoms;
        unlabelled = x.unlabelled;
        labelled = x.labelled@[lab];
    };;








