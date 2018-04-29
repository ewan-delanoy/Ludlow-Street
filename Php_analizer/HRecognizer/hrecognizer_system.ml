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
      ;;
      (*
      ||
      (List.exists (fun rcgzr->n=nahme) x.unlabelled)
       *)


let add_keyword x (s,l)={
   keywords = x.keywords@[s,l];
   atoms = x.atoms;
   unlabelled = x.unlabelled;
   labelled = x.labelled;
};;

let add_atom x atm={
   keywords = x.keywords;
   atoms = (x.atoms)@[atm];
   unlabelled = x.unlabelled;
   labelled = x.labelled;
};;

let add_unlabelled x ulab={
   keywords = x.keywords;
   atoms = x.atoms;
   unlabelled = x.unlabelled@[ulab];
   labelled = x.labelled;
};;

let add_labelled x lab={
   keywords = x.keywords;
   atoms = x.atoms;
   unlabelled = x.unlabelled;
   labelled = x.labelled@[lab];
};;








