(*

#use"Php_analizer/HRecognizer/hrecognizer_system.ml";;

Manages a modifiable  set of inter-related recognizers.

*)

type t= {
    keywords : (string*(string list)) list;
    atoms : Atomic_hrecognizer.t list;
    unlabelled : Abstractified_nonatomic_hrecognizer.t list;
    labelled : Abstractified_nonatomic_hrecognizer.t list;
};;


