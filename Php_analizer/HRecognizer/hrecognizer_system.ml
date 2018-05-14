(*

#use"Php_analizer/HRecognizer/hrecognizer_system.ml";;

Manages a modifiable  set of inter-related recognizers.

Rules : avoidables are always constant Atomic_hrecognizer.t objects, 
or concatenation of such.

*)



type t= {
    avoidables : List_of_avoidables.t;
    definitions : Abstractified_nonatomic_hrecognizer.t list;
    recognizers : Nonatomic_hrecognizer.t list;
    outermost_definition : string list;
    outermost_recognizer : Nonatomic_hrecognizer.t list;
    counter_for_anonymous_recognizers : int;
};;

let empty_one={
    avoidables = List_of_avoidables.empty_one;
    definitions = [];
    recognizers = [];
    outermost_definition = [];
    outermost_recognizer = [];
    counter_for_anonymous_recognizers =0;
};;

let name_is_used x nahme=
      (List_of_avoidables.name_already_used x.avoidables nahme)
      ||
      (List.exists 
       (fun rcgzr->Abstractified_nonatomic_hrecognizer.name rcgzr=nahme) 
       x.definitions);; 

exception Check_that_name_is_used_exn of string;;      

let check_that_name_is_used x nahme=
     if name_is_used x nahme then () else raise(Check_that_name_is_used_exn(nahme));;
     
exception Check_that_name_is_not_used_exn of string;;      

let check_that_name_is_not_used x nahme=
      if not(name_is_used x nahme) then () else raise(Check_that_name_is_not_used_exn(nahme));;

let add_avoidable_item x avdbl nahme parts=
    let _=(check_that_name_is_not_used x nahme) in
    let new_avoidables=List_of_avoidables.add_new_element x.avoidables avdbl nahme parts in 
    let new_definition=Abstractified_nonatomic_hrecognizer.Chain(nahme,parts) in
    let new_recgzr=Nonatomic_hrecognizer.chain nahme (
        Image.image (fun s->
        Nonatomic_hrecognizer.leaf s (Atomic_hrecognizer.constant s)
        ) parts
    ) in
    {
        avoidables = new_avoidables;
        definitions = x.definitions@[new_definition];
        recognizers = x.recognizers@[new_recgzr];
        outermost_definition = x.outermost_definition;
        outermost_recognizer = x.outermost_recognizer;
        counter_for_anonymous_recognizers = x.counter_for_anonymous_recognizers;
   };;



let add_definition x defn=
    let nahme=Abstractified_nonatomic_hrecognizer.name defn 
    and support=Abstractified_nonatomic_hrecognizer.support defn  in
    let _=(check_that_name_is_not_used x nahme;
           List.iter (check_that_name_is_used x) support) in
    let new_recgzr = Concretize_hrecognizer.concretize 
       (x.avoidables,x.recognizers)  defn in
    {
        avoidables = x.avoidables;
        definitions = x.definitions@[defn];
        recognizers = x.recognizers@[new_recgzr];
        outermost_definition = x.outermost_definition;
        outermost_recognizer = x.outermost_recognizer;
        counter_for_anonymous_recognizers = x.counter_for_anonymous_recognizers;
    };;


exception Unused_name_in_outermost_addition of string;;

(*
let add_in_outermost x name=
    let _=check_that_name_is_used x name in
    let new_disjunctees_list=
        Ordered.insert_plaen Total_ordering.lex_for_strings name x.outermost_definition in
*)    
    

(*
let temp2=Ennig.index_everything temp1 in
  let temp3=Image.image (fun (j,(x,l))->(x,
     Nonatomic_hrecognizer.chain ("anon_"^(string_of_int j)) l)) temp2 in
  let temp4=Prepared.partition_according_to_fst temp3 in
  let temp5=Image.image (fun (name,l)->
       (name,Nonatomic_hrecognizer.ordered_disjunction name l)) temp4 in
    Ordered.diforchan_plaen Hrecognizer_related_order.for_labelled_ones temp5
  ;;
*)


