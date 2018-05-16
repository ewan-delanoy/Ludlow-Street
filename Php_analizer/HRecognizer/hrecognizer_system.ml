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


exception Unused_name_in_outermost_insertion of string;;



let insert_in_outermost x name=
    let opt=Option.seek (
        fun rcgzr->Nonatomic_hrecognizer.name rcgzr=name
    ) x.recognizers in
    if opt=None
    then raise(Unused_name_in_outermost_insertion(name))
    else 
    let inserted_one=Option.unpack opt in
    let opt2=
        Check_hrecognizers_disjointness.repair_disjunction_list_of_recognizers
      (x.counter_for_anonymous_recognizers) (inserted_one::x.outermost_recognizer) in
    if opt2=None then x else
    let (new_counter_value,outers_with_their_suites,outers_without_their_suites)=Option.unpack opt2 in
    let temp1=List.combine x.definitions x.recognizers in
    let temp2=Image.image (
         fun (defn,rcgzr)->
           let name2=Nonatomic_hrecognizer.name  rcgzr in
           match Option.seek (
            fun (rcgzr2,suite2)->Nonatomic_hrecognizer.name rcgzr2=name2
           ) outers_with_their_suites with
           None->[defn,rcgzr]
           |Some(rcgzr3,suite3)->suite3@[defn,rcgzr]
    ) temp1 in
    let temp3=List.flatten temp2 in
    let new_defn_list=Image.image fst temp3 
    and new_rcgzr_list=Image.image snd temp3 in
    {
        avoidables = x.avoidables;
        definitions = new_defn_list;
        recognizers = new_rcgzr_list;
        outermost_definition = Image.image Nonatomic_hrecognizer.name outers_without_their_suites ;
        outermost_recognizer = outers_without_their_suites;
        counter_for_anonymous_recognizers = new_counter_value;
    };;   
   
              

