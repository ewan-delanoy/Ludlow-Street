(*
#use"Php_analizer/HRecognizer/order_for_hrecognizer_chains.ml";;

If none of the two chains is an initial segment of the other, dictionary
order is applied.
If one of the two chains is an initial segment of the other, the longest comes
first (so as not to be made redundant by the other one).

*)

let for_unlabelled_ones =
   (* Different recognizers have different names, so we just need to look at the names *) 
  ((fun x y->
  Total_ordering.lex_for_strings
  (Nonatomic_hrecognizer.name x)
  (Nonatomic_hrecognizer.name y) 
  ):
   Nonatomic_hrecognizer.t Total_ordering.t);; 

let for_lists=
  let rec tempf=(fun l1 l2->
     if l1=[]
     then if l2=[] then Total_ordering.Equal else Total_ordering.Greater
     else 
     let (a1,peurrest1)=Listennou.ht l1 in
     match l2 with
     []->Total_ordering.Lower
     |a2::peurrest2->
       if a1=a2 then tempf peurrest1 peurrest2 else   
      for_unlabelled_ones a1 a2
  )
  in
  (tempf: (Nonatomic_hrecognizer.t list) Total_ordering.t);;

let for_labelled_ones=
  ((fun x y->Total_ordering.from_snd for_unlabelled_ones x y):
   ('a * Nonatomic_hrecognizer.t) Total_ordering.t);; 