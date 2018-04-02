(*

#use"Php_analizer/HRecognizer/order_for_jrecognizer_chains.ml";;

If none of the two chains is an initial segment of the other, dictionary
order is applied.
If one of the two chains is an initial segment of the other, the longest comes
first (so as not to be overriden by the other one).

*)


let order=
   let rec tempf=(fun l1 l2->
      if l1=[]
      then if l2=[] then Total_ordering.Equal else Total_ordering.Greater
      else 
      let (a1,peurrest1)=Listennou.ht l1 in
      match l2 with
      []->Total_ordering.Lower
      |a2::peurrest2->
        if a1=a2 then tempf peurrest1 peurrest2 else   
       (* Different recognizers have different names *) 
       Total_ordering.lex_for_strings
         (Nonatomic_jrecognizer.name a1)
         (Nonatomic_jrecognizer.name a2) 
   )
   in
   (tempf: (Nonatomic_jrecognizer.t list) Total_ordering.t);;

let order_for_pairs=((Total_ordering.from_snd order): 
  (string * Nonatomic_jrecognizer.t list) Total_ordering.t);;  