(*

Operation on substring finding, with indexes starting from 1.

#use"find_substring.ml";;


*)



let begins_with x y=
      let ly=String.length(y) in
      if String.length(x)<ly
      then false
      else (String.sub x 0 ly)=y;;
      
 let is_the_beginning_of y x=begins_with x y;;     
   
 let ends_with x y=
      let ly=String.length(y) in
      if String.length(x)<ly
      then false
      else (String.sub x ((String.length x)-ly) ly)=y;;  
   
 let is_the_ending_of y x=ends_with x y;;  

 let is_a_substring_located_at y x old_j =
    let j=old_j-1 in
    let ly=String.length(y) in
      if (String.length(x)<j+ly)||(j<0)
      then false
      else (String.sub x j ly)=y;;
 
  let is_a_substring_of x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      Ennig.exists tester 0 (String.length(y)-lx);; 
      
  let leftmost_index_of_in x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      try (Option.unpack(Ennig.find_it tester 0 (String.length(y)-lx))+1) with
      _->(-1);;
  
  let rightmost_index_of_in x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) 
      and temp1=List.rev(Ennig.ennig(0)(String.length(y)-lx)) in
      try ((Option.find tester temp1)+1) with
      _->(-1);;
  
   let leftmost_index_of_in_from x y i=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      match Ennig.find_it tester (i-1) (String.length(y)-lx) with
         None->(-1)
        |Some(k)->k+1;;
  
 let occurrences_of_in x y=
   let lx=String.length x 
   and n=String.length y in
   let rec tempf=(fun (j,accu)->
      if j>n then List.rev(accu) else
      let k=leftmost_index_of_in_from x y j in
      if k<0 then List.rev(accu) else
      tempf(k+lx,k::accu)
   )  in
   tempf (1,[]);;

 
   