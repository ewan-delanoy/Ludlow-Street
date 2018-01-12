(*

#use"Prepare_html/html_indent_tags.ml";;

*)


exception Main_pusher_exn;;

let main_ender (s,n,graet,current_weight,idx_start,idx)=
     let graet2=(
         if idx=idx_start
         then graet
         else let elt=(String.make current_weight ' ')^
                      (Cull_string.interval s idx_start n) in
              elt::graet        
      ) in
      String.concat "\n" (List.rev graet2) ;;

let rec main_pusher (s,n,graet,current_weight,idx_start,idx)=
  if idx>n
  then raise(Main_pusher_exn) 
  else 
  let c=(Strung.get s idx) in
  if c='\n'
  then  let graet2=(
        if idx=idx_start
        then ""::graet
        else let elt=(String.make current_weight ' ')^
                (Cull_string.interval s idx_start idx) in
             elt::graet        
        ) in
        (s,n,graet2,current_weight,idx+1,idx+1)
  else 
  if c<>'<'
  then (s,n,graet,current_weight,idx_start,idx+1)
  else 
  let jdx=Substring.leftmost_index_of_in_from ">" s (idx+1) in
  let uncorrected_elt=Cull_string.interval s idx_start jdx in
  let elt=if Substring.begins_with uncorrected_elt "\n"
          then uncorrected_elt
          else "\n"^uncorrected_elt in
  if (Strung.get s (idx+1))='/'
  then let new_weight=current_weight-(jdx-idx) in
       (s,n,elt::graet,new_weight,jdx+1,idx+1)
  else let new_weight=current_weight+(jdx-idx+1) in
       (s,n,elt::graet,new_weight,jdx+1,idx+1);;

let rec main_helper u=
   let (s,n,graet,current_weight,idx_start,idx)= u in
   if idx>n
   then main_ender u
   else 
   main_helper(main_pusher u);;

let indent s=main_helper(s,String.length s,[],0,1,1);;   

let z1="<html><head>abc</head><body>def</body></html>";;
let z2=indent z1;;
let z3=print_string z2;;



