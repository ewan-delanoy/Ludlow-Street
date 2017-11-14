(*


#use"Text_editing/decompose_into_paragraphs.ml";;

*)

module Private=struct

type state=
      Inside_paragraph
     |Exiting_paragraph
     |Outside_paragraph;;

let eitv s i j=((i,j),Cull_string.interval s i j);;

let inside_paragraph
    (graet,s,n,idx_start,idx,idx_last)=
       if idx>n
       then (true,Inside_paragraph,(graet,s,n,idx_start,n,0))
       else
       let c=Strung.get s idx in
       if c='\n'
       then  (false,Exiting_paragraph,(graet,s,n,idx_start,idx+1,idx-1))
       else  (false,Inside_paragraph,(graet,s,n,idx_start,idx+1,0));;

let exiting_paragraph
    (graet,s,n,idx_start,idx,idx_last)=
        if idx>n
        then (true,Exiting_paragraph,(graet,s,n,idx_start,n,idx_last))
        else
        let c=Strung.get s idx in
        if c='\n'
        then let graet2=(true,(eitv s idx_start idx_last))::graet in 
             (false,Outside_paragraph,(graet2,s,n,idx_last+1,idx+1,0))
        else 
        if List.mem c [' ';'\r';'\t']
        then (false,Exiting_paragraph,(graet,s,n,idx_start,idx+1,idx_last))
        else (false,Inside_paragraph,(graet,s,n,idx_start,idx+1,0));;

let outside_paragraph
    (graet,s,n,idx_start,idx,idx_last)=
       if idx>n
       then (true,Outside_paragraph,(graet,s,n,idx_start,n,0))
       else
       let c=Strung.get s idx in
       if List.mem c [' ';'\r';'\t';'\n']
       then  (false,Outside_paragraph,(graet,s,n,idx_start,idx+1,0))
       else let graet2=(
              if idx=1
              then graet
              else (false,(eitv s idx_start (idx-1)))::graet) in  
            (false,Inside_paragraph,(graet2,s,n,idx,idx+1,0));;
           

let close_computation (state_v,(graet,s,n,idx_start,idx,idx_last))=
    if idx_start=idx
    then List.rev(graet)
    else 
    match state_v with
    Inside_paragraph->List.rev( (true,eitv s idx_start idx) ::graet)
   |Exiting_paragraph->List.rev( (false,eitv s (idx_last+1) n):: 
                                 (true,eitv s idx_start idx_last) ::graet)
   |Outside_paragraph->List.rev( (false,eitv s idx_start n) ::graet);;

let next_item state_v p=
    match state_v with
     Inside_paragraph->inside_paragraph p
    |Exiting_paragraph->exiting_paragraph p
    |Outside_paragraph->outside_paragraph p;;
 
let rec main_helper (bowl,state_v,p)=
     if bowl 
     then close_computation (state_v,p)
     else main_helper(next_item state_v p);;

end;;

let dec s=Private.main_helper(false,Private.Outside_paragraph,
   ([],s,String.length s,1,1,0));;     


(*
dec "xyz";;
dec "xyz\n";;
dec "xyz\n     ";;
dec "xyz\n\n";;
dec "xyz\n\n\n";;
dec "\nxyz";;
dec "\n\nxyz";;
dec "\n\n\nxyz";;
dec " xyz";;
dec "xyz\n\nabc\n\ndef";;
dec "xyz\n abc\n\ndef";;
*)