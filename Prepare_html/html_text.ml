(*

#use"Prepare_html/html_text.ml";;

*)


type t=
    T of Html_line.t list;;

(*  
let helper (params,graet,weight,da_ober)=
    match da_ober with
     []->String.concat "\n" (List.rev graet)
    |line::peurrest->
        let pushed_line=(
            if Html_line.is_pushable line
            then (String.make weight ' ')^line
            else line
        )
*)

let rec apply (T lines) params="";;