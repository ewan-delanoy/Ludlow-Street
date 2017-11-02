(*

#use"Prepare_html/html_text.ml";;

*)


type t=
    T of Html_line.t list;;

(* 
let rec helper (params,graet,weight,da_ober)=
    match da_ober with
     []->String.concat "\n" (List.rev graet)
    |html_line::peurrest->
        let pushed_line=(
            if Html_line.is_pushable line
            then (String.make weight ' ')^line
            else line
        ) in
        helper(params,pushed_line::graet,weight+Html_line.weight line,peurrest);;
*)

let rec apply (T lines) params="";;