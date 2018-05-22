(*

#use"Pretty_printing/aggregate_ocaml_name.ml";;

*)

let max_line_length=ref(100);;

let agg (Disaggregated_ocaml_name.D l)=
    let rec tempf=(
        fun (graet,etre,ment_etre,da_ober)->
          match da_ober with
          []->String.concat "\n" (etre::graet)
          |a::peurrest->
             let ment_nevez=ment_etre+String.length(a) in
             if ment_nevez<=(!max_line_length)
             then tempf(graet,etre^a,ment_nevez,peurrest)
             else tempf(etre::graet,a,String.length a,peurrest)
    ) in
    match l with
    []->""
    |a::peurrest->tempf([],a,String.length a,peurrest);;

   