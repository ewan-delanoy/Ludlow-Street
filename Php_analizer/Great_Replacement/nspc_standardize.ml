(*

#use"Php_analizer/Great_Replacement/nspc_standardize.ml";;

*)

exception Unusual;;

let string_and_remember_name s=
    if not(Substring.begins_with s "<?php")
    then raise(Unusual)
    else
    let temp1=Lines_in_string.core s in
    let opt1=Option.find_and_stop(fun (j,line)->
          Nspc_detect.extract_namespace_name line) temp1 in
    if opt1<>None           
    then let nahme=fst(Option.unpack opt1) in
         let (_,is_already_standard)=Option.unpack opt1 in
         if is_already_standard
         then (s,nahme)
         else 
         let temp2=Image.image(
           fun (j,line)->
             if  Nspc_detect.test_for_namespace_line line
             then Replace_inside.replace_inside_string
                   (";","{") line
             else line
         ) temp1 in
         (*in PHP syntax, there can be at most one semicoloned
           namespace, so we only need add one right brace. *)
         ((String.concat "\n" temp2)^"}",nahme)
    else
    match Option.seek (fun (j,line)->
    Nspc_detect.test_for_declaration_line line
    ) temp1  with
     None->("<?php\n\nnamespace {\n"^(Cull_string.cobeginning 5 s)^"}","")
    |Some(j1,_)->
        let temp3=Image.image(
            fun (j,line)->
                if  j=j1
                then line^"\n\nnamespace {\n"
                else line
        ) temp1 in
        ((String.concat "\n" temp3)^"}","");;

let string s=
            if not(Substring.begins_with s "<?php")
            then raise(Unusual)
            else
            let temp1=Lines_in_string.core s in
            let opt1=Option.find_and_stop(fun (j,line)->
                  Nspc_detect.extract_namespace_name line) temp1 in
            if opt1<>None           
            then let (_,is_already_standard)=Option.unpack opt1 in
                 if is_already_standard
                 then s
                 else 
                 let temp2=Image.image(
                   fun (j,line)->
                     if  Nspc_detect.test_for_namespace_line line
                     then Replace_inside.replace_inside_string
                           (";","{") line
                     else line
                 ) temp1 in
                 (*in PHP syntax, there can be at most one semicoloned
                   namespace, so we only need add one right brace. *)
                 (String.concat "\n" temp2)^"}"
            else
            match Option.seek (fun (j,line)->
            Nspc_detect.test_for_declaration_line line
            ) temp1  with
             None->"<?php\n\nnamespace {\n"^(Cull_string.cobeginning 5 s)^"}"
            |Some(j1,_)->
                let temp3=Image.image(
                    fun (j,line)->
                        if  j=j1
                        then line^"\n\nnamespace {\n"
                        else line
                ) temp1 in
                (String.concat "\n" temp3)^"}";;        

let file fn=
     let old_text=Io.read_whole_file fn in
     let new_text=string old_text in
     Io.overwrite_with fn new_text;;


(*

string "<?php \na\nb\nc\n namespace uvw ;\ndef\n hi ;\n jk";;
string "<?php \na\nb\nc\n numespace uvw ;\ndef\n hi ;\n jk";;
string "<?php \na\nb\nc\n declare(def);\n jk";;

*)




