(*

#use"Php_analizer/Great_Replacement/publicize_block.ml";;

*)



exception Nonunique_block_beginning of string;;
exception Bad_end_for_block_beginning of string;;

let rememberer=ref"";;

let in_string beginning_of_block s=
    if not(Substring.ends_with beginning_of_block "{")
    then raise(Bad_end_for_block_beginning(beginning_of_block))
    else
    let temp1=Substring.occurrences_of_in beginning_of_block s in
    if List.length(temp1)<>1
    then raise(Nonunique_block_beginning(beginning_of_block))
    else
    let i=(List.hd temp1)+(String.length beginning_of_block) in
    let j=(After.after_closing_character ('{','}') s (i,1))-2 in
    let old_block=Cull_string.interval s i j in
    let new_block=Replace_inside.replace_several_inside_string
       [
         "protected","public   ";
         "private","public "
       ] old_block in
    let _=(rememberer:=new_block) in   
    (Cull_string.beginning (i-1) s)^
    new_block^
    (Cull_string.cobeginning j s) 
       ;;

(*

in_string "3{" "123{56private{protected}56private}5";;

*)

let in_file beginning_of_block fn=
     let old_text=Io.read_whole_file fn in
     let new_text=in_string beginning_of_block old_text in
     Io.overwrite_with fn new_text;;





