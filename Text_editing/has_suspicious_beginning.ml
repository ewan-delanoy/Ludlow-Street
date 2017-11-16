(*

#use"Text_editing/has_suspicious_beginning.ml";;

*)

let hsb paragraph=
      let temp=Cull_string.trim_spaces_on_the_left paragraph in
      if List.exists(
        fun beg->Substring.begins_with temp beg
      ) ["\195\168";"\195\169"]
      then true
      else 
      let i=int_of_char(String.get temp 0) in
      (97<=i)&&(i<=122);;

