(*

#use"debugger.ml";;
Used for debugging purposes only.


*)


let helper_content=
"variable assign ##( () ?  _l_ id _u_ sqs _rd_  :  _l_ id _u_ variable _rd_  )## ;";;

let trmt=Termite.of_string helper_content;;

Php_constructible_recognizer.helper_for_string_reading
Php_constructible_recognizer.of_string
(Some ("_l_", "_rd_"), "id _u_ sqs");;