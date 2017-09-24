(*

Wrapper on the Sys dot command function.

#use"unix_command.ml";;

*)


exception Command_failed of string;;

let accu=ref([]:string list);;
let remember_commands_mode=ref(false);;


let hardcore_uc s=
   let i=Sys.command s in
   if i<>0
   then raise(Command_failed(s))
   else let _=(if (!remember_commands_mode) then accu:=s::(!accu)) in 
        i;;

let hardcore_verbose_uc s=
   let _=(print_string ("Executing "^s);flush stdout) in
   hardcore_uc s;;

let mild_uc s=
   let i=Sys.command s in
   let _=(
   if i<>0
   then (print_string ("Failed during "^s);flush stdout)
   else (if (!remember_commands_mode) then accu:=s::(!accu))
   ) in
   i;;

let hardcore_mode=ref(false);;

let uc s=
   if (!hardcore_mode)
   then hardcore_uc s
   else mild_uc s;;


