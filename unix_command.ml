(*

Wrapper on the Sys dot command function.

#use"unix_command.ml";;

*)


exception Command_failed of string;;

let accu=ref([]:string list);;

let uc s=
   let i=Sys.command s in
   if i<>0
   then raise(Command_failed(s))
   else let _=(accu:=s::(!accu)) in i;;



