(*

#use"Kernighan/gparser.ml";;

*)

type t={
   description : Gparser_description.t;
   worker : string ->int-> Gparser_result.t option;
};;

let description x=x.description;;
let apply x s i=x.worker s i;;

let veil a b={
	description=a;
	worker=b;
};;
