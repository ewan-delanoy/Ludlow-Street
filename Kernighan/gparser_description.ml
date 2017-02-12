(*

#use"Kernighan/gparser_description.ml";;

*)


type t={
   name : string;
   parameters : string list;
};;

let name x=x.name;;
let parameters x=x.parameters;;
let wrap x=Gparser_special_character.wrap(name x);;

let chain l=
  let l_names=Image.image wrap l
  and l_params=Image.image parameters l in
  let sep=String.make 1 Gparser_special_character.separator_for_chain in
  {
    name=String.concat sep l_names;
    parameters=List.flatten l_params;
  };;

let disjunction l=
  let l_names=Image.image wrap l
  and l_params=Image.image parameters l in
  let sep=String.make 1 Gparser_special_character.separator_for_disjunction in
  {
    name=String.concat sep l_names;
    parameters=List.flatten l_params;
  };;

let star x=
  {
    name=(wrap x)^(String.make 1 (Gparser_special_character.marker_for_star));
    parameters=x.parameters;
  };;

let one_or_more x=
  {
    name=(wrap x)^(String.make 1 (Gparser_special_character.marker_for_one_or_more));
    parameters=x.parameters;
  };;


let optional x=
  {
    name=(wrap x)^(String.make 1 (Gparser_special_character.marker_for_optional));
    parameters=x.parameters;
  };;




