(*

#use"Prepare_html/Parametrization_related/html_parametrized_atom.ml";;

*)

type t= ((Html_param_config.t)*string) list;;

exception Unknown_configuration of Html_param_config.t;;

let eval pc p_atom=
  match Option.seek (fun (pc2,txt)->
      Html_param_config.test_for_inclusion pc pc2
  ) p_atom with
  None->raise(Unknown_configuration(pc))
  |Some(_,txt)->txt;;

  