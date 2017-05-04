(*

#use"check_ocaml_dircopy.ml";;

Usable on a github clone of the remote master version.

*)


let is_admissible s=
  (List.for_all (
     fun edg->not(Substring.ends_with s ("."^edg))
  ) ["depend";"ocamlinit";"cmi";"cmo";"DS_Store";"txt";"php";
     "ocaml_made";"ocaml_debuggable"])
  &&
   (List.for_all (
     fun beg->not(Substring.begins_with s beg)
  ) ["Remembered/";"Forgotten/"])
  &&
  (
    not(
    List.mem s
    ["debugger.ml";"my_loadings.ml";
     "ecaml";"makefile";"neptu"]
    )
  )
  ;;

let check remotedir=
  let diff=Prepare_dircopy_update.compute_greedy_diff
     German_constant.root remotedir in
  let rc1=List.filter is_admissible (Dircopy_diff.recently_changed diff)
  and rc2=List.filter is_admissible (Dircopy_diff.recently_changed diff)
  and rc3=List.filter is_admissible (Dircopy_diff.recently_changed diff) in
  (rc1,rc2,rc3);;
