(*

#use"directory_summary.ml";;

List of all files present, with their modification dates.

*)

type t={
    subdirectories : string list;
    files : (string*float) list;
};;

let compute dir=
    let temp1=More_unix.complete_ls dir in
    let (temp2,temp3)=List.partition (
         More_unix.is_a_directory
    ) temp1 in
    let tempf=(fun ap->
       let s_ap=Absolute_path.to_string ap in
       Directory_name.cut_beginning dir s_ap
    ) in
    let temp4=Image.image tempf temp2
    and temp5=Image.image (fun ap->
      let st=Unix.stat(Absolute_path.to_string ap) in
    (tempf(ap),st.Unix.st_mtime)) temp3 
    in
    {
      subdirectories=temp4;
      files=temp5;
    }
    ;;

(*    
let ocaml_description =    
*)
