(*

#use"directory_summary.ml";;

List of all files present, with their modification dates.

*)

type t={
    root : Directory_name.t ;
    subdirectories : string list;
    files : (string*float) list;
};;

let make dir l1 l2={
    root = dir; 
    subdirectories=Ordered.forget_order(Ordered_string.diforchan l1);
    files=Ordered.forget_order(
    Ordered.diforchan Total_ordering.for_longest_match_pairs  
    l2
    );
};;

let empty_instance dir =make dir [] [];;

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
      root=dir;
      subdirectories=temp4;
      files=temp5;
    }
    ;;


let ocaml_description x=
   "D"^"irectory_summary"^"."^"make\n\n"^
   ("("^Directory_name.ocaml_name x.root)^")\n\n"^
   (Copyable_printing.print_stringlist 3 x.subdirectories)^"\n\n"^
   (Copyable_printing.print_sbf_list 3 x.files);;
  
module Private=struct   

let compute_differences old_x new_x=
    let (_,o_removed_dirs,o_new_dirs)=Ordered_string.cooperation_for_two 
    (Ordered_string.safe_set old_x.subdirectories)
    (Ordered_string.safe_set new_x.subdirectories) in
    let removed_dirs=Ordered.forget_order o_removed_dirs
    and new_dirs=Ordered.forget_order o_new_dirs in
    let temp1=Image.image fst old_x.files
    and temp2=Image.image fst new_x.files in
    let (o_common_files,o_removed_files,o_new_files)=Ordered_string.cooperation_for_two 
    (Ordered_string.diforchan temp1)
    (Ordered_string.diforchan temp2) in
    let common_files=Ordered.forget_order o_common_files
    and removed_files=Ordered.forget_order o_removed_files
    and new_files=Ordered.forget_order o_new_files in
    let changed_files=List.filter (
       fun fn->
       (List.assoc fn old_x.files)
       <>
       (List.assoc fn new_x.files)
    ) common_files in 
    ((removed_dirs,new_dirs),(changed_files,removed_files,new_files));;
  
let commands_for_removed_dirs 
 (remote_port,remote_host,remote_dir)  
 removed_dirs=Image.image(fun dir->
    "ssh -p "^(string_of_int remote_port)^
    " "^remote_host^" "^
    (Strung.enclose ("rm -rf "^remote_dir^"/"^dir))
 ) removed_dirs;;    

let commands_for_new_dirs 
 (remote_port,remote_host,remote_dir)  
 new_dirs=Image.image(fun dir->
    "ssh -p "^(string_of_int remote_port)^
    " "^remote_host^" "^
    (Strung.enclose ("mkdir -p "^remote_dir^"/"^dir))
 ) new_dirs;;    

let commands_for_changed_or_new_files 
 (remote_port,remote_host,remote_dir)  
 root files=Image.image(fun fn->
    let above_fn=Father_and_son.father fn '/'  in
    "scp -p "^(string_of_int remote_port)^
    " "^(Directory_name.connectable_to_subpath root)^fn^
    " "^remote_host^":"^remote_dir^"/"^above_fn
 ) files;;  

let commands_for_removed_files
  (remote_port,remote_host,remote_dir)  
    removed_files=Image.image(fun fn->
   "ssh -p "^(string_of_int remote_port)^
   " "^remote_host^
   (Strung.enclose "rm -f "^remote_dir^"/"^fn)
) removed_files;;  

let commands_for_remote_update 
  data (old_x,new_x)=
    let ((removed_dirs,new_dirs),(changed_files,removed_files,new_files))=
        compute_differences  old_x new_x in
    List.flatten [
       commands_for_removed_dirs data removed_dirs;
       commands_for_new_dirs data new_dirs;
       commands_for_removed_files data removed_files;
       commands_for_changed_or_new_files data old_x.root (changed_files@new_files)
    ];;




end;;    

(*   

let dir1=Directory_name.of_string "Remembered/Tests";;   
let g1=compute dir1;;
let g2=ocaml_description g1;;
print_string ("\n\n\n let g3="^g2^";;\n\n\n");;

*)