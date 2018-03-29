(*

#use"neptu.ml";;

*)

let s_idaho_ap="/Users/ewandelanoy/Documents/OCaml/Idaho";;
let idaho_ap=Absolute_path.of_string s_idaho_ap;;
let idaho_dir=Directory_name.of_string s_idaho_ap;;


let act1=Sys.command ("rm -rf "^s_idaho_ap^"/*");;
let act2=Sys.command ("mkdir -p "^s_idaho_ap^"/_build");;

let aa_file=Absolute_path.create_file (s_idaho_ap^"/a.ml");;
let bb_file=Absolute_path.create_file (s_idaho_ap^"/b.ml");;


let act3=Io.overwrite_with aa_file "type t=U | V;;";;
let act4=Io.overwrite_with bb_file "let f=function A.U->1 |A.V->2;;";;


let (mdata,tgts,outsiders,preqt)=
   Alaskan_create_target_system.from_main_directory idaho_dir None [];;

let dirs=Alaskan_data.compute_subdirectories_list mdata;;

let constants=(
    idaho_dir,German_constant.main_toplevel_name,
    German_constant.name_for_makefile,
    German_constant.name_for_targetfile,
    German_constant.name_for_loadingsfile,
    German_constant.name_for_printersfile
    );;   

let uple=(mdata,dirs,tgts,
      [],[],
      Recently_deleted.of_string_list [],
      Recently_changed.of_string_list [],
      Recently_created.of_string_list [],
      preqt);;

Alaskan_save_all.write_all constants uple;;



(*
let ur s=Nonatomic_hrecognizer.unveil (Hregistrar.recognizer_with_name s);;


let z1=(!(Hregistrar.Private.the_list));;

let z2=Option.filter_and_unpack (
   fun (name,r)->
   let (sort,l,_,_)=Nonatomic_hrecognizer.unveil r in
   if sort<>"ordered_disjunction"
   then None
   else Some(name,l)
) z1;;

let transformer (name,li)=
  let temp1=Image.image 
   (fun r->(r,Check_hrecognizers_disjointness.flatten_nonatomic_hrecognizer r)) li in
  let temp2=Uple.list_of_pairs temp1 in
  let temp3=Image.image (fun ((xn,xc),(yn,yc))->(name,xn,yn,xc,yc)) temp2 in
  temp3;;

let z3=List.flatten (Image.image transformer z2);;

let z4=Option.filter_and_unpack
Check_hrecognizers_disjointness.main_problem_finder
z3;;

let z5=List.hd z4;;
*)


(*
Check_hrecognizers_disjointness.common_prefix (rn "linebreak");;
Check_hrecognizers_disjointness.common_prefix (rn "space");;
*)



(*

let ingr_for_top =Alaskan_ingredients_for_ocaml_target.marked_ingredients_for_unprepared_toplevel;;



let extended_unit_make dir (bowl,(mdata,tgts)) tgt=
    if (not bowl)
    then (bowl,(mdata,tgts),1)
    else
    if Alaskan_make_ocaml_target.is_up_to_date dir tgts tgt
    then (true,(mdata,tgts),2)
    else 
    let temp1=Image.image Unix_command.uc (Alaskan_command_for_ocaml_target.command_for_ocaml_target dir mdata tgt) in 
    if List.for_all (fun i->i=0) temp1
    then let opt_tgt=(if Ocaml_target.is_a_debuggable tgt 
                      then None 
                      else (Some tgt)) in
         let tgts2=Option.add_perhaps opt_tgt tgts in
          match Ocaml_target.ml_from_lex_or_yacc_data tgt with
         None->(true,(mdata,tgts2),3)
         |Some(mlx)->
                     let mdata2=Alaskan_force_modification_time.update dir mdata mlx in
                     (true,(mdata2,tgts2),3)        
    else (false,(mdata,tgts),3);;

let prepare_recompilation ()=
   let tolerate_cycles=false 
   and (old_mdata,old_tgts)=(!German_wrapper.Private.data_ref,
   !German_wrapper.Private.up_to_date_targets_ref) in
   let ((new_mdata,hms_to_be_updated),short_paths)=
    German_recompile.on_monitored_modules tolerate_cycles old_mdata in
   if hms_to_be_updated=[] then failwith("nothing to compile") else
   let new_dirs=German_directories.from_data new_mdata 
   and new_tgts1=Ocaml_target.still_up_to_date_targets hms_to_be_updated old_tgts in
   let dir=German_constant.root in
   let checker=(Ocaml_target.test_target_existence dir) in
   let new_tgts=List.filter checker new_tgts1 in
   let default_top=(German_data.default_toplevel new_mdata) in
   let opt2=Ocaml_target.toplevel_data default_top in
   if opt2=None then failwith("This is Zorglub") else
   let (name,l)=Option.unpack opt2 in
   let temp1=ingr_for_top new_mdata name l in
   (dir,name,temp1,new_mdata,new_tgts,new_dirs,short_paths);;

let finish_compilation
   (dir,name,new_dirs,short_paths,successful_ones,ts2,_)
  = 
   let new_toplevel=Ocaml_target.toplevel name successful_ones in
   let (_,(new_mdata,new_tgts),_)=
    extended_unit_make dir (true,ts2) new_toplevel in
   let changes=German_changed.update short_paths 
      (!(German_wrapper.Private.recently_changed_ref)) in
   let _=(
         German_wrapper.Private.data_ref:=new_mdata;
         German_wrapper.Private.directories_ref:=new_dirs;
         German_wrapper.Private.up_to_date_targets_ref:=new_tgts;
         German_wrapper.Private.recently_changed_ref:=changes;
   ) in
   let _=German_wrapper.Private.save_all() in
   ();;

let  extended_pusher dir (successful_ones,to_be_treated,ts,j,compilation_idx)=
    match to_be_treated with
    []->raise(Alaskan_make_ocaml_target.Ending_for_toplevel_pusher)
    |(tgt,is_an_ending_or_not)::others->
    let (bowl2,ts2,cidx2)=extended_unit_make dir (true,ts) tgt in
    if bowl2
    then let new_successful_ones=(
           if is_an_ending_or_not=Is_an_ending_or_not.Yes
           then let hm=Option.unpack(Ocaml_target.main_module tgt) in
                (*
                  Note that the cmi and cmo give the same hm
                *)
                if List.mem hm successful_ones
                then successful_ones
                else hm::successful_ones
           else successful_ones
         ) in
         (new_successful_ones,others,ts2,j+1,cidx2)
    else let hm=Option.unpack(Ocaml_target.main_module tgt) in
         let root=Half_dressed_module.bundle_main_dir hm in
         let s_root=Directory_name.connectable_to_subpath root in
         let (rejects,remains)=List.partition
         (fun (tgtt,_)->
           Alaskan_ingredients_for_ocaml_target.module_dependency_for_ocaml_target 
           (fst ts) [hm] tgtt
         ) others in
         let _=Image.image (
           fun (tgtt,_)->
           if Ocaml_target.has_dependencies tgtt
           then let s_ap=s_root^"_build/"^
                    (Ocaml_target.to_shortened_string tgtt) in
                let _=Unix_command.uc("rm -f "^s_ap) in
                ()
         ) ((tgt,is_an_ending_or_not)::rejects) in
         (successful_ones,remains,ts2,j+1,cidx2)
         ;; 
   
let rec  iterator_for_toplevel dir (successful_ones,to_be_treated,ts,j,cidx)=
    match to_be_treated with
    []->(List.rev successful_ones,ts,j)
    |_->iterator_for_toplevel dir (extended_pusher dir (successful_ones,to_be_treated,ts,j,cidx));;
                 

let hard_part_in_recompilation 
(dir,name,temp1,new_mdata,new_tgts,new_dirs,short_paths)=
let (successful_ones,ts2,k)=iterator_for_toplevel dir 
([],temp1,(new_mdata,new_tgts),0,4) in
(dir,name,new_dirs,short_paths,successful_ones,ts2,k);;





*)

(*

let (dir,name,temp1,new_mdata,new_tgts,new_dirs,short_paths)=prepare_recompilation ();;

let pusher=extended_pusher;;

let v1=pusher dir ([],temp1,(new_mdata,new_tgts),0,4);;

let (successful_ones,to_be_treated,ts,j,cidx)=v1;;

let (tgt,is_an_ending_or_not)::others=to_be_treated;;

let (bowl2,ts2,cidx2)=extended_unit_make dir (true,ts) tgt;;

let (mdata,tgts)=ts;;

let bad1=Alaskan_make_ocaml_target.is_up_to_date dir tgts tgt;;

let check1=Ocaml_target.test_target_existence dir tgt;;
let check2=List.mem tgt tgts;;

let new_successful_ones=(
           if is_an_ending_or_not=Is_an_ending_or_not.Yes
           then let hm=Option.unpack(Ocaml_target.main_module tgt) in
                (*
                  Note that the cmi and cmo give the same hm
                *)
                if List.mem hm successful_ones
                then successful_ones
                else hm::successful_ones
           else successful_ones
         );;

let v2=(new_successful_ones,others,ts2,j+1,cidx2);;


let (successful_ones,ts2,k)=iterator_for_toplevel dir v2;;

finish_compilation (dir,name,new_dirs,short_paths,successful_ones,ts2,k);;




*)

(*

German_recompile.on_monitored_modules;;

Ocaml_target.archive;;

let z1=Option.filter_and_unpack(
   function Ocaml_target.NO_DEPENDENCIES(mlx)->Some(mlx) |_->None
) tgts;;

let ml1= Mlx_ended_absolute_path.MLX (Ocaml_ending.Ml,
"Makefile_makers/mlx_ended_absolute_path",
Directory_name.D "/Users/Ewandelanoy/Documents/OCaml/Ordinary");;

Mlx_ended_absolute_path.to_string ml1;;

*)

(*
let z1=(!old_ones);;

let z2=Option.filter_and_unpack 
(fun (j,tgt)->match Ocaml_target.main_module tgt with
  None->None |Some(hm)->Some(j,hm)
) z1;;

let z3=image (fun (x,y)->
  Naked_module.to_string(Half_dressed_module.naked_module y)) z2;;

let z4=List.filter (
   fun (j,hm)->
   Naked_module.to_string(Half_dressed_module.naked_module hm)="option"
) z2;;
*)

(*



let ap1=Absolute_path.of_string
("/Users/ewandelanoy/Documents/OCaml/Ordinary/Php_analizer/HRecognizer/"^
"nonatomic_hrecognize.ml");;
open_file_for_writing_after_recompilation ap1;;

*)


(*

let prepare_recompilation ()=
   let current_state=Io.read_whole_file ap_for_current_text in
   if current_state=(!last_saved_state)
   then []
   else
   let ap=Absolute_path.of_string(!currently_modified_file) in
   let _=Io.overwrite_with ap current_state in
   let t1=Unix.time() in
   let tolerate_cycles=false 
   and (old_mdata,old_tgts)=(!German_wrapper.Private.data_ref,
   !German_wrapper.Private.up_to_date_targets_ref) in
   let ((new_mdata,hms_to_be_updated),short_paths)=
    German_recompile.on_monitored_modules tolerate_cycles old_mdata in
   let t2=Unix.time() in 
   if hms_to_be_updated=[] then [dd t1 t2] else
   let new_dirs=German_directories.from_data new_mdata 
   and new_tgts1=Ocaml_target.still_up_to_date_targets hms_to_be_updated old_tgts in
   let t3=Unix.time() in 
   let dir=German_constant.root in
   let checker=(fun tgt->
    let s=(Directory_name.connectable_to_subpath dir)^(Ocaml_target.to_string tgt) in 
      Sys.file_exists s ) in
   let new_tgts=List.filter checker new_tgts1 in
   let default_top=(German_data.default_toplevel new_mdata) in
   let t4=Unix.time() in 
   let opt2=Ocaml_target.toplevel_data default_top in
   let t5=Unix.time() in
   if opt2=None then failwith("This is Zorglub") else
   let (name,l)=Option.unpack opt2 in
   let temp1=ingr_for_top new_mdata name l in
   let t6=Unix.time() in
   let (successful_ones,ts2)=Alaskan_make_ocaml_target.iterator_for_toplevel dir 
        ([],temp1,(new_mdata,new_tgts)) in
   let t7=Unix.time() in   
   let new_toplevel=Ocaml_target.toplevel name successful_ones in
   let t8=Unix.time() in 
   let (_,(new_mdata,new_tgts))=
    Alaskan_make_ocaml_target.unit_make dir (true,ts2) new_toplevel in
   let t9=Unix.time() in
   let changes=German_changed.update short_paths 
      (!(German_wrapper.Private.recently_changed_ref)) in
   let _=(
         German_wrapper.Private.data_ref:=new_mdata;
         German_wrapper.Private.directories_ref:=new_dirs;
         German_wrapper.Private.up_to_date_targets_ref:=new_tgts;
         German_wrapper.Private.recently_changed_ref:=changes;
   ) in
   let t10=Unix.time() in  
   let _=German_wrapper.Private.save_all() in
   let t11=Unix.time() in
   [dd t1 t2;dd t2 t3;dd t3 t4;dd t4 t5;dd t5 t6;dd t6 t7;dd t7 t8;
    dd t8 t9;dd t9 t10;dd t10 t11];;


*)

(*

let save_and_recompile ()=
   let current_state=Io.read_whole_file ap_for_current_text in
   if current_state=(!last_saved_state)
   then []
   else
   let ap=Absolute_path.of_string(!currently_modified_file) in
   let _=Io.overwrite_with ap current_state in

*)

(*
let ap_for_current_text=
  Absolute_path.of_string 
  (
  "/Users/ewandelanoy/Documents/OCaml/Ordinary/Pasteboard_related/current_text.ml"
  );;

let currently_modified_file=ref "";;

let last_saved_state=ref "";;

let open_file_for_writing_after_recompilation ap=
  let s_ap=Absolute_path.to_string ap in
  if (!currently_modified_file=s_ap)
  then ()
  else 
  let text=Io.read_whole_file ap in
  (currently_modified_file:=s_ap;
   last_saved_state:=text;
   Io.overwrite_with ap_for_current_text text);;

let dd t1 t2=Chronometer.rewrite_float (t2-.t1);;
*)

(*
let z1=German_pervasives.abo "hrecognize";;
let z2=German_wrapper.data();;



let z3=Image.image (fun hm->
   (hm,List.length (German_data.below z2 hm))
) z1;;
*)


(*
let optional_mds=
  List.filter Modulesystem_data.is_optional
  (German_wrapper.data()) ;;
let optional_hms=Image.image
  Modulesystem_data.name optional_mds;;
let optional_nms=List.rev_map (fun hm->
let (Naked_module.N s)=Half_dressed_module.naked_module(hm) 
in s) optional_hms;;

let anse1=Explicit.image German_pervasives.fg
["caratheodory"; "large_int"; "matrix"; "minimal_transversal";
   "ordered_list_creator"; "periodicity"; "permutation"; "quadratic_form";
   "rational_creator"; "simplex_method"; "theodoracopulos";
   "nontranslatable_interdiction"; "pretty_print_intervals_and_felines";
   "tidel3"; "van_der_waerden_computer"; "logical_subdivision";
   "van_der_waerden_explanation"; "van_der_waerden_greedy_computer";
   "translatable_interdiction"; "van_der_waerden_simplex"; "constraint";
   "van_der_waerden_string"; "van_der_waerden_value"; "window_boundary"];;
*)


(*
let dir1=Directory_name.of_string
"Remembered/Self_contained/Simplex_method_and_others";;

let optional_mds=
    List.filter Modulesystem_data.is_optional
    (German_wrapper.data()) ;;
let optional_hms=Image.image
    Modulesystem_data.name optional_mds;;
let optional_nms=Image.image (fun hm->
  let (Naked_module.N s)=Half_dressed_module.naked_module(hm) 
  in s) optional_hms;;

let z1=German_make_self_contained_copy.mscc dir1 optional_nms;;
*)


(*
let mdata=German_wrapper.data();;
let z1=Alaskan_data.compute_subdirectories_list mdata;;
let z2=German_data.local_directories mdata;;

let gg1 md=let hm=Modulesystem_data.name md in
Half_dressed_module.full_directory hm;;
let gg2 md=try (fun _->false)(gg1 md) with _->true;;
let u1=List.filter gg2 mdata;;

let compute_directories_list mdata=
  let temp1=Image.image (
      fun md->
       let hm=Modulesystem_data.name md in
       Half_dressed_module.full_directory hm
  ) mdata in
  let temp2=Tidel.diforchan temp1 in
  Tidel.forget_order temp2;;


let see1=Half_dressed_module.naked_module;;
let see2=Directory_name.of_string;;
*)

(*
let z2=Alaskan_create_target_system.from_main_directory dir1 None [];;

let see1=German_pervasives.ucc;;
let see2=Alaskan_command_for_ocaml_target.cee;;
let see3=German_wrapper.save_all;;
let see4=Alaskan_data.all_mlx_files;;
let see5=Alaskan_up_to_date_targets.loadings;;
*)

(*
let z1=German_wrapper.data();;
let z2=Image.image(
   fun z->let zn=Modulesystem_data.name z in
   let (Subdirectory.SD temp1)=Half_dressed_module.subdirectory zn
   and (Naked_module.N temp2)=Half_dressed_module.naked_module zn in
   (temp1,temp2,(fun ()->z))
) z1;;
let z3=List.filter (
   fun (s,t,zf)->
     Substring.begins_with s "Country/Alaska"
) z2;;
let unordered_z4=List.flatten (Image.image (
   fun (s,t,zf)->Modulesystem_data.all_ancestors (zf())
) z3 );;
let z4=Tidel.diforchan unordered_z4;;
let z5=List.filter(
   fun (s,t,zf)->
     let hm=Modulesystem_data.name (zf()) in
     Tidel.elfenn hm z4
) z2;;
let z6=List.flatten (Image.image (
   fun (s,t,zf)->Modulesystem_data.acolytes (zf())
 ) z5);;
let z7=Image.image (

) ;;



let ucc=German_pervasives.ucc;;
*)


(*
let fg=German_pervasives.fg;;

let z9=[ "visualization_tools" ;  "php_projected_token_set"; "php_projected_token";
   "php_blocker"; "php_constant_token"; "php_keyword"; "php_operator";
   "php_punctuator"];;

let work1=(Explicit.image fg) z9;;
*)

(*

let z1=German_wrapper.data();;
let z2=Image.image(
   fun z->let zn=Modulesystem_data.name z in
   let (Subdirectory.SD temp1)=Half_dressed_module.subdirectory zn
   and (Naked_module.N temp2)=Half_dressed_module.naked_module zn in
   (temp1,temp2,(fun ()->z))
) z1;;
let z3=List.filter (
   fun (s,t,zf)->
     Substring.begins_with s "Php_analizer"
) z2;;
let (z4,z5)=List.partition (
  fun (s,t,zf)->s="Php_analizer/Great_Replacement"
) z3;;
let (z6,z7)=List.partition (
  fun (s,t,zf)->s="Php_analizer/HRecognizer"
) z5;;
let needed_ones=z4@z6;;

let u1=image (fun (s,t,zf)->let l_hm=abo t in
  Image.image(fun hm->
  let (Naked_module.N temp)=Half_dressed_module.naked_module hm in
  temp) l_hm
) z7;;
let u2=Ordered_string.diforchan(List.flatten u1);;

let involved_ones=List.filter (
  fun (s,t,zf)->Ordered_string.elfenn t u2
) z2;;
let involved_dirs=
  Ordered_string.diforchan(image (fun (s,t,zf)->s) involved_ones);;

let involved_files1=List.flatten(image (fun
   (s,t,zf)->image (fun g->(s,g)) (Modulesystem_data.acolytes (zf()))
) involved_ones);;
let involved_files2=image
   (fun (s,p)->(s,Mlx_ended_absolute_path.to_absolute_path p)) 
   involved_files1;;
let command_for_pair (s,ap)=
   let s_ap=Absolute_path.to_string ap in
   "cp "^s_ap^" "^
   "/Users/Ewandelanoy/Documents/OCaml/Ordinary/"^
   "Remembered//Attempts_at_Php_Handling/Fifth_Attempt_at_Php_handling/"^
   s^"/";;
let involved_cmds=image command_for_pair involved_files2;;
let anse1=image Unix_command.hardcore_verbose_uc involved_cmds;;

let z8=List.rev_map (fun (s,t,zf)->t) z7;;
*)




(*
let test (k,s)=
  let n=String.length s in 
  let (j,_)=Hrecognize.main_exhauster s 1 in
  if j>n
  then None
  else let m=min(n)(j+500) in
       let t=Cull_string.interval s j m in
       Some(k,j,t);;

let chan=open_in "Remembered/Marshaled_data/data1.mshl";;
let u2=((Marshal.from_channel chan):(string list) ) ;;
close_in chan;;

let u3=Ennig.index_everything u2;;
let u4=Chronometer.it (Option.filter_and_unpack test) u3;;
       
       
let (k1,j1,t1)=List.hd u4;;
*)


(*
let s1="realpath($resource) ?: (file_exists($resource) ? $resource : false)";;
let bad1=Nonatomic_hrecognize.recgz (rn "rtripod2") s1 1;;
let see1=Nonatomic_hrecognize.debug (rn "rtripod2") s1 1;;
*)

(*

let example=More_coherent_pdf.chunk
 ~rootdir:"/Users/ewandelanoy/Documents/html_files/Printable"
 ~pdfname:"agreda"
 ~interval:(241,260)
 ~chunksize:None;;

 *)



(*
let s1="$fhandle = fopen('stone.txt','a');";;
let bad1=Nonatomic_hrecognize.recgz (rn "semicoloned_assignment") s1 1;;
let see1=Nonatomic_hrecognize.debug (rn "semicoloned_assignment") s1 1;;
*)

(*
let u5=List.filter (
   fun ((_,_,_,_,lbl),_)->lbl=Hrecognize.label_for_semicoloned_assignment
) u1;;
let u6=image (fun (_,(i,j))->
    Cull_string.interval text1 i j
) u5;;
let u7=image(fun t->
  let j=Substring.leftmost_index_of_in "=" t in
  Cull_string.trim_spaces(itv t 1 (j-1))
) u6;;
let u8=Option.filter_and_unpack(
   fun t->
    let opt=Nonatomic_hrecognize.recgz (rn "php_vname") t 1 in
    let j=Option.unpack opt and n=String.length t in
    if j>n then None else Some(itv t j n)    
) u7;;
*)

(*

let s_ap="/Users/ewandelanoy/Documents/OCaml/Ordinary/Test_directory6/"^
"Test_directory7/Test_directory2/please_test_me.ml";;
let ap=Absolute_path.of_string s_ap;;

let s1=Io.read_whole_file ap;;
let s2=Replace_inside.replace_inside_string ("\r","") s1;;
let s3=s2^"blue\n\n";;

Io.overwrite_with ap s3;;


*)

(*
let s_ap="~/Documents/Sites/Rachel/public_html/iewtopic.php";;
let ap=Absolute_path.of_string s_ap;;
let text1=Io.read_whole_file ap;;    
let u1=Chronometer.it Hpacify_classes.pc text1;;

let fnctn_related_labels=
  [
    Hrecognize.label_for_fnctn;
    Hrecognize.label_for_qualified_fnctn;
  ];;

let u2=Option.filter_and_unpack (fun ((_,_,_,_,lbl),(i,j))->
  if not (List.mem lbl fnctn_related_labels) then None else
  let k=Substring.leftmost_index_of_in_from "{" text1 i in
  Some(Cull_string.interval text1 (k+1) (j-1))
) u1;;

let test (k,s)=
  let n=String.length s in 
  let (j,_)=Hrecognize.main_exhauster s 1 in
  if j>n
  then None
  else let m=min(n)(j+500) in
       let t=Cull_string.interval s j m in
       Some(k,j,t);;

let u3=Ennig.index_everything u2;;
let u4=Chronometer.it (Option.filter_and_unpack test) u3;;
       
       
let (k1,j1,t1)=List.hd u4;;
*)

(*
let chang=open_out  "Remembered/Marshaled_data/data1.mshl";;
(Marshal.to_channel chang u2 [];close_out chang);;
*)

(*

let label_for_one_liner_with_variable="one_liner_with_variable";;
add_label label_for_one_liner_with_variable;;

let one_liner_with_variable_recognizer=rlabch
  label_for_one_liner_with_variable
  [
     dollar;
     naive_php_name;
     sto "" ['\n';'\r';';'];
     semicolon
  ];;

add_recognizer (label_for_one_liner_with_variable,one_liner_with_variable_recognizer);; 

*)


(*

let chang=open_out  "Marshaled_files/data1.mshl";;
(Marshal.to_channel chang u2 [];close_out chang);;

let chan=open_in "Marshaled_files/data1.mshl";;
let u2_again=((Marshal.from_channel chan):(string list) ) ;;
close_in chan;;

*)

(*

let s_ap="~/Documents/Sites/Rachel/public_html/iewtopic.php";;
let ap=Absolute_path.of_string s_ap;;
let text1=Io.read_whole_file ap;;    
let u1=Chronometer.it Hpacify_classes.pc text1;;

let g1=image snd u1;;
let g2=Listennou.universal_delta_list g1;;
let check=List.filter (fun ((a,b),(x,y))->x<>b+1) g2;;



*)

(*

let s_ap="~/Documents/Sites/Rachel/public_html/iewtopic.php";;
let ap=Absolute_path.of_string s_ap;;
let text1=Io.read_whole_file ap;;    
let u1=Hpacify_namespaces.pn text1;;

let class_related_labels=
  [
    Hrecognize.label_for_abstract_glass;
    Hrecognize.label_for_glass;
    Hrecognize.label_for_final_glass;
    Hrecognize.label_for_itrfc
  ];;

let u2=Option.filter_and_unpack (fun ((_,lbl),(i,j))->
  if not (List.mem lbl class_related_labels) then None else
  let k=Substring.leftmost_index_of_in_from "{" text1 i in
  Some(Cull_string.interval text1 i (k-1))
) u1;;
let u3=List.filter (
  fun x->List.for_all(fun y->
      Substring.is_a_substring_of y x 
   ) ["extends";"implements"]
) u2;;  

let u4=List.filter (
  fun x->Substring.begins_with x "interface"
) u2;;  

*)

(*
let test (k,s)=
  let n=String.length s in 
  let (j,_)=Hrecognize.main_exhauster s 1 in
  if j>n
  then None
  else let m=min(n)(j+500) in
       let t=Cull_string.interval s j m in
       Some(k,j,t);;

let s_ap="~/Documents/Sites/Rachel/public_html/iewtopic.php";;
let ap=Absolute_path.of_string s_ap;;
let text1=Io.read_whole_file ap;;    
let u1=Hpacify_namespaces.pn text1;;

let class_related_labels=
  [
    Hrecognize.label_for_abstract_glass;
    Hrecognize.label_for_glass;
    Hrecognize.label_for_final_glass;
    Hrecognize.label_for_itrfc
  ];;

let u2=Option.filter_and_unpack (fun ((_,lbl),(i,j))->
   if not (List.mem lbl class_related_labels) then None else
   let k=Substring.leftmost_index_of_in_from "{" text1 i in
   Some(Cull_string.interval text1 (k+1) (j-1))
) u1;;
let u3=Ennig.index_everything u2;;
let u4=Option.filter_and_unpack test u3;;


let (k1,j1,t1)=List.hd u4;;
*)

(*

let class_related_labels=
  [
    Hrecognize.label_for_one_liner_with_variable
  ];;

let u2=Option.filter_and_unpack (fun ((_,lbl),(i,j))->
   if not (List.mem lbl class_related_labels) then None else
   let k=Substring.leftmost_index_of_in_from "{" text1 i in
   Some(Cull_string.interval text1 (k+1) (j-1))
) u1;;
let u3=Ennig.index_everything u2;;

*)


(*
let s1="protected $loaders = [];";;
let r1=rn "qualified_declaration";;
let bad1=Nonatomic_hrecognize.recgz r1 s1 1;;
let see1=Nonatomic_hrecognize.debug r1 s1 1;;
let bad2=Nonatomic_hrecognize.recgz (rn "assignable") "[]" 1;;
*)


(*
let g1=List.nth u2 406;;
let (j2,g2)=Hrecognize.main_exhauster g1 1;;
let g3=List.rev g2;;

Cull_string.interval g1 3987 4149;;
Substring.leftmost_linedex_of_in 
"public function set_name($name)\n\t{\n\t\t$this->name = $name;\n\t}"  text1;;
*)

(*
let z1="$container_builder\n->without_extensions()\n->with_config($wc->config_php_file)\n->with_config_path($wc->get_config_path())\n->with_environment('production')\n->without_compiled_container()\n->get_container()";;
let bad1=Nonatomic_hrecognize.recgz (rn "assignable") z1 1;;
*)

(*

let s_ap="~/Documents/Sites/Rachel/public_html/iewtopic.php";;
let ap=Absolute_path.of_string s_ap;;
let text1=Io.read_whole_file ap;;    
let u1=Hpacify_namespaces.pn text1;;

let u2=Option.filter_and_unpack (
  fun ((x,y),(i,j))->if y=
    Hrecognize.label_for_one_liner_with_variable
    then Some(Cull_string.interval text1 i j)
    else None
) u1;;

*)

(*
let s1="(empty($can_receive_pm_list) || !isset($can_receive_pm_list[0]['u_readpm'])) ? array() : $can_receive_pm_list[0]['u_readpm']";;
let bad1=Nonatomic_hrecognize.recgz Hrecognize.assignable s1 1;;
let bad2=Nonatomic_hrecognize.recgz (rn "tripod2") s1 1;;
let see1=Nonatomic_hrecognize.debug (rn "tripod2") s1 1;;
*)

(*
let s1="(int) $db->sql_fetchfield('forum_id')";;
let bad1=Nonatomic_hrecognize.recgz Hrecognize.assignable s1 1;;
let bad2=Nonatomic_hrecognize.debug Hrecognize.assignable s1 1;;
*)



(*
let s1="$sql = 'SELECT *\nFROM ' . STYLES_TABLE . \" s\nWHERE s.style_id = $style_id\";";;
let bad1=Nonatomic_hrecognize.recgz Hrecognize.semicoloned_assignment s1 1;;

let bad2=Nonatomic_hrecognize.debug Hrecognize.semicoloned_assignment s1 1;;
*)




(*
let s1="'SELECT *\nFROM ' . STYLES_TABLE . \" s\nWHERE s.style_id = $style_id\"";;
let bad1=Nonatomic_hrecognize.recgz Hrecognize.assignable s1 1;;
*)


(*
let s1="$container_builder\n->without_extensions()\n->with_config($wc->config_php_file)\n->with_config_path($wc->get_config_path())\n->with_environment('production')\n->without_compiled_container()\n->get_container()\n;\n\n$ext_container->register('cache.driver', '\\\\phpbb\\\\cache\\\\driver\\\\dummy')";;

let bad1=Nonatomic_hrecognize.recgz Hrecognize.assignable "'Nina'" 1;; 
*)

(*
let bad1=Nonatomic_hrecognize.recgz Hrecognize.echoable 
("(isset($context[\"S_CONTENT_DIRECTION\"]) ? "^
"$context[\"S_CONTENT_DIRECTION\"] : null)")  1;;
*)

(*
let bad1=Nonatomic_hrecognize.recgz Hrecognize.assignable 
"false"  1;;


*)

(*
let debug_chain old_f l s i=
  let rec tempf=(fun (idx,da_ober)->
   match da_ober with
   []->raise(Debug_chain_exn)
   |atm::peurrest->
     (
       match recgz atm s idx with
        None->old_f atm s idx 
       |Some(new_idx)->tempf(new_idx,peurrest)
     )
  ) in
  tempf (i,l);;

let rec debug natm s i=
 match natm with
 Nonatomic_hrecognizer.Leaf(_,_)
 |Nonatomic_hrecognizer.Ordered_disjunction(_,_)->(natm,i)  
 |Nonatomic_hrecognizer.Chain(_,l)->debug_chain debug l s i      
 |Nonatomic_hrecognizer.Star(_,natm2)
 |Nonatomic_hrecognizer.Maybe(_,natm2)
 |Nonatomic_hrecognizer.Check(_,pair)->
     raise(Debug_star_exn);;



let s1="$yloader     = new \\Symfony\\Component\\DependencyInjection\\Loader\\YamlFileLoader($wc->container,\n// Might be ProxyManager\\FileLocator\\FileLocator\nnew \\Symfony\\Component\\Config\\FileLocator($filesystem->realpath($wc->get_config_path())));";;
let bad1=Hrecognize.parse_all s1;;
let bad2=Nonatomic_hrecognize.recgz Hrecognize.new_fnctn_call s1 1;;
let see1=Nonatomic_hrecognize.extra_debug Hrecognize.new_fnctn_call s1 1;;

let s2="new \\Symfony\\Component\\DependencyInjection\\Loader\\YamlFileLoader($wc->container,\n// Might be ProxyManager\\FileLocator\\FileLocator\nnew \\Symfony\\Component\\Config\\FileLocator($filesystem->realpath($wc->get_config_path())));";;
let good1=Nonatomic_hrecognize.recgz Hrecognize.new_fnctn_call s2 1;;
let good2=Nonatomic_hrecognize.recgz Hrecognize.assignable s2 1;;
let see2=Nonatomic_hrecognize.extra_debug Hrecognize.assignable s2 1;;
let bad3=Nonatomic_hrecognize.recgz Hrecognize.myriam "new" 1;;
let see3=Nonatomic_hrecognize.extra_debug Hrecognize.myriam "new" 1;;
let bad4=Nonatomic_hrecognize.recgz Hrecognize.myriam_element "new" 1;;
let see4=Nonatomic_hrecognize.debug Hrecognize.myriam_element "new" 1;;


let test (k,s)=
  let n=String.length s in 
  let (j,_)=Hrecognize.main_exhauster s 1 in
  if j>n
  then None
  else let m=min(n)(j+500) in
       let t=Cull_string.interval s j m in
       Some(k,j,t);;

let s_ap="~/Documents/Sites/Rachel/public_html/iewtopic.php";;
let ap=Absolute_path.of_string s_ap;;
let text1=Io.read_whole_file ap;;    
let u1=Hpacify_namespaces.pn text1;;

let u2=Option.filter_and_unpack (
  fun ((x,y),(i,j))->if y=
    Hrecognize.label_for_one_liner_with_variable
    then Some(Cull_string.interval text1 i j)
    else None
) u1;;
*)


(*
let u3=List.filter (
  fun ((x,y),(i,j))->List.mem y
  [
    Hrecognize.label_for_assign_to_array;
    Hrecognize.label_for_assign_to_new_fnctn_call ;
    Hrecognize.label_for_assign_to_myriam ;
    Hrecognize.label_for_assign_to_fnctn_call ; 
  ]
) u1;;

let w1=itv text1 818 860;;
let bad1=Hrecognize.semicoloned_assignment_recognizer w1 1;;
let bad2=Nonatomic_hrecognize.recgz Hrecognize.semicoloned_assignment w1 1;;
let see1=Nonatomic_hrecognize.extra_debug Hrecognize.semicoloned_assignment w1 1;;
let w2=Nonatomic_hrecognize.recgz Hrecognize.assignable;;
*)

(*
let test (k,s)=
  let n=String.length s in 
  let (j,_)=Hrecognize.main_exhauster s 1 in
  if j>n
  then None
  else let m=min(n)(j+500) in
       let t=Cull_string.interval s j m in
       Some(k,j,t);;

let s_ap="~/Documents/Sites/Rachel/public_html/iewtopic.php";;
let ap=Absolute_path.of_string s_ap;;
let text1=Io.read_whole_file ap;;    
let u1=Hpacify_namespaces.pn text1;;

let class_related_labels=
  [
    Hrecognize.label_for_abstract_glass;
    Hrecognize.label_for_glass;
    Hrecognize.label_for_final_glass;
  ];;

let u2=Option.filter_and_unpack (fun ((_,lbl),(i,j))->
   if not (List.mem lbl class_related_labels) then None else
   let k=Substring.leftmost_index_of_in_from "{" text1 i in
   Some(Cull_string.interval text1 (k+1) (j-1))
) u1;;
let u3=Ennig.index_everything u2;;
let u4=Option.filter_and_unpack test u3;;


let (k1,j1,t1)=List.hd u4;;
*)

(*
let test (k,s)=
   let n=String.length s in 
   let (j,_)=Hrecognize.main_exhauster s 1 in
   if j>n
   then None
   else let m=min(n)(j+500) in
        let t=Cull_string.interval s j m in
        Some(k,j,t);;

let s_ap="~/Documents/Sites/Rachel/public_html/iewtopic.php";;
let ap=Absolute_path.of_string s_ap;;
let text1=Io.read_whole_file ap;;    
let (_,u1)=Hrecognize.main_exhauster text1 1;;
let u2=Option.filter_and_unpack (fun (lbl,(i,j))->
   if lbl<>"braced_nspc" then None else
   let k=Substring.leftmost_index_of_in_from "{" text1 i in
   Some(Cull_string.interval text1 (k+1) (j-1))
) u1;;
let u3=Ennig.index_everything u2;;
let u4=Option.filter_and_unpack test u3;;
let (k1,j1,t1)=List.hd u4;;

*)

(*

"echo \"\\\">\n\\t\\t<div id=\\\"darken\\\" class=\\\"darken\\\">&nbsp;"

let t2=List.nth u2 (k1-1);; 
let li1=Substring.leftmost_linedex_of_in t1 text1;;

let t3=Cull_string.interval t2 j1 (j1+1000);;
let (_,l1)=Hrecognize.main_exhauster t2 1;;



*)

(*
let s_ap="/Users/ewandelanoy/Documents/Portfolio/"^
"Html_template/en/index.html";;
let ap=Absolute_path.of_string s_ap;;
let s1=Io.read_whole_file ap;;
let tag1=Html_parse_text_with_tags.parse s1;;
let tag2=Html_print_text_with_tags.print tag1;;

let ap2=Absolute_path.create_file ("/Users/ewandelanoy/Documents/Portfolio/"^
"Html_template/en/index2.html");;

Io.overwrite_with ap2 tag2;;
*)


(*
open Hrecognize;;

let s1="$sql = 'SELECT p.post_id\n\tFROM ' . POSTS_TABLE . ' p' . "^
"(($join_user_sql[$sort_key]) ? ', ' . USERS_TABLE . ' u': '') . "^
"\"\n\tWHERE p.topic_id = $topic_id\n\t\tAND \" . $phpbb_content_visibility->get_visibility_sql('post', $forum_id, 'p.') . \"\n\t\t\" . (($join_user_sql[$sort_key]) ? 'AND u.user_id = p.poster_id': '') . \"\n\t\t$limit_posts_time\n\tORDER BY $sql_sort_order\";";;

let bad1=assign_to_myriam_recognizer s1 1;;
let bad2=Nonatomic_hrecognize.recgz assign_to_myriam s1 1;;
let see1=Nonatomic_hrecognize.debug assign_to_myriam s1 1;;
let see2=Nonatomic_hrecognize.extra_debug assign_to_myriam s1 1;;
let see3=Nonatomic_hrecognize.extra_debug myriam s1 8;;

let (stm,_,_)=List.nth see3 1;;
let bad3=Nonatomic_hrecognize.recgz stm s1 33;;
let bad4=Nonatomic_hrecognize.recgz myriam_snippet s1 33;;

let see4=Nonatomic_hrecognize.extra_debug myriam_snippet s1 33;;

let bad5=Nonatomic_hrecognize.recgz myriam_element "POSTS_TABLE" 1;;

let see5=Nonatomic_hrecognize.extra_debug myriam_element s1 33;;


let s_ap="/Users/ewandelanoy/Documents/Burev/Downloads/"^
"webmic/Html_template/en/index.html";;
let ap=Absolute_path.of_string s_ap;;
let s1=Io.read_whole_file ap;;
let tag1=Html_parse_text_with_tags.parse s1;;
let tag2=Html_print_text_with_tags.print tag1;;

*)

(*
let u1=ennig 1 10;;
let u2=doyle (fun _->u1) 1 5;; 
let u3=Cartesian.general_product u2;;

let test l=
  let tf=List.nth l in
  (513*tf(0)-388*tf(1)+3942*tf(2)-1476*tf(3)+11*tf(4)=0);; 

let u4=List.filter test u3;;

let z1=abo "html_parse_text_with_tags";;
let z2=image Half_dressed_module.naked_module z1;;
*)


(*

let s_ap="/Users/ewandelanoy/Documents/Burev/Downloads/"^
"webmic/Html_template/en/index.html";;
let ap=Absolute_path.of_string s_ap;;
let s1=Io.read_whole_file ap;;
let tag1=Html_parse_text_with_tags.parse s1;;



let s1="<!DOCTYPE html>\n<html>\n <head>\n <title>Home / HTML Template</title>\n            <link rel=\"stylesheet\" href=\"../style.css\">\n        </head> \n      </html> ";;

let bad1=Html_parse_text_with_tags.parse s1;;

let v0=Html_parse_text_with_tags.main_initializer s1;;
let ff=Html_parse_text_with_tags.main_pusher  
     (s1,String.length s1);;
let gg=Memoized.small ff v0;;

let v1=gg 12;;
let (Html_hedgehog_pack.P w1)=fst(snd v1);;
let u1=Html_hedgehog.finished_part (List.hd w1);;


let (s,n)=(s1,String.length s1);;
let (ndr,walker)=v1;;
let (graet,idx)=walker;;
let opt1=Option.seek (fun j->
      (Strung.get s j)='<'
)(Ennig.ennig idx n);;
let i1=Option.unpack opt1;;
let opt2=Option.seek (fun j->
      (Strung.get s j)='>'
)(Ennig.ennig (i1+1) n);;
let i2=Option.unpack opt2;;
let temp1=(
    if i1>idx
    then Html_hedgehog_pack.add_string_constant 
          (idx,i1-1,Cull_string.interval s idx (i1-1)) graet
    else graet      
);;
let bad0=Html_hedgehog_pack.add_tag 
  (i1,i2,Cull_string.interval s i1 i2) temp1;;  

let bad1=Html_hedgehog_pack.add_tag 
  (17,22,"<html>") temp1;;

let (i,j,tag_descr)=(17,22,"<html>") and hpack=temp1;;
let t=Cull_string.interval tag_descr 2 (j-i);;
let see1=Html_hedgehog_pack.is_not_a_closable_tag t;;
         
let bad2=Html_hedgehog_pack.add_closing_tag 
         (i,j,Cull_string.interval tag_descr 2 (j-i)) hpack;;

let bad3=Html_hedgehog_pack.add_closing_tag 
         (i,j,"html") hpack;;

let tag_name="html";;      
let (Html_hedgehog_pack.P l_hedgehog)=hpack;;   
let hedgehog1::peurrest=l_hedgehog;;


         let add_closing_tag ((i:int),j,tag_name) (P l_hedgehog)=
          match l_hedgehog with
           []->raise(Unmatched_closing_tag(tag_name))
          |hedgehog1::peurrest->
          (
            match Html_hedgehog.unfinished_part hedgehog1 with
            []->raise(Cold_reception(tag_name))
            |(i2,j2,tag_name2)::peurrest2->
               if tag_name2<>tag_name 
               then raise(Tag_mismatch(tag_name,tag_name2))
               else 
              let new_hedgehog1=
                   Html_hedgehog.close_latest_tag (i,j) hedgehog1 in
              if Html_hedgehog.unfinished_part new_hedgehog1=[]
              then  add_finished_hedgehog new_hedgehog1 peurrest
              else P(new_hedgehog1::peurrest)
          );;        
         

  let add_tag (i,j,tag_descr) hpack=
    if Substring.is_a_substring_located_at "/" tag_descr 2
    then add_opening_tag 
         (i,j,Cull_string.interval tag_descr 3 (j-i)) hpack
    else let t=Cull_string.interval tag_descr 2 (j-i) in
         if is_not_a_closable_tag t
         then add_string_constant 
             (i,j,tag_descr) hpack
         else 
         add_closing_tag 
         (i,j,Cull_string.interval tag_descr 2 (j-i)) hpack ;;  

*)

(*

let s_ap="/Users/ewandelanoy/Documents/Sites/Mikeal/special.sql";;
let ap=Absolute_path.of_string s_ap;;
let s1=Io.read_whole_file ap;;

let u1=Substring.occurrences_of_in "miad" s1;;

Replace_inside.replace_inside_file
("tribunemicael.net","larchange.org") ap;;



*)


(*

let s_ap="/Users/ewandelanoy/Documents/Burev/untitled_text2.txt";;
let ap=Absolute_path.of_string s_ap;;
let s1=Io.read_whole_file ap;;

let s2=Replace_inside.replace_inside_string ("\r","") s1;;
let n2=String.length s2;;

let u1=doyle (fun j->(j,Strung.get s2 j)) 1 n2;;
let u2=List.filter(
   fun (j,c)->
     if (c<>'\n')||(j=1)||(j=n2)
     then true
     else ((Strung.get s2 (j-1))='\n')
          ||
          ((Strung.get s2 (j+1))='\n')
) u1;;
let u3=image (fun (x,y)->String.make 1 y) u2;;
let s3=String.concat "" u3;;

let u4=Lines_in_string.core s3;;
let u5=List.filter (
   fun (j,t)->
    (not (Substring.is_a_substring_of "THE WISH TO BELIEVE." t))
) u4;;
let s4=String.concat "" (image snd u5);;
Io.overwrite_with ap1 s4;;






let s_ap1="/Users/ewandelanoy/Documents/Burev/untitled_text.txt";;
let ap1=Absolute_path.of_string s_ap1;;
Io.overwrite_with ap1 s3;;



*)



(*

let term1=Html_parse_text_with_tags.parse s1;;

let term2=try (fun _->[])(Html_parse_text_with_tags.parse s1) with
Html_hedgehog_pack.Cannot_simplify_multipack(l)->l;;
let term3=List.rev term2;;
let term4=List.hd(term3);;

*)


(*

let s_ap="/Users/ewandelanoy/Documents/html_files/Translations/ws_translated.txt";;
let ap=Absolute_path.of_string s_ap;;
let s1=Io.read_whole_file ap;;
let s2=Replace_inside.replace_inside_string ("\r","") s1;;

let pass="fini. \nTout ";;
let i1=Substring.leftmost_index_of_in pass s2;;
let s3=Cull_string.beginning (i1-1) s2;; 
let s4=Cull_string.cobeginning (i1-1) s2;; 
let better_s4=Str.global_replace (Str.regexp_string"\n") "" s4;;
let better_s2=s3^better_s4;;
Io.overwrite_with ap better_s2;;

*)


(*

let n1=10;;

let part1=doyle (fun t->(t,0)) 1 n1;;

let part2=Cartesian.product (ennig (-n1) n1) (ennig 1 n1);;

let unordered_whole=part1@part2;;

let pre_oord (x1,y1) (x2,y2)=
    let m1=min(abs x1)(abs y1) and mm1=max(abs x1)(abs y1)
    and m2=min(abs x2)(abs y2) and mm2=max(abs x2)(abs y2) in
    let t1=Total_ordering.standard mm1 mm2 in
    if t1<>Total_ordering.Equal then t1 else
    let t2=Total_ordering.standard m1 m2 in
    if t2<>Total_ordering.Equal then t2 else
    let t3=Total_ordering.standard x1 x2 in
    if t3<>Total_ordering.Equal then t3 else
    Total_ordering.standard y1 y2;;

let oord=(pre_oord: (int*int) Total_ordering.t);;

let ordered_whole=Ordered.diforchan oord unordered_whole;;

*)

(*

let s_ap="/Users/ewandelanoy/Documents/Burev/Downloads/webmic/Html_template/en/index.html";;
let ap=Absolute_path.of_string s_ap;;
let s1=Io.read_whole_file ap;;

let term1=Html_parse_text_with_tags.parse s1;;

let term2=try (fun _->[])(Html_parse_text_with_tags.parse s1) with
Html_hedgehog_pack.Cannot_simplify_multipack(l)->l;;
let term3=List.hd(List.rev term2);;
let u1=am();;
let u2=List.filter (
  fun t->Substring.begins_with t "html"
) u1;;

*)


(*
let s_ap="/Users/ewandelanoy/Documents/Arno/instr.pages";;
let ap=Absolute_path.of_string s_ap;;
let s1=Io.read_whole_file ap;;
let n1=String.length s1;;
let s2=Io.read_whole_file ap;;
let n2=String.length s2;;



let s="while(abc){def}hum";;
let i=1;;
let bad1= wiley_recognizer s i;;
*)

(*

let s="$abc->def(u,v)->ghi(7,8);hum";;
  
let s="$abc->def (u,v)->ghi (7,8) ;hum";;

let bad1=snake_recognizer s 1;;

let (_,_,t1)=(Option.unpack bad1);;
itv s t1 (t1+2);;

let i=1;;
let check1=(Substring.is_a_substring_located_at "$" s i);;
let opt2=After.after_php_label  s (i+1);;
let i2=Option.unpack opt2;;
let opt3=After.after_whites s i2;;
let i3=Option.unpack opt3;;
let bad2=snake_iterator_partial_recognizer ([i;i2],s,i3);;

let (graet,i)=([i;i2],i3);;

let bad2=snake_pusher_partial_recognizer s i;;


let check2=(Substring.is_a_substring_located_at "->" s i);;
let opt2=After.after_whites s (i+2);;
let i2=Option.unpack opt2;;
let opt3=After.after_php_label  s i2;;
let i3=Option.unpack opt3;;
let check3=(Substring.is_a_substring_located_at "(" s i3);;


  then None
  else 
  let i4=After.after_closing_character ('(',')') s (i3+1,1) in
  let opt5=After.after_whites s i4 in
  if opt5=None then None else   
  let i5=Option.unpack opt5 in
  Some([i;i+2;i3;i4],i5);;

More_coherent_pdf.chunk;;

*)

(*

let sd1=Subdirectory.of_string 
"Prepare_html/Tag_related";;
  
relo "html_hedgehog" sd1;;
relo "html_hedgehog_pack" sd1;;
relo "html_text_with_tags" sd1;;

*)


(*
German_backup_target_system.github_after_backup:=false;;

More_coherent_pdf.initialize;;

let z1=
"t=$topic_id\" . (($start == 0) ? '' : \"&amp;start=$start\") . ((strlen($u_sort_param)) ? \"&amp;$u_sort_param\" : '') . (($highlight_match) ? \"&amp;hilit=$highlight\" : ''));\n\n// Are we watching this topic?"
;;
*)



(*
let label_for_one_liner_with_variable="one_liner_with_variable";;
add_label label_for_one_liner_with_variable;;

let one_liner_with_variable_recognizer s i=
  if not(Substring.is_a_substring_located_at "/*" s i)
  then None
  else 
  let j=Substring.leftmost_index_of_in_from "*/" s (i+2) in
  if j<1 
  then None
  else Some(label_for_one_liner_with_variable,[i;j+2],j+2);;

add_recognizer (label_for_one_liner_with_variable,one_liner_with_variable_recognizer);; 

*)

(*
let s="define('IN_PHPBB', true);\n$phpbb_root_path = (defin";;
let i=1;;

let difyne_constant_recognizer s i=
  if not(Substring.is_a_substring_located_at "define" s i)
  then None
  else 
  let opt=After.after_whites  s i in
  if opt=None then None else
  let i2=Option.unpack opt in
  if not(Substring.is_a_substring_located_at "(" s i2)
  then None
  else 
  let i3=After.after_closing_character ('(',')') s (i2+1,1) in
  let opt4=After.after_whites  s i3 in
  if opt4=None then None else
  let i4=Option.unpack opt4 in
  if not(Substring.is_a_substring_located_at ";" s i4)
  then None
  else Some(label_for_difyne_constant,[i;i2;i3;i4],i4+1);;
*)

(*  
let s_ap=
  "~/Downloads/arno2.rtfd/TXT.rtf";;
let ap=Absolute_path.of_string s_ap;;  
let text=Io.read_whole_file ap;;
*)

(*

#use"Remembered/Attempts_at_Php_Handling/Fourth_Attempt_at_Php_handling/gr_classmap.ml";;

#use"Remembered/Attempts_at_Php_Handling/Fourth_Attempt_at_Php_handling/gr_tools.ml";;

#use"Remembered/Attempts_at_Php_Handling/Fourth_Attempt_at_Php_handling/gr_prepare_actions.ml";;

let initialization=Io.overwrite_with main_file initial_content;;

try_chunk 0;;

let c=
  cm ("intermDoDisplay",
  "__TwigTemplate_a26f7dafef087abe6048b68fa3a99980d85524ca"^
  "a6c2f78affd678055afc071d","");;  

exception Nonunique_introduction of string;;
exception Bad_introduction_ending of string;;
exception First_ivy_simplification_failed;;
exception Second_ivy_simplification_failed;;

let intro=Functionlike_item.before_content c;;

let main_text=Io.read_whole_file main_file;;
let temp1=Substring.occurrences_of_in intro main_text;;
if List.length(temp1)<>1
then raise(Nonunique_introduction(intro))
else
let i1=List.hd temp1;;
if (not(Substring.ends_with intro "{"))
then raise(Bad_introduction_ending(intro))
else 
let i2=i1+(String.length intro)-1;;
let i3=After.after_closing_character ('{','}') main_text (i2+1,1);;
let i4=String.length main_text;;
let before_body=Cull_string.interval main_text 1 i2;;
let body=Cull_string.interval main_text (i2+1) (i3-2);;
let after_body=Cull_string.interval main_text (i3-1) i4;;
let _=(
  zoom_text body;
  Ivy_aware_inflate.file small_file
  );;
let m1=(!(Ivy_aware_inflate.counter));;
let body2 = zoom_return();;
let _=(Io.overwrite_with main_file (before_body^body2^after_body));;
if (not(shortened_main_test()))
then (Io.overwrite_with main_file main_text;raise(First_ivy_simplification_failed))
else
let tl1=stone_survivors ();;
let _=(Ivy_aware_deflate.file tl1 small_file);;
let body3 = zoom_return();;
let _=Io.overwrite_with main_file (before_body^body3^after_body);;
if (not(shortened_main_test()))
then (Io.overwrite_with main_file main_text;raise(Second_ivy_simplification_failed))
else


Ivy_aware_inflate.counter:=0;;
Ivy_aware_inflate.file small_file;;
let m1=(!(Ivy_aware_inflate.counter));;

let ctr_fun4=zoom_return();;
Io.overwrite_with main_file (before_ctr^ctr_fun4^after_ctr);;
shortened_main_test();;

let tl1=stone_survivors();;

Ivy_aware_deflate.file tl1 small_file;;
let ctr_fun5=zoom_return();;
Io.overwrite_with main_file (before_ctr^ctr_fun5^after_ctr);;
shortened_main_test();;

Ivy_aware_inflate.counter:=0;;
Ivy_aware_inflate.file small_file;;
let m1=(!(Ivy_aware_inflate.counter));;

let ctr_fun6=zoom_return();;
Io.overwrite_with main_file (before_ctr^ctr_fun6^after_ctr);;
shortened_main_test();;

let tl1=stone_survivors();;

Ivy_aware_deflate.file tl1 small_file;;
let ctr_fun7=zoom_return();;
Io.overwrite_with main_file (before_ctr^ctr_fun7^after_ctr);;
shortened_main_test();;

add_overloaded_change_description
(false,
[

"special_replace";
"";
"\n\n    protected function loadTemplate($template, $templateName = null, $line = null, $index = null)\n    {";
"\n\n    public function loadTemplate($template, $templateName = null, $line = null, $index = null)\n    {";

]);;
*)


(*

let s_rachel="/Users/ewandelanoy/Documents/Sites/Rachel/public_html/";;

let small_file=Absolute_path.of_string (s_rachel^"small.php");;

let small_text= Io.read_whole_file small_file;;

let s=small_text;;
let l=Ivy_aware_decomposition.on_string s;;
let s1=String.concat "" 
(image Ivy_aware_item.full_content l);;
let check=(s1=s);;

*)

(*
let u1=am();;
let u2=List.filter (
  fun x->Substring.begins_with x "html_"
)u1;;
*)


(*
let s_rachel="/Users/ewandelanoy/Documents/Sites/Rachel/public_html/";;

let main_file=Absolute_path.of_string (s_rachel^"iewtopic.php");;

let main_text= Io.read_whole_file main_file;;

let u1=Functionlike_decomposition.on_string main_text;;
let u2=image Functionlike_item.full_content u1;;
let u3=String.concat "" u2;;
let check=(u3=main_text);;
*)



(*

let s="123private   function" and i_fnctn=14;;
let opt=Option.seek(
  fun t->not(List.mem 
    (Strung.get s (i_fnctn-t))
    [' ';'\n';'\r';'\t']
  )
) (Ennig.ennig 1 (i_fnctn-1));;
if opt=None then i_fnctn else
let j=i_fnctn-(Option.unpack opt);;
let awpt= Option.seek(fun
  kwd->
   Substring.is_a_substring_located_at kwd s (j-(String.length kwd)+1)
) ["public";"private";"protected"];;
None->i_fnctn
let kwd0=Option.unpack awpt;;
let i2_fnctn=j-(String.length kwd0)+1;;
let opt2=Option.seek(
 fun t->not(List.mem 
   (Strung.get s (i2_fnctn-t))
   [' ';'\n';'\r';'\t']
 )
) (Ennig.ennig 1 (i2_fnctn-1));;
if opt2=None then i2_fnctn else
let j2=i2_fnctn-(Option.unpack opt2);;
if Substring.is_a_substring_located_at "abstract" s (j2-7)
then j2-7
else j2+1;;

*)

(*

let t1=Io.read_whole_file(
   Absolute_path.of_string (s_rachel^"first_sample.php")
);;
let t2=Io.read_whole_file(
  Absolute_path.of_string (s_rachel^"second_sample.php")
);;
let check=(t1=t2);;

let y1=all_kali_items ();;
let y2=List.filter(
   fun x->
    (Classlike_item.kind x)=Classlike_kind.abstract_class
) y1;;
let y3=Option.find(
  fun x->
   (Classlike_item.class_name x)="AbstractConstantFolding"
) y2;;
let y4=Classlike_item.full_content y3;;
let y5="\n\n\n"^y4^"\n\n\n";;

*)

(*

let cl_full=Classlike_item.full_content cl;;
let i_cl_full=loc cl_full;;

let z1=Cull_string.beginning 2000 cl_full;;
let jz1=oc z1;;
let m1=String.length cl_full;;
let text1=mf();;
Option.seek(fun k->
   (Strung.get cl_full k)
   <>(Strung.get text1 (k+2010561))
)(Ennig.ennig 1 m1);;


let z2=Unqsubstr_replace.left_helper (mf())
2010562 (2010562+13);;


let u1=Io.read_whole_file small_file;;
let u2=Lines_in_string.interval u1 3 120;;
let u3=Cull_string.coending 1 u2;;
let u4=Str.split (Str.regexp_string ",\n") u3;;

exception Arrower_exn of string;; 

let arrower s=
   let temp1=Substring.occurrences_of_in "=>" s in
    let i=List.hd temp1 in
   (Cull_string.beginning (i+1) s,
    Cull_string.cobeginning (i+1) s);;

let u5=image arrower u4;;    
let u6=Ennig.index_everything u5;;
let u7=image(
  fun (j,(before_arrow,after_arrow))->
  "$dutronc"^(string_of_int j)^" = "^after_arrow^";"
) u6;;
let part_one = String.concat "\n" u7;;
let u8=image(
  fun (j,(before_arrow,after_arrow))->
  before_arrow^"$dutronc"^(string_of_int j)^","
) u6;;
let u9 = String.concat "\n" u8;;
let part_two = 
  "$template->assign_vars(array(\n"^ u9 ^ "\n));";;
let whole = part_one ^ "\n\n\n" ^ part_two;;
*)



(*

let j1=get_function_text_from_prelude "public function get($id, $invalidBehavior = ContainerInterface::EXCEPTION_ON_INVALID_REFERENCE)\n    {";;

let j2=fst j1;;


let j3=Replace_inside.replace_inside_string 
("ContainerInterface","\\Symfony\\Component\\DependencyInjection\\ContainerInterface")
j2;;

let j4=Replace_inside.replace_inside_string 
("InactiveScopeException","\\Symfony\\Component\\DependencyInjection\\Exception\\InactiveScopeException")
j3;;

*)

(*


ion
(false,
[

"special_replace";
"extract($phpbb_dispatcher->trigger_event('core.modify_text_for_display_before', compact($vars)));\n";
"$renderer = $phpbb_container->get('text_formatter.renderer');";
"mark_on_stone('Claire');\n$renderer = $phpbb_container->get('text_formatter.renderer');\nmark_on_stone('Severac');";

]);;

let text=mf();;

let u1=Chronometer.it Functionlike_decomposition.on_string text;;

let counter=ref(0);;

let u2=image(
     fun x->
       if (Functionlike_kind.is_a_method (Functionlike_item.kind x))&&
          (Functionlike_item.function_name x="get")
       then let k=(!counter)+1 in
            let _=(counter:=k) in
            let added_one="\nmark_on_stone('Caluire"^(string_of_int k)^"');\n" in
            (Functionlike_item.before_content x)^added_one^
            (Functionlike_item.content x)^ 
            (Functionlike_item.after_content x)
       else Functionlike_item.full_content x
) u1;;

let u3=String.concat "" u2;;
Io.overwrite_with main_file u3;;

*)




(*
let prelude="function setup($lang_set = false, $style_id = false)\n\t{";;
let temp1=oc prelude;;
let i1=List.hd temp1;;
let m=String.length(prelude);;
let opt1=Option.seek(
    fun t->List.exists(
     fun keyword->Substring.is_a_substring_located_at   
       keyword prelude t        
    ) ["function";"private";"protected";"public"]
) (Ennig.ennig 1 m);;
if opt1=None
then raise(Bad_prelude(prelude))
else
let i2=i1+(Option.unpack opt1)-1;;
let opt2=First_pass_parse.fnctn (mf()) i2;;
if opt2=None
then raise(Wrong_location_for_prelude)
else 
let i3=fst(Option.unpack opt2)-1;;
Cull_string.interval (mf()) i2 i3;;
*)


(*
let text=mf();;

let u1=Chronometer.it Functionlike_decomposition.on_string text;;

let u2=List.filter(
  fun x->
    Functionlike_kind.is_a_method (Functionlike_item.kind x)
) u1;;

let u3=List.filter(
  fun x->
     (Functionlike_item.function_name x)="set_default_language"
) u2;;

let u4=List.filter(
  fun x->
     (Functionlike_item.function_name x)="language"
) u2;;

*)

(*
let text=mf();;

let u1=Chronometer.it Functionlike_decomposition.on_string text;;

let u2=List.filter(
   fun x->
     (Functionlike_item.kind=)
);;

let s_rachel="/Users/ewandelanoy/Documents/Sites/Rachel/public_html/";;

let small_file=Absolute_path.of_string (s_rachel^"small.php");;

Io.overwrite_with small_file w1;;

Replace_inside.replace_inside_file ("\r","") small_file;;

let w2=Lines_in_string.interval 
  (Io.read_whole_file small_file) 362 367;;

let s_rachel="/Users/ewandelanoy/Documents/Sites/Rachel/public_html/";;

let main_file=Absolute_path.of_string (s_rachel^"iewtopic.php");;

let text=Io.read_whole_file main_file;;

let u1=Chronometer.it Functionlike_decomposition.on_string text;;
let u2=image Functionlike_item.full_content u1;;
let u3=String.concat "" u2;;
let check1=(u3=text);;

let m=min (String.length text) (String.length u3);;
let opt1=Option.seek(
    fun j->(Strung.get text j)<>(Strung.get u3 j)
) (ennig 1 m);;
let j1=Option.unpack opt1;;


let z1="<?php\nnamespace{\n"^
(Lines_in_string.interval text 11049 11070)^"\n}";;
let z2=Classlike_decomposition.on_string z1;;
let should_be_z1=String.concat "" (image Classlike_item.full_content z2);;
let check_1=(z1=should_be_z1);;
let z3=Functionlike_decomposition.on_string z1;;
let z4=image Functionlike_item.kind z3;;
*)



(*
let s=("private function peggy ($u,$v) {567;} "^
"public function arthur ($a,$b) {765;}"^
"function betty($g,$h) {678;} 098");;

let u1=on_class_contained_text "NPX" "myclass" s;;
let u2=String.concat "" (image Functionlike_item.full_content u1);;
let check=(u2=s);;
*)


(*

{                                                                
  kind : Functionlike_kind.t;
  namespace : string;
  containing_class : string;
  method_qualifier : string;
  function_name : string;
  before_content : string;
  content : string;
  after_content : string;
}

let s="private  \t function amy   ($u,$v,$w=83) \n"^
" {for($x=7;x++) {a=b;} dann();} unt; ";;

let s=" \t function amy  ($u,$v,$w=83)      \n"^
" {for($x=7;x++) {a=b;} dann();} unt; ";;

First_pass_parse.fnctn s 1;;

(*

on_namespaced_text "NKP" "123 class abc xyz {uvw}";;
on_namespaced_text "NKP" "class abc xyz {uvw}";;

*)

let on_namespaced_part
(nspc_line,nspc_content,nspc_offset,after_nspc)=
  let (nspc_name,_)=Option.unpack(Nspc_detect.extract_namespace_name nspc_line) in
  let elt1=Classlike_item.namespace_line nspc_line
  and temp2=on_namespaced_text nspc_name nspc_content
  and elt3=Classlike_item.after_namespace_comments (nspc_offset^after_nspc) in
  Merge_nonclasses.mn (elt1::temp2@[elt3]);;
  
(* The decomposed form is assumed to be standardized already. *)

let on_decomposed_form df=
    let elt1=Classlike_item.non_class "" 
      (Nspc_decomposed_form.before_namespaces df)  
    and temp2=Image.image on_namespaced_part 
    (Nspc_decomposed_form.namespaced_parts df) in
    Merge_nonclasses.mn (List.flatten([elt1]::temp2));;  
  
let on_string s=on_decomposed_form(
  Nspc_split.decompose s
);;

*)



(*
let determine_exact_beginning s i_fnctn=
  let opt=Option.seek(
     fun t->not(List.mem 
       (Strung.get s (i_fnctn-t))
       [' ';'\n';'\r';'\t']
     )
  ) (Ennig.ennig 1 (i_fnctn-1)) in
  if opt=None then ("1",i_fnctn,None,-1,None) else
  let j=i_fnctn-(Option.unpack opt) in
  match Option.seek(fun
     kwd->
      Substring.is_a_substring_located_at kwd s (j-(String.length kwd)+1)
  ) ["private";"protected"] with
   None->("2",i_fnctn,opt,j,None)
  |Some(kwd0)->("3",j-(String.length kwd0)+1,opt,j,Some kwd0);;
*)
  

(*

determine_exact_beginning "123private function" 12;;

*)    


(*
let text="<?php\n"^
"\nnamespace Peggy{\n class Arnold { blacken(); } whiten(); \n\n}\n /* Always there */ "^
"\nnamespace Amy{\n cold(); class Dorian { hot(); } \n\n}\n /* There again */ ";;

let haag1=Nspc_split.decompose text;;

let bad1=Classlike_decomposition.on_decomposed_form haag1;;

let df=haag1;;
let elt1=Classlike_item.non_class "" 
(Nspc_decomposed_form.before_namespaces df)  ;;
let haag2=Nspc_decomposed_form.namespaced_parts df;;

let temp2=Image.image Classlike_decomposition.on_namespaced_part 
haag2;;
let haag3=List.flatten([elt1]::temp2);;

let see=String.concat "" (image Classlike_item.full_content haag3);;
let haag4=List.hd haag2;;
let bad2=Classlike_decomposition.on_namespaced_part haag4;;

let (nspc_line,nspc_content,nspc_offset,after_nspc)=haag4;;


let (nspc_name,_)=Option.unpack
(Nspc_detect.extract_namespace_name nspc_line);;
let elt1=Classlike_item.namespace_line nspc_line
and temp2=Classlike_decomposition.on_namespaced_text nspc_name nspc_content
and elt3=Classlike_item.after_namespace_comments 
(nspc_offset^after_nspc);;

let haag5=elt1::temp2@[elt3];;
let haag6=image (fun x->x.Classlike_item.kind) haag5;;


Merge_nonclasses.mn (elt1::temp2@[elt3]);;


let bad3=Classlike_decomposition.on_namespaced_text nspc_name nspc_content;;

let s=nspc_content;;
let n=String.length s;;
let rec push=(
  fun (graet,idx)->
    if idx>n
    then failwith("Ice cream")
    else 
    match Unshadowed_appearance.next 
     s ["abstract";"final";"class";"interface"] idx with
    None->
        failwith("Pieces of me") 
    |Some(jdx)->
         let opt=First_pass_parse.classlike_item nspc_name s jdx in
         if opt=None
         then failwith("Dangling")
         else
         let item2=Option.unpack opt in
         let new_idx=jdx+(Classlike_item.length item2) in
         if jdx=idx
         then (item2::graet, new_idx) 
         else let item1=
               Classlike_item.non_class nspc_name 
                (Cull_string.interval s idx (jdx-1)) in
              (item2::item1::graet, new_idx)
);;
let v0=([],1);;
let ff=Memoized.small push v0;;
let (graet,idx)=v0;;
let upt=Unshadowed_appearance.next 
s ["abstract";"final";"class";"interface"] idx;;
let jdx=Option.unpack upt;;
let opt=First_pass_parse.classlike_item nspc_name s jdx ;;
let item2=Option.unpack opt;;
let new_idx=jdx+(Classlike_item.length item2);;
let item1=
  Classlike_item.non_class nspc_name 
   (Cull_string.interval s idx (jdx-1));;

let (graet,idx)=ff 1;;
let last_item=
    Classlike_item.non_class nspc_name 
      (Cull_string.interval s idx n);;
*)

(*

let s_rachel="/Users/ewandelanoy/Documents/Sites/Rachel/public_html/";;

let main_file=Absolute_path.of_string (s_rachel^"iewtopic.php");;

let text=Io.read_whole_file main_file;;

let u1=Classlike_decomposition.on_string text;;
let u2=image Classlike_item.full_content u1;;
let u3=String.concat "" u2;;
let check1=(u3=text);;


*)



(*

let qualification="abstract"
and nspc_name="NPK" 
and s="  abstract class 456{789}01" 
and idx=1;;



let opt1=After.after_whites s idx;;
if opt1=None then None else 
let i1=Option.unpack opt1;;
if not(Substring.is_a_substring_located_at qualification s i1)
then None
else
let opt2=After.after_whites s (i1+(String.length qualification));;
if opt2=None then None else 
let i2=Option.unpack opt2;;
if not(Substring.is_a_substring_located_at "class" s i2)
then None
else 
let opt3=After.after_whites s (i2+5);;
if opt3=None then None else 
let i3=Option.unpack opt3;;
let opt4=After.after_php_label s i3;; 
if opt4=None then None else 
let i4=Option.unpack opt4;;
let i5=Substring.leftmost_index_of_in_from "{" s i4;;
if i5<1 then None else
let i6=After.after_closing_character ('{','}')  s (i5+1,1);;
let answer=Classlike_item.make 
  (Classlike_kind.from_class_qualification qualification)
  nspc_name
  (Cull_string.interval s i3 (i4-1))
  (Cull_string.trim_spaces(Cull_string.interval s i4 (i5-1)))
  (Cull_string.interval s idx i5)
  (Cull_string.interval s (i5+1) (i6-2))
  (Cull_string.interval s (i6-1) (i6-1))

*)

(*
let text=mf();;
let z1=Unshadowed_appearance.all 
text ["include_once";"require_once";"include";"require"] ([],1);;
*)


(*
let classes_inside text=
   let temp1=Unshadowed_appearance.all text ["class"] ([],1) in
   let temp2=List.filter(
    fun i->if i=1 then true else
           let c=Strung.get text (i-1) in
     not(List.mem c ['$';'>'])
    ) temp1 in
   let temp3=image(
      fun i->let j=Substring.leftmost_index_of_in_from "{" text (i+6) in
        (i,itv text (i+6) (j-1))
   ) temp2 in
   let temp4=image (fun (i,t)->
   let m=String.length t in
   let ttemp5=image (fun wd->
   Substring.leftmost_index_of_in_from wd t 1 ) ["implements";"extends"] in
   let ttemp6=List.filter(fun j->j>0) ttemp5 in
   let p=Min.list ((m+1)::ttemp6) in
   let ttemp7=itv t 1 (p-1) in
   (i,Cull_string.trim_spaces ttemp7) 
 ) temp3 in
   temp4;;



let text=mf();;
let dec_form=Nspc_split.decompose text;;
let u1=Nspc_decomposed_form.namespaced_parts dec_form;;
let nana x=fst(Option.unpack(Nspc_detect.extract_namespace_name x));;
let u2=Explicit.image (fun (a,b,c,d)->
  let nspc_name=nana(a) in
  let ttemp1=classes_inside b in
  (nspc_name,Image.image snd ttemp1)
) u1;;
let u3=List.flatten(Image.image snd  u2);;
let u4=ofo(Ordered.diforchan Total_ordering.lex_for_strings u3);;
let u5=Explicit.image(
   fun cn->
    (cn,Option.filter_and_unpack(fun 
       (nn,l)->if List.mem cn l
               then Some(nn)
               else None
    ) u2)
) u4;;
let rw_string s=Strung.enclose(String.escaped s);;
let rw_string_list l=
    let temp1=Image.image rw_string l in
    let temp2=String.concat ";" temp1 in
    "["^temp2^"]";;
let rw_pair (s,l)=
  "("^(rw_string s)^",\n"^(rw_string_list l)^")";;
let rw_pair_list ll=
  let temp1=Image.image rw_pair ll in
  let temp2=String.concat ";\n" temp1 in
  "[\n"^temp2^"\n]";;
let u6=rw_pair_list u5;;
let u7="let classmap=\n"^u6^";;\n\n\n";;

let ap=Absolute_path.of_string
"Remembered/Attempts_at_Php_Handling/Fourth_Attempt_at_Php_handling/gr_classmap.ml";;

Io.append_string_to_file u7 ap;;

*)

(*
let v1=List.filter (fun (a,b,c,d)->
let nspc_name=nana(a) in
nspc_name="Symfony\\Component\\DependencyInjection\\Dumper"
) u1;;
let (_,v2,_,_)=List.hd v1;;
let v3=classes_inside v2;;

*)

(*

let (_,text,_,_)=List.nth u1 5;;
let temp1=Unshadowed_appearance.all text ["class"] ([],1);;
let temp2=List.filter(
  fun i->let c=Strung.get text (i-1) in
   not(List.mem c ['$';'>'])
  ) temp1;;
 let temp3=image(
    fun i->let j=Substring.leftmost_index_of_in_from "{" text (i+6);;
      (i,itv text (i+6) (j-1))
 ) temp2;;
 let temp4=image (fun (i,t)->
 let m=String.length t;;
 let ttemp5=image (fun wd->
 Substring.leftmost_index_of_in_from wd t 1 ) ["implements";"extends"];;
 let ttemp6=List.filter(fun j->j>0) ttemp5;;
 let p=Min.list ((m+1)::ttemp6);;
 let ttemp7=itv t 1 (p-1);;
 (i,Cull_string.trim_spaces ttemp7) 
) temp3;;
 temp4;;

*)



(*
let vv edg=List.filter (
fun (i,t)->Substring.ends_with t edg
) v2;;


let iv x=itv (mf()) (x-1200) (x+20);;
*)


(*

let s=mf();;
let (i,j)=(303176, 303244);;

let hard1=Substring.occurrences_of_in(String.sub s 0 j) s;;

  let hard2=Substring.occurrences_of_in (String.sub s (i-1) (j-i+1)) s in
  if List.length(hard2)=1
  then Unqsubstr_helper.of_string ""
  else 
  let bad_ones=List.filter (fun t->t<>i) hard2;;
  let bounds=Ennig.ennig 1 (i-1);;
  let measure=
      fun t->Option.find(
        fun d->(String.get s (i-d-1))<>(String.get s (t-d-1))
      ) bounds
  ;;
  let (_,m)=Max.maximize_it measure bad_ones;;
  Unqsubstr_helper.of_string(String.sub s (i-m-1) m);;

*)  

(*
#use"Remembered/Attempts_at_Php_Handling/Fourth_Attempt_at_Php_handling/old_gr_tools.ml";;

#use"Remembered/Attempts_at_Php_Handling/Fourth_Attempt_at_Php_handling/old_gr_prepare_actions.ml";;

#use"Remembered/Attempts_at_Php_Handling/Fourth_Attempt_at_Php_handling/gr_tools.ml";;

#use"Remembered/Attempts_at_Php_Handling/Fourth_Attempt_at_Php_handling/gr_prepare_actions.ml";;


let z1=(!reference_for_changes);;
let z2=Tidel.diforchan(image(
   fun (t0,_,_,_,_,_,_)->t0
) z1);;

let usual_list (t0,t1,t2,t3,t4,t5,opt)=
  if t0="Comment"
  then [label_for_add_comment;t1]
  else
  if t0="Expand function"
  then [label_for_expand_fnctn;t1;t2]
  else
  if (t0="Expand inclusion")||(t0="Expand inclusion with memo")
  then [label_for_expand_inclusion;t1;t2]
  else
  if t0="Replace"
  then [label_for_special_replace;t1;t2;t3]
  else 
  if t0="Retract"
  then [label_for_retract;t1;t2;t3;t4;t5]
  else failwith("pro");;

let rewrite (t0,t1,t2,t3,t4,t5,opt)=
    let l=usual_list (t0,t1,t2,t3,t4,t5,opt) in
    match opt with
    None->(false,l)
    |Some(memo)->(true,l@[memo]);;

let z3=Explicit.image rewrite z1;;

reference_for_change_descriptions:=z3;;    

wwrite_flash_back ();;

*)

(*
let unreversed_s= " $admiral =  betty ( $u , $v+3 , 'abcde' )  ; ";;
let s=Strung.reverse unreversed_s;;
let n=String.length s;;
let i1=Substring.leftmost_index_of_in ")" s;;
let i2=After.after_closing_character (')','(') s (i1+1,1);;
let opt3=After.after_whites s i2;;
let i3=Option.unpack opt3;;
let opt4=After.after_star 
Charset.strictly_alphanumeric_characters s i3;;
let i4=Option.unpack opt4;;
let opt5=analyse_terms_between_method_and_assignment computed_s i4;;

let idx=i4;;
let opt=Option.seek(fun j->(Strung.get s j)='=')(Ennig.ennig idx n);;
let j0=Option.unpack opt;;
let temp1=Cull_string.cobeginning j0 s;;
let temp2=Cull_string.trim_spaces temp1;; 
let (computed_s,receiver)=
         (Cull_string.beginning (j0-1) s,
          Strung.reverse temp2);;


let (computed_s,receiver)=
    (
        match opt with
        None->(s,"")
        |Some(j0)->
           let temp1=Cull_string.cobeginning j0 s in
           let temp2=Cull_string.trim_spaces temp1 in 
           (Cull_string.beginning (j0-1) s,
            Strung.reverse temp2)
    ) in
match analyse_terms_between_method_and_assignment computed_s idx with
None->None
|Some(caller,methkind)->Some(receiver,caller,methkind);;
*)


(*
let z1=am();;
let z2=List.filter (
  Substring.is_a_substring_of "char"
) z1;;
*)

(*
let agreda=Absolute_path.of_string
"/Users/ewandelanoy/Documents/html_files/Printable/agreda.txt";;

let ur_agreda=Absolute_path.of_string
"/Users/ewandelanoy/Documents/html_files/Printable/unreplaced_agreda.txt";;

let short_file=Absolute_path.of_string
"/Users/ewandelanoy/Documents/html_files/Printable/short.txt";;

let latinized_short_file=Absolute_path.of_string
"/Users/ewandelanoy/Documents/html_files/Printable/latinized_short.txt";;

let short_text=Io.read_whole_file short_file;;


let ag ()=
  Cull_string.beginning 500 (Io.read_whole_file agreda);;

Cull_string.beginning 500 (Io.read_whole_file ur_agreda);;

let ur_ag=Io.read_whole_file ur_agreda;;
Io.overwrite_with agreda ur_ag;;

let replacements=
  [
  "\197\147","oe"; 
  "\226\128\147","--"; 
  "\226\128\148","--"; 
  "\226\128\153","'"; 
   "\226\128\158","\""; 
   "\226\128\162","\248"; 
   "\226\128\166","..."; 
   "\226\128\168","\n"
   ] ;;
  
Replace_inside.replace_several_inside_file 
  replacements agreda;;

let u1=Lines_in_string.core (Io.read_whole_file agreda);;
let u2=List.assoc 10098 u1;;
let u3=Cull_string.cobeginning 1690 u2;;

Io.overwrite_with short_file 
*)


(*

let peggy_list=[
  ['\194'; '\160'];
  ['\194'; '\161'];
  ['\194'; '\162'];
  ['\194'; '\167'];
  ['\194'; '\171'];
  ['\194'; '\176'];
  ['\194'; '\187'];
  ['\195'; '\128'];
  ['\195'; '\130'];
  ['\195'; '\135'];
  ['\195'; '\136'];
  ['\195'; '\137'];
  ['\195'; '\138'];
  ['\195'; '\153'];
  ['\195'; '\160'];
  ['\195'; '\162'];
  ['\195'; '\167'];
  ['\195'; '\168'];
  ['\195'; '\169'];
  ['\195'; '\170'];
  ['\195'; '\171'];
  ['\195'; '\174'];
  ['\195'; '\175'];
  ['\195'; '\177'];
  ['\195'; '\180'];
  ['\195'; '\185'];
  ['\195'; '\187'];
  ['\195'; '\188'];
  ['\197'; '\147'];
  ['\226'; '\128';'\147'];
  ['\226'; '\128';'\148'];
  ['\226'; '\128';'\153'];
  ['\226'; '\128';'\158'];
  ['\226'; '\128';'\162'];
  ['\226'; '\128';'\166'];
  ['\226'; '\128';'\168'];
];;

let v1=image Strung.implode peggy_list;;
let v2=String.concat "a" v1;;

let short_file=Absolute_path.of_string
"/Users/ewandelanoy/Documents/html_files/Printable/short.txt";;

let latinized_short_file=Absolute_path.of_string
"/Users/ewandelanoy/Documents/html_files/Printable/latinized_short.txt";;

let command= 
  "/opt/local/bin/iconv -f UTF-8 -t latin1 "^
  "/Users/ewandelanoy/Documents/html_files/Printable/short.txt "^
  "> "^
  "/Users/ewandelanoy/Documents/html_files/Printable/latinized_short.txt";;
  

let clara s=
    let _=Io.overwrite_with short_file s in
    let j=Sys.command command in
    if j<>0 then None else
    let translation=Io.read_whole_file latinized_short_file in
    Some(translation);;

let res1=image (fun s->(s,clara s)) v1;;
let v3=List.filter (fun (s,opt)->opt=None ) res1;;
let v4=image fst v3;;
let pp k=print_string(List.nth v4 (k-1));;

let viz s=
   let temp1=More_uchar.utf8_decode s in
   Image.image (fun u->
     (More_uchar.unicode_point u,More_uchar.utf8_coordinates u)
   ) temp1;;
let vv k=viz(List.nth v4 (k-1));;

let replacements=
[
"\197\147","oe"; 
"\226\128\147","--"; 
"\226\128\148","--"; 
"\226\128\153","'"; 
 "\226\128\158","\""; 
 "\226\128\162","\248"; 
 "\226\128\166","..."; 
 "\226\128\168","\n"
 ] ;;

let agreda=Absolute_path.of_string
 "/Users/ewandelanoy/Documents/html_files/Printable/agreda.txt";;

Replace_inside.replace_several_inside_file 
replacements agreda;;

Replace_inside.replace_several_inside_file 
["\r",""] agreda;;

let agreda_text=
  Cull_string.beginning 500 (Io.read_whole_file agreda);;

let g1="\nV\142n\142rable Marie d'Agreda\nLa Cit\142 Mystique de Dieu\n\n\nLIVRE 1/30 \r\nINTRODUCTION A LA VIE DE LA REINE DU CIEL. Des raisons qu'on a eues de l'\142crire, et de plusieurs autres avis sur ce sujet.\nLA CIT\131 MYSTIQUE DE DIEU\nPREMI\233RE PARTIE. DE LA VIE ET DES MYST\233RES DE LA SAINTS VIERGE, REINE DU CIEL. \209 CE QUE LE TR\233S-SAUT OP\131RA EN CETTE PURE CR\131ATURE DEPUIS SON IMMACUL\131E CONCEPTION JUSQU'\203 CE QUE LE VERBE PRIT CHAIR HUMAINE DANS SON SEIN VIRGINAL. \209 LES FAVEURS QU'IL LUI FIT PENDANT LES QUINZE PREMI\233RES ";;



Sys.command command;;

*)

(*

let ag_file=Absolute_path.of_string
"/Users/ewandelanoy/Documents/html_files/Printable/old_agreda.txt";;
let ag_text=Io.read_whole_file ag_file;;   

let u1=Strung.explode ag_text;;


let u2=List.filter (
   fun c->(int_of_char c)>126
) u1;;

let peggy_list=[
  ['\195'; '\128'];
  ['\195'; '\136'];
  ['\195'; '\137'];
  ['\195'; '\160'];
  ['\195'; '\162'];
  ['\195'; '\167'];
  ['\195'; '\168'];
  ['\195'; '\169'];
  ['\195'; '\170'];
  ['\195'; '\174'];
  ['\195'; '\185'];
  ['\195'; '\187'];
  ['\226'; '\128';'\148'];
  ['\226'; '\128';'\153'];
  ['\226'; '\128';'\168'];
];;

let rec expurge l=
    if l=[] then [] else
    match Option.find_and_stop (
        fun blade -> Listennou.nice_cut blade l
    ) peggy_list with
    Some(new_l)->expurge new_l
    |None->
    let m=min(List.length l)(20) in
    Listennou.big_head m l;;

let u3=expurge u2;;

let short_file=
  Absolute_path.of_string
  "/Users/ewandelanoy/Documents/html_files/Printable/latinized_short.txt";;

let short_text=Io.read_whole_file short_file;;  

*)

(*

let short_file=
  Absolute_path.of_string
  "/Users/ewandelanoy/Documents/html_files/Printable/short.txt";;

*)


(*

let ag_file=
  Absolute_path.of_string
  "/Users/ewandelanoy/Documents/html_files/Printable/agreda.txt";;

let trim_footnote old_s=
  let s=Cull_string.trim_spaces old_s in
  let i=Substring.leftmost_index_of_in ")" s in
  String.capitalize_ascii 
  (Cull_string.trim_spaces(Cull_string.cobeginning i s));;

let parse_footnote_list old_s=
  let s=Cull_string.trim_spaces old_s in
  let temp1=Str.split (Str.regexp_string "\226\128\148") s in
  let temp2=Image.image trim_footnote temp1 in
   temp2;;

let is_a_footnote_line line=
  Substring.begins_with (Cull_string.trim_spaces line) "(1)";;

let rec df1_helper (graet,current,current_is_footnote,da_ober)=
  let next_block=(String.concat "\n" (List.rev current)) in
  match da_ober with
  []->List.rev( next_block ::graet)
  |(j,line)::peurrest->
     if is_a_footnote_line line
     then df1_helper (next_block ::graet,[line],true,peurrest)
     else 
     if current_is_footnote
     then df1_helper (next_block ::graet,[line],false,peurrest)
     else 
     df1_helper (graet,line::current,false,peurrest);;

let df1 s=
 if s="" then [] else
 match Lines_in_string.core s 
 with 
 []->[]
 |(_,line1)::b->df1_helper([],[line1],is_a_footnote_line line1,b);;       

let df2 s=
  match df1 s with
  []->[]
  |a::peurrest->
    let temp1=Image.image (fun x->["\n";x]) peurrest in
    let temp2=a::(List.flatten temp1) in
    List.filter (fun x->x<>"") temp2;;

let rec df3_helper (graet,char_count,da_ober)=
   match da_ober with
    []->List.rev(graet)
   |a::peurrest->
     let d=String.length(a) in
     let new_count=char_count+d in
     df3_helper (((char_count+1,new_count),a)::graet,new_count,peurrest);;

let naive_df3 s= df3_helper ([],0,df2 s);;
let df3=naive_df3;;

exception Bc1_exn;;
exception Bc2_exn;;

let back_check s l=
  if List.exists (fun ((i,j),t)->t<>(Cull_string.interval s i j)) l
  then raise(Bc1_exn)
  else
  if (String.concat "" (image snd l))<>s 
  then raise(Bc2_exn)
  else true;;

let df4 x=
let (bowl,((i,j),s))=x in
 if bowl
 then Image.image (fun ((a,b),t)->(true,((i-1+a,i-1+b),t)) ) (df3 s)
 else [x];;

let df5 l=List.flatten(Image.image df4 l);;

let is_a_num_char c=
let i=int_of_char c in (48<=i)&&(i<=57);;

let is_a_num_string s=
(s<>"")&&(List.for_all is_a_num_char (Strung.explode s));;

let is_a_num_line line =
let t=Cull_string.trim_spaces line in
(is_a_num_string(t));;

let test_for_orange_at_index s idx=
  if idx>(String.length s) then None else
  if (Strung.get s idx)<>'('
  then None
  else 
  let jdx=Substring.leftmost_index_of_in_from ")" s (idx+1) in
  if jdx<0 then None else
  let temp1=Cull_string.interval s (idx+1) (jdx-1) in
  if not(is_a_num_string temp1)
  then None
  else Some(jdx,int_of_string temp1);;

(*    
test_for_orange_at_index "(123)567" 1;;
test_for_orange_at_index "(1234567" 1;;
*)         


let rec orange_helper (graet,s,n,idx_start,idx)=
  if idx>n
  then let graet2=(
            if idx=idx_start
            then graet
            else (Cull_string.interval s idx_start n,None)::graet ) in
       List.rev(graet2)
  else
  match test_for_orange_at_index s idx with
   None->orange_helper (graet,s,n,idx_start,idx+1)
  |Some(idx_end,written_int)->
      let elt2=(Cull_string.interval s idx idx_end,Some(written_int)) in
      let graet2=(
         if idx=idx_start
         then elt2::graet
         else let elt1=(Cull_string.interval s idx_start (idx-1),None) in
              elt2::elt1::graet
      ) in
      orange_helper (graet2,s,n,idx_end+1,idx_end+1);;

let orange_decomposition s=
orange_helper ([],s,String.length s,1,1);;

(*  
orange_decomposition "(12)abc(3)de(4)ghi(5)uv";;
orange_decomposition "xy(12)abc(3)de(4)ghi(5)uv";;
orange_decomposition "xy(12)abc(3)de(4)ghi(5)uv(6)";;
*)

let tf6_transform (text,opt)=
  if opt=None then [text] else
  let footnotes=Option.unpack opt in
  let d=List.length footnotes in
  let temp1=orange_decomposition text in
  let temp2=Image.image(
     fun (part,opt2)->match opt2 with
      None->[part]
     |Some(i)->
         if (i<1)||(i>d)
         then []
         else ["("^(List.nth footnotes (i-1))^")"]
  )  temp1 in
  List.flatten temp2;;

let ex1="ab\ncd\n\nefg\n\n\nhi\n(1)uv\n89\n(1)xy\n88\n67\n(1)op\n";;
let res1=df3 ex1;;
let check1=(back_check ex1 res1);;

let ag_file=
    Absolute_path.of_string
    "/Users/ewandelanoy/Documents/html_files/Printable/agreda.txt";;

let u1=Io.read_whole_file ag_file;;
let u2=Decompose_into_paragraphs.dec u1;;
let u3=df5 u2;;
let u4=image snd u3;;
let check2=back_check u1 u4;;
let u5=Ennig.index_everything (image snd u4);;
let footnotes_in_u5=Option.filter_and_unpack (
 fun (j,line)->
   if is_a_footnote_line line
   then Some(j,parse_footnote_list line)
   else None
) u5;;

let find_victim=Memoized.make(fun j1->
  let temp1=List.rev(List.filter(fun (j,_)->j<j1) u5) in
  fst(Option.find (fun (j,line)->(Cull_string.trim_spaces line)<>"") temp1));;

let comp1=Explicit.image (fun (j,_)->find_victim j) footnotes_in_u5;;
let u6=image (fun (k,line)->(line,None)) u4;;
let arr_u6=Array.of_list u6;;
let comp2=Explicit.image(fun
  (j,footnotes)->
  let k=find_victim j in
  let (text,_)=Array.get arr_u6 (k-1) in
  Array.set arr_u6 (k-1) (text,Some(footnotes))
) footnotes_in_u5;;
let changed_u6=Array.to_list arr_u6;;


let (m1,sols1)=Max.maximize_it_with_care(
fun (text,opt)->match opt with
None->0
|Some(l)->List.length(l)
) changed_u6;;

let u10=Image.image tf6_transform changed_u6;;
let u11=String.concat "" (List.flatten u10);;

Io.overwrite_with ag_file u11;;

let u12=image snd (Lines_in_string.core u11);;
let u13=List.filter (fun line->
not(is_a_footnote_line line)
) u12;;
let u14=String.concat "\n" u13;;

Io.overwrite_with ag_file u14;;

let u15=Io.read_whole_file ag_file;;
let u16=orange_decomposition u15;;

*)


(*
let u7=Option.filter_and_unpack(
 fun (text,opt)->match opt with
 None->None
 |Some(footnotes)->Some(orange_decomposition text,footnotes)
) changed_u6;;


let u8=List.filter (
 fun (oranges,footnotes)->
   let d=List.length(footnotes) in
   List.exists(
     fun (part,opt)->if opt=None then false else
     let i=Option.unpack opt in
     if (i>10) then false else
     (i<1)||(i>d)
   ) oranges
) u7;;


let u9=image (
fun (_,footnotes0)->
  Option.find (fun (x,y)->y=footnotes0 ) u7
) u8;;

let (g1,g2)=List.nth u9 0;;
let g3=List.hd g1;;
let g4=List.hd(List.rev g1);;
*)




(*
let is_a_num_char c=
     let i=int_of_char c in (48<=i)&&(i<=57);;

let is_a_num_string s=
    List.for_all is_a_num_char (Strung.explode s);;

let is_a_num_line line =
    let t=Cull_string.trim_spaces line in
    (is_a_num_string(t))&&(t<>"");;

let ag_file=
  Absolute_path.of_string
  "/Users/ewandelanoy/Documents/html_files/Printable/agreda.txt";;

Replace_inside.replace_inside_file
   ("\226\128\168\194\160","")
   ag_file;;

let u1=Io.read_whole_file ag_file;;
let v1=Lines_in_string.core u1;;
let u2=Str.split (Str.regexp_string "\n") u1;;
let u3=List.filter (fun line->not(is_a_num_line line) ) u2;;
let u4=String.concat "\n" u3;;
Io.overwrite_with ag_file u4;;
*)


(*

let old_text=Io.read_whole_file main_file;;
let _=Put_markers_everywhere.in_file main_file;;
let res1=main_test();;
let ((mark_count1,line_count1),fn1,_)=res1;;
if mark_count1=0 then raise(Bad_marker) else
let new_text=Io.read_whole_file main_file;;
let needed_line=Marker.from_numbers mark_count1 line_count1;;
let needed_idx=Substring.leftmost_index_of_in needed_line new_text;;
let new_prelude=Cull_string.beginning (needed_idx-1) new_text;; 
let old_prelude=Marker.remove_all_markers new_prelude;;
let m1=(String.length old_prelude)+1;;
let check1=(Substring.begins_with old_text old_prelude);;
if not check1 then raise(Bad_namespace_split) else
let c=Strung.get old_text m1;;
if (c<>'\n') then raise(Bad_line_split(c)) else
let _=inform "Computation with markers finished.";;
let _=Io.overwrite_with main_file old_text;;
let hlpr=Unqsubstr_replace.left_helper old_text m1 m1;;
let helper=Unqsubstr_helper.to_string hlpr;;
let inclusion_order="include '"^fn1^"';";;    
let _=special_replace (helper,"\n") ("\n"^inclusion_order^"\n");;
if (not(smt())) then raise(Bad_inclusion_insertion) else
let _=write_latest ();;
let _=inform "Replacement before inclusion finished.";;
let _=expand_inclusion inclusion_order fn1;;
if (not(smt())) then raise(Bad_inclusion_expansion) else
let _=write_latest ();;
(!reference_for_main_test);;

*)

(*
let tt j=(j+1)*(j+1)*(j+1)-2*j*j*j;;

let modd (a,b)=
    let r =a mod b in
    if (r>=0)&&(r<b) then r else
    b+r;;

let ff p=
  doyle(fun j->
     (j,modd(tt j,p))
  ) 0 (p-1);;

let ff p=
    let tempf1=Memoized.make(fun j->
       modd(tt j,p)
    ) in
    let q=((p-1)/2) in
    let tempf2=(fun old_k->
      let k=(if old_k<0 then old_k+p else old_k) in
      let ttemp3=List.filter (fun j->tempf1 j=k) (ennig (-q) q) in
      if List.length(ttemp3)<2
      then None
      else Some(k,ttemp3)
    ) in
    Option.filter_and_unpack tempf2 (ennig (-q) q);;  
*)

(*

let peggy1 i=if (i>5)||(i<0) then "end" else
             if i=0 then "index" else (string_of_int i);;

let peggy2 i=Absolute_path.of_string(
  "/Users/ewandelanoy/Documents/html_files/Trower/trower_"^(peggy1 i)^".html"
) ;;            

let peggy3 i=if (i>5)||(i<1) then failwith("peggy3 error") else
   if i=3 then 579 else 573;;

let peggy4 x=List.assoc x
  [
    1,864;2,1005;3,801;4,1072;5,1057;5,838
  ];;

let peggy5 i=Absolute_path.of_string(
  "/Users/ewandelanoy/Documents/html_files/Trower/"^
  "trower_"^(peggy1 i)^"_files/master.css"
) ;;   

let u1=doyle peggy5 0 6;;

let v1=Io.read_whole_file (peggy2 6);;
let v2=Lines_in_string.interval v1 92 98;;
let act1=Explicit.image(
   fun ap->Replace_inside.replace_inside_file
      (v2,"") ap
) u1;;
let v1=Io.read_whole_file (peggy2 6);;
let v2=Cull_string.cobeginning 9 (Lines_in_string.interval v1 573 573);;
let act2=Explicit.image(
  fun ap->Replace_inside.replace_inside_file
     (v2,"") ap
) u1;;

let act3=Explicit.image(
  fun i->
  let ap=peggy2 i and j=peggy3 i in
  let old_text=Io.read_whole_file ap in
  let new_text=Lines_in_string.remove_interval old_text j j in
  Io.overwrite_with ap new_text
) (ennig 1 5);;


let act4=Explicit.image(
  fun i->
  let ap=peggy2 i in
  let old_text=Io.read_whole_file ap in
  let new_text=Lines_in_string.remove_interval old_text 252 468 in
  Io.overwrite_with ap new_text
) (ennig 0 6);;

let act5=Explicit.image(
  fun i->
  let ap=peggy2 i and j=peggy4 i in
  let old_text=Io.read_whole_file ap in
  let new_text=Lines_in_string.remove_interval old_text j (j+5) in
  Io.overwrite_with ap new_text
) (ennig 1 5);;

let v3=doyle (fun i->
    let ap=peggy2 i in
    let old_text=Io.read_whole_file ap in
    let u="<img src=\"trower_"^(peggy1 i)^"_files/search.png\" id=\"searchButton\">" in
    (i,ap,Substring.occurrences_of_in u old_text)
) 0 6;;

let act6=Explicit.image(
  fun i->
  let ap=peggy2 i in
  let u="<img src=\"trower_"^(peggy1 i)^"_files/search.png\" id=\"searchButton\">" in
   Replace_inside.replace_inside_file (u,"") ap
  ) (ennig 0 6);;

let v1=Io.read_whole_file (peggy2 6);;
let v2=Lines_in_string.interval v1 384 393;;

let v4=doyle (fun i->
let ap=peggy2 i in
let old_text=Io.read_whole_file ap in
(i,ap,Substring.occurrences_of_in v2 old_text)
) 0 6;;

let act7=Explicit.image(
  fun i->
  let ap=peggy2 i in
   Replace_inside.replace_inside_file (v2,"") ap
  ) (ennig 0 6);;

let v5=doyle (fun i->
  let ap=peggy2 i in
  let old_text=Io.read_whole_file ap in
  let i1=Substring.leftmost_index_of_in 
      "<div class=\"sidebar_right\">" old_text in
  let j1=After.after_div old_text i1 in    
  let sub_text=Cull_string.interval old_text i1 (j1-1) in
  Replace_inside.replace_inside_file (sub_text,"") ap
) 0 6;;

let tag="<div class=\"ad\" id=\"adspot_horizontal_mobile\">";;

let v6=doyle (fun i->
let ap=peggy2 i in
let old_text=Io.read_whole_file ap in
let i1=Substring.leftmost_index_of_in 
    tag old_text in
i1
) 0 6;;

let v7=doyle (fun i->
let ap=peggy2 i in
let old_text=Io.read_whole_file ap in
let i1=Substring.leftmost_index_of_in 
    tag old_text in
let j1=After.after_div old_text i1 in    
let sub_text=Cull_string.interval old_text i1 (j1-1) in
Replace_inside.replace_inside_file (sub_text,"") ap
) 0 6;;

let a1=105 and b1=110;;

let v8=doyle (fun i->
let ap=peggy5 i in
let old_text=Io.read_whole_file ap in
let sub_text=Lines_in_string.interval old_text a1 b1 in
 (i,sub_text)
) 0 6;;

let correct_version=snd(List.nth v8 6);;

let v8=doyle (fun i->
let ap=peggy5 i in
let old_text=Io.read_whole_file ap in
let sub_text=Lines_in_string.interval old_text a1 b1 in
Replace_inside.replace_inside_file (sub_text,correct_version) ap
) 0 6;;

*)

(*

let old_text=mf();;
let act1=Put_markers_everywhere.in_file main_file;;
let res1=main_test();;
let ((mark_count1,line_count1),fn1,_)=res1;;
let new_text=Io.read_whole_file main_file;;
let needed_line=Marker.from_numbers mark_count1 line_count1;;
let needed_idx=Substring.leftmost_index_of_in needed_line new_text;;
let new_prelude=Cull_string.beginning (needed_idx-1) new_text;; 
let old_prelude=Marker.remove_all_markers new_prelude;;
let m1=(String.length old_prelude)+1;;
let check1=(Substring.begins_with old_text old_prelude);;

let c=Strung.get old_text m1;;
let act2=Io.overwrite_with main_file old_text;;
let helper=Substring.left_helper_for_unique_occurrence
        old_text m1 m1;;
let inclusion_order="include '"^fn1^"';";;    
let act3=special_replace (helper,"\n") ("\n"^inclusion_order^"\n");;
let act4=expand_inclusion inclusion_order fn1;;

*)

(*
let inclusion_line="include 'vendor/marc1706/fast-image-size/lib/Type/TypeBase.php';"
and fn="vendor/marc1706/fast-image-size/lib/Type/TypeBase.php";;

let comment_before="\n\n/* Inclusion of "^fn^" starts here */\n\n"
and comment_after="\n\n/* Inclusion of "^fn^" ends here */\n\n" 
and l_rep=[
        "'__DIR__'","'__D' . 'I' . 'R__'";
        "__DIR__","'"^(Father_and_son.father fn '/')^"'"
      ]@
      balancings;;


Nspc_expand_inclusion.file_in_file
  (comment_before,comment_after)
  l_rep
  (Absolute_path.of_string (s_rachel^fn)) inclusion_line main_file ;;
*)

(*
let old_text=mf();;
let act1=Put_markers_everywhere.in_file main_file;;
let res1=main_test();;
let ((mark_count1,line_count1),fn1,_)=res1;;
let new_text=Io.read_whole_file main_file;;
let needed_line=Marker.from_numbers mark_count1 line_count1;;
let needed_idx=Substring.leftmost_index_of_in needed_line new_text;;
let new_prelude=Cull_string.beginning (needed_idx-1) new_text;; 
let old_prelude=Marker.remove_all_markers new_prelude;;
let m1=(String.length old_prelude)+1;;
let check1=(Substring.begins_with old_text old_prelude);;
let opt1=Option.seek(fun j->
   (Strung.get old_text j)<>(Strung.get old_prelude j)
) (ennig 1 (String.length old_prelude));;

let check1=
  ((Io.read_whole_file(
    Absolute_path.of_string(s_rachel^"iewtopic1.php")))
    =mf());;
*)

(*
let old_text=mf();;
let bad1=Put_markers_everywhere.in_string old_text;;

let dec_form=Nspc_split.decompose old_text;;
let before_namespaces=Nspc_decomposed_form.before_namespaces dec_form
and items=Nspc_decomposed_form.namespaced_parts dec_form ;;

let u1=Ennig.index_everything items;;
let opt1=Option.seek(
    fun (j,(a,b,c,d))->
    try (fun _->false)(Put_markers_everywhere.in_namespace b) with
    _->true
 ) u1;;
let  (j1,(a1,b1,c1,d1))=Option.unpack opt1;; 
*)

(*

let rec nh
(mark_count,line_count,idx_start,idx,s,n,accu)=
  if idx>n
  then failwith("e1")
  else 
  if Substring.is_a_substring_located_at "/*" s idx
  then let j=Substring.leftmost_index_of_in_from "*/" s (idx+2) in
       let d=Lines_in_string.number_of_lines_in_char_interval s idx j in
       (mark_count,line_count+d,idx_start,j+2,s,n,accu)
  else 
  if Substring.is_a_substring_located_at "//" s idx
  then let j=Substring.leftmost_index_of_in_from "\n" s (idx+2) in
       (mark_count,line_count+1,idx_start,j+1,s,n,accu)
  else 
  if (Substring.is_a_substring_located_at "<<<EOF\n" s idx)
     ||
     (Substring.is_a_substring_located_at "<<<'EOF'\n" s idx) 
  then let j=Substring.leftmost_index_of_in_from "\nEOF;\n" s (idx+7) in
       let d=Lines_in_string.number_of_lines_in_char_interval s idx (j+5) in
       (mark_count,line_count+d,idx_start,j+6,s,n,accu)
  else
  let opt=After.after_classlike_block s idx in
  if opt<>None
  then let jdx=Option.unpack opt in
       let d=Lines_in_string.number_of_lines_in_char_interval s idx jdx in
       let marker_line=
        "\nmarker_here("^(string_of_int(mark_count+1))^
        ","^(string_of_int (line_count+d+1))^");\n" in
        let elt=
          (Cull_string.interval s idx_start (jdx-1))^marker_line in
          (mark_count+1,line_count+d+2,jdx,jdx,s,n,elt::accu)
  else
  let c=Strung.get s idx in
  if c='\n'
  then (
         if Substring.is_a_substring_located_at ";" s (idx-1)
         then let marker_line=
               "marker_here("^(string_of_int(mark_count+1))^
               ","^(string_of_int (line_count+1))^");\n" in
              let elt=
               (Cull_string.interval s idx_start idx)^marker_line in
               (mark_count+1,line_count+2,idx+1,idx+1,s,n,elt::accu)
         else  (mark_count,line_count+1,idx_start,idx+1,s,n,accu)     
       )
  else
  if c='{'
  then let j=After.after_closing_character ('{','}') s (idx,0) in
       let d=Lines_in_string.number_of_lines_in_char_interval s idx (j-1) in
        (mark_count,line_count+d,idx_start,j,s,n,accu)
  else  (mark_count,line_count,idx_start,idx+1,s,n,accu);;

let z1="\n2345;\nclass3{5\n7\n9}12345;\n89";;
let v0=(0,1,1,1,z1,String.length z1,[]);;
let ff=Memoized.small nh v0;;

let (mark_count2,line_count2,idx_start2,idx2,_,_,accu2)=ff 76;;
let opt=After.after_classlike_block z1 idx2;;

*)


(*

let old_text=mf();;
let act1=Put_markers_everywhere.in_file main_file;;
let res1=main_test();;
let ((mark_count1,line_count1),fn1,_)=res1;;
let new_text=Io.read_whole_file main_file;;
let needed_line=Marker.from_numbers mark_count1 line_count1;;
let needed_idx=Substring.leftmost_index_of_in needed_line new_text;;
let new_prelude=Cull_string.beginning (needed_idx-1) new_text;; 
let old_prelude=Marker.remove_all_markers new_prelude;;
let m1=(String.length old_prelude)+1;;
let check1=(Substring.begins_with old_text old_prelude);;
let c=Strung.get old_text m1;;
let _=Io.overwrite_with main_file old_text;;
let helper=Substring.left_helper_for_unique_occurrence
        old_text m1 m1;;
let inclusion_order="include '"^fn1^"';";;    
let _=special_replace (helper,"\n") ("\n"^inclusion_order^"\n");;

*)

(*

let old_try_chunk d=
  let _=Io.overwrite_with main_file initial_content in
  let temp1=List.rev(!reference_for_changes) in
  let temp2=(if d<1 then temp1 else Listennou.big_head d temp1) in
  let _=Explicit.image fulfill_announcement temp2 in
  smt();;

let dbg_file=Absolute_path.of_string "debugged.ml";;

let u1=
  "let old_text =\n "^
  (Strung.enclose(String.escaped old_text))^ "\n ;;\n\n";;

Io.append_string_to_file u1 dbg_file;;

*)

(*

let dbg_file=Absolute_path.of_string "debugged.ml";;

let u1=
  "let container_text =\n "^
  (Strung.enclose(String.escaped container_text))^ "\n ;;\n\n"^
  "let inserted_text =\n "^
  (Strung.enclose(String.escaped inserted_text))^ "\n ;;\n\n"
  ;;

Io.append_string_to_file u1 dbg_file;;

let global_namespace_name s=
  let temp1=Str.split (Str.regexp_string "\n") s in
  match Option.find_and_stop extract_namespace_name temp1 with
  None->""
  |Some(nahme,_)->nahme;;

let inclusion_line="include($phpbb_root_path . 'common.' . $phpEx);";;
let fn="common.php";;


let comment_before="\n\n/* Inclusion of "^fn^" starts here */\n\n"
and comment_after="\n\n/* Inclusion of "^fn^" ends here */\n\n" 
and l_rep=[
      "'__DIR__'","'__D' . 'I' . 'R__'";
      "__DIR__","'"^(Father_and_son.father fn '/')^"'"
    ]@
    balancings;;

let inserted_text=Io.read_whole_file (Absolute_path.of_string (s_rachel^fn))
and container_text=Io.read_whole_file main_file;;   

let bad1=Nspc_expand_inclusion.string_in_string
(comment_before,comment_after)
[]
inserted_text inclusion_line container_text ;;

let g1=(!( Nspc_split.reference_for_decomposition_problems ));;




let s_micael="/Users/ewandelanoy/Documents/Sites/Mikeal/public_html/";;
let micael_dir=Directory_name.of_string s_micael;;

let u1=More_unix.complete_ls_with_nondirectories_only
  micael_dir;;
let u2=List.filter (
   fun ap->Substring.ends_with (Absolute_path.to_string ap) ".php"
) u1;; 
let check1=List.filter(
   fun ap->let text=Io.read_whole_file ap in
   not(Substring.begins_with text "<?php")
) u2;;


let viz s=
    let temp1=image snd (Lines_in_string.core s) in
    let n=List.length(temp1) in
    let temp2=Listennou.big_head 2 temp1
    and temp3=Listennou.big_head 2 (List.rev temp1) in
    (temp2,List.rev temp3);;
let u3=image (fun ap->(ap,viz(Io.read_whole_file ap))) u2;;

let (u5,u6)=List.partition(
  fun (ap,(l1,l2))->((List.length l1)<2)||((List.length l2)<2)
) u3;;

let u4=image(
   fun (ap,(l1,l2))->
    (ap,List.nth l1 0,List.nth l1 1,List.nth l2 0,List.nth l2 1)
) u3;;

*)


(*

expand_inclusion
   ("","include($phpbb_root_path . 'common.' . $phpEx);")
   "common.php";;

expand_inclusion
   ("","require($phpbb_root_path . 'includes/startup.' . $phpEx);")
   "includes/startup.php";;   

special_replace   
("","if (getenv('PHPBB_NO_COMPOSER_AUTOLOAD'))\n{\n\tif (getenv"^
"('PHPBB_AUTOLOAD'))\n\t{\n\t\trequire(getenv('PHPBB_AUTOLOAD'));"^
"\n\t}\n}\nelse\n{\n\tif (!file_exists($phpbb_root_path . "^
"'vendor/autoload.php'))\n\t{\n\t\ttrigger_error(\n\t\t\t'Composer "^
"dependencies have not been set up yet, run ' .\n\t\t\t\"'php "^
"../composer.phar install' from the phpBB directory to do so.\",\n\t"^
"\t\tE_USER_ERROR\n\t\t);\n\t}\n\trequire($phpbb_root_path"^
" . 'vendor/autoload.php');\n}") 
"require($phpbb_root_path . 'vendor/autoload.php');\n";;

expand_inclusion
("","require($phpbb_root_path . 'vendor/autoload.php');")
"vendor/autoload.php";;  

special_replace 
("","\nreturn ComposerAutoloaderInit4bd4e7054c34cf3fee5cb753339be372::getLoader();")
"\n$loader = ComposerAutoloaderInit4bd4e7054c34cf3fee5cb753339be372::getLoader();";;

expand_inclusion
("","require_once 'vendor' . '/composer' . '/autoload_real.php';")
"vendor/composer/autoload_real.php";;  

let partial1=
"spl_autoload_register(array('ComposerAutoloaderInit4bd4e7054c34cf3fee5cb7"^
"53339be372', 'loadClassLoader'), true, true);\n        self::$loader = "^
"$loader = new \\Composer\\Autoload\\ClassLoader();\n        spl_"^
"autoload_unregister(array('ComposerAutoloaderInit4bd4e7054c34cf3fee5cb7"^
"53339be372', 'loadClassLoader'));\n\n        $map = require 'vendor/"^
"composer' . '/autoload_namespaces.php';\n        foreach ($map "^
"as $namespace => $path) {\n            $loader->set($namespace, "^
"$path);\n        }\n\n        $map = require 'vendor/composer' . '/au"^
"toload_psr4.php';\n        foreach ($map as $namespace => $path) {\n"^
"            $loader->setPsr4($namespace, $path);\n        }\n\n        "^
"$classMap = require 'vendor/composer' . '/autoload_classmap.php';\n        "^
"if ($classMap) {\n            $loader->addClassMap($classMap);\n        }\n"^
"\n        $loader->register(true);\n\n        $includeFiles = require 'ven"^
"dor/composer' . '/autoload_files.php';\n        foreach ($includeFiles as "^
"$fileIdentifier => $file) {\n            composerRequire4bd4e7054c34cf3fee5cb7"^
"53339be372($fileIdentifier, $file);\n        }\n";;

let partial2=
  "public static function getLoader()\n    {\n        if (null !== se"^
  "lf::$loader) {\n            return self::$loader;\n        }\n\n        "^
  partial1^
  "\n        return $loader;\n    }\n";;

special_replace 
  ("","$loader = ComposerAutoloaderInit4bd4e7054c34cf3fee5cb753339be372::getLoader();")
  partial1;;  

special_replace
  (
    "",
    (
    "\nspl_autoload_register(array('ComposerAutoloaderInit4bd4e7054c34cf3fee5cb75"^
    "3339be372', 'loadClassLoader'), true, true);\n        self::$loader = $loader ="
    )
  )
  (
    "\n        spl_autoload_register(array('ComposerAutoloaderInit4bd4e7054c34cf3fee5cb75"^
    "3339be372', 'loadClassLoader'), true, true);\n        $loader ="
    );;

special_replace ("",partial2) "";;


special_replace 
 ("",
   (
    "\n        spl_autoload_register(array('ComposerAutoloaderInit4bd4e705"^
    "4c34cf3fee5cb753339be372', 'loadClassLoader'), true, true);\n        $lo"^
    "ader = new \\Composer\\Autoload\\ClassLoader();\n        spl_autoload_un"^
    "register(array('ComposerAutoloaderInit4bd4e7054c34cf3fee5cb753339be3"^
    "72', 'loadClassLoader'));"
   )
 )
 (
   "\n        include('vendor/composer/ClassLoader.php');\n"^
   "\n        $loader = new \\Composer\\Autoload\\ClassLoader();\n"
 )
;;

special_replace 
("",
("class ComposerAutoloaderInit4bd4e7054c34cf3fee5cb753339be372\n{\n    pri"^
"vate static $loader;\n\n    public static function loadClassLoader($cla"^
"ss)\n    {\n        if ('Composer\\Autoload\\ClassLoader' === $cla"^
"ss) {\n            require 'vendor/composer' . '/ClassLoader.ph"^
"p';\n        }\n    }\n\n    }")
)
"";;

let autoload_files=
  [
  "0e6d7bf4a5811bfa5cf40c5ccd6fae6a" , "symfony/polyfill-mbstring/bootstrap.php";
  "e40631d46120a9c38ea139981f8dab26" , "ircmaxell/password-compat/lib/password.php";
  "edc6464955a37aa4d5fbf39d40fb6ee7" , "symfony/polyfill-php55/bootstrap.php";
  "3e2471375464aac821502deb0ac64275" , "symfony/polyfill-php54/bootstrap.php";
  "ad155f8f1cf0d418fe49e248db8c661b" , "react/promise/src/functions_include.php";
  "5255c38a0faeba867671b61dfda6d864" , "paragonie/random_compat/lib/random.php";
  ];;

let command_for_autoload_file (fileIdentifier,fn)=
"require 'vendor/"^fn^"';\n\n"^
"$GLOBALS['__composer_autoload_files']['"^fileIdentifier^"'] = true;\n"  ;;


let commands_for_autoload_files =
    let temp1=Image.image command_for_autoload_file autoload_files in
    String.concat "\n" temp1;;

special_replace 
    ("",
    ("        $includeFiles = require 'vendor/composer' . '/autoload_fil"^
    "es.php';\n        foreach ($includeFiles as $fileIdentifier => $fil"^
    "e) {\n            composerRequire4bd4e7054c34cf3fee5cb753339be372($fi"^
    "leIdentifier, $file);\n        }\n")
    )
    commands_for_autoload_files;;    

special_replace 
    ("",
    ("function composerRequire4bd4e7054c34cf3fee5cb753339be372($fileIdentifi"^
    "er, $file)\n{\n    if (empty($GLOBALS['__composer_autoload_files'][$file"^
    "Identifier])) {\n        require $file;\n\n        $GLOBALS['__composer_a"^
    "utoload_files'][$fileIdentifier] = true;\n    }\n}")
    )
    "";;      

expand_inclusion
    ("","        include('vendor/composer/ClassLoader.php');")
    "vendor/composer/ClassLoader.php";;  

special_replace 
    (
    "function includeFile($file)\n{\n    ",
    "include $file;"
    )
    "enbarzan(1,$file);";;   

expand_inclusion
    ("","require($phpbb_root_path . 'includes/utf/utf_tools.' . $phpEx);")
    "includes/utf/utf_tools.php";;  


*)

(*

let text1=(mf ());;
let tag1="private $prefixLengthsPsr4 = array();";;
let i1=(List.hd(oc tag1))+(String.length tag1);;

let text2=(Cull_string.beginning i1 text1)^"\n\n}\n}\n\n\n";;

let bad1=Put_markers_everywhere.in_string text2;;
let temp1=Cnspc.decompose text2;;
let bad2=Put_markers_everywhere.high_level_helper ([],temp1);;

let (graet1,da_ober1)=([],temp1);;
let (dec_content1,nspc_name1,nspc_content1)::peurrest1=da_ober1;;
let marked_content1=Put_markers_everywhere.in_namespace nspc_content1;;
let temp11=Cnspc.rewrite_item (dec_content1,nspc_name1,marked_content1);;


let (graet2,da_ober2)=(temp11::graet1,peurrest1);;
let (dec_content2,nspc_name2,nspc_content2)::peurrest2=da_ober2;;
let bad3=Put_markers_everywhere.in_namespace nspc_content2;;



let ff (mark_count,line_count,idx_start,idx,s,n,accu)=
  if idx>n
  then failwith("Brigandes")
  else 
  if Substring.is_a_substring_located_at "/*" s idx
  then let j=Substring.leftmost_index_of_in_from "*/" s (idx+2) in
       let d=Lines_in_string.number_of_lines_in_char_interval s idx j in
       (mark_count,line_count+d,idx_start,j+2,s,n,accu)
  else 
  if Substring.is_a_substring_located_at "//" s idx
  then let j=Substring.leftmost_index_of_in_from "\n" s (idx+2) in
       (mark_count,line_count+1,idx_start,j+1,s,n,accu)
  else 
  if (Substring.is_a_substring_located_at "<<<EOF\n" s idx)
     ||
     (Substring.is_a_substring_located_at "<<<'EOF'\n" s idx) 
  then let j=Substring.leftmost_index_of_in_from "\nEOF;\n" s (idx+7) in
       let d=Lines_in_string.number_of_lines_in_char_interval s idx (j+5) in
       (mark_count,line_count+d,idx_start,j+6,s,n,accu)
  else
  let c=Strung.get s idx in
  if c='\n'
  then (
         if Substring.is_a_substring_located_at ";" s (idx-1)
         then let marker_line=
               "marker_here("^(string_of_int(mark_count+1))^
               ","^(string_of_int (line_count+2))^");\n" in
              let elt=
               (Cull_string.interval s idx_start idx)^marker_line in
               (mark_count+1,line_count+2,idx+1,idx+1,s,n,elt::accu)
         else  (mark_count,line_count+1,idx_start,idx+1,s,n,accu)     
       )
  else
  if c='{'
  then let j=After.after_closing_character ('{','}') s (idx,0) in
       let d=Lines_in_string.number_of_lines_in_char_interval s idx j in
        (mark_count,line_count+d,idx_start,j,s,n,accu)
  else  (mark_count,line_count,idx_start,idx+1,s,n,accu);;


let v0=
(0,0,1,1,nspc_content2,String.length nspc_content2,[]);;

let gg=Memoized.small ff v0;;


*)

(*
let (lchar,rchar)=('{','}');;
let s=mf();;
let part1=itv s 5708 89547;;
let act1=Replace_inside.replace_inside_file (part1,"") main_file;;

let s=mf();;
let n=String.length s;;


let tempf=(
    fun (k,count)->
      if k>n
      then raise(After.Unbalanced_expression(lchar,rchar))
      else 
      if Substring.is_a_substring_located_at "/*" s k
      then let j=Substring.leftmost_index_of_in_from "*/" s (k+2) in
           (j+2,count)
      else 
      if (Substring.is_a_substring_located_at "<<<EOF\n" s k)
         ||
         (Substring.is_a_substring_located_at "<<<'EOF'\n" s k) 
      then let j=Substring.leftmost_index_of_in_from "\nEOF;\n" s (k+7) in
           (j+6,count)
      else 
      let c=String.get s (k-1) in
      if c=lchar
      then (k+1,count+1)
      else 
      if c='\''
      then let j=After.after_simple_quoted_string s k in
           (j,count)
      else
      if c='"'
      then let j=After.after_double_quoted_string s k in
           (j,count)
      else     
      if c<>rchar
      then (k+1,count)
      else 
        if count=1
        then failwith("no bug")
        else (k+1,count-1)
  );;



let ff=Memoized.small tempf (308,0);;
let gg k=try (Some(ff k)) with _->None;;
let m1=(Option.find (fun k->gg k=None)(ennig 1 (String.length s)))-1;;

let v1=Ennig.doyle(fun k->fst(ff k)) 1 (m1-1);;
let v2=List.filter (fun j->Strung.get s j='{') v1;;

let v3=List.filter (fun j->Strung.get s j='{') 
  (ennig 1 (String.length s));;
let v4=List.filter (
   fun j->not(List.mem j v2)
) v3;;
*)


(*
let u1=Ennig.doyle(fun k->fst(ff k)) 1 m1;;
let u2=Listennou.universal_delta_list u1;;
let u3=Explicit.image (fun (i,j)->itv s i (j-1)) u2;;
let u4=List.filter (fun t->(String.length t)>1) u3;;
let u5=String.concat "\n\n\n" u4;;
let temp_ap=Absolute_path.of_string 
"/Users/ewandelanoy/Documents/Sites/Rachel/public_html/temp.php";;
let act1=Io.erase_file_and_fill_it_with_string temp_ap u5;;

let u6=List.filter (fun k->snd(ff k)=1) (ennig 1 41192);;
*)




(*

let (left_complement,place)=(
  "",
  "include($phpbb_root_path . 'common.' . $phpEx);"
);;
let fn="common.php";;
let ap=Absolute_path.of_string (s_rachel^fn);;
let l_rep=
  (
    [
       "'__DIR__'","'__D' . 'I' . 'R__'";
       "__DIR__","'"^(Father_and_son.father fn '/')^"'"
    ]@
    balancings
    );;

let bad1=
  Namespacize.expand_inclusion
  (fn,Absolute_path.of_string (s_rachel^fn))
  (left_complement,place)
  main_file
  l_rep
  ;;

let inserted_file=ap;;
let container_file=main_file;;  
let pre_content=
    Replace_inside.replace_several_inside_string l_rep
 (Io.read_whole_file inserted_file);;
let comment_before="\n\n/* Inclusion of "^fn^" starts here */\n\n"
 and comment_after="\n\n/* Inclusion of "^fn^" ends here */\n\n";;
let bad2=Namespacize.Private.insert_at_unique_place_in_string 
                      pre_content
                       (left_complement,place)
                       (Io.read_whole_file container_file) 
                       (comment_before,comment_after);;
let inserted_text=pre_content;;
let container_text=Io.read_whole_file container_file;;
let unique_place=left_complement^place;;
let temp1=Substring.occurrences_of_in unique_place container_text;;
let i1=List.hd(temp1);;
let i=i1+(String.length left_complement);;
let j=i+(String.length place)-1;;
let bad3=Namespacize.Private.insert_at_interval 
        inserted_text (i,j) container_text
        (comment_before,comment_after);;
let n=String.length container_text;;
let bad3=Namespacize.Private.namespace_at_index container_text i;;  
let bad4=After.after_closing_character ('{','}') container_text (308,0);;


let tag1=Namespacize.Private.namespace_computation container_text 297;;
*)


(*

let (lchar,rchar)=('{','}');;
let s="{\"3}5\"}89";;
let n=String.length s;;

let (k,count)=(1,0);;
let bowl1=Substring.is_a_substring_located_at "/*" s k;;
let bowl2=(Substring.is_a_substring_located_at "<<<EOF\n" s k)
||
(Substring.is_a_substring_located_at "<<<'EOF'\n" s k) ;;
let c=String.get s (k-1);;
let bowl3=(c=lchar);;

let (k,count)=(k+1,count+1);;
let bowl1=Substring.is_a_substring_located_at "/*" s k;;
let bowl2=(Substring.is_a_substring_located_at "<<<EOF\n" s k)
||
(Substring.is_a_substring_located_at "<<<'EOF'\n" s k) ;;
let c=String.get s (k-1);;
let bowl3=(c=lchar);;
let bowl4=(c='\'');;
let bowl5=(c='"');;
let j=after_double_quoted_string s k;;


let after_closing_character (lchar,rchar) s=
  let n=String.length s in
  let rec tempf=(
    fun (k,count)->
      if k>n
      then raise(Unbalanced_expression(lchar,rchar))
      else 
      if Substring.is_a_substring_located_at "/*" s k
      then let j=Substring.leftmost_index_of_in_from "*/" s (k+2) in
           tempf(j+2,count)
      else 
      if (Substring.is_a_substring_located_at "<<<EOF\n" s k)
         ||
         (Substring.is_a_substring_located_at "<<<'EOF'\n" s k) 
      then let j=Substring.leftmost_index_of_in_from "\nEOF;\n" s (k+7) in
           tempf(j+6,count)
      else 
      let c=String.get s (k-1) in
      if c=lchar
      then tempf(k+1,count+1)
      else 
      if c='\''
      then let j=after_simple_quoted_string s k in
           tempf(j,count)
      else
      if c='"'
      then let j=after_double_quoted_string s k in
           tempf(j,count)
      else     
      if c<>rchar
      then tempf(k+1,count)
      else 
        if count=1
        then k+1
        else tempf(k+1,count-1)
  ) in
  tempf;;

*)  


(*

let ap1=Absolute_path.of_string
"/Users/ewandelanoy/Documents/html_files/bible_vigouroux/bible_vigouroux_thessaloniciens.html";;
let text1=Io.read_whole_file ap1;;

let s1="<FONT FACE=\"Times New Roman, serif\"><FONT SIZE=6><B>Les &eacute;p&icirc;tres";;
let u1=Substring.occurrences_of_in s1 text1;;

let part1=itv text1 72121 72306;;

let s2="Thessalonique &eacute;tait devenue la";;
let u2=Substring.occurrences_of_in s2 text1;;
let part2=itv text1 216483 216498;;

let text2=itv text1 72122 216482;;
Replace_inside.replace_inside_file (text2,"") ap1;;

let quoted_text=Strung.enclose(String.escaped(mf()));;

let some_text=" let w="^quoted_text^";;\n\n\n"^
"let bad=Put_markers_everywhere.in_string w;;";;

let dbg_file=Absolute_path.of_string "debugged.ml";;

Io.append_string_to_file some_text dbg_file;;

let temp_ap=Absolute_path.of_string (s_rachel^"temp.php");;

let act1=Replace_inside.replace_several_inside_file
    balancings temp_ap;;
let act2=Namespacize.standardize temp_ap;;

let bad=Cnspc.rewrite_file temp_ap;;
  

let text1=Io.read_whole_file temp_ap;;
let line=Lines_in_string.line_at_index text1 430;;

let g1=mf();;
let (i1,i2,g2)=Cnspc.dh_debug g1 ([],8);;
let g3=List.hd g2;;

let i3=fst g3;;
let g3="<?php "^(Cull_string.cobeginning  (i3-1) g1);;
let opt4=Option.seek (Namespacize.Private.test_for_namespace_at_index g3) 
(ennig 1 (String.length g3));;
let i4=Option.unpack opt4;;

let opt5=Option.seek (Namespacize.Private.test_for_namespace_at_index g3) 
(ennig (i4+1) (String.length g3));;
let i5=Option.unpack opt5;;



let g4=Cull_string.beginning (i5-1) g3;;

let ap2=Absolute_path.of_string(s_rachel^"temp.php");;
Io.erase_file_and_fill_it_with_string ap2 g4;;

let text1=g4;;


let individual_compression s=
  let j=Substring.leftmost_index_of_in "}" s in
  if j<1
  then None
  else 
  let temp1=List.rev(ennig 1 (j-1)) in
  let opt=Option.seek (fun k->Strung.get s k='{') temp1 in
  if opt=None
  then None 
  else
  let i=Option.unpack opt in
  Some(
  (Cull_string.interval s 1 (i-1))^" abba "^
  (Cull_string.interval s (j+1) (String.length s)));;

individual_compression "uvw { {xyz} {ab} } cde";;

let rec compress (d,s)=
  match individual_compression s with
  None->(d,s)
  |Some(t)->compress(d+1,t);;

let (d1,see1)=compress (0,text1);;

let see2=Memoized.small (fun s->
Option.unpack (individual_compression s)) text1 98;; 

Io.erase_file_and_fill_it_with_string ap2 see2;;

let gg=Io.read_whole_file ap2;;
let g4=Str.split (Str.regexp_string "\n") gg;;
let g5=Ennig.index_everything g4;;
let peggy (i,j)=
  let temp1=List.filter (fun (k,s)->(i<=k)&&(k<=j)) g5 in
  String.concat "\n" (Image.image snd temp1);;

let g6=Image.image peggy
[(773,782);(856,878);(891,900);(911,914);(928,935);(960,963);(1178,1181)];;
let ff k=
  let temp=List.nth g6 (k-1) in
  let _=print_string("\n\n\n"^temp^"\n\n\n") in
  temp;;

let test_for_first_marker s=
    let opt1=After.after_whites_and_comments s 1 in
    if opt1=None then false else
    let i1=Option.unpack opt1 in
    Substring.is_a_substring_located_at "marker_here(" s i1;;  

*)    

(*

let peggy d=
  let _=Sys.command("cp "^s_rachel^"temp.php "^s_rachel^"iewtopic.php ") in
  let _=Explicit.image(
    fun fn->rrrr_expand_inclusion ("","include('"^fn^"');") fn
 ) (Listennou.big_head d chunk) in
  mt();; 

let generic_final_chunk d=
  let temp1=Image.image(
     fun fn->"\ninclude('"^fn^"');\n"
  ) (Listennou.big_head d (chunk)) in
  String.concat "\n" temp1;;

let peggy d=  
  let mark="marker_here(106,12876);\n" in
  let _=
  Replace_inside.overwrite_between_markers_inside_file
    (Overwriter.of_string "\n\n\n")
  (mark,"$phpbb")
  main_file in
  let _=(
  special_replace 
  (
  "",  
  mark
  )
  ("marker_here(0,0);\n"^(generic_final_chunk d)^"\n\n")) in
  mt();;  


*)

(*
let mark="marker_here(106,12876);\n";;
let command=String.concat "\n" (Image.image
   (fun fn->"\ninclude('"^fn^"');\n") chunk
);;
let act1=rrrr_special_replace ("",mark) (mark^command);;


Explicit.image(
  fun fn->rrrr_expand_inclusion ("","include('"^fn^"');") fn
) (Listennou.big_head 5 chunk);;


let fm=List.nth chunk 4;;
rrrr_expand_inclusion ("","include('"^fm^"');") fm;;
*)
(*
let temp_ap=Absolute_path.of_string (s_rachel^"temp.php");;

let fm=List.nth chunk 4;;
Replace_inside.replace_inside_file
 ("include('"^fm^"');","include('temp.php');") main_file;;

let old_text=Io.read_whole_file temp_ap;; 
Replace_inside.replace_several_inside_file
   balancings temp_ap;;
let new_text=Io.read_whole_file temp_ap;; 

let v1=Str.split (Str.regexp_string "\n") old_text;;
let v2=Str.split (Str.regexp_string "\n") new_text;;
let v3=List.combine v1 v2;;
let v4=Ennig.index_everything v3;;
let v5=List.filter (fun (i,(u,v))->u<>v )v4;;

let w1=Substring.occurrences_of_in "EOF;" old_text;;

let w2=image (fun k->itv old_text k (k+15)) w1;;
*)

(*

short_one
(106, 12876)
chunk;;

let chunk=[
  
];;


short_one
(106, 12876)
chunk;;
*)



(*
let g4=Cull_string.beginning (i1-1) g1;;
*)

(*
let g4="<?php "^(itv g1 349772 (i1-1))^" } }";;

let g5=Cnspc.decompose g4;;
let g6=List.rev g5;;
let ff k=List.nth g6 (k-1);;

let g7=Str.split (Str.regexp_string "\n") gg;;
let g8=Ennig.index_everything g7;;
let g9=List.filter (fun (i,s)->(365<=i)&&(i<=365)) g8;;
let g10=String.concat "\n" (image snd g9);;

let j1=Substring.leftmost_index_of_in "NON_FIRST_CHARS" g1;;
let g11=itv g1 (j1-2000) (j1+000);;
let g12="<?php "^(Cull_string.cobeginning (j1-38) g1);;
let g13=Cnspc.rewrite_string g12;;

let ap2=Absolute_path.of_string(s_rachel^"temp.php");;
Io.erase_file_and_fill_it_with_string ap2 g4;;
*)

(*
let u1=Directory_name.of_string
"/Users/ewandelanoy/Documents/Sites/Mikeal/public_html";;
let u2=More_unix.complete_ls_with_nondirectories_only u1;;
let u3=image (fun ap->
  let s_ap=Absolute_path.to_string ap in
  let stad=Unix.stat s_ap in
  (stad.Unix.st_ctime,Cull_string.cobeginning 54 s_ap)
)u2;;
let u4=ofo(Tidel2.diforchan u3);;
let u5=List.rev u4;;

let u6=List.filter (fun (x,y)->not(Substring.begins_with y "Cache/")
) u5;;


let u4=Max.maximize_it_with_care snd u3;;
let ex1=List.hd(snd u4);;

let shorter_u3=List.filter (fun t->t<>ex1) u3;;
let u5=Max.maximize_it_with_care snd shorter_u3;;
*)


(*


let u1=Int_uple.list_of_pairs 1000;;
let u2=List.filter (fun (x,y)->Gcd.gcd x y=1) u1;;
let u3=Option.filter_and_unpack (fun (x,y)->
    let m=x*x+y*y in    
    let i=isqrt(m) in
    if i*i=m
    then Some((i,(x,y)))
    else None
) u2;;

let u4=[ 3; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71;     73; 79; 83; 89; 97; 101; 103; 107; 109; 113; 127; 131; 137; 139; 149; 151;     
157; 163; 167; 173; 179; 181; 191; 193; 197; 199; 211; 223; 227; 229; 233;
239; 241; 251; 257; 263; 269; 271; 277; 281; 283; 293; 307; 311; 313; 317;
331; 337; 347; 349; 353; 359; 367; 373; 379; 383; 389; 397; 401; 409; 419;
421; 431; 433; 439; 443; 449; 457; 461; 463; 467; 479; 487; 491; 499; 503;
509; 521; 523; 541; 547; 557; 563; 569; 571; 577; 587; 593; 599; 601; 607;
613; 617; 619; 631; 641; 643; 647; 653; 659; 661; 673; 677; 683; 691; 701;
709; 719; 727; 733; 739; 743; 751; 757; 761; 769; 773; 787; 797; 809; 811;
821; 823; 827; 829; 839; 853; 857; 859; 863; 877; 881; 883; 887; 907; 911;
919; 929; 937; 941; 947; 953; 967; 971; 977; 983; 991; 997;
1009; 1013; 1019;
1021; 1031; 1033; 1039; 1049; 1051; 1061; 1063; 1069;1087; 1091; 1093; 1097;   1103; 1109; 1117; 1123; 1129; 1151; 1153; 1163; 1171; 1181; 1187; 1193; 1201;   
1213; 1217; 1223; 1229; 1231; 1237; 1249; 1259; 1277;];;

let is_good x=List.for_all (fun p->(x mod p)>0) u4;;
let u5=List.filter (fun (a,_)->is_good a) u3;;

*)


(*

let (i,j)=(113, 12800) and l=["vendor/ocramius/proxy-manager/src/ProxyManager/GeneratorStrategy/GeneratorStrategyInterface.php"];;

let mark="marker_here("^(string_of_int i)^","^(string_of_int j)^");\n";;
let command=String.concat "\n" (Image.image
   (fun fn->"\ninclude('"^fn^"');\n") l
);;
let act1=rrrr_special_replace ("",mark) (mark^command);;

let bad1=Explicit.image(
    fun fn->rrrr_expand_inclusion ("","include('"^fn^"');") fn
) l;;


let fn=List.hd l;;
let inserted_file=(Absolute_path.of_string (s_rachel^fn));;
let (left_complement,place)=("","include('"^fn^"');");;
let container_file=main_file;;
let l_rep=[
  "'__DIR__'","'__D' . 'I' . 'R__'";
  "__DIR__","'"^(Father_and_son.father fn '/')^"'"
]@
balancings;;


let bad2=Namespacize.expand_inclusion
inserted_file
(left_complement,place)
container_file
l_rep;;


let container_text=Io.read_whole_file container_file;;
let pre_content=
  Replace_inside.replace_several_inside_string l_rep
(Io.read_whole_file inserted_file);;
(*
let bad3=Namespacize.Private.insert_at_unique_place_in_string 
                    pre_content
                     (left_complement,place)
                     container_text;;
*)

let unique_place=left_complement^place;;
let temp1=Substring.occurrences_of_in unique_place container_text;;
let i1=List.hd(temp1);;
let i=i1+(String.length left_complement);;
let j=i+(String.length place)-1;;
(*
let bad4=Namespacize.Private.insert_at_interval pre_content (i,j) container_text;;
*)

let inserted_text=pre_content;;
let n=String.length container_text;;
let bad5=Namespacize.Private.namespace_at_index container_text i;;


*)


(*
let g1=Replace_inside.replace_several_inside_string 
[
  "'__DIR__'","'__D' . 'I' . 'R__'";
  "__DIR__","ladada"
] 
  "abc __DIR__ def '__DIR__' ghi ";;

let ap1=Absolute_path.of_string (s_rachel^"vendor/symfony/yaml/Parser.php");;
let text1=Io.read_whole_file ap1;;
let u1=Str.split (Str.regexp_string "\n") text1;; 
let u2=image (List.nth u1) [144;161;535];;
*)

(*

let individual_compression s=
    let j=Substring.leftmost_index_of_in "}" s in
    if j<1
    then None
    else 
    let temp1=List.rev(ennig 1 (j-1)) in
    let opt=Option.seek (fun k->Strung.get s k='{') temp1 in
    if opt=None
    then None 
    else
    let i=Option.unpack opt in
    Some(
    (Cull_string.interval s 1 (i-1))^" abba "^
    (Cull_string.interval s (j+1) (String.length s)));;

individual_compression "uvw { {xyz} {ab} } cde";;

let rec compress (d,s)=
    match individual_compression s with
    None->(d,s)
    |Some(t)->compress(d+1,t);;

let ap1=Absolute_path.of_string(s_rachel^
"vendor/twig/twig/lib/Twig/Lexer.php");;
let text1=Io.read_whole_file ap1;;

let (i1,see1)=compress (0,text1);;

let see2=Memoized.small (fun s->
Option.unpack (individual_compression s)) text1 74;; 

let ap2=Absolute_path.of_string(s_rachel^"temp.php");;
Io.erase_file_and_fill_it_with_string ap2 see2;;

let u1=Substring.leftmost_index_of_in "const REGEX_DQ_STRING_PART =" see2;;
let u2=Substring.leftmost_index_of_in_from ";" see2 u1;;
let u3=itv see2 u1 u2;;
let u4=String.escaped u3;;
let u5="const REGEX_DQ_STRING_PART = '/[^#\\\"\\\\\\\\\\\\\\\\]*(?:(?:\\\\\\\\\\\\\\\\.|#(?!\\\\{))[^#\\\"\\\\\\\\\\\\\\\\]* )*/As';";;
let u6="const REGEX_DQ_STRING_PART = '/[^#\"\\\\\\\\]*(?:(?:\\\\\\\\.|#(?!\\{))[^#\"\\\\\\\\]* )*/As';";;

*)



(*

let s_rachel="/Users/ewandelanoy/Documents/Sites/Rachel/public_html/";;

let main_file=Absolute_path.of_string (s_rachel^"iewtopic.php");;

let main_text=Io.read_whole_file main_file;;

Private.cnspc_decompose main_text;;

*)

(*

let commands_for_one_more t (i,j) l=
   let chunk="chunk"^(string_of_int t) 
   and mark="marker_here("^(string_of_int i)^","^(string_of_int j)^")" in
   "let "^chunk^"=\n"^
   "[\n"^(String.concat "\n"(Image.image(
     fun s->"     \""^s^"\";"
   ) l))^
   "\n];;\n\n\n"^
   " let command_for_"^chunk^" =\n"^
   "\tString.concat \"\\n\"\n (Image.image \n"^
   "\t(fun fn->\"\\ninclude('\"^fn^\"');\\n\") "^chunk^"\n"
   "\t);;\n\n\n"^
  "special_replace (\"\",\""^mark^";\\n\")\n"^
  "("^mark^
  ("marker_here(0,0);\n"^(command_for_sixth_chunk)^"\n");; 

*)

  
(*

(*
let bad1=expand_inclusion ("","include('"^fmi^"');") fmi;;  
*)

let fn=fmi;;
let ap1=Absolute_path.of_string (s_rachel^fn);;
let (left_complement,place)=("","include('"^fmi^"');");;
let l_rep=["__DIR__","'"^(Father_and_son.father fn '/')^"'"];;
(*
let bad2=Namespacize.rexpand_inclusion
     ap1 (left_complement,place) main_file l_rep;;
*)

let inserted_file=ap1;;
let container_file=main_file;;
let pre_content=
        Replace_inside.replace_several_inside_string l_rep
     (Io.read_whole_file inserted_file);;
(*     
let bad3=Namespacize.Private.insert_at_unique_place_in_string 
                          pre_content
                           (left_complement,place)
                           (Io.read_whole_file container_file);;
*)                           
let inserted_text=pre_content;;
let container_text=Io.read_whole_file container_file;;
let unique_place=left_complement^place;;
let temp1=Substring.occurrences_of_in unique_place container_text;;
let i1=List.hd(temp1);;
let i=i1+(String.length left_complement);;
let j=i+(String.length place)-1;;
(*
let bad4=Namespacize.Private.insert_at_interval inserted_text (i,j) container_text;;
*)

let n=String.length container_text;;
(*
let bad5=Namespacize.Private.namespace_at_index container_text i;;
*)
let s=container_text and j=261322;;
let j1=Substring.leftmost_index_of_in_from "namespace" s j;;

let (nspc_name,nspc_idx,_,right_idx,_)=namespace_computation s j1 in
if nspc_idx=0
then ("",(String.length s)+1)
else try (nspc_name,after_closing_character ('{','}') s (right_idx,0) )
     with
     _->raise(Name_and_end_exn(j));;


(*
special_replace ("",  "marker_here(,);\n")
                ("marker_here(0,0);\ninclude('"^fmi^"');\nmarker_here(0,0);\n");;
*)      


*)


(*
let s_stone_file ="/Users/ewandelanoy/Documents/Sites/Rachel/public_html/stone.txt";; 
let stone_file=Absolute_path.of_string s_stone_file;;

let u1=Io.read_whole_file stone_file;;
let u2=Str.split (Str.regexp_string "\n") u1;;
let u3=image (Cull_string.cobeginning 29)  u2;;
let u4=List.rev u3;;
*)

(*
let s_my_file =
  ("/Users/ewandelanoy/Documents/Sites/Rachel/public_html/"^
  "vendor/zendframework/zend-code/src/Reflection/ParameterReflection.php");;
  
let my_file=Absolute_path.of_string s_my_file;;
let t1=Io.read_whole_file my_file;;
let t2=Namespacize.Private.standardize t1;;
let bad=Namespacize.Private.name_and_end t2 261322;;

let t2=
  "<?php\n/*\nabc*/\n\nnamespace PM\\G\\U;\n\nuse 01;";;

open Namespacize.Private;;
open Namespacize;;

*)

(*
let expand_inclusion
inserted_file
(left_complement,place)
container_file=
   let new_content=Private.insert_at_unique_place_in_string 
                     (Io.read_whole_file inserted_file)
                      (left_complement,place)
                     (Io.read_whole_file container_file) in
   Io.erase_file_and_fill_it_with_string 
      container_file new_content;;

*)


(*


*)


(*
let dir1=Directory_name.of_string
"/Users/ewandelanoy/Documents/Web_Projects/Falchun_all/Control_Tower";;

let u1=More_unix.complete_ls dir1;;
let u2=image (
  fun ap->(ap,Absolute_path.to_string ap)
) u1;;
let u3=List.filter (
fun (ap,s_ap)->(Substring.ends_with s_ap ".txt")
&&
(not(Substring.ends_with s_ap "history.txt"))
) u2;;
let act1=Explicit.image(
   fun (ap,s_ap)->
     Replace_inside.replace_several_inside_file
     [
       "Falchun","OC";
       "falchun","oc";
     ]
     ap
) u3;;
*)

(*

let (left_complement,place)=
("","require($phpbb_root_path . 'vendor/autoload.php');");;
let fn="vendor/autoload.php";;  
let inserted_file=(Absolute_path.of_string (s_rachel^fn));;
let container_file=main_file;;
let l_rep=
  [
    "__DIR__","'"^(Father_and_son.father fn '/')^"'"
  ]
  ;;


let pre_content=
   Replace_inside.replace_several_inside_string l_rep
(Io.read_whole_file inserted_file);;


   let new_content=Private.insert_at_unique_place_in_string 
                     (Io.read_whole_file inserted_file)
                      (left_complement,place)
                     pre_content in
   Io.erase_file_and_fill_it_with_string 
      container_file new_content;;

*)


(*

let s_unveiler="/Users/ewandelanoy/Documents/Bash_scripts/unveil.bash_script";;

let unveiler=Absolute_path.of_string s_unveiler;;

let z1=Io.read_whole_file unveiler;;

Replace_inside.replace_inside_file ("\r","") unveiler;;

*)

(*
exception Bad_exn;;



let past_constant x y=
   if Substring.begins_with y x
   then Cull_string.cobeginning (String.length x) y
   else y;;

let past_whites y=
  let n=String.length y in
  let opt1=Option.seek(fun j->
  not(List.mem (Strung.get y j) [' ';'\n';'\t'])
  )(ennig 1 n) in  
  if opt1=None then y else
  let i1=Option.unpack opt1 in 
  Cull_string.cobeginning (i1-1) y;;

let nspc_list=
    (doyle char_of_int 65 90)@
    (doyle char_of_int 97 122)@
    (doyle char_of_int 48 57)@
    ['\\'];;

let past_nspc y=
      let n=String.length y in
      let opt1=Option.seek(fun j->
      not(List.mem (Strung.get y j) nspc_list)
      )(ennig 1 n) in  
      if opt1=None then y else
      let i1=Option.unpack opt1 in 
      Cull_string.cobeginning (i1-1) y;;

let past_comment y=
  if not(Substring.begins_with y "/*")
  then y
  else 
  let i1=Substring.leftmost_index_of_in_from "*/" y 3 in
  Cull_string.cobeginning (i1+1) y;;

let past_wc y=past_whites(past_comment y);; 

let left_complement x y=Cull_string.beginning 
 ((String.length y)-(String.length x)) y;;

let small_push s=
  let t1=past_constant "<?php" s in
  let t2=past_whites t1 in
  let t3=past_wc t2 in
  past_wc t3;;

let v1=image (fun (a,b)->small_push b) u3;;

let enuum=["abstract class ";"class ";"if (";"interface "];;

let check1=List.filter (
  fun x->List.for_all (fun b->
    not(Substring.begins_with x b)
  ) ("namespace"::enuum)
) v1;;



let push s=
  let t4=small_push s in
  if List.exists (fun t->
    Substring.begins_with t4 t
  ) enuum
  then (left_complement t4 s,"",t4)
  else 
  let t5=past_constant "namespace " t4 in
  let t6=past_nspc t5 in
  let t7=past_whites t6 in
  (left_complement t5 s,left_complement t6 t5,t7)
  ;;

let u4=image (fun (a,b)->
  let (before_nspc,nspc,after_nspc)=push b in
  (a,b,before_nspc,nspc,after_nspc)
) u3;;


let (u5,u6)=List.partition(
   fun (a,b,before_nspc,nspc,after_nspc)->
     nspc=""
) u4;;

let check2=Tidel.diforchan(image (
  fun (_,_,_,_,after_nspc)->Strung.get after_nspc 1
) u6);;    

let rewrite1 (a,b,before_nspc,nspc,after_nspc)=
   if nspc=""
   then before_nspc^"namespace{\n\n\n"^after_nspc^"}"
   else if Strung.get after_nspc 1='{'
        then b
        else before_nspc^nspc^" {\n\n\n"^
             (Cull_string.cobeginning 1 after_nspc)^"}";;



let precaution=image
  (fun (a,_)->
   Sys.command(
     "cp ~/Documents/Sites/Rachel/public_html/"^a^" ~/Documents/Sites/Mara/public_html/"^a
   ))
u3;;

let external_content a=
  let ap=Absolute_path.of_string("~/Documents/Sites/Mara/public_html/"^a) in
  Io.read_whole_file ap;;

let reinitialize()=List.iter (
    fun (a,b,_,_,_)->
    let ap=Absolute_path.of_string("~/Documents/Sites/Rachel/public_html/"^a) in 
    Io.erase_file_and_fill_it_with_string ap (external_content a)
) u4;;   

let abba=;;

let baba=abba;;

let check3=List.filter (fun (a,b)->b<>external_content a) u3;;

let cc ()=Sys.command "rm -rf ~/Documents/Sites/Rachel/public_html/cache/*";;

let act1 j=
  let _=cc () in
  List.iter (
        fun w->
        let (a,b,_,_,_)=w in
        let ap=Absolute_path.of_string("~/Documents/Sites/Rachel/public_html/"^a) in 
        Io.erase_file_and_fill_it_with_string ap (rewrite1 w)
  ) (Listennou.big_head j u4);;   


let act2 j=
    let temp1=Listennou.big_head j u4 in
    List.iter (
      fun w->
      let (a,b,_,_,_)=w in
      let ap=Absolute_path.of_string("~/Documents/Sites/Rachel/public_html/"^a) in 
      Io.erase_file_and_fill_it_with_string ap (rewrite1 w)
   ) temp1;;   

let u7=List.rev u4;;



let aggregate j=
  let temp1=Listennou.big_head j u7 in
  let temp2=image (fun s->past_constant "<?php" (rewrite1 s)) temp1 in
  String.concat "\n\n\n" temp2;;   

let main_ap=Absolute_path.of_string "~/Documents/Sites/Rachel/public_html/iewtopic.php";;

let act3 j=
  let _=cc() in
  let _=Io.erase_file_and_fill_it_with_string 
  (Absolute_path.of_string "~/Documents/Sites/Rachel/public_html/temp.txt")
  "" in
    Replace_inside.overwrite_between_markers_inside_file
   (Overwriter.of_string (aggregate j)) 
   (
     "/* Heterogeneous block starts here */",
     "/* Heterogeneous block ends here */"
   )  
   main_ap
 ;; 

let (a1,_,_,_,_)=List.nth u7 14;;
*)


(*

let w1=Explicit.image (fun (a,b)->
  let n=String.length b in 
  Option.filter_and_unpack(fun j->
     if Cull_string.interval b j (j+1)="__"
     then let m=min n (j+42) in
          Some(a,Cull_string.interval b (j+2) m)
     else None)
  (ennig 1 (n-2)) )
 u3;;
let w2=List.flatten w1;;
let w3=List.filter (
  fun (a,z)->let c=Strung.get z 1 in
  let ic=(int_of_char c) in (65<=ic)&&(ic<=90)
) w2;;
let w4=List.filter(
   fun (a,z)->List.for_all(
     fun b->not(Substring.begins_with z b)
   ) ["CLASS__";"METHOD__";"PM__";"NAMESPACE__";"FUNCTION__";"TRAIT__"]
) w3;;
let w5=ofo(Tidel.diforchan(image fst w4));;
let w6=image (fun a->
("open -a /Applications/TextWrangler.app ~/Documents/Sites/Rachel/public_html/"^a)
) w5;;
let w7=Explicit.image Sys.command w6;;

*)

(*

let h1=image(
  fun (a,b,before_nspc,nspc,after_nspc)->
    nspc
) u4;;

let (v1,v2,v3,v4,v5)=List.hd u4;;
let v6=Cull_string.beginning 500 v2;;
let (before_nspc1,nspc1,after_nspc1)= push v6;;

let bush s=
  let t1=past_constant "<?php" s in
  let t2=past_whites t1 in
  let t3=past_wc t2 in
  let t4=past_wc t3 in
  let t5=past_constant "namespace " t4 in
  let t6=past_nspc t5 in
  let t7=past_whites t6 in
  (t1,t2,t3,t4,t5,t6,t7)
  ;;

let (g1,g2,g3,g4,g5,g6,g7) = bush v6;;  

let u5=List.filter(
  fun (a,b)->
    List.for_all (
      function c->not(Substring.begins_with b c)
    ) ["abstract class ";"class ";"if (";"interface ";"namespace "]
) u4;;



let u6=snd(List.hd u5);;
let u7=Cull_string.beginning 50 u6;;

let (_,_,u7,u8)=dec1 u6;;

*)

(*
let ap1=Absolute_path.of_string(
  "~/Documents/Sites/Rachel/public_html/temp.txt");;
let text1=Io.read_whole_file ap1;;

let u1=Str.split (Str.regexp_string "\n") text1;;
let u2=Image.image(
    fun s->
      let l=Str.split (Str.regexp_string " => ") s in
      (List.nth l 0,List.nth l 1)
) u1;;
let u3=image (fun (_,q)->
   let path="~/Documents/Sites/Rachel/public_html/"^q in
   let ap=Absolute_path.of_string path in
   (q,Io.read_whole_file ap)
   )u2;;
*)

(*

*)

(*
let ap1=Absolute_path.of_string(
  "Remembered/aztec.ml");;
  let text1=Io.read_whole_file ap1;;

let n1=String.length text1;;  

let u1=Substring.occurrences_of_in "sql_query" text1;;
let u2=Substring.occurrences_of_in "->sql_query" text1;;
let u4=Substring.occurrences_of_in "// sql_query" text1;;
let u5=Substring.occurrences_of_in "$sql_query" text1;;

let test i=
  (List.mem (i-2) u2)
  ||
  (List.mem (i-3) u4)
  ||
  (List.mem (i-1) u5)
  ;;
let u3=List.filter (fun i->not(test i)) u1;;  

let i1=List.nth u3 1;;
let a1=max(1)(i1-50);;
let b1=min(n1)(i1+500);;
let text2=Cull_string.interval text1 a1 b1;;

  let act1=Replace_inside.replace_inside_file
  ("tribunem@a2ss23.a2hosting.com","myusername@amywebhost.com")
  ap1;;
  
  let act2=Replace_inside.replace_inside_file
  ("sauvegarde.micael@gmx.com","destmail@gmx.com")
  ap1;;
  
  let act3=Replace_inside.replace_inside_file
  ("@amywebhost","@mywebhost")
  ap1;;

  let act4=Replace_inside.replace_inside_file
  ("a2hosting|x-authuser|tribunem","mywebhost|x-authuser|myusername")
  ap1;; 
   
  let act5=Replace_inside.replace_inside_file
  ("sauvegarde@tribunemicael.net","sourcemail@mydomain.net")
  ap1;;  
  
  let act6=Replace_inside.replace_inside_file
  ("a2hosting","mywebhost")
  ap1;;  

  let act7=Replace_inside.replace_inside_file
  ("tribunem","myusername")
  ap1;;  
*)


(*

let big_m=power 10 7;;    
let u1=Ennig.doyle (power 3) 0 14;;
let u2=Ennig.doyle (power 4) 0 11;;
let u3=Cartesian.product u1 u2;;
let u4=Option.filter_and_unpack (fun (x,y)->let z=x*y in
 if z<=big_m then Some(z) else None) u3;;
let o_u4=Tidel.diforchan u4;;
let level1=ofo(o_u4);;

let dec2=Memoized.make(fun x->
    let temp1=Prepared.filter (fun y->y<x) level1 in
    Option.filter_and_unpack(
       fun a->let b=x-a in
       if (Tidel.elfenn b o_u4)&&(a<=b)
       then Some(a,b)
       else None
    ) temp1);;

let dec3=Memoized.make(fun x->
    let temp1=Prepared.filter (fun y->y<x) level1 in
    let temp2=Image.image(
       fun a->Image.image (fun (b,c)->(a,b,c)) (dec2(x-a)) 
    ) temp1 in
    let temp3=List.flatten temp2 in
    List.filter (fun (a,b,c)->a<=b) temp3
    );;

let dec4=Memoized.make(fun x->
    let temp1=Prepared.filter (fun y->y<x) level1 in
    let temp2=Image.image(
       fun a->Image.image (fun (b,c,d)->(a,b,c,d)) (dec3(x-a)) 
    ) temp1 in
    let temp3=List.flatten temp2 in
    List.filter (fun (a,b,c,d)->a<=b) temp3
    );;    

let dec5=Memoized.make(fun x->
    let temp1=Prepared.filter (fun y->y<x) level1 in
    let temp2=Image.image(
       fun a->Image.image (fun (b,c,d,e)->(a,b,c,d,e)) (dec4(x-a)) 
    ) temp1 in
    let temp3=List.flatten temp2 in
    List.filter (fun (a,b,c,d,e)->a<=b) temp3
    );;    

let ff n=
    let temp1=dec4(power 3 n) in
    List.filter (fun (a,b,c,d)->Gcd.gcd_for_many [a;b;c;d]=1) temp1;;

let gg n=
      let temp1=dec5(power 3 n) in
      List.filter (fun (a,b,c,d,e)->Gcd.gcd_for_many [a;b;c;d;e]=1) temp1;;




let u5=Cartesian.product level1 level1;;
let u6=List.filter (fun (u,v)->u<=v) u5;;
let u7=Explicit.image (fun (x,y)->(x+y,(x,y))) u6;;
let u8=ofo(Tidel2.diforchan u7);;
let u9=ofo(Tidel.diforchan (image fst u8));;
let level2=Explicit.image (
   fun i->(i,Option.filter_and_unpack (
      fun (j,p)->if j=i then Some(p) else None
   ) u8)
) u9;;


let u10=Cartesian.product level1 level2;;
let u11=Explicit.image (
   fun (u,(v,lv))->
     let temp=Option.filter_and_unpack 
     (fun (b,a)->if u<=b then Some(u,b,a) else None) lv in
     if temp=[]
     then None
     else Some(u+v,Tidel.diforchan temp)
) u10;;
let u12=Option.filter_and_unpack (fun t->t) u11;;
let u13=ofo(Tidel2.diforchan u12);;
let u14=ofo(Tidel.diforchan (image fst u13));;
let level3=Explicit.image (
   fun i->
     let temp=Option.filter_and_unpack (
      fun (j,p)->if j=i then Some(p) else None
     ) u13 in
   (i,ofo(Tidel.big_teuzin temp))
) u14;;

*)



(*
let u1=ennig (-5) (5);;

let three=Rational.of_int 3;;
let four=Rational.of_int 4;;

let u2=image (Rational.pow three) u1;;


let ap1=Absolute_path.of_string(
"Remembered/aztec.ml");;
let text1=Io.read_whole_file ap1;;

let act1=Replace_inside.replace_inside_file
("\204\129","\195\169")
ap1;;

let act2=Replace_inside.replace_inside_file
("e\195\169","\195\169")
ap1;;

let act3=Replace_inside.replace_inside_file
("e\204\128","\195\168")
ap1;;

let passwd="oxGgHgYrjyVBxsGZ12@";;



let fan s=
    let ap=Absolute_path.of_string
    ("/Users/ewandelanoy/Documents/Web_Projects/Online/"^
    "Prepare_online/"^s) in
    Localized_html.enforce_localized_file ap;;

fan "Persistent_example/localized_indexview.php";;
fan "Persistent_example/localized_createview.php";;
fan "Persistent_example/localized_deleteview.php";;
fan "Persistent_example/localized_editview.php";;

let s1="Persistent_example/localized_createview.php";;
let ap1=Absolute_path.of_string("/Users/ewandelanoy/Documents/Web_Projects/Online/"^
"Prepare_online/"^s1);;
let text1=Io.read_whole_file ap1;;
let (tag1,tag2)=Localized_html.parse_localized_text text1;;

let z1=['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O';                                        'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'; 'a'; 'b'; 'c'; 'd';                                       
'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's';
't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'];;

let n1=List.length z1;;

let random_char ()=List.nth z1 (Random.int n1);;

let z2=Ennig.doyle (fun _->random_char ()) 1 16;;

let z3=Strung.implode z2;;

let rpd=Php_named_recognizer.replace_dependencies;;
let rp=rpd "statement";;
let ei=Php_named_recognizer.erase_item;;
let si=Php_spider.see_item;;
let ai=Php_named_recognizer.analize_item;;
let aai=Php_named_recognizer.analize_all;;
let st ()=si "statement";;

let u1= Manage_lexed_data.get_data ["symblog";"phpbb"];;

let walker=ref(u1);;
let walk ()=(walker:=Php_named_recognizer.clean_lily "statement" (!walker));;
let see()=Php_molecularize.molecularize(List.hd(!walker));;


walk();see();;

*)

(*

rpd "statement" []
[
 "if () {} vvar = id () ;"
];;

echo 'mail content' | mailx -s 'email subject' -A 27_09_2017.sql.zip ewan.delanoy@gmx.fr

echo "ghijkl" | mailx -s "Test attachement" -a peggy.txt -S from="sauvegarde@tribunemicael.net" ewan.delanoy@zoho.com


Php_named_recognizer.eat "include_like _l_ stringy _r*_ ;" "include_once id . sqs ;";;


let h1=List.hd (see());;
let (_,(h2,_))=h1;;
let s_ap1=h2.Lexing.pos_fname;;
let ap1=Absolute_path.of_string s_ap1;;

let text1=Php_lexer.parse_file ap1;;
let short_text1=Php_molecularize.molecularize text1;;

let text2=Listennou.big_tail 72 text1;;
let tag1=image fst text2;;
let tag2=Ennig.index_everything tag1;;
let tag3=List.nth tag1 24;;
let (`Token(tag4,_))=tag3;;

let w1=Php_lexer.parse_string "<?php endif ?>";;
let w2=Php_lexer.parse_string "<?php endif?>";;


let ff x=
    let (_,_,y)=
       Option.unpack(Php_named_recognizer.basic_parser "statement" x) in
    y;;
let gg=Memoized.small ff text1;;



let h2=Php_named_recognizer.basic_parser "statement" text1;;

rpd "statement" []
[
  "vvar assign id . sqs ;";
];;


let z1=aai();;
let z2=List.hd z1;;

rpd "beheaded_ivy" []
[
  "vvar = new id () ;";
];;

rpd "statement" [36]
[
  
];;

*)

(*

Sys.command
("ssh -p 7822 tribunem@tribunemicael.net "^
"\"rm -f ~/webexamples.tribunemicael.net/file_one.txt\"");;


let dir1=Directory_name.of_string "Remembered/Tests";;   
let g1=Directory_summary.compute dir1;;
let g2=Directory_summary.ocaml_description g1;;
print_string ("\n\n\n let g3="^g2^";;\n\n\n");;

let data1=(7822,
"tribunem@tribunemicael.net",
"~/webexamples.tribunemicael.net");;

let dir1=Directory_name.of_string
"/Users/ewandelanoy/Documents/Web_Projects/Online/";;

let state1=Directory_summary.compute dir1;;
let state2=Directory_summary.compute dir1;;
let res1=
  Directory_summary.Private.commands_for_remote_update
    data1 (state1,state2);;
let res2=
  Directory_summary.do_remote_update
    data1 (state1,state2);;
let state3=Directory_summary.compute dir1;;
let res3=
  Directory_summary.Private.commands_for_remote_update
    data1 (state2,state3);;
let res4=
  Directory_summary.do_remote_update
    data1 (state2,state3);;



let rpd=Php_named_recognizer.replace_dependencies;;
let rp=rpd "statement";;
let ei=Php_named_recognizer.erase_item;;
let si=Php_spider.see_item;;
let aai=Php_named_recognizer.analize_all_items;;
let st ()=si "statement";;

let u1= Manage_lexed_data.get_data ["symblog";"phpbb"];;

let walker=ref(u1);;
let walk ()=(walker:=Php_named_recognizer.clean_lily "statement" (!walker));;
let see()=Php_molecularize.molecularize(List.hd(!walker));;


walk();;

Php_named_recognizer.eat "include_like _l_ stringy _r*_ ;" "include_once id . sqs ;";;

rpd "statement" []
[
  "end_of_file";
];;

rpd "beheaded_ivy" []
[
  "vvar = new id () ;";
];;

rpd "statement" [36]
[
  
];;

*)

(*

let update()= image Unix_command.hardcore_uc
[
  (
  "scp -P 7822 "^
  "tribunem@tribunemicael.net:webexamples.tribunemicael.net/Bootstrap_again/index.html "^
  "/Users/ewandelanoy/Documents/OCaml/Ordinary/Remembered/Online/"
  );
  (
    "scp -P 7822 "^
    "tribunem@tribunemicael.net:webexamples.tribunemicael.net/Bootstrap_again/old_index.html "^
    "/Users/ewandelanoy/Documents/OCaml/Ordinary/Remembered/Online/"
  );
];;

let rf x=Io.read_whole_file(Absolute_path.of_string x);;

let g1=rf "/Users/ewandelanoy/Documents/OCaml/Ordinary/Remembered/Online/index.html";;
let g2=rf "/Users/ewandelanoy/Documents/OCaml/Ordinary/Remembered/Online/old_index.html";;

let m=min(String.length g1)(String.length g2);;
let d=Option.find(fun j->(String.get g1 (j-1))<>(String.get g2 (j-1))) (ennig 1 m);;
let a=max(1)(d-10);;
let b=min(m)(d+10);;
let see=(itv g1 a b,itv g2 a b);;

rpd "after_var_in_assignable" []
[
  "-> id ()";
];;

rpd "statement" [34]
[
  "vvar assign vvar _l_ after_var_in_assignable _r?_ ;";
];;

*)

(*
rpd "beheaded2_iwy" [-1]
[
  "if () : _l_ no_ivies _r*_ beheaded3_iwy";
];;

rpd "beheaded3_iwy" []
[
  "else : _l_ no_ivies _r*_ endif ; _l_ no_ivies _r*_";
  "endif ; _l_ no_ivies _r*_";
];;



rpd "beheaded_iwy" [1;2;3]
[
  "no_ivies _l_ no_ivies _r*_ beheaded2_iwy";
];;

rpd "beheaded2_iwy" []
[
  "if () : _l_ no_ivies _r*_ else : _l_ no_ivies _r*_ endif ; _l_ no_ivies _r*_";
  "if () : _l_ no_ivies _r*_ endif ; _l_ no_ivies _r*_";
];;


rpd "assignable" []
[
  "vvar _l_ after_var_in_assignable _r?_";
];;

rpd "after_var_in_assignable" []
[
  ". sqs";
  "= sqs";
  "[ sqs ]"
];;

rpd "assignable" [12;13;14]
[
  "sqs _l_ after_sqs_in_assignable _r?_";
];;

rpd "after_sqs_in_assignable" []
[
  ". vvar . dqs . vvar -> id () . dqs . vvar -> id () . dqs";
  ". vvar . sqs";
];;


rpd "assignable" [4;5;6]
[
  "id () _l_ after_id_paren_in_assignable _r?_";
];;

rpd "after_id_paren_in_assignable" []
[
  "?: no_semicolon";
  ". sqs";
];;


rpd "beheaded_ivwy" [2;3]
[
  "{} else {}";
  "{} else if () {} _l_ else if () {} _r*_ else {}";
  "{} if () {} include_like id () . sqs ;";
];;

rpd "beheaded_ivy" []
[
  "else {}";
  "else if () {} _l_ else if () {} _r*_ else {}";
  "if () {} include_like id () . sqs ;";
];;

rpd "beheaded_ivwy" [2;3;4]
[
  "{} beheaded_ivy"
];;
*)


(*
open Php_molecularize.Private;;

let v0=([],u2);;
let ff=Memoized.small pusher_for_molecularization v0;;
*)

(*

let z1=Php_constructible_recognizer.chain [];;
let z2=Php_constructible_recognizer.head_tail_decomposition z1;;

let rpd=Php_named_recognizer.replace_dependencies;;
let rp=rpd "statement";;
let ei=Php_named_recognizer.erase_item;;
let si=Php_spider.see_item;;
let ay ()=List.hd(Php_named_recognizer.analize_item "statement");;
let st ()=si "statement";;

rp [30] ["vvar [ int_or_string_or_var ] = & assignable ;"];;
rp [31] ["vvar [ int_or_string_or_var ] = no_ampersand assignable ;"];;
rp [34] ["vvar assign no_ampersand assignable ;"];;

let g1=Php_projected_token_set.Private.setminus
   Php_projected_token_set.Private.whole 
    (Php_projected_token_set.Private.from_list [
       Php_projected_token.constant (`T_AMPERSAND)
    ]);;

rp [30;31] ["vvar -> id_or_var () beheaded_varan"];;
rpd "beheaded_varan" []
 [";";
  "-> id_or_var optional_pblock  _l_ -> id_or_var optional_pblock  _r*_ ;"];;

                      rp [17] ["namespace  id {}";"namespace  nmspc {}"];;

rp [17] ["namespace id {}"];;
rp [17] ["namespace nmspc {}"];;
rp [17] ["namespace {}"];;
rp [33] ["vvar -> id_or_var _l_ -> id_or_var optional_pblock  _r*_ ;";
         "vvar -> id_or_var () _l_ -> id_or_var optional_pblock  _r*_ ;"];;
rp [31] ["vvar -> id_or_var _l_ ;";
         "vvar -> id_or_var  id_or_var optional_pblock -> id_or_var optional_pblock  _r*_ ;"];;

rpd "beheaded_ivwy" []
 [
   ": beheaded_iwy endif ;";
   "{} _l_ else if () {} _r*_ else {}"
 ];;
rp [14;15] ["if () beheaded_ivwy"];;

rpd "beheaded_ivwy" [1;4] [];;

rp [12;13] [];;

rpd "beheaded_ivyw" [] [];;

Php_spider.erase_item "beheaded_ivyw";;
*)

(*
Php_named_recognizer.replace_dependencies
"statement"
[12]
[
 "id :: id () -> id () _l_ -> id () _r*_  ;";
]
;;

Php_named_recognizer.replace_dependencies
"beheaded_droid"
[1]
[
 "-> id () _l_ -> id () _r*_  ;";
]
;;

Php_named_recognizer.replace_dependencies
"statement"
[]
[
 "id :: id () -> id () _l_ -> id () _r+_  ;";
 "id :: id =  assignable ;"]
;;

Php_named_recognizer.add_dependencies
("beheaded_droid",
  ["-> id () _l_ -> id () _r+_  ;";";"]
);;

Php_named_recognizer.add_dependencies
("statement",
  ["id :: id () beheaded_droid"]
);;


Php_named_recognizer.Private.remove_idependencies
("statement",[12]);;  

open Php_named_recognizer.Private;;

let (x,l)=("statement",[12]);;  
let nr1=of_name x;;
let l1=nr1.elements;;
let l2=Image.image (fun j->
  let nr2=List.nth l1 (j-1) in nr2.definition) l;;

remove_dependencies (x,l2);;  

Php_named_recognizer.replace_dependencies
  "statement"
  [13;14]
  [
   "id :: id () -> id () _l_ -> id () _r+_  ;";
   "id :: id =  assignable ;"]
  ;;

Php_named_recognizer.Private.remove_idependencies
 ("statement",[14;16]);;  

 Php_named_recognizer.Private.remove_dependencies
 ("statement",[
  "id :: id () _l_ -> id () _r+_  ;";
  "id :: id_or_var =  assignable ;"
 ]);;   


Php_named_recognizer.add_dependencies
 ("beheaded_foreach",
   [":  _l_ no_breach _r*_  endforeach ;";"{}"]
 );;

Php_named_recognizer.add_dependencies
 ("statement",
   ["foreach () beheaded_foreach"]
 );;

Php_named_recognizer.remove_dependencies
 ("statement",
   ["foreach () :  _l_ no_breach _r*_  endforeach ;";
    "foreach () {}"]
 );; 
*)


(*

let u1=Php_named_recognizer.Private.of_definition (Some"uvwwyz",None) "ext";;
let u2=u1.Php_named_recognizer.names;;

open Php_named_recognizer.Private;;

let name_options=(Some"uvwwyz",None) and rough_s="ext";;

let s=Cull_string.trim_spaces rough_s;;
  
let temp1=Parenthesed_block.decompose_without_taking_blanks_into_account 
     Php_symbols_for_recognizer_description.all_pairs s;;
let temp2=Image.image (fun (opt,t)->(opt,Cull_string.trim_spaces t) ) temp1;;
let temp3=List.filter (fun (opt,t)->t<>"") temp2;;
let temp4=List.hd temp3;;
let bad1=helper_for_definition_reading name_options temp4;;
         

let tag1=Php_named_recognizer.of_definition None "@ id () ;";;
let tag2=Php_named_recognizer.of_definition None "id :: id () _l_ -> id () _r+_  ;";;

let tag3=tag1.Php_named_recognizer.unnamed_content;;
let tag4=tag2.Php_named_recognizer.unnamed_content;;

let (tag5,tag6)=Php_constructible_recognizer.big_head_tail_decomposition tag3;;
let (tag7,tag8)=Php_constructible_recognizer.big_head_tail_decomposition tag4;;

open Php_named_recognizer.Private;;


let g1=Php_named_recognizer.of_name "statement";;
let g2=g1.Php_named_recognizer.elements;;
let g3=image (fun x->x.Php_named_recognizer.names) g2;;

Php_spider.see_item "statement";;

Php_spider.add_dependencies 
("statement",["echo no_var no_semicolon _r*_ ;"]);;

Php_spider.remove_dependencies 
("statement",["echo _l_ no_semicolon _r*_ ;"]);;

Php_spider.erase_item "beheaded_ivy";;


let z1=Php_named_recognizer.analize_item "statement";;
let z2=List.hd(z1);;

*)

(*
Php_projected_token_set.Private.complement_from_list;;

let g1=Php_named_recognizer.data_in_apparition_order ();;
let g2=List.filter (fun x->x.Php_named_recognizer.is_a_disjunction) g1;;
let g3=List.nth g2 1;;
let g4=g3.Php_named_recognizer.unnamed_content;;
let g5=Option.unpack(Php_constructible_recognizer.disjunction_content g4);;

let g6=Php_disjointness_check.check g5;;

let g7=Php_disjointness_check.Private.start g5;;
let g8=Php_disjointness_check.Private.pusher g7;;
let g9=Php_disjointness_check.Private.pusher g8;;
*)

(*

let g1=Absolute_path.of_string "~/Downloads/comp_num.txt";;
let g2=Io.read_whole_file g1;;
let g3="400  86\\\n3808  87\\\n3809  88\\\n3810  89\\\n4782  90\\\n4779  91\\\n4781  92\\\n4785  93\\\n3816  94\\\n3815  95\\\n3812  96\\\n4786  97\\\n5852  98\\\n3821  99\\\n3805  100\\\n4980  101\\\n432  102\\\n3806  103\\\n434  104\\\n433  105\\\n5940  106\\\n5934  107\\\n5901  108\\\n431  109\\\n4985  110\\\n5559  111\\\n4986  112\\\n3804  113\\\n3803  114\\\n5855  115\\\n4979  116\\\n4745  117\\\n231  118\\\n4982  119\\\n4987  120\\\n3807  121\\\n4918  122\\\n3889  123\\\n3775  124\\\n636  125\\\n192  126\\\n3782  127\\\n3799  128\\\n4968  129\\\n4964  130\\\n430   131\\\n4974  132\\\n4982  133\\\n4965  134\\\n4967  135\\\n4971  136\\\n5912  137\\\n5904  138\\\n3796  139\\\n4969  140\\\n429  141\\\n4880  142\\\n176  143\\\n3794  144\\\n3800  145\\\n5931  146\\\n3801  147\\\n3797  148\\\n425  149\\\n4958  150\\\n426  151\\\n422  152\\\n424  153\\\n4957  154\\\n4959  155\\\n3802  156\\\n5684  157\\\n5685  158\\\n194  159\\\n380  160\\\n5550  161\\\n5937  162\\\n411  163\\\n401  164\\\n414  165\\\n5548  166\\\n4922  167\\\n4923  168\\\n4926  169\\\n4915  170\\\n4928  171\\\n4931  172\\\n3783  173\\\n3830  174\\\n3788  175\\\n3791  176\\\n3786  177\\\n5732  178\\\n3787  179\\\n3795  180\\\n5713  181\\\n5724  182\\\n4945  183\\\n3785   184\\\n3784  185\\\n4937  186\\\n4948  187\\\n4935  188\\\n4936  189\\\n5727  190\\\n5728  191\\\n307  192\\\n1927  193\\\n5729  194\\\n575  195\\\n760  196\\\n5581  197\\\n1386  198\\\n746  199\\\n191   200\\\n394   201\\\n399  202\\\n397  203\\\n404   204\\\n405   205\\\n1396   206\\\n1459  207\\\n81   208\\\n643  209\\\n1931  210\\\n3762  211\\\n622   212\\\n589  213\\\n617  215\\\n803  221\\\n1273  222\\\n3934  223\\\n3141  224\\\n4878  225\\\n2022  226\\\n3983  227\\\n5273  228\\\n1967  229\\\n3834  230\\\n4862  231\\\n4681  232\\\n4737  234\\\n1043  235\\\n1244  236 \\\n1460  237\\\n1389  238\\\n1407   239\\\n1283   240\\\n1301   241\\\n768    242\\\n848    243\\\n859    244\\\n725    245\\\n818    246\\\n1880  247\\\n727    248\\\n713    249\\\n628    250\\\n595    251\\\n627    252\\\n605   253\\\n3840  254\\\n3776   256\\\n4758   257\\\n4650   258\\\n198   259\\\n178   260\\\n4106  261\\\n4800  262\\\n4792   263\\\n228   264\\\n4793   265\\\n199   266\\\n203   267\\\n208   268\\\n367   269\\\n370   270\\\n3833  271\\\n221   272\\\n4804  273\\\n237   274\\\n226   275\\\n2798   276\\\n229   277\\\n232   278\\\n236   279\\\n4821   280\\\n4819  281\\\n4822  282\\\n4824  283\\\n4863  284\\\n4867  285\\\n4865  286\\\n4864  287\\\n239  288\\\n240  289\\\n322  290\\\n323  291\\\n4884  292\\\n4885  293\\\n345   294\\\n347  295\\\n349  296\\\n350  297\\\n352  298\\\n4877  299\\\n4883  300\\\n325   301\\\n326   302\\\n4875  303\\\n4873  304\\\n334   305\\\n257  306\\\n4847  307\\\n4845   308\\\n4851  309\\\n4853  310\\\n4854  311\\\n4855  312\\\n4856  313\\\n378  314\\\n4901  315\\\n4903  316\\\n4836  317\\\n4840  318\\\n4841  319\\\n4837  320\\\n246  321\\\n245  322\\\n4857  323\\\n4858  324\\\n4859  325\\\n4860  326\\\n4861  327\\\n263  328\\\n264  329\\\n265  330\\\n266  331\\\n401  332\\\n300  333\\\n302  334\\\n3750  335\\\n4888  336\\\n4889  337\\\n3822  338\\\n308  339\\\n309  340\\\n256  341\\\n250  342\\\n3779  343\\\n5702  344\\\n3764  345\\\n3777  346\\\n600  348\\\n4771  349\\\n2001  350\\\n2016  351\\\n238  352\\\n4938  353\\\n5597  354\\\n4870  355\\\n4871  356\\\n4872  357\\\n4924  358\\\n1077  359\\\n1885  360\\\n624  361\\\n5939  362\\\n258  363\\\n2015  364\\\n181   365\\\n653   366\\\n497   367\\\n508   368\\\n4834  369\\\n4839  370\\\n4842  371\\\n4834  372\\\n4483  373\\\n327   374\\\n328   375\\\n809  376\\\n4846  377\\\n330  378\\\n338  379\\\n3745  380\\\n3746  381\\\n5741  382\\\n5714  383\\\n4808  384\\\n4816  385\\\n4821  386\\\n4886  387\\\n4905  388\\\n299  389\\\n5518  390\\\n5919 391\\\n5522  393\\\n3778  394\\\n3921  395";;
let g4=Str.split (Str.regexp_string "\\\n") g3;;
let g5=image (fun s->Str.split (Str.regexp "\( \)+") s) g4;;
let g6=image(
    fun l->let tf=(fun i->int_of_string(List.nth l (i-1))) in
    (tf(1),tf(2))
) g5;;
let g7=Ordered.forget_order(Tidel2.diforchan g6);;
let g8=Image.image(
    fun (i,j)->
       let si=string_of_int i
       and sj=string_of_int j in
       let d=10-(String.length si)-(String.length sj) in
       si^(String.make d ' ')^sj
) g7;;
let g9="\n\n\n"^(String.concat "\n" g8)^"\n\n\n";;

*)

(*

let agatha()=
  let uu1= Manage_lexed_data.get_data ["symblog";"phpbb"] in
  let uu2=Explicit.image (fun l->Php_named_recognizer.star_parser 
      (Php_named_recognizer.of_name "statement") l ) uu1 in
  let uu3=Php_named_recognizer.data_in_apparition_order () in
  let uu4=List.rev_map (fun x->x.Php_named_recognizer.unnamed_content) uu3 in
  let uu5=Explicit.image Php_constructible_recognizer.head_tail_decomposition uu4 in
  let uu6=Option.filter_and_unpack 
  Php_constructible_recognizer.disjunction_content uu4 in
  (uu1,uu2,uu3,uu4,uu5,uu6);;

 

let (u1,u2,u3,u4,u5,u6)=Chronometer.it agatha ();;
let see_time = Chronometer.duration_of_last_computation ();;

*)


(*
let h1=Image.image Recreating_tools.encode_cons_rcgzr   g5;;
let rg5=Image.image Recreating_tools.decode_cons_rcgzr   h1;;
*)


(*

#use"php_spider.ml";;
open Private;;
let z1=(!php_ref);;
let z2=Sp(                                    
[("optional_pblock", ["_l_ () _r?_"]);                                                               
("namespace_name", ["_l_ id _u_ nmspc _rd_"])]);;
change_and_remember z2;;
change_and_remember z1;;

*)


(*
let z1=helper_for_rememberance (!(php_ref));;
let z2=print_string z1;;


let z1=Php_spider.Private.helper_for_rememberance (!(Php_spider.Private.php_ref));;
let z2=print_string z1;;
*)

(*
exception Bad_one of string*Php_positioned_token_list.t;;
let prison=ref(None);;
let prison2=ref(None);;
let cpar=Beaver_for_statement.classical_parser;;
let data=Beaver_for_statement.current_data_list;;

let prsr l=
    try Beaver_for_statement.parser l with
    _->
      let _=(prison:=Some(l)) in
      let dt=Option.find 
      (fun tr->try (function _->false)(cpar tr l) with _->true) (!data) in
      let _=(prison2:=Some(dt)) in
      raise(Bad_one(dt.Beaver_for_statement.name,l));;

let agatha()=
      let uu1= Manage_lexed_data.get_data ["symblog";"phpbb"] in
      let uu2=Explicit.image (fun l->Php_parser_homomorphism.star
      prsr l ) uu1 in
      let uu3=Php_named_recognizer.data_in_apparition_order () in
      let uu4=List.rev_map (fun x->x.Php_named_recognizer.unnamed_content) uu3 in
      let uu5=Explicit.image Php_constructible_recognizer.head_tail_decomposition uu4 in
      let uu6=Option.filter_and_unpack 
      Php_constructible_recognizer.disjunction_content uu4 in
      (uu1,uu2,uu3,uu4,uu5,uu6);;

     

let (u1,u2,u3,u4,u5,u6)=Chronometer.it agatha ();;
let see_time = Chronometer.duration_of_last_computation ();;

let v1=List.rev (!(Php_projected_token_set.Private.ref_for_new_names));;  

let g1=Php_named_recognizer.data_in_apparition_order ();;
let g2=List.filter (fun x->x.Php_named_recognizer.is_a_disjunction) g1;;
let g3=List.nth g2 1;;
let g4=g3.Php_named_recognizer.unnamed_content;;
let g5=Option.unpack(Php_constructible_recognizer.disjunction_content g4);;

let h1=Image.image Recreating_tools.encode_cons_rcgzr   g5;;
let rg5=Image.image Recreating_tools.decode_cons_rcgzr   h1;;

let g6=Php_disjointness_check.check g5;;

let w1=Php_named_recognizer.Private.absorb_spider Php_spider.php;;

*)


(*

let u1= Manage_lexed_data.get_data ["symblog";"phpbb"];;

let u2=Explicit.image (fun l->Php_parser_homomorphism.star
prsr l ) u1;;

let u3=Php_named_recognizer.data_in_apparition_order();;
let u4=List.rev_map (fun x->x.Php_named_recognizer.unnamed_content) u3;;
let u5=Explicit.image Php_constructible_recognizer.head_tail_decomposition u4;;





let z1=(!(Beaver_for_statement.current_data_list));;
let z2=image (fun x->
(x.Beaver_for_statement.name,x.Beaver_for_statement.content)) z1;;
*)

(*
let u3=Option.unpack(!prison);;
let elt1=Option.unpack(!prison2);;

let u4=cpar elt1 u3;;

let trmt=Termite.of_string elt1.Beaver_for_statement.content ;;
let u5=Termite.parse trmt u3;;

let u6=Php_lexer.parse_string "<?php $amy->b(1,2)->c(3)->d($u,$v);";;
let u7=Termite.parse trmt u6;;

let g1=Termite.of_string "foreach () : ##( _l_no_breach _r*_ )## endforeach ;";;
let g2=Termite.of_string "foreach () :  _l_no_breach _r*_ endforeach ;";;

let tc x y=
    let trm1=Termite.of_string x
    and trm2=Termite.of_string y
    in (trm1=trm2,trm1,trm2);;

let g1=vfm "Php_constructible_recognizer";;
let g2=List.filter(
   fun (s,l)->String.capitalize_ascii s=s
) g1;;
*)

(*

open Beaver_for_statement;;
let w1=List.filter(fun z->z.catalyser<>"")(!current_data_list);;
let w2=image(
    fun x->x.content
) (!current_data_list);;

*)


(*
let u4=List.hd u1;;

let u5=Listennou.big_head 18 u4;;

let l_data=(!(Php_named_recognizer.Private.data));;
let names=Image.image (fun nr->nr.Php_named_recognizer.name) (l_data) in
let temp1=Strung.longest_match_parsing names "id_or_var";;

let ru5=decode_postok_list [(94, "if", "", 4, 274, 275, 4, 274, 276);                                                               (9, "(", "", 4, 274, 278, 4, 274, 278);                                                              
(1, "!", "", 4, 274, 279, 4, 274, 279);
(93, "isset", "", 4, 274, 280, 4, 274, 284);
(9, "(", "", 4, 274, 285, 4, 274, 285);
(123, "$_SERVER", "", 4, 274, 286, 4, 274, 293);
(51, "[", "", 4, 274, 294, 4, 274, 294);
(115, "HTTP_HOST", "", 4, 274, 295, 4, 274, 305);
(52, "]", "", 4, 274, 306, 4, 274, 306);
(16, ")", "", 4, 274, 307, 4, 274, 307);
(16, ")", "", 4, 274, 308, 4, 274, 308);
(127, "{", "", 4, 274, 310, 4, 274, 310);
(81, "exit", "", 5, 311, 316, 5, 311, 319);
(9, "(", "", 5, 311, 320, 5, 311, 320);
(115, "This script cannot be run from the CLI. Run it from a browser.", "",
 5, 311, 321, 5, 311, 384);
(16, ")", "", 5, 311, 385, 5, 311, 385);
(35, ";", "", 5, 311, 386, 5, 311, 386);
(131, "}", "", 6, 387, 388, 6, 387, 388)];;


let bad=Beaver_for_statement.parser ru5;;

#remove_printer Php_positioned_token_list.print_out;;

*)

(*
let hm=hmx "php_positioned_molecule_list";;
German_wrapper.declare_printer_equipped_type hm;;
*)

(*
let g1=vfm "Php_token";;
let g2=List.assoc "form" g1;;
let g3=(hmx "php_parser")::(bel "php_parser");;
let g4=image (fun hm->
Mlx_ended_absolute_path.to_path(
Mlx_ended_absolute_path.join
  hm Ocaml_ending.ml) )g3;;
let g5=image (fun (x,y)->(x,List.filter (fun z->List.mem z g4) y )) g1;;

let g6=List.filter (fun (x,y)->y<>[]) g5;;


let g4=image fst g3;;
let g5=Ordered.diforchan Dictionary_order.dictionary_order g4;;
*)

(*
let g1=dbel "Php_positioned_token_list";;
let g2=vfm "Php_positioned_token_list";;

let peggy=(fun term->
 let gg3=List.assoc term g2  in
  image (
  Replace_inside.replace_inside_file
  ("Php_positioned_token_list."^term,"List."^term)
) gg3
);;

let act2=Explicit.image peggy 
["hd"; "tl"; "rev"; "cons"; "filter"; "length"]  ;;

let term="big_head";;
let g3=List.assoc term g2;;
let act1=Explicit.image (
    Replace_inside.replace_inside_file
    ("Php_positioned_token_list."^term,"Listennou."^term)
) g3;;


let d1=German_pervasives.fg_without_backup;;
*)


(*
let u1=Manage_lexed_data.force_recompute_data
["symblog";"phpbb"];;

let u1=Manage_lexed_data.get_data
["symblog";"phpbb"];;

let u2=Explicit.image Level_one.level_one u1;;
*)

(*
let u3=List.hd u1;;

let u4=Php_positioned_token_list.big_head 18 u3;;

let u5=u4.Php_positioned_token_list.contained;;

let u6=image Php_positioned_token.unveil u5;;

let (u7,u8)=(image fst u6,image snd u6);;

let u9=image (fun tok->(Php_token.form tok,Php_token.content tok)) u7;;

let u10=image (fun (lx1,lx2)->
(lx1.Lexing.pos_lnum,lx1.Lexing.pos_bol,lx1.Lexing.pos_cnum,
lx2.Lexing.pos_lnum,lx2.Lexing.pos_bol,lx2.Lexing.pos_cnum
)
) u8;;

let ru8=image(
   fun (n1,b1,c1,n2,b2,c2)->
   ({Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = n1; pos_bol = b1; pos_cnum = c1},
 {Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/config.php";
  pos_lnum = n2; pos_bol = b2; pos_cnum = c2})
)
[(4, 274, 275, 4, 274, 276); (4, 274, 278, 4, 274, 278);                                                                                            (4, 274, 279, 4, 274, 279); (4, 274, 280, 4, 274, 284);                                                                                         
(4, 274, 285, 4, 274, 285); (4, 274, 286, 4, 274, 293);
(4, 274, 294, 4, 274, 294); (4, 274, 295, 4, 274, 305);
(4, 274, 306, 4, 274, 306); (4, 274, 307, 4, 274, 307);
(4, 274, 308, 4, 274, 308); (4, 274, 310, 4, 274, 310);
(5, 311, 316, 5, 311, 319); (5, 311, 320, 5, 311, 320);
(5, 311, 321, 5, 311, 384); (5, 311, 385, 5, 311, 385);
(5, 311, 386, 5, 311, 386); (6, 387, 388, 6, 387, 388)];;

let ru9=[(`T_IF, "if"); (`T_LPARENTHESIS, "("); (`T_EXCLAMATION, "!");                                                                                      (`Ident, "isset"); (`T_LPARENTHESIS, "("); (`Variable, "$_SERVER");                                                                               
(`T_LBRACKET, "["); (`Single_quoted, "HTTP_HOST"); (`T_RBRACKET, "]");
(`T_RPARENTHESIS, ")"); (`T_RPARENTHESIS, ")"); (`T_LBRACE, "{");
(`T_EXIT, "exit"); (`T_LPARENTHESIS, "(");
(`Single_quoted,
 "This script cannot be run from the CLI. Run it from a browser.");
(`T_RPARENTHESIS, ")"); (`T_SEMICOLON, ";"); (`T_RBRACE, "}")];;

let ru7=image (fun (x,y)->Php_token.make x y) ru9;;

let ru6=List.combine ru7 ru8;;

let ru5=image (fun (x,y)->Php_positioned_token.make x y) ru6;;

let ru4={Php_positioned_token_list.contained=ru5};;

let bad=Beaver_for_statement.parser ru4;;
*)

(*
let u5=u3.Php_positioned_token_list.contained;;

let u6=image (fun 
   (Php_positioned_token.PPL(x,_))->Php_token.readable x
) u5;;

let u7=String.concat " " u6;;
*)

(*
let g1=dbel "php_positioned_token";;
let g2=image (fun x->x.Half_dressed_module.naked_module) g1;;
*)


(*
module Vegetable=struct
    type t=[`Eggplant | `Leek];;
end;;  

module Fruit=struct
  type t=[`Apple | `Pear];;
end;;  

module Vegetable_or_Fruit=struct
   type t=[Vegetable.t | Fruit.t];;

   let of_vegetable =((fun (x:Vegetable.t)->(x:>t)): Vegetable.t -> t);;

end;;  
*)

(*
let current="Php_projected_token";;
let pointed=current^".";;
let g1=vfm current;;
let g2=image fst g1;;
let g3=List.filter (fun x->
   String.capitalize_ascii x=x
) g2;;
let g4=image (
 fun s->(pointed^s,pointed^""^(String.lowercase_ascii s))
) g3;;

let g5=Explicit.image(
  fun (a,b)->
  German_pervasives.srv_without_backup a b
) g4;;
*)


(*
let current="Php_constant_token";;
let pointed=current^".";;

rv (pointed^"all_punctuators") "all";;

rv (pointed^"all_triples") "data";;

rv (pointed^"to_string") "make_visible";;

rv (pointed^"of_string") "from_visible";;
*)


(*
let current="Php_punctuator";;
let pointed=current^".";;

rv (pointed^"all_punctuators") "all";;

rv (pointed^"all_triples") "data";;

rv (pointed^"to_string") "make_visible";;

rv (pointed^"of_string") "from_visible";;
*)

(*

let current="Php_keyword";;
let pointed=current^".";;

rv (pointed^"all_keywords") "all";;

rv (pointed^"all_pairs") "data";;

rv (pointed^"to_string") "make_visible";;

rv (pointed^"of_string") "from_visible";;

*)

(*
let current="Php_operator";;
let pointed=current^".";;

rv (pointed^"all_operators") "all";;

rv (pointed^"all_fiftuples") "data";;

rv (pointed^"to_string") "make_visible";;

rv (pointed^"of_string") "from_visible";;
*)

(*
let current="Php_punctuator";;
let pointed=current^".";;
let g1=vfm current;;
let g2=image fst g1;;
let g3=List.filter (fun x->
   Substring.begins_with x "T_"
) g2;;
let g4=image (
 fun s->(pointed^s,pointed^(String.lowercase_ascii s))
) g3;;
let g5=Explicit.image(
  fun (a,b)->
  German_pervasives.srv_without_backup a b
) g4;;
*)

(*

let ap=Absolute_path.of_string 
"Php_analizer/php_punctuator.ml";;

let v1=Php_punctuator.all_punctuators ;;

let v2="Php_punctuator.T_LPARENTHESIS; Php_punctuator.T_RPARENTHESIS;                                                                                      Php_punctuator.T_COMMA; Php_punctuator.T_ARROW;                                                                                                 \n   Php_punctuator.T_COLON_COLON; Php_punctuator.T_SEMICOLON;\n   Php_punctuator.T_LBRACE; Php_punctuator.T_RBRACE";;

let v3=Str.global_replace (Str.regexp_string "Php_punctuator.") "" v2;;
let v4=Str.global_replace (Str.regexp_string "\n") "" v3;;
let v5=Str.global_replace (Str.regexp_string " ") "" v4;;
let v6=Str.split (Str.regexp_string ";")  v5;;
let v7=List.combine v6 v1;;

let v8=image (
  fun (x,pkt)->
  let y=String.lowercase_ascii x in
  (y,
  Strung.enclose(Php_punctuator.to_string pkt),
  Strung.enclose(Cull_string.cobeginning 2 y))
) v7;;

let v9=image (fun (s,viz,sn)->
"   ("^s^","^viz^","^sn^");"
) v8;;

let v10=String.concat "\n" v9;;
let uu ()=print_string("\n\n\n let all_triples=[\n  "
^v10^"\n];;\n\n\n");;


let v11=image (
  fun (x,pkt)->
  let y=String.lowercase_ascii x in
  "   let "^y^" = ("^x^");;"
) v7;;

let v12=String.concat "\n" v11;;
let uu ()=print_string("\n\n\n"
^v12^"\n\n\n");;

*)

(*

let string_of_asso=function
Associativity.Left_associative->"left"
|Associativity.Right_associative->"right"
|_->"nonassoc";;

let asso op=
    string_of_asso (Php_operator.associativity op);;

let ap=Absolute_path.of_string 
"Php_analizer/php_operator.ml";;

let v1=image (fun (op,_,_,_)->op)
Php_operator.all_fourtuples ;;

let v2="Php_operator.T_CLONE; Php_operator.T_NEW; Php_operator.T_LBRACKET;                                                                                 Php_operator.T_RBRACKET; Php_operator.T_STAR_STAR; Php_operator.T_PLUS_PLUS;                                                                    \n   Php_operator.T_MINUS_MINUS; Php_operator.T_TILDA;\n   Php_operator.T_COERCE_TO_INT; Php_operator.T_COERCE_TO_FLOAT;\n   Php_operator.T_COERCE_TO_STRING; Php_operator.T_COERCE_TO_ARRAY;\n   Php_operator.T_COERCE_TO_OBJECT; Php_operator.T_COERCE_TO_BOOL;\n   Php_operator.T_AT; Php_operator.T_INSTANCEOF; Php_operator.T_EXCLAMATION;\n   Php_operator.T_STAR; Php_operator.T_DIVIDE; Php_operator.T_PERCENTAGE;\n   Php_operator.T_PLUS; Php_operator.T_MINUS; Php_operator.T_DOT;\n   Php_operator.T_LESS_LESS; Php_operator.T_MORE_MORE; Php_operator.T_LESS;\n   Php_operator.T_LESS_EQUALS; Php_operator.T_MORE; Php_operator.T_MORE_EQUALS;\n   Php_operator.T_EQUALS_EQUALS; Php_operator.T_EXCLAMATION_EQUALS;\n   Php_operator.T_EQUALS_EQUALS_EQUALS;\n   Php_operator.T_EXCLAMATION_EQUALS_EQUALS; Php_operator.T_LESS_MORE;\n   Php_operator.T_AMPERSAND; Php_operator.T_CIRCUMFLEX; Php_operator.T_VLINE;\n   Php_operator.T_AMPERSAND_AMPERSAND; Php_operator.T_VLINE_VLINE;\n   Php_operator.T_QUESTION; Php_operator.T_COLON; Php_operator.T_EQUALS;\n   Php_operator.T_PLUS_EQUALS; Php_operator.T_MINUS_EQUALS;\n   Php_operator.T_STAR_EQUALS; Php_operator.T_STAR_STAR_EQUALS;\n   Php_operator.T_DIVIDE_EQUALS; Php_operator.T_DOT_EQUALS;\n   Php_operator.T_PERCENTAGE_EQUALS; Php_operator.T_AMPERSAND_EQUALS;\n   Php_operator.T_VLINE_EQUALS; Php_operator.T_CIRCUMFLEX_EQUALS;\n   Php_operator.T_LESS_LESS_EQUALS; Php_operator.T_MORE_MORE_EQUALS;\n   Php_operator.T_EQUALS_MORE; Php_operator.T_AND; Php_operator.T_XOR;\n   Php_operator.T_OR";;
let v3=Str.global_replace (Str.regexp_string "Php_operator.") "" v2;;
let v4=Str.global_replace (Str.regexp_string "\n") "" v3;;
let v5=Str.global_replace (Str.regexp_string " ") "" v4;;
let v6=Str.split (Str.regexp_string ";")  v5;;
let v7=List.combine v6 v1;;


let v8=image (
   fun (x,op)->
   let y=String.lowercase_ascii x in
   (String.lowercase_ascii x,
   asso op,string_of_int(Php_operator.precedence op),
   Strung.enclose(Php_operator.to_string op),
   Strung.enclose(Cull_string.cobeginning 2 y))
) v7;;

let v9=image (fun (s,asc,prec,viz,sn)->
"   ("^s^","^asc^","^prec^","^viz^","^sn^");"
) v8;;

let v10=String.concat "\n" v9;;
let uu ()=print_string("\n\n\n let all_fiftuples=[\n  "
^v10^"\n];;\n\n\n");;

let g1=vfm "php_operator";;
let g2=List.filter (fun (s,l)->
   String.capitalize_ascii(s)<>s
) g1;;

*)

(*
let ap=Absolute_path.of_string 
"Php_analizer/php_keyword.ml";;

let s1=Io.read_whole_file ap;;
let j1=Substring.leftmost_index_of_in "T_ABSTRACT;\n" s1;;
let s2=Cull_string.cobeginning (j1-1) s1;;
let j2=Substring.leftmost_index_of_in "YIELD" s2;;
let s3=Cull_string.beginning (j2+5) s2;;
let s4=Str.global_replace (Str.regexp_string ";") "" s3;;
let s5=Str.global_replace (Str.regexp_string " ") "" s4;;
let u1=Str.split (Str.regexp_string "\n") s5;;
let u2=image (fun x->(String.lowercase_ascii x,x)) u1;;
let u3=image (fun (a,b)->
 "let "^a^" = ("^b^") ;;"
) u2;;
let u4=String.concat "\n" u3;;
let uu ()=print_string("\n\n\n"^u4^"\n\n\n");;
let u5=image (fun (a,b)->
"   (\""^(Cull_string.cobeginning 2 a)^"\","^a^");"
) u2;;
let u6=String.concat "\n" u5;;
let uu ()=print_string("\n\n\n let all_pairs=[\n  "
^u6^"\n];;\n\n\n");;
*)


(*
let u1=Manage_lexed_data.get_data
["symblog";"phpbb"];;

let u2=Explicit.image Level_one.level_one u1;;

let u3=Explicit.image Option.unpack u2;;

let u4=Option.filter_and_unpack (fun (a,b,c)->
   if Positioned_php_token_list.is_empty c 
   then None 
   else Some(c)
) u3;;

let u5=List.hd u4;;
*)


(*

Unix_command.remember_commands_mode:=true;;

let z1=List.rev (!(Unix_command.accu));;

*)

(*

let z1=
  [
  "rm -f /Users/Ewandelanoy/Documents/OCaml/Idaho/_build/*.d.cm*";
  "rm -f /Users/Ewandelanoy/Documents/OCaml/Idaho/_build/*.ocaml_debuggable";                                                  
  "ocamlc -bin-annot -g  -I /Users/Ewandelanoy/Documents/OCaml/Idaho/_build str.cma unix.cma  -o /Users/Ewandelanoy/Documents/OCaml/Idaho/debugged.d.cmo -c /Users/Ewandelanoy/Documents/OCaml/Idaho/debugged.ml";
  "mv /Users/Ewandelanoy/Documents/OCaml/Idaho/debugged.d.cm* /Users/Ewandelanoy/Documents/OCaml/Idaho/_build/";
  "ocamlc -bin-annot -g  -I /Users/Ewandelanoy/Documents/OCaml/Idaho/_build str.cma unix.cma  -o /Users/Ewandelanoy/Documents/OCaml/Idaho/debugged.ocaml_debuggable ";
  "mv /Users/Ewandelanoy/Documents/OCaml/Idaho/debugged.ocaml_debuggable /Users/Ewandelanoy/Documents/OCaml/Idaho/_build/"
  ];;

Sys.chdir "/Users/Ewandelanoy/Documents/OCaml/Idaho/";;

let z1=
  [
  "rm -f _build/*.d.cm*";
  "rm -f _build/*.ocaml_debuggable";                                                  
  "ocamlc -bin-annot -g  -I _build str.cma unix.cma  -o debugged.d.cmo -c debugged.ml";
  "mv debugged.d.cm* _build/";
  "ocamlc -bin-annot -g  -I _build str.cma unix.cma debugged.d.cmo -o debugged.ocaml_debuggable ";
  "mv debugged.ocaml_debuggable _build/"
  ];;

let z1=
    [
    "rm -f _build/*.d.cm*";
    "rm -f _build/*.ocaml_debuggable";                                                  
    "ocamlc  -g  -I _build str.cma unix.cma  -o debugged.d.cmo -c debugged.ml";
    "mv debugged.d.cm* _build/";
    "ocamlc  -g  -I _build str.cma unix.cma  -o debugged.ocaml_debuggable ";
    "mv debugged.ocaml_debuggable _build/"
    ];; 

let z1=
      [
      "rm -f _build/*.cm*";
      "rm -f _build/*.ocaml_debuggable";                                                  
      "ocamlc  -g  -I _build str.cma unix.cma  -o debugged.cmo -c debugged.ml";
      "mv debugged.cm* _build/";
      "ocamlc  -g  -I _build str.cma unix.cma  -o debugged.ocaml_debuggable ";
      "mv debugged.ocaml_debuggable _build/"
      ];; 

let z1=
        [
        "rm -f *.cm*";
        "rm -f *.ocaml_debuggable";                                                  
        "ocamlc  -g  str.cma unix.cma  -o debugged.cmo -c debugged.ml";
        "ocamlc  -g  str.cma unix.cma  -o debugged.ocaml_debuggable "
        ];; 

let z1=
          [
          "rm -f *.cm*";
          "rm -f *.ocaml_debuggable";                                                  
          "ocamlc  -g  str.cma unix.cma  -c debugged.ml";
          "ocamlc  -g  str.cma unix.cma  -o debugged.ocaml_debuggable "
          ];; 

   



    let z2=Explicit.image Sys.command z1;;

*)

(*

let g1=
  Self_contained_module_copy.self_contained_module_copy
    "sc" (hmx "please_test_me")
;;

let ap=Absolute_path.of_string "debugged.ml";;

Io.erase_file_and_fill_it_with_string ap g1 ;;

*)


(*
let level_one=Php_parser_homomorphism.star Beaver_for_statement.parser;;

let u1=Manage_lexed_data.get_data
["symblog";"phpbb"];;

let u2=Explicit.image Level_one.level_one u1;;

let v1=Debugging_tools.image level_one u1;;
let (i1,v2)=v1;;
let v3=v2.Positioned_php_token_list.contained;;
let v4=Listennou.big_head 18 v3;;
let v5={Positioned_php_token_list.contained=v4};;

#use"debugged.ml";;


#use"Remembered/recreating_tools.ml";;

let v6=encode_postokenlist v5;;

let v61=Listennou.big_tail 10 v6;;


let vv=Beaver_for_statement.parser v5;; 


let u3=Explicit.image Option.unpack u2;;

let u4=Option.filter_and_unpack (fun (a,b,c)->
   if Positioned_php_token_list.is_empty c 
   then None 
   else Some(c)
) u3;;

let u5=List.hd u4;;

#use"Php_analizer/Beavers/beaver_for_statement.ml";;
*)


(*

German_wrapper.undeclare_printer_equipped_type
(hmx "debugged");;
German_wrapper.save_all();;


let u1=abo "beaver_for_statement";;
let u2=abo "php_token";;
let u3=image (fun x->
  x.Half_dressed_module.naked_module
) u2;;

let g1=vfm "php_atomic_selector";;
let g2=image fst g1;;
let g3=List.filter (
   fun s->String.capitalize_ascii s=s
) g2;;
let g4=Ordered.diforchan Dictionary_order.dictionary_order g3;;

let tf s=
    if List.mem s ["Char";"Int";"Float"]
    then "of_"^(String.lowercase_ascii s)
    else String.lowercase_ascii s;;

let g5=image (fun s->(tf s,s)) (ofo g4);;
let owl=Total_ordering.product 
 Dictionary_order.dictionary_order 
 Total_ordering.standard;;
let g6=ofo(Ordered.diforchan owl g5);;
let g7=image (fun (a,b)->
 "let "^a^" s = make Php_projected_token."^b^" s;;"
) g6;;
let g8="\n\n\n"^(String.concat "\n" g7)^"\n\n\n";;

let g9=Explicit.image (fun (a,b)->
German_values_in_modules.replace_string 
 (German_wrapper.data()) ("Php_token."^b) ("Php_token."^a)
) g5;;

let g10=Explici

*)



(*

let u1=Manage_lexed_data.get_data
["symblog";"phpbb"];;

let u2=Explicit.image Level_one.level_one u1;;

let u3=Explicit.image Option.unpack u2;;

let u4=Option.filter_and_unpack (fun (a,b,c)->
   if Positioned_php_token_list.is_empty c 
   then None 
   else Some(c)
) u3;;

let u5=List.hd u4;;

#use"Php_analizer/Beavers/beaver_for_statement.ml";;

Beaver_for_statement.parser u5;;

*)

(*
let s1="00:00:10,996 --> 00:00:13,198";;
let n1=String.length s1;;
let u1=Ennig.doyle (
  fun j->(j,String.get s1 j)
) 0 (n1-1);;


let u1=Int_uple.list_of_pairs 44;;
let u2=List.filter (fun (x,y)->x*y*(y*y-x*x)=2016) u1;;


let u1=Int_uple.list_of_pairs 250;;
let u3=image (fun (x,y)->(x*y*(y*y-x*x),(x,y))) u1;;
let u4=ofo(Tidel2.diforchan u3);;


let u4=List.filter (fun (m,(x,y))->) u3;;


let v1=(ennig 1 6)@[8;9];;
let v2=ennig 1 1000;;

*)


(*
exception Uu_error;;
exception Vv_error;;
exception Ww_error;;


let ff x=Memoized.small usual_son_for_list [x] 20;;
let gg n=Memoized.small usual_son_for_list [p12] n;;


let old_uu x=let l=sons x in if List.length(l)<>2 then raise(Uu_error) else List.nth l 0;;
let old_vv x=let l=sons x in if List.length(l)<>2 then raise(Vv_error) else List.nth l 1;;
let old_ww x=let l=sons x in if List.length(l)<>1 then raise(Ww_error) else List.nth l 0;;
let uu=enhance old_uu;;
let vv=enhance old_vv;;
let ww=enhance old_ww;;
*)

(*
let long_base=
  "/Users/ewandelanoy/Documents/Web_Projects/Nand_to_Tetris/";;

let long_name=long_base^"nand2tetris_again";;

let preliminaries=Explicit.image Unix_command.hardcore_uc
[
  "mkdir -p "^long_name;
  "rm -rf "^long_name^"/*";
  "cp -R "^long_base^"nand2tetris/* "^long_name^"/";
];;

let dir=Directory_name.of_string long_name;;

let u1=More_unix.complete_ls_with_nondirectories_only dir;;

let u2=image (
  fun ap->
  let old_s=Absolute_path.to_string ap in
  let new_s=Replace_inside.replace_inside_string (" ","\\ ") old_s in
  (ap,new_s)
) u1;;

let u3=Ordered_string.diforchan(image (fun (ap,s)->Father_and_son.son s '.'  )u2);;

let u4=Option.filter_and_unpack (
  fun (ap,s)->
    let (t1,t2)=Father_and_son.father_and_son s '.' in
    if List.mem t2 ["gif";"out";"pdf";"xml";"html";"DS_Store";"txt"]
    then None
    else let new_s=t1^"_"^t2^".txt" in
         Some("mv "^s^" "^new_s)
) u2;;

let u5=Explicit.image Unix_command.hardcore_uc u4;;
*)


(*
let u1=[
  238;1043;1077;1927;1931;1967;
  1283;1273;1244;1389;1386;1396;1407;1460;1459;1880;1885;
  1927;1931;1967;
  2001;2015;2016;2022;2798;
  3746;3822;
  3141;3745;3750;3820;3834;3800;3983;3921;3934;
  4106;4483;4771;
  5273;5702;5732;5771
];;

let u2=ofo(Tidel.diforchan u1);;

let dir="/Users/ewandelanoy/Documents/Giulio/";;

let writer i=
  let si=string_of_int i in
  "/Applications/cpdf -scale-to-fit a4portrait "^dir^"p"^si^".pdf -o "^dir^"p_"^si^".pdf";;

let u3=Ennig.doyle writer 1 14;;
let act1=Explicit.image Unix_command.hardcore_uc u3;;

let act2=More_coherent_pdf.merge
   ~rootdir:"/Users/ewandelanoy/Documents/Giulio/"
   ~pdfname:"p"
   ~interval:(1,14);;

*)   


(*

let dir=Directory_name.of_string "/Users/ewandelanoy/Documents/Giulio/";;
let u1=More_unix.complete_ls dir;;
let u2=image (fun ap->(ap,Absolute_path.to_string ap) ) u1;;
let u3=List.filter(
   fun (ap,s)->not(List.mem s
     ["/Users/Ewandelanoy/Documents/Giulio/.DS_Store";                         
      "/Users/Ewandelanoy/Documents/Giulio/"] 
   )
) u2;;

let first_index s=
   let i1=Substring.leftmost_index_of_in "-" s in
    let i2=Substring.leftmost_index_of_in_from "-" s (i1+1) in
    let t=Cull_string.interval s (i1+1) (i2-1) in
    (int_of_string t);;

let indexes=ofo(Tidel.diforchan(image (fun (ap,s)->first_index s) u3));;    
let whole=
     let a=List.hd(indexes) and b=List.hd(List.rev indexes) in
     Ordered.S(Ennig.ennig a b);;
let holes=Tidel.lemel whole (Ordered.S indexes);;

let u4=image (
  fun (ap,s)->
    let j=find_index(first_index s) indexes in
    let base=Father_and_son.father s '/' in
    let new_s=(base)^"/isacruz_"^(string_of_int(j))^".pdf" in
    "cp "^s^" "^new_s
) u3;;

let u5=Explicit.image Unix_command.hardcore_uc u4;;

let u6=More_coherent_pdf.merge
   ~rootdir:"/Users/ewandelanoy/Documents/Giulio/"
   ~pdfname:"isacruz"
   ~interval:(1,563);;


let z5=List.filter (fun (ap,s)->List.mem (first_index s) [516;519;534;537]) u3;;
let z6=image fst z5;;

let  check1=List.filter (
  fun (ap,s)->not(Substring.begins_with 
    s "/Users/Ewandelanoy/Documents/Giulio/uva."
  )
) u3;;

let  check2=List.filter (
  fun (ap,s)->not(Substring.ends_with 
    s ".pdf"
  )
) u3;;



*)













(*
let dir="/Users/ewandelanoy/Documents/Firpo/";;

let z1=More_coherent_pdf.merge
   ~rootdir:"//Users/ewandelanoy/Documents/Firpo/"
   ~pdfname:"p"
   ~interval:(1,23);;

let u1=Ennig.doyle (fun j->
  let sj=string_of_int j in
  "/Applications/cpdf -scale-to-fit a4portrait "^
  dir^"p"^sj^".pdf -o "^dir^"p_"^sj^".pdf"
) 1 23;;   

let u2=image Unix_command.hardcore_uc u1;;
*)

(*
let u1=[
  636;643;617;628;627;605;624;653;
  575;589;595;
  760;746;768;725;727;713;
  811;803;848;859;818;809
];;

let u2=ofo(Tidel.diforchan u1);;
*)
(*
let ap=Absolute_path.of_string "/Users/ewandelanoy/Documents/html_files/Translations/membership_translated.txt";;
let z1=Io.read_whole_file ap;;
let i1=Substring.leftmost_index_of_in "OCTOBERR" z1;;

let z2=itv z1 i1 (i1+1000);;

let act1=Replace_inside.replace_several_inside_file
[
  ("a\204\128","\195\160");
  ("a\204\130","\195\162");
  ("c\204\167","\195\167");
  ("e\204\128","\195\168");
  ("e\204\129","\195\169");
  ("e\204\130","\195\170");
  ("e\204\136","\195\171");
  ("o\204\130","\195\180");
  ("u\204\130","\195\187");
  ("C\204\167","\195\135");
  ("E\204\129","\195\137");
]
ap;;

*)





(*
type wrapped_int=Wr of int;;


let print_out_wrapped_int (fmt:Format.formatter) (Wr i)=
   Format.fprintf fmt "@[<1>%s%s@<1>%s@]" 
   "\xe3\x80\x90" 
   (" "^(string_of_int i)^" ")  
   "\xe3\x80\x91";;  

#install_printer print_out_wrapped_int;;


let example1=Wr 7;;
let example2=print_string(print_wrapped_int example1);;
*)

(*

let print_out_wrapped_int (fmt:Format.formatter) (Wr i)=
   Format.fprintf fmt "[@<1>%s%s@<2>%s@]" 
   "\xe3\x80\x90" 
   (" "^(string_of_int i)^" ")  
   "\xe3\x80\x91";;  

#install_printer print_out_wrapped_int;;

let u1=Manage_lexed_data.get_data ["symblog";"phpbb"];;

let old_u2=Explicit.image Php_parser_homomorphism.star Beaver_for_statement.parser u1;;

let dummy=Positioned_php_token_list.print;;
*)


(*

let h1=
{Positioned_php_token_list.contained=
Image.image
(fun (x,y)->Positioned_php_token.PPL(x,y))
[(Php_token.Constant (Php_constant_token.Kwd Php_keyword.T_ECHO),                                                                                 ({Lexing.pos_fname =                                                                                                                        
       "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Vendor/Symfony/Symfony/Src/Symfony/Component/Templating/Tests/Fixtures/Templates/foo.php";
      pos_lnum = 1; pos_bol = 0; pos_cnum = 7},
     {Lexing.pos_fname =
       "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Vendor/Symfony/Symfony/Src/Symfony/Component/Templating/Tests/Fixtures/Templates/foo.php";
      pos_lnum = 1; pos_bol = 0; pos_cnum = 10}));
   (Php_token.Variable "$foo",
    ({Lexing.pos_fname =
       "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Vendor/Symfony/Symfony/Src/Symfony/Component/Templating/Tests/Fixtures/Templates/foo.php";
      pos_lnum = 1; pos_bol = 0; pos_cnum = 12},
     {Lexing.pos_fname =
       "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Vendor/Symfony/Symfony/Src/Symfony/Component/Templating/Tests/Fixtures/Templates/foo.php";
      pos_lnum = 1; pos_bol = 0; pos_cnum = 15}));
   (Php_token.External_echo "\n",
    ({Lexing.pos_fname =
       "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Vendor/Symfony/Symfony/Src/Symfony/Component/Templating/Tests/Fixtures/Templates/foo.php";
      pos_lnum = 1; pos_bol = 0; pos_cnum = 16},
     {Lexing.pos_fname =
       "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Vendor/Symfony/Symfony/Src/Symfony/Component/Templating/Tests/Fixtures/Templates/foo.php";
      pos_lnum = 1; pos_bol = 0; pos_cnum = 19}))]
};;

let h2=Option.find_really (fun x->x.Beaver_for_statement.name="echo2") 
(!(Beaver_for_statement.current_data_list));;

let bad1=Beaver_for_statement.classical_parser h2 h1;;

let elt=h2 and l=h1;;
let trmt1=Termite.of_string elt.Beaver_for_statement.unadbriged_content;;
let opt2=Termite.parse trmt1 l;;
let (l2,cr2,peurrest)=Option.unpack opt2;;
let cata=elt.Beaver_for_statement.catalyser;;
let catalyser_check=(
        if cata=""
        then true
        else (Termite.parse (Termite.of_string cata) peurrest)<>None
);;

*)

(*
let some_value=5;;

module Boogie=struct

let b=6;;

module Woogie=struct

let parker=7;;

module Andrew=struct

let d=8;;

let first_user=d+1;;

end;;

let second_user=Andrew.d+2;;

let fleury=9;;

end;;

let third_user=Woogie.Andrew.d+3;;

let burp=10;;

end;;



let fourth_user=Boogie.Woogie.Andrew.d+3;;


let g=48+Boogie.Woogie.parker;;
let h=49+some_value;;

*)

(*

let f x=match x.Ocaml_gsyntax_item.category with                                                                     
  | Ocaml_gsyntax_category.Type
  | Ocaml_gsyntax_category.Exception->(1,x)
  | Ocaml_gsyntax_category.Module_opener->(2,x)
  | Ocaml_gsyntax_category.Module_closer->(3,x)
  | Ocaml_gsyntax_category.Module_inclusion->(4,x);;

module Mood=struct

type mytype= A |B |C |D |E;;

end;;

let f x=match x with Mood.A|Mood.B|Mood.C->(1,x) |Mood.D->(2,Mood.D) |Mood.E->(3,Mood.E);;


module Weak=struct
let e=6;;
end;;

*)




(*
let z1=(!(Unix_command.accu));;
let z2=List.rev z1;;

"ocamlc -bin-annot -g  str.cma unix.cma  -o debugger.d.cmo -c debugger.ml";
"ocamlopt -bin-annot -g str.cmxa unix.cmxa  -o debugger.ocaml_debuggable ";
 *)


(*
let mdata=German_wrapper.data();;
let tgts=German_wrapper.up_to_date_targets();;

let _=Alaskan_remove_debuggables.rd German_constant.root mdata;;
let dbg=Debugger_name.debugger_name;;
let dir=German_constant.root;;
let rdir=German_directories.from_data mdata;;
let ap=Find_suitable_ending.find_file_location dir rdir 
	     (dbg^".ml");;
let hm=Half_dressed_module.of_path_and_root ap dir;;
let mdata2=German_modify_modulesystem.recompute_module_info mdata hm;;
let tgt=Ocaml_target.debuggable hm;;

let bad1=Alaskan_make_ocaml_target.make_nontoplevel German_constant.root
	(mdata2,tgts) tgt;;
*)

(*
let l=Alaskan_ingredients_for_ocaml_target.ingredients_for_ocaml_target
   mdata2 tgt;;
let v0=(true,(mdata2,tgts));;
let v1=Alaskan_make_ocaml_target.unit_make dir v0 (List.nth l 0);;
*)

(*
let bad2=Alaskan_make_ocaml_target.unit_make dir v1 (List.nth l 1);;
*)

(*
let (bowl,(mdata,tgts))=v1 and tgt=List.nth l 1;;

let see1=Alaskan_make_ocaml_target.is_up_to_date dir tgts tgt;;
let l_cmd=Alaskan_command_for_ocaml_target.command_for_ocaml_target dir mdata tgt;;


  let temp1=Image.image Unix_command.uc (cmd_for_tgt dir mdata tgt) in 
  if List.for_all (fun i->i=0) temp1
  then let opt_tgt=(if Ocaml_target.is_a_debuggable tgt 
                    then None 
                    else (Some tgt)) in
       let tgts2=Option.add_perhaps opt_tgt tgts in
        match Ocaml_target.ml_from_lex_or_yacc_data tgt with
       None->(true,(mdata,tgts2))
       |Some(mlx)->
                   let mdata2=Alaskan_force_modification_time.update dir mdata mlx in
                   (true,(mdata2,tgts2))        
  else (false,(mdata,tgts));;
*)

(*
let txt1=Self_contained_module_copy.unsharped_content (hmx "php_lexer");;

let see1=Substring.occurrences_of_in " 18 ->" txt1;;
let i1=List.hd(see1);;
let see2=itv txt1 i1 (i1+200);;

let nm1=Naked_module.of_string "Positioned_php_token_list";;
let nm2=Naked_module.of_string "Scp_positioned_php_token_list";;

let txt2=Look_for_module_names.change_module_name_in_string
   nm1 nm2 txt1;;

let see3=Substring.occurrences_of_in " 18 ->" txt2;;
let i2=List.hd(see3);;
let see4=itv txt2 i2 (i2+200);;

let bad1=Outside_comments_and_strings.good_substrings txt1;;
let tag1=Option.find_really (fun (i,j,t)->i>=i1) bad1;;

German_pervasives.sd;;


let bad1=Look_for_module_names.indices_in_string txt1;;

let tag1=List.filter (fun (x,y,z)->Substring.is_a_substring_of " 18 ->" z ) bad1;;
*)

(*

let g1=hmx "please_test_me";;
let g2=Self_contained_module_copy.self_contained_module_copy "Sc_" g1;;

let ap=Absolute_path.of_string "debugger.ml";;
let act1=Io.erase_file_and_fill_it_with_string ap g2;;



*)

(*
Unix_command.hardcore_mode:=true;;

let dir=Directory_name.of_string(
"/Users/ewandelanoy/Documents/OCaml/Idaho"
);;

German_update_copied_compiler.ucc dir;;
*)

(*
`
let dir1=German_constant.root;;

let g1=More_unix.complete_ls_with_nondirectories_only dir1;;
let g2=image Absolute_path.to_string g1;;
let g3=List.filter (
  fun s->List.for_all
  (fun t->not(Substring.begins_with s t))
  [
    "/Users/Ewandelanoy/Documents/OCaml/Ordinary/_build";
    "/Users/Ewandelanoy/Documents/OCaml/Ordinary/Forgotten"
  ]
) g2;;
let g4=List.filter (
  fun s->List.for_all
  (fun t->not(Substring.ends_with s t))
  [
    ".ml";".mli";".mll";".mly";".merlin";".md";".txt"
  ]
) g3;;


*)

(*
let g1=dbel "shell_command";;
let g2=image (fun hm->
Mlx_ended_absolute_path.to_absolute_path
(Mlx_ended_absolute_path.join hm Ocaml_ending.Ml))
g1;;
let g3=Image.image (fun ap->
   Replace_inside.replace_inside_file
   ("Shell_command.announce_and_do","Unix_command.uc")
   ap
) g2;;
*)

(*
let z1=(!(German_wrapper.Private.directories_ref));;

let z2=image (
  fun sd->let s=Subdirectory.connectable_to_subpath sd;;
  "mv "^s^"*.cm* _build"
) z1;;
let z3=Explicit.image Sys.command z2;;

let z3=Explicit.image Sys.command
["mv *.cm* _build"; "mv Country/Alaska/*.cm* _build";                             
 "mv Country/Germany/*.cm* _build"; "mv Creators/*.cm* _build";                
   "mv GParser/*.cm* _build"; "mv Generic_syntax_types/*.cm* _build";
   "mv Global_variables/*.cm* _build"; "mv Makefile_makers/*.cm* _build";
   "mv Ocaml_analysis/*.cm* _build"; "mv Optional/*.cm* _build";
   "mv Ordered_Lists/*.cm* _build"; "mv Php_analizer/*.cm* _build";
   "mv Php_analizer/Beavers/*.cm* _build";
   "mv Php_analizer/Php_syntax_types/*.cm* _build";
   "mv Preprinters/*.cm* _build";
   "mv Test_directory6/Test_directory7/Test_directory2/*.cm* _build"];;



*)


(*
let z3=String.concat " " (List.flatten z2);;

let z4=German_data.inside_files (German_wrapper.data());;
let z5=image Absolute_path.to_string z4;;
let z6=String.concat " " ("git add"::z5);;
*)

(*

let z1=muv "Sys.command";; 
let z2=List.filter (
  fun t->t.Half_dressed_module.naked_module <> "unix_command"
) z1;;

let z3=image (fun hm->
Mlx_ended_absolute_path.to_absolute_path
(Mlx_ended_absolute_path.join hm Ocaml_ending.Ml))
z2;;



let z4=image (fun ap->
Replace_inside.replace_inside_file
("Sys.command","Unix_command.uc") ap
)z3;;

*)

(*

let g1=Directory_name.of_string 
"/Applications/Visual Studio Code.app/Contents/Resources";;

let g2= More_unix.complete_ls_with_nondirectories_only g1 ;;

let g3=image Absolute_path.to_string g2;;

let g4=List.filter 
  (fun x->Substring.is_a_substring_of ".fr." x)
g3;;

let g5=image(
   fun x->let i=Substring.leftmost_index_of_in ".fr." x;;
    (itv x 1 (i-1),Cull_string.cobeginning (i+3) x)
) g4;;

let g6=Explicit.image(
   fun (s1,s2)->
      let temp=Option.filter_and_unpack (
         fun z->
            if (Substring.begins_with z s1)&&(Substring.ends_with z s2)
            then let t=itv z (String.length(s1)+1) (String.length(z)-String.length(s2));;
                 Some(t)
            else None
      ) g3;;
      (temp,s1,s2)
) g5;;

let g7=List.filter (fun (l,s1,s2)->not(List.mem "." l)) g6;;

let g8=image(
  fun (l,t1,t2)->
  let s1=Str.global_replace (Str.regexp_string " ") "\\ " t1
  and s2=Str.global_replace (Str.regexp_string " ") "\\ " t2;; 
  "cp "^s1^"."^s2^" "^s1^".fr."^s2
) g6;;

let g9=List.hd g8;;

let act=image Sys.command g8;;

let check1=List.filter (
   fun (s1,s2)->not(Substring.ends_with s1 ".nls")
) g5;;

let g10=image (fun ap->(ap,Absolute_path.to_string ap)) g2;;

let g11=List.filter 
  (fun (ap,x)->Substring.is_a_substring_of ".fr." x)
g10;;

let g12=Explicit.image(
   fun (ap,x)->
   Replace_inside.replace_inside_file
   (".nls\",{",".nls.fr\",{") ap
) g11;;


let g13=image(
  fun (t1,t2)->
  let s1=Str.global_replace (Str.regexp_string " ") "\\ " t1
  and s2=Str.global_replace (Str.regexp_string " ") "\\ " t2;; 
  (s1,s2,Absolute_path.of_string(s1^".fr."^s2))
) g5;;

*)



(*
let xaa=7;;

let y=xaa+1;;

let g1=Absolute_path.of_string 
"/Users/ewandelanoy/Documents/html_files/Wilhelm/wilhelm_main.html";;
let text1=Io.read_whole_file g1;;
let stext1=itv text1 1 1000;;

let g2=Absolute_path.of_string 
"/Users/ewandelanoy/Documents/html_files/Wilhelm/wilhelm_01.html";;
let text2=Io.read_whole_file g2;;
let stext2=itv text1 1 1000;;

let u1=doyle (fun i->"w"^(string_of_int i)^".pdf") 1 29;;
let u2=String.concat " " u1;;

let soi i=let s=string_of_int i;; if i<10 then "0"^s else s;;


let g2=doyle (
   fun i->
    let s=soi i;;
   ("<a href=\"http://strobertbellarmine.net/wilhelm_scannell_"^s^".html",
    "<a href=\"wilhelm_"^s^".html")
) 1 19;;

let act1=Replace_inside.replace_several_inside_file g2 g1;;

let g3=doyle (
   fun i->
    let s=string_of_int i and t=soi(19+i);;
   ("<a href=\"http://strobertbellarmine.net/wilhelm_scannell_2_"^s^".html",
    "<a href=\"wilhelm_"^t^".html")
) 1 7;;

let act2=Replace_inside.replace_several_inside_file g3 g1;;

let g4=(
  "<a href=\"http://strobertbellarmine.net/index.htm\">Home</a>",
  "<a href=\"wilhelm_main.html\">Home</a>"
);;

let g5=doyle (fun k->
   let s=soi k;;
   Absolute_path.of_string 
   ("/Users/ewandelanoy/Documents/html_files/Wilhelm/wilhelm_"^s^".html")
) 1 26;;

let act3=image (Replace_inside.replace_inside_file g4) g5;;




(*

let z1=German_wrapper.data();;
let z2=image Modulesystem_data.name z1;;
let z3=image Half_dressed_module.naked_module z2;;
let z4=image (fun (Naked_module.N s)->s) z3;;
let z5=big_head 20 z4;;



let g1=Directory_name.of_string "~/.opam/";;
let g2=More_unix.complete_ls_with_nondirectories_only g1;;
let g3=image Absolute_path.to_string g2;;
let g4=List.filter (fun s->Substring.is_a_substring_of "erlin" s) g3;;

*)



(*

let zz i=
   let si=string_of_int(i)
   and sii=string_of_int(i+1)
   and siii=string_of_int(i+2);;
  "pre_level"^si^"=polrem(level"^siii^",level"^sii^",a)\n"^
  "level"^si^"=normalize(pre_level"^si^")";;

let u1=doyle (fun k->12-k) 1 11;;
let u2=image zz u1;;
let u3=String.concat "\n\n" u2;;
let u4="\n\n\n"^u3^"\n\n\n";;
print_string u4;;



*)


(*


let z1=Io.read_whole_file(
Absolute_path.of_string "/Users/ewandelanoy/Documents/OCaml/Ordinary/Remembered/aztec2.ml"
);;
let z2=Cull_string.coending 2 z1;;
let z3=Str.split (Str.regexp_string"\n") z2;; 
let z4=image (Cull_string.cobeginning 3) z3;;
let z5=image (Str.split (Str.regexp_string" ")) z4;; 
let z6=image (fun l->"cp "^(List.nth l 1)^" "^(List.nth l 0) ) z5;;
let z7=String.concat "\n" ("\n\n\n"::(z6@["\n\n\n"]));;
print_string z7;;

let tower="/Users/ewandelanoy/Documents/Web_Projects/Patientcare_CakePHP/Control_Tower/";;

let z8=image (fun l->
   let s1=List.hd l;;
   let s2=tower^(Cull_string.cobeginning 7 s1);;
   let s3=Cull_string.cobeginning 86 s2;;
   let s4=List.hd(Str.split (Str.regexp_string"_") s3);;
   let s5=String.capitalize(Cull_string.coending 1 s4);;
   (Absolute_path.of_string s2,s5)
) z5;;

let zz s="<nav class=\"large-3 medium-4 columns\" id=\"actions-sidebar\">\n    <?= $this->Element('actions',array(\n       'type'=>'"^s^"',\n       'typePlural'=>'"^s^"s'\n    )); ?>\n</nav>";;

let z9=image (fun (ap,t)->
   Replace_inside.overwrite_and_dump_markers_inside_file
   (Overwriter.of_string (zz t))
   ("<nav","nav>")
   ap
) z8;;




*)

(*


let dir=Directory_name.of_string "/Users/ewandelanoy/Documents";;
let u1=More_unix.all_files_with_endings dir [".tex"];;
let u2=image Absolute_path.to_string u1;;
let u3=List.filter (
   fun s->List.exists(fun t->Substring.is_a_substring_of t s) ["aTeX"]
) u2;;

let g1=u2.Positioned_php_token_list.contained;;
let g2=List.nth g1 0;;
let g3=List.nth g1 1;;
let g4=List.nth g1 2;;

let g5=image (fun (Positioned_php_token.PPL(x,y))->(x,y)) g1;;


*)


(*

let chan=open_in (Manage_lexed_data.Private.marshaled_file_for_item "data1");;
let u1=((Marshal.from_channel chan):Positioned_php_token_list.t list) ;;
close_in chan;;


#use"Php_analizer/Beavers/beaver_for_statement.ml";;

let tt1=Explicit.image (
  fun l->match Php_parser_homomorphism.star parser l with
   None->(if Positioned_php_token_list.is_empty l then None else Some(l))
  |Some(_,_,l2)->(if Positioned_php_token_list.is_empty l2 then None else Some(l2))
) old_u1;;
let new_u1=Option.filter_and_unpack (fun x->x) tt1;;

let u2=List.hd new_u1 ;;

#use"Php_analizer/Beavers/beaver_for_statement.ml";;
let u3=Php_parser_homomorphism.star parser u2;;
let (u4,u5,u6)=Option.unpack u3;;
let u7=Positioned_php_token_list.is_empty u6;;
u6;;





let old_u1=new_u1;;

let g1= "<?php id ( variable [;;teger ] ) ? (int) variable [;;teger ] :;;teger";;
let sg1="id ()         ? _l_ no_ternary _r+_ : no_semicolon";;

let t1=Termite.of_string sg1;;
let l1=Php_lexer.parse_string g1;;
let dbg1=Termite.parse t1 l1;;
let dbg2=Termite_reverse_parse.rp t1 l1;;
let (w1,w2,w3,w4)=dbg2;;

let s1="<?php <<<ABC\nthey woke up\nABC;\n2";;
let g1=Php_lexer.parse_string s1;;
let g2=g1.Positioned_php_token_list.contained;;
let g3=image Positioned_php_token.snd g2;;




*)

(*

variable = id ( variable [;;teger ] ) ? (int) variable [;;teger ] :;;teger ;

let z1=Php_lexer.parse_string "<?php (bool)";;
let z2=Positioned_php_token_list.hd z1;;

*)


(*


German_wrapper.Private.printer_equipped_types_ref;;

let u1=Chronometer.it Manage_lexed_data.get_data ["symblog";"phpbb"];;
let see1=Chronometer.duration_of_last_computation();;


let u2=Explicit.image (fun x->(x,Level_one.level_one x)) u1;;

let viz1=Explicit.filter(fun (x,y)->y=None) u2;;
let u3=Explicit.image (fun (x,y)->
  let (a,b,c)=Option.unpack y;;
  (x,a,b,c) ) u2;;
let u4=Explicit.filter (fun (x,a,b,c)->c<>Positioned_php_token_list.empty) u3;;
let u5=image (fun (x,a,b,c)->c) u4;;

let chang=open_out (Manage_lexed_data.Private.marshaled_file_for_item "data1");;
(Marshal.to_channel chang u5 [];close_out chang);;

let chan=open_in (Manage_lexed_data.Private.marshaled_file_for_item "data1");;
let v1=((Marshal.from_channel chan):Positioned_php_token_list.t list) ;;
close_in chan;;


let u1=Manage_lexed_data.get_data ["symblog";"phpbb"];;

let old_u2=Explicit.image Php_parser_homomorphism.star Beaver_for_statement.parser u1;;

*)


(*

let commentless_lexer t=
  Positioned_php_token_list.filter (fun ptok->
   let tok=Positioned_php_token.fst ptok;;
   not(Php_token.is_a_comment tok)
)(Php_lexer.parse_file t);;


let u1=Directory_name.of_string
"/Users/ewandelanoy/Documents/Sites/Symblog/symblogproject/";;
let u2=More_unix.complete_ls u1;;
let u3=List.filter(
   fun ap->
     let s_ap=Absolute_path.to_string ap;;
     Substring.ends_with s_ap ".php"
) u2;;
let u4=Explicit.image (commentless_lexer) u3;;




let u5=Explicit.image (fun x->(x,Level_one.level_one x)) u4;;

let viz1=Explicit.filter(fun (x,y)->y=None) u5;;
let u6=Explicit.image (fun (x,y)->
  let (a,b,c)=Option.unpack y;;
  (x,a,b,c) ) u5;;
let u7=List.filter (fun (x,a,b,c)->c<>Positioned_php_token_list.empty) u6;;


let (_,_,_,u8)=List.hd u7;;

let u9=Positioned_php_token_list.hd u8;;
let (Positioned_php_token.PPL(u10,u11))=u9;;

*)

(*

#use"Php_analizer/Beavers/beaver_for_statement.ml";;

ea assignable4 "<?php +($melts*$down)";;



let t1=Termite.of_string assignable4;;
let l1=Php_lexer.parse_string "<?php +($melts*$down)";;
let dbg1=Termite.parse t1 l1;;
let dbg2=Termite_reverse_parse.rp t1 l1;;
let (u1,u2,u3,u4)=dbg2;;

let (Termite.Trmt trmt1)=t2;;
let v0=(([],trmt1,[],l2),None);;
let ff=Memoized.small Termite.pusher_for_parsing v0;;
let gg n=snd(ff n);;

let tok1=List.hd l1;;



*)

(*

#use"Php_analizer/Beavers/beaver_for_statement.ml";;

German_wrapper.Private.printer_equipped_types_ref;;
German_wrapper.declare_printer_equipped_type (hmx "positioned_php_token_list");;

German_wrapper.initialize();;
German_wrapper.Private.printer_equipped_types_ref;;

*)

*)

(*

let ea=Termite.eat;;

let assignable1="variable _l_ -> id _l_ () _r?_ _r*_";;

let barley1="_l_ id _u_ nmspc _rd_";;
let barley2="_l_ :: id _r?_";;
let assignable2=barley1^" "^barley2^" ()";;

let assignable=" _l_ "^(String.concat " _u_ " [assignable1;assignable2])^" _rd_";;

let s1="variable ##( assign )## "^assignable ^" ;";;

ea 
(assignable3 ^" ;") 
"<?php $amy->finds($x,$y)->beaver->eats($it)->again();";;

ea 
(" ##( assign )## "^assignable ^" ;") 
"<?php =$amy->finds($x,$y)->beaver->eats($it)->again();";;

ea 
("variable ##( assign )## "^assignable ^" ;") 
"<?php $bob=$amy->finds($x,$y)->beaver->eats($it)->again();";;

let s2=assignable^" ;";;
let t2=Termite.of_string s2;;
let l2=Php_lexer.parse_string "<?php $amy->finds($x,$y)->beaver->eats($it)->again();";;


let dbg4=Termite.parse t2 l2;;

let dbg5=Termite_reverse_parse.rp t2 l2;;
let (u1,u2,u3,u4)=dbg5;;

let (Termite.Trmt trmt1)=t2;;
let v0=(([],trmt1,[],l2),None);;
let ff=Memoized.small Termite.pusher_for_parsing v0;;
let gg n=snd(ff n);;


let (ret1,wh1)=List.hd trmt1;;

let (a1,a2,a3,a4)=fst(ff 1);;
let (b1,b2,b3,b4)=fst(ff 2);;





German_wrapper.Private.printer_equipped_types_ref;;
German_wrapper.declare_printer_equipped_type (hmx "positioned_php_token_list");;

German_wrapper.initialize();;
German_wrapper.Private.printer_equipped_types_ref;;

*)

(*



#use"Php_analizer/Beavers/beaver_for_statement.ml";;

let ea=Termite.eat;;



let barley1="_l_ _l_;;clude_like _u_ new _u_ @ _rd_ _r?_";;
let barley2="_l_ _l_ -> id () _r+_ _u_ _l_ loose= _r*_ _rd_";;
let assignable1="variable "^barley2;;

let barley3="_l_ id _u_ nmspc _rd_";;
let barley4="_l_ :: id _r?_";;
let assignable2=barley3^" "^barley4^" ()";;


let assignable3="variable _l_ -> id _l_ () _r?_ _r*_";;

let assignable=" _l_ "^(String.concat " _u_ " [assignable1;assignable2;assignable3])^" _rd_";;

let s1="variable ##( assign )##  "^assignable^"  ;";;

let dbg1=ea s1 "<?php $bob=$amy->finds($x,$y)->beaver->eats($it)->again();";;


let dbg2=ea assignable3 "<?php $amy->finds($x,$y)->beaver->eats($it)->again()";;

let dbg3=ea 
  (assignable^" ;")
   ("<?php $amy->finds($x,$y)->beaver->eats($it)->again();");;


let s2=assignable^" ;";;
let t2=Termite.of_string s2;;
let l2=Php_lexer.parse_string "<?php $amy->finds($x,$y)->beaver->eats($it)->again()";;

let dbg4=Termite.parse t2 l2;;

let dbg5=Termite.reverse_parse t2 l2;;

let (u1,u2,u3,u4)=Option.unpack dbg5;;

German_wrapper.declare_printer_equipped_type (hmx "positioned_php_token_list");;
German_wrapper.save_all();;

let dbg4=Termite.parse t2 



let t1=Termite.of_string s1;;

let g1=Termite.parse_string 



let dir1=Directory_name.of_string "~/.opam/packages/utop/utop.1.17/";;
let u1=More_unix.complete_ls dir1;;

let g1=Io.read_whole_file (Absolute_path.of_string "~/.bash_profile");;


let z1=see();;
let z2=Beaver_for_statement.should_be_empty();;
let (z3,z4,z5,z6)=List.hd z2;;


let u5=Explicit.image Level_one.level_one u4;;

let z1=Beaver_for_statement.see();;
let z2=Beaver_for_statement.should_be_empty();;
let (z3,z4,z5,z6)=List.hd z2;;

let term1=Termite.of_string z5;;
let lexed1=Php_lexer.parse_string z6;;
let res1=Termite.parse term1 lexed1;;
let res2=Termite.reverse_parse term1 lexed1;;

let z7=Option.unpack res2;;
let (z8,z9,z10,z11)=z7;;





*)


(*


let z1=Php_atomic_selector.all_constants;;

let part1=image 
  Php_token.projected_version Php_token.fixture_of_nonconstants;;

let z1=image (fun cst->
   let tok=Php_token.Constant(cst);;
    (Php_token.projected_version tok,Php_constant_token.to_string cst))
    Php_constant_token.all ;;
let z2=List.filter (fun (u,v)->u<>v) z1;;    
    
    
let part2=image  Php_token.projected_version z1;;

let part3=image snd Php_atomic_selector.special_list;;

let part4=image fst Php_short_selector.new_constants;;

let part5=
   ()
   @
   ()

let labelled_parts=[
   "nonconst",part1;
   "const",part2;
   "atomic",part3;
   "short",part4;
];;

let parts=image snd labelled_parts;;

let u1=image (fun x->(x,ofo(Tidel.diforchan(x))) ) parts;;
let check1=List.filter (fun (x,y)->hi(x)<>hi(y)) u1;;
let u2=image snd u1;;
let labelled_u2=image (fun ((x,y),z)->(x,z) ) (List.combine labelled_parts u2);;
let whole=ofo(Tidel.diforchan(List.flatten u2));;
let u3=image (
  fun w->
   (w,Option.filter_and_unpack (
     fun (j,l)->
       if List.mem w l
       then Some(j)
       else None
   ) labelled_u2)
) whole;;
let check2=List.filter (fun (w,l)->List.length(l)>1) u3;;






let whole=part1@part2;;

let;;dexed_whole=
   List.flatten (
   [
    image (fun x->(1,x)) part1;
    image (fun x->(2,x)) part2
   ]
   );;
let n1=hi;;dexed_whole;;   
let ordered_whole=ofo(Tidel.diforchan(whole));;
let u1=List.filter (
   fun x->Option.filter_and_unpack (fun ) (ennig 1 n1)
) ordered_whole;;


let check1=(hi(ofo(Tidel.diforchan(whole)))=hi(whole));;

*)


(*


let g1=["preprinter_example"; "php_parser"; "php_recognizer";                  "extract_left_block"; "php_recognize_block"; "php_short_selector";    
 "php_recognizer_homomorphism"; "php_constructible_recognizer";
 "termite"; "php_yuze_modifier"; "php_parser_homomorphism";
 "php_lexer"; "beaver_for_statement"; "level_one"];;
 
let g2=List.rev_map (fun x->"old_"^x) g1;; 

let g3=Explicit.image fg g2;;

Level_one.level_one;;




German_wrapper.declare_printer_equipped_type
(hmx "positioned_php_token_list");;



let g1=bel "positioned_php_token";;
let g2=image Half_dressed_module.naked_module g1;;
let g3=image Naked_module.to_string g2;;

let g3=
["preprinter_example"; "php_parser"; "php_recognizer";                  "extract_left_block"; "php_recognize_block"; "php_short_selector";    
 "php_recognizer_homomorphism"; "php_constructible_recognizer";
 "termite"; "php_yuze_modifier"; "php_parser_homomorphism";
 "php_lexer"; "beaver_for_statement"; "level_one"];;

let g4=am();;
let g5=List.filter (Substring.begins_with "old_") g4;;

let g6=image (fun x->(x,"old_"^x))


let g7=Explicit.image (fun (x,y)->ren x y) g6;;

let gg x=cf ("old_"^x) x;;


*)


(***********************************************************************************)

(*

let z1=Compute_all_ocaml_items.caoi (German_wrapper.data());;
let z2=List.filter(
        fun itm->
        let s=Ocaml_gsyntax_item.name itm;;
        let j=String.index(s)('.')+1;;
        (Cull_string.beginning (j-1) s)="Please_test_me"
) z1;;
let z3=image Ocaml_gsyntax_item.name z2;;


let z3=Find_value_descendants.fvd 
z1
"Please_test_me.Boogie.Woogie.Andrew.d";;

*)

(***********************************************************************************)

(*

let z1=German_wrapper.data();;
let z2=image Modulesystem_data.name z1;;
let z3=List.filter(
   fun hm->
    let nm=Half_dressed_module.naked_module hm;;
    Substring.is_a_substring_of "ango"
    (Naked_module.to_string nm)
) z2;;
let z4=List.hd z3;;
let z5=List.hd(List.filter (fun md->
   Modulesystem_data.name(md)=z4
) z1);;
let corrected_z4={Half_dressed_module.bundle_main_dir =                                   "/Users/ewandelanoy/Documents/OCaml/Ordinary";                       
 subdirectory = "Ocaml_analysis"; naked_module = "lingo"};;
let corrected_z5=
{                                                         
      Modulesystem_data.name =corrected_z4;
      ml_present =true;
      mli_present =false;
      mll_present =false;
      mly_present =false;
      ml_modification_time = Modulesystem_data.ml_modification_time(z5);
      mli_modification_time = 0.;
      mll_modification_time = 0.;
      mly_modification_time = 0.;
      needed_libraries=Modulesystem_data.needed_libraries(z5);
      direct_fathers=Modulesystem_data.direct_fathers(z5);
      all_ancestors=Modulesystem_data.all_ancestors(z5);
      needed_directories = [Subdirectory.SD "Ocaml_analysis"];
    };;

let corrected_z1=image (fun x->if x=z5 then corrected_z5 else x) z1;;

German_wrapper.Private.data_ref:=corrected_z1;;

German_wrapper.data()=corrected_z1;;

Modulesystem_data.rename1 corrected_z4 z5;;



*)

(*

let s1=Io.read_whole_file 
(Absolute_path.of_string "outside_comments_and_strings.ml");;
let s2=Cull_string.cobeginning 3301 s1;;

let z1=
   let (a,b,c)=Gparser_for_ocaml_language.data_for_prsr_for_comment;;
   Gparser_house_with_doors.hwd (a,b) c s2 1;;

let v0=
let (a,b,c)=Gparser_for_ocaml_language.data_for_prsr_for_comment;;
Gparser_house_with_doors.Private.starter_for_hwd (a,b) c s2 1;;  

let ff=Memoized.small   
  Gparser_house_with_doors.Private.pusher_for_hwd v0;; 
let gg n=snd(ff n);;   

let u1=doyle (fun k->(k,gg k)) 1 700;;
let u2=List.filter (
  fun (k,x)->x.Gparser_house_with_doors.Private.awaited_closer<>None
) u1;;
let u3=image fst u2;;
let u4=Listennou.connected_components_in_intlist u3;;

let s3=Cull_string.cobeginning 520 s2;; 

*)

(***********************************************************************************)



(***********************************************************************************)

(*


let s1=Io.read_whole_file 
(Absolute_path.of_string "outside_comments_and_strings.ml");;
let s2=Cull_string.cobeginning 3301 s1;;

let z1=Read_ocaml_files.Private.read1 s2;;
let z2=Gparser_apply.apply Gparser_for_ocaml_language.main_prsr s2 1;;
let z3=Gparser_apply.apply Gparser_for_ocaml_language.elt_prsr s2 1;;
let z4=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_comment s2 1;;
let z5=
   let (a,b,c)=Gparser_for_ocaml_language.data_for_prsr_for_comment;;
   Gparser_apply.Private.house_with_doors (a,b) c s2 1;;


*)

(*

let ordered_u2=ofo(Ordered_string.diforchan u2);;
let u4=image (fun x->(x,
  List.assoc x u3
) ) ordered_u2;;

let list_for_translation=
[
   ("09 ", '\t'); ("0A ", '\n'); ("20 ", ' '); ("23 ", '#'); ("28 ", '(');                                                                         ("29 ", ')'); ("2B ", '+'); ("2C ", ','); ("2D ", '-'); ("2E ", '.');                                                                        
   ("30 ", '0'); ("31 ", '1'); ("32 ", '2'); ("33 ", '3'); ("34 ", '4'); 
   ("35 ", '5'); ("36 ", '6'); ("37 ", '7'); ("38 ", '8'); ("39 ", '9'); 
   ("3A ", ':'); ("3D ", '='); ("3F ", '?'); ("42 ", 'B'); ("43 ", 'C'); 
   ("44 ", 'D'); ("47 ", 'G'); ("48 ", 'H'); ("49 ", 'I'); ("4B ", 'K'); 
   ("4C ", 'L'); ("4D ", 'M'); ("4E ", 'N'); ("50 ", 'P'); ("51 ", 'Q'); 
   ("52 ", 'R'); ("53 ", 'S'); ("54 ", 'T'); ("56 ", 'V'); ("57 ", 'W'); 
   ("59 ", 'Y'); ("5A ", 'Z'); ("5B ", '['); ("5D ", ']'); ("61 ", 'a'); 
   ("62 ", 'b'); ("63 ", 'c'); ("64 ", 'd'); ("65 ", 'e'); ("66 ", 'f'); 
   ("67 ", 'g'); ("68 ", 'h'); ("69 ", 'i'); ("6A ", 'j'); ("6B ", 'k'); 
   ("6C ", 'l'); ("6D ", 'm'); ("6E ", 'n'); ("6F ", 'o'); ("70 ", 'p'); 
   ("71 ", 'q'); ("72 ", 'r'); ("73 ", 's'); ("74 ", 't'); ("75 ", 'u'); 
   ("76 ", 'v'); ("77 ", 'w'); ("78 ", 'x'); ("79 ", 'y'); ("7A ", 'z'); 
   ("7E ", '~'); ("80 ", '\128'); ("84 ", '\132'); ("98 ", '\152'); ("99 ", '\153'); 
   ("9C ", '\156'); ("9D ", '\157'); ("A0 ", '\ya0'); ("A1 ", '\ya1'); ("A2 ", '\ya2'); 
   ("A4 ", '\ya4'); ("A5 ", '\ya5'); ("A6 ", '\ya6'); ("A8 ", '\ya8'); ("AB ", '\yab'); 
   ("AC ", '\yac'); ("B8 ", '\yb8'); ("B9 ", '\yb9'); ("BB ", '\ybb'); ("BF ", '\ybf'); 
   ("C2 ", '\yc2'); ("C5 ", '\yc5'); ("CE ", '\yce'); ("CF ", '\ycf'); ("E1 ", '\ye1'); 
   ("E2 ", '\ye2')
];;

let u5=List.filter (fun i->List.nth u2 (i-1)="80 ") (ennig 1 n1);;
let u6=image (fun i->(List.nth u2 (i),List.nth u2 (i+1)) ) u5;;
let u7=Tidel.diforchan u6;;
let u8=image (fun (x,y)->"80 "^x^y) (ofo u7);;
let u9=image (
   fun z->(z,Substring.leftmost_index_of_in z u1)
) u8;;
let u10=image (
  fun (z,k)->
    let a=(k+2)/3;;
    let b=min(a+100)(n1);;
    (z,itv txt1 (a-2) b)
) u9;;

let u9=image (fun l->(List.hd(l)<>("0A ", "\n"),l) ) u8;;
let u10=image (fun (bowl,l)->
   if bowl
   then let temp1=Listennou.constant_slices (fun t->List.mem(fst t) first_order) l;;
        image (fun z->(List.mem(fst (List.hd z)) first_order,z) ) temp1
   else [(bowl,l)]
) u9;;
let u11=List.flatten u10;;
let u12=image (fun (bowl,l)->
   if bowl
   then (true,ofo(Ordered.diforchan word_order l))
   else (false,l)
) u11;;

let example=Slow_copy_task.initialize_from_data
  (7822,
   "tribunem@tribunemicael.net",
   "/Users/ewandelanoy/Documents/",
   "~/private_html/",
   Directory_name.of_string "/Users/ewandelanoy/Documents/Amailh",
   "Remembered/task_keeper1");;


let ap=Absolute_path.of_string "Remembered/task_keeper1";;
Replace_inside.replace_several_inside_file
[
 (* "mpdykruvueaoqhkt010","mpdykruvueaoqhkt001"; *)
 "mpdykruvueaoqhkt012","mpdykruvueaoqhkt002";
] ap;;

let text1=Io.read_whole_file ap;;
let example2=Slow_copy_task.unarchive text1;;

let jeng=open_out "temp";;
output_string jeng u9;;
close_out jeng;;

let chang=open_in "temp";;
let buffer = Bytes.create 16;;
let count=really_input_string chang 3;;

exception Hexchar_exn of char;;

let;;t_of_hexchar c=
   try List.assoc c
   [
     ('0', 0); ('1', 1); ('2', 2); ('3', 3); ('4', 4); ('5', 5); 
     ('6', 6); ('7', 7); ('8', 8); ('9', 9);
     ('A', 10); ('B', 11); ('C', 12); ('D', 13); ('E', 14); ('F', 15)
   ]  
   with _->raise(Hexchar_exn(c));;

let;;t_of_hex s=
  let n=String.length(s) 
  and accu:=ref(0);;
  for i=1 to n do accu:=int_of_hexchar(String.get s (j-1))+16*(!accu) done;
  return(!accu);;
  
let z1=Charset.unix_filename_admissible_characters;;
let z2=Image.image (fun c->let s=String.make 1 c;; (s,s) ) 
  unix_filename_admissible_characters;;
  
let ioh=int_of_hex;;

let tf1 k=
  if k<=127 then [k] else
  if k<=2047 then let t=(k-128)/64;; [194+t;k-64*t] else
  if k<=4095 then let t=(k-2048)/64;; [224;160+t;k-1920-64*t] else
  if k<=8191 then let t=(k-4096)/64;; [225;128+t;k-3968-64*t] else
  if k<=k    then let t=(k-8192)/64;; [226;128+t;k-8064-64*t] else [];;
  
let tf2 k=
 (Printf.sprintf "%X" k,
  String.concat " " (image (Printf.sprintf "%x") (tf1 k)) )  
  ;;

let tf3 s=tf2(int_of_hex s);;  



*)

(*


let u1=Directory_name.of_string "~/Documents/html_files";;
let u2=More_unix.complete_ls_with_nondirectories_only u1;;
let u3=image Absolute_path.to_string u2;;


let u4=image Charset.unix_unknowns_in_string u3;;
let u5=ofo(Tidel.diforchan(List.flatten u4));;



let example=Slow_copy_task.initialize_from_data
  (7822,
   "tribunem@tribunemicael.net",
   "/Users/ewandelanoy/Documents/",
   "~/private_html/",
   Directory_name.of_string "/Users/ewandelanoy/Documents/html_files",
   "Remembered/task_keeper1");;

let act=Slow_copy_task.execute_all_steps example;;


let u6=image (fun t->
  let r1=Utf_eight.unicode_point t;;
  let r2=int_of_string("0x"^r1);;
  (t,r1,r2,Utf_eight.encode r2)
) u5;;

let u4=image Strung.explode u3;;
let u5=ofo(Tidel.diforchan(List.flatten u4));;
let u6=List.filter (
   fun c->
   (not(List.mem c Charset.unix_filename_admissible_characters))
   &&
   (not(List.mem c (image fst Charset.list_for_unix_rewriting)))
) u5;;
let u7=Explicit.filter(
  fun s->List.mem '\128' (Strung.explode s)
) u3;;

let u8=List.hd u7;;
let u9=itv u8 83 85;;

let example=Slow_copy_task.initialize_from_file (Absolute_path.of_string "Remembered/task_keeper1");;



let z1=example.Slow_copy_task.filenames;;
let (z2,z3)=List.partition Charset.is_unix_filename_admissible z1;;


example.Slow_copy_task.filenames<-(List.hd z3)::(example.Slow_copy_task.filenames);;

example.Slow_copy_task.filenames<-List.tl(example.Slow_copy_task.filenames);;

let act=Slow_copy_task.execute_all_steps example;;


let act=Slow_copy_task.execute_one_step example;;


let print (CR(a,b))=
  let s1=string_of_int(a.Lexing.pos_cnum)
  and s2=string_of_int(b.Lexing.pos_cnum);;
  "char_range("^s1^","^s2^")";;

let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;    


*)

(*

let old_classical_parser elt=
  let f=(fun l->
     let opt2=Termite.parse (Termite.of_string elt.unadbriged_content) l in
     if opt2=None then None else
     let (l2,cr2,peurrest)=Option.unpack opt2 in
     let catalyser_check=(
       if elt.catalyser=""
       then true
       else (Termite.parse (Termite.of_string elt.catalyser) peurrest)<>None
     ) in
     if catalyser_check
     then Some(elt.helper l2 cr2,cr2,peurrest)
     else None
  ) in
  (f : t Php_parser.t);;
   
let bad_cases=ref[];;

let classical_parser elt l=
    try (
      old_classical_parser elt l 
    ) with
    _->let _=(bad_cases:=[elt,l]) in
     failwith("It is the animal");;

*)     

(*
let encode_ctoken=function
   Php_constant_token.c_kwd(kwd)->(Some(kwd),None,None)
  |Php_constant_token.c_punct(pct)->(None,Some(pct),None)
  |Php_constant_token.c_op(op)->(None,None,Some(op));;

exception Decode_ctoken_exn;;

let decode_ctoken (opt1,opt2,opt3)=
   if opt1<>None
   then Php_constant_token.c_kwd(Option.unpack opt1)
   else 
   if opt2<>None
   then Php_constant_token.c_punct(Option.unpack opt2)
   else 
   if opt3<>None
   then Php_constant_token.c_op(Option.unpack opt3)
   else raise(Decode_ctoken_exn);;  

let encode_ptoken ptok=match ptok with
  Php_projected_token.Constant(ctok)->(encode_ctoken ctok,Php_projected_token.Int)
  |_->((None,None,None),ptok);; 

let decode_ptoken (w,ptok1)=
    if w=(None,None,None)
    then ptok1
    else Php_projected_token.Constant(decode_ctoken w);;  

let encode_token tok=
    (encode_ptoken (tok.Php_token.form),tok.Php_token.content);;

let decode_token (frm,ctnt)=
   {
    Php_token.form=decode_ptoken(frm);
    Php_token.content=ctnt;
   };;
   
let encode_postoken (Positioned_php_token.PPL(x,y))=
    (encode_token x,y);;

let decode_postoken (x1,y)=
  (Positioned_php_token.PPL(decode_token x1,y));;
      
let encode_postokenlist x=
    Image.image encode_postoken
    (x.Positioned_php_token_list.contained);;

let decode_postokenlist l=  
  { Positioned_php_token_list.contained=
     Image.image decode_postoken l };;
*)


(*

let niy=[];;

let ld_leaf sel=niy;;

let ld_generalized old_f gen rcgzr=niy;;

exception Empty_chain;;

let ld_chain old_f l=
   if l=[] then raise(Empty_chain) else
   let h=List.hd(l) and peurrest=List.tl(l) in
   let temp1=old_f(h) in
   Image.image (fun (x,y)->
   (x,Php_constructible_recognizer.Chain (y::peurrest))) temp1;;

exception Nondisjoint of  Php_constructible_recognizer.t list;;

let ld_disjunction old_f l=
    let temp1=List.flatten(Image.image old_f l) in
    let temp2=Image.image fst temp1 in
    let temp3=Uple.list_of_pairs temp2 in
    if List.exists (fun (x,y)->
      not(Php_projected_token_set.kengeij_goullo x y)
    ) temp3
    then raise(Nondisjoint(l))
    else 
    let temp4=Ordered.diforchan_plaen Php_projected_token_set.order temp2 in
    let temp5=Image.image (fun x->(x,List.assoc x temp1)) temp4 in
    temp5;;


let rec ld =function
   Php_constructible_recognizer.Leaf(sel)->ld_leaf sel
  |Php_constructible_recognizer.Generalized(gen,rcgzr)->ld_generalized ld gen rcgzr
  |Php_constructible_recognizer.Chain(l)->ld_chain ld l
  |Php_constructible_recognizer.Disjunction(l)->ld_disjunction ld l
  |Php_constructible_recognizer.End_already_reached->[];;




*)

(*

<div class="container">
<button class="btn btn-primary btn-lg"
data-toggle="modal" data-target="#myModal">
Click me
</button>
<div class="modal fade" id="myModal">
       <div class="modal-dialog">
           <div class="modal-content">
               <div class="modal-header">
                   <button type"button" class="close"
                   data-dismiss="modal"></button>
                   <h4 class="modal-title" id="myModalLabel">Modal Title</h4>    
               </div>    
               <div class="modal-body">
                   This is the modal body. 
               </div>  
               <div class="modal-footer">
                       <button type"button" 
                       class="btn btn-default"
                       data-dismiss="modal">Close </button>
                       <button type"button" 
                       class="btn btn-primary"
                       data-dismiss="modal">Save changes </button>
               </div>                 
           </div>    
       </div>
</div>=


*)

(*

let analize_item s=
       let temp1=List.assoc s (Php_spider.php()) in
       if List.length (temp1)<2
       then []
       else 
       let temp2=Ennig.index_everything temp1 in
       let temp3=Image.image(
           fun (j,t)->
              let sj=s^"_"^(string_of_int j) in
              let named_rcgzr=of_name sj in
              let rcgzr=named_rcgzr.unnamed_content in
              (t,fst(Php_constructible_recognizer.big_head_tail_decomposition rcgzr))
       ) temp2 in
       let temp4=Uple.list_of_pairs temp3 in
       let temp5=List.filter (
          fun ((_,l1),(_,l2))->Listennou.comparable_for_prefix_order l1 l2
       ) temp4 in
       temp5;;
    
    let analize_all_items ()=
        let temp1=Php_spider.php() in
        let temp2=List.filter (fun (s,l)->List.length(l)>1) temp1 in
        Option.filter_and_unpack(
             fun (s,_)->
               let l=analize_item s in 
               if l=[] 
               then None
               else Some(s,analize_item s)
        ) temp2;;


*)


(*

let rec helper_for_bhtd (graet,da_ober)=
  let (aew,l)=head_tail_decomposition da_ober in
  if aew||(List.length(l)<>1)
  then (List.rev graet,da_ober)
  else let (a,peurrest)=List.hd(l) in
       helper_for_bhtd (a::graet,peurrest);; 

let big_head_tail_decomposition x=helper_for_bhtd ([],x);;         

let rec iterator_for_exhaustion (j,graet,da_ober)=
  match da_ober with
  []->(j,graet)
  |a::peurrest->
    let tempf=(
        fun (idx,rcgzr)->
          let (_,ttemp1)=head_tail_decomposition rcgzr in
          let ttemp2=Option.filter_and_unpack (
             fun (tokset,rcgzr2)->
               if Php_projected_token_set.test tokset (Php_token.form(fst a))
               then Some(rcgzr2)
               else None
          ) ttemp1 in
          if ttemp2=[]
          then None
          else Some(idx,disjunction ttemp2)
    ) in
    let temp3=Option.filter_and_unpack tempf graet in
    if temp3=[]
    then (j,graet)
    else iterator_for_exhaustion (j+1,temp3,peurrest);;

let exhaust l (arg:Php_positioned_token_list.t)=
  iterator_for_exhaustion (0,Ennig.index_everything l,arg);;


*)

(*
INSERT INTO `mysql_table_fake_users` SELECT * FROM `mysql_table_users` 
WHERE (user_id>=378) AND (user_id<=488);

DELETE FROM `mysql_table_users` WHERE (user_id>=378) AND (user_id<=488);


INSERT INTO `mysql_table_fake_users` SELECT * FROM `mysql_table_users` 
WHERE user_id=161;

DELETE FROM `mysql_table_users` WHERE user_id=161;

DELETE FROM `mysql_table_fake_users` WHERE user_id<=175;


204.12.207.34
*)
