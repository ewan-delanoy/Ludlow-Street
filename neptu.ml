(*

#use"neptu.ml";;


*)



(*

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


(*
let z1=(!(German_wrapper.Private.directories_ref));;

let z2=image (
  fun sd->let s=Subdirectory.connectable_to_subpath sd in
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
   fun x->let i=Substring.leftmost_index_of_in ".fr." x in
    (itv x 1 (i-1),Cull_string.cobeginning (i+3) x)
) g4;;

let g6=Explicit.image(
   fun (s1,s2)->
      let temp=Option.filter_and_unpack (
         fun z->
            if (Substring.begins_with z s1)&&(Substring.ends_with z s2)
            then let t=itv z (String.length(s1)+1) (String.length(z)-String.length(s2)) in
                 Some(t)
            else None
      ) g3 in
      (temp,s1,s2)
) g5;;

let g7=List.filter (fun (l,s1,s2)->not(List.mem "." l)) g6;;

let g8=image(
  fun (l,t1,t2)->
  let s1=Str.global_replace (Str.regexp_string " ") "\\ " t1
  and s2=Str.global_replace (Str.regexp_string " ") "\\ " t2 in 
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
  and s2=Str.global_replace (Str.regexp_string " ") "\\ " t2 in 
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

let soi i=let s=string_of_int i in if i<10 then "0"^s else s;;


let g2=doyle (
   fun i->
    let s=soi i in
   ("<a href=\"http://strobertbellarmine.net/wilhelm_scannell_"^s^".html",
    "<a href=\"wilhelm_"^s^".html")
) 1 19;;

let act1=Replace_inside.replace_several_inside_file g2 g1;;

let g3=doyle (
   fun i->
    let s=string_of_int i and t=soi(19+i) in
   ("<a href=\"http://strobertbellarmine.net/wilhelm_scannell_2_"^s^".html",
    "<a href=\"wilhelm_"^t^".html")
) 1 7;;

let act2=Replace_inside.replace_several_inside_file g3 g1;;

let g4=(
  "<a href=\"http://strobertbellarmine.net/index.htm\">Home</a>",
  "<a href=\"wilhelm_main.html\">Home</a>"
);;

let g5=doyle (fun k->
   let s=soi k in
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
   and siii=string_of_int(i+2) in
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
   let s1=List.hd l in
   let s2=tower^(Cull_string.cobeginning 7 s1) in
   let s3=Cull_string.cobeginning 86 s2 in
   let s4=List.hd(Str.split (Str.regexp_string"_") s3) in
   let s5=String.capitalize(Cull_string.coending 1 s4) in
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

let g1= "<?php id ( variable [ integer ] ) ? (int) variable [ integer ] : integer";;
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

variable = id ( variable [ integer ] ) ? (int) variable [ integer ] : integer ;

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
  let (a,b,c)=Option.unpack y in
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
   let tok=Positioned_php_token.fst ptok in
   not(Php_token.is_a_comment tok)
)(Php_lexer.parse_file t);;


let u1=Directory_name.of_string
"/Users/ewandelanoy/Documents/Sites/Symblog/symblogproject/";;
let u2=More_unix.complete_ls u1;;
let u3=List.filter(
   fun ap->
     let s_ap=Absolute_path.to_string ap in
     Substring.ends_with s_ap ".php"
) u2;;
let u4=Explicit.image (commentless_lexer) u3;;




let u5=Explicit.image (fun x->(x,Level_one.level_one x)) u4;;

let viz1=Explicit.filter(fun (x,y)->y=None) u5;;
let u6=Explicit.image (fun (x,y)->
  let (a,b,c)=Option.unpack y in
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



let barley1="_l_ _l_ include_like _u_ new _u_ @ _rd_ _r?_";;
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
   let tok=Php_token.Constant(cst) in
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

let indexed_whole=
   List.flatten (
   [
    image (fun x->(1,x)) part1;
    image (fun x->(2,x)) part2
   ]
   );;
let n1=hi indexed_whole;;   
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
        let s=Ocaml_gsyntax_item.name itm in
        let j=String.index(s)('.')+1 in
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
    let nm=Half_dressed_module.naked_module hm in
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
   let (a,b,c)=Gparser_for_ocaml_language.data_for_prsr_for_comment in
   Gparser_house_with_doors.hwd (a,b) c s2 1;;

let v0=
let (a,b,c)=Gparser_for_ocaml_language.data_for_prsr_for_comment in
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
   let (a,b,c)=Gparser_for_ocaml_language.data_for_prsr_for_comment in
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
    let a=(k+2)/3 in
    let b=min(a+100)(n1) in
    (z,itv txt1 (a-2) b)
) u9;;

let u9=image (fun l->(List.hd(l)<>("0A ", "\n"),l) ) u8;;
let u10=image (fun (bowl,l)->
   if bowl
   then let temp1=Listennou.constant_slices (fun t->List.mem(fst t) first_order) l in
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

let int_of_hexchar c=
   try List.assoc c
   [
     ('0', 0); ('1', 1); ('2', 2); ('3', 3); ('4', 4); ('5', 5); 
     ('6', 6); ('7', 7); ('8', 8); ('9', 9);
     ('A', 10); ('B', 11); ('C', 12); ('D', 13); ('E', 14); ('F', 15)
   ]  
   with _->raise(Hexchar_exn(c));;

let int_of_hex s=
  let n=String.length(s) 
  and accu:=ref(0) in
  for i=1 to n do accu:=int_of_hexchar(String.get s (j-1))+16*(!accu) done;
  return(!accu);;
  
let z1=Charset.unix_filename_admissible_characters;;
let z2=Image.image (fun c->let s=String.make 1 c in (s,s) ) 
  unix_filename_admissible_characters;;
  
let ioh=int_of_hex;;

let tf1 k=
  if k<=127 then [k] else
  if k<=2047 then let t=(k-128)/64 in [194+t;k-64*t] else
  if k<=4095 then let t=(k-2048)/64 in [224;160+t;k-1920-64*t] else
  if k<=8191 then let t=(k-4096)/64 in [225;128+t;k-3968-64*t] else
  if k<=k    then let t=(k-8192)/64 in [226;128+t;k-8064-64*t] else [];;
  
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
  let r1=Utf_eight.unicode_point t in
  let r2=int_of_string("0x"^r1) in
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
  and s2=string_of_int(b.Lexing.pos_cnum) in
  "char_range("^s1^","^s2^")";;

let print_out (dummy:Format.formatter) x=
   Format.open_box 0;
   Format.print_string(print x);
   Format.close_box();;    


*)

(*
INSERT INTO `mysql_table_fake_users` SELECT * FROM `mysql_table_users` 
WHERE (user_id>=302) AND (user_id<=377);

DELETE FROM `mysql_table_users` WHERE (user_id>=302) AND (user_id<=377);


INSERT INTO `mysql_table_fake_users` SELECT * FROM `mysql_table_users` 
WHERE user_id=161;

DELETE FROM `mysql_table_users` WHERE user_id=161;

DELETE FROM `mysql_table_fake_users` WHERE user_id<=175;


204.12.207.34
*)
