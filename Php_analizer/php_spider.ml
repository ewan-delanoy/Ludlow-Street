(*

#use"Php_analizer/php_spider.ml";;

Partial grammar definition, with no recursion
(but with references to previously defined terminals. )

*)

module Private=struct

type t=Sp of (string*(string list)) list;;

exception Cycle of string list;;
exception Unregistered_dependencies of (string*string) list;;


let unveil (Sp l)=l;;

(* Definition of PHP spider begins here *)


 let php_ref=ref(Sp[
   "optional_pblock",["_l_ () _r?_"];

   "beheaded_varan",[
                      ";";
                      "-> id_or_var optional_pblock  _l_ -> id_or_var optional_pblock  _r*_ ;"
                    ];

   "beheaded_iwy",[
                    "_l_ no_ivies _r*_ if () : _l_ no_ivies _r*_ else : _l_ no_ivies _r*_ endif ; _l_ no_ivies _r*_";
                    "_l_ no_ivies _r*_ if () : _l_ no_ivies _r*_ endif ; _l_ no_ivies _r*_";
                    "_l_ no_ivies _r*_"
                  ];

   "beheaded_droid",[
                      "-> id () _l_ -> id () _r*_  ;";
                      ";"
                    ];

   "beheaded_foreach",[
                        ":  _l_ no_breach _r*_  endforeach ;";
                        "{}"
                      ];

   "beheaded_ivwy",[
                     ": beheaded_iwy endif ;";
                     "exit ;";
                     "{} _l_ else if () {} _r*_ else {}"
                   ];

   "namespace_name",["_l_ id _u_ nmspc _rd_"];

   "assignable",[
                  "coerce           id ()";
                  "nmspc            _l_ :: id _r?_ optional_pblock";
                  "id ::            id ()";
                  "id () ?: no_semicolon";
                  "id () .          sqs";
                  "id ()            ";
                  "hdoc ";
                  "include_like     _l_ loose= _r*_ ";
                  "int          ";
                  "new id           ()";
                  "new nmspc        ()";
                  "sqs . vvar . dqs . vvar -> id () . dqs . vvar -> id () . dqs";
                  "sqs . vvar . sqs";
                  "sqs";
                  "vvar [ sqs ]";
                  "vvar . sqs";
                  "vvar = sqs";
                  "vvar -> id optional_pblock _l_ -> id optional_pblock _r*_";
                  "vvar + _l_ loose= _r*_ ";
                  "vvar";
                  "@                id ()";
                  "() ?  string_or_var  :  string_or_var  "
                ];

   "statement",[
                 "@ id () ;";
                 "abstract class _l_ no_left_brace _r*_ {}";
                 "class _l_ no_left_brace _r*_ {}";
                 "declare () ;";
                 "echo no_var _l_ no_semicolon _r*_ ;";
                 "echo vvar ext";
                 "exit ;";
                 "ext";
                 "final class _l_ no_left_brace _r*_ {}";
                 "foreach () beheaded_foreach";
                 "function id () {}";
                 "id :: id () beheaded_droid";
                 "id :: id =  assignable ;";
                 "if () beheaded_ivwy";
                 "include_like _l_ stringy _r*_ ;";
                 "interface _l_ no_left_brace _r*_ {}";
                 "namespace id ;";
                 "namespace id {}";
                 "namespace nmspc ;";
                 "namespace nmspc {}";
                 "namespace {}";
                 "nmspc :: id () ;";
                 "return _l_ no_semicolon _r*_ ;";
                 "static vvar assign assignable ;";
                 "switch () {}";
                 "trait id {}";
                 "try {} catch () {}";
                 "use _l_ no_semicolon _r*_ ;";
                 "vvar -> id_or_var  =  assignable ;";
                 "vvar -> id_or_var () beheaded_varan";
                 "vvar [  int_or_string_or_var   ]  =   & assignable  ;";
                 "vvar [  int_or_string_or_var  ] = assignable ;";
                 "vvar [ ] = assignable ;";
                 "vvar assign  assignable ;";
                 "vvar assign & assignable ;";
                 "while () {}"
               ]


]);;


(* Definition of PHP spider ends here *)

let php ()=unveil(!php_ref);;

let print_stringlist_naively l=
  let temp1=Image.image (fun s->(Strung.enclose s)) l in
  let temp2=String.concat ";" temp1 in
  "["^temp2^"]";;  

let print_stringlist_with_offset l w=
    let offset=String.make w ' ' in
    let temp1=Image.image (fun s->offset^"  "^(Strung.enclose s)) l in
    let temp2=String.concat ";\n" temp1 in
    ("[\n")^temp2^"\n"^offset^"]";;

let print_stringlist w l=
     if List.length(l)=1
     then print_stringlist_naively l
     else print_stringlist_with_offset l w;;

let padding=3;;

let print_spider_item (s,l)=
    let n=String.length(s) in
    let padder=String.make padding ' ' in    
    padder^(Strung.enclose s)^","^
    (print_stringlist (n+padding+3) l);;

let print_spider (Sp l)=
   let temp1=Image.image print_spider_item l in
   let temp2=String.concat ";\n\n" temp1 in
   temp2;;

let helper_for_rememberance new_spider=
  "\n\n\n let php_ref=ref(Sp[\n"^(print_spider new_spider)^"\n\n\n]);;\n\n\n";;

let change_and_remember new_spider=
    let this_file=Absolute_path.of_string "Php_analizer/php_spider.ml" in
    (
      php_ref:=new_spider;
      Replace_inside.overwrite_between_markers_inside_file
       (Overwriter.of_string (helper_for_rememberance new_spider))
       (
         "(* Definition of PHP spider begins here *)",
         "(* Definition of PHP spider ends here *)"
       )
       this_file
    );;   

let check_dependencies (Sp l)=
   let naively_ordered=Image.image fst l in
   let table_for_coatoms=Image.image(
      fun s->
        let ttemp1=List.assoc s l in
        let ttemp2=Image.image Php_short_selector.dependencies ttemp1 in
        let ttemp3=Ordered_string.big_teuzin ttemp2 in
        let ttemp4=Ordered_string.forget_order ttemp3 in
        (s,ttemp4)
   ) naively_ordered in
   let temp5=Image.image(fun (s,coatoms)->
      let ttemp7=Ordered.filter(fun x->not(List.mem x naively_ordered)) 
       (Ordered.unsafe_set(coatoms))  in
      Ordered.image (fun t->(s,t)) ttemp7) 
     table_for_coatoms in
   let temp6=List.flatten temp5 in
   if  temp6<>[] then raise(Unregistered_dependencies(temp6)) else
   let coat_function=Memoized.make(fun s->List.assoc s table_for_coatoms) in
   let (cycles,better_ordered)=
      Reconstruct_linear_poset.reconstruct_linear_poset 
       coat_function naively_ordered in
   if cycles<>[] then raise(Cycle(List.hd cycles)) else
   Sp(Image.image (fun (s,_)->(s,List.assoc s l) ) better_ordered)
   ;;
  
  let temporary_spider_for_insertion  (s,l)=
    let temp1=php() in
    match Option.seek(fun (s1,_)->s1=s)(temp1) with
     Some(_,l1)->let l2=Ordered.diforchan_plaen Total_ordering.lex_for_strings (l1@l) in
                 let temp2=Image.image (fun (s3,l3)->if s3=s then (s,l2) else (s3,l3)) temp1 in
                 Sp(temp2)
    |None->Sp((s,l)::temp1);;




  let add_dependencies (s,l)=
    let temp=temporary_spider_for_insertion  (s,l) in
    let new_spider=check_dependencies temp in
    change_and_remember new_spider;;
  
  let erase_item s=
     let temp=Sp(List.filter (fun (s1,_)->s1<>s) (php()) ) in
     let new_spider=check_dependencies temp in
     change_and_remember new_spider;;  


  let remove_dependencies (s,l)=
      let temp1=php() in
      match Option.seek(fun (s1,_)->s1=s)(temp1) with
      None->()
      |Some(_,l1)->
         let l2=List.filter (fun t->not(List.mem t l)) l1 in
         let new_list=(
          if l2=[]
          then List.filter (fun (s1,_)->s1<>s) temp1
          else Image.image (fun (s1,l1)->
               if s1<>s 
              then (s1,l1)
              else (s,l2)
          ) temp1
         ) in 
        let new_spider=check_dependencies(Sp(new_list)) in
        change_and_remember new_spider;; 
   
    let see_item s=
       let temp1=List.assoc(s)(php()) in
       let temp2=Ennig.index_everything temp1 in
       let temp3=Image.image (fun (j,s)->
         (Strung.left_completed_string_of_int 2 j)^":"^(Strung.enclose s) ) temp2 in
       let temp4="\n\n\n"^(String.concat "\n" temp3)^"\n\n\n" in
       print_string temp4;;
   
end;;  
  
let php=Private.php;;
let add_dependencies=Private.add_dependencies;;
let erase_item=Private.erase_item;;  
let remove_dependencies=Private.remove_dependencies;; 
let see_item=Private.see_item;;    

