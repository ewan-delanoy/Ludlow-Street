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
   "statement",[
                 "@ id () ;";
                 "abstract class _l_ no_left_brace _r*_ {}";
                 "class _l_ no_left_brace _r*_ {}";
                 "cmt";
                 "declare () ;";
                 "echo dqs ;";
                 "echo id () . dqs ;";
                 "echo id () ?: sqs ext";
                 "echo id () ext";
                 "echo id ;";
                 "echo sqs . id () . sqs ;";
                 "echo sqs ;";
                 "echo vvar -> id () -> id () ;";
                 "echo vvar -> id () ext";
                 "echo vvar [] -> id () ;";
                 "echo vvar [] -> id () ext";
                 "echo vvar ext";
                 "end_of_file";
                 "exit ;";
                 "ext";
                 "final class _l_ no_left_brace _r*_ {}";
                 "foreach () : _l_ no_breach _r*_ endforeach ;";
                 "foreach () : _l_ no_breach _r*_ endforeach ext";
                 "foreach () {}";
                 "function id () {}";
                 "id () ;";
                 "id :: id () -> id () _l_ -> id () _r*_  ;";
                 "id :: id () ;";
                 "if () : _l_ no_ivies _r+_ else : _l_ no_ivies _r+_ endif ext";
                 "if () : _l_ no_ivies _r+_ else : _l_ no_ivies _r+_ if () {} if () {} _l_ no_ivies _r+_ endif ext";
                 "if () : _l_ no_ivies _r+_  endif ;";
                 "if () : _l_ no_ivies _r+_ endif ext";
                 "if () : _l_ no_ivies _r+_ if () : _l_ no_ivies _r*_ else : _l_ no_ivies _r*_ endif ; _l_ no_ivies _r*_ endif ;";
                 "if () : _l_ no_ivies _r+_ if () : _l_ no_ivies _r+_ endif _l_ no_ivies _r+_ endif ;";
                 "if () : _l_ no_ivies _r+_ if () {} _l_ no_ivies _r+_ if () {} _l_ no_ivies _r+_ if () {} _l_ no_ivies _r+_ endif ext";
                 "if () {} class id extends id {}";
                 "if () {} echo id ;";
                 "if () {} else if () {} _l_ else if () {} _r*_ else {}";
                 "if () {} else {}";
                 "if () {} end_of_file";
                 "if () {} ext";
                 "if () {} if () {} include_like id () . sqs ;";
                 "if () {} include_like vvar ;";
                 "if () {} return new id () ;";
                 "if () {} vvar = new id () ;";
                 "if () {} vvar = require id . sqs ;";
                 "include_like id () . sqs ;";
                 "include_like id . sqs ;";
                 "include_like stringy _l_ stringy _r*_ ;";
                 "interface _l_ no_left_brace _r*_ {}";
                 "namespace id ;";
                 "namespace id {}";
                 "namespace nmspc ;";
                 "namespace nmspc {}";
                 "namespace {}";
                 "nmspc :: id () ;";
                 "return function () {} ;";
                 "return id () ;";
                 "return new nmspc () ;";
                 "return vvar ;";
                 "switch () {}";
                 "trait id {}";
                 "try {} catch () {}";
                 "use _l_ no_semicolon _r*_ ;";
                 "vvar -> id () -> id () _l_ -> id () _r*_ ;";
                 "vvar -> id () ;";
                 "vvar -> id = sqs ;";
                 "vvar [] -> id () ext";
                 "vvar [] = & vvar ;";
                 "vvar [] = & vvar [] ;";
                 "vvar [] = & vvar [] [] ;";
                 "vvar [] = id ;";
                 "vvar [] = sqs ;";
                 "vvar [] = vvar [] ;";
                 "vvar [] = vvar [] [] ;";
                 "vvar assign () ;";
                 "vvar assign (bool) id () ;";
                 "vvar assign hdoc ;";
                 "vvar assign id () . sqs ;";
                 "vvar assign id () ;";
                 "vvar assign id () ?: int ;";
                 "vvar assign id () ?: sqs ext";
                 "vvar assign id () ext";
                 "vvar assign id . sqs ;";
                 "vvar assign id :: id () ;";
                 "vvar assign id ;";
                 "vvar assign id = sqs ;";
                 "vvar assign int ;";
                 "vvar assign ndoc ;";
                 "vvar assign new id () ;";
                 "vvar assign require id . sqs ;";
                 "vvar assign sqs . vvar . sqs ;";
                 "vvar assign sqs ;";
                 "vvar assign vvar -> id () ;";
                 "vvar assign vvar . sqs ;";
                 "vvar assign vvar ;";
                 "vvar assign vvar = sqs ;";
                 "vvar assign vvar === sqs || vvar === sqs ?: sqs ext";
                 "vvar assign vvar [] ;";
                 "while () {}"
               ]


]);;


(* Definition of PHP spider ends here *)

let php ()=unveil(!php_ref);;


let padding=3;;

let print_spider_item (s,l)=
    let n=String.length(s) in
    let padder=String.make padding ' ' in    
    padder^(Strung.enclose s)^","^
    (Copyable_printing.print_stringlist (n+padding+3) l);;

let print_spider (Sp l)=
   let temp1=Image.image print_spider_item l in
   let temp2=String.concat ";\n\n" temp1 in
   temp2;;

let helper_for_rememberance new_spider=
  "\n\n\n let php_ref=ref(Sp[\n"^(print_spider new_spider)^"\n\n\n]);;\n\n\n";;

let order_for_branches=((
    fun s1 s2->
      let l1=List.filter(fun t->t<>"")(Str.split (Str.regexp_string " ") s1) 
      and l2=List.filter(fun t->t<>"")(Str.split (Str.regexp_string " ") s2) in
      Total_ordering.lex_for_string_lists l1 l2
): string Total_ordering.t);;

let sort_branch l=Ordered.diforchan_plaen order_for_branches l;;

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
   Sp(Image.image (fun (s,_)->(s,sort_branch(List.assoc s l)) ) better_ordered)
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
   
    let substitute_dependencies (s,l)=
      let temp=Sp(Image.image (fun (s1,l1)->if s1<>s then (s1,l1) else (s,l)) (php()) ) in
      let new_spider=check_dependencies temp in
      change_and_remember new_spider;;  

    let see_item s=
       let temp1=List.assoc(s)(php()) in
       let temp2=Ennig.index_everything temp1 in
       let temp3=Image.image (fun (j,s)->
         (Strung.left_completed_string_of_int 3 j)^":"^(Strung.enclose s) ) temp2 in
       let temp4="\n\n\n"^(String.concat "\n" temp3)^"\n\n\n" in
       print_string temp4;;
    
    let pair_is_bad (t1,t2)=
      let current_list=php() in
      let l1=List.filter(fun t->t<>"")(Str.split (Str.regexp_string " ") t1) 
      and l2=List.filter(fun t->t<>"")(Str.split (Str.regexp_string " ") t2)  in
      let tempf1=(fun s->try Some(List.assoc s current_list) with _->None)   in
      let rec tempf2=(
          (* by construction, gl1 and gl2 are always non-equal *)
          fun (gl1,gl2)->
            if (gl1=[])||(gl2=[])
            then false
            else  let (a1,peurrest1)=Listennou.ht gl1 
                  and (a2,peurrest2)=Listennou.ht gl2 in
                  let opt1=tempf1 a1
                  and opt2=tempf1 a2 in
                  if ((opt1=None)||(opt2=None))
                  then true   
                  else 
                  if opt1=opt2
                  then tempf2(peurrest1,peurrest2)
                  else false
      ) in
      if l1=l2
      then true
      else tempf2(l1,l2);;
    
    

end;;  
  
let php=Private.php;;
let add_dependencies=Private.add_dependencies;;
let erase_item=Private.erase_item;;  
let remove_dependencies=Private.remove_dependencies;; 
let substitute_dependencies=Private.substitute_dependencies;; 
let see_item=Private.see_item;;    

