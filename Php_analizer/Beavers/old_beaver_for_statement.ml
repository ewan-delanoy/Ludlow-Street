(*

#use"Php_analizer/Beavers/beaver_for_statement.ml";;

*)


type plex=Positioned_php_token.t;;
type plexl=plex list;;

type php_var=Positioned_php_token.t;;
type php_assign_op=Positioned_php_token.t;;
type php_class=Positioned_php_token.t;;
type php_static_method=Positioned_php_token.t;;
type php_class_property=Positioned_php_token.t;;
type php_fun_name=Positioned_php_token.t;;
type php_interface_name=Positioned_php_token.t;;
type php_trait_name=Positioned_php_token.t;;
type php_namespace=Positioned_php_token.t;;
type php_index=Positioned_php_token.t;;


type t=
    External_echo of string*Php_char_range.t
   |Comment of string*Php_char_range.t
   |Class_decl of Php_class_modifier.t*plexl*plexl*Php_char_range.t
   |Ivy of plexl*plexl*plexl*Php_char_range.t
   |Script_inclusion of Php_script_includer.t*plexl*Php_char_range.t
   |Assignment of php_var*php_assign_op*plexl*Php_char_range.t
   |Assignment_on_class_property of php_var*php_class_property*php_assign_op*plexl*Php_char_range.t
   |Static_assignment of php_var*php_assign_op*plexl*Php_char_range.t
   |Yuze_decl of plexl*Php_char_range.t
   |Static_method_call of php_class*php_static_method*plexl*Php_char_range.t
   |Nonstatic_method_call of plexl*php_static_method*plexl*Php_char_range.t
   |Class_property_call of plexl*php_class_property*Php_char_range.t
   |TryCatch of plexl*plexl*plexl*Php_char_range.t
   |Fun_def of php_fun_name*plexl*plexl*Php_char_range.t
   |Returning of plexl*Php_char_range.t
   |Interface_decl of php_interface_name*plexl*plexl*Php_char_range.t
   |Fun_call of bool*php_fun_name*plexl*Php_char_range.t
   |Namespace_def of php_namespace*Php_char_range.t
   |Appending of php_var*plexl*Php_char_range.t
   |Cell_assignment of php_var*php_index*plexl*Php_char_range.t
   |Trait_decl of php_trait_name*plexl*Php_char_range.t
   |Echo of plexl*Php_char_range.t
   |Exit of Php_char_range.t
   |WhileLoop of plexl*plexl*Php_char_range.t
   |ForeachLoop of plexl*plexl*Php_char_range.t
   |NamespaceBlock of (plex option)*plexl*Php_char_range.t
   |Declare of plexl*Php_char_range.t
   |Switch of plex*plexl*Php_char_range.t;;
   
let char_range=function
    External_echo(_,cr)->cr
   |Comment(_,cr)->cr
   |Class_decl(_,_,_,cr)->cr
   |Ivy(_,_,_,cr)->cr 
   |Script_inclusion(_,_,cr)->cr 
   |Assignment(_,_,_,cr)->cr
   |Assignment_on_class_property(_,_,_,_,cr)->cr
   |Static_assignment(_,_,_,cr)->cr
   |Yuze_decl(_,cr)->cr
   |Static_method_call(_,_,_,cr)->cr 
   |Nonstatic_method_call(_,_,_,cr)->cr 
   |Class_property_call(_,_,cr)->cr
   |TryCatch(_,_,_,cr)->cr
   |Fun_def(_,_,_,cr)->cr
   |Returning(_,cr)->cr
   |Interface_decl(_,_,_,cr)->cr
   |Fun_call(_,_,_,cr)->cr
   |Namespace_def(_,cr)->cr
   |Appending(_,_,cr)->cr
   |Cell_assignment(_,_,_,cr)->cr
   |Trait_decl(_,_,cr)->cr
   |Echo(_,cr)->cr
   |Exit(cr)->cr
   |WhileLoop(_,_,cr)->cr
   |ForeachLoop(_,_,cr)->cr
   |NamespaceBlock(_,_,cr)->cr
   |Declare(_,cr)->cr
   |Switch(_,_,cr)->cr;;
     
let dummy=External_echo("",Php_char_range.dummy);;     
     

   
let special_string_order=
  (fun s1 s2->
    let bowl1=Substring.begins_with s1 "nonsemi_"
    and bowl2=Substring.begins_with s2 "nonsemi_" in
    if bowl1=bowl2
    then Dictionary_order.dictionary_order s1 s2
    else 
    if bowl1
    then Total_ordering.Greater
    else Total_ordering.Lower
  );;   
   
let classical_parser (_,(_,string_for_termite,f,_))=
  ((fun l->
     match Old_termite.parse (Old_termite.of_string string_for_termite) l with
     None->None
    |Some(l1,cr,peurrest)->Some(f l1 cr,cr,peurrest)
  ) : t Old_php_parser.t);;      
   
let order=Total_ordering.product special_string_order Total_ordering.standard;;   
   
   
let current_data_list=ref [];;
let shortcuts_list=ref ([]:(string*string) list);;


let add_data a b c d=
   let old_version=Ordered.safe_set order (!current_data_list) in
   let expanded_b=Replace_inside.replace_several_inside_string
      (!shortcuts_list) b  in
   let new_set=Ordered.insert order (a,(b,expanded_b,c,d)) old_version in
   let new_version=Ordered.forget_order new_set in 
   (current_data_list:=new_version);;
    
let update_data_list ()=
   let rewriter=(fun 
     (a,(b,_,c,d))->
     let new_expanded_b=Replace_inside.replace_several_inside_string
      (!shortcuts_list) b  in
      (a,(b,new_expanded_b,c,d))
   ) in
   let new_list=Image.image rewriter (!current_data_list) in
   current_data_list:=new_list;;    
    
let add_shortcut x y=
   (
     shortcuts_list:=(x,y)::(!shortcuts_list);
     update_data_list ()
   );;    
    
let current_main_parser=
  (
    fun l->Option.find_and_stop 
    (fun tr->classical_parser tr l) (!current_data_list)
  );;
    
    
let naive_individual_test_for_data helper_content helper s=
       let lexed=Old_php_lexer.parse_string s in
       let termiteparsed=Old_termite.parse (Old_termite.of_string helper_content) lexed in
       let (termited,cr,_)=Option.unpack termiteparsed in
       let helped=helper termited cr in
       helped=dummy;;
    
let  individual_test_for_data helper_content helper s=
  try naive_individual_test_for_data helper_content helper s
  with 
  _->true;;
    
let should_be_empty ()=
   let finder=(fun 
      (helper_name,(short_helper_content,helper_content,helper,l))->
       let ttemp2=List.filter (fun s->
        individual_test_for_data helper_content helper s
       ) l in
       Image.image (fun s->(helper_name,short_helper_content,helper_content,s)) ttemp2
   ) in
   let temp1=Image.image finder (!current_data_list) in
   List.flatten temp1;;   

let display_data ()=
   let m1=snd(Max.maximize_it(fun 
      (helper_name,(_,helper_content,helper,l))->
        String.length(helper_name) 
   ) (!current_data_list)) in  
   let temp1=Image.image (fun 
      (helper_name,(short_helper_content,_,helper,l))->
         let d=m1-(String.length(helper_name)) in
         let filler=String.make d ' ' in
         "\""^helper_name^"\""^filler^"   ,   \""^short_helper_content^"\""
   )(!current_data_list) in
   let temp2="\n\n\n"^(String.concat "\n" temp1)^"\n\n\n" in
   print_string temp2;;
   
   
let visualizer (helper_name,short_helper_content,helper_content,s)=
    let message=String.concat "\n"
       ([" ";helper_name]@
       (Image.image ( Ocaml_long_name.ocaml_long_name "") [short_helper_content;s])
       @[""]) in
    let _=print_string message in   
    let lexed=Old_php_lexer.parse_string s 
      and trmt=Old_termite.of_string helper_content in
      let revp=Old_termite.reverse_parse trmt lexed in
      match Old_termite.parse trmt lexed with 
      None->(None,revp)
      |Some(termited,_,_)->(Some(termited),revp);; 
            
let see ()=match (should_be_empty()) with
   []->(None,None)
   |a::_->visualizer a;;
 
    
let helper_for_assignment l1 cr=
  let a=List.hd(List.nth l1 0) 
  and b=List.hd(List.nth l1 1)  in
  Assignment(a,b,List.nth l1 2,cr);;   


   
exception Singleton_exn;; 
   
let helper_for_singleton l1 cr=
     match Positioned_php_token.fst(List.hd(List.hd l1)) with
       Php_token.External_echo(s)->External_echo(s,cr)
      |_->raise(Singleton_exn);;
   
add_data 
  "singleton"
  "ext"
  helper_for_singleton
  ["a+b"];;   


let helper_for_abstract_class l1 cr=
   Class_decl(Php_class_modifier.Abstract,List.nth l1 0,List.nth l1 1,cr);;
   
add_data
  "class_abstract"
  "abstract class _l_ no_left_brace _r*_ {}"
  helper_for_abstract_class
  [ "<?php abstract class Amy implements Peggy extends Betty {$cant=$be+$with;} "];;   
  


let helper_for_final_class l1 cr=
   Class_decl(Php_class_modifier.Final,List.nth l1 0,List.nth l1 1,cr);;
   
add_data 
   "class_final"
   "final class _l_ no_left_brace _r*_ {}"
   helper_for_final_class
   ["<?php final class Amy implements Peggy extends Betty {$cant=$be+$with;}"];;   
  
let helper_for_usual_class l1 cr=
   Class_decl(Php_class_modifier.Usual,List.nth l1 0,List.nth l1 1,cr);;
   
add_data
  "class_usual"
  "class _l_ no_left_brace _r*_ {}"
  helper_for_usual_class
  ["<?php class Amy implements Peggy extends Betty {$cant=$be+$with;}"];;   
  


let assignable1="_l_ _l_ include_like _u_ new _u_ @ _rd_ _r?_";;
let assignable2="_l_ _l_ -> id () _r+_ _u_ _l_ loose= _r*_ _rd_";;
let assignable3="variable "^assignable2;;

let assignable4="_l_ id _u_ nmspc _rd_";;
let assignable5="_l_ :: id _r?_";;
let assignable6=assignable4^" "^assignable5^" ()";;

let assignable=" _l_ "^(String.concat " _u_ " [assignable3;assignable6])^" _rd_";;

add_shortcut "assignable" assignable;;


add_data 
  "assign_to_simple"
  "variable ##( assign )## _l_ assignable _r*_ ;"
  helper_for_assignment
  [
    "<?php $winter=$snow+($melts*$down);";
    "<?php $bob=$amy->finds($x,$y)->beaver->eats($it)->again();";
    "<?php $jane += require_once $amy.PEGGY.'betty'.\"abc\";";
    "<?php $amy=@peggy($x,$y,$z);";
    "<?php $amy=new Peggy($x,$y,$z);";
	"<?php $classLoader = new \\Doctrine\\Common\\ClassLoader('Doctrine');";
	"<?php $bob=Debug::enable($dee,$mac);";
	"<?php $bob=\\Amy\\Mac\\Donald\\Debug :: walking;"
  ];;

let helper_for_static_assignment l1 cr=
  let a=List.hd(List.nth l1 0) 
  and b=List.hd(List.nth l1 1)  in
  Static_assignment(a,b,List.nth l1 2,cr);; 

add_data 
  "static_assignment"
  "static variable assign ##( id () )## ;"
  helper_for_static_assignment
  [
    "<?php static $x=array($y,$z);"
  ];;




let helper_for_servant_assign l1 cr=
   let tf=(fun j->List.hd(List.nth l1 j)) in
   Assignment_on_class_property(tf 0,tf 1,tf 2,List.nth l1 3,cr);;

add_data 
	"assign_on_servant"
	"variable -> id_or_var ##( assign )## _l_ loose= _r*_ ;"
	helper_for_servant_assign
	[
	  "<?php $foo->bar = 'bar'. $baz;"
	];;


add_data 
	"assign_on_static"
	"id :: id_or_var ##( assign )## _l_ loose= _r*_ ;"
	helper_for_servant_assign
	[
	  "<?php SqlFormatter::$max_cachekey_size = 15;";
	  "<?php SqlFormatter::$cli = false;"
	];;


add_data 
	"assign_to_terna"
	"variable ##( assign )## ##( () ? : new nmspc () )## ;"
	helper_for_assignment
	[
	  ("<?php $helperSet = ($helperSet) ?: new "^
	  "\\Symfony\\Component\\Console\\Helper\\HelperSet();")
	];;


add_data 
	"assign_byref"
	"variable assign ##( & _l_loose= _r*_ )## ;"
	helper_for_assignment
	[
	  "<?php $result =& $data;"
	];;



add_data 
	"assign1"
	"variable assign ##( () ?  _l_ id _u_ sqs _rd_  :  _l_ id _u_ variable _rd_  )## ;"
	helper_for_assignment
	[
	  "<?php $safe_mode		= (@ini_get('safe_mode') == '1' || strtolower(@ini_get('safe_mode')) === 'on') ? true : false;\n";
	  "<?php $form = (!preg_match('/^[a-z0-9_-]+$/i', $form)) ? '' : $form;\n"

	];;


add_data 
	"assign2"
	"variable assign  ##( sqs . id . dqs . variable -> id () . dqs . variable -> id () . dqs )## ;"
	helper_for_assignment
	[
	  ("<?php $sql = 'UPDATE ' . CONFIG_TABLE . \"\n"^
      "	SET config_value = '\" . $db->sql_escape(CRON_ID) . \"'\n"^
      "	WHERE config_name = 'cron_lock' AND config_value = '\" . $db->sql_escape($config['cron_lock']) . \"'\";\n")


	];;  

  
add_data 
	"assign_to_cell"
	"variable assign ##( variable [ sqs ] )## ;"
	helper_for_assignment
	[
	  "<?php $formHelper = $view['form'];"
	];;


let helper_for_ivy l1 cr=
  Ivy(List.nth l1 0,List.nth l1 1,List.nth l1 2,cr);;

add_data 
   "ivy1"
   "if () ##( {} )## ##( _l_else if () {} _r*__l_else {} _r?_ )##"
   helper_for_ivy
   ["<?php if($a){$b}elseif($z1-$g1){$d1*$f1}elseif($z2-$g2){$d2*$f2}elseif($z3-$g3){$d3*$f3}else{$u+$v}"];; 
   
     

let helper_for_ivy2 l1 cr=Ivy(List.nth l1 0,List.nth l1 1,[],cr);; 

add_data 
  "ivy2"
  "if () ##( exit ; )##"
  helper_for_ivy2
  [
    "<?php if (!defined('IN_PHPBB')) exit;\n"
  ];;    

add_data 
  "ivy3"
  "if () ##( {} )## else ##(if () {})## "
  helper_for_ivy
  [
     (
	   "<?php if (isset($_SERVER['CONTENT_TYPE']))\n"^
	   "{\n"^
	   "	if ($_SERVER['CONTENT_TYPE'] === 'application/x-java-archive')\n"^
	   "	{\n"^
	   "		exit;\n"^
	   "	}\n"^
	   "}\n"^
	   "else if (isset($_SERVER['HTTP_USER_AGENT']) && strpos($_SERVER['HTTP_USER_AGENT'], 'Java') !== false)\n"^
	   "{\n"^
	   "	exit;\n"^
	   "}\n"
	  )

  ];;    


add_data 
   "ivy4"
   "if () : ##( _l_no_ivies _r*_ )## endif ;"
   helper_for_ivy2
   [
     "<?php if ($required && null === $placeholder && $placeholder_in_choices === false && $multiple === false && (!isset($attr['size']) || $attr['size'] <= 1)):\n"^
     "        $required = false;\n"^
     "    endif; ?>\n"
    ];; 

add_data 
  "ivy5"
  "if () : ##( _l_no_ivies _r*_ if () : _l_no_ivies _r*_ endif _l_no_ivies _r*_ )## endif ;"
  helper_for_ivy2
  [
   "<?php if (null !== $placeholder): ?><option value=\"\"<?php if ($required && empty($value) "^
   "&& '0' !== $value): ?> selected=\"selected\"<?php endif?>><?php echo '' != "^
   "$placeholder ? $view->escape(false !== $translation_domain ? "^
   "$view['translator']->trans($placeholder, array(), $translation_domain) : "^
   "$placeholder) : '' ?></option><?php endif; ?>\n"
  ];; 
  


add_data 
  "ivy6"
  "if () : ##( _l_no_ivies _r*_ if () : _l_no_ivies _r*_ else : _l_no_ivies _r*_ endif ; _l_no_ivies _r*_ )## endif ;"
  helper_for_ivy2
  [
    "<?php if ($hasMinorProblems): ?>\n"^
	"                            <h2>Recommendations</h2>\n"^
	"                            <p>\n"^
	"                                <?php if ($hasMajorProblems): ?>Additionally, to<?php else: ?>To<?php endif; ?> enhance your Symfony experience,\n"^
	"                                it’s recommended that you fix the following:\n"^
	"                            </p>\n"^
	"                            <ol>\n"^
	"                                <?php foreach ($minorProblems as $problem): ?>\n"^
	"                                    <li><?php echo $problem->getTestMessage() ?>\n"^
	"                                        <p class=\"help\"><em><?php echo $problem->getHelpHtml() ?></em></p>\n"^
	"                                    </li>\n"^
	"                                <?php endforeach; ?>\n"^
	"                            </ol>\n"^
	"                        <?php endif; ?>"
  ];; 
  

  
let helper_for_include_like l1 cr=
      let a=List.hd(List.hd l1) in
      let fa=Positioned_php_token.fst a in
      let na=Php_script_includer.from_lexeme fa in
      Script_inclusion(na,List.nth l1 1,cr);; 

add_data 
   "include_like"
   "include_like _l_stringy _r*_ ;"
   helper_for_include_like
   [
     "<?php require_once $amy.(PEGGY.'betty').\"Friday\";"
   ];; 




let helper_for_yuze_decl l1 cr=Yuze_decl(List.hd l1,cr);;

add_data 
	"yuze_decl"
	"use _l_no_semicolon _r*_ ;"
	helper_for_yuze_decl
	[
	  "<?php use function /Hum/Harry/Figaro as Hairy;$cant=$be+$with;"
	];;





let helper_for_static_meth l1 cr=
   let a1=List.hd(List.hd l1) 
   and a2=List.hd(List.nth l1 1) in
   Static_method_call(a1,a2,List.nth l1 2,cr);;

add_data 
	"static_meth"
	"id :: id_or_var () ;"
	helper_for_static_meth
	[
	  "<?php Debug::enable($again+3);"
	];;


let helper_for_snake_call l1 cr=
  let temp1=(List.hd l1)@(List.nth l1 1) 
  and last_leader=List.hd(List.nth l1 2) in
  if (List.nth l1 3)=[]
  then Class_property_call(temp1,last_leader,cr)
  else Nonstatic_method_call(temp1,last_leader,List.nth l1 3,cr);;

add_data 
	"snake_call"
	"id_or_var _l_ -> id_or_var _l_() _r?_  _r~_ -> id_or_var _l_() _r?_ ;"
	helper_for_snake_call
	[
	  "<?php $amy->finds($x,$y)->$beaver->eats($it)->again($another,$leg);";
	  "<?php $amy->finds($x,$y)->$beaver->eats($it)->again;"
	];;

let helper_for_trycatch l1 cr=TryCatch(List.nth l1 0,List.nth l1 1,List.nth l1 2,cr);;

add_data 
	"trycatch"
	"try {} catch () {}"
	helper_for_trycatch
	[
	  "<?php try{$a+$b} catch($c-$d){$e+$f} "
	];;





let helper_for_fun_def l1 cr=
   let a=List.hd(List.hd l1) in
   Fun_def(a,List.nth l1 1,List.nth l1 2,cr);;

add_data 
	"fun_def"
	"function id () {}"
	helper_for_fun_def
	[
	  "<?php function amy($a,$b) {$x+$y}"
	];;





let helper_for_returning l1 cr=Returning(List.hd(l1),cr);;

add_data 
	"returning"
	"return _l_ no_semicolon _r*_ ;"
	helper_for_returning
	[
	  "<?php return try{$a+$b} catch($c-$d){$e+$f};"
	];;






let helper_for_interface_decl l1 cr=
     let temp1=List.hd l1 in
     Interface_decl(List.hd temp1,List.tl temp1,List.nth l1 1,cr);;

add_data 
	"interface_decl"
	"interface _l_ no_left_brace _r*_ {}"
	helper_for_interface_decl
	[
	  "<?php interface Amy implements Peggy extends Betty {$cant=$be+$with;}"
	];;




let helper_for_fun_call l1 cr=Fun_call(List.hd(l1)<>[],List.hd(List.nth l1 1),List.nth l1 2,cr);;

add_data 
	"fun_call"
	"_l_ @ _r?_ ##(id)## () ;"
	helper_for_fun_call
	[
	  "<?php @peggy($x,$y,$z);";
	  "<?php peggy($x,$y,$z);"
	];;




let helper_for_nmspc_definition l1 cr=
   let a=List.hd(List.hd l1) in
   Namespace_def(a,cr);;

add_data 
	"nmspc_definition"
	"namespace nmspc ;"
	helper_for_nmspc_definition
	[
	  "<?php namespace Marcia\\Balia\\Conmigo;";
	  "<?php namespace \\Me\\Tiene\\Loco;"
	];;






let helper_for_byref_append l1 cr=
   let a=List.hd(List.hd l1) in
   Appending(a,List.nth l1 1,cr);;

add_data 
	"append_byref"
	"variable [ ] = ##( & _l_no_semicolon _r*_ )## ;"
	helper_for_byref_append
	[
	  "<?php $ding[ ] = &$var['nobj'][0];"
	];;



let helper_for_cell_assign l1 cr=
   let a=List.hd(List.hd l1) in
   Cell_assignment(a,List.hd(List.nth l1 1),List.nth l1 2,cr);;

add_data 
	"cell_assign_byref"
	"variable [  ##( int_or_string_or_var )##  ]  =  ##( & _l_loose= _r*_ )## ;"
	helper_for_cell_assign
	[
	  "<?php $var[ 'marcia' ] = &$var['nobj'][0];"
	];;








add_data 
	"cell_assign"
	"variable [ ##( int_or_string_or_var )## ] = _l_loose= _r*_ ;"
	helper_for_cell_assign
	[
	  "<?php $var[ 'marcia' ] = &$var['nobj'][0];"
	];;





let helper_for_trait_decl l1 cr=
  let temp1=List.hd l1 in
  Trait_decl(List.hd temp1,List.nth l1 1,cr);;

add_data 
	"trait_decl"
	"trait id {}"
	helper_for_trait_decl
	[
	  "<?php trait Amy {$cant=$be+$with;}"
	];;





let helper_for_nmspc_lonely l1 cr=
   let a=List.hd(List.hd l1) in
   Namespace_def(a,cr);;

add_data 
	"nmspc_lonely"
	"namespace id ;"
	helper_for_nmspc_lonely
	[
	  "<?php namespace Enrique;"
	];;



let helper_for_while_loop l1 cr=WhileLoop(List.nth l1 0,List.nth l1 1,cr);;

add_data 
	"while_loop"
	"while () {}"
	helper_for_while_loop
	[
	  "<?php while ($a+$b) {$c-$d}"
	];;





let helper_for_foreach_loop l1 cr=ForeachLoop(List.nth l1 0,List.nth l1 1,cr);;

add_data 
	"foreach_loop"
	"foreach () {}"
	helper_for_foreach_loop
	[
	  "<?php foreach ($a+$b) {$c-$d}"
	];;





let helper_for_foreach2 l1 cr=ForeachLoop(List.nth l1 0,List.nth l1 1,cr);;

add_data 
	"foreach2"
	"foreach () : ##( _l_no_breach _r*_ )## endforeach ;"
	helper_for_foreach2
	[
	  "<?php foreach ($a+$b): $c-$d endforeach ;"
	];;



let helper_for_echo l1 cr=Echo(List.hd l1,cr);;


add_data 
	"echo1"
	"echo _l_ no_semicolon _r*_ ;"
	helper_for_echo
	[
	  "<?php echo $amy.(PEGGY.'betty').\"abc\";";
	  "<?php echo '/'.implode('|', $regex).'/A';";
	  "<?php echo $this->get('actions')->render($this->get('actions')->controller('TestBundle:Fragment:forwardlocale')); ";
	  "<?php echo $view['form']->block($form, 'form_widget_simple', array('type' => isset($type) ? $type : 'range')); ";
	  "<?php echo SqlFormatter::format($sql);";
	  ("<?php echo \"<p>Formatted \".$num.\" queries using a max_cachekey_size "^
	  "of \".SqlFormatter::$max_cachekey_size.\"</p>\";");
	  ("<?php echo \"<p>Average query length of \".number_format($chars/$num,5).\" "^
	  "characters</p>\";");
	  ("<?php echo \"<p>Took \".number_format($end-$start,5).\" seconds "^
	   "total, \".number_format(($end-$start)/$num,5).\" seconds per "^
	   "query, \".number_format(1000*($end-$start)/$chars,5).\" seconds "^
	   "per 1000 characters</p>\";");

	
	];;



let helper_for_fun_returning l1 cr=Returning(List.hd(l1),cr);;

add_data 
	"fun_returning"
	"return ##( function () {} )## ;"
	helper_for_fun_returning
	[
	  "<?php return function($a,$b) {catch($c-$d){$e+$f}}; "
	];;

let helper_for_statmeth_call l1 cr=
  Static_method_call(List.hd(List.nth l1 0),List.hd(List.nth l1 1),List.nth l1 2,cr);;

add_data 
	"statmeth_call"
	"_l_ id_u_nmspc _rd_ :: ##(id)## () ;"
	helper_for_statmeth_call
	[
	  "<?php Doctrine\\ORM\\Tools\\Console\\ConsoleRunner::run($helperSet, $commands);"
	];;

let helper_for_meth_call_on_snake l1 cr=
  Nonstatic_method_call(List.nth l1 0,List.hd(List.nth l1 1),List.nth l1 2,cr);;


add_data 
	"meth_call_on_snake"
	"##( id :: id () _l_-> id () _r~_ )## -> id () ;"
	helper_for_meth_call_on_snake
	[
	  "<?php Mockery::getConfiguration()->allowMockingNonExistentMethods(true);";
	  "<?php Swift_DependencyContainer::getInstance()\n"^
	  "    ->register('message.message')\n"^
	  "    ->asNewInstanceOf('Swift_Message')\n"^
	  "\n"^
	  "    ->register('message.mimepart')\n"^
	  "    ->asNewInstanceOf('Swift_MimePart')\n"^
	  ";";
	  "<?php Swift_DependencyContainer::getInstance()\n"^
	  "    ->register('cache')\n"^
	  "    ->asAliasOf('cache.array')\n"^
	  "\n"^
	  "    ->register('tempdir')\n"^
	  "    ->asValue('/tmp')\n"^
	  "\n"^
	  "    ->register('cache.null')\n"^
	  "    ->asSharedInstanceOf('Swift_KeyCache_NullKeyCache')\n"^
	  "\n"^
	  "    ->register('cache.array')\n"^
	  "    ->asSharedInstanceOf('Swift_KeyCache_ArrayKeyCache')\n"^
	  "    ->withDependencies(array('cache.inputstream'))\n"^
	  "\n"^
	  "    ->register('cache.disk')\n"^
	  "    ->asSharedInstanceOf('Swift_KeyCache_DiskKeyCache')\n"^
	  "    ->withDependencies(array('cache.inputstream', 'tempdir'))\n"^
	  "\n"^
	  "    ->register('cache.inputstream')\n"^
      "    ->asNewInstanceOf('Swift_KeyCache_SimpleKeyCacheInputStream')\n"^
      ";\n"
	  
	];;






let helper_for_nmspc_block l1 cr=
   let optionized=(fun x->if x=[] then None else Some(List.hd x) )(List.hd l1) in
   NamespaceBlock(optionized,List.nth l1 1,cr);;

add_data 
	"nmspc_block"
	"namespace _l_ _l_ id_u_nmspc _rd_ _r?_ {}"
	helper_for_nmspc_block
	[
	  "<?php namespace {return function($a,$b) {catch($c-$d){$e+$f}};} ";
	  "<?php namespace Amy {return function($a,$b) {catch($c-$d){$e+$f}};} ";
	  "<?php namespace Amy\\Betty {return function($a,$b) {catch($c-$d){$e+$f}};} "
	];;



let helper_for_exit l1 cr=Exit(cr);;

add_data 
	"exit"
	"exit ;"
	helper_for_exit
	[
	  "<?php exit;\n"
	];;


let helper_for_switch l1 cr=Switch(List.hd(List.hd l1),List.nth l1 1,cr);;

add_data 
	"switch"
	"switch () {}"
	helper_for_switch
	[
	  "<?php switch($a) {$b+$c}"
	];;



let helper_for_decl l1 cr=Declare(List.hd l1,cr);;

add_data 
	"decl"
	"declare () ;"
	helper_for_decl
	[
	  "<?php declare (strict_types = 1);"
	];;


let parser=(current_main_parser:t Old_php_parser.t);; 


 
 


