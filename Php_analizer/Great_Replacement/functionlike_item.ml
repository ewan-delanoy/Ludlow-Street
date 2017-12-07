(*

#use"Php_analizer/Great_Replacement/functionlike_item.ml";;



*)

type t={
    kind : Functionlike_kind.t;
    namespace : string;
    containing_class : string;
    method_qualifier : string;
    function_name : string;
    before_content : string;
    content : string;
    after_content : string;
};;

let kind x=x.kind;;
let namespace x=x.namespace;;
let containing_class x=x.containing_class;;
let method_qualifier x=x.method_qualifier;;
let function_name x=x.function_name;;
let before_content x=x.before_content;;
let content x=x.content;;
let after_content x=x.after_content;;

let full_content x=
    (x.before_content)^
    (x.content)^
    (x.after_content);;  

let length x=
   String.length(x.before_content)+
   String.length(x.content)+
   String.length(x.after_content);; 

let namespace_line nspc_line=
    let (nspc_name,_)=
        Option.unpack(Nspc_detect.extract_namespace_name nspc_line) in
    {
    kind = Functionlike_kind.namespace_line;
    namespace =nspc_name;
    containing_class ="";
    method_qualifier ="";
    function_name="";
    before_content="\n";
    content =nspc_line;
    after_content="\n";
};;

let non_function nspc_name class_name text={
    kind = Functionlike_kind.non_function;
    namespace =nspc_name;
    containing_class =nspc_name;
    method_qualifier =class_name;
    function_name="";
    before_content="";
    content =text;
    after_content="";
};;

let after_namespace_comments text={
    kind = Functionlike_kind.after_namespace_comments;
    namespace ="";
    containing_class ="";
    method_qualifier ="";
    function_name="";
    before_content="";
    content =text;
    after_content="";
};;


let make a b c d e f g h={
    kind =a;
    namespace =b;
    containing_class =c;
    method_qualifier =d;
    function_name=e;
    before_content=f;
    content =g;
    after_content=h;
};;