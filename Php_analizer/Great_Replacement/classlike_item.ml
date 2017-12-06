(*

#use"Php_analizer/Great_Replacement/classlike_item.ml";;



*)

type t={
    kind : Classlike_kind.t;
    namespace : string;
    class_name : string;
    class_qualifiers : string;
    before_content : string;
    content : string;
    after_content : string;
};;

let kind x=x.kind;;
let namespace x=x.namespace;;
let class_name x=x.class_name;;
let class_qualifiers x=x.class_qualifiers;;
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
    kind = Classlike_kind.namespace_line;
    namespace =nspc_name;
    class_name ="";
    class_qualifiers ="";
    before_content="\n";
    content =nspc_line;
    after_content="\n";
};;

let non_class nspc_name text={
    kind = Classlike_kind.plain_text;
    namespace =nspc_name;
    class_name ="";
    class_qualifiers ="";
    before_content="";
    content =text;
    after_content="";
};;

let after_namespace_comments text={
    kind = Classlike_kind.after_namespace_comments;
    namespace ="";
    class_name ="";
    class_qualifiers ="";
    before_content="";
    content =text;
    after_content="";
};;


let make a b c d e f g={
    kind =a;
    namespace =b;
    class_name =c;
    class_qualifiers =d;
    before_content=e;
    content =f;
    after_content=g;
};;