(*

#use"GParser/hparser.ml";;

*)

type t=
     Constant of string
    |Enclosure of string*string
    |Footless_constant of string
    |Sample_char of string
    |Sample_neg of string
    |Sample_star of string
    |Sample_negstar of string
    |Sample_plus of string
    |Race of string*string
    |House_with_doors of string*string*((string*string) list)
    
    |Chain of t list
    |Disjunction of t list
    |Star of t
    |One_or_more of t
    |Optional of t
    |Recoiling_ending of t*t
;;
