(*

#use"Php_analizer/php_token_category.ml";;


   
*)


let compute =function
      Php_token.Constant(ctok)   ->Php_constant_token.token_category ctok
     |Php_token.Variable(_)      ->Token_category.Variable
     |Php_token.Ident(_)         ->Token_category.Identifier
     |Php_token.Comment (_)      ->Token_category.Comment
     |Php_token.Single_quoted(_) ->Token_category.Single_quoted_string
     |Php_token.Double_quoted(_) ->Token_category.Double_quoted_string
     |Php_token.Heredoc(_)       ->Token_category.Heredoc_string
     |Php_token.Nowdoc(_)        ->Token_category.Nowdoc_string
     |Php_token.Namespacer(_,_,_)->Token_category.Namespacer
     |Php_token.External_echo(_) ->Token_category.External_item
     |Php_token.Int(_)           ->Token_category.Integer
     |Php_token.Float(_)         ->Token_category.Floating_number
     |Php_token.Char(_)          ->Token_category.Character
     |Php_token.End_of_text      ->Token_category.End_of_text;;
     
     