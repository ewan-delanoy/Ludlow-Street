
val chain : Old_php_recognizer.t list -> Old_php_recognizer.t
val ordered_disjunction : Old_php_recognizer.t list -> Old_php_recognizer.t
val star : Old_php_recognizer.t -> Old_php_recognizer.t
val unfinished_star : Old_php_recognizer.t -> Old_php_recognizer.t
val plus : Old_php_recognizer.t -> Old_php_recognizer.t
val optional : Old_php_recognizer.t -> Old_php_recognizer.t
val generalize : Generalizer.t -> Old_php_recognizer.t -> Old_php_recognizer.t
