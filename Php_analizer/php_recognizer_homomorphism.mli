
val chain : Php_recognizer.t list -> Php_recognizer.t
val ordered_disjunction : Php_recognizer.t list -> Php_recognizer.t
val star : Php_recognizer.t -> Php_recognizer.t
val unfinished_star : Php_recognizer.t -> Php_recognizer.t
val plus : Php_recognizer.t -> Php_recognizer.t
val optional : Php_recognizer.t -> Php_recognizer.t
val generalize : Generalizer.t -> Php_recognizer.t -> Php_recognizer.t
