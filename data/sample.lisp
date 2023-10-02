;; These first 5 expressions are parsed correctly by the grammar:
(+ 1 2) ;; a 'math_list'. 
(+ 3 (* 4 5)) ;; another 'math_list'. 
(+ x 6) ;; a normal 'list', due to the presence of the 'x' symbol.
(+ 7 x) ;; a normal 'list', due to the presence of the 'x' symbol. 
(+ x y) ;; also a normal 'list', due to presence of the the symbols. 
(+ 8 . (9)) ;; a normal 'list' identical to (8 9).

;; This one used to parse as a normal 'list' before the addition of the 'math_list' rule, but now fails to parse.
;; I'd compromise for it successfully parsing as a normal 'list', as it used to, it doesn't have to be a 'math_list'.
(+ 10 . (11)) ;; a normal list, due to the grammar not supporting the dotted notation in math_lists, identical to (+ 10 11)
