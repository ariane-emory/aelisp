;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fancy output macros:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro princn  exprs           $('progn (cons 'princ exprs) $('nl)))
(defmacro printn  exprs           $('progn (cons 'print exprs) $('nl)))
(defmacro putn    exprs           $('progn (cons 'put  exprs)  $('nl)))
(defmacro writen  exprs           $('progn (cons 'write exprs) $('nl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro princni (delim . exprs) $('apply 'princn $('intercalate delim $('quote exprs))))
(defmacro printi  (delim . exprs) $('apply 'print  $('intercalate delim $('quote exprs))))
(defmacro putni   (delim . exprs) $('apply 'putn   $('intercalate delim $('quote exprs))))
(defmacro writeni (delim . exprs) $('apply 'writen $('intercalate delim $('quote exprs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro princns exprs           $('princni " " . exprs))
(defmacro prints  exprs           $('princti " " . exprs))
(defmacro putns   exprs           $('putni   " " . exprs))
(defmacro writens exprs           $('writeni " " . exprs))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'print-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
