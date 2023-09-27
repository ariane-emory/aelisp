(setq nl (lambda ()  (princ "
")))
(setq stop   (lambda ()  (terpri) (exit 0)))
(setq sleep  (lambda (x) (msleep (* 1000 x))))
(setq defmacro (macro (name params . body) (list (quote setq) name (list (quote macro) params . body))))  
(defmacro defun (name params . body) (list (quote setq) name (list (quote lambda) params . body)))

(setq defun (macro (name params . body) (list (quote setq) name (list (quote lambda) params . body))))
(setq add   (macro (xxx yyy)            (list (quote +) xxx yyy)))

(print (env :syms))

(nl)

(nl)
(princ "BEFORE defmacro") (nl)
(write defmacro) (nl)
(princ "Params: ") (write (params defmacro)) (nl)
(princ "Body:   ") (write (body   defmacro))   (nl)
(princ "AFTER") (nl)


(nl) (princ "BEFORE add") (nl)
(write add) (nl)
(princ "Params: ") (write (params add)) (nl)
(princ "Body:   ") (write (body add)) (nl)
(princ "AFTER") ; (nl)

(nl) (princ "BEFORE defun") (nl)
(write defun) (nl)
(princ "Params: ") (write (params defun)) (nl)
(princ "Body:   ") (write (body defun)) (nl)
(princ "AFTER") ; (nl)

(stop)
