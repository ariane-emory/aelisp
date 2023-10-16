(setq! defmacro
 (macro (name params . body)
  (list 'setq! name (list 'macro params . body))))

(defmacro defun (name params . body)
 (list (quote setq!) name (list (quote lambda) params . body)))

(defun 2* (x) (* 2 x)) (nl)

(princ "this should print 6: " ) (write (2* 3)) (nl) ;; successfuly prints 6.

