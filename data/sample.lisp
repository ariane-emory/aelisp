(setq nl (lambda ()  (princ "
")))
(setq stop   (lambda ()  (terpri) (exit 0)))
(setq sleep  (lambda (x) (msleep (* 1000 x))))
(setq defmacro (macro (name params . body) (list (quote setq) name (list (quote macro) params . body))))  
(defmacro defun (name params . body) (list (quote setq) name (list (quote lambda) params . body)))

(print (env :syms))

(nl)

(nl)
(princ "BEFORE defmacro") (nl)
(write defmacro) (nl)
(princ "Params: ") (write (params defmacro)) (nl)
(princ "Body:   ") (write (body   defmacro))   (nl)
(princ "AFTER") (nl)
(nl)

(stop)

(nl) (princ "BEFORE") (nl)
(write (defmacro add (x y) (list (quote +) x y)))
(princ "Params: ") (write (body (defmacro add (x y) (list (params +) x y)))) (nl)
(princ "Body:   ") (write (body (defmacro add (x y) (list (quote +) x y)))) (nl)
(nl) (princ "AFTER") (nl) (nl)


(nl) (princ "BEFORE") (nl)
(write (defmacro add (x y) (list (quote +) x y)))
(nl) (princ "AFTER") (nl) (nl)

(nl) (princ "BEFORE") (nl)
(write (body (defmacro add (x y) (list (quote +) x y))))
(nl) (princ "AFTER") (nl) (nl)

;; (write (quote (name params . body))) (nl)

;; (write (defmacro defun (name params . body) (list (quote setq) name (list (quote lambda) params . body))))

;; (defun print-and-die (x) (print "Dying...") (exit 0))

(nl)

;; (print-and-die)

;; (setq m-add (macro (xxx yyy) (list (quote +) xxx yyy)))
;; (setq qq (m-add 4 9))
;; (print qq)
;; (print (eval qq))

;; (nl)

;; ((lambda (x y) (+ x y)) 4 9)

;; (qq)

;; (stop)

;; (setq test
;;   (lambda (x y . z)
;;     (nl)
;;     (princ "Rest: ")
;;     (princ z)
;;     (nl)
;;     (+ x y)))

;; (test 1 2 3 4)

;; (setq zzz 777)
;; (setq test
;;   (lambda ()
;;     (setq zzz 888)))

;; (test)

;; (print zzz)

;; (print (env :vals))
