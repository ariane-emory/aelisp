;; (setq stop   (lambda ()  (terpri) (exit 0)))
;; (setq sleep  (lambda (x) (msleep (* 1000 x))))
(setq nl (lambda ()  (princ "
")))

;; (setq defmacro (macro (name params . body) (list (quote setq) name (list (quote macro) params . body))))  
;; (defmacro defun (name params . body) (list (quote setq) name (list (quote lambda) params . body)))
;; ;; (defmacro madd2 (xxx yyy) (list (quote +) xxx yyy))

;; (defun print-and-die (x) (print "Dying...") (exit 0))

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

(print (env))
(print (env :syms))
(print (env :vals))
