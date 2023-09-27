(a . b) (a b)
((a b c . d) "bang" 1 1.5 3/4 'x' ?\y () nil 'a' '\t' ?\\t :asd &more t (= 1 3)) (a . b) (x y . z)
(a ∞ b) (a b c) ;; This is a comment.
(a b c d (e f))
;; This is another comment.
"This string spans multiple lines...
 look, here is the second line!"

;;; Lightweight advice/hook
(defvar advice--how-alist
  (advice--make-how-alist
   (:around (apply car cdr r))
   (:before (apply car r) (apply cdr r))
   (:after (prog1 (apply cdr r) (apply car r)))
   (:override (apply car r))
   (:after-until (or (apply cdr r) (apply car r)))
   (:after-while (and (apply cdr r) (apply car r)))
   (:before-until (or (apply car r) (apply cdr r)))
   (:before-while (and (apply car r) (apply cdr r)))
   (:filter-args (apply cdr (funcall car r)))
   (:filter-return (funcall car (apply cdr r))))
  "List of descriptions of how to add a function.
Each element has the form (HOW OCL DOC) where HOW is a keyword,
OCL is a \"prototype\" function of type `advice', and
DOC is a string where \"FUNCTION\" and \"OLDFUN\" are expected.")

(defun advice--cd*r (f)
  (while (advice--p f)
    (setq f (advice--cdr f)))
  f)

(defun advice--cd*r (f)
  (while (advice--p f)
    (setq f (advice--cdr f)))
  f)

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
