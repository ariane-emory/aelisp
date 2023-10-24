;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! lst $(2 4 1 5 3 7 9 6 8))

(princ (select-and-move-to-front! (lambda (o) (eql? o 9)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 8)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 7)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 6)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 5)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 4)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 3)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 2)) lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! (lambda (o) (eql? o 1)) lst)) (spc) (write lst) (nl)

(select-and-move-to-front! (lambda (o) (eql? o 2)) (list 2))

(princ (nthcdr 0 lst)) (nl)
(princ (nthcdr 1 lst)) (nl)
(princ (nthcdr 2 lst)) (nl)
(setq! c (curry1 nthcdr 3))
(princ (c lst)) (nl)

(princ (pop! lst)) (nl)
(princ lst) (nl)
(princ (push! 99 lst)) (nl)
(princ lst) (nl)

(ignore
 ;; a deliberate invalid call:
 (nl) (princ "AN ERROR WILL FOLLOW: ") (nl)
 ;; (log-eval t)
 (s x 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-list (pred? lst)
  "Splits the list LST into two sublists:
   1. The longest initial sublist of elements satisfying PRED?
   2. The rest of the elements."
  (let ((prefix '())
        (suffix lst))
    (while (and suffix (funcall pred? (car suffix)))
      (push (pop suffix) prefix))
    (list (nreverse prefix) suffix)))

;; ;; Testing with your example:
;; (let ((lst '("asdw" "erer" "rerw" 1 nil (lambda (x) x) "zoop" z (1 2 . 3) 8)))
;;  (write (split-list string? lst)))
;; (nl)
