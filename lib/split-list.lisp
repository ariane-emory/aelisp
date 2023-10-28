;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; split lists:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-list-alternate! (pred? lst)
 "Destructively split the LST into two sublists:"
 "1. The longest initial sublist of elements satisfying PRED?"
 "2. The rest of the elements."
 (unless (fun? pred?) (error "PRED? must be a function"))
 (unless (list? lst) (error "LST must be a list"))
 (let ((front nil)
       (back lst))
  (while (and back (pred? (car back)))
   (push! (pop! back) front))
  $((reverse front) back)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-list (pred? lst)
 "Destructivly split LST into two sublists:"
 "1. The longest initial sublist of elements satisfying PRED?"
 "2. The rest of the elements."
 (unless (fun? pred?) (error "PRED? must be a function"))
 (unless (list? lst) (error "LST must be a list"))
 (let ((prev nil)
       (current lst))
  (while (and current (pred? (car current)))
   (setq! prev current)
   (setq! current (cdr current)))
  (if prev
   (progn
    (rplacd! prev nil)
    $(lst current))
   $(nil lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-list (pred? lst)
 "Split LST into two sublists:"
 "1. The longest initial sublist of elements satisfying PRED?"
 "2. The rest of the elements."
 (unless (fun? pred?) (error "PRED? must be a function"))
 (unless (list? lst) (error "LST must be a list"))
 (let ((front nil)
       (current lst))
  (while (and current (pred? (car current)))
   (setq front (cons (car current) front))
   (setq current (cdr current)))
  $((reverse front) current)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'split-list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
