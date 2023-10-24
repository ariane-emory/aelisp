(ignore
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
 (princ lst) (nl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ignore
 ;; a deliberate invalid call:
 (nl) (princ "AN ERROR WILL FOLLOW: ") (nl)
 ;; (log-eval t)
 (s x 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This one triggers an indentation bug, investigate:
;; (defun split-list (pred? lst)
;;  (let ((front nil)
;;        (current lst))
;;   (while (and current (funcall pred? (car current)))
;;    (setq front (cons (car current) front))
;;    (setq current (cdr current)))
;;   $((fake-reverse front) current)))

(ignore
 (s lst  '("asdw" "erer" "rerw" 1 nil (lambda (x) x) "zoop" z (1 2 . 3) 8))
 (s split (split-list string? lst))
 (princ "front: ") (write (apply concat (intercalate " " (car split))))  (nl)
 (princ "back:  ") (write (cadr split)) (nl))

(defmacro defun (name params . docs-and-body)
 (let* ((split (split-list string? docs-and-body))
        (docs  (apply concat (intercalate " " (car split))))
        (body  (cadr split)))
  $('progn 
    $('setq! name $('lambda params . body))
    $('put! docs ':doc name))))

(defun somefun (x y)
 "Multiply two"
 "numbers."
 (* x y))

(write (get :doc somefun)) (nl)
;;(log-eval t)

(write (doc somefun)) (nl)
(write (doc write)) (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun binlist-to-dec-1 (lst)
 (letrec
  ((chase
    (lambda (acc lst)
     (if (nil? lst)
      acc
      (chase (+ (<< acc 1) (car lst)) (cdr lst))))))
  (chase 0 lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun binlist-to-dec-2 (lst) (reduce   (lambda (x y) (+ (<< x 1) y)) lst 0))
(setq! binlist-to-dec-3       (reduced  (lambda (x y) (+ (<< x 1) y))))
(setq! bins-to-dec            (reduced* (lambda (x y) (+ (<< x 1) y))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(write (binlist-to-dec-1 '(1 0 1 0 1 0 1))) (nl) ;; ⇒ 85
(write (binlist-to-dec-2 '(1 0 1 0 1 0 1))) (nl) ;; ⇒ 85
(write (binlist-to-dec-3 '(1 0 1 0 1 0 1))) (nl) ;; ⇒ 85
(write (bins-to-dec        1 0 1 0 1 0 1))  (nl) ;; ⇒ 85
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun right-reduce-inner (fun lst)
 "Right-reduce (foldr) LST by applying FUN to successive pairs."
 (cond
  ((nil? lst)        nil)
  ((nil? (cdr lst))  (car lst))
  (t                 (fun (car lst) (right-reduce-inner fun (cdr lst))))))
(defun right-reduce (fun lst . init-val)
 "Right-reduce (foldr) LST by applying FUN to successive pairs."
 (cond
  ((nil? init-val)   (right-reduce-inner fun lst))
  ((cdr init-val)    (error "init-val must be a single object"))
  (t                 (right-reduce-inner fun (append lst (list (car init-val)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun left-reduce-inner (fun lst)
 "Left-reduce (foldl) LST by applying FUN to successive pairs."
 (cond
  ((nil? lst)        nil)
  ((nil? (cdr lst))  (car lst))
  (t                 (left-reduce-inner fun (cons (fun (car lst) (cadr lst)) (cddr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun left-reduce (fun lst . init-val)
 "Left-reduce (foldl) LST by applying FUN to successive pairs."
 (cond
  ((nil? init-val)   (left-reduce-inner fun lst))
  ((cdr init-val)    (error "init-val must be a single object"))
  (t                 (left-reduce-inner fun (cons (car init-val) lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
;;(log-eval t)
(princ "rri  #1: ") (write (right-reduce-inner  + '()))           (nl)
(princ "rri  #2: ") (write (right-reduce-inner  + '(1)))          (nl)
(princ "rri  #3: ") (write (right-reduce-inner  + '(1 2)))        (nl)
(princ "rri  #4: ") (write (right-reduce-inner  + '(1 2 3)))      (nl)
(princ "rri  #5: ") (write (right-reduce-inner  + '(1 2 3 4)))    (nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
;; (log-eval t)
(princ "lri  #1: ") (write (left-reduce-inner   + '()))           (nl)
(princ "lri  #2: ") (write (left-reduce-inner   + '(1)))          (nl)
(princ "lri  #3: ") (write (left-reduce-inner   + '(1 2)))        (nl)
(princ "lri  #4: ") (write (left-reduce-inner   + '(1 2 3)))      (nl)
(princ "lri  #5: ") (write (left-reduce-inner   + '(1 2 3 4)))    (nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
;; (log-eval t)
(princ "lr   #1: ") (write (left-reduce         + '()))           (nl)
(princ "lr   #2: ") (write (left-reduce         + '(1)))          (nl)
(princ "lr   #3: ") (write (left-reduce         + '(1 2)))        (nl)
(princ "lr   #4: ") (write (left-reduce         + '(1 2 3)))      (nl)
(princ "lr   #5: ") (write (left-reduce         + '(1 2 3 4)))    (nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
;; (log-eval t)
(princ "lr 0 #1: ") (write (left-reduce         + '()         0)) (nl)
(princ "lr 0 #2: ") (write (left-reduce         + '(1)        0)) (nl)
(princ "lr 0 #3: ") (write (left-reduce         + '(1 2)      0)) (nl)
(princ "lr 0 #4: ") (write (left-reduce         + '(1 2 3)    0)) (nl)
(princ "lr 0 #5: ") (write (left-reduce         + '(1 2 3 4)  0)) (nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
;; (log-evalj t)
(princ "rr   #1: ") (write (right-reduce        + '()))           (nl)
(princ "rr   #2: ") (write (right-reduce        + '(1)))          (nl)
(princ "rr   #3: ") (write (right-reduce        + '(1 2)))        (nl)
(princ "rr   #4: ") (write (right-reduce        + '(1 2 3)))      (nl)
(princ "rr   #5: ") (write (right-reduce        + '(1 2 3 4)))    (nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
;; (log-eval t)
(princ "rr 0 #1: ") (write (right-reduce        + '()         0)) (nl)
(princ "rr 0 #2: ") (write (right-reduce        + '(1)        0)) (nl)
(princ "rr 0 #3: ") (write (right-reduce        + '(1 2)      0)) (nl)
(princ "rr 0 #4: ") (write (right-reduce        + '(1 2 3)    0)) (nl)
(princ "rr 0 #5: ") (write (right-reduce        + '(1 2 3 4)  0)) (nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore
 (nl)
 ;; (log-eval t)
 (princ "r 0  #1: ") (write (reduce              + '()         0)) (nl)
 (princ "r 0  #2: ") (write (reduce              + '(1)        0)) (nl)
 (princ "r 0  #3: ") (write (reduce              + '(1 2)      0)) (nl)
 (princ "r 0  #4: ") (write (reduce              + '(1 2 3)    0)) (nl)
 (princ "r 0  #5: ") (write (reduce              + '(1 2 3 4)  0)) (nl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
