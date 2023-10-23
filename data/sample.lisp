;; (exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun curry1 (fun arg1)
;;  (lambda args
;;   (apply fun arg1 args)))

;; (setq! 2+ (curry1 + 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq! lst (union2 memql? '(1 2 3 4) '(4 5 2 2)))
(setq! lst '(1 2 3 4 5 6 7 8 9 10))

(princ "initial lst:     ") (write lst)                  (nl)
;; (log-eval t) (log-core t)
(princ "memql? 2:        ") (write (memql?   3 lst))     (nl)
(princ "removeql 4:      ") (write (removeql 4 lst))     (nl)
(princ "indexql 5:       ") (write (indexql  5 lst))     (nl)
;; (log-core t)
(princ "mapcar!:         ") (write (mapcar! double lst)) (nl)
(princ "doubled:         ") (write lst)                  (nl)
;; (log-eval t)
;; (log-core t)
(princ "butlast:         ") (write (butlast '(1 2 3 4 5)))             (nl)
;; (exit)
(princ "reverse:         ") (write (reverse '(1 2 3 4 5)))             (nl)
(princ "reverse butlast: ") (write (reverse (butlast '(1 2 3 4 5))))   (nl)
;; (princ "union:           ") (write (unionql '(1 2 3) '(4 5 6)))        (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! lst (make-list 6 200))
(princ "make-list:       ") (write lst) (nl)

(list-set! lst 0 100)
(princ "list-set! 0 100: ") (write lst) (nl)
(princ "list-ref  0:     ") (write (list-ref lst 0)) (nl)

(list-set! lst 1 101)
(princ "list-set! 1 101: ") (write lst) (nl)
(princ "list-ref  1:     ") (write (list-ref lst 1)) (nl)

(list-set! lst 2 102)
(princ "list-set! 2 102: ") (write lst) (nl)
(princ "list-ref  2:     ") (write (list-ref lst 2)) (nl)

(list-set! lst 3 103)
(princ "list-set! 3 103: ") (write lst) (nl)
(princ "list-ref  3:     ") (write (list-ref lst 3)) (nl)

(list-set! lst 4 104)
(princ "list-set! 4 104: ") (write lst) (nl)
(princ "list-ref  4:     ") (write (list-ref lst 4)) (nl)

(list-set! lst 5 105)
(princ "list-set! 5 105: ") (write lst) (nl)
(princ "list-ref  5:     ") (write (list-ref lst 5)) (nl)

;; (exit)

;;(write (list-ref lst 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when nil
 (defun lshift4 (n) (<< n 4))
 
 (add-logging-to lshift4) (nl)
 (princ 'lshift4 "'s body is now " (body lshift4)) (nl) (nl)
 (princ "Call returned " (lshift4 4) ".") (nl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (log-eval t)

(defun select-and-move-to-front! (pred? lst)
 "Move the first item matching pred? to the front of the list."
 (let ((head (car lst)))
  (if (pred? head)
   head
   (let ((obj      (remove-first! pred? lst))
         (new-tail (cons (car lst) (cdr lst))))
    (rplaca! lst obj)
    (rplacd! lst new-tail)
    obj))))

(defun remove-first! (pred? lst)
 "Destructivelyl Remove the first item matching pred? from the list."
 (if (pred? (car lst))
  (if (cdr lst)
   (progn 
    (rplaca! lst (cadr lst))
    (rplacd! lst (cddr lst)))
   (error "can't remove last item"))
  (let ((prev lst) (current (cdr lst)))
   (while (and current (not (pred? (car current))))
    (setq! prev current)
    (setq! current (cdr current)))
   (if current
    (progn
     (rplacd! prev (cdr current))
     (car current))
    (error "obj was not in lst")))))

(defun remove-first! (pred? lst)
 "Destructively remove the first item matching pred? from the list."
 (let ((head (car lst))
       (tail (cdr lst)))
  (if (pred? head)
   (if (nil? tail)
    (error "can't remove last item")
    (rplaca! lst (second lst))
    (rplacd! lst (cddr   lst)))
   (let ((prev     lst)
         (current  (cdr lst)))
    (letrec
     ((chase
       (lambda (lst)
        (let ((head (car lst))
              (next (cdr lst)))
         (cond
          ((pred? head) (progn (rplacd! prev next) head))
          (next         (progn (setq! prev lst) (chase next)))
          (t            (error "obj was not in lst"))
          )))))
     (chase current))))))

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

(defun curry1 (fun arg1) (lambda args (apply fun arg1 args)))

(log-eval t)

((curry1 + 2) 8)

(exit)

(setq! c (curry1 nthcdr 2)))

(princ "Here ") (nl)
(princ (c lst)) (nl)

;; (defmacro pop! (list-sym &optional (n 1))
;;  `(let ((head (car ,list-sym)))
;;    (setq! ,list-sym (nthcdr ,n ,list-sym))
;;    head))

