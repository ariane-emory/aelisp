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


(setq! lst $(2 4 1 5 3 7 9 6 8))
(nl)

(defun copy-list (lst)
  "Returns a shallow copy of the given list."
  (when lst (cons (car lst) (copy-list (cdr lst)))))

(defun removeql! (obj lst)
 (let ((head (car lst))
       (tail (cdr lst)))
  (if (eql? obj head)
   (if (nil? tail)
    (error "can't remove last item")
    (rplaca! lst (second lst))
    (rplacd! lst (cddr   lst)))
   (let ((prev     lst)
         (position (cdr lst)))
    (letrec
     ((chase
       (lambda (lst)
        (let ((head (car lst))
              (tail (cdr lst)))
         (cond
          ((eql? obj head) (rplacd! prev tail))
          (tail            (progn (setq! prev lst) (chase tail)))
          (t               (error "obj was not in lst"))
          )))))
     (chase position)))))
 obj)

(defun remove-first! (obj lst)
 (let ((head (car lst))
       (tail (cdr lst)))
  (if (eql? obj head)
   (if (nil? tail)
    (error "can't remove last item")
    (rplaca! lst (second lst))
    (rplacd! lst (cddr   lst)))
   (let ((prev     lst)
         (position (cdr lst)))
    (letrec
     ((chase
       (lambda (lst)
        (let ((head (car lst))
              (tail (cdr lst)))
         (cond
          ((eql? obj head) (rplacd! prev tail))
          (tail            (progn (setq! prev lst) (chase tail)))
          (t               (error "obj was not in lst"))
          )))))
     (chase position)))))
 obj)

(defun select-and-move-to-front! (obj lst)
 (remove-first! obj lst)
 (let ((new-tail (cons (car lst) (cdr lst))))
  (rplaca! lst obj)
  (rplacd! lst new-tail))
 obj)
  ;; (1 2 3 4)

(princ (select-and-move-to-front! 9 lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! 8 lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! 7 lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! 6 lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! 5 lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! 4 lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! 3 lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! 2 lst)) (spc) (write lst) (nl)
(princ (select-and-move-to-front! 1 lst)) (spc) (write lst) (nl)


;; (princ (removeql! 1 lst)) (spc) (write lst) (nl)
;; (princ (removeql! 6 lst)) (spc) (write lst) (nl)
;; (princ (removeql! 9 lst)) (spc) (write lst) (nl)
;; (princ (removeql! 2 lst)) (spc) (write lst) (nl)
;; (princ (removeql! 3 lst)) (spc) (write lst) (nl)
;; (princ (removeql! 5 lst)) (spc) (write lst) (nl)
;; (princ (removeql! 4 lst)) (spc) (write lst) (nl)
;; (removeql! 5 lst) (write lst) (nl)
