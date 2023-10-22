
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

;; (defun curry1 (fun arg1)
;;  (lambda args
;;   (apply fun arg1 args)))

;; (setq! 2+ (curry1 + 2))

(defun lshift4 (n) (<< n 4))

(add-logging-to lshift4) (nl)
(princ 'lshift4 "'s body is now " (body lshift4)) (nl) (nl)
(princ "Call returned " (lshift4 4) ".") (nl)
;; (add-logging-to lshift4) (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun select-and-move-to-front (lst pred?)
  "Incrementally sorts the list by selecting the first element that matches the predicate.
  If a matching element is found, it is moved to the front of the list.
  Returns the selected value or nil if not found."

  (cond
    ((nil? lst) nil)
    ((pred? (car lst)) (car lst)) ; The first element matches the predicate.

    (t
     (let ((prev lst)
           (current (cdr lst)))
       (while (and current (not (pred? (car current))))
         (setq! prev current)
         (setq! current (cdr current)))

       ;; If we found a match and it's not the last element
       (when (and current (pred? (car current)))
         (if (atom? (cdr current)) ; If the current is pointing to the last element
           (car current) ; We just return the value without modifying the list

           ;; Otherwise, we remove the current element from the list
           ;; and place it at the front
           (progn
             (rplacd! prev (cdr current)) ; Remove current element from list
             (rplacd! current lst) ; Make current's next point to the beginning of the list
             (rplaca! lst (car current))
             (car current)))))))) ; Return the selected value

(log-eval t)
(princ (select-and-move-to-front (lambda (x) (> x 2)) '(1 2 3 . 4)))
(nl)


