;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (log-eval  t)
;; (log-core  t)
;; (log-macro t)
(setq! lst '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)

;; (write (memq? 5 lst)) (nl)
;; (write (memql? 5 lst)) (nl)
;; (write (indexq 5 lst)) (nl)
;; (write (indexql 5 lst)) (nl)
;; (write (removeq 5 lst)) (nl)
;; (write (removeql 5 lst)) (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sort (lst predicate)
  (if (or (not lst) (not (cdr lst))) ; If list has 0 or 1 element
      lst                          ; it's already sorted
      (let ((middle (half lst)))
        (merge (sort (car middle) predicate)
               (sort (cdr middle) predicate)
               predicate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun half (lst)
  "Splits lst into two approximately equal halves."
  (let ((slow lst)
        (fast (cdr lst)))
    (while (and fast (cdr fast))
      (setq! slow (cdr slow))
      (setq! fast (cdr (cdr fast))))
    (let ((second-half (cdr slow)))
      (rplacd! slow nil) ; Split the list
      (cons lst second-half))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun merge (lst1 lst2 predicate)
  "Merges two sorted lists based on predicate."
  (cond
   ((not lst1) lst2)
   ((not lst2) lst1)
   ((predicate (car lst1) (car lst2))
    (cons (car lst1) (merge (cdr lst1) lst2 predicate)))
   (t
    (cons (car lst2) (merge lst1 (cdr lst2) predicate)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun even-first-comparator (a b)
  (cond
    ;; If both are even or both are odd, just compare them
    ((== (/ a 2) (/ b 2)) (< a b))
    ;; If a is even and b is odd, a should come first
    ((even? a) t)
    ;; Otherwise, b should come first
    (t nil)))


(setq! combined-comparator 
  (lambda (x y) 
    (cond 
      ((and (even? x) (even? y)) (< x y))  ; both even, compare values
      ((even? x) t)                        ; x is even, y is odd, x comes first
      ((even? y) nil)                      ; y is even, x is odd, y comes first
      (t (< x y)))))                       ; both odd, compare values


(setq! lst '(3 1 13 2 8 4 5 12 7 11 9 6 10 15 14))

(write (sort lst combined-comparator)) (nl)

(exit)
