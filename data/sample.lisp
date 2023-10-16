
(defun 2* (x) (* 2 x)) (nl)

(princ "this should print 6: " ) (write (2* 3)) (nl) ;; successfuly prints 6.
(princ "this should print 6: " ) (write (2* 3)) (nl) ;; does not re-expand.

(write (append '(1 2) '(3 4) '(5 6))) (nl)

;; (log-all t)
(write (nconc! '(1 2) '(3 4) '(5 6))) (nl)


;; (defun nconc! lists
;;  (reduce nconc2! (car lists) (cdr lists)))

(write (nconc! '(1 2) '(3 4) '(5 6))) (nl)

(setq! nconc! (reduced nconc2!))
(write (nconc! '(1 2) '(3 4) '(5 6))) (nl)


(defun add (x y) (+ x y))

(setq! sum (reduced add))

(write (sum 1 2 3 4 5 6)) (nl)

(princni ", " 1 2 3)

(princ "zip2: ") (write (zip2 '(1 2 3) '(a b c))) (nl)

(princ "zip3: ") (write (zip3 '(1 2 3) '(a b c) '(10 20 30))) (nl)


(defun flatten (lst)
 (cond
  ((atom? lst) (list lst))
  (t (append (flatten (car lst)) (flatten (cdr lst))))))


;; What you just gave me seems to recurse forever without returning.

;; This verstion of zip:
(defun zip2 (lst1 lst2)
 "Zip two lists."
 (cond
  ((or? (nil? lst1) (nil? lst2)) nil)
  (t (cons (list (car lst1) (car lst2))
      (zip2 (cdr lst1) (cdr lst2))))))
(defun reduced (fun)
 "Return a function that is a reduction of the binary function fun."
 (lambda args
  (reduce fun (car args) (cdr args))))
(setq! zip (reduced (lambda (x y) (zip2 x y)) arg))
(princ "zip: ") (write (zip '(1 2 3) '(a b c) '(10 20 30) '(x y z))) (nl)

;; Prints: zip: ((((1 a) 10) x) (((2 b) 20) y) (((3 c) 30) z))
;; If I could mapcar some fun over the results, I would have the desired result.



(defun flatten-deep (lst)
  "Flatten a deeply nested list."
  (cond
    ((not? (cons? lst)) (list lst)) ; If it's an atom, return it as a single-element list
    ((not? (cons? (car lst))) ; If the car is atomic
     (cons (car lst) (if (cons? (cdr lst)) (flatten-deep (cdr lst)) (list (cdr lst)))))
    (t (append (flatten-deep (car lst)) (flatten-deep (cdr lst))))))

(defun flatten-zipped-lists (zipped-lst)
  "Flatten the zipped lists."
  (mapcar flatten-deep zipped-lst))

(princ "flat: ") (write (flatten-zipped-lists (zip '(1 2 3) '(a b c) '(10 20 30) '(x y z)))) (nl)

(princ "flat: ") (write (flatten-zipped-lists (zip '(1 2 3) '(a b c) '(10 20 30) '(x y z)))) (nl)



;; Test cases
(write (flatten '(((2 b) 20) y))) (nl)
(write (flatten '((((z 2) b) 20) y))) (nl)
 

(defun flatten (lst)
  (if (nil? lst)
      nil
      (if (cons? (car lst))
          (append (flatten (car lst)) (flatten (cdr lst)))
          (cons (car lst) (flatten (cdr lst))))))

;; Test cases
(flatten '(((2 b) 20) y))   ; Output: (2 B 20 Y)
(flatten '((((z 2) b) 20) y)) ; Output: (Z 2 B 20 Y)


(princ "flat: ") (write (mapcar flatten (zip '(1 2 3) '(a b c) '(10 20 30) '(x y z)))) (nl)
