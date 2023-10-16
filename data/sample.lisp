
(defun 2* (x) (* 2 x)) (nl)

(princ "this should print 6: " ) (write (2* 3)) (nl) ;; successfuly prints 6.
(princ "this should print 6: " ) (write (2* 3)) (nl) ;; does not re-expand.


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

(setq! zip (reduced (lambda (x y) (zip2 x y)) arg))
(princ "zip: ") (write (zip '(1 2 3) '(a b c) '(10 20 30) '(x y z))) (nl)
 

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
