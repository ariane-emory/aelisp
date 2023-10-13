(setq! ct 8)

(while (> ct 0)
  (write ct) (nl)
  (setq! ct (- ct 1)))

(write (filter odd? '(1 2 3 4 5 6 7 8 9 10))) (nl)

(write (filter (lambda (x) (not (nil? x))) '(a nil b c nil d))) (nl)

(setq! lst  '(1 2 3 4))
(setq! lst2 '(6 7 8 9))

(princ "one:   ") (write (push-back! lst 5))    (nl)
(princ "two:   ") (write (push!      0   lst))  (nl)
(princ "three: ") (write (nconc!     lst lst2)) (nl)

(princ "four:  ") (write (mapconcat         (lambda (x) x) '("a" "b" "c") " "))  (nl)
(princ "five:  ") (write (apply mapconcat '((lambda (x) x) '("a" "b" "c") " "))) (nl)

;; (write (equal? '(a (1 2)) '(a (1 2)))) (nl)

(equal? '(1) '(1))

(setq! l '(1 2 (3 4)))
(transform! l integer? double)
(write l) (nl) ;; (2 4 (6 8))

(setq! l '(1 2 (3 4)))
(setq! new-l (transform l integer? double))
(write l) (nl)     ;; (1 2 (3 4)) remains unchanged
(write new-l) (nl) ;; (2 4 (6 8)) the transformed list

(setq! replicate-or-ignore
 (lambda (x)
  (if (integer? x)
      (list x x)
      nil)))

(setq! mylist '(1 "a" 2 3 "b" 4)) 
(write (mapcan replicate-or-ignore mylist)) (nl)
(write (mapcan replicate-or-ignore mylist)) (nl)
(write mylist) (nl)

(setq! lst1 '(1 2))
(setq! lst2 '(3 4))
(setq! lst3 '(5 6))

(write (nconc! lst1 lst2 lst3)) (nl)

(write lst1) (nl)

(setq! append-multiple
 (lambda lists
  "Append multiple lists."
  (cond
   ((nil? lists) '())                 ; Return an empty list if no lists are provided.
   ((nil? (cdr lists)) (car lists))   ; If there's only one list left, return it.
   (t (append (car lists)             ; Otherwise, append the first list with the result of appending the rest.
              (apply append-multiple (cdr lists)))))))

(setq! append-multiple
 (lambda (lists)
  "Append multiple lists."
  (cond
   ((nil? lists) '())                 ; Return an empty list if no lists are provided.
   ((nil? (cdr lists)) (car lists))   ; If there's only one list left, return it.
   (t (append (car lists)             ; Otherwise, append the first list with the result of appending the rest.
              (append-multiple (cdr lists)))))))

;; I had to adjust some syntax to fit my language:
(setq! append-multiple
  (lambda lists
    "Append multiple lists."
    (cond
     ((nil? lists) '())                 ; Return an empty list if no lists are provided.
     ((nil? (cdr lists)) (car lists))   ; If there's only one list left, return it.
     (t (append (car lists)             ; Otherwise, append the first list with the result of appending the rest.
                (append-multiple (cdr lists)))))))

(setq! append-multiple
 (lambda lists
  "Append multiple lists."
  (cond
   ((nil? lists) '())                 ; Return an empty list if no lists are provided.
   ((nil? (cdr lists)) (car lists))   ; If there's only one list left, return it.
   (t (append (car lists)             ; Otherwise, append the first list with the result of appending the rest.
              (apply append-multiple (cdr lists)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; construction zone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (write (apply + 1 2)) (nl)

(setq! append
 (lambda lists
  "Append two lists."
  (let ((lst1 (car lst))
        (lst2 (cadr lst)))
  (if (nil? lst1)
   lst2
   (cons (car lst1) (append (cons (cdr lst1) lst2)))))))

(princ "six:      ") (write (append '(1 2) '(3 4))) (nl)
;;(princ "seven:    ") (write (append '(1 2) '(3 4) '(5 6) '(7 8))) (nl)

