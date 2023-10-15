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


;; (setq! qq
;;  (with-log-all
;;   3 
;;   '(* 2 7) 
;;   (lambda () 44)))

(nl)
(princ "state eval: ") (write (log-eval)) (nl)
(princ "state core: ") (write (log-core)) (nl)
(princ "this: ") (princ qq) (nl)
(princ "I'm not sure if debug is still on here?") (nl)

(write (apply + '(1 2))) (nl)
(write (apply + 1 2 '(3 4))) (nl)
(write (apply + 1 2 '(* 3 4) '(5 6))) (nl)

(log-core nil)
(log-eval nil)

(nl)
(write (apply + '(1 2))) (nl)
(write (apply + 1 2 '(3 4))) (nl)
(write (apply + 1 2 '(* 3 4) '(5 6))) (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; construction zone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (log-core t)
;; (log-eval t)


(setq! all-not-nil?
  (lambda (lsts)
    (if (nil? lsts)
        t
        (and (not (nil? (car lsts))) (all-not-nil? (cdr lsts))))))

(setq! heads
  (lambda (lsts)
    (if (nil? lsts)
        nil
        (cons (car (car lsts)) (heads (cdr lsts))))))

(setq! tails
  (lambda (lsts)
    (if (nil? lsts)
        nil
        (cons (cdr (car lsts)) (tails (cdr lsts))))))

(setq! zip
  (lambda args
    (let ((lsts args))
      (if (all-not-nil? lsts)
          (cons (heads lsts) (zip (tails lsts)))
          nil))))

;; Expected output: ((1 a) (2 b) (3 c))

(write (any? even? '(1 3 a 5))) (nl)
(write (any? even? '(1 3 a 4 5))) (nl)

;; (log-all t)

(write (zip2 '(1 2 3) '(a b c d))) (nl)

(write (flatten1 '(a (b c)))) (nl)

(setq! lsts '((a b) (2 3) (4 5)))
(nl)
(write (mapcar flatten1 (reduce zip2 (car lsts) (cdr lsts)))) (nl)
(write (zip3 '(a b) '(2 3) '(4 5))) (nl)


(setq! even? (lambda (n) (== 0 (% n 2))))
(setq! odd?  (lambda (n) (== 1 (% n 2))))

(setq! preds-match
 (lambda (val preds acc)
  (cond
   ((nil? (car preds)) acc)
   ((nil? ((car preds) val)) nil)
   (t (preds-match val (cdr preds) acc)))))
  
  ;; (if (nil? ((car preds) val))
  ;;  nil
  ;;  (if (nil? (cdr preds))
  ;;   t
  ;;   (preds-match val (cdr preds))))))

(write (preds-match 7  (list integer? odd?) t)) (nl)
(write (preds-match 8  (list integer? odd?) t)) (nl)
(write (preds-match 'a (list integer? odd?) t)) (nl)
