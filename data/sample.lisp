;; (setq! ct 8)

;; (while (> ct 0)
;;   (write ct) (nl)
;;   (setq! ct (- ct 1)))

;; (write (filter odd? '(1 2 3 4 5 6 7 8 9 10))) (nl)

;; (write (filter (lambda (x) (not (nil? x))) '(a nil b c nil d))) (nl)

;; (setq! lst  '(1 2 3 4))
;; (setq! lst2 '(6 7 8 9))

;; (princ "one:   ") (write (push-back! lst 5))    (nl)
;; (princ "two:   ") (write (push!      0   lst))  (nl)
;; (princ "three: ") (write (nconc!     lst lst2)) (nl)

;; (setq! lst '(1 2))
;; (setq! lst2 '(3 4))
;; (setq! lst3 '(5 6))

;; (write (nconc! lst lst2 lst3)) (nl)

;; (write lst) (nl)

;; (write (mapconcat         (lambda (x) x) '("a" "b" "c") " "))  (nl)
;; (write (apply mapconcat '((lambda (x) x) '("a" "b" "c") " "))) (nl)

;; ;; (write (equal? '(a (1 2)) '(a (1 2)))) (nl)

;; (equal? '(1) '(1))

;; (setq! l '(1 2 (3 4)))
;; (transform! l integer? double)
;; (write l) (nl) ;; (2 4 (6 8))

;; (setq! l '(1 2 (3 4)))
;; (setq! new-l (transform l integer? double))
;; (write l) (nl)     ;; (1 2 (3 4)) remains unchanged
;; (write new-l) (nl) ;; (2 4 (6 8)) the transformed list

(setq! replicate-or-ignore
 (lambda (x)
  (if (integer? x)
      (list x x)
      nil)))

(setq! mapcan!
  (lambda (fun lst)
    (let* ((loop (lambda (prev current)
                   (if (nil? current)
                       lst
                       (let ((result (fun (car current))))
                         (cond
                          ((nil? result) ; Skip this element
                           (progn
                             (if prev 
                                 (rplacd! prev (cdr current))
                                 (setq! lst (cdr lst)))
                             (loop prev (cdr current))))
                          (t ; Merge in the result list
                           (progn
                             (rplaca! current (car result))
                             (rplacd! current (cdr result))
                             (while (cdr result) ; Traverse the result list
                               (setq! prev result)
                               (setq! result (cdr result)))
                             (loop prev (cdr current))))))))))

      (loop nil lst))))

(setq! mapcan!
 (lambda (fun lst)
  (let* ((dummy-head (cons 'dummy nil))
         (last-cell dummy-head))
   (mapc (lambda (item)
          (let ((result (fun item)))
           (when result
            (rplacd! last-cell result)
            (while (cdr last-cell)
             (setq! last-cell (cdr last-cell))))))
    lst)
   (cdr dummy-head))))

(setq! mapcan!
 (lambda (fun lst)
  (let ((dummy-head (cons 'dummy lst))
        (prev dummy-head))
    (while (cdr prev)
      (let ((result (fun (car (cdr prev)))))
        (if result
            (begin
              (rplacd! prev result)
              (while (cdr prev)
                (setq! prev (cdr prev))))
            (rplacd! prev (cdr (cdr prev))))))
    (cdr dummy-head))))

(setq! mylist '(1 "a" 2 3 "b" 4)) 
(write (mapcan! replicate-or-ignore mylist)) (nl)
(write mylist) (nl)
