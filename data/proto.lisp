;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'proto)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-recursive reduce I haven't tested yet.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun reduce (func lst &optional initial-value)
;;   "Reduce LST using FUNC starting with INITIAL-VALUE."
;;   (unless (list? lst) (error "LST must be a list"))
;;   ;; If there's an initial value, start with it. Otherwise, start with the first element of the list.
;;   (let ((acc (if initial-value
;;                  initial-value
;;                  (pop! lst))))
;;     ;; Iterate over the rest of the list, updating the accumulator.
;;     (while lst
;;       (setq! acc (funcall func acc (pop! lst)))
;;     ;; Return the accumulated result.
;;     acc)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a bunch of Traveller UPPs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
 (defun trav-upp ()
  (let ((attrs $(:soc :edu :int :end :dex :str))
        (upp nil))
   (while attrs
    (setq! upp (kset (car attrs) upp(+ (random 1 6) (random 1 6))))
    (setq! attrs (cdr attrs)))
   upp))
 (repeat 200
  (princ (trav-upp))
  (nl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Count results of rolling 2d6 10000 times.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
 (setq! *counts* '(2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0 10 0 11 0 12 0))
 (repeat 10000
  (let ((roll (+ (random 1 6) (random 1 6))))
   (setq! *counts*
    (plist-set roll *counts*
     (if (plist-has? roll *counts*)
      (1+ (plist-get roll *counts*))
      1)))))
 (princ *counts*) (nl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Count results of rolling 1d6 10000 times.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
 (setq! *counts* '(1 0 2 0 3 0 4 0 5 0 6 0))
 (repeat 10000
  (let ((roll (random 1 7)))
   (setq! *counts*
    (plist-set roll *counts*
     (if (plist-has? roll *counts*)
      (1+ (plist-get roll *counts*))
      1)))))
 (princ *counts*) (nl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore
 (setq! *counter* 0)
 (setq! *matrix*
  (list
   (list 0 0 0 0 0 0)
   (list 0 0 0 0 0 0)
   (list 0 0 0 0 0 0)
   (list 0 0 0 0 0 0)
   (list 0 0 0 0 0 0)
   (list 0 0 0 0 0 0)))

 (defun set-next-int (row)
  (list-set! row (mod *counter* 6) (setq! *counter* (+ *counter* 1))))

 (repeat 6
  (let ((current-row (nth (/ *counter* 6) *matrix*)))
   (set-next-int current-row)
   (set-next-int current-row)
   (set-next-int current-row)
   (set-next-int current-row)
   (set-next-int current-row)
   (set-next-int current-row)))
 (princ *matrix*) (nl)

 (setq! *matrix*
  (list
   (list 0 0 0 0 0 0)
   (list 0 0 0 0 0 0)
   (list 0 0 0 0 0 0)
   (list 0 0 0 0 0 0)
   (list 0 0 0 0 0 0)
   (list 0 0 0 0 0 0)))

 (repeat 10000
  (let ((roll1 (random 1 6))
        (roll2 (random 1 6)))
   (unless (and (>= roll1 1) (<= roll1 6) (>= roll2 1) (<= roll2 6))
    (princ "Unexpected rolls: roll1=" roll1 " roll2=" roll2) (nl))
   (let* ((sublist (nth (- roll1 1) *matrix*))
          (current-val (nth (- roll2 1) sublist))
          (new-val (1+ current-val)))
    (list-set! sublist (- roll2 1) new-val)
    )))

 (princ *matrix*) (nl)

 (repeat 100 (princ (random 1 6)) (nl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Hmm.. I could use a function that rotates a 'matrix' structure right by 90 degrees. So, if given: 

'((1 2 3)
  (4 5 6)
  (7 8 9))

;; ... it should return:

'((7 4 1)
  (8 5 2)
  (9 6 3))


(defun transpose (matrix)
 (if (null (car matrix))
  '()
  (cons (mapcar car matrix) (transpose (mapcar cdr matrix)))))

(defun reverse-rows (matrix)
 (mapcar reverse matrix))

(defun consistent-matrix? (matrix)
 "t if the rows in MATRIX all have the same length."
 (unless (cons? matrix) (error "MATRIX must be a non-empty list"))
 (let ((first-row-length (length (car matrix))))
  (all? (lambda (row)
         (unless (cons? matrix) (error "MATRIX's rows must all be non-empty lists"))
         (= (length row) first-row-length))
   (cdr matrix))))

(defun rotate-right-90 (matrix)
 (unless (consistent-matrix? matrix)
  (error "MATRIX is inconsistent. All rows must have the same number of columns."))
 (reverse-rows (transpose matrix)))



(ignore
 (let ((test-matrix '((1 2 3) (4 5 6) (7 8 9))))
  (princ (rotate-right-90 test-matrix)))
 (nl))

;;((7 4 1) (8 5 2) (9 6 3))

(setq! data (rotate-right-90 (mapcar vals (set! data (cdr (read "results.lisp"))))))

(when t
 (setq! ctr 1)
 (princ (mapcar (lambda (x) (cons (setq! ctr (+ 1 ctr)) (list x))) (mapcar sum (rotate-right-90 (mapcar vals (cdr (read "results.lisp"))))))) 
 (nl))



