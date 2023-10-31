;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (nl)
;; (repeat 10 (princ (random))         (nl))
;; (repeat 10 (princ (random 10))      (nl))
;; (repeat 10 (princ (random -10 10))  (nl))

;; #|

(defun trav-upp ()
 (let ((attrs $(:soc :edu :int :end :dex :str))
       (upp nil))
  (while attrs
   (setq! upp (kset (car attrs) upp(+ (random 1 7) (random 1 7))))
   (setq! attrs (cdr attrs)))
  upp))

;; (repeat 200
;;  (princ (trav-upp))
;;  (nl))

#|
(setq! *counts* '(1 0 2 0 3 0 4 0 5 0 6 0))

(repeat 15000
(let ((roll (random 1 7)))
(setq! *counts* (kset roll *counts*
(if (khas? roll *counts*)
(1+ (kget roll *counts*))
1)))))

(princ *counts*) (nl)
|#


;; Using the random function we came up with, I tried to simulate rolling 2d6 50,000 times.

;; (setq! *counts* '(2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0 10 0 11 0 12 0))

;; (repeat 50000
;;  (let ((roll (+ (random 1 7) (random 1 7))))
;;   (setq! *counts*
;;    (plist-set roll *counts*
;;     (if (plist-has? roll *counts*)
;;      (1+ (plist-get roll *counts*))
;;      1)))))

;; (princ *counts*) (nl)

;; Output: (2 0 3 5495 4 0 5 11329 6 0 7 16435 8 0 9 11149 10 0 11 5592 12 0)

;; It seems surprising that, in 50,000 rolls, totals of 2 and 12 never came up.


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq! *counts* '(1 0 2 0 3 0 4 0 5 0 6 0))

;; (repeat 50000
;;  (let ((roll (random 1 7)))
;;   (setq! *counts*
;;    (plist-set roll *counts*
;;     (if (plist-has? roll *counts*)
;;      (1+ (plist-get roll *counts*))
;;      1)))))

;; (princ *counts*) (nl)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; (princ *matrix*) (nl)


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

;;(log-eval t)
(repeat 6
 (let ((current-row (nth (/ *counter* 6) *matrix*)))
  (set-next-int current-row)
  (set-next-int current-row)
  (set-next-int current-row)
  (set-next-int current-row)
  (set-next-int current-row)
  (set-next-int current-row)))

(princ *matrix*) (nl)


(setq! *counter* 0)
(setq! *matrix*
 (list
  (list 0 0 0 0 0 0)
  (list 0 0 0 0 0 0)
  (list 0 0 0 0 0 0)
  (list 0 0 0 0 0 0)
  (list 0 0 0 0 0 0)
  (list 0 0 0 0 0 0)))


;; Well, we were working towards making this work, to test whether randomness is maintained properly between successive die rolls:
;; (repeat 20000
;;  (let ((roll1 (random 1 7))
;;        (roll2 (random 1 7)))
;;   (unless (and (>= roll1 1) (<= roll1 6) (>= roll2 1) (<= roll2 6))
;;    (princ "Unexpected rolls: roll1=" roll1 " roll2=" roll2) (nl))

;;   ;;(princ roll1 " " roll2) (nl)
;;   ;; Let's break it down step-by-step
;;   (let* ((sublist (nth (- roll1 1) *matrix*))
;;          (current-val (nth (- roll2 1) sublist))
;;          (new-val (1+ current-val)))
;;    (list-set! sublist (- roll2 1) new-val)
;;    )))

(princ *matrix*) (nl)

;; But currently, it prints ((0 0 0 0 0 0) (2195 0 2200 0 2173 0) (0 0 0 0 0 0) (2238 0 2265 0 2237 0) (0 0 0 0 0 0) (2187 0 2175 0 2330 0)),
;; it's only setting odd rows and even columns.

;; Maybe, now that we have that working example of setting matrix cells, we can fix it.

;;(repeat 100 (princ (random)) (nl))


;;(defun massaged-random (min max) (let ((substitute-max (- (<< max 1) 1))) (>> (+ (random min substitute-max) 1) 1))) (repeat 100 (princ (massaged-random 1 7)) (nl))



(let ((seed (now-us)))
 (defun xorshift64 ()
  "Generate a pseudo-random positive integer."
  (when (zero? seed)
   (setq! seed (now-us)))
  
  (setq! seed (^ seed (<< seed 13)))
  (setq! seed (^ seed (>> seed 7)))
  (setq! seed (^ seed (<< seed 17)))
  
  (abs seed)))

(defun random args
 "Return a psuedo-random integer between MIN and MAX inclusive."
 (unless (or (nil? args) (nil? (cddr args)))
  (error "random takes either 0, 1 or 2 arguments"))
 (unless (or (nil? (car args)) (integer? (car args)))
  (error "If provided, first argument must be an integer"))
 (unless (or (nil? (cdr args)) (integer? (cadr args)))
  (error "If provided, second argument must be an integer"))

 (let ((randval (xorshift64)))
  (if args
   (let* ((min (if (cadr args) (car  args) 0))
          (max (if (cadr args) (cadr args) (car args)))
          (range (+ 1 (- max min))))
    (+ min (mod (xorshift64) range)))
   randval)))

(repeat 100 (princ (random 1 6)) (nl))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'proto)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
