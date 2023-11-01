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
 (princ *counts*) (nl)
 (exit))
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
;; Test setting matrix values.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (ignore
;;  (setq! *counter* 0)
;;  (setq! *matrix*
;;   (list
;;    (list 0 0 0 0 0 0)
;;    (list 0 0 0 0 0 0)
;;    (list 0 0 0 0 0 0)
;;    (list 0 0 0 0 0 0)
;;    (list 0 0 0 0 0 0)
;;    (list 0 0 0 0 0 0)))

;;  (defun set-next-int (row)
;;   (list-set! row (mod *counter* 6) (setq! *counter* (+ *counter* 1))))

;;  (repeat 6
;;   (let ((current-row (nth (/ *counter* 6) *matrix*)))
;;    (set-next-int current-row)
;;    (set-next-int current-row)
;;    (set-next-int current-row)
;;    (set-next-int current-row)
;;    (set-next-int current-row)
;;    (set-next-int current-row)))
;;  (princ *matrix*) (nl)

;;  (setq! *matrix*
;;   (list
;;    (list 0 0 0 0 0 0)
;;    (list 0 0 0 0 0 0)
;;    (list 0 0 0 0 0 0)
;;    (list 0 0 0 0 0 0)
;;    (list 0 0 0 0 0 0)
;;    (list 0 0 0 0 0 0)))

;;  (princ *matrix*) (nl)

;;  (repeat 100 (princ (random 1 6)) (nl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sum up the test results in results.lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
 (setq! ctr 1)
 (princ
  (mapcar
   (lambda (x)
    (cons (setq! ctr (+ 1 ctr)) (list x)))
   (mapcar sum (rotate-right-90 (mapcar vals (cdr (read "results.lisp"))))))) 
 (nl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try to approximate a rational number with a simpler fraction. This isn't working yet.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun continued-fractions (num den limit)
 "Generate the continued fraction representation of NUM/DEN."
 (let ((whole (floor (rational num den))))
  (cons whole
   (when (and (> limit 0) (not (zero? (- num (* whole den)))))
    (continued-fractions den (- num (* whole den)) (1- limit))))))

(defun cf-to-rational (cfs)
 "Convert a continued fraction representation to a rational number."
 (if (null (cdr cfs))
  (cons (car cfs) 1)
  (let ((recursion (cf-to-rational (cdr cfs))))
   (rational (+ (* (car cfs) (car recursion)) (cdr recursion)) 
    (car recursion)))))

(defun simpler-fraction (num den limit)
 "Get a simpler fraction approximation for NUM/DEN."
 (cf-to-rational (butlast (continued-fractions num den limit))))

(defun find-closest-approximation (num den max-denominator max-depth)
 "Find the closest rational approximation with a denominator less than MAX-DENOMINATOR."
 (let ((approx-depth 1)
       (best-num num)
       (best-den den)
       (best-error (abs (- num den)))  ; Set to the difference between num and den as the initial error
       current-rat current-error) 
  (while (<= approx-depth max-depth)
   (setq! current-rat (simpler-fraction num den approx-depth))
   (setq! current-error (abs (- (* num (cdr current-rat)) (* den (car current-rat)))))  ; Calculate the difference without division
   (when (< current-error best-error)
    (setq! best-error current-error)
    (setq! best-num (car current-rat))
    (setq! best-den (cdr current-rat)))
   (setq! approx-depth (1+ approx-depth)))
  (rational best-num best-den)))

;;(log-core t)
(defun brute-force-approximation (num den max-denominator)
 "Find a simpler approximation by brute force."
 (let ((best-num num)
       (best-den den)
       (best-error (abs (sub-rational (mul-rational num max-denominator) den))) ; Initial error
       (d 1)
       current-error)
  (while (<= d max-denominator)
   (when (not (zero? d))
    (let* ((n (round  (rational-to-float (rational-div num den))))
           (approx (rational-mul n d))
           (error (abs (sub-rational approx (cons num den)))))
     (when (< (rational-to-float error) (rational-to-float best-error))
      (setq best-error error)
      (setq best-num n)
      (setq best-den d))))
   (setq d (1+ d)))
  (cons best-num best-den)))

;; (let ((rat (cf-to-rational (continued-fractions 408 500 10))))
;;  (princ (brute-force-approximation (numer rat) (denom rat) 32)) (nl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Untested alternate approach to approximating a rational number with a simpler fraction.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun approximate-rational (num den max-denom max-error)
 "Find a simple rational approximation to the fraction num/den."
 (let ((best-approximation (rational num den))
       (best-error (rational-sub (rational num den) (rational 1 1))) ;; start with max error
       (current-denom 1))
  ;; For each denominator up to max-denom
  (while (<= current-denom max-denom)
   (let* ((multiplier (rational-div num current-denom))
          ;; simulate rounding using floor
          (approx-num (floor (rational-add multiplier (rational-div current-denom 2))))
          (approximation (rational approx-num current-denom))
          (current-error (rational-sub (rational num den) approximation)))
    ;; Check if this approximation is closer than the best found so far
    (when (rational-less-than? (abs current-error) (abs best-error))
     (setq! best-approximation approximation)
     (setq! best-error current-error)))
   ;; Increase the current denominator for the next iteration
   (setq! current-denom (1+ current-denom)))
  ;; Return the best approximation found
  best-approximation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wild west:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ignore 
 (defstruct cat name legs whiskers)
 (setq! higgs (make-cat "higgy" 4 t))

 (setq! matrix (make-matrix 6 6 0))
 (mutate-matrix matrix 6 6 (lambda (row col val) (+ val (* 10 row) col)))
 (write-matrix matrix))
;; (log-eval t)

;; (princ (max-delta '(1 3 7))) (nl)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xorshift64 ()
 "Generate a pseudo-random positive integer."
 (when (zero? *xorshift64-seed*) (setq! *xorshift64-seed* (now-us)))  
 (setq! *xorshift64-seed* (^ *xorshift64-seed* (<< *xorshift64-seed* 13)))
 (setq! *xorshift64-seed* (^ *xorshift64-seed* (>> *xorshift64-seed* 7)))
 (setq! *xorshift64-seed* (^ *xorshift64-seed* (<< *xorshift64-seed* 17)))
 (abs *xorshift64-seed*))

(setq! deltas nil)
(setq! ctr 0)

(repeat 5000
 (setq! *xorshift64-seed* (now-us))
 (sleep (random 2 20))
 (setq! *xorshift64-seed* (now-us))
 (setq! ctr (+ 1 ctr))
 (princ "Iter #" ctr) (nl)
 (setq! counts '(1 0 2 0 3 0 4 0 5 0 6 0)) ; Reset counts for each outer cycle.

 (repeat 5000
  (let ((roll (random 1 6)))
   ;; Increment the count for the generated roll.
   (setq! counts (plist-set roll counts (1+ (plist-get roll counts))))
   ))

 (princ "This cycle's counts:    " (vals counts)) (nl)
 (princ "This cycle's counts sum    " (sum (vals counts))) (nl)
 (princ "This cycle's max delta: " (max-delta (plist-values counts))) (nl)
 (setq! deltas (cons (max-delta (vals counts)) deltas)) 
 (princ "Deltas so far:          " deltas) (nl)
 (nl))
