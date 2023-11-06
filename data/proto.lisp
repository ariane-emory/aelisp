;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'proto)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ignore
 (defmacro defun (name params . docs-and-body)
  "Defun with support for stashing docstrings in a property. No tests written for this one yet."
  (let* ((split (split-list string? docs-and-body))
         (docs  (apply concat (intercalate " " (car split))))
         (body  (cadr split)))
   $('progn 
     $('setq name $('lambda params . body))
     $('put  docs ':doc name))))
 (defun somefun (x y)
  "Multiply two"
  "numbers."
  (* x y))
 (write (get :doc somefun)) (nl)
 (write (doc somefun))      (nl)
 (write (doc write))        (nl))
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
;;                  (pop lst))))
;;     ;; Iterate over the rest of the list, updating the accumulator.
;;     (while lst
;;       (setq acc (funcall func acc (pop lst)))
;;     ;; Return the accumulated result.
;;     acc)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Count results of rolling 2d6 10000 times.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
 (setq *counts* (copy-list '(2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0 10 0 11 0 12 0)))
 (repeat 1000
  (let ((roll (+ (random 1 6) (random 1 6))))
   (plist-set! *counts* roll
    (if (plist-has? *counts* roll)
     (1+ (plist-get *counts* roll))
     1))))
 (princ *counts*) (nl)
 (exit))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Count results of rolling 1d6 10000 times.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
 (setq *counts* (copy-list '(1 0 2 0 3 0 4 0 5 0 6 0)))
 (repeat 10000
  (let ((roll (random 1 7)))
   (plist-set! *counts* roll
    (if (plist-has? *counts* roll)
     (1+ (plist-get *counts* roll))
     1))))
 (princ *counts*) (nl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sum up the test results in results.lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
 (setq ctr 1)
 (princ
  (mapcar
   (lambda (x)
    (cons (setq ctr (+ 1 ctr)) (list x)))
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
   (setq current-rat (simpler-fraction num den approx-depth))
   (setq current-error (abs (- (* num (cdr current-rat)) (* den (car current-rat)))))  ; Calculate the difference without division
   (when (< current-error best-error)
    (setq best-error current-error)
    (setq best-num (car current-rat))
    (setq best-den (cdr current-rat)))
   (setq approx-depth (1+ approx-depth)))
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
     (setq best-approximation approximation)
     (setq best-error current-error)))
   ;; Increase the current denominator for the next iteration
   (setq current-denom (1+ current-denom)))
  ;; Return the best approximation found
  best-approximation))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wild west:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq deltas nil)
(setq ctr 0)

(when nil
 (repeat 10
  (setq *xorshift64-seed* (now-us))
  (sleep (random 2 20))
  (setq *xorshift64-seed* (now-us))
  (incr ctr)
  (princ "Iter #" ctr) (nl)

  ;; Initialize/reset the counts for this cycle.
  (setq counts (copy-list '(1 0 2 0 3 0 4 0 5 0 6 0)))

  (repeat 1000
   (sleep 3)

   (let ((roll (random 1 6)))
    (plist-set! counts roll (1+ (plist-get counts roll)))))

  (princ "This cycle's counts:    " (vals counts)) (nl)
  ;; (princ "This cycle's counts sum    " (sum (vals counts))) (nl)
  (princ "This cycle's max delta: " (max-delta (plist-vals counts))) (nl)
  (setq deltas (cons (max-delta (vals counts)) deltas)) 
  (princ "Deltas so far:          " deltas) (nl)
  (nl))

 (princ "Max delta: " (apply max deltas)) (nl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a bunch of Traveller UPPs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when nil

 (defun new-upp ()
  (let ((attrs $(:str :dex :end :edu :int :soc))
        rolls)
   (repeat 6 (push (+ (random 6) (random 6)) rolls))
   (make-plist attrs rolls)))

 (defstruct upp str dex end edu int soc)

 (defun new-upp ()
  (let ((upp (make-upp))
        (index -1))
   (until (= index 11)
    (list-set! upp (setq index (+ index 2)) (+ (random 1 6) (random 1 6))))
   upp))

 (defun upp-total (upp)
  (unless (upp? upp) (error "UPP must be a upp struct"))
  (apply + (vals upp)))
 
 (ignore ;; hypothetical
  (defun new-upp ()
   (build (upp (make-upp))
    (let ((index -1))
     (until (= index 11)
      (list-set! upp (setq index (+ index 2)) (+ (random 1 6) (random 1 6)))))))

  (defconstructor upp () ;; hypothetical
   (let ((index -1))
    (until (= index 11)
     (list-set! upp (setq index (+ index 2)) (+ (random 1 6) (random 1 6)))))))

 ;; (log-eval t)

 (setq
  ctr  0
  upps nil)
 
 (until (= ctr 500)
  (let ((upp (new-upp)))
   (if (>= (upp-total upp) 42)
    (push upp upps)
    (incr ctr))))

 (setq upps (sort!! upps (lambda (a b) (> (upp-total a) (upp-total b)))))
 
 (mapc (lambda (upp) (princ upp " " (upp-total upp)) (nl)) upps)
 nil 
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (ignore
(defun laugh (n . rest)
 (let ((acc (car rest)))
  (if (zero? n)
   acc
   (laugh (1- n) (cons 'ha acc)))))

(write (subst '(1 2 3 4 5 6 7 (8 5 9 5 10)) 5 'five)) (nl)
(write (transform-tree even? double '(1 2 3 4 5 6 7 (8 5 9 5 10)))) (nl)
(write (laugh 6)) (nl)

;;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mapcar (fun lst)
 "Map FUN over LST, returning the resulting list."
 (unless (fun?  fun) (error "FUN must be a function"))
 (unless (list? lst) (error "LST must be a list"))
 (when lst
  (let* ((result (list (fun (pop lst))))
         (tail   result))
   (while lst
    (let ((new-tail (list (fun (pop lst)))))     
     (setq tail (rplacd! tail new-tail))))
   result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun heads (lsts)
 "Return a list of the heads of the lists in LSTS."
 (unless (list? lsts) (error "LSTS must be a list of lists"))
 (unless (all list? lsts) (error "LSTS must be a list of lists"))
 (mapcar car lsts))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tails (lsts)
 "Return a list of the tails of the lists in LSTS."
 (unless (list? lsts) (error "LSTS must be a list of lists"))
 (unless (all list? lsts) (error "LSTS must be a list of lists"))
 (mapcar cdr lsts))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcar+ (fun first-lst . rest-lsts)
 "Map FUN over LSTS, returning the resulting list."
 (unless (fun?  fun)            (error "FUN must be a function"))
 (unless (list? first-lst)      (error "FIRST-LST must be a list"))
 (unless (all list? rest-lsts)  (error "REST-LSTS must all be lists"))
 (let ((lsts (cons first-lst rest-lsts)))
  (when lsts
   (princ lsts) (nl)
   (let* ((car-heads  (heads lsts))
          (lsts       (tails lsts))
          (result     (list  (apply fun car-heads)))
          (tail       result))
    (while lsts
     (let ((car-heads  (heads lsts))
           (new-tail   (list  (apply fun car-heads))))
      (setq lsts (tails lsts))
      (setq tail (rplacd! tail new-tail))))
    result))))

(defun mapcar+ (fun first-lst . rest-lsts)
 "Map FUN over FIRST-LST and REST-LSTS, returning the resulting list."
 (unless (fun? fun)            (error "FUN must be a function"))
 (unless (list? first-lst)     (error "FIRST-LST must be a list"))
 (unless (all list? rest-lsts) (error "REST-LSTS must all be lists"))
 
 (let ((lsts (cons first-lst rest-lsts))
       result tail new-item)

   ;; Check if any of the lists are empty, and if so, return nil
   (if (any nil? lsts) 
       nil
     ;; Initialize result and tail on the first set of items
     (setq new-item (apply fun (mapcar car lsts)))
     (setq result   (list new-item))
     (setq tail     result)
     
     ;; Now update lsts to be the tails of the lists
     (setq lsts (mapcar cdr lsts))

     ;; Iterate over the rest of the elements
     (while (not (any nil? lsts))
       (setq new-item (apply fun (mapcar car lsts)))
       ;; Add new item to the result list by updating the tail
       (rplacd! tail (list new-item))
       ;; Update tail to point to the last element
       (setq tail (cdr tail))
       (setq lsts (mapcar cdr lsts)))
     result)))


(princ "In.") (nl)
(write (mapcar+ + '(1 2 3) '(4 5 6) '(7 8 9))) (nl)
(princ "Out.") (nl)
;;(mapcar+ + '(1 2 3) '(4 5 6) '(7 8 9))
