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
(when t
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
;; Count results of rolling 2d6 20000 times.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil
 (setq! *counts* '(2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0 10 0 11 0 12 0))
 (repeat 20000
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'proto)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

