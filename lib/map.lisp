(require 'std-minimal)
(require 'measure-time)
(require 'nconc)

(report-time-us "def map variants               "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; list funs (map variants):                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun mapcar (fun lst)
  "Map fun over LST, returning the resulting list."
  (unless (fun? fun)   (error "FUN must be a function"))
  (unless (list? lst)  (error "LST must be a list"))
  (when lst
   (cons (fun (car lst)) (mapcar fun (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun mapcar* (fun . args) (apply mapcar fun (list args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun mapcar! (fun lst)
  "Map fun over LST, altering the list."
  (unless (fun? fun)   (error "FUN must be a function"))
  (unless (list? lst)  (error "LST must be a list"))
  (letrec
   ((mapcar-internal!
     (lambda (fun lst)
      (when lst
       (rplaca! lst (fun (car lst)))
       (mapcar! fun (cdr lst))))))
   (mapcar-internal! fun lst)
   lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun mapconcat (fun lst . rest)
  "Map fun over LST, returning the result of concatenating the resulting
   strings."
  (unless (fun? fun)     (error "FUN must be a function"))
  (unless (list? lst)    (error "LST must be a list"))
  (unless (or (nil? rest) (single? rest))
   (error "MAPCONCAT takes exactly only one optional arguments after LST"))
  (let ((delimiter (car rest)))
   (unless (or (nil? delimiter) (string? delimiter))
    (error "DELIMITER must be a string or nil"))
   (if lst
    (reduce
     (lambda (acc item) (concat acc delimiter item))
     (mapcar fun lst))
    "")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun mapcan (fun lst)
  "Map fun over LST and concatenate the results by altering them."
  (unless (fun? fun)   (error "FUN must be a function"))
  (unless (list? lst)  (error "LST must be a list"))
  (when lst
   (let ((result (fun (car lst)))
         (rest   (mapcan fun (cdr lst))))
    (if result
     (nconc! result rest)
     rest))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun mapc (fun lst)
  "Apply FUN to each element of LST for side effects only and return LST."
  (unless (fun? fun)   (error "FUN must be a function"))
  (unless (list? lst)  (error "LST must be a list"))
  (let ((current lst))
   (while current
    (fun (car current))
    (setq! current (cdr current)))
   lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun mapc* (fun . args) (apply mapc fun (list args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )

(provide 'map)
