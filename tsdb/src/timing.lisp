;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file:
;;;      module:
;;;     version:
;;;  written by:
;;; last update:
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "TSDB")

(defparameter %clk-tck% 100)

#+:allegro
(ff:def-c-type struct_tms :struct 
  (tms_utime :long)
  (tms_stime :long)
  (tms_cutime :long)
  (tms_cstime :long))

#+:allegro
(ff:defforeign 'times :return-type :integer)

#+:allegro
(eval-when (:load-toplevel :compile-toplevel :execute)
  (let ((start (make-struct_tms))
        (end (make-struct_tms)))
    (defun tsdb-timer (&optional action)
      (cond
       ((member action (list :start))
        (times start))
       ((member action (list :stop nil))
        (times end)
        (pairlis (list :cpu :system)
                 (list (/ (- (struct_tms-tms_utime end) 
                             (struct_tms-tms_utime start))
                          %clk-tck%)
                       (/ (- (struct_tms-tms_stime end) 
                             (struct_tms-tms_stime start))
                          %clk-tck%))))))))


