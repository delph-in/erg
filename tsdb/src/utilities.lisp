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

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (unless (find-package "CSLI-UNIFY")
    (make-package "CSLI-UNIFY" :use (list "COMMON-LISP" "MAKE"))))
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (unless (find-class 'csli-unify::fs nil)
    (defstruct csli-unify::fs)))

(defmethod get-fs-type ((fs unify::node))
  (tdl::value-type (unify::get-type fs)))
(defmethod get-fs-type ((fs csli-unify::fs))
  (csli-unify::fs-type fs))

(defun get-informative-item-label (item)
  (when (pg::combo-item-p item)
    (case (pg::combo-item-itype item)
      ((:lex-entry :c-lex-entry)
       (let* ((spare (pg::combo-item-spare item))
              (spare (when spare (string spare)))
              (cfs (pg::combo-item-cfs item))
              (fs (and cfs (pg::cfs-fs cfs))))
         (and fs (string-downcase 
                  (or spare (format nil "~a" (get-fs-type fs)))))))
      (t (string-downcase (format nil "~a" (pg::item-label item)))))))
