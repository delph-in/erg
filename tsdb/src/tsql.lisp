;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

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

(defmacro item-i-id (item)
  `(first ,item))

(defmacro item-i-wf (item)
  `(second ,item))

(defmacro item-i-input (item)
  `(third ,item))

(defmacro item-o-ignore (item)
  `(fourth ,item))

(defmacro item-o-wf (item)
  `(fifth ,item))

(defmacro item-o-gc (item)
  `(sixth ,item))

(defmacro item-o-tasks (item)
  `(seventh ,item))

(defmacro item-o-derivation (item)
  `(eighth ,item))

(defmacro get-field (field alist)
  `(rest (assoc ,field ,alist)))

(defun compute-report-string (types)
  (let* ((types (substitute "\"%s\"" :string types))
         (types (substitute "%d" :integer types)))
    (concatenate 'string 
      "("
      (reduce #'(lambda (x y) (concatenate 'string x " " y)) types)
      ")")))

(defun select (attributes types relations condition
               &optional (language *tsdb-data*))
  (let ((attributes (if (listp attributes) attributes (list attributes)))
        (types (if (listp types) types (list types)))
        (relations (if (listp relations) relations (list relations))))
    (if (not (= (length attributes) (length types)))
      (format
       *tsdb-io*
       "~&select(): `attributes' vs. `types' mismatch (~d vs. ~d).~%"
       (length attributes) (length types))
      (let* ((keys (map 
                     'list 
                     #'(lambda (foo)
                         (intern (string-upcase foo) :keyword))
                     attributes))
             (attributes (format nil "~{~a ~}" attributes))
             (relations (when relations (format nil "~{~a ~}" relations)))
             (report (compute-report-string types))
             (query 
              (format 
               nil
               "select ~a~@[from ~a~]~@[where ~a ~]report ~s"
               attributes relations condition report))
             (result (call-tsdb query language))
             data)
        (with-input-from-string (stream result)
          (do ((line (read stream nil) (read stream nil)))
              ((null line))
            (push (pairlis keys line) data)))
        data))))

;;;
;;; functions borrowed from introductory Common-Lisp course by \me; i always
;;; used to say this code can be made useful.  (22-may-97  -  oe@coli)
;;;

;;;
;;; iterative select() function: loop through .relation., extract current value
;;; for .field., funcall() .test., accumulate .result. destructively (push()).
;;;
(defun iselect (relation field value &key (test #'equal))
  (do* ((relation relation (rest relation))
        (record (first relation) (first relation))
        (result nil result))
      ((null relation) result)
    (let ((comparison (get-field field record)))
      (when (and comparison (funcall test comparison value))
        (push record result)))))

;;;
;;; lazy combine(): fails to check for value compatibility in common fields;
;;; compact coding, though :-).
;;;
(defun lcombine (record-1 record-2)
  (union record-1 record-2 :key #'first))

;;;
;;; iterative join() function: traverse .relation-1., extract .value. for
;;; .key., select() correspoding (equal .key. value) records from
;;; .relation-2., compute .record. x .records. cross product, accumulate
;;; .result. destructively; not the most efficient algorithm, though.
;;;
(defun join (relation-1 relation-2 key)
  (do* ((relation-1 relation-1 (rest relation-1))
        (record (first relation-1) (first relation-1))
        (value (get-field key record) 
               (get-field key record))
        (records (iselect relation-2 key value)
                 (iselect relation-2 key value))
        (result nil result))
      ((null relation-1) result)
    (dolist (foo records)
      (push (lcombine record foo) result))))




