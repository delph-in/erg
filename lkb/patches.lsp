;;;
;;; a couple of _temporary_ patches to LKB system code for better generation
;;;                                                           (9-dec-03; oe)
;;;

(in-package :mrs)

;;; 1. The generator munging rules now make use of an early version of a
;;; grammar-external rels hierarchy supplied by Ann last week, in order to
;;; identify the class of predicates which require expletive "it".  This new
;;; functionality is called in a changed version of
;;; compatible-types-or-values(), as below:

(defun compatible-types-or-values (val1 val2)
  ;;
  ;; in the current untyped universe, it seems legitimate to not have values
  ;; for PRED or any of the roles: while this should not happen for the input
  ;; MRS, allow null() values in the munging rule to be considered compatible.
  ;;                                                          (3-nov-03; oe)
  (unless (or (eq val1 (vsym "NEVER_UNIFY_REL"))
              (eq val2 (vsym "NEVER_UNIFY_REL")))
    (or (is-top-type val1) (is-top-type val2)
        (null val1)
        (and (is-valid-type val1) (is-valid-type val2) 
	     (compatible-types val1 val2))
        (and (is-munge-type val1) (is-munge-type val2)
             (compatible-munge-types val1 val2))
        (cond ((and (symbolp val1) (symbolp val2))
               (same-names val1 val2))
              ((and (stringp val1) (stringp val2))
               (equal val1 val2))
              ((and (stringp val1) (symbolp val2))
               (equal val1 (symbol-name val2)))
              ((and (stringp val2) (symbolp val1))
               (equal val2 (symbol-name val1)))
              (t (equal val1 val2))))))

(defparameter *munge-expl-preds* nil)

(defun compatible-munge-types (val1 val2)
  (or (and (string-equal val1 "test-expl-type")
           (member (string-downcase val2) *munge-expl-preds*
                   :test #'string-equal))
      (and (string-equal val2 "test-expl-type")
           (member (string-downcase val1) *munge-expl-preds*
                   :test #'string-equal))))

(defun is-munge-type (val)
  (declare (ignore val))
  t)

#|
(in-package :lkb)

(defun idiom-rel-p (rel)
  ;;; cheat for now
  (let* ((relpred (mrs::rel-pred rel))
         (relname (when relpred (string relpred))))
    (and relname
         (equal "_i_rel" (subseq relname (- (length relname) 6))))))

; Changed 
;   (unless (zerop nanalyses) nanalyses)
; to 
;   (or (zerop nanalyses) nanalyses)
; since the former undesirably returns NIL if nanalyses is indeed 0, which
; results in *gen-first-only-p* being set to NIL.

(in-package :lkb)
(defun tsdb::generate-item (mrs
                      &key id string exhaustive nanalyses trace
                           edges derivations semantix-hook trees-hook
                           burst (nresults 0))
  (declare (ignore derivations string id trees-hook)
           (special tsdb::*process-scope-generator-input-p*))

  (let* ((*package* *lkb-package*)
         (*maximum-number-of-edges* (if (or (null edges) (zerop edges))
                                      *maximum-number-of-edges*
                                      edges))
         (*gen-first-only-p* (if exhaustive
                               nil
                               (if (integerp nanalyses)
                                 (or (zerop nanalyses) nanalyses)
                                 (if (integerp *gen-first-only-p*) 
                                   *gen-first-only-p*
                                   1))))
         (*do-something-with-parse* nil)
         (stream (make-string-output-stream))
         (*standard-output* 
          (if trace (make-broadcast-stream *standard-output* stream) stream))
         (*unifications* 0)
         (*copies* 0)
         (*subsumptions* 0)
         tgc tcpu treal conses symbols others)

    (multiple-value-bind (return condition)
        (ignore-errors
         (when (or (null mrs) (not (mrs::psoa-p mrs)))
           (error "null or malformed input MRS"))
         (unless (or (null tsdb::*process-scope-generator-input-p*)
                     (mrs::make-scoped-mrs mrs))
           (error "input MRS does not scope"))
         (multiple-value-bind (strings f-tasks e-tasks s-tasks
                               unifications copies aedges pedges)
             (tsdb::time-a-funcall
              #'(lambda ()
                  (let (#+:pooling (*dag-recycling-p* (null trace)))
                    (generate-from-mrs mrs :signal t)))
              #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                  (declare (ignore ignore))
                  (setf tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr
                        conses (* scons 8) symbols (* ssym 24) 
                        others sother)))

           (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                  (output (get-output-stream-string stream))
                  (readings (length strings))
                  (readings (if (or (equal output "") (> readings 0))
                              readings
                              -1))
                  #+:pooling
                  (pool (and (find-symbol "*DAG-POOL*")
                             (boundp (find-symbol "*DAG-POOL*"))
                             (symbol-value (find-symbol "*DAG-POOL*"))))
                  #+:pooling
                  (position (when pool
                              (funcall 
                               (symbol-function (find-symbol "POOL-POSITION"))
                               pool)))
                  #+:pooling
                  (garbage (when pool
                             (funcall 
                              (symbol-function (find-symbol "POOL-GARBAGE"))
                              pool)))
                  (comment (format nil "~{~(~s~)~^ ~}" %generator-statistics%))
                  (comment 
                   (if *gen-packing-p*
                    (format
                     nil
                     "~a (:subsumptions . ~d) (:equivalence . ~d) ~
                      (:proactive . ~d) (:retroactive . ~d)"
                     comment 
                     *subsumptions* (packings-equivalent *packings*)
                     (packings-proactive *packings*) 
                     (packings-retroactive *packings*))
                    comment))
                  #+:pooling
                  (comment
                   (format 
                    nil 
                    "~a (:pool . ~d) (:garbage . ~d)" 
                    comment position garbage)))
             `((:others . ,others) (:symbols . ,symbols) (:conses . ,conses)
                (:treal . ,treal) (:tcpu . ,tcpu) (:tgc . ,tgc)
                (:pedges . ,pedges) (:aedges . ,aedges)
                (:p-stasks . ,s-tasks) (:p-etasks . ,e-tasks)
                (:p-ftasks . ,f-tasks)
                (:unifications . ,unifications) (:copies . ,copies)
                (:readings . ,readings)
                (:error . ,(pprint-error output))
                (:comment . ,comment)
                (:results .
                 ,(loop
                      with *package* = *lkb-package*
                      with nresults = (if (<= nresults 0)
                                        (length *gen-record*)
                                        nresults)
                      for i from 0
                      for parse in *gen-record*
                      for string in strings
                      for derivation = (with-standard-io-syntax
                                         (let ((*package* *lkb-package*))
                                           (write-to-string
                                            (compute-derivation-tree parse) 
                                            :case :downcase)))
                      for size = (parse-tsdb-count-nodes parse)
                      for tree = #-:null (format nil "~{~a~^ ~}" string)
                                 #+:null (tsdb::call-hook trees-hook parse)
                      for mrs = (tsdb::call-hook semantix-hook parse)
                      while (>= (decf nresults) 0) collect
                        (pairlis '(:result-id :mrs :tree :string
                                   :derivation :size)
                                 (list i mrs tree string
                                       derivation size))))))))
      (unless trace 
        (release-temporary-storage :task :generate))
    
      (append
       (when condition
         (let* ((error (tsdb::normalize-string 
                        (format nil "~a" condition)))
                (error (pprint-error error)))
           (pairlis '(:readings :condition :error)
                    (list -1 (unless burst condition) error))))
       return))))

(in-package :cl-user)
|#

;;;
;;; since, for fragments at least, we use `geq' handle constraints, need to at
;;; least enable construction of MRS objects; presumably, there is no way for
;;; these to scope without work on the algorithm, but alas.     (5-jun-04; oe)
;;;

(defun create-hcons-relation (type)
  (cond ((eql type *qeq-type*) "qeq")
        ((eql type (vsym "geq")) "geq")
        (t (error "Unknown relation type ~A"))))

(in-package :lkb)

(defun idiom-rel-p (rel)
  ;;; cheat for now
  (let* ((relpred (mrs::rel-pred rel))
         (relname (when relpred (string relpred))))
    (and relname
         (equal "_i_rel" (subseq relname (- (length relname) 6))))))


; From lkb/src/mrs/spell.lisp:
; Commented out the a/an spelling fix, since the grammar is now enforcing
; this directly, with each lexical entry identifying its phonological
; onset as either consonantal or vocalic.

(defun fix-spelling (string)
  (setf string (mapcar #'string-downcase string))
  (let ((res nil))
    (loop
      (when (null string) (return))
      (let ((word (car string))
            ;(rest (cdr string))
            )
        (setf string (cdr string))
        ;(if (equal word "a")
        ;    (if (vowel-first-p (car rest))
        ;        (push "an" res)
        ;      (push "a" res))
          (push word res)
        ;  )
      ))
    (nreverse res)))

(in-package :cl-user)
