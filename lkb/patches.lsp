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

