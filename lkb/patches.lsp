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

;;; 3. (show-gen-chart) breaks on adjectives since degree specifiers now have
;;; pred string names as values, and create-gen-chart-pointers() in
;;; mrstoplevel.lsp tries to apply symbol-name() to sem-args but gets this
;;; string, unhappily.

(in-package :lkb)

(defun create-gen-chart-pointers (root all-p)
  ;; create a global mapping from edge-ids to symbols, not interned - so we
  ;; don't end up hanging on to old edges
  (let ((edge-symbols nil))
    (dolist (entry *gen-chart*)
      (dolist (e (append (cadr entry) (cddr entry)))
        (push
         (list* (dotted-edge-id e)
                (make-edge-symbol (dotted-edge-id e))
                (dotted-edge-needed e))
         edge-symbols)))
    (dolist (entry *gen-chart*)
      (let ((chart-index (string-downcase 
                          (if (stringp (car entry)) (car entry)
                            (symbol-name (car entry))))))
        (dolist (e (append (cadr entry) (cddr entry)))
          (let ((edge-symbol
                 (cadr (assoc (dotted-edge-id e) edge-symbols))))
            (setf (get edge-symbol 'chart-edge-span)
              (if (dotted-edge-needed e)
                  (concatenate 'string chart-index " A") chart-index))
            (setf (get edge-symbol 'chart-edge-contents) e)
            (if (dotted-edge-children e)
                (dolist (c (dotted-edge-children e))
                  (when c
                    (push edge-symbol
                          (get (cadr (assoc (dotted-edge-id c) edge-symbols))
                               'chart-edge-descendents))))
              (push edge-symbol (get root 'chart-edge-descendents)))))))
    (unless all-p
      ;; remove intermediate links consisting of active edges
      (dolist (pair edge-symbols)
        (setf (get (cadr pair) 'chart-edge-descendents)
          (create-gen-chart-pointers-collapse
           (get (cadr pair) 'chart-edge-descendents)
           edge-symbols))))))
