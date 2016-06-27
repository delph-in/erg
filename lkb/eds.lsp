(in-package :mrs)

;;;
;;; EDS configuration options (these used to be default values in the EDS code,
;;; but better make the ERG settings explicit with each release of the grammar).
;;;
(setf *eds-show-properties-p* t)
(setf *eds-show-status-p* nil)
(setf *eds-include-quantifiers-p* t)
(setf *eds-quantifier-argument* (vsym "BV"))
(setf *eds-message-relation* nil)
(setf *eds-bleached-relations*
  (if *normalize-predicates-p*
    (list (vsym "selected"))
    (list (vsym "selected_rel"))))
(setf *eds-non-representatives*
  (if *normalize-predicates-p*
    (list (vsym "appos") (vsym "id") (vsym "focus_d") (vsym "parg_d"))
    (list (vsym "appos_rel") (vsym "id_rel")
          (vsym "focus_d_rel") (vsym "parg_d_rel"))))
(setf *eds-predicate-modifiers*
  (if *normalize-predicates-p*
    (list (ppcre:create-scanner "_x_deg$|^neg$|^_quite_x$"))
    (list (ppcre:create-scanner "_x_deg_rel$"))))
(setf *eds-untensed* (list (cons (vsym "TENSE") (vsym "untensed"))))
