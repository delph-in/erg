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
(setf *eds-bleached-relations* nil)
(setf *eds-non-representatives*
  (list (vsym "appos") (vsym "id") (vsym "focus_d") (vsym "parg_d")))
(setf *eds-predicate-filter* 
  "^focus_d$|^parg_d$")
(setf *eds-argument-filter*
  (list
   (list (vsym "L-INDEX") (vsym "L-HNDL") "^h")
   (list (vsym "R-INDEX") (vsym "R-HNDL") "^h")))
(setf *eds-predicate-modifiers*
  (list (ppcre:create-scanner "_x_deg$|^neg$|^_quite_x$")))
(setf *eds-untensed* (list (cons (vsym "TENSE") (vsym "untensed"))))
