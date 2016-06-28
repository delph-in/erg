(in-package :mrs)

;;;
;;; these are general MRS variables, irrespective of the AVM encoding used in
;;; a particular grammar.
;;; 

(setf *rel-name-path* `(,(vsym "PRED") ))

(setf *rel-handel-path* (list (vsym "LBL")))

(setf *sc-arg-feature* (vsym "HARG"))

(setf *outscpd-feature* (vsym "LARG"))

(setf *bv-feature* (vsym "ARG0"))

(setf *scope-feat* (vsym "BODY"))

(setf *top-semantics-type* (vsym "predsort"))

(setf *value-feats* (list (vsym "CARG")))

;;;
;;; the following are grammar-specific paths
;;;

(setf *sem-relation-suffix* "_rel")

(setf *initial-semantics-path* nil)

(setf *main-semantics-path* 
  (append *initial-semantics-path* (list (vsym "RELS"))))

(setf *construction-semantics-path* nil)

(setf *psoa-top-h-path* (list (vsym "TOP")))

(setf *psoa-index-path* (list (vsym "INDEX")))

(setf *psoa-liszt-path* (list (vsym "RELS")))

(setf *psoa-rh-cons-path* (list (vsym "HCONS")))

;;;
;;; supress old-style use of determine-variable-type(), as it does not know
;;; about the transfer-specific `anti-'type.  requesting VPM-based type mapping
;;; without an actual (named) VPM will result in MRS variable types being the
;;; same as the TFS-level names.
;;;
(setf *variable-type-mapping* t)

;;;
;;; for better debugging, print MRS structures readably, by default
;;;
#-:debug
(setf mrs::*mrs-raw-output-p* nil)
