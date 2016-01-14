(in-package :mt)

(setf *transfer-filter-p* nil)

#+:logon
(setf *semi-fragment-relations*
  (list "fragment_rel" (mrs::vsym "unspec_conj_rel")))

;;;
;;; for the time being, predicate synopses in the auto-generated ERG SEM-I can
;;; be incomplete; for example, most conjunctions only have L-INDEX and R-INDEX
;;; (and lack L-HNDL and R-HNDL, because their lexical type in 1214 at least is
;;; just ‘basic_conj_relation’).  to still get something useful out of testing
;;; for SEM-I compliance, suppress the :roles and :arity tests.  (7-jan-16; oe)
;;;
(setf mt::*semi-test* '(:predicates :properties))
