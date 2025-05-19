(in-package :mt)

(setf *transfer-filter-p* nil)

#+:logon
(setf *semi-fragment-relations*
  (list "fragment_rel" (mrs::vsym "unspec_conj_rel")))
