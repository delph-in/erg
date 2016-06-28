(in-package :mt)


(setf *transfer-edge-limit* 500)

;;
;; limit post-transfer SEM-I comparison to checking the validity of predicate
;; names and variable properties (but not role values and EP arity).
;;
(setf *semi-test* '(:predicates :properties))
