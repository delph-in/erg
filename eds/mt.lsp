(in-package :mt)


(setf *transfer-edge-limit* 200)

;;
;; limit post-transfer SEM-I comparison to checking the validity of predicate
;; names and variable properties (but not role values and EP arity).
;;
(setf *semi-test* '(:predicates :properties))
