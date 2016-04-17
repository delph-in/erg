;;;
;;; we must be careful not to rename any predicates that are used in grammar
;;; entities (lexical entries or rules), because these provide the link from
;;; the pieces of an input semantics to realization to the intial generator
;;; chart.
;;; 
;;; + block foo --- prevent descend into (grammar-internal) sub-types of .foo.
;;; + alias foo bar --- (expose in the SEMI-I and) rename type .bar. as .foo.
;;; + parent
;;;
((:block "card")
 (:block "comp")
 (:block "dofm")
 (:block "interval_p_end")
 (:block "numbered_hour")
 (:block "plus")
 (:block "temp")
 (:block "times")
 (:block "v_event")
 (:alias "exist_q" def_udef_some_a_q_rel)
 (:parent "_all_q" "universal_q")
 (:parent "_each_q" "universal_q")
 (:parent "every_q" "universal_q")
 (:parent "_both_q" "universal_q")
 (:parent "_either_q" "universal_q"))
 