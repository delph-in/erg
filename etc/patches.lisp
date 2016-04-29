;;;
;;; we must be careful not to rename any predicates that are used in grammar
;;; entities (lexical entries or rules), because these provide the link from
;;; the pieces of an input semantics to realization to the intial generator
;;; chart.
;;; 
;;; + alias predicate type
;;; + block predicate
;;; + link type type
;;; + parent predicate predicate
;;;
((:block "be_v_prd-or-id")
 (:block "be_v_prd-or-nv")
 (:block "card")
 (:block "comp")
 (:block "dofm")
 (:block "every_q")
 (:block "interval_p_end")
 (:block "numbered_hour")
 (:block "person_name")
 (:block "plus")
 (:block "some_q")
 (:block "temp")
 (:block "time_n")
 (:block "times")
 (:block "v_event")
 (:alias "existential_q" def_udef_some_a_q_rel)
 (:link def_or_demon_q_rel def_udef_some_a_q_rel)
 (:link def_or_proper_q_rel def_udef_some_a_q_rel)
 (:link basic_free_relative_q_rel def_udef_some_a_q_rel)
 (:parent "_all_q" "universal_q")
 (:parent "_each_q" "universal_q")
 (:parent "every_q" "universal_q")
 (:parent "_both_q" "universal_q")
 (:parent "_either_q" "universal_q")
 (:parent "_across_p_temp" "_across_p")
 (:parent "_at_p_temp" "_at_p")
 (:parent "_by_p_temp" "_by_p")
 (:parent "_in_p_temp" "_in_p")
 (:parent "_on_p_temp" "_on_p"))

#+:null
(progn
  (setf semi
    (construct-semi       
     :ids t :rules t :descendp t :embedp t
     :semi (read-semi
            "~/src/logon/lingo/erg/etc/erg.smi"
            :includep nil :finalizep nil :recordp nil)
     :patches "~/src/logon/lingo/erg/etc/patches.lisp"
     :finalizep t))
  (print-semi
   semi :stream "~/src/logon/lingo/erg/etc/hierarchy.smi"
   :format :hierarchy)
  (print-semi
   semi :stream "~/src/logon/lingo/erg/etc/abstract.smi"
   :format :compact :filter "^[^_]")
  (print-semi
   semi :stream "~/src/logon/lingo/erg/etc/surface.smi"
   :format :compact :filter "^_"))


