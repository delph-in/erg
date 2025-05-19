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
 (:block "mod")
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
 )

#+:null
(progn
  (in-package "MT")
  (mt:read-vpm "~/logon/lingo/terg/etc/semi-output.vpm" :semi)
  (setf semi
    (construct-semi       
     :ids t :rules t :descendp t :embedp t
     :semi (read-semi
            "~/logon/lingo/terg/etc/erg.smi"
            :includep nil :finalizep nil :recordp nil)
     :patches "~/logon/lingo/terg/etc/patches.lisp"
     :finalizep t))
  (print-semi
   semi :stream "~/logon/lingo/terg/etc/hierarchy.smi"
   :format :hierarchy)
  (print-semi
   semi :stream "~/logon/lingo/terg/etc/abstract.smi"
   :format :compact :filter "^[^_]")
  (print-semi
   semi :stream "~/logon/lingo/terg/etc/surface.smi"
   :format :compact :filter "^_")
  (mt:read-vpm "~/logon/lingo/terg/semi.vpm" :semi)
  )
