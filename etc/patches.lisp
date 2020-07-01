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

; Original, for 1214 release
#+:null
(progn
  (in-package :mt)
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

#+:null
(progn
  (setf semi
    (construct-semi       
     :ids t :rules t :descendp t :embedp t
     :semi (read-semi
            "~/erg/etc/erg.smi"
            :includep nil :finalizep nil :recordp nil)
     :patches "~/erg/etc/patches.lisp"
     :finalizep t))
  (print-semi
   semi :stream "~/erg/etc/hierarchy.smi"
   :format :hierarchy)
  (print-semi
   semi :stream "~/erg/etc/abstract.smi"
   :format :compact :filter "^[^_]")
  (print-semi
   semi :stream "~/erg/etc/surface.smi"
   :format :compact :filter "^_"))

;; For trunk
#+:null
(setf mrs::*normalize-predicates-p* t)
#+:null
(progn
  (in-package :mt)
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
   :format :compact :filter "^_"))
