;;  tuneup.lisp
;;
;;    Parser tuning for CSLI Verbmobil grammar
;;
;;    Created: Rob Malouf, 22-Aug-1994
;;    Last revised: Dan Flickinger, 24-Feb-97

(in-package "LEXICON")

(if main::*pagelite*
  (setf *result-rest*
    '( T .    ;;; this is an inverse restrictor: delete the specified paths
      (
       ((:path (disco::args))
	(:path (disco::head-dtr))
	(:path (disco::lconj-dtr))
	(:path (disco::rconj-dtr))
	(:path (disco::non-head-dtr))
	)
       NIL
       NIL)))
  (setf *result-rest*
    '( T .    ;;; this is an inverse restrictor: delete the specified paths
      (
       ((:path (disco::head-dtr))
	(:path (disco::lconj-dtr))
	(:path (disco::rconj-dtr))
	(:path (disco::non-head-dtr))
	)
       NIL
       NIL))))

(unless main::*pagelite*
  (load (concatenate 'string disco::*source-grammar*
		     "cslipage/patches/tuneup-patches.ofasl")))

(unless lexicon::*rf-filter*
  (rule-filter :print nil)

; Prevent the other adjunct rules from feeding extradj
; Prevent adjh from feeding hadj, so first pick up post-head adjuncts,
;   attaching them lower than pre-head adjuncts.  This assumes that scope will
;   be handled by semantic means, not read directly off the syntactic tree.

  (lex::augment-filter 
   '((extradj (:forbid hadj))
     (extradj (:forbid adjh))
     (hadj (:forbid adjh))
     ))
  )

(in-package "MAIN")

(get-grammar-from-tdl)

