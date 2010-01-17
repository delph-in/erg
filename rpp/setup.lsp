(in-package :lkb)

(clear-repp)

;;;
;;; as of September 2008, REPP supports `ensembles' of rule sets, where select
;;; modules (XML or LaTeX markup normalization, for example) can be activated
;;; in the REPP environment or top-level repp() call.  by default, turn on the
;;; XML and ASCII modules.
;;;
(read-repp (lkb-pathname (parent-directory "rpp") "xml.rpp"))
(read-repp (lkb-pathname (parent-directory "rpp") "latex.rpp"))
(read-repp (lkb-pathname (parent-directory "rpp") "ascii.rpp"))
(read-repp (lkb-pathname (parent-directory "rpp") "wiki.rpp"))
(read-repp (lkb-pathname (parent-directory "rpp") "bioscope.rpp"))
(read-repp (lkb-pathname (parent-directory "rpp") "erg.rpp"))
(read-repp (lkb-pathname (parent-directory "rpp") "tokenizer.rpp"))
(setf *repp-calls* '(:xml :ascii))
(setf *repp-characterize-p* t)
(setf *repp-interactive* '(:tokenizer :xml :ascii :erg))
(setf *repp-debug-p* nil)
