;;  tuneup.lisp
;;
;;    Parser tuning for CSLI Verbmobil grammar
;;
;;    Created: Rob Malouf, 22-Aug-1994
;;    Last revised: Dan Flickinger, 24-Feb-97
;;
;;    $Id$

(in-package :lexicon)

(excl:compile-file-if-needed 
 (dir-and-name tdl::*patches-dir* "tuneup-patches"))
(load (dir-and-name tdl::*patches-dir* "tuneup-patches"))

;;; *** Still need to get the right restrictor from DFKI

(setf *result-rest*
  '( T .    ;;; this is an inverse restrictor: delete the specified paths
    ((disco::args)
     (disco::head-dtr)
     (disco::lconj-dtr)
     (disco::rconj-dtr)
     (disco::non-head-dtr))))

(unless lexicon::*rf-filter*
  (rule-filter :print nil)

;; Prevent the other adjunct rules from feeding extradj
;; Prevent adjh from feeding hadj, so first pick up post-head adjuncts,
;;   attaching them lower than pre-head adjuncts.  This assumes that scope will
;;   be handled by semantic means, not read directly off the syntactic tree.

  (lex::augment-filter 
   '((extradj (:forbid hadj))
     (extradj (:forbid adjh))
     (hadj (:forbid adjh)))))

(main::get-grammar-from-tdl)

