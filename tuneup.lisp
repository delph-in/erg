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

;; 1. Prevent the other adjunct rules from feeding extradj
;; 2. Prevent adjh from feeding hadj, so first pick up post-head adjuncts,
;;    attaching them lower than pre-head adjuncts.  This assumes that scope will
;;    be handled by semantic means, not read directly off the syntactic tree.
;; 3. The grammar currently does not block extracted complements in imperatives,
;;    since we want to treat examples like "on Tuesday, schedule a meeting" as
;;    adjunct extraction.  So we block at least simple complement extraction  
;;    in imperatives by preventing extracomp from feeding imper.
;; 4. Prevent the thatless-relative rule from feeding extracted-adj rule
;;    (Subjacency)
;; 5. Prevent imperative from feeding extracted-adj - any extraction should come
;;    from within the VP.  Same for imperative and head-adj.
;; 6. Prevent extracomp from feeding itself (the constraints in the rule are not
;;    yet adequate to do so.

  (lex::augment-filter 
   '((extradj (:forbid hadj))
     (extradj (:forbid adjh))
     (extradj (:forbid fin_non_wh_rel))
     (extradj (:forbid imper))
     (hadj (:forbid adjh))
     (hadj (:forbid imper))
     (imper (:forbid extracomp))
     (extracomp (:forbid extracomp))
     )))


(main::get-grammar-from-tdl)

(setf main::*source-grammar* "~/grammar/")
