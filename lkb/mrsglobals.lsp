(in-package "MRS")

(setf *sem-relation-suffix* "_rel")

(setf *value-feats* `(,(vsym "CARG")))

(setf *feat-priority-list*  
  `( ,(vsym "LTOP") ,(vsym "INDEX") ,(vsym "LBL")
     ,(vsym "ARG0") ,(vsym "ARG1") ,(vsym "ARG2") ,(vsym "ARG3") 
     ,(vsym "RSTR") ,(vsym "BODY")
     ,(vsym "MARG") ,(vsym "CARG")))
                                 
(setf *ignored-sem-features* 
  (append 
   *ignored-sem-features* 
   (list (vsym "IDIOMP") (vsym "LNK") (vsym "WLINK") (vsym "PARAMS")
         (vsym "CFROM") (vsym "CTO") (vsym "--PSV"))))

(setf *ignored-extra-features* 
  (append
   *ignored-extra-features*
   (list (vsym "SORT") (vsym "INSTLOC"))))

; (setf *top-level-rel-types*  `(,(vsym "PRON_REL")))
(setf *top-level-rel-types*  nil)

(defparameter *mrs-to-vit* nil)

(defparameter *mrs-for-language* 'english)

(defparameter *mrs-scoping* nil)
(setf *scoping-call-limit* 1000000)

(setf *initial-semantics-path* 
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT") ))

(setf *main-semantics-path* 
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT") 
    ,(vsym "RELS") ,(vsym "LIST")))

(setf *construction-semantics-path*
  `(,(vsym "C-CONT") ,(vsym "RELS") ,(vsym "LIST")))

(setf *psoa-top-h-path* nil
  ;`(,(vsym "HOOK") ,(vsym "LTOP"))
  )

(defparameter *psoa-index-path* 
  `(,(vsym "HOOK") ,(vsym "INDEX"))
  "path to get an index from a psoa")

(defparameter *psoa-event-path* `(,(vsym "HOOK") ,(vsym "INDEX")))
(defparameter *psoa-liszt-path* `(,(vsym "RELS") ,(vsym "LIST")))
(defparameter *psoa-rh-cons-path* `(,(vsym "HCONS") ,(vsym "LIST")))

(defparameter *rel-handel-path*
    `(,(vsym "LBL"))
  "path to get the handel from a relation")

(defparameter *sc-arg-feature* (vsym "HARG")
  "the feature in a qeq that leads to the first argument")

(defparameter *outscpd-feature* (vsym "LARG")
  "the feature in a qeq that leads to the second argument")

(defparameter *quant-rel-types* nil)

(defparameter *bv-feature* (vsym "ARG0"))

(defparameter *scope-feat* (vsym "BODY"))

(setf *top-semantics-type* 
  (vsym "predsort"))

(setf *rel-name-path* `(,(vsym "PRED") ))

;;;
;;; as of late 2008, the VPM (aka variable property mapping) machinery also 
;;; defines the correspondence of grammar-internal variable types to variable
;;; types (or sorts, in MRS XML lingo) on actual MRSs, e.g. `event' == `e', et
;;; al.  with that mapping as part of the VPM, the old parameters *event-type*
;;; etc. are no longer needed.  yes, we can!                   (20-jan-09; oe)
;;;
(setf *variable-type-mapping* :semi)

;;;
;;; context condition in MRS munging rules
;;; 
(defparameter *mrs-rule-condition-path* (list (vsym "CONTEXT")))


;;; the following are needed only for the detection of fragments
;;; indicated in the LinGO gramar by the value of ROOT

(setf *root-path* `(,(vsym "ROOT")))

(setf *false-type* (vsym "-"))

(setf *true-type* (vsym "+"))

;;;
;;; disable so-called `CARG injection' on so-called `ersatz' lexical entries.
;;; this mechanism was unprincipled in the first place (firing on everything
;;; whose orthography ended in the literal string "ersatz"), and now that the
;;; ERG has moved into the chart mapping universe (PET only, so far), generic
;;; lexical entries are treated differently anyway.  `ersatz'ing was a remnant
;;; from the YY days, and in this one case at least we are happy to have put 
;;; it behind us :-).                                          (21-jan-09; oe)
;;;
#+:smaf
(setf smaf::*ersatz-carg-path* nil)

;;;
; DPF Upped from 100 to 200
; DPF 1-Jul-03 Upped from 200 to 500 due to treatment of unspec_loc_rel for
; e.g. free relatives.  Maybe should make this generic rel more specific.
(setf *maximum-genindex-relations* 500)

(defparameter *var-extra-conversion-table*
'(
  ((png.gen fem) . (gender f))
  ((png.gen masc) . (gender m))
  ((png.gen andro) . (gender m-or-f))
  ((png.gen neut) . (gender n))

  ((png.pn 1s) . (AND (pers 1) (num sg)))
  ((png.pn 2s) . (AND (pers 1) (num sg)))
  ((png.pn 3s) . (AND (pers 3) (num sg)))
  ((png.pn -1s) . (AND (pers 2-or-3) (num sg)))
  
  ((png.pn 1p) . (AND (pers 1) (num pl)))
  ((png.pn 2p) . (AND (pers 1) (num pl)))
  ((png.pn 3p) . (AND (pers 3) (num pl)))

  ((png.pn 1) .  (pers 1))
  ((png.pn 2) .  (pers 2))
  ((png.pn 3) .  (pers 3))

  ((e.tense basic_tense) . (tense u))
  ((e.tense no_tense) . (tense u))
  ((e.tense nontense) . (tense u))
  ((e.tense future) . (tense future))
  ((e.tense present) . (tense present))
  ((e.tense past) . (tense past))
;;;  ((e.tense nonpresent) . (tense non-present))
  ((e.tense nonpresent) . (tense u))
  ;;; my version of the DTD doesn't have `non-present'
  ;;; replace this with line above if using a DTD that does
  ((e.tense nonpast) . (tense non-past))
  
 ;;; note the interpretation is intended to be that the 
 ;;; first match is taken.  For RMRS->MRS conversion, there's
 ;;; a sl problem in that nontense and no_tense are 
 ;;; both possible values corresponding to (tense u)
 ;;; and that this also corresponds to the `don't know'
 ;;; case.  We therefore need to translate the RMRS `u'
 ;;; into `basic_tense'
))

