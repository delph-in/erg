;;; -*- Mode: Common-Lisp; Package: LKB; -*-

;;; Copyright (c) 1991--2018
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see LKB `licence.txt' for conditions.

;;;
;;; LinGO grammar specific globals file
;;; parameters only - grammar specific functions 
;;; should go in user-fns.lsp
;;; patches in lkb-code-patches.lsp


;;; Avoiding multiple inheritance on letypes

(defparameter *active-parsing-p* t)

;;; Strings

(defparameter *toptype* '*top*)

(defparameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; Lexical files

(defparameter *orth-path* '(orth))

(defparameter *list-tail* '(rest))

(defparameter *list-head* '(first))

(defparameter *empty-list-type* '*null*)

(defparameter *list-type* '*list*)

(defparameter *diff-list-type* '*diff-list*)

(defparameter *diff-list-list* 'list)

(defparameter *diff-list-last* 'last)

(defparameter *lex-rule-suffix* ""
 "creates the inflectional rule name from the information
   in irregs.tab - for PAGE compatability")

(defparameter *irregular-forms-only-p* t)

(defparameter *display-type-hierarchy-on-load* nil)

;;; Lexical database

(defparameter *lexdb-dump-source* "LinGO")
(defparameter *lexdb-dump-lang* "EN")
(defparameter *lexdb-dump-country* "US")


;;; Parsing

(defparameter *chart-limit* 100)

(defparameter *maximum-number-of-edges* 4000)

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")

(defparameter *start-symbol* 
  #-(or :speech :educ :essay :arboretum)
  '(root_strict root_frag)
  ;'(root_strict)
  #+:speech
  '(root_informal root_frag root_inffrag)
  ;'(root_informal root_spoken root_spoken_frag)
  ;'(root_informal root_frag root_inffrag root_robust)
  #+(or :educ :essay)
  '(root_decl root_question root_robust_s root_inffrag root_robust_frag 
    root_lex root_robust_ques)
  ;'(root_formal)
  ;'(root_decl)
  #+:arboretum
  '(root_standard root_question root_robust_s root_lex root_frag root_inffrag)
   "specifing valid parses")

;; Default for generation (enabled for LKB-FOS)
(defparameter *gen-start-symbol* 
  '(root_gen))

;;;
;;; _fix_me_
;;; now that the old `root_lex' et al. are no longer available, we may need to
;;; do something in addition for LOGON fragment generation.
;;;                                                      (dan & oe; 20-apr-05)
(setf *fragment-start-symbols*
  '(root_strict root_informal root_frag 
    root_lex root_phr root_conj root_subord))

;;;
;;; two settings that (somewhat redundantly) support picking the head daughter
;;; in constructions.
;;;
(setf *head-path* '(synsem local cat head))
(setf *head-daughter-path* '(hd-dtr))

;;;
;;; as we move into the chart mapping universe, lexical entries behave similar
;;; to rules: the list of input tokens that license a lexical entry are unified
;;; into *lexicon-tokens-path* (when set).  furthermore, to give the grammarian
;;; easier access to the token in the right periphery, the last element of the 
;;; tokens list is made re-entrant with *lexicon-last-token-path*.
;;;
(setf *lexicon-tokens-path* '(TOKENS +LIST))
(setf *lexicon-last-token-path* '(TOKENS +LAST))
(setf *token-id-path* '(+ID))

;;;
;;; _fix_me_
;;; since sometime in 2010, there was an unused +STAG feature in token feature
;;; structures, in anticipation of supertagger integration.  following our
;;; first international ubertagging symposium (oslo, november 2011), we now
;;; rather plan on generalizing +TNT (as +POS, maybe) to possibly hold more
;;; than one view on lexical category assignments.  until the near-final phase
;;; in token mapping, it is undesirable (tedious, in fact) to distinguish the
;;; various types of category suggestions (be they sub-, PoS, or super-tags).
;;; furthermore, this way we can even stick to the tried and tested YY input
;;; format (unless the supertagger is directly interfaced to PET, which be the
;;; preferred form of integration nowadays).  while there may be profiles with
;;; old-style derivations around, make the [incr tsdb()] dag reader ignore the
;;; feature +STAG globally.  sometime in 2012, we should probably purge this
;;; setting.                                                  (26-nov-11; oe)
;;; 
(setf *token-ignore* '(+STAG))

;;; Set to true for arboretum, enabling parsing with robust rules and lexicon
;;; (this assumes that :arboretum was pushed onto *features* before compiling
;;; the LKB and loading the grammar).  Then (after indexing lexicon for 
;;; generator) test by calling e.g. (lkb::grammar-check "dog barks").  
;;; Remember to touch letypes.tdl before loading ERG to flush the lexicon 
;;; cache, ensuring that mal-letypes.tdl gets loaded.
;
#+:arboretum
(defparameter *mal-active-p* t)

(defparameter *maximal-lex-rule-applications* 7
   "The number of lexical rule applications which may be made
   before it is assumed that some rules are applying circularly")

(defparameter *deleted-daughter-features* 
  '(ARGS HD-DTR NH-DTR LCONJ-DTR RCONJ-DTR DTR DTR1 DTR2 DTRA DTRB)
  "features pointing to daughters deleted on building a constituent")

;;;
;;; to enable local ambiguity packing
;;;

#+:null
(defparameter *chart-packing-p* t)

(defparameter *packing-restrictor*
  ;'(ORTH RELS HCONS RNAME RPUNCT)
  ;'(ORTH RELS HCONS RNAME PSF)
  '(RELS HCONS ICONS RNAME +TI +LL +TG)
  "restrictor used when parsing with ambiguity packing")

;;;
;;; increase dag pool size
;;;
(defparameter *dag-pool-size* 200000)
(defparameter *dag-pool*
  (if (and (pool-p *dag-pool*) 
           (not (= (pool-size *dag-pool*) *dag-pool-size*)))
    (create-pool *dag-pool-size* #'(lambda () (make-safe-dag-x nil nil)))
    *dag-pool*))

;;; Parse tree node labels

;;; the path where the name string is stored
(defparameter *label-path* '(LNAME))

;;; the path for the meta prefix symbol
(defparameter *prefix-path* '(META-PREFIX))

;;; the path for the meta suffix symbol
(defparameter *suffix-path* '(META-SUFFIX))

;;; the path for the recursive category
(defparameter *recursive-path* '(SYNSEM NONLOC SLASH LIST FIRST))

;;; the path inside the node to be unified with the recursive node
(defparameter *local-path* '(SYNSEM LOCAL))

;;; the path inside the node to be unified with the label node
(defparameter *label-fs-path* '())

(defparameter *label-template-type* 'label)

;;; the default sentence
(defparameter *last-parses*
  (let ((symbol (find-symbol "*LAST-PARSES*" :lkb)))
    (if (and (boundp symbol) (rest (symbol-value symbol)))
      (symbol-value symbol)
      '("Abrams hired two competent programmers."))))

; For character encoding
(defparameter cdb::*cdb-ascii-p* nil)

; Turn on characterization in preprocessor
(setf *characterize-p* t)

(defparameter *discriminant-path* '(SYNSEM LOCAL MINORS MIN))

;;
;; Read in settings for globals including *gen-ignore-rules* and the ever-young
;; temporary expedient *duplicate-lex-ids*.
;;
(defun load-erg-settings-file (file)
  (with-open-file (stream file :direction :input)
    (loop for item = (read-line stream nil nil)
	while item
	unless (or (zerop (length item)) (eq (elt item 0) #\;))
	collect (read-from-string item))))

(setf *duplicate-lex-ids*
  (load-erg-settings-file 
    (merge-pathnames "lkb/nogen-lex.set" *grammar-directory*)))

(setf *gen-ignore-rules* 
  (load-erg-settings-file 
    (merge-pathnames "lkb/nogen-rules.set" *grammar-directory*)))

(setf *parse-ignore-rules* 
  (load-erg-settings-file 
    (merge-pathnames "lkb/noparse-rules.set" *grammar-directory*)))

#+:openproof
(setf *gen-ignore-rules* 
  (load-erg-settings-file 
    (merge-pathnames "openproof/nogen-rules.set" *grammar-directory*)))

#+:educ
(setf *gen-ignore-rules* 
  (load-erg-settings-file 
    (merge-pathnames "educ/nogen-rules.set" *grammar-directory*)))


(setf *semantics-index-path* '(SYNSEM LOCAL CONT HOOK INDEX))

;;;
;;; turn on packing in the generator, index accessibility filtering, and the
;;; treatment of QEQs as equations while in the generator; the latter requires
;;; that INSTLOC values in QEQs be re-entrant for it to work as intended.
;;;                                                            (14-jul-04; oe)
(setf *gen-packing-p* t)
(setf *gen-filtering-p* t)
(setf *gen-equate-qeqs-p* t)

; DPF 27-Nov-03 - Finally noticed that on the current clever approach to
; intersective modification, we can't generate np-adverbial modifiers as
; in "Kim arrived the day after Sandy" since "*Kim arrived the day" is not
; well-formed, which means we don't generate that 'skeleton' into which we
; could then insert the PP-modifier.  Given that the presence of intersective
; modifiers is apparently sometimes syntactically required, this two-stage
; approach to generation may be threatened.  For now, commenting out the
; relevant noun-modifying rules.  Also hd-aj_int-unsl_c, since only verb-participles
; which are post-modified can be reduced relatives: "women working for Browne"

;(setf *intersective-rule-names* '(adjn_i aj-hd_int_c hdn-aj_rc_c
;                                  nadj_rr_nt nadj_rr_t hd-aj_int-unsl_c))
;(setf *intersective-rule-names* '(aj-hd_int_c nadj_rr_nt))

;;;
;;; as of mid-december 2003, the generator allows specification of the non-foot
;;; daughters in adjunction rules; make this conditional on LKB source version,
;;; so the grammar still loads into older LKBs.                (18-dec-03; oe)
;;;
;;; index accessibility filtering is incompatible with two-phase generation,
;;; which should not be needed anymore, anyway.                (14-jul-04; oe)
;;;                          
(setf *intersective-rule-names* nil
  #+:null
  '((aj-hd_int_c . (1)) (nadj_rr_nt . (2))))

(defparameter *chart-dependencies*
  '((SYNSEM LKEYS --+COMPKEY) (SYNSEM LOCAL CAT HEAD MINORS MIN)
    (SYNSEM LKEYS --+OCOMPKEY) (SYNSEM LOCAL CAT HEAD MINORS MIN)
    (SYNSEM LKEYS --COMPHD) (SYNSEM LOCAL CAT HEAD)
    (SYNSEM LKEYS --+ARGIND) (SYNSEM --SIND)))

;;; AAC - Dec 2003
;;; *unknown-word-types*
;;; deliberately commented out, but code in user-fns
;;; depends on this just being proper names, since it sets CARG
;;; to the word string (downcased)
;;; (defparameter *unknown-word-types* '(n_proper_le))

;;;
;;; a new facility as of April 2005: initially for use in the generator only, 
;;; provide a set of generic lexical entries (i.e. actual instances) that get
;;; specialized according to their `surface' form, i.e. the value for ORTH and
;;; CARG (or the equivalent in non-ERG grammars); specialization is triggered
;;; by unknown (singleton) relations in the generator input that actually have
;;; a CARG.  a new, temporary lexical entry is created and has the CARG value 
;;; destructively inserted (using instantiate-generic-lexical-entry(), which a
;;; grammar has to supply among its user functions :-{).         (7-apr-05; oe)
;;;
;;; in response to encouragement from a japanese colleague, we now support the
;;; use of generics without a CARG in generation, e.g. underspecified nouns or
;;; verbs.  for an input like "_baz_v_1_rel", these entries are activated on
;;; the basis of a `trigger' predicate (match-pred()), which is given a full EP
;;; for inspection (in principle at least, the predicate should make sure the
;;; role set of the EP is compatible with the generic lexical entry, rejecting
;;; for example relational nouns).  when successful, the predicate returns the
;;; string to be used as the stem for the newly created lexical entry (for our
;;; example input above, that would be |baz|).                   (2-jun-09; oe)
;;;
(labels ((match-pred (ep tag)
           (let ((pred (string (mrs:rel-pred ep)))
                 (re (format nil "^_([^_]+)_~a(?:_[^_]+)?_rel$" tag)))
             (multiple-value-bind (start end starts ends)
                 (ppcre:scan re pred)
               (declare (ignore start end))
               (when (and starts ends)
                 (subseq pred (aref starts 0) (aref ends 0)))))))
  (setf *generic-lexical-entries*
    `((generic_proper_ne :generate)
      (generic_card_ne :generate) (generic_ord_ne :generate)
      (generic_dom_card_ne :generate) (generic_dom_ord_ne :generate)
      (generic_year_ne :generate) (generic_date_ne :generate) 
      (generic_pl_noun_ne :generate)
      (generic_adj :generate ,#'(lambda (ep) (match-pred ep "a")))
      (generic_adverb :generate ,#'(lambda (ep) (match-pred ep "a")))
      (gen_generic_noun :generate ,#'(lambda (ep) (match-pred ep "n")))
      (gen_generic_verb :generate ,#'(lambda (ep) (match-pred ep "v"))))))

(defparameter *non-idiom-root*
    'root_non_idiom )

;;;
;;; for use in LOGON, set post-generation semantic equivalence check to filter
;;; mode, i.e. prefer results that satisfy the test when available, but output
;;; all complete generator results, in case none pass the equivalence test.
;;;
#+:logon
(setf *bypass-equality-check* :filter)

;;;
;;; with recent LKB versions (as of 23-jul-05), there is now better support for
;;; the (still primitive) `remote' generation mode: a `translation grid' can be
;;; configured from any number of LKB processes, each potentially prepared to
;;; act as a generator server.  the following, for example:
;;;
;;;  (setf *translate-grid* '(:ja . (:ja)))
;;;
;;; indicates that we can act as a generator server for japanese ourselves and
;;; will send of generation requests (from selection `Rephrase' on the parse
;;; summary view or `Generate' on the LOGON MRS browser) to a japanese server,
;;; i.e. ourselves.  likewise,
;;;
;;;   (setf *translate-grid* '(:ja . (:ja :en :no)))
;;;
;;; will send requests to three servers, which is something emily has long
;;; wanted (using an array of Matrix grammars and an interlingua semantics).
;;;
(setf *translate-grid* '(:en . (:en)))

;; Disable sanitizing check for constructions, since the conjunction reduction
;; rules don't constrain the C-CONT diff lists at definition time.
(setf *c-cont-check-path* nil)

;; Set default MRS XML output file name, since this appears to be necessary
;; in order for the "Save as XML" button on MRS windows to work.  FIX?

(setf *mrs-xml-output-file* "~/tmp/mrs.xml")

;;;
;;; chart mapping is enabled by setting *token-type* to the name of the token
;;; type (standardly, `token'). Token feature structures have type *token-type*
;;; and consist of grammar-specific bundles of properties that were input to
;;; parsing; to access individual components, there are a number of
;;; (customizable) paths.
;;;
(def-lkb-parameter *token-type* 'token)

(def-lkb-parameter *token-form-path* '(+FORM))
(def-lkb-parameter *token-id-path* '(+ID))
(def-lkb-parameter *token-from-path* '(+FROM))
(def-lkb-parameter *token-to-path* '(+TO))
(def-lkb-parameter *token-postags-path* '(+TNT +TAGS))
(def-lkb-parameter *token-posprobs-path* '(+TNT +PRBS))

;;;
;;; when token mapping is enabled, lexical entries behave similarly to grammar
;;; rules in one respect: the list of input tokens that license a lexical entry
;;; are unified into *lexicon-tokens-path*.  furthermore, to give the
;;; grammarian easier access to the token in the right periphery, the last element
;;; of the tokens list is made re-entrant with *lexicon-last-token-path*.
;;;
(def-lkb-parameter *lexicon-tokens-path* '(TOKENS +LIST))
(def-lkb-parameter *lexicon-last-token-path* '(TOKENS +LAST))

;;;
;;; chart mapping rules consist of between 1 and 5 components (the minimum being
;;; a single input FS)
;;;
(def-lkb-parameter *chart-mapping-context-path* '(+CONTEXT))
(def-lkb-parameter *chart-mapping-input-path* '(+INPUT))
(def-lkb-parameter *chart-mapping-output-path* '(+OUTPUT))
(def-lkb-parameter *chart-mapping-position-path* '(+POSITION))
(def-lkb-parameter *chart-mapping-jump-path* '(+JUMP))

;;;
;;; some rules should be constrained to only apply over the entire string, i.e.
;;; to edges that span the full input; this should improve parsing efficiency
;;; only, rather than be considered part of the linguistic analyses.
;;;
(defparameter *spanning-only-rules*
  '(aj-hd_int-inv_c
    aj-r_frg_c np-aj_frg_c cl_rel-frg_c aj-np_int-frg_c
    pp-aj_frg_c j-aj_frg_c np_nb-frg_c np-cl_numitem_c np-cl_lettitem_c 
    cl_cp-frg_c cl-np_runon-prn_c conj-frg_c hd-aj_scp-noclpnct_c))
