;;; Copyright (c) 1991--2004
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

(defparameter *orth-path* '(stem))

(defparameter *list-tail* '(rest))

(defparameter *list-head* '(first))

(defparameter *empty-list-type* '*null*)

(defparameter *list-type* '*list*)

(defparameter *diff-list-type* '*diff-list*)

(defparameter *diff-list-list* 'list)

(defparameter *diff-list-last* 'last)

(defparameter *lex-rule-suffix* "_INFL_RULE"
 "creates the inflectional rule name from the information
   in irregs.tab - for PAGE compatability")

(defparameter *irregular-forms-only-p* t)

;;;

(defparameter *display-type-hierarchy-on-load* nil)

;;; Parsing

(defparameter *chart-limit* 100)

(defparameter *maximum-number-of-edges* 4000)

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")

(defparameter *start-symbol* 
  #-:arboretum
  '(root_strict)
  #+:arboretum
  '(root_strict root_strict_robust)
   "specifing valid parses")
;; Use the following for parsing fragments as well as full clauses:
#|
(defparameter *start-symbol* '(root_strict root_lex root_phr root_conj root_subord)
  "specifing valid parses including fragments")
|#

;;; Set to true for arboretum, enabling parsing with robust rules and lexicon
;;; (this assumes that :arboretum was pushed onto *features* before compiling
;;; the LKB and loading the grammar).  Then (after indexing lexicon for 
;;; generator) test by calling e.g. (lkb::grammar-check "dog barks").  
;;; Remember to touch letypes.tdl before loading ERG to flush the lexicon 
;;; cache, ensuring that mal-letypes.tdl gets loaded.
;
;(defparameter *mal-active-p* t)

(defparameter *maximal-lex-rule-applications* 7
   "The number of lexical rule applications which may be made
   before it is assumed that some rules are applying circularly")

(defparameter *deleted-daughter-features* 
  '(ARGS HEAD-DTR NON-HEAD-DTR LCONJ-DTR RCONJ-DTR DTR MOD-DTR NONMOD-DTR)
  "features pointing to daughters deleted on building a constituent")

;;;
;;; to enable local ambiguity packing
;;;

#+:null
(defparameter *chart-packing-p* t)

(defparameter *packing-restrictor*
  '(STEM RELS HCONS RULE-NAME)
  "restrictor used when parsing with ambiguity packing")

;;; (setf *chart-packing-p* t)

(setf *gen-packing-p* t)

;;;
;;; increase dag pool size
;;;

(defparameter *dag-pool-size* 200000)
(defparameter *dag-pool*
  (if (and (pool-p *dag-pool*) 
           (not (= (pool-size *dag-pool*) *dag-pool-size*)))
    (create-pool *dag-pool-size* #'(lambda () (make-safe-dag-x nil nil)))
    *dag-pool*))

;;;
;;; connection parameters for lexical database, an association list with fields
;;; `:host', `:db', `:table', and `:user' (optional) 
;;; if unset we fall back to .tdl lexicon files

(defparameter *psql-lexicon-parameters* 
  #+:psql
  ;; bmw20 (16 Feb 04) - phase out use of :table
  ;;  '((:user "bmw20") (:db "erg") (:host "localhost") (:table "erg2"))
  '((:db "erg") (:host "localhost") (:port "5432") (:table "erg"))
  #-:psql
  nil)

;;; Parse tree node labels

;;; the path where the name string is stored
(defparameter *label-path* '(LABEL-NAME))

;;; the path for the meta prefix symbol
(defparameter *prefix-path* '(META-PREFIX))

;;; the path for the meta suffix symbol
(defparameter *suffix-path* '(META-SUFFIX))

;;; the path for the recursive category
(defparameter *recursive-path* '(SYNSEM NON-LOCAL SLASH LIST FIRST))

;;; the path inside the node to be unified with the recursive node
(defparameter *local-path* '(SYNSEM LOCAL))

;;; the path inside the node to be unified with the label node
(defparameter *label-fs-path* '())

(defparameter *label-template-type* 'label)

;;; for the compare function 

(defparameter *discriminant-path* '(SYNSEM LOCAL KEYS KEY))

;;; Hide lexical rule nodes in parse tree
;;; (setf  *dont-show-lex-rules* t)
;;; this belongs in the user-prefs file, not here

(defparameter *duplicate-lex-ids* 
  '(an_det will_aux_neg_2 would_aux_neg_2 do1_neg_2 hadnt_aux_1 hadnt_aux_2
    hadnt_aux_subj_1 hadnt_aux_subj_2 hasnt_aux_1 hasnt_aux_2 be_c_is_neg_2
    aint_sg_have_aux_1 aint_sg_have_aux_2 have_fin_aux_neg_2 be_c_is_neg_1  
    aint_be_c_is_neg_1 aint_be_c_is_neg_2 be_id_is_neg_1 be_id_is_neg_2
    aint_be_id_is_neg_1 aint_be_id_is_neg_2 be_th_cop_is_neg_1 
    be_th_cop_is_neg_2 aint_be_th_cop_is_neg_1 aint_be_th_cop_is_neg_2 
    might_aux_neg_1 might_aux_neg_2 must_aux_neg_1 must_aux_neg_2
    need_aux_neg_1 need_aux_neg_2 ought_aux_neg_1 ought_aux_neg_2
    should_aux_neg_2 could_aux_neg_2 be_id_was_neg_1 be_id_was_neg_2 
    be_th_cop_was_neg_1 be_th_cop_was_neg_2 be_c_was_neg_1 be_c_was_neg_2 
    be_id_was_neg_subj_1 be_id_was_neg_subj_2 be_th_cop_was_neg_subj_1 
    be_th_cop_was_neg_subj_2 be_c_was_neg_subj_1 be_c_was_neg_subj_2 
    be_c_were_neg_1 be_c_were_neg_2 be_id_were_neg_1 be_id_were_neg_2 
    be_th_cop_were_neg_1 be_th_cop_were_neg_2 be_c_were_neg_subj_1 
    be_c_were_neg_subj_2 be_id_were_neg_subj_1 be_id_were_neg_subj_2 
    be_th_cop_were_neg_subj_1 be_th_cop_were_neg_subj_2 
    be_c_am_cx be_c_am_cx_2 be_id_am_cx be_id_am_cx_2 be_c_are_cx be_c_are_cx_2
    be_id_are_cx be_id_are_cx_2 had_aux_cx had_aux_cx_2 has_aux_cx has_aux_cx_2
    have_fin_aux_cx have_fin_aux_cx_2 have_bse_aux_cx_1 have_bse_aux_cx_2 
    be_c_is_cx be_c_is_cx_2 be_id_is_cx be_id_is_cx_2 be_th_cop_is_cx 
    be_th_cop_is_cx_2 will_aux_pos_cx will_aux_pos_cx_2 would_aux_pos_cx 
    would_aux_pos_cx_2 had_better_cx had_better_cx_2
    not_really not_quite still_not rather_not
    u_pro you_guys you_people yall yall_2 yall_3 you_all
    and_conj_slash and_then_1 and_conj_2 and_or_conj_1 and_or_conj_2
    and_or_conj_3 and_conj_amp and_conj_2_amp then_conj_1
    apostrophe_s_lex apostrophe_s_phr apostrophe_s_3_lex apostrophe_s_3_phr
    mister missus mr_title_2 doctor_ttl dr_ttl_2 prof_title mrs_title_2
    ms_title_2 mount_ttl_2 number_abb_title number_abb_title_2 
    number_abb2_title number_abb2_title_2 order_abb_ttl order_abb_ttl_2
    pound_sign_title pres_ttl pres_ttl_2 aint_be_c_am_neg_2 aint_be_c_are_neg_1
    aint_be_c_are_neg_2 aint_be_c_is_neg_2 aint_be_id_am_neg_2
    aint_be_id_are_neg_2 aint_be_id_is_neg_2 aint_be_th_cop_are_neg_2
    aint_be_th_cop_is_neg_2 aint_pl_have_aux_2 aint_sg_have_aux_2
    be_c_am_neg_2 be_c_are_neg_2 be_c_is_neg_2 be_c_was_neg_2
    be_c_was_neg_subj_2 be_c_were_neg_2 be_c_were_neg_subj_2
    be_id_am_neg_2 be_id_are_neg_2 be_id_is_neg_2 be_id_was_neg_2
    be_id_was_neg_subj_2 be_id_were_neg_2 be_id_were_neg_subj_2
    be_th_cop_are_neg_2 be_th_cop_is_neg_2 be_th_cop_was_neg_2
    be_th_cop_was_neg_subj_2 be_th_cop_were_neg_2 be_th_cop_were_neg_subj_2
    can_aux_neg_2 dare_aux_neg_2 did1_neg_2 does1_neg_2 dont_2 
    might_aux_neg_2 must_aux_neg_2 need_aux_neg_2 ought_aux_neg_2 
    should_aux_neg_2 gonna_v1 and_c_nonprop that_c_subj wherein
    be_it_cop_is_neg_1 be_it_cop_is_neg_2 aint_be_it_cop_is_neg_1 
    aint_be_it_cop_is_neg_2 be_it_cop_was_neg_1 be_it_cop_was_neg_2
    be_it_cop_was_neg_subj_1 be_it_cop_was_neg_subj_2 be_it_cop_were_neg_1
    be_it_cop_were_neg_2 be_it_cop_were_neg_subj_1 be_it_cop_were_neg_subj_2
    be_it_cop_is_cx be_it_cop_is_cx_2 aint_be_it_cop_are_neg_2
    aint_be_it_cop_is_neg_2 be_it_cop_are_neg_2 be_it_cop_is_neg_2 
    be_it_cop_was_neg_2 be_it_cop_was_neg_subj_2 be_it_cop_were_neg_2 
    be_it_cop_were_neg_subj_2 shall_aux_pos
    sunday_n2 monday_n2 tuesday_n2 wednesday_n2 thursday_n2 friday_n2
    saturday_n2 slash_punct_adv1 or_else_1
    whom2 yours_truly_pn1 
    numvaldet one_day_num_ersatz one_num_determiner
   )
  "temporary expedient to avoid generating dual forms")

(setf *semantics-index-path* '(SYNSEM LOCAL CONT HOOK INDEX))

; DPF 27-Nov-03 - Finally noticed that on the current clever approach to
; intersective modification, we can't generate np-adverbial modifiers as
; in "Kim arrived the day after Sandy" since "*Kim arrived the day" is not
; well-formed, which means we don't generate that 'skeleton' into which we
; could then insert the PP-modifier.  Given that the presence of intersective
; modifiers is apparently sometimes syntactically required, this two-stage
; approach to generation may be threatened.  For now, commenting out the
; relevant noun-modifying rules.  Also hadj_i_uns, since only verb-participles
; which are post-modified can be reduced relatives: "women working for Browne"

;(setf *intersective-rule-names* '(adjn_i adjh_i nadj_rc
;                                  nadj_rr_nt nadj_rr_t hadj_i_uns))
;(setf *intersective-rule-names* '(adjh_i nadj_rr_nt))

;;;
;;; as of mid-december 2003, the generator allows specification of the non-foot
;;; daughters in adjunction rules; make this conditional on LKB source version,
;;; so the grammar still loads into older LKBs.                (18-dec-03; oe)
;;; 
(setf *intersective-rule-names* 
  (if (find-symbol "%GENERATOR-LEXICAL-ITEMS%" :lkb)
    '((adjh_i . (1)) (nadj_rr_nt . (2)))
    '(adjh_i nadj_rr_nt)))

(defparameter *chart-dependencies*
  '((SYNSEM LKEYS --+COMPKEY) (SYNSEM LOCAL CAT HEAD KEYS KEY)
    (SYNSEM LKEYS --+OCOMPKEY) (SYNSEM LOCAL CAT HEAD KEYS KEY)
    (SYNSEM LKEYS --+SUBJIND) (SYNSEM --SEMIND)))

;;; AAC - Dec 2003
;;; *unknown-word-types*
;;; deliberately commented out, but code in user-fns
;;; depends on this just being proper names, since it sets CARG
;;; to the word string (downcased)
;;; (defparameter *unknown-word-types* '(n_proper_le))

(defparameter *non-idiom-root*
    'root_non_idiom )

