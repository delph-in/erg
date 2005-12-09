;;; -*- Mode: Common-Lisp; Package: LKB; -*-

;;; Copyright (c) 1991--2005
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

;(defparameter *lex-rule-suffix* "_INFL_RULE"
; "creates the inflectional rule name from the information
;   in irregs.tab - for PAGE compatability")

(defparameter *lex-rule-suffix* ""
 "creates the inflectional rule name from the information
   in irregs.tab - for PAGE compatability")

(defparameter *irregular-forms-only-p* t)

(defparameter *display-type-hierarchy-on-load* nil)

;;; Parsing

(defparameter *chart-limit* 100)

(defparameter *maximum-number-of-edges* 4000)

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")

(defparameter *start-symbol* 
  #-:arboretum
  ;'(root_strict)
  '(root_strict root_informal)
  ;'(root_strict root_frag)
  ;'(root_strict root_informal root_frag)
  ;'(root_strict root_informal root_frag root_robust)
  #+:arboretum
  '(root_strict root_strict_robust)
   "specifing valid parses")

;;;
;;; _fix_me_
;;; now that the old `root_lex' et al. are no longer available, we may need to
;;; do something in addition for LOGON fragment generation.
;;;                                                      (dan & oe; 20-apr-05)
(setf *fragment-start-symbols*
  '(root_strict root_informal root_frag 
    root_lex root_phr root_conj root_subord))
  
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
  ;'(STEM RELS HCONS RNAME PUNCT)
  '(STEM RELS HCONS RNAME)
  "restrictor used when parsing with ambiguity packing")

;;; (setf *chart-packing-p* t)

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

;;; for the compare function 

; For character encoding
(defparameter cdb::*cdb-ascii-p* nil)

; Turn on characterization in preprocessor
(setf *characterize-p* t)

(defparameter *discriminant-path* '(SYNSEM LOCAL KEYS KEY))

;;; Hide lexical rule nodes in parse tree
;;; (setf  *dont-show-lex-rules* t)
;;; this belongs in the user-prefs file, not here

(defparameter *duplicate-lex-ids* 
  '(will_aux_neg_2 would_aux_neg_2 do1_neg_2 hadnt_aux_1 hadnt_aux_2
    hadnt_aux_subj_1 hadnt_aux_subj_2 hasnt_aux_2 be_c_is_neg_2
    aint_sg_have_aux_1 aint_sg_have_aux_2 have_fin_aux_neg_2 
    aint_be_c_is_neg_1 aint_be_c_is_neg_2 be_id_is_neg_2
    aint_be_id_is_neg_1 aint_be_id_is_neg_2 
    be_th_cop_is_neg_2 aint_be_th_cop_is_neg_1 aint_be_th_cop_is_neg_2 
    might_aux_neg_1 might_aux_neg_2 must_aux_neg_1 must_aux_neg_2
    need_aux_neg_1 need_aux_neg_2 ought_aux_neg_1 ought_aux_neg_2
    should_aux_neg_2 could_aux_neg_2 be_id_was_neg_2 
    be_th_cop_was_neg_2 be_c_was_neg_2 
    be_id_was_neg_subj_1 be_id_was_neg_subj_2 be_th_cop_was_neg_subj_1 
    be_th_cop_was_neg_subj_2 be_c_was_neg_subj_1 be_c_was_neg_subj_2 
    be_c_were_neg_2 be_id_were_neg_2 
    be_th_cop_were_neg_2 be_c_were_neg_subj_1
    be_c_were_neg_subj_2 be_id_were_neg_subj_1 be_id_were_neg_subj_2
    be_th_cop_were_neg_subj_1 be_th_cop_were_neg_subj_2
    be_c_am_cx be_id_am_cx be_id_am_cx_2 be_c_are_cx be_c_are_cx_2
    be_id_are_cx be_id_are_cx_2 had_aux_cx had_aux_cx_2 has_aux_cx has_aux_cx_2
    have_fin_aux_cx have_fin_aux_cx_2 have_bse_aux_cx_1 have_bse_aux_cx_2 
    be_c_is_cx be_id_is_cx be_th_cop_is_cx 
    will_aux_pos_cx will_aux_pos_cx_2 would_aux_pos_cx 
    would_aux_pos_cx_2 had_better_cx had_better_cx_2
    u_pro you_guys you_people yall yall_2 yall_3 you_all
    and_conj_slash and_then_1 and_or_conj_1 and_or_conj_2
    and_or_conj_3 and_conj_amp and_conj_2_amp
    apostrophe_s_lex apostrophe_s_3_lex
    mister missus mr_title_2 doctor_ttl dr_ttl_2 prof_title mrs_title_2
    ms_title_2 mount_ttl_2 number_abb_title number_abb_title_2 
    number_abb2_title number_abb2_title_2 order_abb_ttl order_abb_ttl_2
    pound_sign_title pres_ttl pres_ttl_2 aint_be_c_am_neg_2 aint_be_c_are_neg_1
    aint_be_c_am_neg_1
    aint_be_c_are_neg_2 aint_be_c_is_neg_2 aint_be_id_am_neg_2
    aint_be_id_are_neg_2 aint_be_id_is_neg_2 aint_be_th_cop_are_neg_2
    aint_be_th_cop_is_neg_2 aint_pl_have_aux_2 aint_sg_have_aux_2
    be_c_am_neg_2 be_c_are_neg_2 be_c_was_neg_2 
    be_c_was_neg_subj_2 be_c_were_neg_2 be_c_were_neg_subj_2
    be_id_am_neg_2 be_id_are_neg_2 be_id_is_neg_2 be_id_was_neg_2
    be_id_was_neg_subj_2 be_id_were_neg_2 be_id_were_neg_subj_2
    be_th_cop_are_neg_2 be_th_cop_is_neg_2 be_th_cop_was_neg_2
    be_th_cop_was_neg_subj_2 be_th_cop_were_neg_2 be_th_cop_were_neg_subj_2
    can_aux_neg_2 dare_aux_neg_2 did1_neg_2 does1_neg_2 dont_2 
    might_aux_neg_2 must_aux_neg_2 need_aux_neg_2 ought_aux_neg_2 
    should_aux_neg_2 gonna_v1 that_c_subj wherein
    be_it_cop_is_neg_1 be_it_cop_is_neg_2 aint_be_it_cop_is_neg_1 
    aint_be_it_cop_is_neg_2 be_it_cop_was_neg_1 be_it_cop_was_neg_2
    be_it_cop_was_neg_subj_1 be_it_cop_was_neg_subj_2
    be_it_cop_is_cx be_it_cop_is_cx_2
    aint_be_it_cop_is_neg_2 be_it_cop_are_neg_2 be_it_cop_is_neg_2
    be_it_cop_was_neg_2 be_it_cop_was_neg_subj_2
    shall_aux_pos
    sunday_n2 monday_n2 tuesday_n2 wednesday_n2 thursday_n2 friday_n2
    saturday_n2 sunday_n3 monday_n3 tuesday_n3 wednesday_n3 thursday_n3 
    friday_n3 saturday_n3 slash_punct_adv1 or_else_1
    whom2 yours_truly_pn1 hour_n2 couple_adj
    customer_abb_n1 customer_abb_n2 
    april_abb_n1 april_abb_n2 april_abb_n3 april_abb_n4 
    august_abb_n1 august_abb_n22 august_abb_n3 august_abb_n4
    december_abb_n1 december_abb_n2 december_abb_n3 december_abb_n4 
    february_abb_n1 february_abb_n2 february_abb_n3 february_abb_n4
    january_abb_n1 january_abb_n2 january_abb_n3 january_abb_n4
    july_abb_n1 july_abb_n2 july_abb_n3 july_abb_n4
    june_abb_n1 june_abb_n2 june_abb_n3 june_abb_n4 
    march_abb_n1 march_abb_n2 march_abb_n3 march_abb_n4
    november_abb_n1 november_abb_n2 november_abb_n3 november_abb_n4 
    october_abb_n1 october_abb_n2 october_abb_n3 october_abb_n4
    september_abb_n1 september_abb_n2 september_abb_n3 september_abb_n4 
    number_abb_n1 number_abb_n2 number_abb_n3 number_abb_n4 number_abb_title 
    number_abb_title_2 order_abb_n1 order_abb_ttl 
    april_the_det august_the_det december_the_det february_the_det 
    january_the_det july_the_det june_the_det march_the_det may_the_det 
    november_the_det october_the_det september_the_det 
    km_abb_n1 lets_2 lets_3 a_det_2 i_2
    whether_or_not_c_fin whether_or_not_c_inf
    thee thou thy thine thine_nq ye aught threescore fourscore
    am_temp 
    noon_min pm_temp pm_temp_3
    clocktime-ersatz_2
    wanna_v1 wanna_v2 gotta_v1 cuz_subconj less_than_a_one_adj
    a+little_det_2 a_bit_adv2 a_couple_det2 a_det_2 a_few_det2 a_half_deg_2
    a_little_bit_adv2 a_little_deg_3 a_little_deg_4 a_lot_deg_2 a_np1
    a_one_adj_2 a_quarter_adj2 i_2 i_guess_disc_2 i_guess_disc_4
    i_mean_disc_2 i_must_say_root_2 i_must_say_root_4 i_think_disc_2
    or_conj_1a or_conj_2a order_n1a order_n2a order_ttla oregon_n2
    one_adj_digit his_her_poss_2 backcountry_n1
    3d_adj eight_day_num eight_day_num_yofc eighteen_day_num 
    eighteen_day_num_yofc eighteenth_day_num eighth_day_num eleven_day_num 
    eleven_day_num_yofc eleventh_day_num fifteen_day_num 
    fifteen_day_num_yofc fifteenth_day_num fifth_day_num
    first_day_num five_day_num five_day_num_yofc four_day_num 
    four_day_num_yofc fourteen_day_num fourteen_day_num_yofc 
    fourteenth_day_num fourth_day_num nine_day_num nine_day_num_yofc 
    nineteen_day_num nineteen_day_num_yofc nineteenth_day_num 
    ninth_day_num one_day_num one_day_num_euro one_day_num_yofc 
    second_day_num seven_day_num seven_day_num_yofc seventeen_day_num 
    seventeen_day_num_yofc seventeenth_day_num seventh_day_num 
    six_day_num six_day_num_yofc sixteen_day_num sixteen_day_num_yofc 
    sixteenth_day_num sixth_day_num ten_day_num ten_day_num_yofc 
    tenth_day_num third_day_num third_day_num_2 thirteen_day_num 
    thirteen_day_num_yofc thirteenth_day_num thirtieth_day_num 
    thirty_day_num thirty_day_num_yofc thirty_first_day_num 
    thirty_one_day_num thirty_one_day_num_yofc three_day_num 
    three_day_num_yofc twelfth_day_num twelve_day_num twelve_day_num_yofc 
    twentieth_day_num twenty_day_num twenty_day_num_yofc twentyeight_day_num 
    twentyeight_day_num_yofc twentyeighth_day_num twentyfifth_day_num 
    twentyfirst_day_num twentyfive_day_num twentyfive_day_num_yofc 
    twentyfour_day_num twentyfour_day_num_yofc twentyfourth_day_num 
    twentynine_day_num twentynine_day_num_yofc twentyninth_day_num 
    twentyone_day_num twentyone_day_num_yofc twentysecond_day_num 
    twentyseven_day_num twentyseven_day_num_yofc twentyseventh_day_num 
    twentysix_day_num twentysix_day_num_yofc twentysixth_day_num 
    twentythird_day_num twentythree_day_num twentythree_day_num_yofc 
    twentytwo_day_num twentytwo_day_num_yofc two_day_num two_day_num_yofc 
    three_day_yofc twelve_day_yofc twenty_day_yofc twenty_one_day_yofc 
    twentyeight_day_yofc twentyfive_day_yofc twentyfour_day_yofc 
    twentynine_day_yofc twentyseven_day_yofc twentysix_day_yofc 
    twentythree_day_yofc twentytwo_day_yofc two_day_yofc two_digit_day_yofc
    three_day twelve_day twenty_day twenty_num twenty_one_day twenty_one_day_2
    twentyeight_day twentyeight_day_2 twentyeighth_day_2 twentyfirst_day_2
    twentyfifth_day_2
    twentyfive_day twentyfive_day_2 twentyfour_day twentyfour_day_2
    twentyfourth_day_2 twentynine_day twentynine_day_2 twentyninth_day_2
    twentysecond_day_2 twentyseven_day twentyseven_day_2 twentyseventh_day_2
    twentysix_day twentysix_day_2 twentysixth_day_2 twentythird_day_2
    twentythree_day twentythree_day_2 twentythree_day_2_yofc twentytwo_day
    twentytwo_day_2 two_day number_char_n1
    i_2 i_stutter i_stutter_2
    be_th_cop_is_cx_2 be_id_is_cx_2 be_c_is_cx_2 be_c_am_cx_2
    telephone_abb_n1 telephone_abb_n2 adj_abb_n1 adv_abb_n1 anat_abb_n1
    centimeter_abb_n1 comparative_abb_n1 customer_abb_n1 customer_abb_n2
    diameter_abb_n1 foot_abb_n1 geometry_abb_n1 illustration_abb_n1
    illustration_abb_n3 imp_abb_n1 meter_abb_n1 milliliter_abb_n1
    okay_s_adv2 mhm_root_pre2 sure_root_pre2 yeah_root_pre2
    millimeter_abb_n1 minute_abb_n1 minute_abb_n2 noun_abb_n1
    number_abb_n1 number_abb_n2 number_abb_n3 number_abb_n4 order_abb_n1
    plural_abb_n1 prep_abb_n1 scripture_abb_n1 singular_abb_n1 st_abb_n1
    st_abb_n2 with_p_abb
    be_c_am_cx_neg_1 be_c_are_cx_neg_1 be_c_is_cx_neg_1 be_id_am_cx_neg_1
    be_id_are_cx_neg_1 be_id_is_cx_neg_1 be_nv_is_cx_neg_1
    be_th_cop_is_cx_neg_1 had_aux_cx_neg_1 had_better_cx_neg_1
    has_aux_cx_neg_1 have_aux_cx_neg_1 both_conj either_conj first_conj
    till_cp_p1 till_cp_p2 till_p1 till_p2 thru_p thru_a1
    how_bout how_bout_s how_bout_vp brine_cured_a2 account_n3 account_n4
    first_day_num ord1ersatz second_day_num ord2ersatz ord3ersatz
    1000s_n1 100s_n1 100s_n2 more_or_less_nc_deg
    oslofjorden_n1 like_minded_a2 like_minded_a3 quick_adv1
    bc_temp_1 bc_temp_2 ad_temp_1 ad_temp_2 x_to_y_adj_- x_to_y_nbar_hyphen
    x_to_y_np_pl_- x_to_y_np_sg_- x_to_y_np_sg_through x_to_y_np_until
    colour_n1 round anyplace_n2 anywhere_n2 everywhere_n2 nowhere_n2 
    someplace_n2 sometime_n2 somewhere_n2 en_route_pp_2 round_trip_n2
    hour_n2 hour_n3 hour_n4 approximately_abb seaside_n2
    gallon_abb_n1 gallon_abb_n2 millimeter_abb_n2 milliliter_abb_n2
    foot_abb_n2 centimeter_abb_n2 meter_abb_n2 with_p_abb2
    )
  "temporary expedient to avoid generating dual forms")

(setf *gen-ignore-rules*
  '(punct_bang_orule punct_semicol_orule punct_colon_orule
    punct_rparen_orule punct_lparen_orule punct_rbracket_orule 
    punct_lbracket_orule punct_dqright_orule punct_dqleft_orule 
    punct_sqright_orule punct_sqleft_orule 
    punct_sqleft2_orule punct_hyphen_orule punct_comma_informal_orule 
    paren_float_s paren_float_n
    ; This rule is relatively expensive and of marginal benefit in generation
    adjh_i_ques
   ))

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
;;; index accessibility filtering is incompatible with two-phase generation,
;;; and should also not be needed anymore.                     (14-jul-04; oe)
;;;                          
(setf *intersective-rule-names* nil
  #+:null
  '((adjh_i . (1)) (nadj_rr_nt . (2))))

(defparameter *chart-dependencies*
  '((SYNSEM LKEYS --+COMPKEY) (SYNSEM LOCAL CAT HEAD KEYS KEY)
    (SYNSEM LKEYS --+OCOMPKEY) (SYNSEM LOCAL CAT HEAD KEYS KEY)
    (SYNSEM LKEYS --COMPHD) (SYNSEM LOCAL CAT HEAD)
    (SYNSEM LKEYS --+SUBJIND) (SYNSEM --SIND)))

;;; AAC - Dec 2003
;;; *unknown-word-types*
;;; deliberately commented out, but code in user-fns
;;; depends on this just being proper names, since it sets CARG
;;; to the word string (downcased)
;;; (defparameter *unknown-word-types* '(n_proper_le))

;;;
;;; a new facility as of April 2005: initially for use in the generator only, 
;;; provide a set of generic lexical entries (i.e. actual instances) that get
;;; specialized according to their `surface' form, i.e. the value for STEM and
;;; CARG (or the equivalent in non-ERG grammars); specialization is triggered
;;; by unknown (singleton) relations in the generator input that actually have
;;; a CARG.  a new, temporary lexical entry is created and has the CARG value 
;;; destructively inserted (using instantiate-generic-lexical-entry(), which a
;;; grammar has to supply among its user functions :-{).        (7-apr-05; oe)
;;;
(defparameter *generic-lexical-entries*
  '((named_gle :generate) (guess_n_gle :generate)
    (card_gle :generate) (ord_gle :generate) 
    (yofc_gle :generate) (decade_gle :generate)))

(defparameter *non-idiom-root*
    'root_non_idiom )

;;;
;;; when loaded into an environment including [incr tsdb()] and the Redwoods
;;; tools, enable selective unpacking against a Maximum Entropy model.
;;;
#+:tsdb
(setf *unpacking-scoring-hook* #'tsdb::mem-score-configuration)

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

