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

;;; Parsing

(defparameter *chart-limit* 100)

(defparameter *maximum-number-of-edges* 4000)

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")

(defparameter *start-symbol* 
  #-(or :speech :arboretum)
  '(root_strict root_frag)
  #+:speech
  '(root_informal root_spoken root_spoken_frag)
  ;'(root_informal root_frag root_inffrag)
  ;'(root_informal root_frag root_inffrag root_robust)
  ;'(root_formal)
  ;'(root_decl)
  #+:arboretum
  '(root_standard root_question root_robust_s root_lex root_frag root_inffrag)
   "specifing valid parses")

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
  '(ORTH RELS HCONS RNAME)
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

(defparameter *duplicate-lex-ids* 
  '(will_aux_neg_2 would_aux_neg_2 do1_neg_2 hadnt_aux_1 hadnt_aux_2
    hadnt_aux_subj_1 hadnt_aux_subj_2 hasnt_aux_2 be_c_is_neg_2
    aint_sg_have_aux_1 aint_sg_have_aux_2 have_fin_aux_neg_2 
    aint_be_c_is_neg_1 aint_be_c_is_neg_2 be_id_is_neg_2
    aint_be_id_is_neg_1 aint_be_id_is_neg_2 
    be_th_cop_is_neg_2 aint_be_th_cop_is_neg_1 aint_be_th_cop_is_neg_2 
    might_aux_neg_1 might_aux_neg_2 must_aux_neg_1 must_aux_neg_2
    need_aux_neg_1 need_aux_neg_2 ought_aux_neg_1 ought_aux_neg_2
    should_aux_neg_2 could_aux_neg_2 could_aux_neg_2b be_id_was_neg_2 
    be_th_cop_was_neg_2 be_c_was_neg_2 
    be_id_was_neg_subj_1 be_id_was_neg_subj_2 be_th_cop_was_neg_subj_1 
    be_th_cop_was_neg_subj_2 be_c_was_neg_subj_1 be_c_was_neg_subj_2 
    be_c_were_neg_2 be_id_were_neg_2 
    be_th_cop_were_neg_2 be_c_were_neg_subj_1
    be_c_were_neg_subj_2 be_id_were_neg_subj_1 be_id_were_neg_subj_2
    be_th_cop_were_neg_subj_1 be_th_cop_were_neg_subj_2 need_not_v3
    u_pro you_people yall yall_2 yall_3 you_all
    and_or_conj_1 and_or_conj_2 and_or_conj_3 
    and_or_conj_4 and_or_conj_5 and_or_conj_6 and_conj_amp
    apostrophe_s_3_lex apostrophe_s_4_lex
    apostrophe_s_12_lex apostrophe_s_14_lex apostrophe_s_nq_2_lex
    mister missus mister_abb_n1 doctor_abb_n1 professor_abb_n1 missus_abb_n1
    number_abb_n2 number_abb_n3 number_abb_n4 number_abb_n5 number_char_n1
    order_abb_n1 order_abb_n2 president_abb_n1 president_abb_n2
    aint_be_c_am_neg_2 aint_be_c_are_neg_1
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
    can_aux_neg_2 dare_aux_neg_2 did1_neg_2 does1_neg_2 dont_2 dont_4
    might_aux_neg_2 must_aux_neg_2 need_aux_neg_2 ought_aux_neg_2 
    should_aux_neg_2 gonna_v1 that_c_subj wherein
    be_it_cop_is_neg_1 be_it_cop_is_neg_2 aint_be_it_cop_is_neg_1 
    aint_be_it_cop_is_neg_2 be_it_cop_was_neg_1 be_it_cop_was_neg_2
    be_it_cop_was_neg_subj_1 be_it_cop_was_neg_subj_2
    aint_be_it_cop_is_neg_2 be_it_cop_is_neg_2
    be_it_cop_was_neg_2 be_it_cop_was_neg_subj_2
    be_nv_are_neg_2 be_nv_is_neg_2 be_nv_was_neg_2 be_nv_was_neg_subj_2
    be_nv_were_neg_2 be_nv_were_neg_subj_2
    shall_aux_pos
    sunday_n2 monday_n2 tuesday_n2 wednesday_n2 thursday_n2 friday_n2
    saturday_n2 sunday_n3 monday_n3 tuesday_n3 wednesday_n3 thursday_n3 
    friday_n3 saturday_n3 thursday_n4 thursday_n5 or_else_1
    whom2 yours_truly_pn1 hour_n2 couple_adj
    customer_abb_n1 customer_abb_n2 
    april_abb_n1 april_abb_n3 august_abb_n1 august_abb_n3
    december_abb_n1 december_abb_n3 february_abb_n1 february_abb_n3
    january_abb_n1 january_abb_n3 july_abb_n1 july_abb_n3
    june_abb_n1 june_abb_n3 march_abb_n1 march_abb_n3
    november_abb_n1 november_abb_n3 october_abb_n1 october_abb_n3
    september_abb_n1 september_abb_n4 september_abb_n5 september_abb_n8
    april_the_det august_the_det december_the_det february_the_det 
    january_the_det july_the_det june_the_det march_the_det may_the_det 
    november_the_det october_the_det september_the_det 
    km_abb_n1 km_abb_n2 lets_2 lets_3
    whether_or_not_c_fin whether_or_not_c_inf
    thee thou thy thine thine_nq ye aught threescore fourscore
    am_temp_2 am_temp_3 am_temp_4 am_temp_5 pm_temp_2 pm_temp_3 
    pm_temp_4 pm_temp_5 pm_temp_6 noon_min 
    wanna_v1 wanna_v2 gotta_v1 cuz_subconj
    his_her_poss his_her_poss_2 his_her_poss_3 backcountry_n1
    eight_day eighteen_day eleven_day fifteen_day five_day
    four_day fourteen_day nine_day nineteen_day one_day seven_day
    seventeen_day six_day sixteen_day ten_day thirteen_day third_day_num 
    thirty_day thirty_one_day three_day twelve_day twenty_day twenty_num 
    twenty_one_day twenty_one_day_2 twentyeight_day twentyeight_day_2 
    twentyeighth_day_2 twentyfirst_day_2 twentyfifth_day_2 twentyfive_day 
    twentyfive_day_2 twentyfour_day twentyfour_day_2 twentyfourth_day_2 
    twentynine_day twentynine_day_2 twentyninth_day_2 twentysecond_day_2 
    twentyseven_day twentyseven_day_2 twentyseventh_day_2 twentysix_day 
    twentysix_day_2 twentysixth_day_2 twentythird_day_2 twentythree_day 
    twentythree_day_2 twentytwo_day twentytwo_day_2 two_day 3d_adj 
    third_day_num_2 number_char_n1 
    i_stutter_4_rbst i_stutter_6_rbst i_stutter_rbst
    we_stutter_1_rbst we_stutter_2_rbst we_stutter_3_rbst
    you_stutter_1_rbst you_stutter_2_rbst you_stutter_3_rbst
    they_stutter_1_rbst they_stutter_2_rbst they_stutter_3_rbst
    he_stutter_1_rbst he_stutter_2_rbst he_stutter_3_rbst
    she_stutter_1_rbst she_stutter_2_rbst she_stutter_3_rbst
    it_stutter_1_rbst it_stutter_2_rbst it_stutter_3_rbst
    telephone_abb_n1 telephone_abb_n2 adj_abb_n1 adv_abb_n1 anat_abb_n1
    centimeter_abb_n1 comparative_abb_n1 customer_abb_n1 customer_abb_n2
    diameter_abb_n1 foot_abb_n1 geometry_abb_n1 illustration_abb_n1
    illustration_abb_n3 imp_abb_n1 imp_abb_a1 meter_abb_n1 milliliter_abb_n1
    okay_s_adv2 millimeter_abb_n1 minute_abb_n1 minute_abb_n2 noun_abb_n1
    plural_abb_n1 plural_abb_n2 prep_abb_n1 scripture_abb_n1 singular_abb_n1 
    singular_abb_n2 thanks_abb_root_post thanks_abb_root_pre thanks_abb_v1
    st_abb_n1 st_abb_n2 with_p_abb prep_abb_n1 prep_abb_n2
    reverend_abb_n1 reverend_abb_n2
    be_c_am_cx_neg_1 be_c_are_cx_neg_1 be_c_are_cx_neg_2
    be_c_is_cx_neg_1 be_c_is_cx_neg_2 be_id_am_cx_neg_1 be_id_am_cx_neg_2
    be_id_are_cx_neg_1 be_id_are_cx_neg_2 be_id_is_cx_neg_1 be_id_is_cx_neg_2
    be_nv_is_cx_neg_1 be_nv_is_cx_neg_2
    be_th_cop_is_cx_neg_1 be_th_cop_is_cx_neg_2
    had_aux_cx_neg_1 had_aux_cx_neg_2 
    had_better_cx_neg_1 had_better_cx_neg_2
    has_aux_cx_neg_1 has_aux_cx_neg_2 have_aux_cx_neg_1 have_aux_cx_neg_2
    had_best_cx had_best_cx_2 had_best_cx_3 had_best_cx_neg_1 had_best_cx_neg_2
    both_conj either_conj first_conj
    till_cp_p1 till_cp_p2 till_p1 till_p2 thru_p thru_a1
    how_bout how_bout_s how_bout_vp account_n3 account_n4
    second_abb_n1 second_abb_n2 second_n2 second_n3
    1000s_n1 100s_n1 100s_n2 more_or_less_nc_deg
    oslofjorden_n1 quick_adv1
    bc_temp_1 bc_temp_2 ad_temp_1 ad_temp_2 x_to_y_adj_- x_to_y_nbar_hyphen
    x_to_y_np_pl_- x_to_y_np_sg_- x_to_y_np_sg_through x_to_y_np_until
    colour_n1 anywhere_n2 everywhere_n2 nowhere_n2 
    someplace_n2 sometime_n2 somewhere_n2 en_route_pp_2 round_trip_n2 hour_n5
    hour_n2 hour_n3 hour_n4 approximately_abb approximately_abb_2 
    approximately_abb_3 appointment_abb_n1 appointment_abb_n2 seaside_n2
    gallon_abb_n1 gallon_abb_n2 millimeter_abb_n2 milliliter_abb_n2
    foot_abb_n2 centimeter_abb_n2 meter_abb_n2 kg_abb_n1 kg_abb_n2 km2_abb_n1
    with_p_abb2 with_p_abb3 with_p_abb4 with_p_abb5 figure_abb_n1
    fig_abb obs_abb_a1 obs_abb_a2 please_abb_adv please_abb_adv3
    please_abb_root route_abb_n1 route_abb_n2
    orig_abb orig_abb_2 specif_abb specif_abb_2 adj_abb_n1 adj_abb_n2
    anat_abb_n2 adv_abb_n1 adv_abb_n2 as_abb_post_ttl esp_abb_2 esp_abb_3
    alabama_n2 alaska_n2 arizona_n2 arkansas_n2 california_n2
    connecticut_n2 florida_n2 georgia_n2 hawaii_n2 idaho_n2 iowa_n2
    illinois_n2 kansas_n2 kentucky_n2 louisiana_n2 maryland_n2 massachusetts_n2
    michigan_n2 minnesota_n2 missouri_n2 montana_n2 nebraska_n2 
    nevada_n2 new_york_n2 newhampshire_n2 newjersey_n2 newmexico_n2
    northcarolina_n2 northdakota_n2 ohio_n2 oklahoma_n2 
    pennsylvania_n2 rhodeisland_n2 southcarolina_n2 southdakota_n2 tennessee_n2
    texas_n2 utah_n2 vermont_n2 virginia_n2 washington_n2 washingtondc_n2
    washingtondc_n4 washingtondc_n5 washingtondc_n6  westvirginia_n2 
    wisconsin_n2 wyoming_n2 goodwill_n1 
    okay_s_adv2 okay_s_adv3 e_mail_n1 e_mail_n2 e_mail_n3
    doubtlessly spec_n1 hon_abb_a1 them_abb
    yer_pro e_g_pp2 e_g_disc_adv2 e_g_a2 e_g_pp3 e_g_disc_adv3 e_g_a3
    year_abb_n1 year_abb_n2 years_abb_n1
    full-grown_a2 because_abb because_of_abb_p worthwhile_a2 worthwhile_a4
    slash_per_p slash_per_p2 slash_per_p3 slash_per_p4 slash_per_p5 
    be_inv_are be_inv_is be_inv_was be_inv_were come_v3 go_v3 lie_v4 run_v4
    stand_v3 micro_a2 mid_isect super_deg1 temperature_abb_n1
    zero_det liquify_v1 liquify_v2 used_to_aux_nonfin_2 and_num hafta_v1
    be_inv_is_cx be_inv_is_cx_2 be_inv_are_cx be_inv_are_cx_2 
    had_better_cx had_better_cx_2 had_better_cx_3
    but+rather_np_conj overall_a2 anti_a2
    till_p3 today_adv3 today_np2 miscellaneous_abb_a1 private_abb_a1
    standup_a1 standup_a3 departure_abb_n1 arrival_abb_n1 arriving_abb_a1
    criteria_n1 noontime tv_n2
    lean_to_n2 non_a2 the_rbst_1 the_rbst_2 the_rbst_3
    the_rbst_4 the_rbst_5 the_rbst_6 the_rbst_7 
    a_det_rbst a_det_3_rbst an_det_rbst an_det_2_rbst
    its_poss_rbst permit_rbst_v1 recommend_rbst_v1
    advise_rbst_v1 allow_rbst_v1 enable_rbst_v1 suggest_rbst_v1 
    a_a_det_rbst an_an_det_rbst
    foot_apostr_n1 avec_nbar threshhold_n1 awful_adv broke_robust
    ; Exclude contracted auxiliaries for the time being
    be_c_am_cx be_c_am_cx_2 be_c_am_cx_3 be_c_are_cx be_c_are_cx_2
    be_c_is_cx be_c_is_cx_2 be_c_is_cx_3
    be_id_am_cx be_id_am_cx_2 be_id_am_cx_3
    be_id_are_cx be_id_are_cx_2 be_id_is_cx be_id_is_cx_2 be_id_is_cx_3
    be_it_cop_is_cx be_it_cop_is_cx_2 be_it_cop_is_cx_3
    be_nv_is_cx be_nv_is_cx_2 be_nv_is_cx_3
    be_th_cop_is_cx be_th_cop_is_cx_2 be_th_cop_is_cx_3
    had_aux_cx had_aux_cx_2 had_aux_cx_3 has_aux_cx has_aux_cx_2 
    has_aux_cx_3 have_bse_aux_cx_1 have_bse_aux_cx_2 have_bse_aux_cx_3
    have_fin_aux_cx have_fin_aux_cx_2 have_fin_aux_cx_3
    had_aux_cx had_aux_cx_2 has_aux_cx has_aux_cx_2
    have_bse_aux_cx_1 have_bse_aux_cx_2 have_fin_aux_cx have_fin_aux_cx_2
    will_aux_pos_cx will_aux_pos_cx_2 will_aux_pos_cx_3
    would_aux_pos_cx would_aux_pos_cx_2 would_aux_pos_cx_3
    whatsoever1 whatsoever_det whosoever_n1
    easy_to_use_a2 on_screen_a2 psyche_out_v2 no_brainer_n2 off_camera_a2
    all_in_all_adv2 must_have_n2 after_market_a2 in_depth_a2
    top_of_the_line_a2 out_of_town_a2 set_up_n2 
    height_abb_n1 weight_abb_n1
    care_of_prep_2 care_of_prep_3 m_z_n2
    reference_abb_n1 reference_abb_pl_n1 circa_abb_deg_2 circa_abb_deg_1
    e_g_cma_p1 e_g_cma_p2 e_g_cma_p3 e_g_cma_p4 e_g_cma_p5 e_g_cma_p6
    e_g_cma_p7 outta_p1 furthermore_root_pre_2
    arriving_abb_a1 goodbye_n2 goodbye_root_pre_2 very_first_adj1
    the_1_rbst the_2_rbst the_3_rbst the_4_rbst the_5_rbst the_6_rbst 
    the_7_rbst
    one_digit_adj
    )
  "temporary expedient to avoid generating dual forms")

(setf *gen-ignore-rules*
  '(w_bang_plr w_semicol_plr w_sqright_plr
    w_sqleft_plr w_dqleft_plr w_rparen_plr w_lparen_plr
    w_dqright_plr w_rbrack_plr w_lbrack_plr w_comma-rp_plr
    w_hyphen_plr w_comma-nf_plr w_double_semicol_plr
    w_qqmark_plr w_qmark-bang_plr w_drop-iright_plr
    w_drop-ileft_plr w_italleft_plr w_italright_plr 
    v_prp-nf_olr
    hdn-cl_prnth_c hdn-n_prnth_c
    ; This rule allows a missing final conjunction in multiple coordination
    ; The following rules allow variation in usage of commas
    aj-hd_scp-pr_c hd-aj_scp-pr_c hdn-np_app_c hdn-aj_redrel-pr_c
    cl-cl_crd-im_c pp-pp_crd-im_c
    r-r_crd-im_c np-np_crd-im_c n-n_crd-im_c 
    j-j_crd-prd-im_c flr-hd_nwh-nc_c
    ; The following rule allows that-S as a fragment, but with MRS the same
    ; as for an ordinary matrix S, so we don't use it for generation.      
    cl_cp-frg_c
    ; The following rule allows inverted quoting verbs, as in "said Kim",
    ; giving unnecessary variation in generation.
    v_inv-quot_dlr
    ; The following rule allows adverbs and PPs after verbs but preceding
    ; complements (one part of 'heavy-NP shift' phenomena)
    hd-aj_vmod_c
    ;;
    ;; suggested in email by dan (9-feb-08), suppress in-situ WH pronouns.
    ;;
    sb-hd_q_c
    robust_np_sg robust_subjh_sg robust_subjh_pl
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


