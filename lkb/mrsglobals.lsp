(in-package "MRS")

(setf *sem-relation-suffix* "_rel")

(setf *value-feats* `(,(vsym "CARG")))

(setf *feat-priority-list*  
  `( ,(vsym "LTOP") ,(vsym "INDEX") ,(vsym "LBL")
     ,(vsym "ARG0") ,(vsym "ARG1") ,(vsym "ARG2") ,(vsym "ARG3") 
     ,(vsym "RSTR") ,(vsym "BODY")
     ,(vsym "MARG") ,(vsym "CARG")))
                                 
(setf *ignored-sem-features* 
  (append *ignored-sem-features* 
  `(,(vsym "PARAMS") ,(vsym "WLINK"))))

(setf *ignored-extra-features* 
  (cons (vsym "SORT") *ignored-extra-features*))

(setf *top-level-rel-types*  nil)

;;; features for extracting semantics from expanded lexical entries

(setf *dummy-relations* `(,(vsym "NO_REL") ,(vsym "MESSAGE")))

(defparameter *mrs-to-vit* nil)

(defparameter *mrs-for-language* 'english)

(defparameter *mrs-scoping* nil)

(setf *initial-semantics-path* 
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT") ))

(setf *main-semantics-path* 
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT") 
    ,(vsym "RELS") ,(vsym "LIST")))

(setf *construction-semantics-path*
  `(,(vsym "C-CONT") ,(vsym "RELS") ,(vsym "LIST")))

(setf *psoa-top-h-path* 
  `(,(vsym "HOOK") ,(vsym "LTOP")))

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
  (vsym "RELATION"))

(setf *rel-name-path* `(,(vsym "PRED") ))

(setf *top-pred-type* (vsym "PREDSORT"))

;;;
;;; add these to ERG `mrsglobals.lisp' (although they correspond to initial MRS
;;; defaults, so we can load the ERG on top of another grammar (and spare some
;;; debugging effort the day some of the MRS defaults changes :-).

(defparameter *event-type* (vsym "event"))
(defparameter *event_or_index-type* (vsym "event_or_index"))
(defparameter *non_expl-ind-type* (vsym "non_expl-ind"))
(defparameter *handle-type* (vsym "handle"))
(defparameter *ref-ind-type* (vsym "ref-ind"))

;;; the following are needed only for the detection of fragments
;;; indicated in the LinGO gramar by the value of ROOT

(setf *root-path* `(,(vsym "ROOT")))

(setf *false-type* (vsym "-"))

(setf *true-type* (vsym "+"))

; DPF Upped from 100 to 200
; DPF 1-Jul-03 Upped from 200 to 500 due to treatment of unspec_loc_rel for
; e.g. free relatives.  Maybe should make this generic rel more specific.
(setf *maximum-genindex-relations* 500)

;;;
;;; interim solution for MRS `unfilling' until we construct a proper SEM-I
;;;
(setf %mrs-extras-filter%
  ;;
  ;; _fix_me_
  ;; even without the full SEMI, we should compute these.  (21-nov-03; oe)
  ;;
  (list
   (cons (mrs::vsym "E.TENSE") (mrs::vsym "BASIC_TENSE"))
   (cons (mrs::vsym "E.ASPECT.PROGR") (mrs::vsym "LUK"))
   (cons (mrs::vsym "E.ASPECT.PERF") (mrs::vsym "LUK"))
   (cons (mrs::vsym "E.MOOD") (mrs::vsym "MOOD"))
   (cons (mrs::vsym "PNG.GEN") (mrs::vsym "REAL_GENDER"))
   (cons (mrs::vsym "DIVISIBLE") (mrs::vsym "BOOL"))
   (cons (mrs::vsym "PRONTYPE") (mrs::vsym "PRONTYPE"))))

;;;
;;; yet another (i believe) interim solution: while there is no way of having
;;; a (possibly grammar-external) hierarchy of predicates, enumerate the set of
;;; predicates that require explitive arguments.
;;;
(defparameter *munge-expl-preds* 
  '("_rain_v_rel" "_drizzle_v_rel" "_hail_v_rel" "_snow_v_rel"
    "_sprinkle_v_rel" "_annoy_v_2_rel" "_appear_v_rel" "_arrange_v_expl_rel"
    "_bother_v_expl_rel" "_breezy_j_expl_rel" "_blustery_j_expl_rel"
    "_chilly_j_expl_rel" "_cloudy_j_expl_rel" "_cold_j_eexpl_rel"
    "_cool_j_expl_rel" "_cost_v_x_rel" "_damp_j_expl_rel" "_dry_j_expl_rel"
    "_exist_v_expl_rel" "_feel_v_seem_rel" "_foggy_j_expl_rel"
    "_frigid_j_expl_rel" "_gusty_j_expl_rel" "_hot_j_expl_rel"
    "_leave_v_to_rel" "_look_v_seem_rel" "_make_v_1_rel" "_mild_j_expl_rel"
    "_prefer_v_expl_rel" "_rainy_j_expl_rel" "_seem_v_rel" "_sound_v_seem_rel"
    "_sunny_j_expl_rel" "_take_v_expl_rel" "_time_j_expl_rel" "_turn_v_out_rel"
    "_warm_j_expl_rel" "_wet_j_expl_rel" "_windy_j_expl_rel"
    "_be_v_itcleft_rel" "_acceptable_j_rel" "_available_j_rel" "_clear_j_rel"
    "_complicated_j_rel" "_crucial_j_rel" "_difficult_j_rel" "_dreadful_j_rel"
    "_easier_j_rel" "_easiest_j_rel" "_easy_j_rel" "_emphasized_j_rel"
    "_encouraging_j_rel" "_good_j_rel" "_hard_j_rel" "_horrendous_j_rel"
    "_horrible_j_rel" "_imperative_j_rel" "_important_j_rel"
    "_impossible_j_rel" "_incredible_j_rel" "_interesting_j_rel"
    "_liberating_j_rel" "_likely_j_rel" "_lucky_j_rel" "_necessary_j_rel"
    "_nice_j_rel" "_obvious_j_rel" "_okay_j_accept_rel" "_optional_j_rel"
    "_possible_j_rel" "_profitable_j_rel" "_promising_j_rel"
    "_reasonable_j_rel" "_relieving_j_rel" "_remarkable_j_rel"
    "_satisfactory_j_rel" "_stupendous_j_rel" "_suffice_v_rel" "_tough_j_rel"
    "_true_j_rel" "_unlikely_j_rel" "_unnecessary_j_rel"
    "_unsatisfactory_j_rel" "_urgent_j_rel" "_vital_j_rel" "_check_v_1_rel"
    "_decide_v_1_rel" "_determine_v_rel" "_discover_v_rel" "_forget_v_1_rel"
    "_guarantee_v_rel" "_guess_v_rel" "_imagine_v_rel" "_investigate_v_rel"
    "_know_v_1_rel" "_learn_v_rel" "_recall_v_rel" "_remember_v_rel"
    "_see_v_understand_rel" "_tell_v_1_rel" "_verify_v_rel" "_wonder_v_rel"
    "_acknowledge_v_rel" "_admit_v_rel" "_assume_v_rel" "_believe_v_1_rel"
    "_claim_v_rel" "_complain_v_1_rel" "_conclude_v_rel" "_declare_v_rel"
    "_demand_v_rel" "_deny_v_rel" "_desire_v_rel" "_emphasize_v_rel"
    "_ensure_v_rel" "_exhibit_v_rel" "_feel_v_1_rel" "_forecast_v_rel"
    "_fret_v_rel" "_hope_v_1_rel" "_maintain_v_rel" "_mean_v_rel"
    "_note_v_1_rel" "_predict_v_rel" "_presume_v_rel" "_pretend_v_rel"
    "_promise_v_rel" "_propose_v_rel" "_realize_v_rel" "_regret_v_rel"
    "_report_v_1_rel" "_sense_v_rel" "_stress_v_rel" "_suspect_v_rel"
    "_think_v_1_rel" "_trust_v_rel" "_wish_v_rel" "_argue_v_1_rel"
    "_insist_v_1_rel" "_joke_v_rel" "_reckon_v_1_rel" "_remark_v_rel"
    "_suppose_v_rel" "_find_v_1_rel" "_suppose_v_rel" "_agree_v_1_rel"
    "_confirm_v_rel" "_explain_v_rel" "_reconfirm_v_rel" "_reply_v_rel"
    "_say_v_1_rel" "_suggest_v_rel" "_swear_v_rel" "_understand_v_by_rel"
    "_write_v_1_rel" "_figure_v_out_rel" "_find_v_out_rel"
    "_point_v_out_rel"))
