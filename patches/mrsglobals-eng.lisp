;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   $RCSfile$
;;  $Revision$
;;      $Date$
;;     Author: Walter Kasper (DFKI)
;;    Purpose: 
;;   Language: Allegro Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Log$
;; Revision 1.8  1998/06/22 21:19:21  danf
;; LKB-consistent grammar with coverage equivalent to PAGE 2.1 version
;;
;; Revision 1.7  1998/05/01 07:47:21  dan
;; Grand release with all parses for test suite
;;
;; Revision 1.6  1998/03/12 22:12:29  dan
;; More debugging of lexical threading, and further clean-up of well-formedness for types.
;;
;; Revision 1.5  1998/01/09 23:41:49  dan
;; Minor changes for Verbmobil
;;
;; Revision 1.4  1998/01/07 21:25:32  dan
;; Further debugging of SLASH amalgamation, and Verbmobil extensions
;;
;; Revision 1.3  1997/12/18 18:23:36  dan
;; Repairs to trees and MRS printing for tsdb machinery.
;;
;; Revision 1.3  1997/12/16 01:16:36  dan
;; Returned MOD to its rightful status as HEAD feature
;;
;; Revision 1.2  1997/12/11 05:41:23  malouf
;; Fix amalgamation bugs.
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MRS")

(setf *MRS-RESULTS-CHECK* nil)

(setf *MRS-FOR-LANGUAGE* 'english)

(setf *MRS-SCOPING* nil)

(setf *initial-semantics-path* 
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT")))

;;; DPF - Added more value-feats

(setf *value-feats* `( ,(vsym "MONTH") ,(vsym "DAY") ,(vsym "HOUR") 
		       ,(vsym "MINUTE") ,(vsym "YEAR") ,(vsym "SEASON") 
                       ,(vsym "ORD") ,(vsym "I-ORD") ,(vsym "CONST_VALUE") 
                       ,(vsym "NAMED") ,(vsym "EXCL")))

(setf *feat-priority-list*  
  `( ,(vsym "TOP") ,(vsym "HANDEL") ,(vsym "INDEX") ,(vsym "EVENT") 
     ,(vsym "INST") ,(vsym "ACT") ,(vsym "BV") ,(vsym "RESTR") 
     ,(vsym "SOA") ,(vsym "SCOPE") ,(vsym "QUANT") ,(vsym "XARG") 
     ,(vsym "CONST_VALUE")))

(setf *psoa-top-h-path* 
  `(,(vsym "TOP-H")))

(setf *psoa-handel-path* 
  `(,(vsym "TOP")))

(setf *rel-handel-path* 
  `(,(vsym "HANDEL")))

(setf *psoa-event-path* 
  `(,(vsym "INDEX")))

(setf *psoa-liszt-path* 
    `(,(vsym "LISZT") ,(vsym "LIST")))

(setf *psoa-rh-cons-path*
    `(,(vsym "H-CONS") ,(vsym "LIST")))

(setf *psoa-h-cons-path*
    `(,(vsym "H-CONS") ,(vsym "LIST")))

(setf *psoa-constr-path* 
  `(,(vsym "SC-ARG")))

(setf *liszt-first-path* 
  `(,(vsym "FIRST")))

(setf *liszt-rest-path* 
  `(,(vsym "REST")))

;;; WK: I copy the *VM-arg-roles ... parameters from main-package into MRS
;; they appear to work without PAGE then
(setf main::*VM-arg-roles-only-p* t)
(setf main::*VM-arg-roles* `(,(vsym "arg1") ,(vsym "arg2") ,(vsym "arg3")))
(setf main::*suppressed-VM-arg-roles* 
    `(,(vsym "act") ,(vsym "und") ,(vsym "fig") ,(vsym "gnd") 
      ,(vsym "i-ord") ,(vsym "preparg") 
      ,(vsym "thm") ,(vsym "id1") 
      ,(vsym "id2") ,(vsym "params") ,(vsym "nprep") 
      ,(vsym "nomarg") ,(vsym "expr") ,(vsym "carg") ,(vsym "varg")))

(setf *do-not-convert-sort-list* nil)
                                  
(setf *relation-extra-feats* `(,(vsym "PNG") ,(vsym "PN") 
                               ,(vsym "VITTENSE") ,(vsym "VITMOOD")
                               ,(vsym "VIT")
			       ,(vsym "PRONTYPE")
			       ,(vsym "SPTYPE")
                               ,(vsym "VREF") ,(vsym "VTYPE") 
                               ,(vsym "FUN")))

(setf *complex-extra-feats* `(,(vsym "VIT") ,(vsym "PNG")))

(setf *vit-sort-feature* (vsym "SORT"))

(setf *index-feature-transform-table*
  `((,(vsym "SORT") vit-sorts
                 (,(vsym "*SORT*"))
                 (,(vsym "*TOP*"))
                 (t vit_sort))
    (,(vsym "PN") vit-syntax 
     (,(vsym "2PER") 
      (vit_person 2))
     (,(vsym "3SG") 
      (vit_number sg)
      (vit_person 3))
     (,(vsym "2SG")
      (vit_number sg)
      (vit_person 2))
     (,(vsym "1SG")
      (vit_number sg)
      (vit_person 1))
     (,(vsym "3PL")
      (vit_number pl)
      (vit_person 3))
     (,(vsym "2PL")
      (vit_number pl)
      (vit_person 2))
     (,(vsym "1PL")
      (vit_number pl)
      (vit_person 1))
     (,(vsym "3SG*")
      (vit_number sg)
      (vit_person 3))
     (,(vsym "2SG*")
      (vit_number sg)
      (vit_person 2))
     (,(vsym "1SG*")
      (vit_number sg)
      (vit_person 1))
     (,(vsym "3PL*")
      (vit_number pl)
      (vit_person 3))
     (,(vsym "2PL*")
      (vit_number pl)
      (vit_person 2))
     (,(vsym "1PL*")
      (vit_number pl)
      (vit_person 1))
     ((:AND ,(vsym "3SG*") ,(vsym "STRICT_PN"))
      (vit_number sg)
      (vit_person 3))
     ((:AND ,(vsym "2SG*") ,(vsym "STRICT_PN"))
      (vit_number sg)
      (vit_person 2))
     ((:AND ,(vsym "1SG*") ,(vsym "STRICT_PN"))
      (vit_number sg)
      (vit_person 1))
     ((:AND ,(vsym "3PL*") ,(vsym "STRICT_PN"))
      (vit_number pl)
      (vit_person 3))
     ((:AND ,(vsym "2PL*") ,(vsym "STRICT_PN"))
      (vit_number pl)
      (vit_person 2))
     ((:AND ,(vsym "1PL*") ,(vsym "STRICT_PN"))
      (vit_number pl)
      (vit_person 1)))

    (,(vsym "GEN") vit-syntax
      (,(vsym "MASC") (vit_gender masc))
      (,(vsym "FEM") (vit_gender fem))
      (,(vsym "NEUT") (vit_gender neut))
      (,(vsym "MASC*") (vit_gender masc))
      (,(vsym "FEM*") (vit_gender fem))
      (,(vsym "NEUT*") (vit_gender neut)))
    (,(vsym "PRONTYPE") vit-discourse
     (,(vsym "STD_1SG") (vit_prontype sp std))
     (,(vsym "STD_1PL") (vit_prontype sp_he std))
     (,(vsym "STD_2") (vit_prontype he std))
     (,(vsym "STD_3") (vit_prontype third std))
     (,(vsym "REFL") (vit_prontype third refl))
     (,(vsym "RECIP") (vit_prontype third recip))
     (,(vsym "IMPERS") (vit_prontype third imp))
     (,(vsym "DEMON") (vit_prontype third demon))
     (,(vsym "ZERO_PRON") (vit_prontype top zero)))
    (,(vsym "VITTENSE") vit-tenseandaspect
     (,(vsym "PRESENT") (vit_tense pres) (vit_perf nonperf))
     (,(vsym "PRESENT*") (vit_tense pres) (vit_perf nonperf))
     (,(vsym "PAST") (vit_tense past) (vit_perf nonperf))
     (,(vsym "PAST*") (vit_tense past) (vit_perf nonperf))
     (,(vsym "FUTURE") (vit_tense future) (vit_perf nonperf))
     (,(vsym "FUTURE*") (vit_tense future) (vit_perf nonperf))
     (,(vsym "PRESPERF") (vit_tense pres) (vit_perf perf))
     (,(vsym "PRESPERF*") (vit_tense pres) (vit_perf perf))
     (,(vsym "PASTPERF") (vit_tense past) (vit_perf perf))
     (,(vsym "PASTPERF*") (vit_tense past) (vit_perf perf))
     (,(vsym "*SORT*") (vit_perf nonperf))
     (,(vsym "TENSE") (vit_perf nonperf))
     (,(vsym "BSE") (vit_perf nonperf))
     (,(vsym "IMP_VFORM") (vit_perf nonperf))
     (,(vsym "FIN") (vit_tense pres) (vit_perf nonperf)))

    (,(vsym "VITMOOD") vit-tenseandaspect
     ((:AND ,(vsym "INDICATIVE*") ,(vsym "STRICT_MOOD")) (vit_mood ind))
     ((:AND ,(vsym "MODAL_SUBJ*") ,(vsym "STRICT_MOOD")) (vit_mood ind))
     ((:AND ,(vsym "IND_OR_MOD_SUBJ") ,(vsym "STRICT_MOOD")) (vit_mood imp))
     ((:AND ,(vsym "STRICT_MOOD") ,(vsym "WOULD_SUBJ*")) (vit_mood conj))
     (,(vsym "INDICATIVE") (vit_mood ind))
     (,(vsym "MODAL_SUBJ") (vit_mood ind))
     (,(vsym "WOULD_SUBJ") (vit_mood conj))
     (,(vsym "SUBJUNCTIVE") (vit_mood conj))
     (,(vsym "IND_OR_MOD_SUBJ") (vit_mood imp))
     )))

;;; this is very tentative

(setf *mrs-arg-features* `((,(vsym "arg1") . ARG1) 
                           (,(vsym "arg2") . ARG2)
                           (,(vsym "arg3") . ARG3)
			   (,(vsym "dim") . DIM)))

;; These roles are pulled out of the predicate Parsons-style, but do not include
;; the predicate's instance variable in the new role-predicate.

(setf *no-inst-arg-roles* `(DIM))

(setf *sem-relation-suffix* "_rel")

(setf *sem-relation-prefix* "_")

(setf *relation-type-check* 
  `((,(vsym "dir_rel") vit-discourse (vit_dir yes))
    (,(vsym "prep_rel") vit-discourse (vit_dir no))
    (,(vsym "poss_rel") vit-discourse (vit_dir no))
    (,(vsym "meas_adj_rel") vit-discourse (vit_dir no))
    (,(vsym "unspec_rel") vit-discourse (vit_dir no))
    ))

(setf *top-level-rel-types* 
  `(,(vsym "pron_rel") ,(vsym "mofy_rel") ,(vsym "the_afternoon_rel")
    ,(vsym "the_morning_rel") ,(vsym "the_evening_rel")
    ,(vsym "numbered_hour_rel") ,(vsym "minute_rel") ,(vsym "dofw_rel")
    ,(vsym "named_rel") ,(vsym "_vacation_rel") ,(vsym "holiday_rel")
    ,(vsym "ctime_rel") ,(vsym "_hour_rel") ,(vsym "_minute_rel") 
    ,(vsym "dim_rel") ,(vsym "unspec_rel") ,(vsym "recip_pro_rel") 
    ,(vsym "_the_day_after_rel") ,(vsym "dofm_rel")
    ,(vsym "_abroad_rel") ,(vsym "_afterward_rel") 
    ,(vsym "_afterwards_rel") ,(vsym "_ahead_rel") 
    ,(vsym "_all_day_rel") ,(vsym "_anytime_rel") 
    ,(vsym "_as_soon_as_possible_rel") ,(vsym "_aside_rel")
    ,(vsym "_astray_rel") ,(vsym "_away_rel") 
    ,(vsym "_back_adv_rel") ,(vsym "_backward_rel") 
    ,(vsym "_backwards_rel") ,(vsym "_beforehand_rel") 
    ,(vsym "_forth_rel") ,(vsym "_forward_rel") 
    ,(vsym "_forwards_rel") ,(vsym "_here_rel") 
    ,(vsym "_hither_rel") ,(vsym "_home_loc_rel") 
    ,(vsym "_last_time_rel") ,(vsym "_maximum_adv_rel") 
    ,(vsym "_nearby_rel") ,(vsym "_now_rel") 
    ,(vsym "_out_of_town_rel") ,(vsym "_right_away_rel") 
    ,(vsym "_right_now_rel") ,(vsym "_sometime_rel") 
    ,(vsym "_somewhere_rel") ,(vsym "_then_temp_rel") 
    ,(vsym "_there_rel") ,(vsym "_thereabouts_rel") 
    ,(vsym "_upstairs_rel")
    ))

(setf *vm-special-label-hack-list* nil)

;  '((,(vsym "nominalize_rel . 2))

