;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   $RCSfile$
;;  $Revision$
;;      $Date$
;;     Author: Walter Kasper (DFKI)
;;    Purpose: 
;;   Language: Allegro Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Log$
;; Revision 1.6  1999/01/15 05:56:49  danf
;; Improvements for generation
;;
;; Revision 1.5  1998/12/23 01:25:20  danf
;; Synchronized PAGE and LKB
;;
;; Revision 1.4  1998/10/30 01:12:04  danf
;; Lots of Verbmobil-based changes, slower processing and somewhat broader coverage
;;
;; Revision 1.3  1998/10/09 23:12:00  danf
;; Completed semantics through VIT for old VM dialogues
;;
;; Revision 1.2  1998/09/29 19:45:16  danf
;; More VM semantics and bug fixes
;;
;; Revision 1.1  1998/09/09 01:18:50  danf
;; Added files for newer MRS
;;
;; Revision 1.6  1998/09/04 00:43:30  aac
;; merging WK's changes
;;
;; Revision 1.5  1998/08/24 21:59:14  oe
;; committing minor changes contributed by the manager; make MRS work for PAGE ...
;;
;; Revision 1.4  1998/07/23 01:24:04  aac
;; mrs equality and removing remnants of page packages
;;
;; Revision 1.3  1998/07/06 01:09:08  aac
;; mostly fixes to lexical lookup for generation
;;
;; Revision 1.2  1998/06/26 02:35:27  aac
;; at least partially working VIT construction
;;
;; Revision 1.1  1998/06/24 17:15:10  aac
;; adding mrs code to source control
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
;;; AAC - corrected value-feats 
;;; feature AM-PM exists but takes ind      ,(vsym "AMPM") 
;;; I-ORD is boolean                       ,(vsym "I-ORD") 
;;; doesn't seem to be used                      ,(vsym "PRED") 
;;; doesn't seem to be used                      ,(vsym "DEMONTYPE") 


(setf *value-feats* `(                        ,(vsym "YEAR") 
                                              ,(vsym "SEASON") 
                                              ,(vsym "MONTH") 
                                              ,(vsym "DAY") 
                                              ,(vsym "HOUR") 
                                               ,(vsym "MINUTE")        
                                               ,(vsym "ORD") 
                                               ,(vsym "CONST_VALUE") 
                                               ,(vsym "NAMED") 
                                               ,(vsym "EXCL")))



(setf *feat-priority-list*  
  `( ,(vsym "TOP") ,(vsym "HANDEL") ,(vsym "INDEX") ,(vsym "EVENT") 
     ,(vsym "INST") ,(vsym "ACT") ,(vsym "BV") ,(vsym "RESTR") 
     ,(vsym "SOA") ,(vsym "SCOPE") ,(vsym "QUANT") ,(vsym "XARG") 
     ,(vsym "ARG") ,(vsym "CONST_VALUE")))

(setf *psoa-top-h-path* 
  `(,(vsym "TOP-H")))

(setf *psoa-handel-path* 
  `(,(vsym "TOP")))

(setf *key-handel-path* 
  `(,(vsym "KEY") ,(vsym "HANDEL")))

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

(setf *psoa-wgliszt-path*
  `(,(vsym "WGLISZT") ,(vsym "LIST")))

(setf *handels-feature* (vsym "WG-HNDLS"))

(setf *word-feature* (vsym "WG-WORD"))

(setf *id-feature* (vsym "WG-ID"))

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
     ((:AND ,(vsym "3SG*") ,(vsym "STRICT_SORT"))
      (vit_number sg)
      (vit_person 3))
     ((:AND ,(vsym "2SG*") ,(vsym "STRICT_SORT"))
      (vit_number sg)
      (vit_person 2))
     ((:AND ,(vsym "1SG*") ,(vsym "STRICT_SORT"))
      (vit_number sg)
      (vit_person 1))
     ((:AND ,(vsym "3PL*") ,(vsym "STRICT_SORT"))
      (vit_number pl)
      (vit_person 3))
     ((:AND ,(vsym "2PL*") ,(vsym "STRICT_SORT"))
      (vit_number pl)
      (vit_person 2))
     ((:AND ,(vsym "1PL*") ,(vsym "STRICT_SORT"))
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
     (,(vsym "BSE_ONLY") (vit_perf nonperf))
     (,(vsym "IMP_VFORM") (vit_perf nonperf))
     (,(vsym "FIN") (vit_tense pres) (vit_perf nonperf))
     (,(vsym "FIN_OR_BSE") (vit_tense pres) (vit_perf nonperf)))

    (,(vsym "VITMOOD") vit-tenseandaspect
     ((:AND ,(vsym "INDICATIVE*") ,(vsym "STRICT_SORT")) (vit_mood ind))
     ((:AND ,(vsym "MODAL_SUBJ*") ,(vsym "STRICT_SORT")) (vit_mood ind))
     ((:AND ,(vsym "MODAL_SUBJ*") ,(vsym "INDICATIVE*") 
	    ,(vsym "STRICT_SORT")) (vit_mood conj))
     ((:AND ,(vsym "INDICATIVE*") ,(vsym "MODAL_SUBJ*")
	    ,(vsym "STRICT_SORT")) (vit_mood conj))
     ((:AND ,(vsym "IND_OR_MOD_SUBJ") ,(vsym "STRICT_SORT")) (vit_mood imp))
     ((:AND ,(vsym "STRICT_SORT") ,(vsym "WOULD_SUBJ*")) (vit_mood conj))
     (,(vsym "INDICATIVE") (vit_mood ind))
     (,(vsym "INDICATIVE*") (vit_mood ind))
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
    (,(vsym "part_of_rel") vit-discourse (vit_dir no))
    (,(vsym "meas_adj_rel") vit-discourse (vit_dir no))
    (,(vsym "unspec_rel") vit-discourse (vit_dir no))
    (,(vsym "abstr_apply") vit-discourse (vit_dir no))
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

#|
(setf *vm-special-label-hack-list* 
  `((,(vsym "support_rel") ,(vsym "equal_rel"))))
|#

(setf *vm-special-label-hack-list* nil)

;;; display of extra features in an MRS

(setf *mrs-extra-display* 
  `((,(vsym "PNG") . "")
    (,(vsym "PN") . "")
    (,(vsym "VITTENSE") . "")
    (,(vsym "VITMOOD") . "")
    (,(vsym "VIT") . "")
    (,(vsym "PRONTYPE") . "")
    (,(vsym "SPTYPE") . "")
    (,(vsym "VREF") . "")
    (,(vsym "VTYPE")  . "")
    (,(vsym "FUN") . "")))


;;; features for extracting semantics from expanded lexical entries

(setf *dummy-relations* `(,(vsym "NO_REL")))

(setf *main-semantics-path* 
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT") 
    ,(vsym "LISZT") ,(vsym "LIST")))

(setf *construction-semantics-path*
  `(,(vsym "C-CONT") ,(vsym "LISZT") ,(vsym "LIST")))

(setf *external-semantics-path*
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT")
    ,(vsym "ECONT") ,(vsym "LISZT") ,(vsym "LIST")))

(setf *message-semantics-path*
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT") 
    #+(and :page :allegro-v5.0)
    tdl::MESSAGE
    #-(and :page :allegro-v5.0)
    ,(vsym "MESSAGE")
    ,(vsym "LIST")))

(setf *non-expl-ind-type* (vsym "non_expl-ind"))
(setf *non-expl-type* (vsym "non_expl"))

;;; from english.tdl
(setq *mrs-to-vit* nil)
;;; (setq main::*vm-arg-roles-only-p* nil)
(setq mrs::*raw-mrs-output-p* nil)
(setq mrs::%vit-indent% ",~%    ")
(setq mrs::*giving-demo-p* t)
