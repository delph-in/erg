;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   $RCSfile$
;;  $Revision$
;;      $Date$
;;     Author: Walter Kasper (DFKI)
;;    Purpose: 
;;   Language: Allegro Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Log$
;; Revision 1.1.2.1  1997/12/16 03:13:05  malouf
;; Convert grammar to use page 2.1
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
  '(DISCO::SYNSEM DISCO::LOCAL DISCO::CONT))

;;; DPF - Added more value-feats

(setf *value-feats* '( DISCO::MONTH DISCO::DAY DISCO::HOUR DISCO::AMPM
		       DISCO::MINUTE DISCO::ORD DISCO::I-ORD DISCO::CONST_VALUE
		       DISCO::NAMED DISCO::PRED DISCO::DEMONTYPE 
		       DISCO::EXCL))

(setf *feat-priority-list*  
  '( DISCO::TOP DISCO::HANDEL DISCO::INDEX DISCO::EVENT DISCO::INST DISCO::ACT
     DISCO::BV DISCO::RESTR DISCO::SOA DISCO::SCOPE DISCO::QUANT DISCO::XARG 
     DISCO::CONST_VALUE))

(setf *psoa-top-h-path* 
  '(DISCO::TOP-H))

(setf *psoa-handel-path* 
  '(DISCO::TOP))

(setf *rel-handel-path* 
  '(DISCO::HANDEL))

(setf *psoa-event-path* 
  '(DISCO::INDEX))

(setf *psoa-liszt-path* 
    '(DISCO::RLISZT DISCO::LIST))

(setf *psoa-rh-cons-path*
    '(DISCO::RH-CONS DISCO::LIST))

(setf *psoa-h-cons-path*
    '(DISCO::RH-CONS DISCO::LIST))

(setf *psoa-constr-path* 
  '(DISCO::SC-ARG))

(setf *liszt-first-path* 
  '(DISCO::FIRST))

(setf *liszt-rest-path* 
  '(DISCO::REST))

;;; WK: I copy the *VM-arg-roles ... parameters from main-package into MRS
;; they appear to work without PAGE then
(setf main::*VM-arg-roles-only-p* t)
(setf main::*VM-arg-roles* '(disco::arg1 disco::arg2 disco::arg3))
(setf main::*suppressed-VM-arg-roles* 
    '(disco::act disco::und disco::fig disco::gnd disco::ord disco::i-ord
      disco::preparg disco::thm disco::main disco::subord
	 disco::id1 disco::id2 disco::params disco::nprep disco::varg
	 disco::nomarg disco::expr disco::carg))

(setf *do-not-convert-sort-list* '(DISCO::temp_prec_rel 
                                   DISCO::temp_over_rel))

;; WK guesses
(setf *relation-extra-feats* '(DISCO::PNG
                               DISCO::VITTENSE
                               DISCO::VITMOOD
                               DISCO::VIT
			       DISCO::PRONTYPE
			       DISCO::SPTYPE
                               DISCO::VREF DISCO::VTYPE 
                               DISCO::FUN))

(setf *vit-sort-feature* 'DISCO::SORT)

(setf *index-feature-transform-table*
  '((DISCO::SORT vit-sorts
                 (DISCO::*SORT*)
                 (DISCO::*TOP*)
                 (t vit_sort))
    (DISCO::PNG vit-syntax 
     (DISCO::3SG_M 
      (vit_gender masc)
      (vit_number sg)
      (vit_person 3))
     (DISCO::3SG_F 
      (vit_gender fem)
      (vit_number sg)
      (vit_person 3))
     (DISCO::3SG_N  
      (vit_gender neut)
      (vit_number sg)
      (vit_person 3))
      (DISCO::3SG
      (vit_number sg)
       (vit_person 3))
     (DISCO::2SG
      (vit_number sg)
      (vit_person 2))
     (DISCO::1SG
      (vit_number sg)
      (vit_person 1))
     (DISCO::3PL
      (vit_number pl)
      (vit_person 3))
     (DISCO::2PL
      (vit_number pl)
      (vit_person 2))
     (DISCO::1PL
      (vit_number pl)
      (vit_person 1)))
    (DISCO::PRONTYPE vit-discourse
     (DISCO::STD_1SG (vit_prontype sp std))
     (DISCO::STD_1PL (vit_prontype sp_he std))
     (DISCO::STD_2 (vit_prontype he std))
     (DISCO::STD_3 (vit_prontype third std))
     (DISCO::REFL (vit_prontype third refl))
     (DISCO::RECIP (vit_prontype third recip))
     (DISCO::IMPERS (vit_prontype third imp))
     (DISCO::DEMON (vit_prontype third demon))
     (DISCO::ZERO_PRON (vit_prontype top zero)))
    (DISCO::VITTENSE vit-tenseandaspect
     (DISCO::PRESENT (vit_tense pres) (vit_perf nonperf))
     (DISCO::PAST (vit_tense past) (vit_perf nonperf))
     (DISCO::FUTURE (vit_tense future) (vit_perf nonperf))
     (DISCO::PRESPERF (vit_tense pres) (vit_perf perf))
     (DISCO::PASTPERF (vit_tense past) (vit_perf perf))
     (DISCO::TENSE (vit_perf nonperf)))
    (DISCO::VITMOOD vit-tenseandaspect
     (DISCO::INDICATIVE (vit_mood ind))
     (DISCO::MODAL_SUBJ (vit_mood ind))
     (DISCO::SUBJUNCTIVE (vit_mood conj))
     (DISCO::MODAL_SUBJ (vit_mood ind))
     (DISCO::IND_OR_MOD_SUBJ (vit_mood imp))
     )))

;;; this is very tentative
(setf *mrs-arg-features* '((disco::arg1 . ARG1) 
                           (disco::arg2 . ARG2)
                           (disco::arg3 . ARG3)
			   (disco::dim  . DIM)))

(setf *sem-relation-suffix* "_rel")

(setf *sem-relation-prefix* "_")

(setf *relation-type-check* 
  '((DISCO::dir_rel vit-discourse (vit_dir yes))
    (DISCO::prep_rel vit-discourse (vit_dir no))
    (DISCO::poss_rel vit-discourse (vit_dir no))))

(setf *top-level-rel-types* 
  '(DISCO::pron_rel DISCO::mofy_rel DISCO::the_afternoon_rel
    DISCO::the_morning_rel DISCO::the_evening_rel
    DISCO::numbered_hour_rel DISCO::minute_rel DISCO::dofw_rel 
    DISCO::named_rel DISCO::_vacation_rel DISCO::holiday_rel
    DISCO::ctime_rel DISCO::_hour_rel DISCO::_minute_rel DISCO::dim_rel))

(setf *vm-special-label-hack-list*
  '((DISCO::support_rel . 1)
    (DISCO::nominalize_rel . 1)))
