;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   $RCSfile$
;;  $Revision$
;;      $Date$
;;     Author: Walter Kasper (DFKI)
;;    Purpose: 
;;   Language: Allegro Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Log$
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
      disco::id1 disco::id2 disco::params disco::nprep 
      disco::nomarg disco::expr disco::carg disco::varg))

(setf *do-not-convert-sort-list* nil)

(setf *relation-extra-feats* '(DISCO::PNG
			       DISCO::PN
                               DISCO::VITTENSE
                               DISCO::VITMOOD
                               DISCO::VIT
			       DISCO::PRONTYPE
			       DISCO::SPTYPE
                               DISCO::VREF DISCO::VTYPE 
                               DISCO::FUN))

(setf *complex-extra-feats* '(DISCO::VIT DISCO::PNG))

(setf *vit-sort-feature* 'DISCO::SORT)

(setf *index-feature-transform-table*
  '((DISCO::SORT vit-sorts
                 (DISCO::*SORT*)
                 (DISCO::*TOP*)
                 (t vit_sort))
    (DISCO::PN vit-syntax 
     (DISCO::2PER 
      (vit_person 2))
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
      (vit_person 1))
     (DISCO::3SG*
      (vit_number sg)
      (vit_person 3))
     (DISCO::2SG*
      (vit_number sg)
      (vit_person 2))
     (DISCO::1SG*
      (vit_number sg)
      (vit_person 1))
     (DISCO::3PL*
      (vit_number pl)
      (vit_person 3))
     (DISCO::2PL*
      (vit_number pl)
      (vit_person 2))
     (DISCO::1PL*
      (vit_number pl)
      (vit_person 1))
     ((:AND DISCO::3SG* DISCO::STRICT_PN)
      (vit_number sg)
      (vit_person 3))
     ((:AND DISCO::2SG* DISCO::STRICT_PN)
      (vit_number sg)
      (vit_person 2))
     ((:AND DISCO::1SG* DISCO::STRICT_PN)
      (vit_number sg)
      (vit_person 1))
     ((:AND DISCO::3PL* DISCO::STRICT_PN)
      (vit_number pl)
      (vit_person 3))
     ((:AND DISCO::2PL* DISCO::STRICT_PN)
      (vit_number pl)
      (vit_person 2))
     ((:AND DISCO::1PL* DISCO::STRICT_PN)
      (vit_number pl)
      (vit_person 1)))
    (DISCO::GEN vit-syntax
      (DISCO::MASC (vit_gender masc))
      (DISCO::FEM (vit_gender fem))
      (DISCO::NEUT (vit_gender neut))
      (DISCO::MASC* (vit_gender masc))
      (DISCO::FEM* (vit_gender fem))
      (DISCO::NEUT* (vit_gender neut)))
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
     (DISCO::PRESENT* (vit_tense pres) (vit_perf nonperf))
     (DISCO::PAST (vit_tense past) (vit_perf nonperf))
     (DISCO::PAST* (vit_tense past) (vit_perf nonperf))
     (DISCO::FUTURE (vit_tense future) (vit_perf nonperf))
     (DISCO::FUTURE* (vit_tense future) (vit_perf nonperf))
     (DISCO::PRESPERF (vit_tense pres) (vit_perf perf))
     (DISCO::PRESPERF* (vit_tense pres) (vit_perf perf))
     (DISCO::PASTPERF (vit_tense past) (vit_perf perf))
     (DISCO::PASTPERF* (vit_tense past) (vit_perf perf))
     (DISCO::TENSE (vit_perf nonperf))
     (DISCO::BSE (vit_perf nonperf))
     (DISCO::FIN (vit_tense pres) (vit_perf nonperf))
      

)
    (DISCO::VITMOOD vit-tenseandaspect
     ((:AND DISCO::INDICATIVE* DISCO::STRICT_MOOD) (vit_mood ind))
     ((:AND DISCO::MODAL_SUBJ* DISCO::STRICT_MOOD) (vit_mood ind))
     ((:AND DISCO::IND_OR_MOD_SUBJ DISCO::STRICT_MOOD) (vit_mood imp))
     ((:AND DISCO::STRICT_MOOD DISCO::WOULD_SUBJ*) (vit_mood conj))
     (DISCO::INDICATIVE (vit_mood ind))
     (DISCO::MODAL_SUBJ (vit_mood ind))
     (DISCO::WOULD_SUBJ (vit_mood conj))
     (DISCO::SUBJUNCTIVE (vit_mood conj))
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
    (DISCO::poss_rel vit-discourse (vit_dir no))
    (DISCO::meas_adj_rel vit-discourse (vit_dir no))
    (DISCO::unspec_rel vit-discourse (vit_dir no))
    ))

(setf *top-level-rel-types* 
  '(DISCO::pron_rel DISCO::mofy_rel DISCO::the_afternoon_rel
    DISCO::the_morning_rel DISCO::the_evening_rel
    DISCO::numbered_hour_rel DISCO::minute_rel DISCO::dofw_rel 
    DISCO::named_rel DISCO::_vacation_rel DISCO::holiday_rel
    DISCO::ctime_rel DISCO::_hour_rel DISCO::_minute_rel DISCO::dim_rel
    DISCO::unspec_rel DISCO::recip_pro_rel DISCO::_the_day_after_rel 
    DISCO::dofm_rel

    DISCO::_abroad_rel DISCO::_afterward_rel DISCO::_afterwards_rel 
    DISCO::_ahead_rel DISCO::_all_day_rel DISCO::_anytime_rel 
    DISCO::_as_soon_as_possible_rel DISCO::_aside_rel DISCO::_astray_rel 
    DISCO::_away_rel DISCO::_back_adv_rel DISCO::_backward_rel 
    DISCO::_backwards_rel DISCO::_beforehand_rel DISCO::_forth_rel 
    DISCO::_forward_rel DISCO::_forwards_rel DISCO::_here_rel DISCO::_hither_rel
    DISCO::_home_loc_rel DISCO::_last_time_rel DISCO::_maximum_adv_rel 
    DISCO::_nearby_rel DISCO::_now_rel DISCO::_out_of_town_rel 
    DISCO::_right_away_rel DISCO::_right_now_rel DISCO::_sometime_rel 
    DISCO::_somewhere_rel DISCO::_then_temp_rel DISCO::_there_rel 
    DISCO::_thereabouts_rel DISCO::_upstairs_rel
    ))

(setf *vm-special-label-hack-list* nil)

;  '((DISCO::nominalize_rel . 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Add access function used by TSDB machinery

(defun get-mrs-strings (parse-list)
  (loop for parse in parse-list
        collecting
        (let* ((fs (get-parse-fs parse))
               (sem-fs (path-value fs *initial-semantics-path*)))
          (if (is-valid-fs sem-fs)
              (let ((mrs-struct (sort-mrs-struct (construct-mrs sem-fs))))
                (with-output-to-string (stream) 
		  (format stream "~%~S" mrs-struct)
                  ;(output-mrs1 mrs-struct 'simple stream)
		  ))))))

(defun get-mrs-resolved-strings (parse-list)
  (loop for parse in parse-list
	collecting
        (let* ((fs (get-parse-fs parse))
               (sem-fs (path-value fs *initial-semantics-path*)))
          (when (is-valid-fs sem-fs)
              (let* ((mrs-struct (sort-mrs-struct (construct-mrs sem-fs)))
		     (binding-sets (make-scoped-mrs mrs-struct)))
		(when binding-sets
		  (with-output-to-string (stream) 
		    (setf *canonical-bindings* (canonical-bindings 
						(first binding-sets)))
		    (output-scoped-mrs mrs-struct :stream stream))))))))

(defun get-parse-fs (parse)
  (if (string-equal "1" (subseq user::*page-version* 0 1))
      (lexicon::cfs-fs (pg::u-item-cfs parse))
  (lexicon::cfs-fs (car (lex::typed-item-args parse)))))

(defun get-vit-strings (parse-list)
  (loop for parse in parse-list
        collecting
	(let* ((fs (get-parse-fs parse))
               (sem-fs (path-value fs *initial-semantics-path*)))
          (if (is-valid-fs sem-fs)
              (let ((mrs-struct (sort-mrs-struct (construct-mrs sem-fs))))
		 (multiple-value-bind (vit binding-sets)
		     (mrs-to-vit mrs-struct)
		   (with-output-to-string (stream) 
		     (format nil "~S" (write-vit stream vit)))))))))

(defun expand-tsdb-results (result-file dest-file &optional (vitp nil))
  (excl::run-shell-command (format nil "sort -n < ~A | sed -f ~A > ~A" 
				   result-file
				   "~/grammar/tsdb/tsnlpsed"
				   (concatenate 'string result-file ".out")))
  (let ((old-raw-mrs main::*raw-mrs-output-p*))
    (setf main::*raw-mrs-output-p* nil)
    (with-open-file 
	(istream (concatenate 'string result-file ".out") :direction :input)
     (with-open-file 
	(ostream dest-file :direction :output :if-exists :supersede)
      (do ((sent-num (read istream nil 'eof))
	   (sent (read istream nil 'eof))
	   (mrs (read istream nil 'eof))
	   (sep (read-char istream nil 'eof))
	   (tree (read istream nil 'eof)))
	  ((eql sent-num 'eof) nil)
	(format t "~%~A" sent)
	(format ostream "~%~A~%" sent)
	(trees::kh-parse-tree tree :stream ostream)
	(if vitp
	    #|
	    (progn
	      (multiple-value-bind 
		  (vit binding-sets)
		  (mrs-to-vit mrs))
	      (write-vit-pretty t (horrible-hack-2 vit))
	      (format ostream "~%")
	      (check-vit vit))
	      |#
	    (progn
	      (format ostream "~A~%~%" mrs)
	      (finish-output ostream)
	      (check-vit mrs t ostream)
	      (format ostream "~%"))
	  (format ostream "~%~A~%" mrs))
	(setf sent-num (read istream nil 'eof)
	      sent (read istream nil 'eof)
	      mrs (read istream nil 'eof)
	      sep (read-char istream nil 'eof)
	      tree (read istream nil 'eof)))))
    (setf main::*raw-mrs-output-p* old-raw-mrs)))

(defun extract-and-output (parse-list)
 (let ((*print-circle* nil))
  (loop for parse in parse-list
        do
        (let* ((fs (get-parse-fs parse))
               (sem-fs (path-value fs *initial-semantics-path*)))
          (if (is-valid-fs sem-fs)
              (let 
                  ((mrs-struct (construct-mrs sem-fs)))
		(unless *mrs-to-vit*
		  (output-mrs mrs-struct 'simple))
                (if *mrs-to-vit*
                    (mrs-to-vit-convert mrs-struct)
                  (if *mrs-scoping-p*
                      (scope-mrs-struct mrs-struct)))
                (when *mrs-results-check*
                    (let ((sorted-mrs-struct (sort-mrs-struct mrs-struct))
			  (previous-result
                           (gethash (remove-trailing-periods
                                     main::*last-sentence*)
                                    *mrs-results-table*)))
                      (if previous-result
                         (unless (mrs-equalp sorted-mrs-struct previous-result)
                                  (when 
                                   (y-or-n-p "Differs from previous result.
                                       Replace?")
                                   (setf 
                                    (gethash
                                     (remove-trailing-periods
                                     main::*last-sentence*)
                                     *mrs-results-table*)
                                    sorted-mrs-struct)))
                        (when (y-or-n-p "No previous result.
                                       Add?")
                              (setf 
                               (gethash
                                (remove-trailing-periods
                                main::*last-sentence*) *mrs-results-table*)
                               sorted-mrs-struct)))))))))))
