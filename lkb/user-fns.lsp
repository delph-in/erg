(in-package :lkb)

;;; LinGO big grammar specific functions

(defun establish-linear-precedence (rule-fs)
   ;;;    A function which will order the features of a rule
   ;;;    to give (mother daughter1 ... daughtern)
   ;;;    
   ;;;  Modification - this must always give a feature
   ;;;  position for the mother - it can be NIL if
   ;;; necessary
  (let* ((mother NIL)
         (daughter1 (get-value-at-end-of rule-fs '(ARGS FIRST)))
         (daughter2 (get-value-at-end-of rule-fs '(ARGS REST FIRST)))
         (daughter3 (get-value-at-end-of rule-fs '(ARGS REST REST FIRST))))
    (declare (ignore mother))
    (unless daughter1 
      (cerror "Ignore it" "Rule without daughter"))
    (append (list nil '(ARGS FIRST))
            (if daughter2 
                (list '(ARGS REST FIRST)))
            (if daughter3 
                (if daughter2 
                    (list '(ARGS REST REST FIRST)))))))

(defun spelling-change-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which affects spelling and
;;; which should therefore only be applied by the morphology
;;; system.  
;;; Old test was for something which was a subtype of
;;; *morph-rule-type* - this tests for 
;;; < NEEDS-AFFIX > = + (assuming bool-value-true is default value)
;;; in the rule
  (let ((affix (get-dag-value (tdfs-indef 
                               (rule-full-fs rule)) 'needs-affix)))
    (and affix (bool-value-true affix))))

(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
;;; this version tests for 
;;; < PRODUCTIVE > = -
;;; in the rule
  (let ((affix (get-dag-value 
                (tdfs-indef (rule-full-fs rule)) 'productive)))
    (and affix (bool-value-false affix))))
             

;;; return true for types that shouldn't be displayed in type hierarchy
;;; window. Descendents (if any) will be displayed, i.e. non-displayed
;;; types are effectively spliced out

(defun hide-in-type-hierarchy-p (type-name)
  ;; starts with _, or ends with _[0-9][M]LE[0-9]
  ;; or contains "GLBTYPE"
   (and (symbolp type-name)
      (or
         ;; graphs are pretty unreadable without glbtypes in there as well
         (search "GLBTYPE" (symbol-name type-name))
         (eql (char (symbol-name type-name) 0) #\_)
         (let* ((name (symbol-name type-name))
                (end (length name))
                (cur (position #\_ name :from-end t)))
            ;; wish I had a regexp package available...
            (and cur
               (< (incf cur) end)
               (if (digit-char-p (char name cur)) (< (incf cur) end) t)
               (if (eql (char name cur) #\M) (< (incf cur) end) t)
               (if (eql (char name cur) #\L) (< (incf cur) end))
               (if (eql (char name cur) #\E) (<= (incf cur) end))
               (or (= cur end)
                   (and (digit-char-p (char name cur)) (= (incf cur) end))))))))


(defun make-orth-tdfs (orth)
  (let ((unifs nil)
        (tmp-orth-path *orth-path*))
    (loop for orth-value in (split-into-words orth)
         do
         (let ((opath (create-path-from-feature-list 
                       (append tmp-orth-path *list-head*))))
           (push (make-unification :lhs opath                    
                                   :rhs
                                   (make-u-value 
                                    :type orth-value))
                 unifs)
           (setq tmp-orth-path (append tmp-orth-path *list-tail*))))
    (let ((indef (process-unifications unifs)))
      (when indef
        (setf indef (create-wffs indef))
        (make-tdfs :indef indef)))))

;; Assign priorities to parser tasks
(defun rule-priority (rule)
  (case (rule-id rule)
    (extradj_s 150)
    (extracomp 250)
    (extrasubj_f 300)
    (extrasubj_i 300)
    (fillhead_non_wh 150)
    (fillhead_wh_r 300)
    (fillhead_wh_subj_r 150)
    (fillhead_wh_nr_f 150)
    (fillhead_wh_nr_i 150)
    (fillhead_rel 250)
    (freerel 100)
    (hoptcomp 200)
    (rootgap_l 100)
    (rootgap_r 100)
    (np_n_cmpnd 300)
    (np_n_cmpnd_2 300)
    (noun_n_cmpnd 500)
    (nadj_rc 400)
    (nadj_rr 600)
    (adjh_i 450)
    (mid_coord_np 800)
    (top_coord_np 700)
    (hcomp 800)
    (hadj_i_uns 450)
    (hadj_s 400)
    (bare_np 500)
    (bare_vger 500)
    (proper_np 800)
    (fin_non_wh_rel 200)
    (inf_non_wh_rel 100)
    (runon_s 50)
    (vpellipsis_ref_lr 100)
    (vpellipsis_expl_lr 100)
    (taglr 300)
    (vgering 350)
    (numadj_np 100)
    (measure_np 200)
    (appos 200)
    (imper 300)
    (temp_np 400)
    (sailr 300)
    (advadd 700)
    (passive 400)
    (intransng 200)
    (transng 300)
    (monthdet 400)
    (weekdaydet 400)
    (monthunsat 400)
    (attr_adj 400)
    (attr_adj_pres_part 200)
    (partitive 200)
    (NP_part_lr 400)
    ;(dative_lr 300)
    (otherwise 500)))

(defun gen-rule-priority (rule)
  (rule-priority rule))

(defparameter *unlikely-le-types* '(N_MEALTIME_LE
				    N_ADV_LE
				    VC_THERE_IS_LE
				    VC_THERE_ARE_LE
				    VC_THERE_WAS_LE
				    VC_THERE_WERE_LE
				    ADV_INT_VP_POST_LE
				    N_FREEREL_PRO_LE
                                    N_FREEREL_PRO_ADV_LE
				    V_SORB_LE
				    DET_PART_ONE_LE
				    ADV_DISC_LIKE_LE
                                    ADV_DISC_LE
				    P_CP_LE
				    V_OBJ_EQUI_NON_TRANS_PRD_LE
                                    V_SUBJ_EQUI_PRD_LE
                                    V_OBJ_EQUI_PRD_LE
                                    N_ADV_LE
				    COMP_TO_PROP_ELIDED_LE
				    COMP_TO_NONPROP_ELIDED_LE
                                    N_HOUR_PREP_LE
	  			    N_PERS_PRO_NOAGR_LE
                                    N_PROPER_ABB_LE
                                    V_NP_TRANS_LOWPRIO_LE
                                    V_NP*_TRANS_LOWPRIO_LE
                                    V_DITRANS_ONLY_LE
                                    N_DEICTIC_PRO_LE
                                    V_UNACC_LE
                                    N_INTR_LOWPRIO_LE
                                    P_NBAR_COMP_LE
                                    N_X_TO_Y_SG_LE
                                    N_X_TO_Y_PLUR_LE
				    ))

(defparameter *likely-le-types* '(CONJ_COMPLEX_LE
				  VA_QUASIMODAL_LE
				  V_POSS_LE
				  N_HOUR_LE
				  P_DITRANS_LE
				  V_EXPL_IT_SUBJ_LIKE_LE
                                  ADV_S_PRE_WORD_NOSPEC_LE
				  ADJ_MORE_LE
				  V_SUBJ_EQUI_LE
				  N_PROPER_LE
				  V_PREP_PARTICLE_NP_LE
				  N_WH_PRO_LE
                                  V_EMPTY_PREP*_INTRANS_LE
                                  N_REL_PRO_LE
                                  V_EMPTY_PREP_INTRANS_LE
				  COMP_TO_PROP_LE
				  COMP_TO_NONPROP_LE
				  CONJ_AND_NUM_LE
				  ADJ_COMPLEMENTED_UNSPECIFIED_CARD_LE
				  ADJ_REG_EQUI_LE
                                  V_NP_PREP_TRANS_LE
                                  V_NP_PREP*_TRANS_LE
                                  V_NP*_PREP_TRANS_LE
                                  P_SUBCONJ_LE
                                  V_EMPTY_PREP*_TRANS_NOSUBJ_LE
                                  N_REL_PRO_LE
                                  V_DOUBLE_PP_LE
                                  V_NP*_TRANS_PRP_NALE
                                  V_NP*_TRANS_PRES3SG_NALE
                                  V_NP*_TRANS_PAST_NALE
                                  V_NP*_TRANS_PSP_NALE
                                  ADJ_TRANS_LE
                                  V_PREP_INTRANS_UNACC_LE
                                  V_SSR_LE
                                  N_TITLE_LE
				  ))

(defun lex-priority (mrec)
  (let ((lex-type (dag-type 
		   (tdfs-indef 
		    (if (mrecord-history mrec)
			(mhistory-fs (car (mrecord-history mrec)))
		      (mrecord-fs mrec))))))
    (cond ((member lex-type *unlikely-le-types* :test #'eq)
	   200)
	  ((member lex-type *likely-le-types* :test #'eq) 
	    800)
	  (t 400))))

(defun gen-lex-priority (fs)
  (let ((lex-type (dag-type (tdfs-indef fs)))) 
    (cond ((member lex-type *unlikely-le-types* :test #'eq) -200)
	  ((member lex-type *likely-le-types* :test #'eq) 800)
	  (t 600))))


(defun set-temporary-lexicon-filenames nil
  (let* ((version (or (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user)
                      (and (find-package :lkb)
                           (find-symbol "*GRAMMAR-VERSION*" :lkb))))
         (prefix
          (if version
            (remove-if-not #'alphanumericp (symbol-value version))
            "biglex")))
    (setf *psorts-temp-file* 
      (make-pathname :name prefix 
                     :directory (pathname-directory (lkb-tmp-dir))))
    (setf *psorts-temp-index-file* 
      (make-pathname :name (concatenate 'string prefix "-index") 
                     :directory (pathname-directory (lkb-tmp-dir))))
    (setf *leaf-temp-file* 
      (make-pathname :name (concatenate 'string prefix "-rels")
                     :directory (pathname-directory (lkb-tmp-dir))))))

;;;
;;; used in lexicon compilation for systems like PET and CHiC: when we compile
;;; out the morphology, there is no point in outputting uninflected entries for
;;; systems that have no on-line morphology.  also used in [incr tsdb()] for
;;; counting of `words'.
;;;
;;; DPF 28-Aug-01 - In fact, we need uninflected forms at least for nouns in 
;;; order to analyze measure phrases like "a ten person group arrived" where
;;; the measure noun "person" is uninflected, so it can unify with the plural
;;; modifier "ten".

(defun dag-inflected-p (dag)           
  (let* ((key (existing-dag-at-end-of dag '(inflected))))
    (and key (not (bool-value-false key)))))

;;; Function to run when batch checking the lexicon

(defun lex-check-lingo (new-fs id)
  #|
  (unless (extract-infl-pos-from-fs (tdfs-indef new-fs))
  (format *lkb-background-stream* "~%No position identified for ~A" id))
  |#
  (when new-fs
    (let* ((inflbool 
           (existing-dag-at-end-of (tdfs-indef new-fs)
                                   '(inflected)))
          (type (and (dag-p inflbool) (dag-type inflbool))))
      (when type
        (when
            (eq type 'bool)
          (format *lkb-background-stream* "~%INFLECTED unset on ~A" id))))))


(setf *grammar-specific-batch-check-fn* #'lex-check-lingo)


(defun bool-value-true (fs)
  (and fs
       (let ((fs-type (type-of-fs fs)))
         (eql fs-type '+))))
  
(defun bool-value-false (fs)
  (and fs
       (let ((fs-type (type-of-fs fs)))
         (eql fs-type '-))))

#-:lui 
(defun lui-status-p (foo)
  (declare (ignore foo))
  nil)