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
    (unless (and daughter1 (not (eql daughter1 'no-way-through)))
      (cerror "Ignore it" "Rule without daughter"))
    (append (list nil '(ARGS FIRST))
            (if (and daughter2 (not (eql daughter2 'no-way-through)))
                (list '(ARGS REST FIRST)))
            (if (and daughter3 (not (eql daughter3 'no-way-through)))
                (if (and daughter2 (not (eql daughter2 'no-way-through)))
                    (list '(ARGS REST REST FIRST)))))))

(defun spelling-change-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which affects spelling and
;;; which should therefore only be applied by the morphology
;;; system.  
;;; Old test was for something which was a subtype of
;;; *morph-rule-type* - this tests for 
;;; < NEEDS-AFFIX > = +
;;; in the rule
  (let ((affix (get-dag-value (tdfs-indef 
                               (rule-full-fs rule)) 'needs-affix)))
    (and affix (equal (type-of-fs affix) '(+)))))

(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
;;; this version tests for 
;;; < PRODUCTIVE > = -
;;; in the rule
  (let ((affix (get-dag-value 
                (tdfs-indef (rule-full-fs rule)) 'productive)))
    (and affix (equal (type-of-fs affix) '(-)))))
             

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
    (for orth-value in (split-into-words orth)
         do
         (let ((opath (create-path-from-feature-list 
                       (append tmp-orth-path *list-head*))))
           (push (make-unification :lhs opath                    
                                   :rhs
                                   (make-u-value 
                                    :types (list orth-value)))
                 unifs)
           (setq tmp-orth-path (append tmp-orth-path *list-tail*))))
    (let ((indef (process-unifications unifs)))
      (when indef
        (setf indef (create-wffs indef))
        (make-tdfs :indef indef)))))

;; Assign priorities to parser tasks
(defun rule-priority (rule)
  (case (rule-id rule)
    (extradj_i 100)
    (extradj_s 150)
    (extracomp 200)
    (extrasubj_f 300)
    (extrasubj_i 300)
    (fillhead_non_wh 150)
    (fillhead_wh_r 150)
    (fillhead_wh_subj_r 150)
    (fillhead_wh_nr_f 150)
    (fillhead_wh_nr_i 150)
    (fillhead_rel 100)
    (hoptcomp 200)
    (rootgap_l 100)
    (rootgap_r 100)
    (n_n_cmpnd 250)
    (nadj_i 400)    
    (adjh_i 350)
    (mid_coord_np 800)
    (top_coord_np 700)
    (hcomp 800)
    (hadj_s 400)
    (bare_np 300)
    (fin_non_wh_rel 200)
    (inf_non_wh_rel 100)
    (vpellipsis_lr 100)
    (taglr 100)
    (vgering 100)
    (temp_np 300)
    (numadj_np 200)
    (appos 200)
    (imper 300)
    (sailr 300)
    (advadd 300)
    (passive 400)
    (intransng 200)
    (transng 400)
    (monthdet 400)
    (weekdaydet 400)
    (monthunsat 400)
    (attr_adj 400)
    (partitive 400)
    (NP_part_lr 400)
    (dative_lr 400)
    (otherwise 
     (if (get-lex-rule-entry (rule-id rule))
	 400
       500))))

(defun gen-rule-priority (rule)
  (rule-priority rule))

(defparameter *unlikely-le-types* '(DISC_ADV_MLE1 LETTER_NAME_LE
				    MEALTIME_WORD_LE NUMADJ_NOUN_WORD_LE
				    NP_ADV_LE SUBCONJ_INF_LE 
				    SUBCONJ_INF_3MLE2
				    BE_TH_COP_IS_LE BE_TH_COP_ARE_LE
				    BE_TH_COP_WAS_LE BE_TH_COP_WERE_LE
				    INT_VP_ADV_POST_WORD_LE 
				    FREEREL_PRO_NP_WORD_LE
				    MV_SORB_LE PDET_ONE_LE 
				    DISC_ADV_LIKE_LE PREP_CP_LE
				    MV_OBJ_EQUI_NON_TRANS_PRD_LE
				    FREEREL_PRO_NP_WORD_LE
                                    MV_SUBJ_EQUI_PRD_LE
                                    MV_OBJ_EQUI_PRD_LE
                                    COORD_C_MLE
                                    NP_ADV_LE
				    ORDINAL_ADJ_LE
				    TO_C_PROP_ELIDED_LE
				    TO_C_NONPROP_ELIDED_LE
				    ))
(defparameter *likely-le-types* '(COORD_C_LE DISC_ADV_LE 
				  QUASIMODAL_LE MV_POSS_LE
				  HOUR_WORD_LE DITRANS_PREP_LE
				  MV_EXPL_IT_SUBJ_LIKE_LE
                                  S_ADV_PRE_WORD_NOSPEC_LE
				  MORE_ADJ_LE MV_SUBJ_EQUI_LE
				  PROPER_LE MV_PREP_PARTICLE_NP_LE
				  WH_PRO_WORD_LE
                                  MV_EMPTY_PREP*_INTRANS_LE
                                  MV_EMPTY_PREP_INTRANS_LE
				  TO_C_NONPROP_LE
				  HOW_ABOUT_N_OR_P_MLE1
				  AND_NUM_LE
				  COMPLEMENTED_UNSPECIFIED_CARD_LE
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
          (if (and version (boundp version))
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
(defun dag-inflected-p (dag)           
  (let* ((key (existing-dag-at-end-of dag 'inflected))
         (type (and (dag-p key) (dag-type key))))
    (when type
      (or (eq type '+) (and (consp type) (eq (first type) '+))))))


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
            (or (eq type 'bool)
                (and (consp type) (eq (first type) 'bool)))
          (format *lkb-background-stream* "~%INFLECTED unset on ~A" id))))))


(setf *grammar-specific-batch-check-fn* #'lex-check-lingo)
