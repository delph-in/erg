(in-package :lkb)
;;; LinGO big grammar specific functions


(defun alphanumeric-or-extended-p (char)
  (and (graphic-char-p char)
       (not (member char '(#\space #\! #\" #\$ #\& #\' #\(
                           #\) #\* #\+ #\, #\- #\. #\/ #\: #\;
                           #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^
                           #\_ #\` #\{ #\| #\} #\~)))))

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
    (extracomp 200)
    (extrasubj_f 300)
    (extrasubj_i 300)
    (fillhead_non_wh 150)
    (fillhead_wh_r 150)
    (fillhead_wh_subj_r 150)
    (fillhead_wh_nr_f 150)
    (fillhead_wh_nr_i 150)
    (fillhead_rel 100)
    (freerel 100)
    (hoptcomp 200)
    (rootgap_l 100)
    (rootgap_r 100)
    (n_n_cmpnd 250)
    (nadj_rc 400)    
    (nadj_rr 700)    
    (adjh_i 350)
    (mid_coord_np 800)
    (top_coord_np 700)
    (hcomp 800)
    (hadj_i_uns 450)
    (hadj_s 400)
    (hadj_ivx_uns 150)
    (bare_np 300)
    (fin_non_wh_rel 200)
    (inf_non_wh_rel 100)
    (vpellipsis_ref_lr 100)
    (vpellipsis_expl_lr 100)
    (taglr 300)
    (vgering 100)
    (numadj_np 100)
    (measure_np 200)
    (appos 200)
    (imper 300)
    (sailr 300)
    (advadd 700)
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

(defparameter *unlikely-le-types* '(letter_name_le
				    n_mealtime_le 
				    n_adv_le 
				    p_subconj_inf_le 
				    vc_there_is_le 
				    vc_there_are_le
				    vc_there_was_le 
				    vc_there_were_le
				    adv_int_vp_post_le 
				    n_freerel_pro_le
				    v_sorb_le 
				    det_part_one_le 
				    adv_disc_like_le 
				    p_cp_le
				    v_obj_equi_non_trans_prd_le
                                    v_subj_equi_prd_le
                                    v_obj_equi_prd_le
                                    n_adv_le
				    comp_to_prop_elided_le
				    comp_to_nonprop_elided_le
                                    n_hour_prep_le
	  			    n_pers_pro_noagr_le 
                                    n_proper_abb_le
                                    v_np_trans_lowprio_le
				    ))

(defparameter *likely-le-types* '(conj_complex_le 
				  adv_disc_le 
				  va_quasimodal_le 
				  v_poss_le
				  n_hour_le 
				  p_ditrans_le
				  v_expl_it_subj_like_le
                                  adv_s_pre_word_nospec_le
				  adj_more_le 
				  v_subj_equi_le
				  n_proper_le 
				  v_prep_particle_np_le
				  n_wh_pro_le
                                  v_empty_prep*_intrans_le
                                  v_empty_prep_intrans_le
				  comp_to_nonprop_le
				  conj_and_num_le
				  adj_complemented_unspecified_card_le
				  adj_reg_equi_le
                                  v_np_prep_trans_le
                                  v_np_prep*_trans_le
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
  (let* ((key (existing-dag-at-end-of dag '(inflected))))
    (and key (bool-value-true key))))


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
