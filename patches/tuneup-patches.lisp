; From ~/page/src/lexicon/rule-filter.lisp

(in-package "DISCO")

(defparameter pg::*unlikely-le-types* '(DISC_ADV_MLE1 LETTER_NAME_LE
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
                                        ))
(defparameter pg::*likely-le-types* '(COORD_C_LE DISC_ADV_LE 
                                      QUASIMODAL_LE MV_POSS_LE
                                      HOUR_WORD_LE DITRANS_PREP_LE
                                      MV_EXPL_IT_SUBJ_LIKE_LE
                                      S_ADV_PRE_WORD_NOSPEC_LE
                                      MORE_ADJ_LE MV_SUBJ_EQUI_LE
                                      PROPER_LE MV_PREP_PARTICLE_NP_LE
                                      WH_PRO_WORD_LE
                                      MV_EMPTY_PREP*_INTRANS_LE
                                      MV_EMPTY_PREP_INTRANS_LE
                                      ))

(defparameter pg::*unlikely-lexrule-types* '(VP_ELLIPSIS 
                                             INTRANS_NOMINAL_GERUND))

(in-package :parsing)

;; Assign a priority to a task, or return NIL if it's bound to fail

(defun csli-determine-task-priority (rule-item item tasktype parser)
  (if (eq tasktype :add-word)
      ;; Prefer EBL chunks if available
      (cond ((find-package "CHUNK") 
             (if (and (not (lex::cfs-rc (pg::combo-item-cfs  rule-item)))
                      (funcall (intern 'inside-p-new 'chunk) rule-item 
                               (eval (intern '*found-chunks* 'chunk))))
                 600
               (+ 300 (* (item-span rule-item) 200))))
            ;; Lower ranking for lex-entries of certain types
            ((and (eq (combo-item-itype rule-item) :lex-entry)
                  (member (fs-type (cfs-fs (combo-item-cfs rule-item)))
                          *unlikely-le-types*))
	       
             -200)
            ;; Higher ranking for lex-entries of certain types
            ((and (eq (combo-item-itype rule-item) :lex-entry)
                  (member (fs-type (cfs-fs (combo-item-cfs rule-item)))
                          *likely-le-types*))
             800)
            ;; Lower ranking for derived words
            ((eq (combo-item-itype rule-item) :lex-rule)
             (if (member (fs-type (cfs-fs (combo-item-cfs rule-item)))
                         *unlikely-lexrule-types*)
                 -200
               400))
            ;; Otherwise assign same preference to each lex entry (for now)
            (t 600))
    (unless (csli-useless-task-filter rule-item item parser)
      (case (intern (get-item-rule-name rule-item) "DISCO")
        (disco::extradj_i 100)
        (disco::extradj_s 150)
        (disco::extracomp 200)
        (disco::extrasubj_f 300)
        (disco::extrasubj_i 300)
        (disco::fillhead_non_wh 150)
        (disco::fillhead_wh_r 150)
        (disco::fillhead_wh_subj_r 150)
        (disco::fillhead_wh_nr_f 150)
        (disco::fillhead_wh_nr_i 150)
        (disco::fillhead_rel 100)
        (disco::hoptcomp 200)
        (disco::rootgap_l 100)
        (disco::rootgap_r 100)
        (disco::n_n_cmpnd 250)
        (disco::vpellipsis_lr 100)
        (disco::taglr 100)
        (disco::vgering 100)
        (disco::temp_np 300)
        (disco::appos 200)
        (disco::imper 300)
        (disco::sailr 300)
        (disco::advadd 300)
        (otherwise (+ 500 (* (item-span rule-item) 500)))))))


(defun csli-useless-task-filter (rule-item item parser)
  (let ((rule-name (get-item-rule-name rule-item))
        (item-rule-name (and item (get-item-rule-name item))))
    (or (not (rule-poss (funcall (parser-arg-to-fill-fn parser)
                                 rule-item parser)
                        rule-name item-rule-name))
        (not (quick-check-okay-p (combo-item-qvector rule-item)
                                 (combo-item-qvector item)))
        (and (member (intern rule-name "DISCO")  
                     '(disco::root_cl disco::frag_mod disco::frag_nomod 
                       disco::frag_subord disco::frag-excl_i
                       disco::frag-msg_i 
                       disco::fin-frag_i disco::coord-frag_i)
                     :test #'eq)
	     (or (not (zerop (item-start item)))
		 (< (item-end item) (chart-max-vertex 
				     (parser-chart parser))))))))

