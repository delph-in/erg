; From ~/page/src/lexicon/rule-filter.lisp

(in-package "PARSING")

(setf *unlikely-le-types* '(DISCO::DISC_ADV_MLE1 DISCO::LETTER_NAME_LE
			    DISCO::MEALTIME_WORD_LE DISCO::NUMADJ_NOUN_WORD_LE
			    DISCO::NP_ADV_LE DISCO::SUBCONJ_LE 
			    DISCO::SUBCONJ_INF_LE DISCO::SUBCONJ_INF_3MLE2
			    ))
(setf *likely-le-types* '(DISCO::COORD_C_LE DISCO::DISC_ADV_LE 
			  DISCO::QUASIMODAL_LE DISCO::TO_C_PROP_LE
			  DISCO::BE_C_IS_LE DISCO::BE_C_ARE_LE
			  DISCO::BE_C_WAS_LE DISCO::BE_C_WERE_LE
			  ))
(setf *unlikely-lexrule-types* '(DISCO::VP_ELLIPSIS 
				 DISCO::INTRANS_NOMINAL_GERUND))

;; Assign a priority to a task, or return NIL if it's bound to fail

(defun csli-determine-task-priority (rule-item item tasktype parser)
  (unless (csli-useless-task-filter rule-item item parser)
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
      (case (intern (get-item-rule-name rule-item) "DISCO")
	(disco::extradj_i 100)
	(disco::extradj_s 150)
	(disco::extracomp 200)
	(disco::fillhead_d 150)
	(disco::fillhead_imp 150)
	(disco::fillhead_wh_r 150)
	(disco::fillhead_wh_nr_f 150)
	(disco::fillhead_wh_nr_i 150)
	(disco::fillhead_rel 150)
	(disco::fin_non_wh_rel 100)
	(disco::hoptcomp 200)
	(disco::rootgap_l 100)
	(disco::rootgap_r 100)
	(disco::temp_np 100)
	(disco::voc_np 100)
	(otherwise (+ 500 (* (item-span rule-item) 500)))))))

(defun csli-useless-task-filter (rule-item item parser)
  (let ((rule-name (get-item-rule-name rule-item))
	(item-rule-name (and item (get-item-rule-name item))))
    (or (not (rule-poss (funcall (parser-arg-to-fill-fn parser)
				 rule-item parser)
			rule-name item-rule-name))
	(and (member (intern rule-name "DISCO")      
		     '(disco::root_cl disco::frag_mod disco::frag_nomod 
		       disco::frag_subord disco::frag-excl_i disco::frag-msg_i 
		       disco::fin-frag_i disco::coord-frag_i)
		     :test #'eq)
	     item
	     (or (not (zerop (item-start item)))
		 (< (item-end item) (chart-max-vertex 
				     (parser-chart parser))))))))

