; From ~/page/src/lexicon/rule-filter.lisp

(in-package "PARSING")

(setf *unlikely-le-types* '(DISCO::DISC_ADV_MLE1 DISCO::LETTER_NAME_LE
			    DISCO::MEALTIME_WORD_LE ))
(setf *likely-le-types* '(DISCO::COORD_C_LE))


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
	       400)
	;; Otherwise assign same preference to each lex entry (for now)
	      (t 600))
      (case (intern (get-item-rule-name rule-item) "DISCO")
	(disco::extradj_i 100)
	(disco::extradj_s 150)
	(disco::extracomp 200)
	(disco::extrasubj 100)
	(disco::fillhead_d 150)
	(disco::fillhead_imp 150)
	(disco::fillhead_wh_r 150)
	(disco::fillhead_wh_nr_f 150)
	(disco::fillhead_wh_nr_i 150)
	(disco::fillhead_rel 150)
	(disco::hoptcomp 200)
	(disco::rootgap_l 100)
	(disco::rootgap_r 100)
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


(in-package "LEXICON")

(defun init-filter (status-list add-rules-list)
  (let ((names-hash (make-hash-table :test #'equal))
	(max-arg-num 0)
	(rule-no 0)
	(rule-list NIL))
    (tdl-find-instances
     #'(lambda (instance)
	 (let ((status (compute-instance-status instance)))
	   (when (member status status-list)
	     (let* ((cfs (make-cfs
			  :fs (convert 
			       (feature-structure-term (prototype instance))
			       *unifier*)
			  :fc (feature-structure-funs (prototype instance))))
		    (name (string (or (get-rule-name cfs) (name instance))))
		    (arity (get-arity-and-keyarg cfs :error-p NIL)))
	       (setq max-arg-num (max max-arg-num arity))
	       (if (gethash name names-hash)
		   (warn "Two Rules With The Same Name: ~a" name)
		 (setf (gethash name names-hash) (cons rule-no arity)))
	       (push (list rule-no arity cfs) rule-list)
	       (incf rule-no))))))
    (loop for cfs in add-rules-list
	do
	  (let ((name (string (or (get-rule-name cfs) ":NONE")))
		(arity (get-arity-and-keyarg cfs :error-p NIL)))
	    (setq max-arg-num (max max-arg-num arity))
	    (if (gethash name names-hash)
		(warn "Two Rules With The Same Name: ~a" name)
	      (setf (gethash name names-hash) (cons rule-no arity)))
	    (push (list rule-no arity cfs) rule-list)
	    (incf rule-no)))
    (values
     (setf *rf-filter*
       (cons names-hash
	     (make-array (list rule-no rule-no max-arg-num)
			 :initial-element NIL)))
     rule-list)))

(in-package "MAIN")

(defun get-grammar-from-tdl ()
  (pg::initialize-lex-parser :priority-fn #'pg::csli-determine-task-priority
			     :lexicon (pg::make-lexicon))
  (pg::initialize-syn-parser :priority-fn #'pg::csli-determine-task-priority
			     :result-restrictor lex::*result-rest*)
  (setf lex::*cached-morph-types* (make-hash-table :test #'eq)))

(define-command 
  '(:load-grammar-from-tdl
    &doc 
 "Fetch TDL instances for the lexicon and parser.")
  #'get-grammar-from-tdl)

