(in-package "MAIN")

;;
;; make print method output MRS strctures to main PAGE window.
;;
(defmethod print-arg-kind ((item pg::combo-item) (kind (eql :tree))
			   t-item stream)
  
  (declare (ignore t-item))
  
  (if *ascii-trees*
      (trees::draw-parse item stream)
    (trees::fancy-draw-parse item))

  (when (and (boundp '*mrs-output-p*) *mrs-output-p*)
    (let ((*package* (find-package "MRS")))
      (mrs::extract-and-output (list item)))))

(defparameter proto-parser-fw
   '((*channel* *scanner* :stop-bw)
     (*scanner* *morphology* :stop-bw)
     ((*morphology* :parse) *lexicon* *scanner*)
     (*lexicon* *parser* *morphology*)
     (*parser* *grounding* :stop-bw)
     (*grounding* :stop-fw *channel*)))


(in-package "MRS")

; In mrsfns.lisp --- hack to suppress printing of var-extra() information

(defun get-print-name (var-struct)
  (if (var-p var-struct)
    (var-name var-struct)
    (format nil "u")))

(in-package "CSLI")

;; In engmorph/english-morphology.lisp, modify EXPAND-ENGL-INFL to remove the
;; type "bse_verb" from the null-affix collection, since we now collapse the
;; entries for bse_verb and fin-non3sg.

(defun expand-engl-infl (affix)
  (cond ((equal affix "-S") '("third_sg_fin_verb" "plur_noun"))
	((equal affix "-ED") '("past_verb" "psp_verb" "subjunctive_verb"))
	((equal affix "-ING") '("prp_verb"))
	((equal affix "-ER") '("er_comp_adj"))
	((equal affix "-EST") '("est_super_adj"))
	((equal affix "") '("non_third_sg_fin_verb" "sing_noun" 
                            "pos_adj" "no-affix"))
	(t (list affix))))


#|
;; In /usr/local/page2.3/src/nutshell/protocols/call-eng-scanner
;; Added special sentence boundary characters, to help with root types.
;;
;; 20-Sept-98 (DPF) No longer need these - now using unary rules for root types,
;; along with useless-task-filter in tuneup-patches.lisp to block root edges
;; which do not span the whole input.

(in-package "MAIN")


(defmethod call-component ((cpu controller)
			   (target eng-scanner)
			   &key direction)
  (declare (ignore direction))
  (let ((in (input-stream target)))
    (setf (direction cpu)
      (if (setf (output-stream target)
	    (strings-to-typed-items 
	     (cons "<" (nreverse 
			 (cons ">" 
			       (nreverse
				(eng-scan-input in t nil)))))))
	  :forward
	:backward)))
  nil)
|#

(in-package "FEGRAMED")

(defun close-fegramed()
  (when *FEGRAMED-IO-STREAM*
    (ignore-errors
     (pass-to-fed "quit" :wait nil :nocheck T)
     (close *FEGRAMED-IO-STREAM*))
    (setq *FEGRAMED-IO-STREAM* NIL))
  (when *FEGRAMED-PID*
    (run-process (format nil "kill -9 ~d" *FEGRAMED-PID*)
		 :output "/dev/null" :error-output "/dev/null"
		 :if-output-exists :overwrite
		 :if-error-output-exists :overwrite)
    #+:allegro(loop while (sys:os-wait t))
    (setq *FEGRAMED-PID* NIL))
  (when (streamp *FEGRAMED-ERROR-STREAM*)
    (ignore-errors
     (when (listen *FEGRAMED-ERROR-STREAM*)
       (format T "~{~A~%~}"
	       (loop for line = (read-line *FEGRAMED-ERROR-STREAM* NIL NIL)
		   while line
		   collect line)))
     (close *FEGRAMED-ERROR-STREAM*)
     (setq *FEGRAMED-ERROR-STREAM* NIL))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changes to match new TSDB machinery

(in-package "TREES")

; From trees.lisp

(defun get-labeled-bracketings (parse-item)
  (format nil "~s" (traverse-item parse-item)))

(in-package "MRS")

; From mrsfns.lisp

(defun get-vit-strings (parse)
  (when (and parse (eq (type-of parse) 'pg::combo-item))
    (let* ((fs (get-parse-fs-alt parse))
	   (sem-fs (path-value fs *initial-semantics-path*)))
      (if (is-valid-fs sem-fs)
	  (let* ((mrs-struct1 (sort-mrs-struct (construct-mrs sem-fs)))
		 (mrs-struct (if (boundp '*ordered-mrs-rule-list*)
				 (munge-mrs-struct mrs-struct1
						   *ordered-mrs-rule-list*)
			       mrs-struct1)))
	    (multiple-value-bind (vit binding-sets)
		(mrs-to-vit mrs-struct)
	      (with-output-to-string (stream) 
		(format nil "~S" (write-vit stream 
					    (horrible-hack-2 vit))))))))))

; From pagemrs.lisp

(defun get-parse-fs-alt (parse)
  (if (string-equal "1" (subseq user::*page-version* 0 1))
      (lexicon::cfs-fs (pg::u-item-cfs parse))
    (lexicon::cfs-fs (pg::combo-item-cfs parse))))

#|
(defun expand-tsdb-results (result-file dest-file &optional (vitp nil))
  (let ((sent-list
	 (tsdb::select "(parse-id i-input)" :string "(item result)" nil 
		       result-file))
	(tree-list
	 (tsdb::select "(parse-id tree)" :string "(item result)" nil 
		       result-file))
	(mrs-list
	 (tsdb::select "(parse-id mrs)" :string "(item result)" nil result-file))
	(*raw-mrs-output-p* nil))
    (with-open-file 
	(ostream dest-file :direction :output :if-exists :supersede)
    (loop for rawsent in sent-list
	  as rawtree in tree-list
          as rawmrs in mrs-list
	do (let* ((sentstr (cdar rawsent))
		  (sent (subseq sentstr (1+ (position #\@ sentstr))))
		  (treestr (cdar rawtree))
		  (tree (read-from-string 
			 (subseq treestr (1+ (position #\@ treestr)))))
		  (mrsstr (cdar rawmrs))
		  (mrs (subseq mrsstr (1+ (position #\@ mrsstr)))))
	     (format t "~%~A" sent)
	     (format ostream "~%~A~%" sent)
	     (output-parse-tree tree ostream)
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
	  (format ostream "~%~A~%" mrs)))))))
|#

;; End TSDB changes

(in-package "TDL")

