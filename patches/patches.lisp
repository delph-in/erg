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

;; End TSDB changes

#|
;; In /usr/local/page2.3/src/nutshell/protocols/call-eng-scanner.lisp
;; Corrected top level scanner call to eliminate punctuation.

(in-package "MAIN")

(defmethod call-component ((cpu controller)
			   (target eng-scanner)
			   &key direction)
  (declare (ignore direction))
  (let ((in (input-stream target)))
    (if (setf (output-stream target)
	  (strings-to-typed-items 
	     (remove-if #'(lambda (x)
			    (unless (consp x)
			      (member (char x 0) %special-chars% 
				      :test #'char-equal)))
			(eng-scan-input in t nil))))
	:forward
      :backward))
  nil)
|#

; Redefine paths now that we've made parse-node labels contain the whole sign.

(in-package "TREES")

(setf *recursive-path* '(DISCO::SYNSEM DISCO::NON-LOCAL DISCO::SLASH DISCO::LIST DISCO::FIRST))
(setf *local-path* '(DISCO:: SYNSEM DISCO::LOCAL))
(setf *label-fs-path* '())

; In pagemrs.lisp
; Changed GET-PARSE-FS to allow the input to already be a list of fs's
(in-package "MRS")

(defun get-parse-fs (parse)
  (if (eq (type-of parse) 'csli-unify::fs) 
      parse
    (if (string-equal "1" (subseq user::*page-version* 0 1))
	(lexicon::cfs-fs (pg::u-item-cfs parse))
      (if (or (eq (type-of parse) 'typed-item)
	      (eq (type-of parse) 'main::typed-item))
	  (lexicon::cfs-fs (car (main::typed-item-args parse)))
	(cfs-fs (pg::combo-item-cfs parse))))))

; In mrsfns.lisp
; Changed GET-MRS-STRINGS to check for flag *raw-mrs-output-p*

(defun get-mrs-strings (parse-list)
  (loop for parse in parse-list
        collecting
        (let* ((fs (get-parse-fs parse))
               (sem-fs (path-value fs *initial-semantics-path*)))
          (if (is-valid-fs sem-fs)
              (let ((mrs-struct (sort-mrs-struct (construct-mrs sem-fs))))
                (with-output-to-string (stream) 
		  (if *raw-mrs-output-p*
		      (format stream "~%~S" mrs-struct)
		    (output-mrs1 mrs-struct 'simple stream))
		  ))))))

; In mrs-to-vit.lisp
; Added check for empty list - was breaking on "how besides"

(defun vit-bind-inst-rel-p (mrsrel)
  (or (when (rel-flist mrsrel)
	(vit-quant-rel-p mrsrel)
      (when (rel-flist mrsrel)
	(nonquantified-var-p (fvpair-value (first (rel-flist mrsrel)))))
      (member (rel-sort mrsrel) *top-level-rel-types*))))

;; In ~page2.3/src/engmorph/english-morphology
;; Changed STRIP-S to match LKB's treatment of "DOES", "GOES"

(in-package "CSLI")
(defun Strip-S (word-string)
  (declare (type simple-string word-string))
  (let* ((len (length word-string))
	 (second (when (> len 1)
		   (schar word-string (the fixnum (- len 2)))))
	 (third  (when (> len 2)
		   (schar word-string (the fixnum (- len 3)))))
	 (fourth (when (> len 3)
		   (schar word-string (the fixnum (- len 4)))))
	 (alt-root-string (when (and (Ends-In word-string "S")
				   (not (Ends-In word-string "SS")))
			  (Strip-Ending word-string "S")))
	 root-string)	
    (declare (type fixnum len))
    (declare (type (or string-char null) second))
    (declare (type (or string-char null) third))
    (declare (type (or string-char null) fourth))
    (setf root-string
      (cond ((and (> len 4)
		  (Ends-In word-string "IES")
		  (not (member fourth *english-vowels* :test #'eql)))
	     (Add-Ending (Strip-Ending word-string "IES") "Y"))
	    ((and (eql #\E second)
		  (or (and (not (member fourth *english-vowels* :test #'eql))
			   (eql third #\H))
		      (member third '(#\X #\S) :test #'eql)))
	     (Strip-Ending word-string "ES"))
	    (t ())))
    (Make-Stripped-Result word-string root-string alt-root-string "-S")))

(in-package "TDL")

; Required for munger for abstract types
(in-package "MRS")

(defun compatible-types (type1 type2)
  (let ((type1name (fetch-and-expand-type type1))
	(type2name (fetch-and-expand-type type2)))
    (when (and type1name type2name)
      (not (eq (csli-unify::unify-types type1name type2name)
	       'CSLI-UNIFY::*FAIL*)))))

(defun is-valid-type (type)
  (tdl::get-infon (intern type lex::*lex-package*) lex::*lex-package* :avms))

(defun fetch-and-expand-type (name &optional (domain *lex-package*))
  (let* ((domain (string domain))
         (name (intern name domain))
         (infon (tdl::get-infon name domain :avms)))
    (when infon
      (tdl::expand-type name :domain domain)
      (lex::convert 
       (tdl::feature-structure-term (tdl::get-prototype name domain :avms))
       user::*unifier*))))

(load "~/grammar/patches/mrsmunge.fasl")


(in-package "TDL")
