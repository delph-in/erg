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
      (mrs::extract-and-output (output-stream *parser*)))))

(defparameter proto-parser-fw
   '((*channel* *scanner* :stop-bw)
     (*scanner* *morphology* :stop-bw)
     ((*morphology* :parse) *lexicon* *scanner*)
     (*lexicon* *parser* *morphology*)
     (*parser* *grounding* :stop-bw)
     (*grounding* :stop-fw *channel*)))


; in kh-tree.lisp
; Fix tree drawing so it can be directed to a stream

(in-package "TREES")

(defun KH-Parse-Tree (nodelist &key (vspace 2) (hspace 2) (stream t))
  (let ((*tree-node-id* -1)
	(*tree-node-list* nil))
    (let* ((pl   (Compute-Parse-List nodelist))
	   (tree (Create-Tree pl :vspace vspace :hspace hspace)))
      (setf *tree-node-list* (reverse *tree-node-list*))
      (Tree-Draw-TTY tree *tree-node-list* stream))))

;; In parse-tree.lisp

(defun Fast-Tree-Draw (height layout stream)
  (let ((cr (princ-to-string #\Newline))
        (result ""))
    (loop 
     for i from 0 to height do
     (let ((s (write-to-string (svref layout i) :escape nil)))
       (setf result (concatenate 'string result s cr))))
    (write result :escape nil :stream stream)))

(defun Tree-Draw-Tty (tree tree-node-list &optional (stream t))
  (let* ((height (Parse-Tree-Height tree))
	 (width (Parse-Tree-Width tree))
	 (layout (make-array (1+ height))))
    (loop 
     for i from 0 to height
     do 
     (setf 
      (svref layout i)
      (make-string (1+ width) :initial-element #\Space)))
    (loop 
     for n in (Parse-Tree-Nodes tree) 
     do (Tree-Place-Node layout n tree-node-list))
    (loop
     for e in (Parse-Tree-Edges tree)
     do (Draw-Stubs layout (first e) (second e) (third e) (fourth e)))
    (loop for n in (Parse-Tree-Nodes tree) do (Connect-Stubs layout n))
    (format stream "~%")
    (Fast-Tree-Draw height layout stream)
    (format stream "~%")
    ))

(in-package "MRS")

; In mrsfns.lisp --- hack to suppress printing of var-extra() information

(defun get-print-name (var-struct)
  (if (var-p var-struct)
    (var-name var-struct)
    (format nil "u")))

; Add munging of MRS to conform to VIT constraints

#|
; DPF 31-May-98: TEMPORARY PATCH:
; For the moment, call the munging twice, since on the first pass, the munger 
; overlooks the second occurrence of the same quantifier, for some reason.

(defun mrs-to-vit-convert (mrs-psoa &optional (standalone t))
  (if (eq *mrs-for-language* 'english)
     (let ((mrsstruct
	    (if (boundp '*ordered-mrs-rule-list*)
		(first (munge-mrs-struct 
			(first
			 (munge-mrs-struct mrs-psoa *ordered-mrs-rule-list*))
			*ordered-mrs-rule-list*))
	      mrs-psoa)))
       (multiple-value-bind 
          (vit binding-sets)
          (mrs-to-vit mrsstruct)
        (setf *canonical-bindings* nil)
        (when standalone
          (format t "~%Unscoped form")
          (output-mrs mrsstruct 'indexed)
            ;;; then try and find sets of bindings which will give a fully scoped 
            ;;; structure, and output the results
          (if binding-sets
              (format t "~%Scoped form(s)")
            (format t "~%WARNING: Invalid MRS structure"))
          (for binding in binding-sets
               do
               (setf *canonical-bindings* (canonical-bindings binding))
               (output-connected-mrs mrsstruct 'indexed)
               (output-scoped-mrs mrsstruct)))
        (when (and vit standalone)
          (write-vit-pretty t (horrible-hack-2 vit))
          (format t "~%"))
	  (check-vit vit)
          vit))
    (let ((vit (german-mrs-to-vit mrsstruct)))
      (when standalone
	(format t "~%Unscoped form")
	(output-mrs mrsstruct 'indexed))
      (when (and vit standalone)
	(write-vit-pretty t vit)
	(format t "~%"))
      vit)))
|#

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


;; In /usr/local/page2.1/licensed/src/nutshell/protocols/call-eng-scanner
;; Added special sentence boundary characters, to help with root types.

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


; In time-convert.lisp
; The ECASE operator doesn't work right in the following function, which is
; inelegantly patched as follows:

(in-package "MRS")

#|
(defun derive-am-pm-spec (sort)
  (ecase sort
    ((vsym "_MORNING_REL") 'am)
    ((vsym "_AFTERNOON_REL") 'pm)
    ((vsym "_EVENING_REL") 'pm)))
|#

(defun derive-am-pm-spec (sort)
  (cond ((eq sort (vsym "_MORNING_REL")) 'am)
	((eq sort (vsym "_AFTERNOON_REL")) 'pm)
	((eq sort (vsym "_EVENING_REL")) 'pm)))

(in-package "TDL")

(in-package "MRS")

; Changed output to be *standard-output* rather than "~/tmp/errorout" - don't
; see why it used to work with the file for output.

(defun check-vit (vit &optional (as-string nil) (stream *standard-output*))
  #+allegro
  (progn
   (with-open-file (vit-out "~/tmp/vitcheck" :direction :output
	                                    :if-exists :supersede)
    (format vit-out "ensure_loaded(vitADT).~%V = ")
    (if as-string 
	(format vit-out "~A" vit)
      (write-vit vit-out vit))
    (format vit-out ",vitCheck(V).~%~%halt.~%"))
   (excl::run-shell-command "cd /eo/e1/vm2/vitADT/lib/Vit_Adt;/opt/quintus/bin3.2/sun4-5/prolog < ~/tmp/vitcheck" :output "~/tmp/vitout" :if-output-exists :supersede :error-output "~/tmp/viterror" :if-error-output-exists :supersede)
   (excl::run-shell-command "tail +65 ~/tmp/viterror | tail -r | tail +2 | tail -r" :output stream :error-output "~/tmp/realerrorout" :if-output-exists :supersede :if-error-output-exists :supersede)
   (format stream "~%"))
  #-allegro
  (warn "function check-vit needs customising for this Lisp"))

#|
; In /usr/local/page2.3/src/tsdb/tsdb.lisp
; Fixed WRITE-RESULTS to check the intended bindings for trees-hook

(in-package "TSDB")

(defun write-results (parse-id results 
                      &optional (language *tsdb-data*)
                      &key cache)
  (let* ((items (main::output-stream main::*parser*))
         (mrss 
          (when (and *tsdb-semantix-hook* (stringp *tsdb-semantix-hook*))
            (when (find-package "MRS")
              (set (intern "*RAW-MRS-OUTPUT-P*" "MRS") nil)
              (set (intern "*RAW-MRS-OUTPUT-P*" "MAIN") nil))
            (ignore-errors
             (funcall (symbol-function (read-from-string *tsdb-semantix-hook*))
                      items))))
         ;(mrss (remove nil mrss))
         (trees 
          (when (and *tsdb-trees-hook* (stringp *tsdb-trees-hook*))
            (ignore-errors
             (funcall (symbol-function (read-from-string  *tsdb-trees-hook*))
                      items)))))
    (if (or (= (length results) (length items) (length mrss) (length trees))
            (and (not *tsdb-semantix-hook*) *tsdb-trees-hook*
                 (= (length results) (length items) (length trees)))
            (and *tsdb-semantix-hook* (not *tsdb-trees-hook*)
                 (= (length results) (length items) (length mrss)))
            (and (not *tsdb-semantix-hook*) (not *tsdb-trees-hook*)
                 (= (length results) (length items))))
      (do* ((results results (rest results))
            (result (first results) (first results))
            (trees (reverse trees) (rest trees))
            (tree (first trees) (first trees))
            (mrss (reverse mrss) (rest mrss))
            (mrs (first mrss) (first mrss)))
          ((null results))
        (write-result parse-id result tree mrs language :cache cache))
      (format 
       *tsdb-io* 
       "~&write-results(): mysterious mismatch [~d : ~d : ~d : ~d].~%"
       (length results) (length items) (length trees) (length mrss)))))
|#

(in-package "MRS")

; In mrsfns.lisp
(defun expand-tsdb-results (result-file dest-file &optional (vitp nil))
  (excl::run-shell-command (format nil "sort -n < ~A | sed -f ~A > ~A" 
				   result-file
				   "~/grammar/tsdb/tsnlpsed"
				   (concatenate 'string result-file ".out")))
  (let ((*raw-mrs-output-p* nil))
    (with-open-file 
	(istream (concatenate 'string result-file ".out") :direction :input)
     (with-open-file 
	(ostream dest-file :direction :output :if-exists :supersede)
      (do ((sent-num (read istream nil 'eof))
	   (sent (read istream nil 'eof))
	   (sep1 (read-char istream nil 'eof))
	   (tree (read istream nil 'eof))
	   (sep2 (read-char istream nil 'eof))
	   (mrs (read istream nil 'eof))
	   )
	  ((eql sent-num 'eof) nil)
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
	  (format ostream "~%~A~%" mrs))
	(setf sent-num (read istream nil 'eof)
	      sent (read istream nil 'eof)
	      sep (read-char istream nil 'eof)
	      tree (read istream nil 'eof)
	      sep (read-char istream nil 'eof)
	      mrs (read istream nil 'eof)))))))

; Also in mrsfns.lisp
(defun get-vit-strings (parse-list)
  (loop for parse in parse-list
        collecting
	(let* ((fs (get-parse-fs parse))
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
		     (format nil "~S" (write-vit stream vit)))))))))