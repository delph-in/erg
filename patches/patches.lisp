
(load (dir-and-name tdl::*patches-dir* "mrsglobals-eng"))

(excl:compile-file-if-needed (dir-and-name *patches-dir* "time-convert"))
(load (dir-and-name tdl::*patches-dir* "time-convert"))

(in-package "MAIN")

(defmethod call-printer :after ((cpu controller)
				(from parser)
				&rest keys-args)
  (declare (ignore keys-args))
  (when *trees-output-p*
    (trees::traverse-parsing-result (output-stream from)))
  (when (and (boundp '*mrs-output-p*) *mrs-output-p*)
    (funcall (symbol-function (read-from-string "mrs::extract-and-output"))
             (output-stream from))))

(in-package "PARSING")
; Allow lexical rules to apply to multi-word lexemes, e.g. to get
; "Can't Kim sleep?"

(defun get-rules-for-passive-item (passive-item parser)
  ;;; get all appropriate rules for this passive item and return them as a list
  ;;; this is for bottom up parsing only
  (case (combo-item-itype passive-item)
    (:MORPH
      (transform-instances
       ;; the result of the following function has to be a list of TDL
       ;; instances that have status :lex-entry or :c-lex-entry
       (funcall (combo-parser-lexicon-access-fn parser) passive-item parser)
       parser))
    ((:LEX-ENTRY :C-LEX-ENTRY :LEX-RULE)
     (append (combo-parser-lex-rules parser)
	     (combo-parser-syn-rules parser)))
    ((:C-LEX-ENTRY :RULE)
     (if (and (combo-parser-syn-rules parser) *RULES-FOR-CATEGORY-TABLE*)
	 (or (get-rules-by-category-info passive-item parser)
	     (combo-parser-syn-rules parser))
       (combo-parser-syn-rules parser)))))

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

; In mrs-to-vit.lisp

(defun check-vit (vit &optional (as-string nil) (stream *standard-output*))
  (with-open-file (vit-out "~/tmp/vitcheck" :direction :output
	                                    :if-exists :supersede)
    (format vit-out "ensure_loaded(vitADT).~%V = ")
    (if as-string 
	(format vit-out "~A" vit)
      (write-vit vit-out vit))
    (format vit-out ",vitCheck(V).~%~%halt.~%"))
  (excl::run-shell-command "cd /eo/e1/vm2/vitADT/lib/Vit_Adt;/opt/quintus/bin3.2/sun4-5/prolog < ~/tmp/vitcheck" :output "~/tmp/vitout" :if-output-exists :supersede :error-output "~/tmp/viterror" :if-error-output-exists :supersede)
  (excl::run-shell-command "tail +56 ~/tmp/viterror | tail -r | tail +2 | tail -r" :output stream)
  (format stream "~%"))


; Add munging of MRS to conform to VIT constraints

(defun mrs-to-vit-convert (mrs-psoa &optional (standalone t))
  (if (eq *mrs-for-language* 'english)
     (let ((mrsstruct
	    (if (boundp '*ordered-mrs-rule-list*)
		(first (munge-mrs-struct mrs-psoa *ordered-mrs-rule-list*))
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

#|
(in-package "LEXICON")

;; in lexicon/morph-interface.lisp
;; Patched expand-morph-type to let *stem-path* be either a one or a two element
;; list, though without providing a general solution to arbitrary length for 
;; *stem-path*.

(defun expand-morph-type (form type stem
			    &optional id index (class :avms))
  "This function gets a morphix result, namely a type and a stem which both
 are strings, and puts each of them into a feature structure that is
 appropriate for further use by the lexical and syntactic processing
 components. 
 If %Klaus-Oude-Grammar% is non-NIL, the stem is encoded as a list 
 If %Klaus-Oude-Grammar% is set to :ATOME, it is put into the feature
 structure as symbol in the *lex-package*, otherwise as string.
 If %Klaus-Oude-Grammar% is NIL, the stem is stored under *stem-path* as 
 string.

Input:
  form: The original form of the word
  type:  the morphix output type (a string)
  stem:  the stem of the analysed word (a string)
  index: (optional) which of the (partially) expanded types do you want ?
  class: (optional) :avms is the default

Output:
  A cfs that is the result of unifying the stem
  and the morphological information into the appropriate positions of a new
  feature structure"

  (let ((type-key (do-in-package *LEX-PACKAGE* (read-from-string type)))
	(id-string (if id (format NIL "(~a ~a)" *id-into* id) ""))
	(stem-string (if %Klaus-Oude-Grammar%
			 (if (eq %Klaus-Oude-Grammar% :ATOME)
			     (format NIL "~{[(~a ~} ~a~2:*~{)]~*~}"
				     (rest *stem-path*)
				     stem)
			   (format NIL
				   "~{ [(~a ~}[(~a ~a)(~a ~a)]~5:*~{)]~*~}"
				   (rest *stem-path*)
				   *first-in-list* stem
				   *rest-in-list* *end-of-list*))
		       (format NIL "~{[(~a ~} ~s~2:*~{)]~*~}"
			       nil
			       stem))))
    (setq type-key
      (or (gethash type *cached-morph-types*)
	  (when (tdl::get-infon type-key *LEX-PACKAGE* class)
	    (expand-type type-key :domain *LEX-PACKAGE*)
	    ;; get prototype from TDL
	    (let ((t-proto (get-prototype type-key *LEX-PACKAGE* class index)))
	      (setf (gethash type *cached-morph-types*)
		(make-cfs :fs (convert (feature-structure-term t-proto)
				       *unifier*)
			  :fc (feature-structure-funs t-proto)))))
	  (progn
	    (format t "~&damn it! better define `~a' as a type.~%" type-key)
	    (do-in-package *LEX-PACKAGE*
	      (make-cfs
	       :fs (convert
		    (build-fs-from-string (format NIL "~a" type-key))
		    *unifier*))))))
    (do-in-package *LEX-PACKAGE*
     (let* ((stem-str (string-trim '(#\space #\") stem-string))
	    (stem-expr (if (eq (length *stem-path*) 1)
			   (list (first *stem-path*) stem-str)
			 (list (first *stem-path*) 
			       (list :conj (list (second *stem-path*) 
						 stem-str))))))
      (unify
       (make-cfs
	:fs (convert
	     (udine::build-fs-from-list
	      (if id
		  `((:conj (,*affix-into* (:conj))
			   (,*form-into* ,form)
			   (,*id-into* ,id)
			   (,(first stem-expr) ,(second stem-expr))))
		`((:conj (,*affix-into* (:conj))
			 (,*form-into* ,form)
			 (,(first stem-expr) ,(second stem-expr))))))
	     *unifier*))
       type-key
       (list (intern *affix-into* *LEX-PACKAGE*)))))))
|#

(in-package "MRS")

; In mrsresolve.lisp
; Changed output-scoped-mrs so it can output to a stream.

(defun output-scoped-mrs (mrs &key (stream t))
  (format stream "~%")
  (let* ((top-handel (get-true-var-num (psoa-handel mrs)))
         (rel-list (psoa-liszt mrs)))
    (output-scoped-rels top-handel rel-list stream))
  (format stream "~%"))

(defun output-scoped-rels (top-handel rel-list stream)
  (let ((top-rels (for rel in rel-list
                        filter
                        (if (eql (get-true-var-num (rel-handel rel)) top-handel)
                          rel))))
    (loop (output-scoped-rel (car top-rels) rel-list stream)
          (if (cdr top-rels)
            (progn (format stream " /~A " #\\)
                   (setf top-rels (cdr top-rels)))
            (return)))))

(defun output-scoped-rel (rel rel-list stream)
  (let ((need-comma nil))
    (format stream "~A(" 
            (remove-right-sequence "_rel" (string-downcase (rel-sort rel))))
    (for feat-val in (rel-flist rel)
         do     
         (when need-comma (format stream ", "))
         (setf need-comma t)
         (let ((var (fvpair-value feat-val)))
           (if (listp var)
             (progn 
               (format stream "(")
               (for val in var
                    do
                    (if (is-handel-var val)
                      (output-scoped-rels (get-true-var-num val) rel-list stream)
                      (format stream "~A" (remove-variable-junk 
                                      (if (var-p val)
                                        (get-bound-var-value val)
                                        val)))))
               (format stream ")"))
             (if (is-handel-var var)
                  (output-scoped-rels (get-true-var-num var) rel-list stream)
                  (format stream "~A" (remove-variable-junk 
                                  (if (var-p var)
                                    (get-bound-var-value var)
                                    var)))))))
    (format stream ")")))

;; In mrsresolve.lisp, remove constraint preventing holes and labels from 
;; unifying (as seen in "my book" where poss_rel's handle has to unify with
;; restrictor, which is a hole).

(defun create-scoped-structures (top-handel bvs bindings rels scoping-handels)
  ;;; AAC Sept 17 - changed behaviour so no rels can be added to
  ;;; an existing set of relations which are known to fill a hole
  ;;;
  ;;; we have a top-handel (i.e. a current `top-handel' - this function
  ;;; is called recursively) and a set of rels, some of which may have the
  ;;; same handel as the top-handel.  We have to generate all possibilities
  ;;; for combinations of rels which COULD have the same handel
  ;;; bindings - current bindings
  ;;; bvs - a list of the variables which are bound by quantifiers we've
  ;;;       already dealt with (nil on first call)
  ;;; rels - rels we haven't incorporated yet (all rels on first call)
  ;;; scoping-handels - handels which will outscope the handels we're
  ;;;        about to incorporate (nil on first call) - added for outscopes
  ;;;        constraint
  (if rels
    ; fail immediately if we're going to be left with
    ; an unsatisfiable handel arg
    (let* ((handel-args (for rel in rels
                                    append (get-handel-args rel)))
           (impossible-top-rels 
            ; the list of rels which introduce variables which are
            ; currently free or which have a handel which is coindexed with a handel
            ; argument of one of the rels
            ; or which are `outscoped' by top-handel
            ; or which are outscoped by a handel which is not on the list
            ; of scoping-handels
            (for rel in rels
                 filter 
                 (let ((vars (collect-unbound-vars-from-rel rel)))
                   (if (or (and vars (not (subsetp vars bvs)))
                           (member (get-var-num (rel-handel rel)) handel-args)
                           (violates-outscopes-p  (get-var-num (rel-handel rel)) 
                                                  top-handel scoping-handels bindings))
                     ; violates-outscopes-p is in mrscons.lsp
                     rel))))
           (impossible-handels 
            (remove-duplicates (for rel in impossible-top-rels
                                    collect (get-var-num (rel-handel rel)))))
           (possible-top-rels 
            ; the list of rels which wouldn't introduce any free variables
            ; and which don't have sisters (i.e. rels with the same handel)
            ; which introduce free variables
            (for rel in (set-difference rels impossible-top-rels)
                 filter 
                 (unless (member (get-var-num (rel-handel rel)) impossible-handels)
                   rel)))
           (top-rels ; the list of rels which have the same handel as the
                     ; top handel
            (for rel in rels
                          filter (if (eql (get-var-num (rel-handel rel)) top-handel)
                                   rel))))
      (if (and possible-top-rels (subsetp top-rels possible-top-rels))
        ; unless there are some possible rels and all the top rels
        ; are possible, we fail
        (for rel-comb-and-bindings in 
             (make-combinations-with-is-one-ofs top-handel top-rels
                           ;;;  (if top-rels nil
                           ;;;    possible-top-rels)
                           ;;; AAC - modification to avoid abstr rels
                           ;;; turning up all over the place
			       (set-difference possible-top-rels top-rels)
						bindings)
             ;;; for each possible set of rels which can have their handels
             ;;; bound to the top-handel, generate a set of results
             append
             (sister-structure-scope 
              (car (bindings-and-sisters-sisters rel-comb-and-bindings))
              (cdr (bindings-and-sisters-sisters rel-comb-and-bindings)) 
              bvs 
              (bindings-and-sisters-bindings rel-comb-and-bindings) 
              (set-difference rels 
                              (bindings-and-sisters-sisters rel-comb-and-bindings))
              (cons top-handel scoping-handels)))))))

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


(in-package "MRS")

;; In mrsoutput.lisp, modified EVENT-ABBREV since we're no longer encoding
;; event/speech/reference time in the event FS.

(defun event-abbrev (fs gen)
  (let ((eventtime (path-value fs '(disco::time)))
        (reftime (path-value fs '(disco::reference)))
	(sptime (path-value fs '(disco::spch))))
    (when (and eventtime sptime)
      (if (not (eql sptime 'unify::*fail*))
	  (format nil "(E:~A, R:~A, S:~A)" 
		  (var-name (create-variable eventtime gen))
		  (var-name (create-variable reftime gen))
		  (var-name (create-variable sptime gen)))
	(format nil "(E:~A, R:~A)" (var-name (create-variable eventtime gen))
		(var-name (create-variable reftime gen)))))))

;; In mrsoutput.lisp, modify CONSTRUCT-CANDS-LIST to accommodate now treating
;; CANDS attribute as taking a diff-list, so we can distinguish local from
;; non-local values for the attribute.

(defun construct-cands-list (fs cands-list variable-generator)
  (let ((real-list-fs (path-value fs '(DISCO::LIST)))
        (last-pointer (path-value fs '(DISCO::LAST))))
    #-pagelite
    (when (is-valid-fs real-list-fs)
      (setf real-list-fs (deref real-list-fs)))
    (construct-cands-list-aux real-list-fs cands-list variable-generator 
                              last-pointer)))

(defun construct-cands-list-aux (real-list-fs cands-list variable-generator
				 last-pointer)
  (if (and (is-valid-fs real-list-fs)
           (not (eql real-list-fs last-pointer)))
      (let ((label-list (fs-arcs real-list-fs)))
        (if label-list
            (let ((first-part (assoc (car *liszt-first-path*)
                                     label-list))
                  (rest-part (assoc (car *liszt-rest-path*)
                                    label-list)))
              (if (and first-part rest-part)
                  (progn
                    (push (create-variable (cdr first-part) variable-generator)
                          cands-list)
                    (construct-cands-list-aux
                     (cdr rest-part)
                     cands-list variable-generator last-pointer))
                cands-list))
          cands-list))
    cands-list))

(in-package "TDL")

