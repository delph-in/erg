;;;
;;; a couple of _temporary_ patches to LKB system code for better generation
;;;                                                           (9-dec-03; oe)
;;;

(in-package :mrs)

;;;
;;; since, for fragments at least, we use `geq' handle constraints, need to at
;;; least enable construction of MRS objects; presumably, there is no way for
;;; these to scope without work on the algorithm, but alas.     (5-jun-04; oe)
;;;

(defun create-hcons-relation (type)
  (cond ((eql type *qeq-type*) "qeq")
        ((eql type (vsym "geq")) "geq")
        (t (error "Unknown relation type ~A"))))

(in-package :lkb)

; From lkb/src/mrs/spell.lisp:
; Commented out the a/an spelling fix, since the grammar is now enforcing
; this directly, with each lexical entry identifying its phonological
; onset as either consonantal or vocalic.

(defun fix-spelling (string)
  (setf string (mapcar #'string-downcase string))
  (let ((res nil))
    (loop
      (when (null string) (return))
      (let ((word (car string))
            ;(rest (cdr string))
            )
        (setf string (cdr string))
        ;(if (equal word "a")
        ;    (if (vowel-first-p (car rest))
        ;        (push "an" res)
        ;      (push "a" res))
          (push word res)
        ;  )
      ))
    (nreverse res)))

;;; For better batch testing of MRS quality, esp produce-one-scope()

; In lkb/src/mrs/mrsresolve.lsp, modified chain-down-margs() to also allow
; for top-level conjunctions (including discourse relation) - clearly
; grammar-specific, so should probably be in user-fns.lsp, or should
; have global in this function bound to list of grammar-specific feature
; names.

(in-package :mrs)
(defun chain-down-margs (rel mrsstruct)
  (let* ((marg-fvp (dolist (fvpair (rel-flist rel))
		    (when (or (eq (fvpair-feature fvpair) 'lkb::marg)
                              (eq (fvpair-feature fvpair) 'lkb::r-hndl)
                              (eq (fvpair-feature fvpair) 'lkb::main))
		      (return fvpair))))
	(marg-value 
	 (if marg-fvp
	       (get-var-num (fvpair-value marg-fvp)))))
    (if marg-value
	(let ((top-rels
	       (get-rels-with-label-num marg-value mrsstruct)))
	  (if top-rels
	      (if (cdr top-rels)
		  nil
		(chain-down-margs (car top-rels) mrsstruct))
	    (dolist (qeq (psoa-h-cons mrsstruct))
	      (when (eq marg-value (var-id (hcons-scarg qeq)))
		(return (values qeq marg-fvp)))))))))

(in-package :lkb)

; In lkb/src/tsdb/lisp/lkb-interface.lisp
; Redefine tsdb::finalize-run to comment out uncache-lexicon() since it
; results in uninitializing the generator lexicon, which is annoying:
;   uncache-lexicon
;     clear-expanded-lex
;       empty-cache
;         clear-generator-lexicon
(in-package :lkb)
#+:tsdb
(defun tsdb::finalize-run (context &key custom)
  (declare (ignore custom))
  ;; called after completion of test run
  (let ((lexicon 0)
        (*package* *lkb-package*))
    (loop 
        for id in (collect-expanded-lex-ids *lexicon*)
        do 
          (pushnew id *lex-ids-used*)
          (incf lexicon)) 
    (clear-type-cache)
    ;;(uncache-lexicon)
    (loop
        for (variable . value) in context do
          (case variable
            (:first-only-p 
             (setf *first-only-p* value))))
    (pairlis '(:lexicon) (list lexicon))))

; In mrs/idioms.lisp
; Added check in idiom_rel-p() since mt::transfer-mrs() is surprised at
; finding a predicate name as value of ARG1 for degree specifiers of
; quantifiers (as in "almost every") and assigns a "u" type variable,
; which this function did not expect as value of PRED.
(in-package :lkb)
(defun idiom-rel-p (rel)
  ;;; FIX
  ;;; relation name ends with _i_rel - this won't quite do because
  ;;; we want to allow for different senses and anyway this should use the
  ;;; standard pred parsing code
  (let* ((relpred (mrs::rel-pred rel))
         (relname (when (and relpred 
                             (or (symbolp relpred) (stringp relpred)))
                    (string relpred))))
    (and relname
         (equal "_i_rel" (subseq relname (- (length relname) 6))))))


; Comment out vacuuming when not changing lexicon - too time-consuming
(defmethod update-lex ((lex psql-lex-database))
  ;(vacuum lex)
  ;(lexdb-time ("updating 'lex' table" "done updating 'lex' table")
  ;	      (update-lex-aux lex))
  ;(vacuum lex)
  (cond
   ((null (semi lex))
    nil)
   ((semi-up-to-date-p lex)
    (unless 
	    (lexdb-time ("loading SEM-I into memory" "done loading SEM-I into memory")
			(mrs::semi-p 
			 (catch :sql-error
			   (mrs::populate-*semi*-from-psql)
			   )))
      (format t "~&(LexDB) unable to retrieve database SEM-I"))
    (index-lexical-rules)
    (index-grammar-rules))
   (t
    (format t "~&(LexDB) WARNING:  no lexical entries indexed for generator")))
  lex)



(in-package :cl-user)

