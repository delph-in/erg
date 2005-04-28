;;;
;;; a couple of _temporary_ patches to LKB system code for better generation
;;;                                                           (9-dec-03; oe)
;;;

(in-package :mrs)

;;; 1. The generator munging rules now make use of an early version of a
;;; grammar-external rels hierarchy supplied by Ann, in order to
;;; identify the class of predicates which require expletive "it".  This
;;; revised functionality is called in a changed version of
;;; compatible-types-or-values():

(defun compatible-types-or-values (val1 val2)
  ;;
  ;; in the current untyped universe, it seems legitimate to not have values
  ;; for PRED or any of the roles: while this should not happen for the input
  ;; MRS, allow null() values in the munging rule to be considered compatible.
  ;;                                                          (3-nov-03; oe)
  (unless (or (eq val1 (vsym "NEVER_UNIFY_REL"))
              (eq val2 (vsym "NEVER_UNIFY_REL")))
    (or (is-top-type val1) (is-top-type val2)
        (null val1)
        (and (is-valid-type val1) (is-valid-type val2) 
	     (compatible-types val1 val2))
        (and (is-munge-type val1) (is-munge-type val2)
             (compatible-munge-types val1 val2))
        (cond ((and (symbolp val1) (symbolp val2))
               (same-names val1 val2))
              ((and (stringp val1) (stringp val2))
               (equal val1 val2))
              ((and (stringp val1) (symbolp val2))
               (equal val1 (symbol-name val2)))
              ((and (stringp val2) (symbolp val1))
               (equal val2 (symbol-name val1)))
              (t (equal val1 val2))))))

(defparameter *munge-expl-preds* nil)

(defun compatible-munge-types (val1 val2)
  (or (and (string-equal val1 "test-expl-type")
           (member (string-downcase val2) *munge-expl-preds*
                   :test #'string-equal))
      (and (string-equal val2 "test-expl-type")
           (member (string-downcase val1) *munge-expl-preds*
                   :test #'string-equal))))

(defun is-munge-type (val)
  (declare (ignore val))
  t)


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

;;; Correction to PostgreSQL Emacs interface:

; 07-apr-05 In psql-emacs.lsp, need to add 'get-value-set and 'set-lex-entry
; to *lexdb-emacs-lexdb-fns*, or get error when try to save a changed entry
; within Emacs

(defconstant *lexdb-emacs-lexdb-fns*
    '(complete
      connection
      dbname
      empty-cache
      fields
      get-field-size-map
      id-to-tdl-str
      lookup
      new-entries
      record-to-tdl
      retrieve-head-record-str
      set-lex-entry-from-record
      get-value-set
      set-lex-entry
      scratch-records))

;;; Correction to PostgreSQL generator interface

; 07-apr-05 - In mrs/semi-psql.lsp, get error when call (index-for-generator): 
; within the function lkb::semi-setup-pre(), get the following error:
; >> PSQL ERROR:  index "semi_pred_lex_id" does not exist
; so making calling function vacuous until fixed.

(in-package :mrs)
#+:psql
(defmethod dump-semi-to-psql ((semi semi) &key (lexicon lkb::*psql-lexicon*))
  (declare (ignore lexicon))
  t)  

(in-package :lkb)
; Related patch - comment out check for semi in order to keep generator index
(defmethod build-lex ((lexicon psql-lex-database))
  (build-lex-aux lexicon)
#|
  (cond
   ((null (semi lexicon))
    nil)
   ((semi-up-to-date-p lexicon)
    (format t "~%(loading SEM-I into memory)")
    (unless (mrs::semi-p 
	     (catch :sql-error
	       (mrs::populate-*semi*-from-psql)))
      (format t "~% (unable to retrieve database SEM-I)"))
    (index-lexical-rules)
    (index-grammar-rules))
   (t
    (format t "~%WARNING: no lexical entries indexed for generator")))
|#
  lexicon)

;;; Modification to psql-lex-database.lsp: build-lex-aux() makes call to
;;; sql-fn-get-val to get the current size of the lexicon, but this call
;;; can be very time-intensive, so disable for now

(in-package :lkb)
(defmethod build-lex-aux ((lexicon psql-lex-database))
  (reconnect lexicon) ;; work around server bug
  (cond 
   ((not (user-read-only-p lexicon (user lexicon)))
    (sql-fn-get-raw-records lexicon 
			    :initialize_current_grammar 
			    :args (list (get-filter lexicon))))
   (t
    (format t "~%(user ~a has read-only privileges)" (user lexicon))))    
  (format t "~%(LexDB filter: ~a )" (get-filter lexicon))
  ;(let ((size (sql-fn-get-val lexicon :size_current_grammar)))
  ;  (if (string= "0" size)
  ;	(format t "~%WARNING: 0 entries passed the LexDB filter" size)
  ;    (format t "~%(active lexical entries: ~a )" size)))
  (empty-cache lexicon))

;;; Old correction to setup for postgres:
;;; 02-mar-05 - Probably now fixed? 
; Earlier, had to change lkb/lexdb/fns_plpgsql.sql to rename function 
; "dump_db_su" to "public.dump_db_su" since that's now how it's defined in 
; su-setup.sql. Same for "dump_db_dfn_fld" => "public.dump_db_dfn_fld".
; Now seems to be unnecessary.


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

(in-package :cl-user)
