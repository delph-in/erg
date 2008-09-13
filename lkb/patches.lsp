;;;
;;; a couple of _temporary_ patches to LKB system code for better generation
;;;                                                           (9-dec-03; oe)
;;;

(in-package :lkb)


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
                    (string-downcase relpred))))
    (and relname
         (or 
          (equal "_i_rel" (subseq relname (- (length relname) 6)))
          (equal "-i_rel" (subseq relname (- (length relname) 6)))))))


; In lkb/src/lexdb/headers.lsp
; Added "5" to load-foreign-types for 64-bit
(defun psql-initialize ()
  (unless (libpq-p)
    #+:linux
    (let (#+allegro 
	  (excl::*load-foreign-types* 
	   (append '("3" "4" "5") excl::*load-foreign-types*))
	  )
      (load-libpq '("libpq.so.5" "libpq.so" "libpq.so.4" "libpq.so.3")))
    #+:mswindows
    (load-libpq '("libpq.dll"))
    #-(or :linux :mswindows)
    (load-libpq nil)))

#+:logon
(setf ppcre:*use-bmh-matchers* nil)

; From lkb/src/glue/repp.lsp
; DPF 12-may-08 - Experimental patch to pretokenize string for the benefit
; of the tagger (separating punctuation etc)
(in-package :lkb)

#+:tsdb
(defun repp-for-pet-with-tagger (string &optional tagger &key (format :pet))
  (setf string
    (let ((pretokscript (format nil "~Alkb/token-n-tnt.prl" 
				   lkb::*grammar-directory*)))
      (with-open-file (stream "/tmp/pretokin" :direction :output 
		       :if-does-not-exist :create :if-exists :supersede)
        (write-string string stream))
      (run-process (format nil "~a < /tmp/pretokin" pretokscript)
		   :wait t :output "/tmp/pretokout"
		   :if-output-exists :supersede)
      (tsdb::read-file "/tmp/pretokout")))
  (if (and tagger (consp tagger) (keywordp (first tagger)))
    (multiple-value-bind (tokens length)
        (case (first tagger)
          (:tnt
           (apply 
            #'tnt
            (repp string :repp :preprocessor-min :format :list :verbose nil)
            (rest tagger))))
      (loop
          for (id start end form surface . tags) in tokens
          for token = (format 
                       nil 
                       "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\",~
                       ~{ ~s ~,4f~})" 
                       id start end 
                       (escape-string form) (escape-string surface) tags)
          collect token into tokens
          finally 
            (return (values (format nil "~{~a~^ ~}" tokens) length))))
                
    (repp string :repp :preprocessor-min :format format :verbose nil)))

(in-package :cl-user)


