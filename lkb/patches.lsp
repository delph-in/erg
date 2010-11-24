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

(setf ppcre:*use-bmh-matchers* nil)

;; DPF 15-feb-10 - In lkb/rmrs/rmrs-convert.lisp, in convert-rmrs-ep-to-mrs()
;; Temporary patch to accommodate conversion of EPs with 
;; unknown-word predicates.  FIX ...
(in-package :mrs)
(defun convert-rmrs-ep-to-mrs (ep rargs)
 (let* ((problems nil)
	 (rmrs-pred (rel-pred ep))
	 (mrs-pred (convert-rmrs-pred-to-mrs rmrs-pred))
	 (semi-pred (or (mt::find-semi-entries mrs-pred)
			mrs-pred)))
   (if semi-pred
	(let ((new-ep
	      (make-rel
	       :handel (rel-handel ep)
	       :parameter-strings (rel-parameter-strings ep)
              :extra (rel-extra ep)
	       :pred semi-pred
	       :flist (cons (convert-rmrs-main-arg (car (rel-flist ep)))
			    (loop for rarg in rargs
				collect
				  (deparsonify rarg)))
              :str (rel-str ep)
	       :cfrom (rel-cfrom ep)
	       :cto (rel-cto ep))))
	  (values new-ep problems))
     (values nil
	      (list (format nil "No entry found in SEM-I for ~A" 
			    rmrs-pred))))))

;; PSQL is not happy with the default "*" for reqd-fields, so supply the
;; necessary list explicitly via grammar-fields()
(in-package :lkb)
(defmethod retrieve-entry2 ((lex mu-psql-lex-database) name &key (reqd-fields '("*")))
  (let ((reqd-fields (grammar-fields lex))
	(qname (psql-quote-literal name)))
    (get-records lex
		 (format nil
			 "SELECT ~a FROM (SELECT rev.* FROM public.rev as rev JOIN lex_cache USING (name,userid,modstamp) WHERE lex_cache.name = ~a UNION SELECT rev.* FROM rev JOIN lex_cache USING (name,userid,modstamp) WHERE lex_cache.name = ~a) as foo"
			 (fields-str lex reqd-fields)
			 qname qname))))

;; Avoid bogus complaint about PSQL server version - now outdated information
(defmethod check-psql-server-version ((lex mu-psql-lex-database))
  t)

