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

(defun gen-instantiate-generics (ep)
  (loop
      with ids
      with pred = (mrs::rel-pred ep)
      with carg = (loop
                      for role in (mrs:rel-flist ep)
                      when (eq (mrs:fvpair-feature role) *generics-carg*)
                      return (mrs:fvpair-value role))
      with surface = (and (stringp carg)
			  (substitute #\space #\_ carg :test #'char=))
      with *package* = (find-package :lkb)
      for gle in (rest %generics-index%)
      for test
      = (when (gle-test gle) (ignore-errors (funcall (gle-test gle) ep)))
      when (or test (and carg (equal pred (gle-pred gle))))
      do
        (let* ((id (format nil "~@:(~a[~a]~)" (gle-id gle) (or test surface)))
               (id (intern id :lkb))
	       (test (when test (gen-lemma test))))
          (if (get-lex-entry-from-id id)
            (push id ids)
            (multiple-value-bind (tdfs orth)
                (instantiate-generic-lexical-entry
                 gle (or test surface) pred carg)
              (when tdfs
                (let ((new
                       (make-lex-entry
                        :orth (list orth) :id id :full-fs tdfs)))
                  ;;
                  ;; _fix_me_
                  ;; we should encapsulate the write access on the lexicon as a
                  ;; method cache-psort() or the like.           (7-jun-09; oe)
                  ;;
                  (with-slots (psorts) *lexicon*
                    (setf (gethash id psorts) new))
                  (mrs::extract-lexical-relations new)
                  (push id ids))))))
      finally (return ids)))

(defun gen-lemma (surface)
  (let ((lemmas (one-step-morph-analyse (string-upcase surface))))
    (if lemmas
	(string-downcase (first (first lemmas)))
      (string-downcase surface))))

;; DPF 15-feb-10 - In lkb/mrs/generate.lisp, in generate-from-mrs-internal()
;; Deleted section on treatment of `ersatz' entries, which
;; were supplanted with the advent of chart mapping machinery.
;; FIX: if okay, update source file

(defun generate-from-mrs-internal (input-sem &key nanalyses)

  ;; (ERB 2003-10-08) For aligned generation -- if we're in first only
  ;; mode, break up the tree in *parse-record* for reference by
  ;; ag-gen-lex-priority and ag-gen-rule-priority.  Store in *found-configs*.
  #+:arboretum
  (populate-found-configs)

  ;;
  ;; inside the generator, apply the VPM in reverse mode to map to grammar-
  ;; internal variable types, properties, and values.  the internal MRS, beyond
  ;; doubt, is what we should use for lexical instantiations and Skolemization.
  ;; regarding trigger rules and the post-generation MRS compatibility test, on
  ;; the other hand, we have a choice.  in principle, these should operate in
  ;; the external (SEM-I) MRS namespace (the real MRS layer); however, trigger
  ;; rules are created from FSs (using grammar-internal nomenclature) and, more
  ;; importantly, the post-generation test uses the grammar-internal hierarchy
  ;; to test for predicate, variable type, and property subsumption.  hence, it
  ;; is currently convenient to apply these MRS-level operations with grammar-
  ;; internal names, i.e. at an ill-defined intermediate layer.
  ;;
  ;; _fix_me_
  ;; the proper solution to all this mysery will be to create separate SEM-I
  ;; hierarchies, i.e. enrich the SEM-I files with whatever underspecifications
  ;; the grammar wants to provide at the MRS level, and then import that file
  ;; into its own, grammar-specific namespace.  one day soon, i hope, i might
  ;; actually get to implementing this design ...               (22-jan-09; oe)
  ;;
  (setf input-sem (mt:map-mrs input-sem :semi :backward))
  
  (setf *generator-internal-mrs* input-sem)
  (with-package (:lkb)
    (clear-gen-chart)
    (setf *cached-category-abbs* nil)

    ;;
    ;; no need to even try generating when there is no relation index
    ;;
    (unless (and (hash-table-p mrs::*relation-index*)
                 (> (hash-table-count mrs::*relation-index*) 0))
      (error 'generator-uninitialized))
    
    (let ((*gen-packing-p* (if *gen-first-only-p* nil *gen-packing-p*))
          lex-results lex-items grules lex-orderings 
          tgc tcpu conses symbols others)
      (time-a-funcall
       #'(lambda () 
           (multiple-value-setq (lex-results grules lex-orderings)
             (mrs::collect-lex-entries-from-mrs input-sem))
           (multiple-value-setq (lex-items grules lex-orderings)
             (filter-generator-lexical-items 
              (apply #'append lex-results) grules lex-orderings)))
       #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
           (declare (ignore tr ignore))
           (setf tgc (+ tgcu tgcs) tcpu (+ tu ts)
                 conses (* scons 8) symbols (* ssym 24) others sother)))
      (setq %generator-statistics%
        (pairlis '(:ltgc :ltcpu :lconses :lsymbols :lothers)
                 (list tgc tcpu conses symbols others)))
      
      (when *debugging* (print-generator-lookup-summary lex-items grules))
      
      (let ((rel-indexes nil) (rel-indexes-n -1) (input-rels 0))
        (dolist (lex lex-items)
          (loop
              with eps = (mrs::found-lex-main-rels lex)
              initially (setf (mrs::found-lex-main-rels lex) 0)
              for ep in eps
              for index = (ash 1 (or (getf rel-indexes ep)
                                     (setf (getf rel-indexes ep)
                                           (incf rel-indexes-n))))
              do 
                (setf (mrs::found-lex-main-rels lex)
                  (logior (mrs::found-lex-main-rels lex) index))))
        (dolist (grule grules)
          (when (mrs::found-rule-p grule)
            (loop
                with eps = (mrs::found-rule-main-rels grule)
                initially (setf (mrs::found-rule-main-rels grule) 0)
                for ep in eps
                for index = (ash 1 (or (getf rel-indexes ep)
                                       (setf (getf rel-indexes ep)
                                             (incf rel-indexes-n))))
                do
                  (setf (mrs::found-rule-main-rels grule)
                    (logior (mrs::found-rule-main-rels grule) index)))))
        (setf %generator-unknown-eps% nil)
        (loop
            for ep in (mrs::psoa-liszt input-sem)
            do
              (if (getf rel-indexes ep)
                (setq input-rels
                   (logior input-rels (ash 1 (getf rel-indexes ep))))
                (push ep %generator-unknown-eps%)))
        (when %generator-unknown-eps%
           (error 'unknown-predicates :eps %generator-unknown-eps%))

        #+:debug
        (setf %rel-indexes rel-indexes %input-rels input-rels)
        
        (chart-generate
         input-sem input-rels lex-items grules lex-orderings rel-indexes
         *gen-first-only-p* :nanalyses nanalyses)))))

;; DPF 15-feb-10 - In lkb/rmrs/rmrs-convert.lisp, in convert-rmrs-ep-to-mrs()
;; Temporary patch to accommodate conversion of EPs with 
;; unknown-word predicates.  FIX ...

(defun convert-rmrs-ep-to-mrs (ep rargs)
  (let* ((problems nil)
	 (rmrs-pred (rel-pred ep))
	 (semi-entries (find-semi-entries rmrs-pred)))
	(let*
	    ((string-p (cond ((every #'(lambda (semi-res)
					 (semi-res-stringp semi-res))
				     semi-entries)
			      t)
			     ((every #'(lambda (semi-res)
					 (not (semi-res-stringp semi-res)))
				     semi-entries)
			      nil)
			     (t (push 
				 (format nil "~A ambiguous between string and non-string" rmrs-pred)
				 problems)
				nil)))
	     (new-ep
	      (make-rel
	       :handel (rel-handel ep)
	       :parameter-strings (rel-parameter-strings ep)
     ;;;   :extra (rel-extra ep)  FIX
	       :pred (convert-rmrs-pred-to-mrs rmrs-pred string-p)
	       :flist (cons (convert-rmrs-main-arg (car (rel-flist ep))
						   rmrs-pred semi-entries)
			    (loop for rarg in rargs
				collect
				  (deparsonify rarg semi-entries)))
               :str (rel-str ep)
	       :cfrom (rel-cfrom ep)
	       :cto (rel-cto ep))))
	  (values new-ep problems))))

