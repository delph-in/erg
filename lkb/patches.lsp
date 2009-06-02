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


;; DPF 20-Feb-09
;; Patches to allow better generation of unknown words.

;; To test
;; (tsdb::tsdb :cpu :mrs :file t)
;; (mt::parse-interactively "Kim arrived in 2222.")

;; To do:
;; 
;; In order to stay consistent with reg-ex assumptions that can refer to 
;; course sense distinctions like "_n_, _v_, _a_" in trigger rules,
;; fix PRED name for unknowns to keep four fields, using both the coarse
;; sense distinctions and the subsense for the actual tags, prefixed
;; with "unk-" to avoid collisions with manual lexical entries,
;; So not "_frob_nn_rel" but "_frob_n_unk-nn_rel", and similarly
;; "_frob_v_unk-vb_rel", "_frob_a_unk-jj_rel", "_frob_a_unk-rb_rel", etc. 

;; 
;; Change identification of the subset of generic lex entries used for 
;; generation (currently based on their being on the list 
;; *generic-lexical-entries* set manually in erg/mrs/globals.lsp).  
;; Instead should mark them as relevant for generation, maybe with 
;; [+CLASS generation], where 'generation' would be a supertype of some 
;; existing +CLASS values like 'proper_ne'.  So index-for-generator() could 
;; find these by the combination of +CLASS and +ONSET (since native entries
;; are unmarked for +CLASS). Perhaps still (now automatically) add them to 
;; *generic-lexical-entries* as currently used in generation.

(in-package :lkb)

;; Redefined from lkb/src/main/generics.lsp
(defun gen-instantiate-generics (ep)
  (loop
      with ids
      with pred = (mrs::rel-pred ep)
      with carg = (loop
                      for role in (mrs:rel-flist ep)
                      when (eq (mrs:fvpair-feature role) *generics-carg*)
                      return (mrs:fvpair-value role))
      with predlist = (unless carg (cl-ppcre:split "_" pred))
      ;; Input is an 'exploded' predname, sans underscore: ("" "foo" "n" "rel")
      ;; Assuming predname was either "_foo_v_unk-vbz_rel" or "_foo_vbz_rel"
      with tag = (when predlist (third predlist))
      with surface = (or (and carg
			      (substitute #\space #\_ carg :test #'char=))
			 (and predlist
			      (stem-generic (second predlist) tag)))
      for gle in (rest %generics-index%)
      when (or (equal pred (gle-pred gle))
	       (gle-tag-p tag gle))
      do
        (let* ((id (format nil "~@:(~a[~a]~)" (gle-id gle) surface))
               (id (intern id :lkb)))
          (if (get-lex-entry-from-id id)
            (push id ids)
            (multiple-value-bind (tdfs orth)
                (instantiate-generic-lexical-entry gle surface pred carg)
              (when tdfs
                (let ((new
                       (make-lex-entry
                        :orth (list orth) :id id :full-fs tdfs)))
                  (with-slots (psorts) *lexicon*
                    (setf (gethash id psorts) new))
                  (mrs::extract-lexical-relations new)
                  (push id ids))))))
      finally (return ids)))

;; Redefined from lkb/src/mrs/lexlookup.lisp
;; Allows for unknown pred names starting with leading underscore
;; Note: index-for-generator() currently does not index 'day_rel' or 'yofc_rel'
;; from the generic entries.
;;
(in-package :mrs)

(defun lexical-rel-p (rel-name)
  (or (gethash rel-name *relation-index*)
      (and (stringp rel-name) (equal (subseq rel-name 0 1) "_"))
      (member rel-name '(lkb::day_rel lkb::yofc_rel))))

(in-package :lkb)

(defun gle-tag-p (tag gle)
  (let ((id (gle-id gle)))
    (or (and (eq id 'guess_n_gle) 
	     (member tag '("n" "nn" "unk-nn") :test #'string-equal))
	(and (eq id 'guess_v_gle) 
	     (member tag '("v" "vb" "vbz" "vbd" "vbg" "vbn"
		           "unk-vb" "unk-vbz" "unk-vbd" "unk-vbg" "unk-vbn")
			  :test #'string-equal))
	(and (eq id 'generic_adj) 
	     (member tag '("j" "jj" "unk-jj") :test #'string-equal))
	(and (eq id 'generic_adverb) 
	     (member tag '("r" "rb" "unk-rb") :test #'string-equal)))))

;; This function is just a place-holder. It should eventually either do a 
;; look-up in a very large table of |surface stem tag| for 'all' English 
;; words, or else compute the stem given the surface and the tag, possibly
;; given by a tagger which also reports the stem.
;;
(defun stem-generic (string tag)
  (declare (ignore tag))
  ;; Compute stem based on tag somehow
  string)

;; 31-mar-09
;; For now, keeping the old definition in batch-check.lsp, since the ERG's
;; diff-lists all generate the warning, which the new code sends to ostream.
;; This patch will disappear as soon as the latest LKB improvements are
;; incorporated into the logon branch
;;
(in-package :lkb)

#+:logon
(defun check-dag-diff-list (dag id path &optional (ostream t))
  (let* ((list-dag (dag-path-val (list *diff-list-list*) dag))
         (last-dag (dag-path-val (list *diff-list-last*) dag)))
    (when
        (and
         (null (top-level-features-of list-dag))
         (null (top-level-features-of last-dag))
         (eq-or-subtype list-dag *list-type*)
         (eq-or-subtype last-dag *list-type*))
      (format *batch-check-diff-list-strict* "~%WARNING: malformed but 'acceptable' \
difference list at ~a in ~a" (reverse path) id)
      (return-from check-dag-diff-list))
    (loop
        with rest-dag
        while (not (eq list-dag
                       last-dag))
        do
          (setf rest-dag (dag-path-val '(rest) list-dag))
          (when (null rest-dag)
            (format ostream "~%WARNING: malformed difference list at ~a in ~a" (reverse path) id)
            (return-from check-dag-diff-list))
        do
          (setf list-dag rest-dag))
    t))
