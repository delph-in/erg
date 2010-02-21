(in-package :lkb)

;;; LinGO big grammar specific functions

(defun establish-linear-precedence (rule-fs)
   ;;;    A function which will order the features of a rule
   ;;;    to give (mother daughter1 ... daughtern)
   ;;;    
   ;;;  Modification - this must always give a feature
   ;;;  position for the mother - it can be NIL if
   ;;; necessary
  (let* ((mother NIL)
         (daughter1 (get-value-at-end-of rule-fs '(ARGS FIRST)))
         (daughter2 (get-value-at-end-of rule-fs '(ARGS REST FIRST)))
         (daughter3 (get-value-at-end-of rule-fs '(ARGS REST REST FIRST))))
    (declare (ignore mother))
    (unless daughter1 
      (cerror "Ignore it" "Rule without daughter"))
    (append (list nil '(ARGS FIRST))
            (if daughter2 
                (list '(ARGS REST FIRST)))
            (if daughter3 
                (if daughter2 
                    (list '(ARGS REST REST FIRST)))))))

(defun spelling-change-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which affects spelling and
;;; which should therefore only be applied by the morphology
;;; system.  
;;; Old test was for something which was a subtype of
;;; *morph-rule-type* - this tests for whether needs affix:
;;; < ND-AFF > = + (assuming bool-value-true is default value)
;;; in the rule
  (let ((affix (get-dag-value (tdfs-indef 
                               (rule-full-fs rule)) 'nd-aff)))
    (and affix (bool-value-true affix))))

(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
;;; this version tests for 
;;; < PRODUCTIVE > = -
;;; in the rule
  (let ((affix (get-dag-value 
                (tdfs-indef (rule-full-fs rule)) 'productive)))
    (and affix (bool-value-false affix))))
             

;;; return true for types that shouldn't be displayed in type hierarchy
;;; window. Descendents (if any) will be displayed, i.e. non-displayed
;;; types are effectively spliced out

(defun hide-in-type-hierarchy-p (type-name)
  ;; starts with _, or ends with _[0-9][M]LE[0-9]
  ;; or contains "GLBTYPE"
   (and (symbolp type-name)
      (or
         ;; graphs are pretty unreadable without glbtypes in there as well
         (search "GLBTYPE" (symbol-name type-name))
         (eql (char (symbol-name type-name) 0) #\_)
         (let* ((name (symbol-name type-name))
                (end (length name))
                (cur (position #\_ name :from-end t)))
            ;; wish I had a regexp package available...
            (and cur
               (< (incf cur) end)
               (if (digit-char-p (char name cur)) (< (incf cur) end) t)
               (if (eql (char name cur) #\M) (< (incf cur) end) t)
               (if (eql (char name cur) #\L) (< (incf cur) end))
               (if (eql (char name cur) #\E) (<= (incf cur) end))
               (or (= cur end)
                   (and (digit-char-p (char name cur))
                        (= (incf cur) end))))))))

(defun make-unknown-word-sense-unifications (word-string &optional orth)
  ;;; this assumes we always treat unknown words as proper names
  ;;; uncomment the *unknown-word-types* in globals.lsp
  ;;; to activate this
  (when word-string
    (list 
       (make-unification :lhs
          (create-path-from-feature-list '(ORTH FIRST))
          :rhs (make-u-value :type (or orth word-string)))
       (make-unification :lhs
          (create-path-from-feature-list '(ORTH REST))
          :rhs (make-u-value :type 'lkb::*null*))
       (make-unification
        :lhs (create-path-from-feature-list '(SYNSEM LKEYS KEYREL CARG))
        :rhs (make-u-value :type (string-downcase word-string))))))

(defun instantiate-generic-lexical-entry (gle surface &optional pred carg)
  (let ((tdfs (copy-tdfs-elements
               (lex-entry-full-fs (if (gle-p gle) (gle-le gle) gle))))
        (spath
         (if carg
           '(SYNSEM LKEYS KEYREL CARG)
           (and pred '(SYNSEM LKEYS KEYREL PRED)))))
    (loop
        with dag = (tdfs-indef tdfs)
        for path in (append '((ORTH FIRST)) (list spath))
        for foo = (existing-dag-at-end-of dag path)
        when foo do (setf (dag-type foo) *string-type*))
    (let* ((surface (or
                     #+:logon
                     (case (gle-id gle)
                       (guess_n_gle 
                        (format nil "/~a/" surface))
                       (decade_gle
                        (format nil "~as" surface)))
                     surface))
           (unifications
            (append
             (list 
              (make-unification
               :lhs (create-path-from-feature-list
                     (append *orth-path* *list-head*))
               :rhs (make-u-value :type surface))
              (make-unification
               :lhs (create-path-from-feature-list
                     (append *orth-path* *list-tail*))
               :rhs (make-u-value :type *empty-list-type*)))
             (when spath
               (list
                (make-unification
                 :lhs (create-path-from-feature-list spath)
                 :rhs (make-u-value :type (or carg pred)))))))
           (indef (process-unifications unifications))
           (indef (and indef (create-wffs indef)))
           (overlay (and indef (make-tdfs :indef indef))))
      (values
       (when overlay
        (with-unification-context (ignore)
          (let ((foo (yadu tdfs overlay)))
            (when foo (copy-tdfs-elements foo)))))
       surface))))

(defun make-orth-tdfs (orth)
  (let ((unifs nil)
        (tmp-orth-path *orth-path*))
    (loop for orth-value in (split-into-words orth)
         do
         (let ((opath (create-path-from-feature-list 
                       (append tmp-orth-path *list-head*))))
           (push (make-unification :lhs opath                    
                                   :rhs
                                   (make-u-value 
                                    :type orth-value))
                 unifs)
           (setq tmp-orth-path (append tmp-orth-path *list-tail*))))
    (let ((indef (process-unifications unifs)))
      (when indef
        (setf indef (create-wffs indef))
        (make-tdfs :indef indef)))))

(defun set-temporary-lexicon-filenames nil
  (let* ((version (or (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user)
                      (and (find-package :lkb)
                           (find-symbol "*GRAMMAR-VERSION*" :lkb))))
         (prefix
          (if version
            (remove-if-not #'alphanumericp (symbol-value version))
            "ERG")))
    (setf *psorts-temp-file* 
      (make-pathname :name (concatenate 'string prefix ".lex") 
		     :host (pathname-host (lkb-tmp-dir))
		     :device (pathname-device (lkb-tmp-dir))
                     :directory (pathname-directory (lkb-tmp-dir))))
    (setf *psorts-temp-index-file* 
      (make-pathname :name (concatenate 'string prefix ".idx") 
		     :host (pathname-host (lkb-tmp-dir))
		     :device (pathname-device (lkb-tmp-dir))
                     :directory (pathname-directory (lkb-tmp-dir))))
    (setf *leaf-temp-file* 
      (make-pathname :name (concatenate 'string prefix ".lts")
		     :host (pathname-host (lkb-tmp-dir))
		     :device (pathname-device (lkb-tmp-dir))
                     :directory (pathname-directory (lkb-tmp-dir))))
    (setf *predicates-temp-file* 
      (make-pathname :name (concatenate 'string prefix ".ric")
		     :host (pathname-host (lkb-tmp-dir))
		     :device (pathname-device (lkb-tmp-dir))
                     :directory (pathname-directory (lkb-tmp-dir))))
    (setf *semantics-temp-file* 
      (make-pathname :name (concatenate 'string prefix ".stc")
		     :host (pathname-host (lkb-tmp-dir))
		     :device (pathname-device (lkb-tmp-dir))
                     :directory (pathname-directory (lkb-tmp-dir))))))
    

;;;
;;; used in lexicon compilation for systems like PET and CHiC: when we compile
;;; out the morphology, there is no point in outputting uninflected entries for
;;; systems that have no on-line morphology.  also used in [incr tsdb()] for
;;; counting of `words'.
;;;
;;; DPF 28-Aug-01 - In fact, we need uninflected forms at least for nouns in 
;;; order to analyze measure phrases like "a ten person group arrived" where
;;; the measure noun "person" is uninflected, so it can unify with the plural
;;; modifier "ten".

(defun dag-inflected-p (dag)           
  (let* ((key (existing-dag-at-end-of dag '(inflectd))))
    (and key (not (bool-value-false key)))))

;;; Function to run when batch checking the lexicon

(defun lex-check-lingo (new-fs id)
  #|
  (unless (extract-infl-pos-from-fs (tdfs-indef new-fs))
  (format *lkb-background-stream* "~%No position identified for ~A" id))
  |#
  (when new-fs
    (let* ((inflbool 
           (existing-dag-at-end-of (tdfs-indef new-fs)
                                   '(inflectd)))
          (type (and (dag-p inflbool) (dag-type inflbool))))
      (when type
        (when
            (eq type 'bool)
          (format *lkb-background-stream* "~%INFLECTD unset on ~A" id))))))


(setf *grammar-specific-batch-check-fn* #'lex-check-lingo)


(defun bool-value-true (fs &key unifiablep)
  (when fs
    (let ((type (type-of-fs fs)))
      (or (eql type '+)
          (and unifiablep (unifiable-dags-p fs (make-dag :type '+)))))))
  
(defun bool-value-false (fs)
  (and fs
       (let ((fs-type (type-of-fs fs)))
         (eql fs-type '-))))

;;;
;;; try a new approach to post-parsing filtering of idioms, building on the new
;;; MRS transfer machinery.  essentially, the idiom phrases have been recast as
;;; MRS transfer rules (MTRs), each of them matching an idiom configuration 
;;; and replacing the idiomatic parts of the MRS with a synthesized relation 
;;; (or nothing, for the time being).  post-transfer, the filter can then just
;;; require that no idiomatic relation remain.  (20-feb-05; dan & oe phx - sfo)
;;;
(defun idiom-complete-p (tdfs)
  (let* ((mrs (and (tdfs-p tdfs)
                   (mrs::extract-mrs-from-fs (tdfs-indef tdfs))))
         (transfers (and (mrs::psoa-p mrs)
                         (mt:transfer-mrs mrs :task :idiom))))
    (loop
        for transfer in transfers
        for mrs = (mt::edge-mrs transfer)
        thereis (loop
                    for ep in (mrs:psoa-liszt mrs)
                    when (idiom-rel-p ep) return nil
                    finally (return t)))))

(eval-when #+:ansi-eval-when (:load-toplevel :execute)
	   #-:ansi-eval-when (load eval)
  (setf *additional-root-condition* #'idiom-complete-p))

(defun determine-argument-optionality (sign arguments)
  ;;
  ;; there appear to be (at least) two ways of linking arguments in the
  ;; semantics to syntactic dependents, either by grabbing the LTOP (or maybe
  ;; sometimes INDEX) of an argument synsem, or just by grabbing its --SIND.
  ;;
  (if (and (dag-p sign)
           (loop for argument in arguments always (dag-p argument)))
    (let* ((cat (existing-dag-at-end-of sign '(SYNSEM LOCAL CAT)))
           (synsems (find-substructures-subsumed-by cat 'synsem_min)))
      ;;
      ;; for all substructures subsumed by `synsem_min' (candidate arguments)
      ;; below CAT, see whether their index or handle corresponds to one of the
      ;; variables we are looking for; if so, determine optionality by looking
      ;; at the OPT value.
      ;;
      (loop
          for argument in arguments
          collect
            (loop
                for (path . synsem) in synsems
                do (setf path path)
                thereis
                  (loop
                      for path in '((--SIND)
                                    (LOCAL CONT HOOK INDEX)
                                    (LOCAL CONT HOOK LTOP))
                      for value = (existing-dag-at-end-of synsem path)
                      when (and value (eq argument (deref-dag value)))
                      return (bool-value-true
                              (existing-dag-at-end-of synsem '(OPT))
                              :unifiablep t)
                      finally (return nil)))))
    (loop repeat (length arguments) collect nil)))

;;;
;;; the following two functions allow customization of how edges are displayed
;;; in the LUI chart browser (not the traditional LKB chart window).  for each
;;; edge, two properties are relevant: (a) its `name' and (b) its `label'; both
;;; should be strings, where name should be a relatively short, yet contentful
;;; identifier used as the primary representation of edges in chart cell, and
;;; label can be a longer string shown in the pop-up area on mouse-over.
;;;
(defun lui-chart-edge-name (edge)
  (let ((rname (existing-dag-at-end-of 
                (tdfs-indef (edge-dag edge)) '(RNAME))))
    (format 
     nil 
     "~a[~a]"
     (cond (rname (dag-type rname))
           ((not (edge-children edge)) 
            (let ((le (get-lex-entry-from-id (first (edge-lex-ids edge)))))
              (dag-type (tdfs-indef (lex-entry-full-fs le)))))
           (t (tree-node-text-string 
               (find-category-abb (edge-dag edge)))))
     (edge-id edge))))

;;;
;;; the following temporary expedient attempts to get capitalization more right
;;; than we used to do in generator outputs.  still, for acronyms like `IBM' or
;;; complex names including lower case elements, i see no alternative to using
;;; ORTH to spell out the actual (canonical) surface form.  that would seem to
;;; require that we re-view assumptions about capitalization across the lexicon
;;; et al.  but the LKB should probably do that one day!        (30-aug-05; oe)
;;; --- as of late, the ERG lexicon actually contains (some) ORTH values that
;;; reflect canonical capitalization; the modified code below will now try to
;;; either (a) respect the orthography from the lexicon, as long as it contains
;;; at least one upper-case letter and is string-equal() to the inflected form
;;; (which tends to be true for proper names at least :-) or (b) invoke the old
;;; heuristics to try and guess appropriate capitalization.  still not quite a
;;; perfect solution, but to do better i now think the morphology would have to
;;; stop upcasing things as soon as one of the inflectional rules applies.
;;;                                                             (18-dec-06; oe)
;;;
(defun gen-extract-surface (edge &optional (initialp t) &key cliticp stream)
  (if stream
    (let ((daughters (edge-children edge)))
      (if daughters
        (loop
            for daughter in daughters
            for foo = initialp then nil
            do 
              (setf cliticp 
                (gen-extract-surface
                 daughter foo :cliticp cliticp :stream stream))
              #+:logon finally
              #+:logon
              (setf (edge-lnk edge)
                (mrs::combine-lnks
                 (edge-lnk (first daughters))
                 (edge-lnk (first (last daughters))))))
        (let* ((entry (get-lex-entry-from-id (first (edge-lex-ids edge))))
               (orth (format nil "~{~a~^ ~}" (lex-entry-orth entry)))
               ;;
               ;; need to fix-up irregular cases like `Englishmen' manually :-{
               ;;
               (orth (if (ppcre::scan "man$" orth)
                       (subseq orth 0 (- (length orth) 3))
                       orth))
               (tdfs (and entry (lex-entry-full-fs entry)))
               (type (and tdfs (type-of-fs (tdfs-indef tdfs))))
               (string (string-downcase (copy-seq (first (edge-leaves edge)))))
               ;;
               ;; _fix_me_
               ;; maybe we could be more courageous and just search for .orth.
               ;; as a sub-sequence of .string., starting at position .prefix.
               ;;                                               (22-dec-06; oe)
               (prefix (loop
                           for c across string
                           while (member c '(#\( #\" #\') :test #'char=)
                           count 1))
               (suffix (min (length string) (+ prefix (length orth))))
               (suffix (when (string-equal
                              orth string :start2 prefix :end2 suffix)
                         suffix))
               (rawp (and suffix
                          (loop for c across orth thereis (upper-case-p c))))
               (capitalizep
                (ignore-errors
                 (loop
                     for match in '(basic_n_proper_lexent
                                    n_-_c-month_le
                                    n_-_c-dow_le
                                    n_-_pr-i_le)
                     thereis (or (eq type match)
                                 (subtype-p type match)))))
               (cliticp (or cliticp
                            (and (> (length string) 0)
                                 (char= (char string 0) #\')))))
          (if rawp
            (setf string
              (concatenate 'string
                (subseq string 0 prefix) orth (subseq string suffix)))
            (when capitalizep
              (loop
                  with spacep = t
                  for i from 0 to (- (length string) 1)
                  for c = (schar string i)
                  when (char= c #\Space) do (setf spacep t)
                  else when (char= c #\_)
                  do
                    (setf spacep t)
                    (setf (schar string i) #\Space)
                  else do
                    (when (and spacep (alphanumericp c))
                      (setf (schar string i) (char-upcase c)))
                       (setf spacep nil))))
          (when (and (> (length string) 1)
                     (char= (char string 0) #\_)
                     (upper-case-p (char string 1)))
            (setf string (subseq string 1)))
          (when (and initialp (alphanumericp (schar string 0)))
            (setf (schar string 0) (char-upcase (schar string 0))))
          (unless (or initialp cliticp)
            (format stream " "))
          (let (#+:logon 
                (start (file-position stream)))
            (loop
                with hyphenp
                for c across string
                unless (and hyphenp (char= c #\space))
                do (write-char c stream)
                when (char= c #\-) do (setf hyphenp t)
                else do (setf hyphenp nil))
            #+:logon
            (setf (edge-lnk edge)
              (list :characters start (file-position stream))))
          ;;
          ;; finally, inform the caller as to whether we output something that
          ;; inhibits intervening space (e.g. `mid-July').
          ;;
          (unless (string= orth "")
            (member (schar orth (- (length orth) 1)) '(#\-) :test #'char=)))))
    (let ((stream (make-string-output-stream)))
      (gen-extract-surface edge initialp :stream stream)
      (get-output-stream-string stream))))

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (setf *gen-extract-surface-hook* 'gen-extract-surface))

;;
;; DPF 20-oct-06: Enable newly enhanced treatment of affixation with 
;; multi-words.
;;
;; _fix_me_
;; a value of `nil' means that inflection can occur at any position, so that
;; we get both prefix punctuation and one or more suffixes: |(co- workers)|.  
;; however, the problem in this new universe is that we get spurious analyses
;; too, e.g. |co- (workers)|.                                   (9-feb-09; oe)
;;
(defun find-infl-pos (unifs orths sense-id)
  (declare (ignore unifs orths sense-id))
  nil)
