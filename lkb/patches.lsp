;;;
;;; a couple of _temporary_ patches to LKB system code for better generation
;;;                                                           (9-dec-03; oe)
;;;

(in-package :mrs)

;;; 1. The generator munging rules now make use of an early version of a
;;; grammar-external rels hierarchy supplied by Ann last week, in order to
;;; identify the class of predicates which require expletive "it".  This new
;;; functionality is called in a changed version of
;;; compatible-types-or-values(), as below:

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

#|
(defun get-filled-mrs-string (edge)
  (when (lkb::edge-p edge)
    (let* ((*package* (find-package :lkb))
	   (mrs (extract-mrs edge)))
      (with-output-to-string (stream)
	(output-mrs1 (fill-mrs (unfill-mrs mrs)) 'simple stream)))))
|#

;;; 3. (show-gen-chart) breaks on adjectives since degree specifiers now have
;;; pred string names as values, and create-gen-chart-pointers() in
;;; mrstoplevel.lsp tries to apply symbol-name() to sem-args but gets this
;;; string, unhappily.

(in-package :lkb)
#|
(defun create-gen-chart-pointers (root all-p)
  ;; create a global mapping from edge-ids to symbols, not interned - so we
  ;; don't end up hanging on to old edges
  (let ((edge-symbols nil))
    (dolist (entry *gen-chart*)
      (dolist (e (append (cadr entry) (cddr entry)))
        (push
         (list* (dotted-edge-id e)
                (make-edge-symbol (dotted-edge-id e))
                (dotted-edge-needed e))
         edge-symbols)))
    (dolist (entry *gen-chart*)
      (let ((chart-index (string-downcase 
                          (if (stringp (car entry)) (car entry)
                            (symbol-name (car entry))))))
        (dolist (e (append (cadr entry) (cddr entry)))
          (let ((edge-symbol
                 (cadr (assoc (dotted-edge-id e) edge-symbols))))
            (setf (get edge-symbol 'chart-edge-span)
              (if (dotted-edge-needed e)
                  (concatenate 'string chart-index " A") chart-index))
            (setf (get edge-symbol 'chart-edge-contents) e)
            (if (dotted-edge-children e)
                (dolist (c (dotted-edge-children e))
                  (when c
                    (push edge-symbol
                          (get (cadr (assoc (dotted-edge-id c) edge-symbols))
                               'chart-edge-descendents))))
              (push edge-symbol (get root 'chart-edge-descendents)))))))
    (unless all-p
      ;; remove intermediate links consisting of active edges
      (dolist (pair edge-symbols)
        (setf (get (cadr pair) 'chart-edge-descendents)
          (create-gen-chart-pointers-collapse
           (get (cadr pair) 'chart-edge-descendents)
           edge-symbols))))))
|#

#|
; From mrs/mrstoplevel.lisp
; Added unfilling and filling steps
(defun really-generate-from-edge (parser-edge)    
  (let* ((input-sem (mrs::extract-mrs parser-edge)))
    (with-output-to-top ()
      (if (and input-sem (mrs::psoa-p input-sem)
               (mrs::psoa-liszt input-sem))
	  (progn
	    (close-existing-chart-windows)
	    (generate-from-mrs (mrs::fill-mrs (mrs::unfill-mrs input-sem)))
	    (show-gen-result))
	(format t "~%Could not extract valid MRS from edge ~A"
		(edge-id parser-edge))))))
|#

#|
; From ACL_specific/parseout.lsp
; For some reason, :mt on *features* doesn't put Clarify on menu, as if
; compiled without :mt present, so redefine here, same as original
(define-parse-tree-frame-command (com-multiple-tree-menu)
    ((tree 'prtree :gesture :select))
  (let ((command (clim:menu-choose
		  `(("Show enlarged tree" :value show)
                    ("Highlight chart nodes" :value chart) 
		    ("Partial chart" :value partial-chart)
                    ("Generate" :value generate :active ,*mrs-loaded*)
                    ("MRS" :value mrs :active ,*mrs-loaded*)
                    ("Prolog MRS" :value prolog :active ,*mrs-loaded*)
                    ("RMRS" :value rmrs :active ,*mrs-loaded*)
                    ("Indexed MRS" :value indexed :active ,*mrs-loaded*)
                    ("Scoped MRS" :value scoped :active ,*mrs-loaded*)
                    ("Dependencies" :value dependencies :active ,*mrs-loaded*)
                    #+:null
                    ("Transfer" :value transfer :active ,*mrs-loaded*)
                    #+:mt
                    ("Clarify" :value clarify :active ,*mrs-loaded*)
                    ))))
    (when command
      (handler-case
	  (ecase command
	    (show (draw-new-parse-tree (prtree-top tree)
				       "Parse tree" nil 
                                       (parse-tree-frame-current-chart 
                                        clim:*application-frame*)))
            (chart
             (if (or (not (parse-tree-frame-current-chart 
                           clim:*application-frame*))
                     (eql (parse-tree-frame-current-chart 
                           clim:*application-frame*)
                     *chart-generation-counter*))
                 (progn
                   (cond ((and *main-chart-frame* 
                               (eql (clim:frame-state *main-chart-frame*) 
                                    :enabled))
                          nil)
                         ((and *main-chart-frame* 
                               (eql (clim:frame-state *main-chart-frame*) 
                                    :shrunk))
                          (clim:raise-frame *main-chart-frame*))
                         (t (show-chart) 
                            (mp:process-wait-with-timeout "Waiting" 
                                                          5 #'chart-ready)))
                   (display-edge-in-chart
                    (prtree-edge tree)))
               (lkb-beep)))
	    (partial-chart
	     (if (or (not (parse-tree-frame-current-chart 
                           clim:*application-frame*))
                     (eql (parse-tree-frame-current-chart 
                           clim:*application-frame*)
                     *chart-generation-counter*))
                 (multiple-value-bind (root subframe-p)
                   (cond ((and *main-chart-frame* 
                               (eql (clim:frame-state *main-chart-frame*) 
                                    :enabled))
			  (values
			   (chart-window-root *main-chart-frame*)
			   t))
                         ((and *main-chart-frame* 
                               (eql (clim:frame-state *main-chart-frame*) 
                                    :shrunk))
			  (values
			   (chart-window-root *main-chart-frame*)
			   t))
                         (t (values (construct-chart-no-display)
				    nil)))
		   (display-partial-chart root (prtree-edge tree)
					  subframe-p))
               (lkb-beep)))
            ;; funcall avoids undefined function warnings
            (generate (funcall 'really-generate-from-edge (prtree-edge tree)))
            (mrs (funcall 'show-mrs-window (prtree-edge tree)))
            (indexed (funcall 'show-mrs-indexed-window (prtree-edge tree)))
            (prolog (funcall 'show-mrs-prolog-window (prtree-edge tree)))
            (scoped (funcall 'show-mrs-scoped-window (prtree-edge tree)))
            (rmrs (funcall 'show-mrs-rmrs-window (prtree-edge tree)))
            (dependencies 
             (funcall 'show-mrs-dependencies-window (prtree-edge tree)))
            #+:mt
            (clarify
             (let ((symbol (find-symbol "TRANSLATE" :mt)))
               (when (and symbol (fboundp symbol))
                 (funcall symbol (prtree-edge tree)))))
            #+:null
            (transfer
             (funcall 'transfer (prtree-edge tree))))
        (storage-condition (condition)
          (with-output-to-top ()
            (format t "~%Memory allocation problem: ~A~%" condition)))
	(error (condition)
	  (with-output-to-top ()
	    (format t "~%Error: ~A~%" condition)))
        (serious-condition (condition)
          (with-output-to-top ()
            (format t "~%Something nasty: ~A~%" condition)))))))
|#

#|
; Missing function definition
(defun clear-idioms-entries () nil)

; Changed get-tdfs-given-id to take (car ip) rather than ip

(defun expand-idioms-phrases nil
  (setf *idiom-phrases-expanded* nil)
  (dolist (ip *idiom-phrases*)
    (let ((iptdfs (get-tdfs-given-id (car ip))))
      (when iptdfs
        (let ((ipmrs (mrs::extract-mrs-from-fs (tdfs-indef iptdfs))))
          (when ipmrs
            (let ((ipstruct 
                   (make-idiom-phrase :id ip
                                      :tdfs iptdfs
                                      :mrs ipmrs)))
              (push ipstruct *idiom-phrases-expanded*))))))))

; Missing definition
(defun get-idiom-entry (id)
  (cdr (assoc id *idiom-phrases*)))
|#

#|
; This function is only being redefined because it was compiled with :psql
(defun index-lexicon (&key mode)
  #-:psql
  (declare (ignore mode))
  #+:psql
  (unless (eq mode :recompile)
    (format t "~% (attempting to retrieve SEM-I from LexDB)")
    (if (mrs::semi-p 
	 (catch 'pg::sql-error
	   (mrs::populate-semi-from-psql mrs::*semi*)))
	(return-from index-lexicon)
      (format t "~% (unable to retrieve database SEM-I)")))
  (format t "~% (recompiling semantic indices)")
  #+:psql
  (when (typep *lexicon* 'psql-lex-database)
    (format t "~%(caching all lexical records)")
    (cache-all-lex-records-orth *lexicon*))
  (mrs::clear-semantic-indices)
  (let ((*batch-mode* t))
    (if mrs::*top-semantics-type*
	(setf mrs::*top-semantics-entry* 
	  (get-type-entry mrs::*top-semantics-type*))
      (progn (cerror "~A will be used (indexing may be inefficient)" 
		     "~%No *top-semantics-type* defined" *toptype*)
	     (setf mrs::*top-semantics-entry*
	       (get-type-entry *toptype*))))
    (unless mrs::*top-semantics-entry*
      (error "~%No entry found for top semantics type ~A" 
	     mrs::*top-semantics-type*))
    (let ((ids-table (make-hash-table :test #'eq)) 
	  (ids nil))
      ;; because of multiple lexical entries, an id may be indexed by
      ;; multiple orthographies
      (dolist (word (lex-words *lexicon*))
	(dolist (inst (lookup-word *lexicon* word :cache nil))
	  (setf (gethash inst ids-table) t)))
      (maphash
       #'(lambda (id val) 
	   (declare (ignore val)) 
	   (push id ids))
       ids-table)
      (process-queue
       #'(lambda ()
	   (let ((id (pop ids)))
	     (if id
		 (read-psort *lexicon* id :cache nil)
	       :eof)))
       #'(lambda (entry)
	   (expand-psort-entry entry)
	   (let ((new-fs (lex-entry-full-fs entry)))
	     (if (and new-fs 
		      (not (eq new-fs :fail)))
		 (mrs::extract-lexical-relations2 entry)
	       (format t "~%No feature structure for ~A~%" 
		       (lex-entry-id entry))))
	   (unexpand-psort *lexicon* (lex-entry-id entry))))
      (mrs::check-for-redundant-filter-rules)))
  (setf *batch-mode* nil))
|#

|#
(in-package :mt)
; Added UNIFY-TYPES check for preds
(defun unify-preds (pred1 pred2 solution)
  ;;
  ;; _fix_me_
  ;; what about PREDs that stand in a subsumption relation?  presumably, we
  ;; also need to record the result somewhere, or return it?    (8-jan-04; oe)
  ;;
  (let ((pred2 (if (mrs::var-p pred2) 
                   (retrieve-variable pred2 solution)
                 pred2)))
    (cond
     ((mrs::var-p pred2)
      (forward-variable pred2 pred1 solution)
      pred1)
     ((and (stringp pred1) (stringp pred2))
      (string-equal pred1 pred2))
     ((or (eq pred1 pred2) 
          (unify-types pred1 pred2) 
          (and (null pred2) pred1))))))
|#

#|
(setf lkb::*show-transfer-p* nil)

; From src/mt/translate.lisp - added flag for showing Transfer Output window
(defun translate (edge)
  (when (lkb::edge-p edge)
    (let* ((mrs (ignore-errors (mrs::extract-mrs edge)))
           (output (transfer-mrs mrs :filterp nil)))
      (when lkb::*show-transfer-p*
        (browse-mrss output "Transfer Output"))
      (loop
          with *package* = (find-package :lkb)
          with file = (format nil "/tmp/.transfer.~a" (lkb::current-user))
          for edge in output
          for mrs = (edge-mrs edge)
          do
            ;;
            ;; _fix_me_
            ;; we need a synchronized way of talking to the generator server,
            ;; preferably a socket connection.                 (10-feb-04; oe)
            ;;
            (with-open-file (stream file
                             :direction :output :if-exists :supersede)
              (mrs::output-mrs1 mrs 'mrs::simple stream))
            (sleep 2)))))
|#
(in-package :cl-user)

