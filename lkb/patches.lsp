;;;
;;; a couple of _temporary_ patches to LKB system code for better generation
;;;                                                           (9-dec-03; oe)
;;;

(in-package :lkb)

; In mrs/idioms.lisp
; Added check in idiom_rel-p() since mt::transfer-mrs() is surprised at
; finding a predicate name as value of ARG1 for degree specifiers of
; quantifiers (as in "almost every") and assigns a "u" type variable,
; which this function did not expect as value of PRED.
(in-package :lkb)
(defun idiom-rel-p (rel)
  ;; FIX
  ;; relation name ends with _i or -i -- this won't quite do because
  ;; we want to allow for different senses and anyway this should use the
  ;; standard pred parsing code
  (let* ((relpred (mrs::rel-pred rel))
         (relname (when (and relpred 
                             (or (symbolp relpred) (stringp relpred)))
                    (string relpred))))
    (and relname
         (or 
          (eql (mismatch "_i" relname :from-end t :test #'char-equal) 0)
          (eql (mismatch "-i" relname :from-end t :test #'char-equal) 0)))))

; In lkb/src/lexdb/headers.lsp
; Added "5" to load-foreign-types for 64-bit
#-:lkb-fos
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

;; DPF 2011-feb-27
;; Recent versions of Postgres (since 2010) are not happy with the string
;; value "*" as a variable over columns, as given for reqd-flds in e.g. 
;; retrieve-entry2().  So replaced these values '("*") with '(*) in four
;; functions in lkb/src/lexdb files as below (removing the double quotes).
;; (The same change would probably be needed in psql-lex-database2.lsp for
;; single-user users.)

(in-package :lkb)

;; In psql-lex-database.lsp
(defmethod retrieve-entry2 ((lex mu-psql-lex-database) name &key (reqd-fields '(*)))
  (let ((qname (psql-quote-literal name)))
    (get-records lex
		 (format nil
			 "SELECT ~a FROM (SELECT rev.* FROM public.rev as rev JOIN lex_cache USING (name,userid,modstamp) WHERE lex_cache.name = ~a UNION SELECT rev.* FROM rev JOIN lex_cache USING (name,userid,modstamp) WHERE lex_cache.name = ~a) as foo"
			 (fields-str lex reqd-fields)
			 qname qname))))

;; In psql-lex-database0.lsp
(defmethod retrieve-raw-record-no-cache ((lex psql-lex-database) id &optional (reqd-fields '(*)))
  (unless (connection lex)
    (format t "~&(LexDB) WARNING:  no connection to psql-lex-database")
    (return-from retrieve-raw-record-no-cache))
  (retrieve-entry2 lex (2-str id) :reqd-fields reqd-fields))

;; Also in psql-lex-database0.lsp
(defmethod get-dot-lex-record ((lex psql-lex-database) id &optional (fields '(*)))
  (let ((table (retrieve-raw-record-no-cache lex id fields)))
    (dot (cols table) (car (recs table)))))

;; And finally in psqllex.lsp, special handling for asterisk value
(defmethod fields-str ((lex psql-lex-database) fields)
  (if (and (consp fields) (eq (first fields) '*))
    '*
    (concat-str
     (mapcar #'(lambda (x) (quote-ident lex x))
	     fields)
     :sep-c #\,)))


;; Avoid bogus complaint about PSQL server version - now outdated information
(defmethod check-psql-server-version ((lex mu-psql-lex-database))
  t)

;; For LexDB, when dumping the database to lexdb.rev file, the final command
;; pq:endcopy now apparently returns "1" for okay, where it used to return "0"
;; (or perhaps something is actually going wrong with pq:endcopy but does not
;; cause any visible bad effects).  So just add "1" as a possible okay output:
;; In psql-database.lsp

(defun endcopy (conn)
  (unless (or (= 0 (pq:endcopy conn))
	      (= 1 (pq:endcopy conn)))
    ;;fix_me
    ;;(format t "~%PSQL ~a" error-message)
    (throw :sql-error (cons :putline "endcopy failed"))))

;; Enable LUI chart display to use abbreviated rule names
;;
(defun lui-chart-edge-name (edge)
  (let* ((rule (edge-rule edge))
         (rname
           (when (rule-p rule)
             (existing-dag-at-end-of 
               (tdfs-indef (rule-full-fs rule)) '(RNAME)))))
    (format nil "~a[~a]"
      (cond 
        ((not (edge-children edge)) 
          (let ((le (get-lex-entry-from-id (first (edge-lex-ids edge)))))
            (dag-type (tdfs-indef (lex-entry-full-fs le)))))
        (rname (dag-type rname))
        (t
          (tree-node-text-string (find-category-abb (edge-dag edge)))))
      (edge-id edge))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Useful utilities provided by John Carroll, October 2022

;------------------------------------------------------------------------------
;; Subsumption test - does edge n1 subsume edge n2, and if not, what path fails
;;
(defun check-subsume-edges (n1 n2)
  (let ((e1 (find-edge-given-id n1))
        (e2 (find-edge-given-id n2)))
    (unless (and e1 e2) (error "Could not find one or both edges in chart"))
    (check-dag-subsumes-p (tdfs-indef (edge-dag e1)) (tdfs-indef (edge-dag e2)) t t)))

(defun check-dag-subsumes-p (dag1 dag2 &optional (forwardp t) (backwardp nil))
  (with-unification-context (dag1)
    (check-subsume-wffs-p dag1 dag2 forwardp backwardp nil)))

(defun check-subsume-wffs-p (dag1 dag2 forwardp backwardp path)
  ;; forwardp, backwardp are true when it's possible that dag1 subsumes dag2
  ;; and vice-versa respectively. When the possibility has been ruled out the
  ;; appropriate variable is set to false. Fail as soon as they are both false.
  (labels
    ((check-subsume-wffs-p-aux (dag1 dag2 path &aux donep)
      ;; dag-p tests allow compiler to elide type checks in dag slot accessors below
      (unless (and (dag-p dag1) (dag-p dag2))
        (error "Inconsistency - ~S called with a non-dag argument" 'check-subsume-wffs-p-aux))
      ;; donep improves on the original algorithm, avoiding repeated processing below
      ;; a pair of nodes we've visited previously due to reentrancies
      (when forwardp
        (let ((c1 (dag-copy dag1)))
          (cond
            ((null c1)
              (setf (dag-copy dag1) dag2))
            ((eq c1 dag2)
              (setq donep t))
            (t (format t "~&Reentrancy at ~A - not forward~%" (reverse path))
               (unless backwardp (return-from check-subsume-wffs-p nil))
               (setq forwardp nil)))))
      (when backwardp
        (let ((c2 (dag-comp-arcs dag2))) ; can't also use copy slot in case dags share nodes
          (cond
            ((null c2)
              (setf (dag-comp-arcs dag2) dag1))
            ((eq c2 dag1)
              (setq donep t))
            (t (format t "~&Reentrancy at ~A - not backward~%" (reverse path))
               (unless forwardp (return-from check-subsume-wffs-p nil))
               (setq backwardp nil)))))
      (cond
        (donep)
        ((eq dag1 dag2)
          ;; when the dags are eq we still need to traverse them to check reentrancies,
          ;; but other processing can be bypassed
          (dolist (arc (dag-arcs dag1))
            (check-subsume-wffs-p-aux
              (dag-arc-value arc) (dag-arc-value arc) (cons (dag-arc-attribute arc) path))))
        (t
          (let ((type1 (dag-type dag1))
                (type2 (dag-type dag2)))
            (unless (or (eq type1 type2)
                        (and (stringp type1) (stringp type2) (string= type1 type2)))
              (let ((gcs (greatest-common-subtype type1 type2)))
                (cond
                  ((eq gcs type1)
                     (format t "~&Types ~A ~A at ~A - not forward~%" type1 type2 (reverse path))
                     (unless backwardp (return-from check-subsume-wffs-p nil))
                     (setq forwardp nil))
                  ((eq gcs type2)
                     (format t "~&Types ~A ~A at ~A - not backward~%" type1 type2 (reverse path))
                     (unless forwardp (return-from check-subsume-wffs-p nil))
                     (setq backwardp nil))
                  (t (format t "~&Types ~A ~A at ~A - no common subtype~%" type1 type2 (reverse path))
                     (return-from check-subsume-wffs-p nil)))))
            (let* ((arcs2 (dag-arcs dag2))
                   (arcs2-tail arcs2))
              (dolist (arc1 (dag-arcs dag1))
                (let ((f1 (dag-arc-attribute arc1)))
                  (block subsume-arc
                    (do ((tail arcs2-tail (cdr tail))) ; start just beyond previous match
                        ((atom tail))
                        #1=(when (eq (dag-arc-attribute (car tail)) f1)
                             (check-subsume-wffs-p-aux
                               (dag-arc-value arc1) (dag-arc-value (car tail))
                               (cons f1 path))
                             (setq arcs2-tail (cdr tail))
                             (return-from subsume-arc)))
                    (do ((tail arcs2 (cdr tail)))
                        ((eq tail arcs2-tail))
                        #1#))))))))))
    (check-subsume-wffs-p-aux dag1 dag2 path)
    (values forwardp backwardp)))
