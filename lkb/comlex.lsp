;; A simple interface to comlex for the lingo gramamr
;;
;;  Loads a comlex database table from:
;;
;;    grammar/lkb/comlex
;;
;;  To build a new database:
;;
;;    (build-comlex "~malouf/comlex/comlex.raw")))
;;


(defparameter *comlex-db-file* 
    (merge-pathnames #p"comlex" (this-directory)))

;;***************************************************************************
;; Utilities.

(defmacro has-featurep (elt list)
  `(member ,elt ,list :key #'car :test #'eq))

(defmacro fcase (keyform &rest clauses)
  (let ((feats (gensym)))
    `(let ((,feats (getf (cdr ,keyform) :features)))
       (cond ,@(loop for clause in clauses
		   collecting 
		     (if (or (eq (car clause) 'otherwise)
			     (eq (car clause) t))
			 `(t ,@(cdr clause))
		       `((has-featurep (quote ,(car clause)) ,feats)
			 ,@(cdr clause))))))))

(defmacro select-subcats (keyform &rest clauses)
  (let ((subcat (gensym))
	(result (gensym)))
    `(let ((,subcat (getf (cdr ,keyform) :subc))
	   (,result nil))
       ,@(loop for clause in clauses
	     collecting 
	       `(when (member (list (quote ,(car clause)))
			      ,subcat :test #'equalp)
		  (setq ,result (append ,result ,@(cdr clause)))))
       ,result)))

;;***************************************************************************
;; Process comlex file into a morphological database.  Each entry in
;; the database is a list of triples: the part of speech, the instance
;; type, and the relation type.

(defvar *processed* 0)
(defvar *total* 0)

(defun build-comlex (infile) 
  (let ((outfile *comlex-db-file*)
	(*total* 0)
	(*processed* 0))
    (when (probe-file outfile)
      (delete-file outfile))
    (let ((cdb (cdb:open-write outfile)))
      (with-open-file (instream infile :direction :input)
        (do ((entry (read instream nil '*eof*)
		    (read instream nil '*eof*)))
	    ((eq entry '*eof*))
	  (store-entry entry cdb)))
      (cdb:close-write cdb))
    (format t "~%~d subcat frames processed out of ~d = ~4f%~%" 
	    *processed* *total* (* 100 (/ *processed* *total*)))))

(defun store-entry (entry stream)
  (let ((key (getf (cdr entry) :orth))
	(type (compute-type entry)))
    (if (getf (cdr entry) :subc)
	(incf *total* (length (getf (cdr entry) :subc)))
      (incf *total*))
    (when type
      (incf *processed* (length type))
      (cdb:write-record stream (string-upcase key)
			(write-to-string type)))))

(defun compute-type (entry)
  (case (car entry)
    (noun 
     (fcase entry
	    (ntitle '((:noun title_le title_rel)
		      (:noun intr_noun_word_le reg_nom_rel)))
	    (countable '((:noun noun_ppof_word_le reg_diadic_nom_rel)))
	    (otherwise '((:noun massn_le reg_nom_rel)))))
    (adverb '((:adverb adv_word_le adv_rel)))
    (adjective 
     (cons '(:adjective intrans_adj_le adj_rel)
	   (select-subcats entry
			   (adj-to-inf '((:adjective reg_adj_equi_le 
						     adj_arg4_rel))))))
    (verb
     (select-subcats entry
		     (intrans '((:verb mv_unerg_le arg1_rel)))
		     (np '((:verb mv_np_trans_le arg13_rel)))
		     (np-to-np '((:verb mv_ditrans_le arg123_rel)))
		     (s '((:verb mv_cp_non_trans_le arg14_rel)))))))

;;*************************************************************************** 
;; Given a string, pull in all the lexical entries with that string in
;; the STEM.  Then pull in the definitions for the relations referred
;; to by those entries.  Define the types (if not already done), and
;; then define the instances.  We return a list of newly defined
;; instances.

(defclass comlex-database (lex-database)
  ((comlex-db :initform nil :accessor comlex-db)))

(defmethod clear-lex ((lexicon comlex-database) &optional no-delete)
  (declare (ignore no-delete))
  ;; Close temporary lexicon files
  (when (comlex-db lexicon)
    (cdb:close-read (comlex-db lexicon))
    (setf (comlex-db lexicon) nil)))

(defmethod lookup-word ((lexicon comlex-database) orth &key (cache t))
  (declare (ignore cache))
  (unless (comlex-db lexicon)
    (setf (comlex-db lexicon) 
      (cdb:open-read *comlex-db-file*)))
  (let ((record (cdb:read-record (comlex-db lexicon) orth))
	(instances nil))
    (when record
      (dolist (entry (mapcan #'read-from-string record))
	(let ((instname (intern 
			 (concatenate 'string "X_" orth
				      "_" (string (second entry)))))
	      (relname (intern
			(concatenate 'string "_" orth "_" 
				     (string (first entry))
				     "_REL"))))
	  (add-relation relname (third entry))
	  (store-temporary-psort *lexicon*
				 instname
				 (make-lex-or-psort
				  :orth (list (string-downcase orth))
				  :infl-pos nil
				  :sense-id instname
				  :id instname
				  :unifs (make-unifs orth relname entry)))
	  #+:mrs
	  (let ((entry (get-psort-entry instname)))
	    (when (lex-or-psort-full-fs entry)
	      (mrs::extract-lexical-relations entry)))
	  (push instname instances))))
    instances))

(defun add-relation (rel supertype)
  (unless (get-type-entry supertype)
    (error "Attempt to add entry with invalid relation type ~A" supertype))
  (let ((entry (make-leaf-type :name rel 
			       :parents (list supertype)
			       :daughters nil
			       :constraint-spec nil
			       :default-spec nil
			       :enumerated-p nil)))
    (pushnew rel *type-names* :test #'eq)
    (pushnew rel (slot-value *leaf-types* 'leaf-types) :test #'eq)
    (set-type-entry rel entry)
    (add-in-leaf-type-entry entry)))
    
(defun make-unifs (key rel entry)
  (list
   ;; Top level type
   (make-unification
    :lhs (make-path :typed-feature-list nil)
    :rhs (make-u-value :types (list (second entry))))
   ;; Orthography
   (make-unification
    :lhs (make-path :typed-feature-list '(stem first))
    :rhs (make-u-value :types (list (string-downcase key))))
   (make-unification
    :lhs (make-path :typed-feature-list '(stem rest))
    :rhs (make-u-value :types (list '*null*)))
   ;; Content
   (make-unification
    :lhs (make-path :typed-feature-list '(synsem local keys key))
    :rhs (make-u-value :types (list rel)))))
   




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:load-toplevel)
  (when (probe-file *comlex-db-file*)
    (format t "~%Loading comlex database.")
    (setf (extra-lexicons *lexicon*) 
      (list (make-instance 'comlex-database)))))
    
    

