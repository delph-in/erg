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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For 01-may-04

; In to-db method in src/lexdb/lexport.lsp, changed last argument to
; extract-field2 for orth-list from "list" to 'list, so it doesn't break.
; Also made the last conditional always t since the length test doesn't work.

(in-package :lkb)
(defmethod to-db ((x lex-entry) (lexicon psql-lex-database))  
  (let* ((fields-map (fields-map lexicon))

	 (keyrel (extract-field x :keyrel fields-map))      
	 (keytag (extract-field x :keytag fields-map))
	 (altkey (extract-field x :altkey fields-map))
	 (altkeytag (extract-field x :altkeytag fields-map))
	 (alt2key (extract-field x :alt2key fields-map))
	 (compkey (extract-field x :compkey fields-map))
	 (ocompkey (extract-field x :ocompkey fields-map))
         ; DPF changed "list" to 'list	 	 
	 (orth-list (extract-field2 x :orth nil 'list))
	 (name (extract-field x :name fields-map))
	 (count (+ 2 (length orth-list)))
	 (total (+ count
		   (if (string> keyrel "") 1 0) 
		   (if (string> keytag "") 1 0) 
		   (if (string> altkey "") 1 0)
		   (if (string> altkeytag "") 1 0)
		   (if (string> alt2key "") 1 0) 
		   (if (string> compkey "") 1 0) 
		   (if (string> ocompkey "") 1 0)))
	 
	 (type (extract-field x :type fields-map))
	 
	 (psql-le
	  (make-instance-psql-lex-entry
	   :name name
	   :type type
	   :orthography orth-list	;list
	   :orthkey (get-orthkey orth-list)
	   :keyrel keyrel
	   :altkey altkey
	   :alt2key alt2key
	   :keytag keytag
	   :altkeytag altkeytag
	   :compkey compkey
	   :ocompkey ocompkey
	   :country *postgres-current-country*
	   :lang *postgres-current-lang*
	   :source (extract-pure-source-from-source *postgres-current-source*)
	   :confidence 1
	   :flags 1
	   )
	  ))
    (cond
     ;;; DPF hack - disabled length test which seems to always be wrong
     ((or t (= total (length (lex-entry-unifs x))))
      (set-lex-entry lexicon psql-le)
      (empty-cache lexicon))
     (t
      (format t "~%skipping super-rich entry: `~a'~%"  name)
      nil))))

(in-package :cl-user)