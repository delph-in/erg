;;; Copyright Ann Copestake 1991-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;;
;;; LinGO grammar specific globals file
;;; parameters only - grammar specific functions 
;;; should go in user-fns.lsp
;;; patches in lkb-code-patches.lsp

;;; Avoiding multiple inheritance on letypes

(defparameter *active-parsing-p* t)

;;; Strings

(defparameter *toptype* '*top*)

(defparameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; Lexical files

(defparameter *orth-path* '(stem))

(defparameter *list-tail* '(rest))

(defparameter *list-head* '(first))

(defparameter *empty-list-type* '*null*)

(defparameter *list-type* '*list*)

(defparameter *diff-list-type* '*diff-list*)

(defparameter *diff-list-list* 'list)

(defparameter *diff-list-last* 'last)

(defparameter *lex-rule-suffix* "_INFL_RULE"
 "creates the inflectional rule name from the information
   in irregs.tab - for PAGE compatability")

(defparameter *irregular-forms-only-p* t)

;;;

(defparameter *display-type-hierarchy-on-load* nil)

;;; Parsing

(defparameter *chart-limit* 100)

(defparameter *maximum-number-of-edges* 4000)

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")

(defparameter *start-symbol* '(root_strict)
   "specifing valid parses")
;; Use the following for parsing fragments as well as full clauses:
#|
(defparameter *start-symbol* '(root_strict root_lex root_phr root_conj root_subord)
  "specifing valid parses including fragments")
|#

(defparameter *maximal-lex-rule-applications* 7
   "The number of lexical rule applications which may be made
   before it is assumed that some rules are applying circularly")

(defparameter *deleted-daughter-features* 
  '(ARGS HEAD-DTR NON-HEAD-DTR LCONJ-DTR RCONJ-DTR DTR MOD-DTR NONMOD-DTR)
  "features pointing to daughters deleted on building a constituent")

#+:packing
(defparameter *packing-restrictor*
  '(RELS HCONS)
  "restrictor used when parsing with ambiguity packing")

;;; (setf *chart-packing-p* :t)

;;;
;;; increase dag pool size
;;;

(defparameter *dag-pool-size* 200000)
(defparameter *dag-pool*
  (if (and (pool-p *dag-pool*) 
           (not (= (pool-size *dag-pool*) *dag-pool-size*)))
    (create-pool *dag-pool-size* #'(lambda () (make-safe-dag-x nil nil)))
    *dag-pool*))

;;; Parse tree node labels

;;; the path where the name string is stored
(defparameter *label-path* '(LABEL-NAME))

;;; the path for the meta prefix symbol
(defparameter *prefix-path* '(META-PREFIX))

;;; the path for the meta suffix symbol
(defparameter *suffix-path* '(META-SUFFIX))

;;; the path for the recursive category
(defparameter *recursive-path* '(SYNSEM NON-LOCAL SLASH LIST FIRST))

;;; the path inside the node to be unified with the recursive node
(defparameter *local-path* '(SYNSEM LOCAL))

;;; the path inside the node to be unified with the label node
(defparameter *label-fs-path* '())

(defparameter *label-template-type* 'label)

;;; for the compare function 

(defparameter *discriminant-path* '(synsem local keys key))

;;; Hide lexical rule nodes in parse tree
;;; (setf  *dont-show-lex-rules* t)
;;; this belongs in the user-prefs file, not here

(defparameter *duplicate-lex-ids* 
  '(AN will_aux_neg_1 would_aux_neg_1 do1_neg_1 hadnt_aux_1 hadnt_aux_subj_1
    hasnt_aux_1 have_fin_aux_neg_1 be_c_is_neg_1  be_id_is_neg_1
    be_th_cop_is_neg_1 might_aux_neg_1 must_aux_neg_1 need_aux_neg_1
    ought_aux_neg_1 should_aux_neg_1 be_id_was_neg_1 be_th_cop_was_neg_1
    be_c_was_neg_1 be_c_were_neg_1 be_id_were_neg_1 be_th_cop_were_neg_1
    will_aux_neg_1 would_aux_neg_1)
  "temporary expedient to avoid generating dual forms")

(setf *semantics-index-path* '(synsem local cont hook index))

(setf *intersective-rule-names* '(adjh_i adjn_i nadj_i adjh_i_ques nadj_rc
                                  nadj_rr_nt nadj_rr_t hadj_i_uns))

(defparameter *chart-dependencies*
  '((SYNSEM LOCAL KEYS --+COMPKEY) (SYNSEM LOCAL KEYS KEY)
    (SYNSEM LOCAL KEYS --+OCOMPKEY) (SYNSEM LOCAL KEYS KEY)))