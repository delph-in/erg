;;; Copyright Ann Copestake 1991-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;;
;;; LinGO grammar specific globals file
;;; parameters only - grammar specific functions 
;;; should go in user-fns.lsp
;;; patches in lkb-code-patches.lsp

;;; Avoiding multiple inheritance on letypes

(defparameter *templates* 
'(lex_entry reg_lex_entry multi_lex_entry1 multi_lex_entry2
two_space_lex_entry1 two_space_lex_entry2 two_space_lex_entry3
three_space_lex_entry1 three_space_lex_entry2 
three_space_lex_entry3 three_space_lex_entry4)
   "types which are treated as templates to avoid excessive glbs")

;;; Strings

(defparameter *toptype* '*top*)

(defparameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; Lexical files

(defparameter *orth-path* '(stem))

(defparameter *list-tail* '(rest))

(defparameter *list-head* '(first))

(defparameter *lex-rule-suffix* "_INFL_RULE"
 "creates the inflectional rule name from the information
   in irregs.tab - for PAGE compatability")

;;; Parsing

(defparameter *chart-limit* 100)

(defparameter *sign-type* 'sign
   "a special type wrt parsing - rule indexing is checked for its
   descendants")

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")

(defparameter *start-symbol* '(root frag frag-msg fin_frag)
   "specifing valid parses")
;;; needs to be changed to allow for roots.tdl


(defparameter *morph-rule-type* 'lex_rule_infl
    "morphology system checks for rules which are
     of this type or a subtype of it")

(defparameter *maximal-lex-rule-applications* 7
   "The number of lexical rule applications which may be made
   before it is assumed that some rules are applying circularly")

(defparameter *deleted-daughter-features* '(ARGS HEAD-DTR NON-HEAD-DTR)
   "features pointing to daughters deleted on building a constituent")

(defparameter *head-marking-path* '(SYNSEM LOCAL CONT KEY)
   "coreferenced between mother and head daughter")


;;; Parse tree node labels

;;; the path where the name string is stored
(defparameter *label-path* '(LABEL-NAME))

;;; the path for the meta prefix symbol
(defparameter *prefix-path* '(META-PREFIX))

;;; the path for the meta suffix symbol
(defparameter *suffix-path* '(META-SUFFIX))

;;; the path for the recursive category
(defparameter *recursive-path* '(NON-LOCAL SLASH LIST FIRST))

;;; the path inside the node to be unified with the recursive node
(defparameter *local-path* '(LOCAL))

;;; the path inside the node to be unified with the label node
(defparameter *label-fs-path* '(SYNSEM))

(defparameter *label-template-type* 'label)
