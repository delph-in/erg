;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   $RCSfile$
;;  $Revision$
;;      $Date$
;;     Author: Walter Kasper (DFKI)
;;    Purpose: 
;;   Language: Allegro Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Log$
;; Revision 1.1.2.1  1997/12/16 03:13:06  malouf
;; Convert grammar to use page 2.1
;;
;; Revision 1.1  1997/12/06 20:32:05  malouf
;; Added cslipage.
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MRS")

(defvar *MRS-RESULTS-CHECK* nil)

(defvar *MRS-FOR-LANGUAGE* 'german)

(defvar *MRS-TO-VIT* t)

(defvar *MRS-SCOPING* nil)

;;; to be done: use (tdl::show-current-domain) rather than hardwire
;;; domain-package 
;;; the defaults are for German

(defvar *initial-semantics-path* 
  '(DISCO::SYNSEM DISCO::LOC DISCO::CONT)
  "Following this path gets you to the MRS structure")

(defvar *rel-name-path*
    '(DISCO::PRED)
  "path to get the predicate name if it exists")

(defvar *psoa-top-h-path* 
  '(DISCO::TOP-H)
  "path to get the top handle from a psoa")

(defvar *psoa-handel-path* 
  '(DISCO::HANDEL)
  "path to get a handle from a psoa")

(defvar *psoa-label-path* 
  '(DISCO::LABEL)
  "path to get a handle from a psoa")

(defvar *psoa-event-path* 
  '(DISCO::INDEX)
  "path to get an event from a psoa")

(defvar *psoa-liszt-path* 
    '(DISCO::LISZT DISCO::LIST)
  "path to get a liszt from a psoa")

(defvar *psoa-rh-cons-path*
    '(DISCO::H-CONS DISCO::LIST)
  "path to get a list of handle constraints from a root psoa")

(defvar *psoa-h-cons-path*
    '(DISCO::H-CONS DISCO::LIST)
  "path to get a list of handle constraints from a psoa")

(defvar *psoa-constr-path* 
  '(DISCO::SC-ARG)
  "path to get a handle from a psoa")

(defvar *psoa-message-path* 
  '(DISCO::MESSAGE)
  "path to get a handle from a psoa")

(defvar *liszt-first-path* 
  '(DISCO::FIRST)
  "path for first element in a liszt")

(defvar *liszt-rest-path* 
  '(DISCO::REST)
  "path for rest of a liszt")

(defvar *psoa-wgliszt-path*
    '(DISCO::WGLISZT DISCO::LIST)
  "path to get the word lattice identifiers from a psoa")

(defvar *value-feats* '(DISCO::VREF DISCO::VTYPE
                              DISCO::MONTH DISCO::DAY DISCO::HOUR 
			       DISCO::MINUTE DISCO::ORD 
                              DISCO::NAMED DISCO::PRED DISCO::DEMONTYPE)
   "A list of features within an MRS that takes constant values
   instead of variables")

(defvar *feat-priority-list*  '( DISCO::HANDEL DISCO::INDEX
				       DISCO::EVENT DISCO::INST DISCO::ACT
				       DISCO::BV DISCO::RESTR DISCO::SCOPE 
				       DISCO::QUANT)
  "A not-necessarily-complete list of features that determines printing
order in an MRS")

(defvar *do-not-convert-sort-list* nil
  "relations which will be ignored in the conversion process")

(defvar *relation-extra-feats* nil
  "A list of features in rel-structures containing additional information")

(defvar *ignored-sem-features* nil
  "A list of features which are ignored completely")

(defvar *sem-sort-suffix* nil
  "a suffix string marking semantic sorts")

(defvar *sem-relation-suffix* nil
  "a suffix string in relation names (removed in VITS)")

(defvar *sem-relation-prefix* nil
  "a prefix string in relation names (removed in VITs)")

;;; at present only simple feature-value-pairs are treated (no complex values)

(defvar *index-feature-transform-table* nil
  "an assoc list of entries (Feature VIT-accessor (Value . VIT-Values)
VIT-Values is a list of pairs (VIT-special-form  VIT-value)
a default special form can be specified by (t VIT-special-form)
Value is then used as it is; otherwise missing values are ignored
a default value can be specified by '(others (VIT-special-form defaultvalue))'
TDL-type-checking is invoked on entries '(type Supertype VIT-Values)'
Search order is left to right
The distinction between simple Value and type checking is used to filter out
  certain Values which either have a different representation in VITs or none
 at all e.g. underspecified values")

(defvar *relation-extra-transform-table* nil
  "an assoc list similar to that of *index-feature-transform-table*
for extra features for using the extra-features of a relation")

(defvar *vit-sort-feature* 'DISCO::SORT
  "name of features for fs-sort slot")

(defvar *vm-top-sort-symbol* 'DISCO::ANYTHING
  "top sort in the VM ontology")

(defvar *vm-ignored-sort-list* (list *vm-top-sort-symbol*)
  "list of sorts which should not enter vits")

(defvar *vm-ignored-sentence-mood* '(DISCO::MESSAGE)
  "list of sentence moods (message-types) which should be ignored in VIT")

(defvar *mrs-arg-features* nil
  "assoc-list of arg-features with default-VIT-roles")

;;; spezielle Typen
(defvar *special-type-info* nil
  "assoc-list for type-value info")

(defvar *special-type-treatment* nil
  "assoc-list (type . function) ; function takes rel, vit, groups, labels and
  should return the vit")

(defvar *relation-type-check* nil
  "assoc list '(supertype accessor VIT-Values)'")

;;; *vm-special-label-hack-list*
;;; assoc list of (relation . arg); rg is a number (the nth element in flist); 
;;; at present this seems to be more robust than the use of feature
;;; names; arg refers to a label which according to spme unaccountable
;;; principles of transfer must be base label

(defvar *vm-special-label-hack-list*
    '((DISCO::support_rel . 1)
    (DISCO::nominalize-rel . 1)
    (DISCO::nominalize_rel . 1)
    (DISCO::support-rel . 1)))
