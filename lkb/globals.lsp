;;; Copyright Ann Copestake 1991-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;;
;;; This is the base globals file because parameters have to be
;;; defined somewhere, but in most cases a particular set of
;;; grammar files will have their own associated parameters

;;; Temporary hacks

(defun create-type-hierarchy-tree nil
;;; takes far too long and looks silly with ERG -
;;; fix this later
  nil)

(defun canonicalise-feature-order nil
  ;;; also taking much too long
  nil)

;;; end temporay hacks


(defparameter *display-glb-messages* nil)

;;; Efficiency for when not using defaults

(defparameter *non-default-system* t)


;;; Avoiding multiple inheritance on letypes

(defparameter *templates* 
'(lex_entry reg_lex_entry multi_lex_entry1 multi_lex_entry2
two_space_lex_entry1 two_space_lex_entry2 two_space_lex_entry3
three_space_lex_entry1 three_space_lex_entry2 
three_space_lex_entry3 three_space_lex_entry4))

;;; Strings

(defparameter *toptype* '*top*)

(defparameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; Lexical files

(defparameter *sense-unif-fn* nil)

(defparameter *orth-path* '(stem))

(defparameter *list-tail* '(rest))

(defparameter *list-head* '(first))

(defparameter *lex-rule-suffix* "_INFL_RULE")
;;; create the lexical rule name from the info in irregs.tab
;;; for TDL - value should be an upcase string - NIL otherwise
  
;;; Parsing

(defun preprocess-sentence-string (str)
  ;; replace all punctuation by spaces
  ;; except
  ;; for PAGE compatability, replace #\' by #\space
  ;; except at end of word, when replace by #\space #\s
  (let ((chars (coerce str 'list))
        (result-chars nil))
    (do* ((next-char (car chars) (car remainder))
          (remainder (cdr chars) (cdr remainder)))
         ((null next-char) nil)
         (cond ((eql next-char #\')
                (if (or (null remainder) (eql (car remainder) #\space))
                    (progn
                      (push #\space result-chars)
                      (push #\s result-chars))
                  (push #\space result-chars)))
               ((not (alphanumericp next-char)) 
                (push #\space result-chars))
               (t (push next-char result-chars))))
    (string-trim '(#\space) (coerce (nreverse result-chars) 'string))))
            
                

(defparameter *chart-limit* 100)

(defparameter *sign-type* 'sign
   "a special type wrt parsing - rule indexing is checked for its
   descendants")

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")

 
(defun establish-linear-precedence (rule-fs)
   ;;;    A function which will order the features of a rule
   ;;;    to give (mother daughter1 ... daughtern)
   ;;;    
   ;;;  Modification - this must always give a feature
   ;;;  position for the mother - it can be NIL if
   ;;; necessary
  (let* ((mother NIL)
         (daughter1 (get-value-at-end-of rule-fs '(ARGS FIRST)))
         (daughter2 (get-value-at-end-of rule-fs '(ARGS REST FIRST)))) 
    (unless (and daughter1 (not (eql daughter1 'no-way-through)))
      (cerror "Ignore it" "Rule without daughter"))
    (append (list nil '(ARGS FIRST))
            (if (and daughter2 (not (eql daughter2 'no-way-through)))
                (list '(ARGS REST FIRST))))))


  
(defparameter *start-symbol* '(root frag frag-msg fin_frag)
   "specifing valid parses")
;;; needs to be changed to allow for roots.tdl


(defparameter *morph-rule-type* 'lex_rule_infl
    "morphology system checks for rules which are
     of this type or a subtype of it")

(defun spelling-change-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which affects spelling and
;;; which should therefore only be applied by the morphology
;;; system.  
;;; Old test was for something which was a subtype of
;;; *morph-rule-type* - this tests for 
;;; < NEEDS-AFFIX > = +
;;; in the rule
  (let ((affix (get-dag-value (tdfs-indef 
                               (rule-full-fs rule)) 'needs-affix)))
    (and affix (equal (type-of-fs affix) '(+)))))

(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
;;; this version tests for 
;;; < PRODUCTIVE > = -
;;; in the rule
  (let ((affix (get-dag-value 
                (tdfs-indef (rule-full-fs rule)) 'productive)))
    (and affix (equal (type-of-fs affix) '(-)))))

(defparameter *maximal-lex-rule-applications* 7
   "The number of lexical rule applications which may be made
   before it is assumed that some rules are applying circularly")

;;; Display 

(defparameter *settings-options* nil
  "controls whether user is asked for type display options file")

(defparameter *feature-abbreviations* 
   '(("-first" . "H")
     ("-last" . "T"))
   "a list of pairs of strings - if the end of a feature name 
   matches the first string it is displayed as the 
   second string in windows.  Used to make lists more readable") 
   
(defparameter *dont-show-morphology* nil
  "if set, the morphological structures are not shown in parse trees")

(defparameter *parse-tree-font-size* 9)

(defparameter *fs-type-font-size* 9)

(defparameter *fs-title-font-size* 9)

(defparameter *type-tree-font-size* 9)

(defparameter *dialog-font-size* 12)

   
;;; Indexing


(defparameter *batch-mode* nil
   "set when indexing, could also be set by the user 
   to prevent errors in expanding a lexical entry being
   signalled as continuable errors, rather than written
   to a file.")

;;; qualia type
;;; qualia psort

(defparameter *type-indices* 
  nil)

(defparameter *parent-indices* 
  nil)

;;; Languages

(defparameter *current-language* 'English
   "Specifies the default language for the
   various interactions where a language has to
   be selected")

(defparameter *possible-languages* '(Spanish Italian Dutch English French)
   "Specifies the possible languages for interactions
   where a language has to be selected or specified")
   

;;; Warnings etc

(defparameter *warn-of-unary-branches* nil
   "If set warns of unary branches when type system is loaded")

(defparameter *do-really-well-formed-check* nil
   "If set, check lexical entries for real wel-formedness when indexing the 
   lexicon")

;; tlinks

(defparameter *tlink-syntactic-sugar* t
   "if set the slashed notation is assumed")



;;; for constraint solving and MT

(defparameter *bag-types* '(word))

(defparameter *lang-path* '(lang))

(defparameter *semantics-path* 
   (create-path-from-feature-list '(sem lizst lst hd)))

(defparameter *dummy-type* 'dummy)

(defparameter *type-of-isign* 's-sign)
(defparameter *type-of-osign* 's-sign)



(defun construct-generate-all (language)
   ;;; < > = s-sign
   ;;; < LANG > = language
   (process-unifications 
      (list 
         (make-unification :lhs
                        (create-path-from-feature-list '(lang))
                        :rhs (make-u-value :types (list language)))
         (make-unification :lhs
            (create-path-from-feature-list nil)
            :rhs (make-u-value :types (list *type-of-osign*))))))


(defun construct-orth-fs (word-list language)
   ;;; < > = s-sign
   ;;; < ORTH : HD > = "MARY"
   ;;; < ORTH : TL : HD > = "SEES" 
   ;;; < ORTH : TL : TL : HD > = "JOHN"
   ;;; < ORTH : TL : TL : TL > = e-list.
   (let ((tl-orths nil))
      (let
         ((unifications
               (when word-list
                  (cons (make-unification :lhs
                        (create-path-from-feature-list '(lang))
                        :rhs (make-u-value :types (list language)))
                     (append
                        (cons
                           (make-unification :lhs
                              (create-path-from-feature-list nil)
                              :rhs (make-u-value :types (list *type-of-isign*)))                   
                           (for poss-word in word-list
                              filter
                              (let ((word (string-upcase (string poss-word))))
                                 (push 'tl tl-orths)
                                 (make-unification :lhs
                                    (create-path-from-feature-list 
                                       (cons 'orth (append (cdr tl-orths) (list 'hd))))
                                    :rhs (make-u-value :types (list word))))))
                        (list (make-unification :lhs
                              (create-path-from-feature-list 
                                 (cons 'orth tl-orths))
                              :rhs (make-u-value :types (list 'e-list)))))))))
         (when unifications (process-unifications unifications)))))

(defparameter *top-variable-type* 'entity)
;;; need to know what types variables are in order to do "SKolemisation"

(defparameter *top-lex-sign-type* 'word)

(defparameter *feature-ordering*
   '(dtrs synsem orth head-dtr comp-dtrs hcs-synsems hcs-orth hd-dtr
      tl-dtr synsem-list))

;;; YADU

(defparameter *lexical-persistence* 'lex
   "Atom marking lexical persistence of tails")

(defparameter *rule-persistence* nil
   "Atom marking persistence of tails in rules")

;;; Linking

(defparameter *bc96lrules* nil)

(defparameter *linking-type* 'linking-type
   "Type from which linking types all inherit")



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
