(in-package "TDL-CONTROL")

;;;
;;; (trivial) patches to TDL that for some reason i could never massage into
;;; the official system |:-{.                             (30-oct-97  -  oe)
;;;

(defparameter *verbose-definition-p* t
  "*verbose-definition-p* controls the tracing of definitions (`#Avm<foo>').")

(DEFUN  DEFINE-FEATURE-TYPE  (type input-form
  &KEY surface
       occur				; all occurences of `value types'
       restriction			; all occurences of `restriction types'
       atomic-symbols
       attributes
       (expand-control :unknown)
       (overwrite-values  ())
       (overwrite-paths   ())
       (doc  *DEFAULT-DOCUMENTATION*)
       (author  *DEFAULT-AUTHOR*)
       (domain  (SHOW-CURRENT-DOMAIN))
       (intermediate-type-p NIL)
       (date  (MULTIPLE-VALUE-BIND (second minute hour day month year)
		  (GET-DECODED-TIME)
		(DECLARE (TYPE FIXNUM  second minute hour day month year))
		(FORMAT NIL
			"The type ~S has been defined on ~2,'0D/~2,'0D/~D at ~2,'0D:~2,'0D:~2,'0D"
			type month day year hour minute second)))
       (status :unknown)
  &AUX (form (SUBLIS (TDL-parse::give-translation-table)    ;;; replace corefs
		     input-form 
		     :test #'EQ))
       (simplified-form  (FIRST form))	                    ;;; extract the pure feature term
       (*FUNCTIONAL-BINDINGS*		                    ;;; extract functional constraints
	(CONS :FUNC-CONSTR 
	      (SECOND (ASSOC :func-constr 
			     (REST form))))) 
       (*SPLICE-COREFS*			; dito for corefs
        (Collect-Values (SECOND (ASSOC :values (REST form)))))
       (*INTERMEDIATE-BINDINGS* '())	;;; this is a global parameter which is locally modified
       (*USE-HIERARCHY-P* NIL)          ;;; during the definition of a FT, this global is always turned off
       (*AND-OPEN-WORLD-REASONING-P* T) ;;; during the definition of a FT, this global is always turned on
       term-node			;;; get its value by BUILD-SKELETON
       var-bindings			;;; dito
       func-constr			;;; dito
       infon
       supers				;;; the superclasses of type
       elements				;;; the disjunction elements in case of a disjunctive definition
       prev-rec-p			;;; is there a former recursive type with the same name
       redef-types			;;; is used in case of an exhaustive redefinition
       (not-defined-types '())
       (prev-enc-p (ENCODED-P type domain))
       (overwrite-value ())  ;;; SIC !!
       (monotonic-p  (IF (NULL overwrite-paths) T NIL))
       (sort-p NIL))
 
  (DECLARE (TYPE SYMBOL  status)
	   (TYPE SYMBOL  type)
           (TYPE SIMPLE-STRING  surface doc author domain date)
           )                                                      ;;; missing declarations

  (UNLESS (OR (EQ (FIRST *BEGIN-END-WHAT*) :TYPE)
              intermediate-type-p)
    (RETURN-FROM DEFINE-FEATURE-TYPE
      (WARN "  >> Type definitions not allowed in ~S environment. Use BEGIN :TYPE." 
	    (FIRST *BEGIN-END-WHAT*))))
  
  (WHEN (OR (BUILT-IN-P type)
	    (EQ type (GET-TOP-SYMBOL domain))
	    (EQ type (GET-BOTTOM-SYMBOL domain)))
    (RETURN-FROM DEFINE-FEATURE-TYPE
      (WARN "  >> ~S is a predefined built-in type---it cannot be redefined." 
	    type)))

  (WHEN *WARN-IF-REDEFINE-TYPE*   ;;; this global variable must _only_ be checked at this point
    (WHEN (AVM-EXISTS-P type domain)
      (WARN "  >> ~S was previously defined as an AVM type in domain ~S." 
	    type domain))
    
    (WHEN (SORT-EXISTS-P type domain)
      (WARN "  >> ~S was previously declared or defined as a SORT type in domain
 ~S." 
            type domain))
    
    (WHEN (ATOM-EXISTS-P type domain)
      (WARN "  >> An ATOM with the name ~S was already used in domain ~S." 
	    type domain)))

  (WHEN (AND *WARN-IF-OVERWRITE-ATTRIBUTE-VALUE*   ;;; this global variable must _only_ be checked at this point
             (NOT monotonic-p))
    (WARN "  >> SLO Overwriting --- The type ~S is defined non-monotonically." 
	  type))

  (WHEN prev-enc-p                                                    ;;; it is necessary to clear the memo
    (Clear-Simplify-Memo-Tables :domain domain)	                      ;;; tables when a class is redefined in
    (WHEN *VERBOSE-P*			                              ;;; order not to get false results (older
      (WARN "  >> Clearing all MEMO tables: ~S will be reencoded."    ;;; inferences may not longer be valid)
	    type))
    (SETQ prev-rec-p
      (ENC::RECURSIVE-CLASS-P type :domain domain))
    (WHEN *EXHAUSTIVE-REDEFINITION-P*
      (SETQ redef-types                          ;;; these redef-types must be defined at the end of
	(Redefine-Feature-Type type domain))))	 ;;; this function
  
  (WHEN (AVM-EXISTS-P type domain)                 ;;; when redefining a feature structure type, it
    (DOLIST (scc (ENC::Compute-SCC type domain))   ;;; is necessary to delete the prototypes of all
      (DOLIST (s scc)				   ;;; dependents of the type in question, more exactly,
	(UNLESS (ENC::Is-Bottom s)                 ;;; to set the prototype slot of the infon to the
          (Reset-protos s domain)                  ;;; skeleton slot as it is done for a totally new type
          (WHEN *VERBOSE-P*
            (WARN "  >> Deleting the prototype of ~S."
		  s))))))

  (Check-for-Sort-Clashes simplified-form domain)   ;;; see description of Check-for-Sort-Clashes above
  
  (UNLESS (AND (AND-P simplified-form)
	       (SOME #'(LAMBDA (obj)
			 (SORT-EXISTS-P obj domain))
		     simplified-form))
    (WHEN *SIMPLIFY-TYPE-P*					;;; first of all, simplify form, i.e., take
      (SETQ simplified-form					;;; the normal form, either CNF or DNF; see
	(SIMPLIFY-TYPE simplified-form			    ;;; global *NORMALFORM-OPERATOR-SYMBOL* iff
		       :use-hierarchy-p *USE-HIERARCHY-P*   ;;; *SIMPLIFY-TYPE-P is T, otherwise take
		       :domain domain))			    ;;; form as it is
      (WHEN *VERBOSE-P*
	(WARN "  >> The input form of ~S undergoes a simplification step."
	      type))))
  
  (SETQ sort-p (SORT-P simplified-form domain))
  
  (IF (SORT-EXISTS-P type domain) 
      (UNLESS sort-p
        (WARN "~S was previously a SORT type in domain ~S.  Redefining as an AVM type..." 
	      type domain)
        (DELETE-TYPE type :domain domain)
        (SETQ prev-enc-p NIL))
    (WHEN (AND (AVM-EXISTS-P type) sort-p)
      (WARN "~S was previously an AVM type in domain ~S.  Redefining as an SORT type..." 
            type domain)
      (DELETE-TYPE type :domain domain)
      (SETQ prev-enc-p NIL)))

  (LET ((*USE-HIERARCHY-P* T))
    (SETQ overwrite-values
      (SUBLIS (TDL-parse::give-translation-table)    ;;; replace corefs
              overwrite-values
              :test #'EQ))
    (MULTIPLE-VALUE-SETQ (term-node var-bindings func-constr overwrite-value)	;;; now build the skeleton, i.e., the smallest
      (BUILD-SKELETON type simplified-form			;;; structure possible which contains all the
                      :overwrite-values overwrite-values        ;;; information given in the surface string
		      :domain domain)))                         ;;; note that this structure is either a term
	                                                  	;;; node, a disjunction node or a neg-atom node

  
  (SETQ *INTERMEDIATE-BINDINGS*                     ;;; this is necessary to define the intermediates
        (NREVERSE *INTERMEDIATE-BINDINGS*))         ;;; in the right order (hope so)

  (UNLESS (AND (NOT *VERBOSE-P*) intermediate-type-p)
      (WHEN *VERBOSE-P*			; return some information for debugging
	(WHEN *INTERMEDIATE-BINDINGS* 
	  (FORMAT T "~%*INTERMEDIATE-BINDINGS*:~%  ~S~%" *INTERMEDIATE-BINDINGS*))
	(FORMAT T "The pure term node:~%  ~S~%" term-node))
      (WHEN (OR *VERBOSE-READER-P* *VERBOSE-P*)
	(FORMAT T "~%UDiNe term node:~%")
	(PRINT-FS term-node 
		  :funcs func-constr
		  :control-obj *CTRL-OBJ*)))
  
  (Define-Intermediate-Types *INTERMEDIATE-BINDINGS* *FUNCTIONAL-BINDINGS* type domain)

  (SETQ supers   (Infer-Superclasses type term-node domain)           ;; infer the supertyes and the disjunction
	elements (Infer-Elements type term-node domain))              ;; elements in case of a disjunctive type
  
  (WHEN (MEMBER (GET-BOTTOM-SYMBOL) supers :test #'EQ)		      ;;; now check whether the hierarchy might
    (RETURN-FROM DEFINE-FEATURE-TYPE		                      ;;; encode type inconsistencies
      (CERROR "If continue, reject this type definition without interrupting the load."
	      "The bottom type is infered as one of the supertypes of ~S."
	      type)))
  (WHEN (MEMBER (GET-BOTTOM-SYMBOL) elements :test #'EQ)              ;;; the same is true for the disjunction
    (RETURN-FROM DEFINE-FEATURE-TYPE                                  ;;; elements
      (CERROR "If continue, reject this type definition without interrupting the load."
	      "The bottom type is infered as one of the disjunction elements of ~S."
	      type)))

  (WHEN (AND (= (LENGTH supers) 1)                         ;;; this is necessary to avoid strange effects in definitions
             monotonic-p
	     (NOT (EQ (FIRST supers)                       ;;; x := y & z AND xx:= y & z;
		      (GET-TOP-SYMBOL domain)))            ;;; note that there are other cases  currently not checked
	     (UNIFY::TERM-NODE-P term-node)                ;;; in general feature term subsumption is needed here; other
	     (NULL (UNIFY::GET-LABEL-LIST term-node)))	   ;;; examples:  x := y & z & [a 1]  or  x := y \| z  (xx same)
    (IF (EQ (FIRST supers) type)                           ;;; make sure that if supers is of length 1, the element is
      (RETURN-FROM DEFINE-FEATURE-TYPE                     ;;; not equal to definition in question;
	(WARN "  >> ~S is already defined with the same definition."
	      type))
      (CERROR "If continue, your type definition is rewritten to \"~S :< ~S.\""  ;;; exactly the same definition as the new one
	      "The type ~S has the same definition as ~S."
	      type (FIRST supers) type (FIRST supers))))

  (WHEN *WARN-IF-TYPE-DOES-NOT-EXIST*	; this global variable must _only_ be checked at this point
    (DOLIST (not-exist-type  (Delete-Top-Symbol supers domain))
      (UNLESS (TYPE-EXISTS-P not-exist-type domain)
	(WARN "  >> The type ~S is currently NOT defined, but is used as a SUPER of ~S."
	      not-exist-type type)
	(PUSHNEW not-exist-type not-defined-types 
		 :test #'EQ)))
    (DOLIST (not-exist-type  (Delete-Top-Symbol elements domain))
      (UNLESS (TYPE-EXISTS-P not-exist-type domain)
	(WARN "  >> The type ~S is currently NOT defined, but is used as an ELEMENT of ~S."
	      not-exist-type type)
	(PUSHNEW not-exist-type not-defined-types 
		 :test #'EQ)))
    (DOLIST (not-exist-type  (Delete-Top-Symbol occur domain))
      (UNLESS (TYPE-EXISTS-P not-exist-type domain)
	(WARN "  >> The type ~S is currently NOT defined, but is used as a VALUE inside ~S."
	      not-exist-type type)
	(PUSHNEW not-exist-type not-defined-types 
		 :test #'EQ)))
    (DOLIST (not-exist-type  (Delete-Top-Symbol restriction domain))
      (UNLESS (TYPE-EXISTS-P not-exist-type domain)
	(WARN "  >> The type ~S is currently NOT defined, but is used as a RESTRICTION inside ~S."
	      not-exist-type type)
	(PUSHNEW not-exist-type not-defined-types 
		 :test #'EQ))))
  
  (WHEN supers                                                   ;; now check whether 'type' wants
    (DOLIST (partition (GET-GLOBAL :partitioned-types domain))   ;; to inherit from a partition
      (WHEN (MEMBER (FIRST partition) supers :test #'EQ)         ;; which however doesn't mention
	(UNLESS (MEMBER type (SECOND partition) :test #'EQ)      ;; 'type' as its part
	  (ERROR "  >> ~S does not belong to the partition of ~S: ~A~2%"
		  type (FIRST partition) (THIRD partition))))))

  (IF (MEMBER type (GET-GLOBAL :partitioned-types domain)    ;; is 'type' the name of a partition?
	      :key #'FIRST                                   ;; if so, it can NOT be redefined as a
	      :test #'EQ)                                    ;; disjunctive type
    (IF (NULL elements)
      (ENC::DEFINE-CLASS type                                ;; this partition can only inherit from
	                 :domain domain                      ;; its supers but cannot have disjunction
			 :superclasses supers                ;; elements
			 :elements (ENC::DISJ-SUBCLASSES type)
			 :occurclasses (Delete-Top-Symbol (UNION occur restriction) domain)   ; before: occur
			 :is-glb (Infer-Is-GLB term-node)
			 :is-lub 1
			 :nonmonotonic-link (NOT monotonic-p)
			 :is-bottom '()
			 :bottom-subclasses (Verify-Bottom-Subclasses type))
      (RETURN-FROM DEFINE-FEATURE-TYPE
	(WARN "  >> ~S is declared as a partition: ~A
         >> It cannot be redefined as a disjunctive type."
	      type  (THIRD (ASSOC type (GET-GLOBAL :partitioned-types domain))))))
    (ENC::DEFINE-CLASS type
	               :domain domain
		       :superclasses supers
		       :elements elements
		       :occurclasses (UNION elements
					    (Delete-Top-Symbol (UNION occur restriction) domain))   ; before: occur 
		       :is-glb (Infer-Is-GLB term-node)
		       :is-lub (Infer-Is-LUB term-node domain)
		       :nonmonotonic-link (NOT monotonic-p)
		       :is-bottom '()
		       :bottom-subclasses (IF prev-enc-p (Verify-Bottom-Subclasses type) '())))
  
  (WHEN (AND (NOT-P simplified-form)
             (SYMBOLP (SECOND simplified-form)))                        ;;; if type is a negative type, we have
    (LET ((pos-type (IF (FIND #\~ (SYMBOL-NAME type)                    ;;; to make sure that there's of course
			      :start 0 :end 1 :test #'CHAR=)            ;;; a positive one AND that they share
		      (INTERN (REMOVE #\~ (SYMBOL-NAME type) :count 1)) ;;; the same bottom type
		      (SECOND simplified-form))))
      (WHEN (NOT (ENCODED-P pos-type))
	(ENC::DEFINE-CLASS pos-type
	                   :domain domain
			   :superclasses (LIST (GET-TOP-SYMBOL domain))
			   :elements '()
			   :occurclasses '()
			   :is-glb -1
			   :is-lub -1
			   :nonmonotonic-link (NOT monotonic-p)
			   :is-bottom '()
			   :bottom-subclasses '())
	(WHEN *VERBOSE-P*
	  (WARN "  >> Introducing ~S, the DUAL of ~S."
		pos-type type)))
      (UNLESS (ASSOC (LIST pos-type type)
		     (GET-GLOBAL :incompatible-types domain)
		     :test #'INTERSECTION)
	(Reduce-and-Introduce-New-Bottom type pos-type domain)
	(WHEN *VERBOSE-P*
	  (WARN "  >> Introducing a common BOTTOM type for ~S and ~S."
		pos-type type)))))
  
  (Record-Atom-Type type simplified-form domain)      ;; record the type of atoms in case of a disjunction

  (LET ((not-def-types (GET-GLOBAL :not-defined-types domain))) ;; store the types occuring in the
    (DOLIST (not-def not-defined-types)	                        ;; definition of `type' which are
      (PUSHNEW not-def not-def-types :test #'EQ))               ;; currently not defined type
    (SET-GLOBAL :not-defined-types domain not-def-types))       ;; definition, delete it

  (SET-GLOBAL :not-defined-types                                ;; delete type because it is defined
	      domain
	      (DELETE type 
		      (GET-GLOBAL :not-defined-types domain) 
		      :test #'EQ))

  (LET ((detected-ids (GET-GLOBAL :detected-identifiers domain)))                 ;; store also all the
    (DOLIST (det-id (CONS type                                                    ;; identifiers which
                          (APPEND not-defined-types atomic-symbols attributes)))  ;; are recognized
      (PUSHNEW det-id  detected-ids :test #'EQ))                                  ;; during parsing
    (SET-GLOBAL :detected-identifiers domain detected-ids))

  (SETQ infon                                                      ;; now record the relevant information
        (MAKE-TYPE-INFON :name type                                ;; in a TYPE-INFON (for a description
                         :domain domain                            ;; of all slots, see the file
                         :surface surface                          ;; "control/infons.lisp")
                         :intermediate input-form
                         :comment doc
                         :author author
                         :date date
                         :value-types occur
                         :restriction-types restriction
                         :atomic-symbols atomic-symbols        ;; apply UNIFY::NSIMPLIFY-FS to term-node (see
                         :attributes attributes                ;; above); 
                         :overwrite-values overwrite-value     ;; <- SIC !!!
                         :overwrite-paths overwrite-paths      ;; Form: fs control-obj &OPTIONAL occur-check
                         :skeleton (MAKE-FEATURE-STRUCTURE :term term-node
							   :funs func-constr)
                         :monotonic monotonic-p
                         :mixed? (OR (Mixed-P simplified-form domain)
                                     (AND (OR-P simplified-form)
                                          (SOME #'(LAMBDA (x)
                                                    (Mixed-Exists-P x domain))
                                                (REST simplified-form))))
			 :creation-index (INCF *CREATION-INDEX*)
                         :class-info (ENC::Class-Infon type)))
  (SETF (Prototype infon)
    (LIST (Skeleton infon)))

  (SETF (GETHASH type                                   ;;; if type should be regarded as a _sort_, one has to declare
                 (GET-GLOBAL (IF sort-p :sorts :avms)   ;;; this fact in a `BEGIN/END :declare' statement _before_ the
                             domain))                   ;;; actual sort definition takes place
    infon)
  
  (SET-AVM-EXPAND-CONTROL type expand-control NIL domain)
  
  (WHEN (AND #+TDL-Complete-Flag (Complete (Unify::Get-Type term-node))
             #-TDL-Complete-Flag (Complete (Unify::Get-Type term-node) (Get-Top-Symbol domain))
             ;; was (Fully-Expanded-Node-P term-node)
             (Unify::Term-Node-P term-node)     ;;; <-- this should be removed if Udine
             (OR (EQ (Value-Type (Unify::Get-Type term-node))
                     (Get-Top-Symbol domain))
                 (Unify::Get-Label-List term-node))
             ) ;;; handles disjunctive type entries correctly
    (SETF (Value-Type (UNIFY::Get-Type term-node)) type))
  
  ;;;(SETQ UNIFY::*TOP-ELEMENT* (MAKE-TOP-TYPE-INFO :domain domain)) ;; refresh top element
  
  (UNLESS (Fully-Expanded-Node-P term-node)
    (WHEN (OR *EXPAND-TYPE-P* *CHECK-WELLTYPEDNESS-P*)
      (WHEN *VERBOSE-P*
        (WARN "  >> Expanding avm ~S ..."
              type))
      (EXPAND-TYPE type :domain domain))
    (WHEN (AND *VERBOSE-P* (NOT (Fully-Expanded-Node-P
                                 (FEATURE-STRUCTURE-TERM
                                  (FIRST (Prototype infon))))))
      (WARN "  >> The type ~S is not fully expanded."  
	    type)))
  
  (WHEN *CHECK-WELLTYPEDNESS-P*
    (Check-Welltypedness-Node term-node :name type :domain domain)
    (FORMAT T "~%Welltypedness check done.~%"))   

  (WHEN *CHECK-RESTRICTION-P*
    (WARN "  >> Checking the restrictions is currently not implemented.")
    "Check for every type info whether the restriction _subsumes_ the value.
     If the object is a disjunction, it might be possible to well type the disjunction")
  
  (WHEN *EXHAUSTIVE-REDEFINITION-P*   ;;; see comment at the begin of this function
    (DOLIST (infon redef-types)
      (EVAL (TDL-PARSE::Parse-TDL (Surface infon)))))
  
  (SETQ *LAST-TYPE* type)
  
  (UNLESS (EQ status :unknown)          ;;; if the status is given a value, record this in
    (Record-Status type status domain)) ;;; the global STATUS-ASSOC-LIST
  
  (IF *VERBOSE-P*   ;;; return this to inform the user
    (PROGN (IF (ATOM type)
	     (FORMAT T "~%[~S] " type)
	     (FORMAT T "~%[~A] " (STRING-LEFT-TRIM '(#\,) (FORMAT NIL "~{,~S~}" type))))
	   (FORMAT T "~A~%" date))
    (when *verbose-definition-p*
      (FORMAT T "#~A<~S> " (IF sort-p "Sort" "Avm") type)))
  
  (VALUES))

(DEFUN  DEFINE-INSTANCE  (instance-name input-form 
  &KEY surface
       occur
       restriction
       atomic-symbols
       attributes
       status
       (overwrite-values ())
       (overwrite-paths ())
       (expand-control :unknown)
       (create-lexical-types-p *CREATE-LEXICAL-TYPES-P*)
       (doc  *DEFAULT-DOCUMENTATION*)
       (author  *DEFAULT-AUTHOR*)
       (domain  (SHOW-CURRENT-DOMAIN))
       (date  (MULTIPLE-VALUE-BIND (second minute hour day month year)
		  (GET-DECODED-TIME)
		(DECLARE (TYPE FIXNUM  second minute hour day month year))
		(FORMAT NIL
			"The instance ~S has been defined on ~2,'0D/~2,'0D/~D at ~2,'0D:~2,'0D:~2,'0D"
			instance-name month day year hour minute second)))
  &AUX (form (SUBLIS (TDL-parse::give-translation-table)
		     input-form 
		     :test #'EQ))
       (simplified-form  (FIRST form))
       (*INTERMEDIATE-BINDINGS* '())
       (*FUNCTIONAL-BINDINGS* (CONS :FUNC-CONSTR 
				    (SECOND (ASSOC :func-constr 
						   (REST form)))))
       (*SPLICE-COREFS* (Collect-Values (SECOND (ASSOC :values (REST form))))) 
       (not-defined-types '())
       (overwrite-value ())  ;;; SIC !!
       (monotonic-p  (IF (NULL overwrite-paths) T NIL))
       infon
       term-node
       var-bindings
       func-constr
       supers
       elements)

  (DECLARE (IGNORE  status))     ;;; the status keyword only makes sense in type definitions
  
  (UNLESS (EQ (FIRST *BEGIN-END-WHAT*) :INSTANCE)
    (RETURN-FROM DEFINE-INSTANCE
      (ERROR "  >> Instance definitions not allowed in ~S environment. Use BEGIN :INSTANCE." 
	    (FIRST *BEGIN-END-WHAT*))))

  (UNLESS (EQ (GET-INSTANCE instance-name :domain domain :errorp NIL) 'NIL)
    (IF *ACCUMULATE-INSTANCE-DEFINITIONS*
        (WARN "There already exists an instance with name ~S in domain ~A.
Pushing the new instance onto the instance list of ~S"
              instance-name domain instance-name)
      (WARN "There already exists an instance with name ~S in domain ~A.
Replacing the old definition of ~S" instance-name domain instance-name)))
  
  (WHEN *VERBOSE-P*
    (WHEN (ATOM-EXISTS-P instance-name domain)
      (WARN "  >> An ATOM with the name ~S was already used in domain ~S." 
	    instance-name domain)))
  
  (WHEN *SIMPLIFY-TYPE-P*
    (SETQ simplified-form
          (SIMPLIFY-TYPE simplified-form
                         :use-hierarchy-p T
                         :domain domain))
    (WHEN *VERBOSE-P*
      (WARN "  >> The instance description of ~S undergoes a simplification step."
	    instance-name)))

  (SETQ overwrite-values
    (SUBLIS (TDL-parse::give-translation-table)    ;;; replace corefs
            overwrite-values
            :test #'EQ))
  
  (MULTIPLE-VALUE-SETQ (term-node var-bindings func-constr overwrite-value)
    (BUILD-SKELETON (Get-Top-Symbol domain) simplified-form
		    :multiply create-lexical-types-p
		    :top-level-p NIL
                    :overwrite-values overwrite-values
                    :domain domain))

  (SETQ *INTERMEDIATE-BINDINGS*                     ;;; this is necessary to define the intermediates
    (NREVERSE *INTERMEDIATE-BINDINGS*))         ;;; in the right order (hope so)
  
  (Define-Intermediate-Types *INTERMEDIATE-BINDINGS*
                             *FUNCTIONAL-BINDINGS*
                             instance-name domain)
  
  (SETQ supers   (Infer-Superclasses instance-name term-node domain)
	elements (Infer-Elements instance-name term-node domain))
  
  (WHEN (MEMBER (GET-BOTTOM-SYMBOL) supers :test #'EQ)
    (RETURN-FROM DEFINE-INSTANCE
      (CERROR "If continue, reject this instance definition without interrupting the load."
	      "The bottom type is infered as one of the supertypes of ~S."
	      instance-name)))
  (WHEN (MEMBER (GET-BOTTOM-SYMBOL) elements :test #'EQ)
    (RETURN-FROM DEFINE-INSTANCE
      (CERROR "If continue, reject this instance definition without interrupting the load."
	      "The bottom type is infered as one of the disjunction elements of ~S."
	      instance-name)))
  
  (WHEN *WARN-IF-TYPE-DOES-NOT-EXIST*
    (DOLIST (not-exist-type  (Delete-Top-Symbol supers domain))
      (UNLESS (TYPE-EXISTS-P not-exist-type domain)
	(WARN "  >> The type ~S is currently NOT defined, but is used as a SUPER of ~S."
	      not-exist-type instance-name)
	(PUSHNEW not-exist-type not-defined-types 
		 :test #'EQ)))
    (DOLIST (not-exist-type  (Delete-Top-Symbol elements domain))
      (UNLESS (TYPE-EXISTS-P not-exist-type domain)
	(WARN "  >> The type ~S is currently NOT defined, but is used as an ELEMENT of ~S."
	      not-exist-type instance-name)
	(PUSHNEW not-exist-type not-defined-types 
		 :test #'EQ)))
    (DOLIST (not-exist-type  (Delete-Top-Symbol occur domain))
      (UNLESS (TYPE-EXISTS-P not-exist-type domain)
	(WARN "  >> The type ~S is currently NOT defined, but is used as a VALUE inside ~S."
	      not-exist-type instance-name)
	(PUSHNEW not-exist-type not-defined-types 
		 :test #'EQ)))
    (DOLIST (not-exist-type  (Delete-Top-Symbol restriction domain))
      (UNLESS (TYPE-EXISTS-P not-exist-type domain)
	(WARN "  >> The type ~S is currently NOT defined, but is used as a RESTRICTION inside ~S."
	      not-exist-type instance-name)
	(PUSHNEW not-exist-type not-defined-types 
		 :test #'EQ))))

  (Record-Atom-Type instance-name simplified-form domain)
  
  (LET ((not-def-types (GET-GLOBAL :not-defined-types domain)))
    (DOLIST (not-def not-defined-types)
      (PUSHNEW not-def not-def-types :test #'EQ))
    (SET-GLOBAL :not-defined-types domain not-def-types))

  (LET ((detected-ids (GET-GLOBAL :detected-identifiers domain)))
    (DOLIST (det-id (CONS instance-name
                          (APPEND not-defined-types atomic-symbols attributes)))
      (PUSHNEW det-id  detected-ids :test #'EQ))
    (SET-GLOBAL :detected-identifiers domain detected-ids))

  (WHEN (AND *WARN-IF-OVERWRITE-ATTRIBUTE-VALUE*   ;;; this global variable must _only_ be checked at this point
             (NOT monotonic-p))
    (WARN "  >> SLO Overwriting --- The instance ~S is defined non-monotonically." 
	  instance-name))
  
  (SETQ infon
        (MAKE-INSTANCE-INFON :name instance-name
			     :domain domain
			     :surface surface
			     :intermediate input-form
			     :comment doc
			     :author author
			     :date date
			     :value-types occur
			     :restriction-types restriction
			     :atomic-symbols atomic-symbols
			     :attributes attributes
                             :monotonic monotonic-p
                             :overwrite-values overwrite-value ;;; SIC !!!
                             :overwrite-paths overwrite-paths
			     :skeleton (MAKE-FEATURE-STRUCTURE :term term-node
							       :funs func-constr)
			     :creation-index (INCF *CREATION-INDEX*)))

  (SETF (prototype infon)
    (skeleton infon))

  (SET-INSTANCE instance-name infon
                :domain domain
                :index (IF *ACCUMULATE-INSTANCE-DEFINITIONS* NIL 0))

  (SET-INSTANCE-EXPAND-CONTROL instance-name expand-control 0 domain)
  
  (IF (OR *EXPAND-TYPE-P* *CHECK-WELLTYPEDNESS-P*)
      (EXPAND-INSTANCE instance-name :domain domain)
    (WHEN (AND *VERBOSE-P*
               (NOT (Fully-Expanded-Node-P term-node)))
      (WARN "  >> The instance ~S is not fully expanded."  
	    instance-name)))

  (WHEN *CHECK-WELLTYPEDNESS-P*
    (Check-Welltypedness-Node term-node :name instance-name :domain domain)
    (FORMAT T "~%Welltypedness check done.~%"))    
  
  (WHEN *CHECK-RESTRICTION-P*
    (WARN "  >> Checking for the restrictions is currently not implemented.")
    "Check for every type info whether the restriction _subsumes_ the value.
     If the object is a disjunction, it might be possible to well type the disjunction")
  
  (SETQ *LAST-INSTANCE* instance-name)
  
  (IF *VERBOSE-P*
    (FORMAT T "~A~%" date)
    (when *verbose-definition-p*
      (FORMAT T "#Instance<~S> " instance-name)))

  (VALUES))

(DEFUN  DEFINE-TEMPLATE  (tname params input-form
  &KEY surface
       (doc  *DEFAULT-DOCUMENTATION*)
       (author  *DEFAULT-AUTHOR*)
       (domain  (SHOW-CURRENT-DOMAIN))
       (date  (MULTIPLE-VALUE-BIND (second minute hour day month year)
		  (GET-DECODED-TIME)
		(DECLARE (TYPE FIXNUM  second minute hour day month year))
		(FORMAT NIL
			"The template ~S has been defined on ~2,'0D/~2,'0D/~D at ~2,'0D:~2,'0D:~2,'0D"
			tname month day year hour minute second))))
  (DECLARE (TYPE SYMBOL  tname)
	   (TYPE LIST  params)
           (TYPE CONS  input-form)
           (TYPE SIMPLE-STRING  surface author doc date domain))
  (WHEN *WARN-IF-REDEFINE-TYPE*
    (Redefine-Template-Warning tname domain))
  ;; (SETQ params (Replace-all-Template-Calls params))
  ;; (SETQ input-form (Replace-all-Template-Calls input-form))
  (LET* ((form (SUBLIS (TDL-parse::give-translation-table)
		       input-form 
		       :test #'EQ))
	 (simplified-form  (FIRST form))
	 (*FUNCTIONAL-BINDINGS* (CONS :FUNC-CONSTR 
				      (SECOND (ASSOC :func-constr 
						     (REST form) 
						     :test #'EQ))))
	 (*SPLICE-COREFS* (Collect-Values (SECOND (ASSOC :values 
							 (REST form)
							 :test #'EQ)))))
    (EVAL `(DEFUN ,tname ,(Build-Lambda-List-with-&KEY params)
	     ,(Build-Template tname simplified-form)))
    (SETF (GETHASH tname
		   (GET-GLOBAL :templates domain))
      (MAKE-TEMPLATE-INFON :name tname
			   :parameters params
			   :intermediate form
			   :surface surface
			   :comment doc
			   :author author
			   :date date
			   :domain domain))
    (IF *VERBOSE-P*
	(FORMAT T "~A~%" date)
        (when *verbose-definition-p*
          (FORMAT T "#Template<~S> " tname)))
    (VALUES)))

