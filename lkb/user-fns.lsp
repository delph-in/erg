;;; LinGO big grammar specific functions

(in-package :cl-user)

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
    (unless (and daughter1 (not (eql daughter1 'no-way-through)))
      (cerror "Ignore it" "Rule without daughter"))
    (append (list nil '(ARGS FIRST))
            (if (and daughter2 (not (eql daughter2 'no-way-through)))
                (list '(ARGS REST FIRST)))
            (if (and daughter3 (not (eql daughter3 'no-way-through)))
                (if (and daughter2 (not (eql daughter2 'no-way-through)))
                    (list '(ARGS REST REST FIRST)))))))

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
             

;;; return true for types that shouldn't be displayed in type hierarchy
;;; window. Descendents (if any) will be displayed, i.e. non-displayed
;;; types are effectively spliced out

(defun hide-in-type-hierarchy-p (type-name)
   ;; starts with _, or ends with _[0-9][M]LE[0-9]
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
                   (and (digit-char-p (char name cur)) (= (incf cur) end))))))))

(defparameter *infl-pos-record* nil)

(defun find-infl-pos (unifs orth-string sense-id)
  (declare (ignore orth-string))
  (let ((types
         (for unif in unifs
              filter
              (if (null (path-typed-feature-list (unification-lhs unif)))
                  (car (u-value-types (unification-rhs unif)))))))
    (cond
     ((null types) 
      (format t 
              "~%Warning ~A doesn't specify any types, no affix position found"
              sense-id)
      nil)
     ((cdr types)
      (format t 
              "~%Warning ~A specifies multiple types, no affix position found"
              sense-id))
     (t
      (let* ((type (car types))
             (res (assoc type *infl-pos-record*)))
        (if res (cdr res)
          (let ((type-entry (get-type-entry type)))
            (cond (type-entry 
                   (eval-possible-leaf-type type)
                   (let ((pos
                          (extract-infl-pos-from-fs 
                           (tdfs-indef (type-tdfs type-entry)))))
                     (unless pos
                       (format t "~%No position identified for ~A" sense-id))
                     (push (cons type pos) *infl-pos-record*)
                     pos))
                  (t
                   (format t "~%Warning ~A specifies invalid type,~
 no affix position found"
                           sense-id)
                   nil)))))))))


(defun extract-infl-pos-from-fs (fs)  
  (let ((current-path '(ARGS))
         (coindexed-position 
          (existing-dag-at-end-of fs '(--FINAL-ARG)))
        (position 1))
    (if coindexed-position
        (loop (let* ((next-path 
                      (append current-path '(FIRST)))
                     (new-pos 
                      (existing-dag-at-end-of 
                       fs next-path)))
                (unless new-pos
                       (return nil))
                (when (eq new-pos coindexed-position)
                  (return position))
                (incf position)
                (setf current-path 
                  (append current-path '(REST))))))))

(defun rule-priority (rule)
  (case (rule-id rule)
    (extradj_i 100)
    (extradj_s 150)
    (extracomp 200)
    (extrasubj 100)
    (fillhead_d 150)
    (fillhead_imp 150)
    (fillhead_wh_r 150)
    (fillhead_wh_nr_f 150)
    (fillhead_wh_nr_i 150)
    (fillhead_rel 150)
    (hoptcomp 200)
    (rootgap_l 100)
    (rootgap_r 100)
    (n_n_cmpnd 200)
    (frag_nomod_i 100)
    (otherwise 500)))
