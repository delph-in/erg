(in-package "MRS")

;;; Time conversion
;;; takes an MRS representing a clock time and converts it into
;;; a VIT-compatible ctime format
;;;
;;; Optional (and limited) cleverness in doing am/pm on the basis of
;;; morning/afternoon expressions

;;;
;;; numbered_hour_rel takes four (non-handel) arguments
;;; INST (an index)
;;; AM-PM (coindexed with am / pm rel if present)
;;; HOUR (oblig - integer - assumed to be using a 12 hour clock)
;;; MIN (coindexed with a minute_rel in expressions like two forty five)
;;;
;;; ctime_rel takes three (non-handel) arguments
;;; INST (an index)
;;; HOUR (oblig - integer or integer/integer when am/pm is ambiguous)
;;; MIN  (oblig - integer)
;;;
;;; HANDEL and INST values for ctime are always identical to
;;; those for nh_rel
;;;
;;; nh-rel, minute_rel, after_hour_rel, before_hour_rel, am_rel, pm_rel
;;; should all be removed and replaced by ctime_rel
;;;
;;; Issues
;;; a) determining the value for MIN in various circumstances
;;;    (should always be found - default is 0)
;;; b) determining the am/pm value (may not be possible)
;;;
;;; Case 1 - e.g. "at three"
;;;
;;; numbered_hour_rel alone present
;;; ctime HOUR = nh_rel HOUR / +12
;;; ctime MIN = 0
;;;
;;; Case 1a - e.g. "at three pm"
;;;
;;; as above, but with am/pm as well - am/pm rel
;;; coindexed to the AM/PM slot in numbered_hour_rel
;;; AM => ctime HOUR = nh_rel HOUR
;;; PM => ctime HOUR = nh_rel HOUR+12
;;;
;;; Case 3 - e.g. "at three forty five"
;;;
;;; minute_rel coindexed with MIN slot of numbered_hour_rel
;;; ctime MIN = minute_rel MINUTE
;;;
;;; Case 3a - e.g. "at three oclock"
;;;
;;; same as above
;;;
;;; Case 4 - e.g. "half past three"
;;;
;;; nhrel (h1 x y-pm number z-min)
;;; after_hour_rel (h2 arg1 x w)
;;; minute_rel (h3 w 30)
;;;
;;; ctime(h1 x number/+12 30)
;;;
;;; Case 4a - e.g. a quarter to three
;;;
;;; nhrel (h1 x y-pm number z-min)
;;; before_hour_rel (h2 arg1 x w)
;;; minute_rel (h3 w 15)
;;;
;;; ctime(h1 x number/+12 45)
;;;
;;; case 6 - e.g. a quarter past
;;; ??? Speculation
;;;
;;; after_hour_rel (h2 x v w)
;;; minute_rel (h3 w 15)
;;; 
;;; here there is no numbered_hour-rel so the
;;; ctime is based on the after/before_hour rel
;;; 
;;; ctime(h2 x UNKNOWN 15)



;;; AM/PM heuristics -
;;; preposition(x y) and am/pmdeterminer(x) and nhrel(y)
;;; or
;;; preposition(x y) and am/pmdeterminer(y) and nhrel(x)
;;; where am/pmdeterminer is morning_rel, etc

;;; UTILITY FN
   
(defun get-rel-feature-value (rel feature)
  ;;; assumes only occurs once
  (dolist (fvpair (rel-flist  rel))
    (when (eql (fvpair-feature fvpair) feature)
      (return (fvpair-value fvpair)))))


;;; GRAMMAR SPECIFIC GLOBALS AND UTILITIES

(defparameter *nhr-inst-feature* 'DISCO::INST)
(defparameter *nhr-ampm-feature* 'DISCO::AM-PM)
(defparameter *nhr-hour-feature* 'DISCO::HOUR)
(defparameter *nhr-min-feature* 'DISCO::MIN)
(defparameter *min-min-feature* 'DISCO::MINUTE)
(defparameter *min-index-feature* 'DISCO::INST)
(defparameter *ampm-index-feature* 'DISCO::INST) ; needs to be checked
(defparameter *inst-feature* 'DISCO::INST)
(defparameter *rel-hour-feature* 'DISCO::HOUR-IND)
(defparameter *rel-minute-feature* 'DISCO::MINUTE-IND)
(defparameter *prep-prep-feature* 'DISCO::PREP)
(defparameter *prep-arg-feature* 'DISCO::ARG)



(defun create-ctime-rel (handel inst hour minutes)
  (make-rel
   :sort 'DISCO::CTIME_REL
   :handel handel
   :flist
   (list (make-fvpair :feature 'DISCO::INST :value inst)
         (make-fvpair :feature 'DISCO::HOUR :value hour)
         (make-fvpair :feature 'DISCO::MIN :value minutes))))


(defun nhrel-p (sort)
  (eql sort 'DISCO::NUMBERED_HOUR_REL))

(defun relrel-p (sort)
    (member sort '(DISCO::_AFTER_HOUR_REL DISCO::_BEFORE_HOUR_REL)))

(defun minrel-p (sort)
  (eql sort 'DISCO::MINUTE_REL))

(defun ampmrel-p (sort)
  (member sort '(DISCO::AM_REL DISCO::PM_REL)))

(defun pm-rel-p (sort)
  (eql sort 'DISCO::PM_REL))

(defun am-rel-p (sort)
  (eql sort 'DISCO::AM_REL))

(defun past-rel-p (sort)
  (eql sort 'DISCO::_AFTER_HOUR_REL))

(defun to-rel-p (sort)
  (eql sort 'DISCO::_BEFORE_HOUR_REL))

(defun daytime-p (sort)
  (member sort '(DISCO::_MORNING_REL DISCO::_AFTERNOON_REL DISCO::_EVENING_REL)))

(defun derive-am-pm-spec (sort)
  (ecase sort
    (DISCO::_MORNING_REL 'am)
    (DISCO::_AFTERNOON_REL 'pm)
    (DISCO::_EVENING_REL 'pm)))



;;; Main function

(defun time-convert-mrs-struct (mrsstruct)
  (let* ((rels (psoa-liszt mrsstruct))
         (nhrels nil)
         (minrels nil)
         (ampmrels nil)
         (relrels nil)
         (others nil))
    (dolist (rel rels)
            (let ((sort (rel-sort rel)))
              (cond ((nhrel-p sort) (push rel nhrels))
                    ((relrel-p sort) (push rel relrels))
                    ((minrel-p sort) (push rel minrels))
                    ((ampmrel-p sort) (push rel ampmrels))
                    (t (push rel others)))))
    (if (or nhrels relrels) 
        (let ((new-liszt (construct-time-expressions 
                          nhrels minrels ampmrels relrels others)))
          (if new-liszt
                (setf (psoa-liszt mrsstruct) new-liszt)
            (format t 
              "~%Warning: error in time conversion, structure unchanged")))
      (if (or minrels ampmrels)
            (cerror "~%Leave structure unchanged"
"~%Error in time conversion: no numbered_hour or after/before_hour relations")))
    mrsstruct))


(defun construct-time-expressions (nhrels minrels ampmrels relrels others)
  (if nhrels
      (append
       (for nhrel in nhrels
            collect
            (construct-ctime nhrel minrels ampmrels relrels others))
       (reverse others))
    (if ampmrels
        (cerror "Ignore the time expression" 
                "~%am/pm specified but no hour")
      (append 
              (for relrel in relrels
                   collect
                   (ctime-case6 relrel minrels))
              (reverse others)))))

(defun construct-ctime (nhrel minrels ampmrels relrels others)
  (let* ((handel (rel-handel nhrel))
         (inst (get-rel-feature-value nhrel *nhr-inst-feature*))
         (am-pm (get-rel-feature-value nhrel *nhr-ampm-feature*))
         (base-hour (get-rel-feature-value nhrel *nhr-hour-feature*))
         (min (get-rel-feature-value nhrel *nhr-min-feature*))
         (direct-mins (get-coindexed-mins min minrels))
         (am-pm-value (or (get-coindexed-ampm-rels am-pm ampmrels)
                         (am-pm-heuristics inst others base-hour)))
                ; returns 'am 'pm nil 
        (direct-rel-rels (get-coindexed-rel-rels inst relrels)))            
    (cond ((and (null direct-mins) (null direct-rel-rels)) 
           (ctime-case1 handel inst 
                        (create-ctime-hour base-hour am-pm-value)))
          (direct-mins
           (when direct-rel-rels
               (error "~%Minutes specified directly and indirectly: ~A"
                      nhrel))
           (when (cdr direct-mins)
                 (error "~%Minutes specified more than one: ~A" nhrel))
           (ctime-case3 handel inst 
                        (create-ctime-hour base-hour am-pm-value)
                        (car direct-mins)))
          ((cdr direct-rel-rels)
                 (error "~%Past/to specified more than once: ~A"
                      nhrel))
          (t
           (let* ((past-or-to-rel (car direct-rel-rels))
                  (rel-minute-index 
                   (get-rel-feature-value past-or-to-rel 
                                          *rel-minute-feature*))
                  (indirect-minutes
                   (if rel-minute-index
                       (get-coindexed-mins rel-minute-index minrels))))
               (unless indirect-minutes
                       (error "~%No minute specification: ~A" nhrel))
               (when (cdr indirect-minutes)
                 (error "~%Minutes specified more than once: ~A" nhrel))
               (ctime-case4 handel inst base-hour am-pm-value
                            (rel-sort past-or-to-rel)
                            (car indirect-minutes)))))))

;;;


(defun get-coindexed-mins (index minrels)
  (for minrel in minrels
       filter
       (if (eql (get-var-num (get-rel-feature-value minrel *min-index-feature*))
               (get-var-num index))
           (get-rel-feature-value minrel *min-min-feature*))))

(defun get-coindexed-ampm-rels (index ampmrels)
  (let ((ampmspecs
         (for ampmrel in ampmrels
              filter
              (if (eql (get-var-num (get-rel-feature-value ampmrel *ampm-index-feature*))
                         (get-var-num index))
                  (rel-sort ampmrel)))))         
    (cond ((null ampmspecs) nil)
          ((cdr ampmspecs) (error "~%Dual specification of AM and PM"))
          ((am-rel-p (car ampmspecs))
           'am)
          ((pm-rel-p (car ampmspecs))
           'pm)
          (t (error "~%Incorrect AM/PM rel sort: ~A" (car ampmspecs))))))

(defun get-coindexed-rel-rels (index relrels)
  (for relrel in relrels
       filter
       (if (eql (get-var-num (get-rel-feature-value relrel *rel-hour-feature*))
               (get-var-num index))
           relrel)))


(defun create-ctime-hour (base-hour am-pm)
  (cond ((eql am-pm 'am) base-hour)
        ((eql am-pm 'pm) (+ 12 base-hour))
        ((null am-pm) (format nil "~A/~A" base-hour (+ 12 base-hour)))
        (t (error "~%Unexpected value for am/pm: ~A" am-pm)))) 



;;; cases as in comments at beginning of file

(defun ctime-case1 (handel inst hour)
  (create-ctime-rel handel inst hour 0))

(defun ctime-case3 (handel inst hour minutes)
  (create-ctime-rel handel inst hour minutes))

(defun ctime-case4 (handel inst base-hour am-pm-value
                           past-or-to-rel minutes)
    (cond 
     ((past-rel-p past-or-to-rel)
        (create-ctime-rel handel inst 
                          (create-ctime-hour base-hour am-pm-value) 
                          minutes))
     ((to-rel-p past-or-to-rel)
        (create-ctime-rel handel inst
                          (create-ctime-hour (- base-hour 1) 
                                             am-pm-value)
                          (- 60 minutes)))
     (t (error "~%Problem with after/to specification"))))
 
                           
(defun ctime-case6 (past-or-to-rel minrels)
  (let* ((handel (rel-handel past-or-to-rel))
         (inst (get-rel-feature-value past-or-to-rel *nhr-inst-feature*))
         (past-or-to (rel-sort past-or-to-rel))
         (rel-minute-index 
                   (get-rel-feature-value past-or-to-rel 
                                          *rel-minute-feature*))
         (direct-mins (get-coindexed-mins rel-minute-index minrels)))           
    (unless direct-mins
                       (error "~%No minute specification: ~A" past-or-to-rel))
    (when (cdr direct-mins)
                 (error "~%Minutes specified more than once: ~A" past-or-to-rel))
    (cond 
     ((past-rel-p past-or-to)
        (create-ctime-rel handel inst 
                          nil
                          (car direct-mins)))
     ((to-rel-p past-or-to)
        (create-ctime-rel handel inst 
                          nil
                          (- 60 (car direct-mins))))
     (t (error "~%Problem with after/to specification")))))
                
;;;
;;; Heuristics for morning / afternoon etc
;;; only called if no explicit am/pm specification
            
;;; AM/PM heuristics -
;;; index = y and
;;; preposition(x y) and am/pmtime(x) 
;;; or
;;; index = x
;;; preposition(x y) and am/pmtime(y) and nhrel(x)
;;; where am/pmtime is morning_rel, etc
     
           
(defun am-pm-heuristics (index others base-hour)
  ;;; prepositions are distinguished by their features
  ;;; specified as globals
  (let ((prep-arg-vals nil)
        (prep-prep-vals nil)
        (daytimes nil))
    (dolist (rel others)
      (cond ((daytime-p (rel-sort rel))
             (push rel daytimes))
            ((eql (get-var-num (get-rel-feature-value rel *prep-arg-feature*))
                  (get-var-num index))
             (push (get-var-num (get-rel-feature-value rel *prep-prep-feature*))
                   prep-prep-vals))
            ((eql (get-var-num (get-rel-feature-value rel *prep-prep-feature*))
                  (get-var-num index))
             (push (get-var-num (get-rel-feature-value rel *prep-arg-feature*))
                   prep-arg-vals))
            (t nil)))
      (let ((ampmspecs nil))
        (dolist (daytime daytimes)
          (let ((daytime-index 
                 (get-var-num (get-rel-feature-value daytime *inst-feature*))))
            (if (or (member daytime-index prep-prep-vals)
                    (member daytime-index prep-arg-vals))
              (push (derive-am-pm-spec (rel-sort daytime))
                    ampmspecs))))
        (cond ((null ampmspecs) (horrible-unmotivated-hack base-hour))
              ((every #'(lambda (spec) (eql spec 'am)) ampmspecs)
               'am)
              ((every #'(lambda (spec) (eql spec 'pm)) ampmspecs)
               'pm)
              (t (horrible-unmotivated-hack base-hour))))))

#|
(defun horrible-unmotivated-hack (base-hour)
        (if (integerp base-hour)
          (if (or (< base-hour 8)
                  (> base-hour 12))
              'pm 
              'am)))
|#
(defun horrible-unmotivated-hack (base-hour)
        (if (and (integerp base-hour)
		 (< base-hour 13))
              'am))
  
            
         

          
          
          
                

    
    