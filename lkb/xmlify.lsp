;;; Copyright (c) 1991--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


;;; an alternative preprocessor for the case when we want to xmlify
;;; redefines preprocess-sentence-string

(in-package :lkb)

;;; chared-word structure is defined in main/parse.lsp


(defun preprocess-sentence-string (str)
  ;; as standard fn, except with xml output as in RASP
  ;; and return list of strings
  ;; e.g., "<w s=\"19\" e=\"24\">bark</w>"
  (let ((in-word nil)
        (chars (coerce str 'list))
	(tmp-result-chars nil)
	(result-list nil)
	(current-word-record nil))
    (do* ((next-char (car chars) (car remainder))
          (remainder (cdr chars) (cdr remainder))
	  (char-count 0 (+ char-count 1)))
	((null next-char) 
	 (when (and current-word-record
		    tmp-result-chars) 
	   (push (make-end-of-word 
		  current-word-record tmp-result-chars char-count)
		 result-list)
	   nil))
      (cond ((eql next-char #\')
	     (cond 
	      ((not in-word) 
	       (if (or (null remainder) (eql (car remainder) #\space))
		   nil			; ' isolated in string - ignore
		 (progn			; ' at beginning of something
					; treat as start of word
		   (setf current-word-record
		     (make-chared-word :cfrom char-count))
		   (push next-char tmp-result-chars)
		   (setf in-word t))))
	      ((or (null remainder) (eql (car remainder) #\space))
	       ;; end of word
	       (push (make-end-of-word 
		      current-word-record tmp-result-chars char-count)
		     result-list)
	       (setf in-word nil)
	       (setf tmp-result-chars nil)
	       ;; start of word with #s
	       (setf current-word-record
		 (make-chared-word :cfrom char-count))
	       (push #\s tmp-result-chars)
	       (setf in-word t))
	      ;; leave the end of word to the normal mechanism
	      (t
	       ;; end of word
	       (push (make-end-of-word 
		      current-word-record tmp-result-chars char-count)
		     result-list)
	       (setf in-word nil)	
	       (setf tmp-result-chars nil))))
	    ((not (alphanumeric-or-extended-p next-char))
					; ignore
	     (when in-word
	       (push (make-end-of-word 
		      current-word-record tmp-result-chars char-count)
		     result-list)
	       (setf in-word nil)
	       (setf tmp-result-chars nil)))
	    ((char= next-char #\Space)
	     (when in-word
	       (push
		(make-end-of-word 
		 current-word-record tmp-result-chars char-count)
		result-list)
	       (setf in-word nil)
	       (setf tmp-result-chars nil)))
	    (t
	     (when (not in-word)
	       (setf current-word-record
		 (make-chared-word :cfrom char-count))
	       (setf in-word t))
	     (push next-char tmp-result-chars))))
  (nreverse result-list)))

(defun make-end-of-word (current-word-record tmp-result-chars char-count) 
  (setf (chared-word-cto current-word-record)
    (- char-count 1))
  (setf (chared-word-word current-word-record)
    (coerce (nreverse tmp-result-chars) 'string))
  current-word-record)

;;; note that the cfrom cto won't necessarily show up on the tree FSs

(defun set-characterization (tdfs cfrom cto)
  (when *within-unification-context-p*
    (error "Why?"))
  (when (and (tdfs-p tdfs) (integerp cfrom)
	     (> cfrom -1) (integerp cto)
	     (> cto -1)) 
    (let* ((replace-alist (list (cons 'initial-cfrom-val  
				      (format nil "~A" cfrom))
				(cons 'initial-cto-val  
				      (format nil "~A" cto))))
	   (new-dag (copy-dag-completely (tdfs-indef tdfs)))
	   (dag (existing-dag-at-end-of new-dag
					mrs::*initial-semantics-path*)))
      (destructively-replace-types dag replace-alist)
      (setf (tdfs-indef tdfs) new-dag))))



#|

;;; not working - may just be that we need a setf on the path value

(defun set-characterization (tdfs cfrom cto)
  (when *within-unification-context-p*
    (error "Why?"))
  (when (and (tdfs-p tdfs) (integerp cfrom)
	     (> cfrom -1) (integerp cto)
	     (> cto -1)) 
    (let* ((replace-alist (list (cons 'initial-cfrom-val  
				      (format nil "~A" cfrom))
				(cons 'initial-cto-val  
				      (format nil "~A" cto))))
	   (dag (existing-dag-at-end-of (tdfs-indef tdfs)
					mrs::*initial-semantics-path*)))
      (destructively-replace-types-top dag replace-alist))))

|#

(defparameter *active-parsing-p* nil)

