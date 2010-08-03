(in-package :lkb)

(defun string-split (split-string string)
  "Returns a list containing items in 'string' split from occurrences of 'split-string'."
  (loop with l = (length split-string)
        for n = 0 then (+ pos l)
        for pos = (search split-string string :start2 n)
        if pos collect (subseq string n pos)
        else collect (subseq string n)
        while pos))

(defun convert-wcn (wcn-file result-items)
  (let (result-string)
   (with-open-file
      (istream wcn-file :direction :input)
    (let ((token-num 41)
	  (item-pos -1))
     (loop
       (let ((next-char (peek-char t istream nil 'eof)))
	 (when (eql next-char 'eof) (return)))
       (let* ((items-string (read-line istream))
	      (items-list (string-split " " items-string)))
	 (when (equal (first items-list) "align")
	   (incf item-pos)
	   (setf result-items 
	     (append result-items
		     (let ((items (rest (rest items-list))))
		       (loop while items
			   append (produce-input-items-from-wcn 
				    (incf token-num) item-pos
				    (pop items) 
				    (pop items)
				    (first items)
				    (second items)))))))))))
  (setf result-string "")
  (loop for elem in result-items
		    do (setf result-string 
			 (concatenate 'string result-string (format nil "~a" elem))))
  result-string))
              
;(defparameter *wcn-prob-threshold* 0.01)
(defparameter *wcn-prob-threshold* 0.1)
(setf tsdb::*tsdb-maximal-number-of-results* 1)
(setf tsdb::*tsdb-maximal-number-of-analyses* 1)
(setf tsdb::*tsdb-trees-hook* t)
(setf tsdb::*tsdb-trees-hook* "lkb::parse-tree-structure")
(setf tsdb::*tsdb-write-mrs-p* t)
(setf tsdb::*tsdb-semantix-hook* nil)
(setf tsdb::*tsdb-maximal-number-of-edges* 500000)

(defun produce-input-items-from-wcn (token-num item-pos token prob 
				     next-token next-prob)
  (let ((tokprob (ignore-errors (read-from-string prob)))
	(nextprob (ignore-errors (read-from-string next-prob))))
    (when (and (> (or tokprob 0) *wcn-prob-threshold*)
	       (if (or (equal token "*DELETE*") (equal token "*SIL*")) 
		   (or (> tokprob 0.5)
		       (when (or (equal next-token "*DELETE*") 
				 (equal next-token "*SIL*"))
			   (> (+ tokprob nextprob) 0.5)))
		 T))
      (list (format nil 
		    "(~A, ~A, ~A, <0:0>, ~A, ~s, 0, ~s) " 
		    token-num item-pos (+ 1 item-pos) 1 token "null")))))


#|
Example:

------------------------------------------------------------------------
Input file "filename":

name M_13.slf
numaligns 8
posterior 1
align 0 *DELETE* 0.991102 of 0.00889821
align 1 okay 0.728816 ok 0.0982883 oh 0.09352 uh 0.0448835 i 0.0255941 that 0.00446913 a 0.00442908
align 2 *DELETE* 0.946564 i 0.0448191 a 0.0086172
align 3 *DELETE* 0.953943 i 0.0460573
align 4 go 0.740513 *DELETE* 0.109779 okay 0.0704799 just 0.0465463 got 0.0128683 or 0.00966086 right 0.00882356 a 0.00132935
align 5 through 1
align 6 the 1
align 7 door 1
------------------------------------------------------------------------
Output from calling 
(convert-wcn "filename" nil)

(42, 0, 1, <0:0>, 1, "*DELETE*", 0, "null") (44, 1, 2, <0:0>, 1, "okay", 0, "null") (51, 2, 3, <0:0>, 1, "*DELETE*", 0, "null") (54, 3, 4, <0:0>, 1, "*DELETE*", 0, "null") (56, 4, 5, <0:0>, 1, "go", 0, "null") (64, 5, 6, <0:0>, 1, "through", 0, "null") (65, 6, 7, <0:0>, 1, "the", 0, "null") (66, 7, 8, <0:0>, 1, "door", 0, "null")

|#
