(in-package "MRS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Add access function used by TSDB machinery

(defun get-mrs-strings (parse-list)
  (loop for parse in parse-list
        collecting
        (let* ((fs (get-parse-fs parse))
               (sem-fs (path-value fs *initial-semantics-path*)))
          (if (is-valid-fs sem-fs)
              (let ((mrs-struct (sort-mrs-struct (construct-mrs sem-fs))))
                (with-output-to-string (stream) 
		  (format stream "~%~S" mrs-struct)
                  ;(output-mrs1 mrs-struct 'simple stream)
		  ))))))

(defun get-mrs-resolved-strings (parse-list)
  (loop for parse in parse-list
	collecting
        (let* ((fs (get-parse-fs parse))
               (sem-fs (path-value fs *initial-semantics-path*)))
          (when (is-valid-fs sem-fs)
              (let* ((mrs-struct (sort-mrs-struct (construct-mrs sem-fs)))
		     (binding-sets (make-scoped-mrs mrs-struct)))
		(when binding-sets
		  (with-output-to-string (stream) 
		    (setf *canonical-bindings* (canonical-bindings 
						(first binding-sets)))
		    (output-scoped-mrs mrs-struct :stream stream))))))))

(defun get-parse-fs (parse)
  (if (string-equal "1" (subseq user::*page-version* 0 1))
      (lexicon::cfs-fs (pg::u-item-cfs parse))
  (lexicon::cfs-fs (car (lex::typed-item-args parse)))))

(defun get-vit-strings (parse-list)
  (loop for parse in parse-list
        collecting
	(let* ((fs (get-parse-fs parse))
               (sem-fs (path-value fs *initial-semantics-path*)))
          (if (is-valid-fs sem-fs)
              (let ((mrs-struct (sort-mrs-struct (construct-mrs sem-fs))))
		 (multiple-value-bind (vit binding-sets)
		     (mrs-to-vit mrs-struct)
		   (with-output-to-string (stream) 
		     (format nil "~S" (write-vit stream vit)))))))))

(defun expand-tsdb-results (result-file dest-file &optional (vitp nil))
  (excl::run-shell-command (format nil "sort -n < ~A | sed -f ~A > ~A" 
				   result-file
				   "~/grammar/tsdb/tsnlpsed"
				   (concatenate 'string result-file ".out")))
  (let ((old-raw-mrs main::*raw-mrs-output-p*))
    (setf main::*raw-mrs-output-p* nil)
    (with-open-file 
	(istream (concatenate 'string result-file ".out") :direction :input)
     (with-open-file 
	(ostream dest-file :direction :output :if-exists :supersede)
      (do ((sent-num (read istream nil 'eof))
	   (sent (read istream nil 'eof))
	   (mrs (read istream nil 'eof))
	   (sep (read-char istream nil 'eof))
	   (tree (read istream nil 'eof)))
	  ((eql sent-num 'eof) nil)
	(format t "~%~A" sent)
	(format ostream "~%~A~%" sent)
	(trees::kh-parse-tree tree :stream ostream)
	(if vitp
	    #|
	    (progn
	      (multiple-value-bind 
		  (vit binding-sets)
		  (mrs-to-vit mrs))
	      (write-vit-pretty t (horrible-hack-2 vit))
	      (format ostream "~%")
	      (check-vit vit))
	      |#
	    (progn
	      (format ostream "~A~%~%" mrs)
	      (finish-output ostream)
	      (check-vit mrs t ostream)
	      (format ostream "~%"))
	  (format ostream "~%~A~%" mrs))
	(setf sent-num (read istream nil 'eof)
	      sent (read istream nil 'eof)
	      mrs (read istream nil 'eof)
	      sep (read-char istream nil 'eof)
	      tree (read istream nil 'eof)))))
    (setf main::*raw-mrs-output-p* old-raw-mrs)))

(defun extract-and-output (parse-list)
 (let ((*print-circle* nil))
  (loop for parse in parse-list
        do
        (let* ((fs (get-parse-fs parse))
               (sem-fs (path-value fs *initial-semantics-path*)))
          (if (is-valid-fs sem-fs)
              (let 
                  ((mrs-struct (construct-mrs sem-fs)))
		(unless *mrs-to-vit*
		  (output-mrs mrs-struct 'simple))
                (if *mrs-to-vit*
                    (mrs-to-vit-convert mrs-struct)
                  (if *mrs-scoping-p*
                      (scope-mrs-struct mrs-struct)))
                (when *mrs-results-check*
                    (let ((sorted-mrs-struct (sort-mrs-struct mrs-struct))
			  (previous-result
                           (gethash (remove-trailing-periods
                                     main::*last-sentence*)
                                    *mrs-results-table*)))
                      (if previous-result
                         (unless (mrs-equalp sorted-mrs-struct previous-result)
                                  (when 
                                   (y-or-n-p "Differs from previous result.
                                       Replace?")
                                   (setf 
                                    (gethash
                                     (remove-trailing-periods
                                     main::*last-sentence*)
                                     *mrs-results-table*)
                                    sorted-mrs-struct)))
                        (when (y-or-n-p "No previous result.
                                       Add?")
                              (setf 
                               (gethash
                                (remove-trailing-periods
                                main::*last-sentence*) *mrs-results-table*)
                               sorted-mrs-struct)))))))))))
