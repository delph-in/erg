(in-package "MAIN")


#|
;; In /usr/local/page2.3/src/nutshell/protocols/call-eng-scanner
;; Added special sentence boundary characters, to help with root types.
;;
;; 20-Sept-98 (DPF) No longer need these - now using unary rules for root types,
;; along with useless-task-filter in tuneup-patches.lisp to block root edges
;; which do not span the whole input.

(in-package "MAIN")


(defmethod call-component ((cpu controller)
			   (target eng-scanner)
			   &key direction)
  (declare (ignore direction))
  (let ((in (input-stream target)))
    (setf (direction cpu)
      (if (setf (output-stream target)
	    (strings-to-typed-items 
	     (cons "<" (nreverse 
			 (cons ">" 
			       (nreverse
				(eng-scan-input in t nil)))))))
	  :forward
	:backward)))
  nil)
|#


#|
;; In /usr/local/page2.3/src/nutshell/protocols/call-eng-scanner.lisp
;; Corrected top level scanner call to eliminate punctuation.

(in-package "MAIN")

(defmethod call-component ((cpu controller)
			   (target eng-scanner)
			   &key direction)
  (declare (ignore direction))
  (let ((in (input-stream target)))
    (if (setf (output-stream target)
	  (strings-to-typed-items 
	     (remove-if #'(lambda (x)
			    (unless (consp x)
			      (member (char x 0) %special-chars% 
				      :test #'char-equal)))
			(eng-scan-input in t nil))))
	:forward
      :backward))
  nil)
|#

; In mrs-to-vit.lisp, adapt to new VIT checker's output format

(in-package "MRS")

(defun check-vit (vit &optional (as-string nil) (stream *standard-output*))
  #+(and :allegro :clim)
  (progn
   (with-open-file (vit-out "~/tmp/vitcheck" :direction :output
	                                    :if-exists :supersede)
    (format vit-out "ensure_loaded(vitADT).~%V = ")
    (if as-string 
	(format vit-out "~A" vit)
      (write-vit vit-out vit))
    (format vit-out ",vitCheck(V).~%~%halt.~%"))
   (excl::run-shell-command "cd /eo/e1/vm2/vitADT/lib/Vit_Adt;/opt/quintus/bin3.2/sun4-5/prolog < ~/tmp/vitcheck" :output "~/tmp/vitout" :if-output-exists :supersede :error-output "~/tmp/viterror" :if-error-output-exists :supersede)
   (excl::run-shell-command "tail +58 ~/tmp/viterror | tail -r | tail +2 | tail -r" :output stream :error-output "~/tmp/realerrorout" :if-output-exists :supersede :if-error-output-exists :supersede)
   (format stream "~%"))
  #-(and :allegro :clim)
  (warn "function check-vit needs customising for this Lisp"))

(in-package "MAIN")