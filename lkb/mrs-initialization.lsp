#+:mrs
(load (merge-pathnames
            (make-pathname :name "mrsglobals-eng.lisp")
            (parent-directory)))

;;; (when (fboundp 'index-for-generator)
;;;   (index-for-generator))

#+mrs(read-mrs-rule-file-aux 
      (merge-pathnames
       (make-pathname :directory 
                      (pathname-directory
                       (dir-append *grammar-directory*
                                   '(:relative "data"))))
      (make-pathname 
       :name "genrules.mrs"))
      t)

; Use the version below for VM rule generation for PAGE ONLY
#|
#+:mrs
(in-package "MRS")
#+:mrs
(defun create-index-property-list (fs)
  #-pagelite
  (when (is-valid-fs fs)
;    (if (is-disjunctive-fs fs)
;        (setf fs (get-first-real-alter fs))
    (setf fs (deref fs)))
;    )
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs))
            (feat-list nil))
        (if (and label-list (consp label-list))
            (loop for feat-val in label-list
                do
                  (cond ((member (car feat-val) *complex-extra-feats*)
                         (setf feat-list 
                           (append feat-list
                                   (create-index-property-list 
                                    (cdr feat-val)))))
                        ((eq (car feat-val) *list-feature*)
                         (push (make-fvpair :feature (car feat-val)
                                            :value (create-coord-list 
                                                    (cdr feat-val)))
                               feat-list))
                        (t (push (make-fvpair :feature (car feat-val)
                                              :value (create-type 
                                                      (fs-type (cdr feat-val))))
                                 feat-list)))))
        feat-list)))
|#


#+:mrs
(in-package "CL-USER")




