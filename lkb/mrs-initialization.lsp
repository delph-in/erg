#+:mrs
(load (merge-pathnames
            (make-pathname :name "mrsglobals.lsp")
            (this-directory)))

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




