#+:mrs
(load (merge-pathnames
            (make-pathname :name "mrsglobals.lsp")
            (this-directory)))

;;; (when (fboundp 'index-for-generator)
;;;   (index-for-generator))

;; lexdb provides cached generator indices
;; but this is useful only if lexical and
;; grammar rules are indexed also (08-jun-04 - bmw)
(when
    (and
     *psql-lexicon*
     (fboundp 'index-lexical-rules)
     (fboundp 'index-grammar-rules))
  (index-lexical-rules)
  (index-grammar-rules))

   

#+mrs(read-mrs-rule-file-aux 
      (merge-pathnames
       (make-pathname :directory 
                      (pathname-directory
                       (dir-append *grammar-directory*
                                   '(:relative "data"))))
      (make-pathname 
       :name "genrules.mrs"))
      t)




