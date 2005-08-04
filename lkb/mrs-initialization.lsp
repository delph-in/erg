#+:mrs
(load (merge-pathnames
            (make-pathname :name "mrsglobals.lsp")
            (this-directory)))

;;; (when (fboundp 'index-for-generator)
;;;   (index-for-generator))

;; lexdb provides cached generator indices
;; but this is useful only if lexical and
;; grammar rules are indexed also (08-jun-04 - bmw)
#+:psql
(when
    (and
     *lexdb*
     (fboundp 'index-lexical-rules)
     (fboundp 'index-grammar-rules))
  (index-lexical-rules)
  (index-grammar-rules))
