
(load (dir-and-name tdl::*patches-dir* "mrsglobals-eng"))

(excl:compile-file-if-needed (dir-and-name *patches-dir* "time-convert"))
(load (dir-and-name tdl::*patches-dir* "time-convert"))

(in-package "MAIN")

(defmethod call-printer :after ((cpu controller)
				(from parser)
				&rest keys-args)
  (declare (ignore keys-args))
  (when *trees-output-p*
    (trees::traverse-parsing-result (output-stream from)))
  (when (and (boundp '*mrs-output-p*) *mrs-output-p*)
    (funcall (symbol-function (read-from-string "mrs::extract-and-output"))
             (output-stream from))))

(in-package "PARSING")
; Allow lexical rules to apply to multi-word lexemes, e.g. to get
; "Can't Kim sleep?"

(defun get-rules-for-passive-item (passive-item parser)
  ;;; get all appropriate rules for this passive item and return them as a list
  ;;; this is for bottom up parsing only
  (case (combo-item-itype passive-item)
    (:MORPH
      (transform-instances
       ;; the result of the following function has to be a list of TDL
       ;; instances that have status :lex-entry or :c-lex-entry
       (funcall (combo-parser-lexicon-access-fn parser) passive-item parser)
       parser))
    ((:LEX-ENTRY :C-LEX-ENTRY :LEX-RULE)
     (append (combo-parser-lex-rules parser)
	     (combo-parser-syn-rules parser)))
    ((:C-LEX-ENTRY :RULE)
     (if (and (combo-parser-syn-rules parser) *RULES-FOR-CATEGORY-TABLE*)
	 (or (get-rules-by-category-info passive-item parser)
	     (combo-parser-syn-rules parser))
       (combo-parser-syn-rules parser)))))

(in-package "TDL")
