
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

; in kh-tree.lisp
; Fix tree drawing so it can be directed to a stream

(in-package "TREES")

(defun KH-Parse-Tree (nodelist &key (vspace 2) (hspace 2) (stream t))
  (let ((*tree-node-id* -1)
	(*tree-node-list* nil))
    (let* ((pl   (Compute-Parse-List nodelist))
	   (tree (Create-Tree pl :vspace vspace :hspace hspace)))
      (setf *tree-node-list* (reverse *tree-node-list*))
      (Tree-Draw-TTY tree *tree-node-list* stream))))

;; In parse-tree.lisp

(defun Fast-Tree-Draw (height layout stream)
  (let ((cr (princ-to-string #\Newline))
        (result ""))
    (loop 
     for i from 0 to height do
     (let ((s (write-to-string (svref layout i) :escape nil)))
       (setf result (concatenate 'string result s cr))))
    (write result :escape nil :stream stream)))

(defun Tree-Draw-Tty (tree tree-node-list &optional (stream t))
  (let* ((height (Parse-Tree-Height tree))
	 (width (Parse-Tree-Width tree))
	 (layout (make-array (1+ height))))
    (loop 
     for i from 0 to height
     do 
     (setf 
      (svref layout i)
      (make-string (1+ width) :initial-element #\Space)))
    (loop 
     for n in (Parse-Tree-Nodes tree) 
     do (Tree-Place-Node layout n tree-node-list))
    (loop
     for e in (Parse-Tree-Edges tree)
     do (Draw-Stubs layout (first e) (second e) (third e) (fourth e)))
    (loop for n in (Parse-Tree-Nodes tree) do (Connect-Stubs layout n))
    (format stream "~%")
    (Fast-Tree-Draw height layout stream)
    (format stream "~%")
    ))

(in-package "TDL")

