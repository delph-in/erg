(pushnew :lingo *features*)

(setf main::*lkb-source-dir* '(:ABSOLUTE "eo" "e1" "users" "dan" "newlkb" "src"))
(setf main::*lkb-fasl-dir* '(:ABSOLUTE "eo" "e1" "users" "dan" "newlkb" "src" "fasl"))

;;; load PAGE-specific definitions needed for further loading
(load
   (make-pathname :name "page-interface"
                  :directory (append main::*lkb-source-dir* (list "mrs"))))
(load
   (make-pathname :name "pagemrs"
                  :directory (append main::*lkb-source-dir* (list "mrs"))))

(in-package "USER")
(eval-when (load eval)
(with-compilation-unit ()
(progn 
      (dolist  (dir-and-file 
        '( 
	   (("mrs") "mrsglobals")     ; have to redefine most of these,
                                      ; but this is mostly 
	                              ; done by mrsglobals-eng 
           (("mrs") "basemrs")        ; MRS structures and printing
           (("mrs") "mrsoutput")      ; constructing MRS from parse result  
          (("mrs") "mrscorpus")         ; checking equality etc
          (("mrs") "interface")
; following two files needed for scoping - can be excluded for generation 
           (("mrs") "mrsresolve")     ; resolving scope
           (("mrs")  "mrscons")       ; constraints on scope
; following five files for mrs to vit  - can be excluded for generation 
           (("mrs") "vit")            ; VIT structures
           (("mrs") "mrs-to-vit")     ; convert MRS to VIT
           (("mrs") "time-convert")   ; temporary code for converting
                                      ; times to VIT format
                                      ; needs fixing
           (("mrs") "mrsmunge")       ; manipulate MRS via rules
                                      ; currently for vitrifying - potential
                                      ; other uses
           (("mrs") "mrsruleinput")   ; creating rules for above
                                      ; requires LKB, but outputs compiled 
                                      ; rules which can be used without LKB
;           (("mrs") "acl-mrs")        ; display etc in CLIM
                                       ; needs fixing
           (("mrs") "mrsfns")         ; from old mrsglobals
;           (("mrs") "lkbmrs")         ; LKB specific - redefines
                                      ; some functions
;           (("mrs") "lexindex")       ; LKB specific - indexing
                                      ; entries etc on semantics
;           (("mrs") "lexlookup")      ; LKB specific - retrieving
                                      ; entries etc for generation
;           (("mrs") "mrsglobals-eng") ; globals for LinGO grammar
))                 
      (let* ((dir (car dir-and-file))
             (file (cadr dir-and-file))
            (source-file (make-pathname :name file
               :directory (append main::*lkb-source-dir* dir)))
            (compiled-file (make-pathname :name file
                                          :directory (append main::*lkb-fasl-dir* dir))))
        #-mcl(compile-file source-file :output-file compiled-file)
        (load #-mcl compiled-file #+mcl source-file))))
))

(load (make-pathname :name "mrsglobals-eng.lisp"
                     :directory 
                     '(:absolute "eo" "e1" "users" "dan" "grammar")))

#|
;;; loading vitrification rules
(when (fboundp 'read-mrs-rule-file-aux)
    (read-mrs-rule-file-aux 
     (make-pathname :name "new-rules.mrs"
                    :directory (append main::*lkb-source-dir* '("mrs" "data")))))
|#

(load (make-pathname :name "pagemrspatches.lisp"
                     :directory 
                     '(:absolute "eo" "e1" "users" "dan" "grammar" "patches")))

(in-package :cl-user)

(setf mrs::*ordered-mrs-rule-list* nil) 
(with-open-file (istream "/eo/e1/users/dan/grammar/data/raw1.rules"
		 :direction :input)
  (loop (let ((rule (read istream nil nil)))
	  (unless rule (return))
	  (push rule mrs::*ordered-mrs-rule-list*))))
(setf mrs::*ordered-mrs-rule-list* (nreverse mrs::*ordered-mrs-rule-list*))
