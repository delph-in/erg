(defparameter *lkb-source-dir* 
               '(:absolute "eon" "e2" "users" "aac" "lkb99-expt"))

(defparameter *grammar-source-dir* 
               '(:absolute "eo" "e1" "users" "dan" "grammar"))

(load
   (make-pathname :name "lkb-mrs-package"
                  :directory (append *lkb-source-dir* (list "mrs"))))

;;; define MRS package, and also DISCO, TREES, UNIFY, TDL and MAIN, to allow
;;; files to be read in without errors
;;; and define a few symbols etc here

(in-package "UNIFY")

(export '(*fail*))

(in-package "TREES")

(defun kh-parse-tree (tree &key stream)
  (declare (ignore tree stream))
  nil)

(in-package "MAIN")

(defparameter *raw-mrs-output-p* nil)

(defparameter *VM-arg-roles-only-p* nil)

(defparameter *suppressed-VM-arg-roles* nil)

(defparameter *VM-arg-roles* nil)

(in-package "TDL")

(defun show-current-domain nil
  nil)


(in-package "MRS")

(export '(psoa-handel psoa-top-h psoa-index psoa-liszt psoa-h-cons
          psoa-message psoa-wgliszt
          rel-extra rel-type rel-sort rel-handel rel-label rel-flist
          fvpair-feature fvpair-value
          var-name var-extra var-id
          handle-var-name handle-var-extra handel-var-id
          group-var-name group-var-extra group-var-id
          hcons-scarg hcons-cands hcons-outscpd
          leq-sc-scarg leq-sc-cands leq-sc-outscpd leq-sc-relation
          whg-id-id whg-id-word whg-id-handel))


(defun vsym (str) 
  ;;; allow mrsglobals-eng file to be system independent
  (intern (string-upcase str) "USER"))

(in-package "USER")

(load
   (make-pathname :name "mrsfns"
                  :directory (append *grammar-source-dir* '("patches"))))

(eval-when (load eval)

(progn 
      (dolist  (dir-and-file 
        '( (("mrs") "mrsglobals")        ; have to redefine most of these,
                                       ; but this is mostly 
                                       ; done by mrsglobals-eng 
                                       ; loaded from the script
           (("mrs" "local-copies") "basemrs")
           (("mrs") "mrsoutput")         
           (("mrs") "mrsresolve")
           (("mrs" "local-copies")  "mrscons")
           (("mrs" "local-copies") "vit")
           (("mrs") "mrs-to-vit")
           (("mrs") "lkbmrs.lsp")))           ; redefines fns as needed
                                       ; adds fns from old mrsglobals-eng
; acl-mrs 
      (let* ((dir (car dir-and-file))
             (file (cadr dir-and-file))
            (real-file (make-pathname :name file
               :directory (append *lkb-source-dir* dir))))
          (load real-file)))))



(in-package :cl-user)
