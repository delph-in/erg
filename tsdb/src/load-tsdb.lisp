(in-package "USER")

(dolist (file '("tsdb-package"
		;;"tdl-patches"
		"utilities"
		"tsql"
		"page-interface"
		"tsdb"
		"server"
		"statistics"
		"page-commands"))
  (load (concatenate 'string 
	  (concatenate 'string tdl::*source-grammar* "tsdb/src/") file)))

(setf tsdb::*tsdb-home* 
  (concatenate 'string tdl::*source-grammar* "tsdb/"))

; Don't gc for every item
(setf tsdb::*tsdb-gc-p* nil)
; Don't repeat parse if includes gc
(setf tsdb::*tsdb-minimize-gcs-p* t)
; Do all parses
(setf tsdb::*tsdb-exhaustive-p* nil)
; Print machine-readable MRS output, not pretty-printed
(setf main::*raw-mrs-output-p* t)
; Set Lisp print level nil to get all parse tree nodes
(setf *print-level* nil)
; Not used anymore, but still expected in tsdb code
(setf csli::*verbose-expansion* nil)
(setf tdl::*verbose-definition-p* nil)
; Set maximum number of edges to be built by parser - block runaways
(setf pg::*maximal-number-of-items* 1000)
