(in-package "MRS")

; Put the old definition back for PAGE

(defun determine-variable-type (fs)
  (let ((type (fs-type fs)))
    (case type
          (disco::event "e")
          (disco::event_or_index "e")
          (disco::eventtime "t")
          (disco::handle "h")
          (disco::hole "h")
          (disco::label "h")
          (disco::ref-ind "x")
          (disco::full_ref-ind "x")
          (disco::deg-ind "d")
          (disco::individual "d")
	  (tdl::*diff-list* "c")  ;; Assume coordination structure
          (t "v"))))
