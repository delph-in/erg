;;; No patches!


(load (dir-and-name tdl::*patches-dir* "mrsglobals-eng"))
(load (dir-and-name tdl::*patches-dir* "time-convert"))

(load-system "trees")
(load-system "mrs2vit")

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

(in-package "TDL")
