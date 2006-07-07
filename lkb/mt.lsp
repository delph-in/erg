(in-package :mt)

;;;
;;; disable all post-transfer post-processing of MRSs; needed in Norwegian --
;;; English MT (due to mismatches in internal variable structure), but surely
;;; not when working with only one grammar.
;;;
(setf *transfer-filter-p* nil)
