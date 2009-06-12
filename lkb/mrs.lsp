(in-package :mrs)

;;;
;;; a function currently only used in the paraphraser transfer grammar, to do
;;; post-parsing normalization of predicates associated to unknown words.  see
;;; the discussion on the `developers' list in May 2009 for background.  this
;;; should in principle be incorporated into MRS read-out already, i.e. there
;;; should be a way of registering MRS post-processing hooks.   (2-jun-09; oe)
;;;
;;; normalize-mrs() is keyed off a table of <tag, rule, pattern> triples, each
;;; pairing a PTB PoS tag with an orthographemic rule of the grammar (to strip
;;; off inflectional suffixes, if any), and a format() template used to create
;;; the normalized PRED value.
;;;
(defparameter *mrs-normalization-heuristics*
  '(("JJ[RS]?" nil "_~a_a_unknown_rel")
    ("(?:FW|NN)" nil "_~a_n_unknown_rel")
    ("NNS" nil "_~a_n_unknown_rel")
    ("RB" nil "_~a_a_unknown_rel")
    ("VBP?" :third_sg_fin_verb_orule "_~a_v_unknown_rel")
    ("VBD" :past_verb_orule "_~a_v_unknown_rel")
    ("VBG" :prp_verb_orule "_~a_v_unknown_rel")
    ("VBN" :psp_verb_orule "_~a_v_unknown_rel")
    ("VBZ" :third_sg_fin_verb_orule "_~a_v_unknown_rel")))

(defun normalize-mrs (mrs)
  (loop
      for ep in (psoa-liszt mrs)
      for pred = (rel-pred ep)
      when (stringp pred) do
        (loop
            for (tag rule pattern) in *mrs-normalization-heuristics*
            for re = (format nil "^_([^_]+)/~a_u_unknown_rel$" tag)
            thereis 
              (multiple-value-bind (start end starts ends)
                  (ppcre:scan re pred)
                (when (and start end)
                  (let* ((form (subseq pred (aref starts 0) (aref ends 0)))
                         (form (string-upcase form)))
                    (cond
                     #+:lkb
                     (rule 
                      (let* ((stems (lkb::one-step-morph-analyse form))
                             (stem (first (rassoc (intern rule :lkb) stems))))
                        (when stem
                          (setf (rel-pred ep)
                            (format nil pattern (string-downcase stem))))))
                     (t
                      (setf (rel-pred ep)
                        (format nil pattern (string-downcase form))))))))))
  mrs)
