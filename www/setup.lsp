(in-package :tsdb)

(setf *www-title* :erg)

(setf *www-custom-js* (merge-pathnames "erg.js" *load-truename*))

(setf *www-introduction* (merge-pathnames "erg.html" *load-truename*))

(setf *www-comparisons* nil)

(setf *www-sample* "Try pressing return in this window!")

(setf *www-roots*
  '(("sentences" lkb::root_informal "$root_standard" t)
    ("fragments" lkb::root_inffrag "$root_inffrag" nil)
    ("less ambiguity" lkb::root_strict "$root_strict" nil)
    ("minor errors" lkb::root_robust "$root_robust" nil)))

(setf *www-generics* t)

(setf *www-urls*
  '((:parse "http://www.delph-in.net/erg")
    (:generate "http://www.delph-in.net/erg")))
