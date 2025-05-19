(in-package :tsdb)

(defparameter *www-title* :erg)

(defparameter *www-custom-js* (merge-pathnames "erg.js" *load-truename*))

(defparameter *www-introduction* (merge-pathnames "erg.html" *load-truename*))

(defparameter *www-comparisons* nil)

(defparameter *www-sample* "Try pressing return in this window!")

(defparameter *www-roots*
  '(("sentences" lkb::root_informal "$root_standard" t)
    ("fragments" lkb::root_inffrag "$root_inffrag" nil)
    ("less ambiguity" lkb::root_strict "$root_strict" nil)
    ("minor errors" lkb::root_robust "$root_robust" nil)))

(defparameter *www-generics* t)

(defparameter *www-urls*
  '((:parse "http://www.delph-in.net/erg")
    (:generate "http://www.delph-in.net/erg")))
