(in-package :tsdb)

(setf *www-title* :erg)

(setf *www-custom-js* (merge-pathnames *load-truename* "erg.js"))

(setf *www-introduction* (merge-pathnames *load-truename* "erg.html"))

(setf *www-comparisons* nil)

(setf *www-sample* "Try pressing return in this window!")

(setf *www-roots*
  '(("sentences" lkb::root_strict "$root_strict" t)
    ("fragments" lkb::root_frag "$root_frag" nil)
    ("infinitives" lkb::root_inffrag "$root_inffrag" nil)
    ("slang" lkb::root_informal "$root_informal" nil)
    ("errors" lkb::root_robust "$root_robust" nil)))

(setf *www-generics* t)

(setf *www-urls*
  '((:parse "http://www.delph-in.net/erg")
    (:generate "http://www.delph-in.net/erg")))
