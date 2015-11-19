(in-package :common-lisp-user)

(pushnew :logon *features*)

;;
;; in a run-time image loading this file as its `-L' argument, we need to call
;; *restart-init-function* ourselves, as slave() below never returns.
;;
#+(and :allegro :runtime-standard)
(when (functionp excl:*restart-init-function*)
  (funcall excl:*restart-init-function*))
  
(let* ((logon (system:getenv "LOGONROOT"))
       (lingo (namestring (parse-namestring (format nil "~a/lingo" logon)))))
  ;;
  ;; load MK defsystem() and LinGO load-up library first
  ;;
  (load (format nil "~a/lingo/lkb/src/general/loadup" logon))

  ;;
  ;; for transfer, we need the full scoop
  ;;
  (unless (find-package :tsdb)
    (pushnew :lkb *features*)
    (pushnew :slave *features*)
    (excl:tenuring 
     (funcall (intern "COMPILE-SYSTEM" :make) "tsdb")))

  (funcall (symbol-function (find-symbol "INITIALIZE-TSDB" :tsdb)))

  (funcall 
   (intern "READ-SCRIPT-FILE-AUX" :lkb)
   (format nil "~a/lingo/erg/eds/script" logon))

  (funcall (symbol-function (find-symbol "SLAVE" :tsdb))))
