;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file:
;;;      module:
;;;     version:
;;;  written by:
;;; last update:
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "TSDB")

(defconstant %tsdb-tsdb-client% 64)

(defconstant %tsdb-client-connect-ok% (character 1))

(defconstant %tsdb-client-connect-error% (character 2))

(defparameter *tsdb-server-host* nil)

(defparameter %tsdb-servers% nil)

(defun server-directory (server)
  (first server))

(defun server-stream (server)
  (second server))

(defun server-pid (server)
  (third server))

(defmacro find-tsdb-server (db)
  `(find ,db %tsdb-servers% :key #'server-directory :test #'equal))

(defun socket-write (output string)
  (format output "~a~%" string)
  (force-output output))

(defun read-from-tsdbd (input &key wait drain)
  (let* ((result (make-array 4096
                             :element-type 'character
                             :adjustable t             
                             :fill-pointer 0))
         (status 
          (do ((c (read-char-no-hang input nil :eof)
                  (read-char-no-hang input nil :eof))
               (last nil c))
              ((or (equal c :eof)
                   (and (not drain)
                        (or (equal c %tsdb-client-connect-ok%)
                            (equal c %tsdb-client-connect-error%))
                        ;;
                        ;; _fix_me_
                        ;; for some weird reason tsdbd(1) seems to send these
                        ;; control characters twice    (6-jun-97  -  oe@coli)
                        ;;
                        (or (read-char-no-hang input nil :eof) t))
                   (and (not wait) (null c))
               (or c (and drain last)))
            (when (characterp c)
              (vector-push-extend c result 1024))))))
    (values (unless (zerop (length result)) result)
            (cond
              ((equal status %tsdb-client-connect-ok%) :ok)
              ((equal status %tsdb-client-connect-error%) :error)
              (t status)))))

(defun initialize-tsdbd (&optional (language *tsdb-data*))
  (let ((data (find-tsdb-directory language))
        (host (or *tsdb-server-host* "localhost"))
        (port 4711))
    (if (or (equal host "localhost") (equal host (current-host)))
      (let* ((command (format
                       nil "~a -server -home=~a -pager=null -max-results=0"
                       *tsdb-application* data))
             (command (concatenate 'string command " -uniquely-project=off"))
             (command (concatenate 'string command " -implicit-commit=off")))
        (multiple-value-bind (foo bar pid)
            (run-process
             command :wait nil
             :output nil :input nil :error-output nil)
          (declare (ignore foo bar))
          (sleep 10)
          ;;
          ;; add further sanity checks here: assert tsdbd(1) is up and running
          ;;
          (let* ((stream (ipc:open-network-stream :host host :port port)))
            (sleep 2)
            (read-from-tsdbd stream :drain t)
            (socket-write
             stream
             (format nil "set status ~d.~%" %tsdb-tsdb-client%))
            (multiple-value-bind (foo status)
                (read-from-tsdbd stream :wait t)
              (declare (ignore foo))
              (if (equal status :ok)
                (push (list data stream pid) %tsdb-servers%)
                (format
                 *tsdb-io*
                 "initialize-tsdbd(): ~
                  unable to communicate with tsdbd(1) (`~a').~%" host))))))
      (format
       *tsdb-io*
       "initialize-tsdbd(): not on tsdb(1) server host (`~a').~%" host))))

(defun shutdown-tsdbd (stream)
  (socket-write stream "shutdown.")
  (close stream))

(defun call-tsdbd (query &optional (language *tsdb-data*))
  (let* ((data (find-tsdb-directory language))
         (server (find-tsdb-server data)))
    (if server
      (let* ((stream (server-stream server))
             (query (string-trim '(#\Space #\Tab #\Newline) query))
             (query (if (equal (elt query (- (length query) 1)) #\.)
                    query
                    (concatenate 'string query "."))))
        (read-from-tsdbd stream :drain t)
        (socket-write stream query)
        (read-from-tsdbd stream :wait t))
      (if (initialize-tsdbd language)
        (call-tsdbd query language)
        (format
         *tsdb-io*
         "call-tsdbd(): no tsdb(1) server for database `~a'.~%" language)))))



