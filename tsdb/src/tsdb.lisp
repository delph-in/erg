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

(defparameter
  *tsdb-application*
  (format
    nil "exec ~a"
    (namestring (make-pathname :directory (pathname-directory make::bin-dir)
                               :name "tsdb"))))

(defparameter 
  *tsdb-home* 
  (namestring (dir-append (get-sources-dir "tsdb") '(:relative "tsdb"))))

(defparameter *tsdb-data* "english")

(defparameter *tsdb-server-mode-p* nil)

(defparameter *tsdb-io* t)

(defparameter *tsdb-write-csli-p* t)

(defparameter *tsdb-write-run-p* t)

(defparameter *tsdb-write-parse-p* t)

(defparameter *tsdb-write-result-p* t)

(defparameter *tsdb-write-output-p* nil)

(defparameter *tsdb-cache-database-writes-p* t)

(defparameter *tsdb-trees-hook* 
  (and (find-package "TREES") "trees::get-labeled-bracketings"))

(defparameter *tsdb-semantix-hook* nil)

(defparameter *tsdb-gc-p* nil)

(defparameter *tsdb-minimize-gcs-p* nil)

(defparameter *tsdb-gc-message-p* nil)

(defparameter *tsdb-item-factor* 2.0)

(defparameter *tsdb-exhaustive-p* t)

(defparameter *tsdb-ignore-output-p* nil)

(defparameter *tsdb-maximal-number-of-tasks* 0)

(defparameter *tsdb-lexical-oracle-p* nil)

(defparameter *tsdb-phrasal-oracle-p* nil)

(defparameter *tsdb-global-gcs* 0)

(defparameter *tsdb-debug-mode-p* nil)

(defmacro tsdb-ignore-p ()
  t)

(defmacro find-tsdb-directory (language)
  `(let* ((data (dir-append *tsdb-home* (list :relative ,language))))
     (namestring data)))

(defun reset-tsdb-paths ()
  (setf
    *tsdb-application*
    (format
      nil "exec ~a"
      (namestring (make-pathname :directory (pathname-directory make::bin-dir)
                                 :name "tsdb"))))
  (setf
    *tsdb-home* 
    (namestring (dir-append (get-sources-dir "tsdb") '(:relative "tsdb")))))

(defun call-tsdb (query language
                  &key redirection cache)
  (if *tsdb-server-mode-p*
    (call-tsdbd query language)
    (if cache
      (cache-query query language cache)
      (let* ((user (current-user))
             (file (format
                    nil "/tmp/.tsdb.io.~a.~a"
                    user (string-downcase (string (gensym "")))))
             (data (find-tsdb-directory language))
             (command (format
                       nil 
                       "~a -home=~a ~
                        -string-escape=lisp -pager=null -max-results=0"
                       *tsdb-application* data))
             (command (format
                       nil "~a -query='do \"~a\"'"
                       command file))
             (query (string-trim '(#\Space #\Tab #\Newline) query))
             (query (if (equal (elt query (- (length query) 1)) #\.)
                      (subseq query 0 (- (length query) 1))
                      query))
             (output (when (eq redirection :output)
                       (format
                        nil "/tmp/.tsdb.data.~a.~a"
                        user (string-downcase (string (gensym ""))))))
             (redirection 
              (if output (concatenate 'string " > \"" output "\"") ""))
             (query (concatenate 'string query redirection ".")))
        (with-open-file (stream file :direction :output
                         :if-exists :overwrite
                         :if-does-not-exist :create)
          (format stream "~a~%" query))
        (multiple-value-bind (stream foo pid)
          (run-process
            command :wait nil
            :output :stream
            :input "/dev/null" :error-output nil)
          (declare (ignore foo))
          (let ((result (make-array
                         4096
                         :element-type 'character
                         :adjustable t             
                         :fill-pointer 0)))
            (do ((c (read-char stream nil :eof) (read-char stream nil :eof)))
                ((equal c :eof))
              (vector-push-extend c result 1024))
            (close stream)
            #+:allegro (sys:os-wait nil pid)
            (when output
              (let ((stream 
                     (open output :direction :input :if-does-not-exist nil)))
                (do ((c (read-char stream nil :eof) 
                        (read-char stream nil :eof)))
                    ((equal c :eof))
                  (vector-push-extend c result 1024))
                (close stream)))
            (unless *tsdb-debug-mode-p*
              (delete-file file)
              (when output
                (delete-file output)))
            result))))))

(defun create-cache (language &key (verbose t))
  (let* ((user (current-user))
         (file (format
                nil "/tmp/.tsdb.cache.~a.~a"
                user (string-downcase (string (gensym "")))))
         (stream (open file 
                       :direction :output 
                       :if-exists :supersede :if-does-not-exist :create)))
    (when stream
      (format stream "set implicit-commit off.~%")
      (when verbose
        (format 
         *tsdb-io*
         "~&create-cache(): tsdb(1) write cache in `~a'.~%"
         file)
        (force-output *tsdb-io*)))
    (pairlis (list :database :file :stream)
             (list language file stream))))

(defun cache-query (query language cache)
  (let* ((database (get-field :database cache))
         (stream (get-field :stream cache))
         (query (string-trim '(#\Space #\Tab #\Newline) query))
         (query (if (equal (elt query (- (length query) 1)) #\.)
                  query
                  (concatenate 'string query "."))))
    (if (string-equal language database)
      (when stream 
        (format stream "~a~%" query)
        (force-output stream))
      (format
       *tsdb-io*
       "~&cache-query() ignoring query to `~a' (on `~a' cache).~%"
       language database))))

(defun flush-cache (cache
                    &key (verbose t))
  (let ((database (get-field :database cache))
        (file (get-field :file cache))
        (stream (get-field :stream cache)))
    (format stream "~&commit.~%")
    (force-output stream)
    (close stream)
    (let* ((query (format nil "do \"~a\"" file)))
      (call-tsdb query database))
    (when verbose
      (format 
       *tsdb-io*
       "~&flush-cache(): tsdb(1) cache for `~a' flushed.~%"
       database file)
      (force-output *tsdb-io*))
    (unless *tsdb-debug-mode-p*
      (delete-file file))))

(defun largest-result-key (&optional (language *tsdb-data*)
                           &key (verbose t))
  (let* ((query "select c-id from csli")
         (result (call-tsdb query language)))
    (with-input-from-string (stream result)
      (do ((c-ids nil c-ids)
           (c-id (read stream nil :eof) (read stream nil :eof)))
        ((equal c-id :eof)
         (let ((c-id (if c-ids (apply #'max c-ids) 0)))
           (when verbose
             (format
              *tsdb-io* 
              "~&largest-result-key(): largest `c-id' is ~a.~%"
              c-id)
             (force-output *tsdb-io*))
           c-id))
        (when (integerp c-id) (push c-id c-ids))))))

(defun largest-run-id (&optional (language *tsdb-data*)
                       &key (verbose t))     
  (let* ((query "select run-id from run")
         (result (call-tsdb query language)))
    (with-input-from-string (stream result)
      (do ((run-ids nil run-ids)
           (run-id (read stream nil :eof) (read stream nil :eof)))
        ((equal run-id :eof)
         (let ((run-id (if run-ids (apply #'max run-ids) 0)))
           (when verbose
             (format
              *tsdb-io* 
              "~&largest-run-id(): largest `run-id' is ~a.~%"
              run-id))
           run-id))
        (when (integerp run-id) (push run-id run-ids))))))

(defun largest-parse-id (run-id &optional (language *tsdb-data*)
                         &key (verbose t))
  (let* ((data (select "parse-id" :integer "parse"
                       (format nil "run-id = ~d" run-id) language))
         (parse-ids (map 'list #'(lambda (foo) (get-field :parse-id foo)) data))
         (parse-id (if parse-ids (apply #'max parse-ids) 0)))
    (when verbose
      (format
       *tsdb-io* 
       "~&largest-parse-id(): largest `parse-id' (for `run' ~d) is ~a.~%"
       run-id parse-id))
    parse-id))

(defun tsdb (query &optional (language *tsdb-data*))
  (let* ((result (call-tsdb query language)))
    (when (and result (not (zerop (length result))))
      (format *tsdb-io* "~a~%" result))))

(defun retrieve (&optional query (language *tsdb-data*)
                 &key (verbose t))
  (let* ((query 
          (format
           nil
           "select i-id i-wf i-input ~@[where ~a~]"
           query))
         (query 
          (format
           nil
           "~a report \"(%s %s \\\"%s\\\")\""
           query))
         (result (call-tsdb query language)))
    (with-input-from-string (stream result)
      (do ((line (read stream nil) (read stream nil))
           (data nil data))
          ((null line)
           (when verbose
             (format
              *tsdb-io* "~&retrieve(): found ~a item~:p.~%" (length data)))
           (merge-with-output-specifications
            (sort data #'< :key #'first) 
            language :verbose verbose))
        (let* ((i-id (first line))
               (i-wf (second line))
               (i-input (third line)))
          (if (integerp i-id)
            (if (and (integerp i-wf)
                              (>= i-wf 0)
                              (<= i-wf 3))
              (if (stringp i-input)
                (push (list i-id i-wf i-input) data)
                (format 
                  *tsdb-io*
                  "~&retrieve() invalid input `~a' in item `~a'.~%"
                  i-input i-id))
              (format 
                *tsdb-io*
                "~&retrieve() invalid wellformedness code `~a' in item `~a'.~%"
                i-wf i-id))
            (format 
              *tsdb-io*
              "~&retrieve() invalid identifier `~a' in item.~%"
              i-id)))))))

(defun merge-with-output-specifications (items language
                                         &key (verbose t))
  (if *tsdb-ignore-output-p*
    (map 'list
      #'(lambda (foo)
          (append foo (list -1 -1 *tsdb-maximal-number-of-tasks*)))
      items)
    (let (outputs)
      (do* ((length (length items) (length items))
            (current (butlast items (- length 100))
                     (butlast items (- length 100)))
            (items (nthcdr 100 items) (nthcdr 100 items)))
          ((null current))
        (let (query)
          (dolist (item current)
            (push (format nil "i-id = ~d" (first item)) query))
          (let* ((query (reduce #'(lambda (foo bar)
                                    (concatenate 'string foo " | " bar))
                                query))
                 (query (concatenate 'string
                          "select i-id o-ignore o-wf o-gc o-tasks "
                          "from output where "
                          query
                          " report \"(%d \\\"%s\\\" %d %d %d)\""))
                 (result (call-tsdb query language)))
            (with-input-from-string (stream result)
              (do ((line (read stream nil) (read stream nil)))
                  ((null line))
                (push line outputs))))))
      (when verbose
        (format
         *tsdb-io*
         "~&merge-with-output-specifications(): ~
          found ~a output specification~:p.~%" (length outputs)))
      (dolist (item items items)
        (let ((output (find (item-i-id item) outputs :key #'first)))
          (if output
            (setf (rest (last item)) (rest output))
            (setf (rest (last item))
              (list "" -1 -1 *tsdb-maximal-number-of-tasks*))))))))

(defun select-derivations (i-id &optional (language *tsdb-data*))
  (let* ((condition (format nil "i-id = ~d" i-id))
         (derivations 
          (select "o-derivation" :string "output" condition language))
         (derivations
          (and derivations
               (map 'list #'(lambda (tuple) (get-field :o-derivation tuple))
                    derivations))))
    (and derivations
         (map 'list (lambda (string) (read-from-string string nil))
              derivations))))

(defun remove-and-insert-punctuation (string)
  (let* ((string (remove #\, string))
         (string (remove #\; string))
         (string (remove #\: string))
         (string (remove #\. string))
         (string (remove #\? string))
         (string (remove #\! string)))
    (concatenate 'string string " .")))

(defun normalize-string (string)
  (let* ((string (nsubstitute #\Space #\Newline string))
         (string (nsubstitute #\# #\@ string))
         (string 
          (if (> (length string) 2)
            (reduce #'(lambda (x y)
                        (if (and (eq x #\Space)
                                 (eq (char (string y) 0) #\Space))
                            (string y)
                          (concatenate 'string (string x) (string y))))
                    string :from-end t)
            string)))
    (string-trim '(#\Space #\Tab) string)))

(defun shell-escape-quotes (string)
  (if (and (stringp string) (>= (length string) 1))
    (let ((prefix (elt string 0)))
      (if (equal prefix #\')
        (concatenate 'string "'\\''" (shell-escape-quotes (subseq string 1)))
        (concatenate 
            'string (string prefix) (shell-escape-quotes (subseq string 1)))))
    string))

(defun tsdb-escape-quotes (string)
  (if (and (stringp string) (>= (length string) 1))
    (let ((prefix (elt string 0)))
      (if (equal prefix #\")
        (concatenate 'string "\\\"" (tsdb-escape-quotes (subseq string 1)))
        (concatenate 
            'string (string prefix) (tsdb-escape-quotes (subseq string 1)))))
    string))

(eval-when (:load-toplevel :execute)
  (let ((default-global-gc-handler excl:*gc-after-hook*))
    (setf
      excl:*gc-after-hook*
      #'(lambda (global foo bar baz damn)
          (when global
            (setf *tsdb-global-gcs* (1+ *tsdb-global-gcs*))
            (when *tsdb-gc-message-p*
              (format
               *tsdb-io*
               "~&damn it; we found ourselves in global gc # ~d.~%"
               *tsdb-global-gcs*)))
          (when default-global-gc-handler
            (funcall default-global-gc-handler global foo bar baz damn))))))

(defun parse-item (string &key (tasks 0) (derivations nil)
                               (trace nil) (gc *tsdb-gc-p*))
  (let* ((foo (open "/dev/null" :direction :output :if-exists :overwrite))
         (string (remove-and-insert-punctuation string))
         (exhaustive main::*exhaustive*)
         (lexicon-task-priority-fn 
          (pg::combo-parser-task-priority-fn (pg::get-parser :lexicon)))
         (syntax-task-priority-fn 
          (pg::combo-parser-task-priority-fn (pg::get-parser :syntax)))
         tcpu tgc treal conses symbols others)
    #+:allegro (when gc (excl:gc t))
    (setf main::*exhaustive* *tsdb-exhaustive-p*)
    (setf pg::*maximal-number-of-tasks*
      (if (and (integerp tasks) (> tasks 0)) tasks nil))
    (setf pg::*current-number-of-tasks* 0)
    (setf pg::*maximal-number-of-tasks-exceeded-p* nil)
    (setf (pg::ebl-parser-external-signal-fn (current-parser))
      (if (> tasks 0)
        #'pg::maximal-number-of-tasks-exceeded-p
        #'(lambda (parser) (declare (ignore parser)))))
    (when (and derivations *tsdb-lexical-oracle-p*)
      (install-lexical-oracle derivations))
    (when (and derivations *tsdb-phrasal-oracle-p*)
      (install-phrasal-oracle derivations))
    (udine::reset-costs)
    (setf (main::input-stream main::*parser*) nil)
    (setf (main::output-stream main::*parser*) nil)
    (multiple-value-bind (result condition)
        (excl::time-a-funcall #'(lambda ()
                                  (run-protocol *tsdb-parser-protocol* 
                                                string :trace trace))
                              #'(lambda (tgcu tgcs tu ts tr
                                         scons ssym sother)
                                  (setf tgc (+ tgcu tgcs)
                                        tcpu (+ tu ts)
                                        treal tr
                                        conses scons
                                        symbols ssym
                                        others sother)))
      (declare (ignore result))
      (close foo)
      (setf main::*exhaustive* exhaustive)
      (setf (pg::combo-parser-task-priority-fn (pg::get-parser :lexicon))
        lexicon-task-priority-fn)
      (setf (pg::combo-parser-task-priority-fn (pg::get-parser :syntax))
        syntax-task-priority-fn)
      (append 
       (pairlis (list :tgc :tcpu :treal :conses :symbols :others)
                (list tgc tcpu treal conses symbols others))
       (cond
        (condition
         (pairlis
          (list :readings :condition :error)
          (list -1 condition (format nil "~a" condition))))
        ((null (main::output-stream main::*lexicon*))
         (pairlis
          (list :readings :error)
          (list -1 "null parser input")))
        (t
         (let* ((items (main::output-stream main::*parser*))
                (readings (length items))
                (words (length (main::output-stream main::*lexicon*)))
                (statistics (pg::parser-stats-readings (current-parser)))
                (first (when statistics (first (last statistics))))
                (first (when first (pg::stats-time first)))
                (first (when first (/ first internal-time-units-per-second)))
                (global (pg::parser-global-stats (current-parser)))
                (total (when global (pg::stats-time global)))
                (total (when total (/ total internal-time-units-per-second)))
                (etasks (when global (pg::stats-executed global)))
                (stasks (when global (pg::stats-successful global)))
                (ftasks (when global (pg::stats-filtered global)))
                (edges (pg::total-number-of-items (current-parser)))
                results)
           (when (= readings (length statistics))
             (do* ((i 0 (+ i 1))
                   (statistics statistics (rest statistics))
                   (statistic (first statistics) (first statistics))
                   (items items (rest items))
                   (item (first items) (first items)))
                 ((null statistics))
               (let* ((item (if (consp item) (seventh item) item))
                      (time (pg::stats-time statistic))
                      (time (when time 
                              (/ time internal-time-units-per-second)))
                      (etasks (pg::stats-executed statistic))
                      (stasks (pg::stats-successful statistic))
                      (ftasks (pg::stats-filtered statistic))
                      (derivation 
                       (format nil "~s"
                               (pg::item-to-node item (current-parser)))))
                 (push (pairlis
                        (list :result-id :time
                              :r-etasks :r-stasks :r-ftasks
                              :derivation)
                        (list i time
                              etasks stasks ftasks
                              derivation))
                       results))))
           (pairlis
            (list :readings :first :total
                  :p-etasks :p-stasks :p-ftasks
                  :words :edges :results)
            (list readings first total
                  etasks stasks ftasks
                  words edges results)))))))))

(defun write-run (result language
                  &key cache)
  (when *tsdb-write-run-p*
    (let* ((*print-circle* nil)
           (run-id (get-field :run-id result))
           (comment (normalize-string (get-field :comment result)))
           (application (normalize-string (get-field :application result)))
           (grammar (normalize-string (get-field :grammar result)))
           (avms (or (get-field :avms result) -1))
           (sorts (or (get-field :sorts result) -1))
           (templates (or (get-field :templates result) -1))
           (user (normalize-string (get-field :user result)))
           (host (normalize-string (get-field :host result)))
           (start (get-field :start result))
           (query
            (format
             nil
             "insert into run values ~d ~s ~s ~s ~d ~d ~d ~s ~s ~a"
             run-id
             comment application grammar
             avms sorts templates
             user host start)))
      (call-tsdb query language :cache cache))))

(defun write-parse (result language
                    &key cache)
  (when *tsdb-write-parse-p*
    (let* ((*print-circle* nil)
           (parse-id (get-field :parse-id result))
           (run-id (get-field :run-id result))
           (i-id (get-field :i-id result))
           (readings (or (get-field :readings result) -1))
           (first (get-field :first result))
           (first (if first (round (* 10 first)) -1))
           (total (get-field :total result))
           (total (if total (round (* 10 total)) -1))
           (tcpu (get-field :tcpu result))
           (tcpu (if tcpu (round (/ tcpu 100)) -1))
           (tgc (get-field :tgc result))
           (tgc (if tgc (round (/ tgc 100)) -1))
           (treal (get-field :treal result))
           (treal (if treal (round (/ treal 100)) -1))
           (p-etasks (or (get-field :p-etasks result) -1))
           (p-stasks (or (get-field :p-stasks result) -1))
           (p-ftasks (or (get-field :p-ftasks result) -1))
           (words (or (get-field :words result) -1))
           (edges (or (get-field :edges result) -1))
           (unifications (or (get-field :unifications result) -1))
           (copies (or (get-field :copies result) -1))
           (conses (or (get-field :conses result) -1))
           (symbols (or (get-field :symbols result) -1))
           (others (or (get-field :others result) -1))
           (gcs (or (get-field :gcs result) -1))
           (initial-load (get-field :initial-load result))
           (initial-load (if initial-load (round (* 100 initial-load)) -1))
           (average-load (get-field :average-load result))
           (average-load (if average-load (round (* 100 average-load)) -1))
           (date (current-time :long t))
           (error (normalize-string (or (get-field :error result) "")))
           (query "insert into parse values")
           (query
            (format
             nil
             "~a ~d ~d ~d ~
              ~d ~d ~d ~d ~d ~d ~
              ~d ~d ~d ~
              ~d ~d ~d ~d ~
              ~d ~d ~d ~
              ~d ~d ~d ~
              ~a ~s"
             query
             parse-id run-id i-id
             readings first total tcpu tgc treal
             p-etasks p-stasks p-ftasks
             words edges unifications copies
             conses symbols others
             gcs initial-load average-load
             date error)))
      (call-tsdb query language :cache cache))))

(defun write-results (parse-id results 
                      &optional (language *tsdb-data*)
                      &key cache)
  (let* ((items (main::output-stream main::*parser*))
         (mrss 
          (when (and *tsdb-semantix-hook* (stringp *tsdb-semantix-hook*))
            (when (find-package "MRS")
              (set (intern "*RAW-MRS-OUTPUT-P*" "MRS") t)
              (set (intern "*RAW-MRS-OUTPUT-P*" "MAIN") t))
            (ignore-errors
             (funcall (symbol-function (read-from-string *tsdb-semantix-hook*))
                      items))))
         (mrss (remove nil mrss))
         (trees 
          (when (and *tsdb-trees-hook* (stringp *tsdb-trees-hook*))
            (ignore-errors
             (funcall (symbol-function (read-from-string  *tsdb-trees-hook*))
                      items)))))
    (if (or (= (length results) (length items) (length mrss) (length trees))
            (and (not *tsdb-semantix-hook*) *tsdb-trees-hook*
                 (= (length results) (length items) (length trees)))
            (and *tsdb-semantix-hook* (not *tsdb-semantix-hook*)
                 (= (length results) (length items) (length mrss)))
            (and (not *tsdb-semantix-hook*) (not *tsdb-semantix-hook*)
                 (= (length results) (length items))))
      (do* ((results results (rest results))
            (result (first results) (first results))
            (trees (reverse trees) (rest trees))
            (tree (first trees) (first trees))
            (mrss (reverse mrss) (rest mrss))
            (mrs (first mrss) (first mrss)))
          ((null results))
        (write-result parse-id result tree mrs language :cache cache))
      (format 
       *tsdb-io* 
       "~&write-results(): mysterious mismatch [~d : ~d : ~d : ~d].~%"
       (length results) (length items) (length trees) (length mrss)))))

(defun write-result (parse-id result tree mrs language
                     &key cache)
  (when *tsdb-write-result-p*
    (let* ((*print-circle* nil)
           (result-id (get-field :result-id result))
           (time (round (* 10 (or (get-field :time result) -1))))
           (r-etasks (or (get-field :r-etasks result) -1))
           (r-stasks (or (get-field :r-stasks result) -1))
           (r-ftasks (or (get-field :r-ftasks result) -1))
           (size (or (get-field :size result) -1))
           (r-edges (or (get-field :r-edges result) -1))
           (derivation
            (or (normalize-string (get-field :derivation result)) ""))
           (query "insert into result values")
           (query
            (format
             nil
             "~a ~d ~d ~d ~d ~d ~d ~d ~d ~s ~s ~s"
             query
             parse-id result-id
             time
             r-etasks r-stasks r-ftasks
             size r-edges
             (normalize-string derivation)
             (normalize-string (or tree "")) 
             (normalize-string (or mrs "")))))
      (call-tsdb query language :cache cache))))

(defun write-output (i-id application 
                     tree mrs tasks 
                     user date
                     language
                     &key cache)
  (when *tsdb-write-output-p*
    (let* ((*print-circle* nil)
           (tree (shell-escape-quotes (remove #\@ (normalize-string tree))))
           (mrs (shell-escape-quotes (remove #\@ (normalize-string mrs))))
           (query
            (format
             nil
             "insert into output values ~a ~d ~s ~s ~s ~d ~s ~a"
             i-id application
             tree mrs tasks
             user date)))
      (call-tsdb query language :cache cache)))) 

(defun current-user ()
  (or #+:allegro (system:getenv "USER")
      #+:lucid (lcl:environment-variable "USER")
      "nobody"))

(defun current-time (&key (long nil))
  (multiple-value-bind (second minute hour day month year foo bar baz)
      (get-decoded-time)
    (declare (ignore foo bar baz))
    (if long
      (format
        nil "~a-~a-~a (~2,'0d:~2,'0d:~2,'0d)"
        day month year hour minute second)
      (format nil "~a-~a-~a" day month year))))

(defun load-average ()
  (multiple-value-bind (output foo pid)
    (run-process "/bin/uptime" :wait nil
                 :output :stream :input "/dev/null" :error-output nil)
    (declare (ignore foo))
    #+:allegro (sys:os-wait nil pid)
    (let* ((line (read-line output nil :eof))
           (colon (position #\: line :from-end t))
           (line (subseq line (+ colon 1)))
           (current (read-from-string line nil 0))
           (comma (position #\, line))
           (line (subseq line (+ comma 1)))
           (recent (read-from-string line nil 0))
           (comma (position #\, line))
           (line (subseq line (+ comma 1)))
           (past (read-from-string line nil 0)))
      (close output)
      (list current recent past))))

(defun current-host ()
  (short-site-name))

(defun pprint-memory-usage (result &optional (separator #\Space))
  (let* ((conses (* (or (get-field :conses result) 0) 8))
         (symbols (* (or (get-field :symbols result) 0) 24))
         (others (get-field :others result)))
    (concatenate 'string
      (pprint-potentially-large-integer conses)
      (string separator)
      (pprint-potentially-large-integer symbols)
      (string separator)
      (pprint-potentially-large-integer others)
      " = "
      (pprint-potentially-large-integer (+ conses symbols others)))))


(defun pprint-potentially-large-integer (n)
  (cond ((zerop n) "")
        ((>= n (expt 2 30)) (format nil "~,1fG" (/ n (expt 2 30))))
        ((>= n (expt 2 20)) (format nil "~,1fM" (/ n (expt 2 20))))
        ((>= n (expt 2 10)) (format nil "~,1fK" (/ n (expt 2 10))))
        (t (format nil "~d" n))))

(defun retrieve-and-process (&optional condition run-id
                                       (language *tsdb-data*) 
                                       comment
                             &key (verbose t)
                                  (cache *tsdb-cache-database-writes-p*)
                                  (gc *tsdb-gc-p*))
  (let ((run-id (or run-id (+ (largest-run-id language :verbose verbose) 1)))
        (data (retrieve condition language :verbose verbose)))
    (when data
      (let ((user (current-user))
            (chart main::*draw-chart-p*)
            (name-rule-fn (pg::ebl-parser-name-rule-fn (current-parser)))
            (external-signal-fn 
             (pg::ebl-parser-external-signal-fn (current-parser)))
            (tasks pg::*maximal-number-of-tasks*)
            (preserve-daughters-p
             (and (find-symbol "PG::*PRESERVE-DAUGHTERS-P*")
                  pg::*preserve-daughters-p*))
            (message (and (find-symbol "CSLI::*VERBOSE-EXPANSION*")
                          csli::*verbose-expansion*))
            (reader tdl::*verbose-reader-p*)
            (definition tdl::*verbose-definition-p*)
            #+:allegro (gc-behavior excl:*global-gc-behavior*)
            (cache (when cache (create-cache language :verbose verbose)))
            gcs result)
        (setf main::*draw-chart-p* nil)
        (setf (pg::ebl-parser-name-rule-fn (current-parser))
          #'get-informative-item-label)
        (setf pg::*preserve-daughters-p* t)
        (setf csli::*verbose-expansion* nil)
        (setf tdl::*verbose-reader-p* nil)
        (setf tdl::*verbose-definition-p* nil)
        ;(scanning::init-scanner)
        #+:allegro
        (unless *tsdb-gc-message-p*
          (setf excl:*global-gc-behavior* :auto))
        (unwind-protect
          (ignore-errors
           (catch :break
             (let* ((comment (or comment "null"))
                    (application
                     (if (boundp 'make::*page-version*)
                       (format nil "PAGE (~a)" make::*page-version*)
                       "PAGE"))
                    (grammar
                     (if (boundp 'disco::*grammar-version*)
                       disco::*grammar-version*
                       "anonymous grammar"))
                    (host (current-host))
                    (start (current-time :long t))
                    (run (pairlis (list :run-id :comment :application :grammar
                                        :user :host :start)
                                  (list run-id comment application grammar
                                        user host start)))
                    (soth (size-of-type-hierarchy)))
               (write-run (append run soth) language :cache cache))
             (do* ((data data (rest data))
                   (item (first data) (first data))
                   (parse-id
                    (+ (largest-parse-id run-id language :verbose verbose) 1)
                    (+ parse-id 1)))
                 ((null data))
               (let* ((i-id (item-i-id item)) 
                      (i-input (item-i-input item))
                      (i-wf (item-i-wf item))
                      (o-ignore (item-o-ignore item))
                      (o-ignore (when (and o-ignore (not (equal o-ignore "")))
                                  o-ignore)))
                 (if (and o-ignore (tsdb-ignore-p))
                   (format
                    *tsdb-io*
                    "~&parse-item(~a): ignoring this item ~
                     (`o-ignore' is `~a').~%"
                    i-id o-ignore)
                   (let* ((o-wf (item-o-wf item))
                          (o-wf (when (and o-wf (not (= o-wf -1))) o-wf))
                          (o-gc (item-o-gc item))
                          (o-gc 
                           (and o-gc (not (= o-gc -1)) (not (zerop o-gc))))
                          (gc (or o-gc gc))
                          (o-tasks (item-o-tasks item))
                          (o-tasks (if (and o-tasks (not (= o-tasks -1)))
                                     (floor (* *tsdb-item-factor* o-tasks))
                                     0))
                          (o-derivations (when (or *tsdb-lexical-oracle-p*
                                                   *tsdb-phrasal-oracle-p*)
                                           (select-derivations i-id language)))
                          (initial-load (first (load-average))))
                     (format
                      *tsdb-io* 
                      "~&(~a) `~:[*~;~]~a' ~:[~;=~]~:[~*~;[~d]~][~a]"
                      i-id (= i-wf 1) i-input 
                      gc o-derivations (length o-derivations) o-tasks)
                     (force-output *tsdb-io*)
                     #+:allegro (excl:gc)
                     (when gc
                       #+:allegro (excl:gc t))
                     (setf *tsdb-global-gcs* 0)
                     (setf result (parse-item i-input :tasks o-tasks :gc nil
                                              :derivations o-derivations))
                     (when (and (not *tsdb-minimize-gcs-p*) 
                                (not gc)
                                (>= *tsdb-global-gcs* 1)
                                (<= *tsdb-global-gcs* 3))
                       (format 
                        *tsdb-io*
                        " (~d gc~:p);~%" *tsdb-global-gcs*)
                       (force-output *tsdb-io*)
                       #+:allegro (excl:gc t)
                       (setf *tsdb-global-gcs* 0)
                       (setf gc t)
                       (format 
                        *tsdb-io* 
                        "(~a) `~:[*~;~]~a' ~:[~;=~]~:[~*~;[~d]~][~a]"
                        i-id (= i-wf 1) i-input 
                        gc o-derivations (length o-derivations) o-tasks)
                       (force-output *tsdb-io*)
                       (setf initial-load (first (load-average)))
                       (setf result (parse-item i-input :tasks o-tasks :gc t
                                                :derivations o-derivations)))
                     (setf gcs *tsdb-global-gcs*)
                     (setf *tsdb-global-gcs* 0)
                     #+:allegro
                     (when (and (= (get-field :readings result) -1)
                                (equal (class-of (get-field :condition result))
                                       (find-class 'excl:interrupt-signal)))
                       (throw :break nil))
                     (let ((readings (get-field :readings result))
                           (words (get-field :words result))
                           (tcpu (/ (or (get-field :tcpu result) 0) 1000))
                           (tgc (/ (or (get-field :tgc result) 0) 1000))
                           (first (get-field :first result))
                           (total (get-field :total result))
                           (edges (get-field :edges result))
                           (timeup pg::*maximal-number-of-tasks-exceeded-p*)
                           (unifications (udine::unify-costs-unify
                                          udine::*unification-costs*))
                           (copies (udine::unify-costs-copy
                                    udine::*unification-costs*)))
                       (if readings
                         (case readings
                           (0 (format 
                               *tsdb-io* 
                               " ---~:[~; time up:~] ~
                                (~,1f~:[~*~;:~,1f~]|~,1f s) ~
                                <~d:~d>~
                                ~:[ {~d:~d}~;~2*~] ~
                                (~a) [~:[~;=~]~d].~%" 
                               timeup 
                               tcpu (>= tgc 0.1) tgc total
                               words edges 
                               (= unifications copies 0) unifications copies 
                               (pprint-memory-usage result) gc gcs))
                           (-1 (format 
                                *tsdb-io* 
                                " --- error: ~a.~%" 
                                (get-field :error result)))
                           (t (format 
                               *tsdb-io* 
                               " ---~:[~; time up:~] ~a ~
                                (~,1f~:[~*~;:~,1f~]|~,1f:~,1f s) ~
                                <~d:~d>~
                                ~:[ {~d:~d}~;~2*~] ~
                                (~a) [~:[~;=~]~d].~%" 
                               timeup readings 
                               tcpu (>= tgc 0.1) tgc first total 
                               words edges 
                               (= unifications copies 0) unifications copies 
                               (pprint-memory-usage result) gc gcs)))
                         (format *tsdb-io* ".~%"))
                       (force-output *tsdb-io*)
                       (push (cons :parse-id parse-id) result)
                       (push (cons :run-id run-id) result)
                       (push (cons :i-id i-id) result)
                       (unless (= readings -1)
                         (push (cons :unifications unifications) result)
                         (push (cons :copies copies) result)
                         (push (cons :gcs gcs) result))
                       (push (cons :initial-load initial-load) result)
                       (let* ((loads (load-average))
                              (average
                               (cond ((null total) (first loads))
                                     ((<= total 60) (first loads))
                                     ((<= total (* 5 60)) (second loads))
                                     ((<= total (* 15 60)) (third loads)))))
                         (push (cons :average-load average) result))
                       (when (and timeup (not (= readings -1)))
                         (push (cons :error "timeup") result))
                       (write-parse result language :cache cache)
                       (unless (= (get-field :readings result) -1)
                         (write-results
                          parse-id 
                          (get-field :results result) language
                          :cache cache)))))))))
          (when cache (flush-cache cache :verbose verbose))
          (setf pg::*maximal-number-of-tasks-exceeded-p* nil)
          (setf pg::*maximal-number-of-tasks* tasks)
          (setf (pg::ebl-parser-external-signal-fn (current-parser))
            external-signal-fn)
          (setf pg::*preserve-daughters-p* preserve-daughters-p)
          (setf main::*draw-chart-p* chart)
          (setf (pg::ebl-parser-name-rule-fn (current-parser)) name-rule-fn)
          (setf csli::*verbose-expansion* message)
          (setf tdl::*verbose-reader-p* reader)
          (setf tdl::*verbose-definition-p* definition)
          #+:allegro (setf excl:*global-gc-behavior* gc-behavior))))))

(defun vocabulary (&optional query (language *tsdb-data*)
                             (load nil) (verbose :quiet))
  (let* ((whitespace '(#\Space #\Newline #\Tab))
         (items (retrieve query language))
         (strings (map 'list #'(lambda (foo) (item-i-input foo)) items))
         (strings (map 'list #'remove-and-insert-punctuation strings))
         (words nil)
         (frequencies (make-hash-table :test #'equal))
         (maximal-frequency 0))
    (when strings
      (dolist (string strings words)
        (do* ((i (position-if #'(lambda (c) (member c whitespace)) string)
                 (position-if #'(lambda (c) (member c whitespace)) string))
              (word (when i (subseq string 0 i))
                    (when i (subseq string 0 i)))
              (word (string-downcase (string-trim whitespace word))
                    (string-downcase (string-trim whitespace word)))
              (n (gethash word frequencies)
                 (gethash word frequencies))
              (string (if i (subseq string i) string)
                      (if i (subseq string i) string))
              (string (string-left-trim whitespace string)
                      (string-left-trim whitespace string)))
            ((not i)
             (let* ((word (string-downcase (string-trim whitespace string)))
                    (n (gethash word frequencies)))
               (when (and word (> (length word) 0))
                 (setf (gethash word frequencies) (+ (or n 0) 1))
                 (setf maximal-frequency 
                   (max maximal-frequency (+ (or n 0) 1)))
                (pushnew word words :test #'equal))))
          (when (and word (> (length word) 0))
            (setf (gethash word frequencies) (+ (or n 0) 1))
            (setf maximal-frequency (max maximal-frequency (+ (or n 0) 1)))
            (pushnew word words :test #'equal))))
      (let* ((width (apply #'max (map 'list #'length words)))
             (tabulation (format 
                          nil
                          "~~~d,0t| ~~~dd"
                          (+ width 2 1) 
                          (length (format nil "~d" maximal-frequency)))))
        (do* ((words (sort (copy-seq words) #'string-lessp) (rest words))
              (word (first words) (first words)))
            ((null words))
          (format 
           *tsdb-io* 
           "~&  ~a ~@?~%" 
           word tabulation (gethash word frequencies)))
        (format *tsdb-io* "~%")
        (when load
          (let ((reader tdl::*verbose-reader-p*)
                (definition tdl::*verbose-definition-p*)
                (expansion (and (find-symbol "CSLI::*VERBOSE-EXPANSION*")
                                csli::*verbose-expansion*))
                unknown-words)
            (case verbose
              ((:none :quiet :warn nil)
               (setf tdl::*verbose-reader-p* nil)
               (setf tdl::*verbose-definition-p* nil)
               (setf csli::*verbose-expansion* nil))
              (:fair
               (setf tdl::*verbose-reader-p* nil)
               (setf tdl::*verbose-definition-p* t)
               (setf csli::*verbose-expansion* nil))
              ((:full :all :verbose t)
               (setf tdl::*verbose-reader-p* t)
               (setf tdl::*verbose-definition-p* t)
               (setf csli::*verbose-expansion* t)))
            (do* ((words (sort (copy-seq words) #'string-lessp) (rest words))
                  (word (first words) (first words)))
                ((null words))
              (unless (morphologically-analyze-word word)
                (push word unknown-words)
                (when (member verbose (list :warn :fair :full :all :verbose t))
                  (format
                   *tsdb-io*
                   "vocabulary(): there seems to be ~
                    no lexicon entry for word `~a'.~%"
                   word))))
            (setf tdl::*verbose-reader-p* reader)
            (setf tdl::*verbose-definition-p* definition)
            (setf csli::*verbose-expansion* expansion)
            unknown-words))))))
