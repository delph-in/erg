;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: statistics.lisp
;;;      module: basic report generation routines for tsdb(1) profiles
;;;     version: 0.0 (experimental)
;;;  written by: oe, coli saarbruecken
;;; last update: 17-dec-97
;;;  updated by: oe, coli saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an attempt to collect a number of routines for report generation and
;;; profile analysis.  pretty hacky and incomplete at this point (17-dec-97).
;;;
;;; some examples of how i use some of the functions:
;;;
;;;   - compute coverage summary of profile stored in `oct-97/english/25-nov-97'
;;;     (relative to *tsdb-home*) and write latex(1) code to `bar.tex' (relative
;;;     to the current working directory of lisp process; use `:cd' to change):
;;;
;;;     (analyze-competence "oct-97/english/25-nov-97" :file "/tmp/bar.tex")
;;;
;;;   - overgeneration summary for same profile (data is cached in the lisp
;;;     universe; hence the first access to a database is _slow_ but things
;;;     get much better once the data is cached):
;;;
;;;     (analyze-competence "oct-97/english/25-nov-97" 
;;;                         :wf 0 :file "/tmp/bar.tex")
;;;
;;;   - performance profile for items that had at least one reading (use the
;;;     optional :restrictor argument to specifiy a condition on items to
;;;     exclude):
;;;
;;;     (analyze-performance "oct-97/english/25-nov-97"
;;;                          :restrictor #'(lambda (foo)
;;;                                          (< (get-field :readings foo) 1))
;;;                          :file "/tmp/bar.tex")
;;;
;;;   - aggregate data on the basis of some property other than the phenomena
;;;     classification; e.g. by length:
;;;
;;;     (aggregate "oct-97/english/25-nov-97"
;;;                :dimension :i-length :aggregate 3 :upper 8)
;;;
;;;     the output of aggregate() can then be passed to the analysis functions
;;;     (see above) instead of the database (string) argument.
;;;
;;;   - compare two profiles performance-wise; set labels for the three major
;;;     columns:
;;;
;;;     (compare-performance "oct-97/english/25-nov-97"
;;;                          "oct-97/english/l+pfilter.15-dec-97"
;;;                          :olabel "oct-97" :nlabel "oct-97 (filter)"
;;;                          :clabel "improvement"
;;;                          :file "/tmp/bar.tex")
;;;
;;; besides, i usually keep a matrix latex(1) file that contains a plain
;;; document header plus an \include for `bar.tex' and an xdvi(1) for the
;;; matrix file around; thus, it only takes two command (one in the lisp world,
;;; one from the shell) to view a new profile view.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "TSDB")

(defvar *tsdb-profile-cache* (make-hash-table :size 42 :test #'equal))

(defparameter *statistics-plot-width* 150)

(defparameter *statistics-plot-height* 150)

(defparameter *english-phenomena*
  (list "S_Types"
        "C_Agreement"
        "C_Complementation"
        "C_Diathesis-Active"
        "C_Diathesis-Passive"
        "C_Tense-Aspect-Modality"
        "C_Negation"
        "NP_Agreement"
        "NP_Modification"
        "NP_Coordination"))

(defparameter *hp-phenomena*
  (list "S_Types"
        "C_Agreement"
        "C_Complementation"
;        "C_Diathesis-Active" ;; not a well-defined category for now (nov-97)
        "C_Diathesis-Passive"
        "C_Tense-Aspect-Modality"
        "C_Negation"
        "C_Coordination"
        "C_Modification"
        "NP_Agreement"
        "NP_Modification"
        "NP_Coordination"))

(defun latexify-string (string)
  (if (and (stringp string) (>= (length string) 1))
    (let ((prefix (elt string 0)))
      (if (equal prefix #\_)
        (concatenate 'string "\\_" (latexify-string (subseq string 1)))
        (concatenate 
            'string (string prefix) (latexify-string (subseq string 1)))))
    string))

(defmacro average (values)
  `(let ((length (length ,values)))
     (if (zerop length) 0 (/ (apply #'+ ,values) length))))

(defmacro divide (numerator denominator)
  `(if (zerop ,denominator) 0 (/ ,numerator ,denominator)))

(defmacro sum (values)
  `(apply #'+ ,values))

(defun create-output-stream (file append)
  (cond
   ((or (stringp file) (stringp append))
    (open (if (stringp append) append file)
          :direction :output 
          :if-exists (if append :append :supersede)
          :if-does-not-exist :create))
   ((or file append) (or file append))
   (t *tsdb-io*)))

(defun analyze (condition &optional (language *tsdb-data*))
  (let* ((key (if condition
                (concatenate 'string language "@" condition)
                language))
         (data (gethash key *tsdb-profile-cache*)))
    (if (not data)
      (let* ((parse (select (list "i-id" "readings" "first" "total"
                                  "p-etasks" "p-stasks" "p-ftasks"
                                  "unifications" "copies"
                                  "conses" "symbols" "others"
                                  "words" "edges" "gcs")
                            (list :integer :integer :integer :integer 
                                  :integer :integer :integer 
                                  :integer :integer
                                  :integer :integer :integer 
                                  :integer :integer :integer)
                            "parse" condition language :redirection :output))
             (item (select (list "i-id" "i-length" "i-wf")
                           (list :integer :integer :integer)
                           "item" condition language :redirection :output))
             (all (join parse item :i-id)))
        (setf (gethash key *tsdb-profile-cache*) all)
        all)
      data)))
#+:cray
(defun analyze (condition &optional (language *tsdb-data*))
  (let* ((all (select (list "i-id" "i-length" "i-wf" "p-name"
                            "readings" "first" "total"
                            "p-etasks" "p-stasks" "p-ftasks"
                            "unifications" "copies"
                            "conses" "symbols" "others"
                            "words" "edges" "gcs")
                      (list :integer :integer :integer :string
                            :integer :integer :integer 
                            :integer :integer :integer 
                            :integer :integer
                            :integer :integer :integer 
                            :integer :integer :integer)
                      (list "item" "item-phenomenon" "phenomenon" "parse")
                      condition language :redirection :output)))
    all))

;;;
;;; needs major speedup; should probably use raw data (with phenomenon name)
;;; and sort out the phenomenon grouping itself.           (14-dec-97  -  oe)
;;;
(defun analyze-phenomena (&optional (language *tsdb-data*)
                                    (phenomena *english-phenomena*))

  (let (items)
    (dolist (phenomenon (reverse phenomena) items)
      (let* ((condition (format nil "p-name ~~ \"^~a\"" phenomenon))
             (data (analyze condition language)))
        (push (cons (intern (string-upcase phenomenon) "KEYWORD")
                    (cons phenomenon data))
              items)))))

(defun summarize-coverage-parameters (items
                                      &key restrictor)

  (let ((itemtotal 0)
        (restrictedtotal 0)
        (lengthtotal 0)
        (wordstotal 0)
        (parsestotal 0)
        (readingstotal 0)
        result)
    (dolist (phenomenon items)
      (let* ((data (rest (rest phenomenon)))
             (items (length data))
             (data (if restrictor
                     (remove-if restrictor data)
                     data))
             (restricted (length data))
             (lengths (map 'list #'(lambda (foo)
                                    (get-field :i-length foo))
                           data))
             (wordss (map 'list #'(lambda (foo)
                                    (get-field :words foo))
                          data))
             (parses (remove-if-not #'(lambda (foo)
                                    (>= (get-field :readings foo) 1))
                                    data))
             (readingss (map 'list #'(lambda (foo)
                                       (get-field :readings foo))
                             parses)))
        (push (cons (first phenomenon)
                    (pairlis (list :items :restricted
                                   :i-length :words
                                   :analyses :results)
                             (list items restricted
                                   (average lengths) (average wordss)
                                   (average readingss) (length parses))))
              result)
        (setf itemtotal (+ itemtotal items))
        (setf restrictedtotal (+ restrictedtotal restricted))
        (setf lengthtotal (+ lengthtotal (sum lengths)))
        (setf wordstotal (+ wordstotal (sum wordss)))
        (setf parsestotal (+ parsestotal (length parses)))
        (setf readingstotal (+ readingstotal (sum readingss)))))
    (cons (cons :total
                (pairlis (list :items :restricted
                               :i-length
                               :words
                               :analyses
                               :results)
                         (list itemtotal restrictedtotal
                               (divide lengthtotal restrictedtotal)
                               (divide wordstotal restrictedtotal)
                               (divide readingstotal parsestotal)
                               parsestotal)))
          result)))

(defun compare-competence (olanguage nlanguage
                           &key (olabel "old") (nlabel "new")
                                file append)

  (let* ((oitems
          (if (stringp olanguage) (analyze-phenomena olanguage) olanguage))
         (nitems
          (if (stringp nlanguage) (analyze-phenomena nlanguage) nlanguage))
         (stream (create-output-stream file append))
         (oaverages (summarize-coverage-parameters oitems))
         (owfaverages 
          (summarize-coverage-parameters 
           oitems :restrictor #'(lambda (foo) 
                                  (not (= (get-field :i-wf foo) 1)))))
         (oifaverages 
          (summarize-coverage-parameters 
           oitems :restrictor #'(lambda (foo) 
                                  (not (= (get-field :i-wf foo) 0)))))
         (naverages (summarize-coverage-parameters nitems))
         (nwfaverages 
          (summarize-coverage-parameters 
           nitems :restrictor #'(lambda (foo) 
                                  (not (= (get-field :i-wf foo) 1)))))
         (nifaverages 
          (summarize-coverage-parameters 
           nitems :restrictor #'(lambda (foo) 
                                  (not (= (get-field :i-wf foo) 0))))))
    (format
     stream
     "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|c|@{}}~%  ~
      \\hline~%  ~
      & \\multicolumn{4}{|c|}{\\bf ~a}~%    ~
      & \\multicolumn{4}{|c|}{\\bf ~a}\\\\~%  ~
      {\\bf Phenomenon} ~
      & {\\bf lexical} & {\\bf parser} ~
      & {\\bf in} & {\\bf out}~%    ~
      & {\\bf lexical} & {\\bf parser} ~
      & {\\bf in} & {\\bf out}\\\\~%  ~
      & $\\propto$ & $\\propto$ & \\% & \\%~%   ~
      & $\\propto$ & $\\propto$ & \\% & \\%\\\\~%  ~
      \\hline~%  ~
      \\hline~%"
     olabel nlabel)
    (dolist (phenomenon oitems)
      (let* ((odata (rest (assoc (first phenomenon) oaverages)))
             (owfdata (rest (assoc (first phenomenon) owfaverages)))
             (oifdata (rest (assoc (first phenomenon) oifaverages)))
             (ndata (rest (assoc (first phenomenon) naverages)))
             (nwfdata (rest (assoc (first phenomenon) nwfaverages)))
             (nifdata (rest (assoc (first phenomenon) nifaverages)))
             (name (latexify-string (second phenomenon)))
             (owords (get-field :words odata))
             (oanalyses (get-field :analyses odata))
             (owfrestricted (get-field :restricted owfdata))
             (owfresults (get-field :results owfdata))
             (owfcoverage (if (zerop owfrestricted)
                            100
                            (float (* 100 (/ owfresults owfrestricted)))))
             (oifrestricted (get-field :restricted oifdata))
             (oifresults (get-field :results oifdata))
             (oifcoverage (if (zerop oifrestricted)
                            100
                            (float (* 100 (/ oifresults oifrestricted)))))
             (nwords (get-field :words ndata))
             (nanalyses (get-field :analyses ndata))
             (nwfrestricted (get-field :restricted nwfdata))
             (nwfresults (get-field :results nwfdata))
             (nwfcoverage (if (zerop nwfrestricted)
                            100
                            (float (* 100 (/ nwfresults nwfrestricted)))))
             (nifrestricted (get-field :restricted nifdata))
             (nifresults (get-field :results nifdata))
             (nifcoverage (if (zerop nifrestricted)
                            100
                            (float (* 100 (/ nifresults nifrestricted))))))
        (format
         stream
         "  ~a & ~,2f & ~,2f & ~,1f & ~,1f~%    ~
          & ~,2f & ~,2f & ~,1f & ~,1f\\\\~%"
         name owords oanalyses owfcoverage oifcoverage
         nwords nanalyses nwfcoverage nifcoverage)))
    (let* ((odata (rest (assoc :total oaverages)))
           (owfdata (rest (assoc :total owfaverages)))
           (oifdata (rest (assoc :total oifaverages)))
           (ndata (rest (assoc :total naverages)))
           (nwfdata (rest (assoc :total nwfaverages)))
           (nifdata (rest (assoc :total nifaverages)))
           (name "Total")
           (owords (get-field :words odata))
           (oanalyses (get-field :analyses odata))
           (owfrestricted (get-field :restricted owfdata))
           (owfresults (get-field :results owfdata))
           (owfcoverage (if (zerop owfrestricted)
                           100
                          (float (* 100 (/ owfresults owfrestricted)))))
           (oifrestricted (get-field :restricted oifdata))
           (oifresults (get-field :results oifdata))
           (oifcoverage (if (zerop oifrestricted)
                          100
                          (float (* 100 (/ oifresults oifrestricted)))))
           (nwords (get-field :words ndata))
           (nanalyses (get-field :analyses ndata))
           (nwfrestricted (get-field :restricted nwfdata))
           (nwfresults (get-field :results nwfdata))
           (nwfcoverage (if (zerop nwfrestricted)
                          100
                          (float (* 100 (/ nwfresults nwfrestricted)))))
           (nifrestricted (get-field :restricted nifdata))
           (nifresults (get-field :results nifdata))
           (nifcoverage (if (zerop nifrestricted)
                          100
                          (float (* 100 (/ nifresults nifrestricted))))))
      (format
       stream
       "  \\hline~%  \\hline~%  ~
        {\\bf ~a} & {\\bf ~,2f} & {\\bf ~,2f} ~
        & {\\bf ~,1f} & {\\bf ~,1f}~%    ~
        & {\\bf ~,2f} & {\\bf ~,2f} ~
        & {\\bf ~,1f} & {\\bf ~,1f}\\\\~%  \\hline~%~
        \\end{tabular}~%"
       name owords oanalyses owfcoverage oifcoverage
       nwords nanalyses nwfcoverage nifcoverage))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun analyze-competence (&optional (language *tsdb-data*)
                           &key (wf 1) file append)

  (let* ((items (if (stringp language) (analyze-phenomena language) language))
         (stream (create-output-stream file append))
         (averages 
          (summarize-coverage-parameters 
           items :restrictor #'(lambda (foo) 
                                 (not (= (get-field :i-wf foo) wf))))))
    (format
     stream
     "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|@{}}~%  ~
      \\hline~%  ~
      \\multicolumn{8}{|c|}~%    {\\bf `{\\bf ~a}' ~a Profile~%     ~
      {\\bf [~a]}}\\\\~%  \\hline\\hline~%  ~
      & {\\bf  total} & {\\bf ~a} & {\\bf word} & {\\bf lexical}~%    ~
        & {\\bf parser} & {\\bf total} & {\\bf overall}\\\\~%  ~
      {\\bf Phenomenon} & {\\bf items} & {\\bf items} & {\\bf string}~%    ~
        & {\\bf items} & {\\bf analyses} & {\\bf results}~%    ~
        & {\\bf coverage}\\\\~%  ~
      & $\\sharp$ & $\\sharp$ & $\\propto$ & $\\propto$ & $\\propto$~%    ~
        & $\\sharp$ & $\\%$\\\\~%  ~
      \\hline~%  ~
      \\hline~%"
     (if (stringp language) language "")
     (if (= wf 1) "Coverage" "Overgeneration")
     (current-time :long t)
     (if (= wf 1) "positive" "negative"))
    (dolist (phenomenon items)
      (let* ((data (rest (assoc (first phenomenon) averages)))
             (name (latexify-string (second phenomenon)))
             (items (get-field :items data))
             (restricted (get-field :restricted data))
             (length (get-field :i-length data))
             (words (get-field :words data))
             (analyses (get-field :analyses data))
             (results (get-field :results data))
             (coverage (if (zerop restricted)
                         100
                         (float (* 100 (/ results restricted))))))
        (format
         stream
         "  ~a & ~d & ~d & ~,2f & ~,2f & ~,2f & ~d & ~,1f\\\\~%"
         name items restricted length words analyses results coverage)))
    (let* ((data (rest (assoc :total averages)))
           (name "Total")
           (items (get-field :items data))
           (restricted (get-field :restricted data))
           (length (get-field :i-length data))
           (words (get-field :words data))
           (analyses (get-field :analyses data))
           (results (get-field :results data))
           (coverage (if (zerop restricted)
                       100
                       (float (* 100 (/ results restricted))))))
      (format
       stream
       "  \\hline~%  \\hline~%  ~
        {\\bf ~a} & {\\bf ~d} & {\\bf ~d} & {\\bf ~,2f} & {\\bf ~,2f}~%    ~
        & {\\bf ~,2f} & {\\bf ~d} & {\\bf ~,1f}\\\\~%  \\hline~%~
        \\end{tabular}~%"
       name items restricted length words analyses results coverage))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun summarize-performance-parameters (items
                                         &key restrictor)

  (let ((itemtotal 0)
        (readingstotal 0)
        (etaskstotal 0)
        (staskstotal 0)
        (ftaskstotal 0)
        (edgestotal 0)
        (firsttotal 0)
        (totaltotal 0)
        (spacetotal 0)
        result)
    (dolist (phenomenon items)
      (let* ((data (rest (rest phenomenon)))
             (data (remove -1 data :key #'(lambda (foo)
                                            (get-field :readings foo))))
             (data (if restrictor
                     (remove-if restrictor data)
                     data))
             (items (length data))
             (readingss
              (length (remove 0 data :key #'(lambda (foo)
                                              (get-field :readings foo)))))
             (etaskss (map 'list #'(lambda (foo)
                                     (get-field :p-etasks foo))
                           data))
             (staskss (map 'list #'(lambda (foo)
                                     (get-field :p-stasks foo))
                           data))
             (ftaskss (map 'list #'(lambda (foo)
                                     (get-field :p-ftasks foo))
                           data))
             (edgess (map 'list #'(lambda (foo)
                                    (get-field :edges foo))
                          data))
             (firsts (map 'list #'(lambda (foo)
                                    (let ((first (get-field :first foo)))
                                      (if (> first 0)
                                        (/ first 10)
                                        0)))
                          data))
             (totals (map 'list #'(lambda (foo)
                                    (/ (get-field :total foo) 10))
                          data))
             (space (map 'list #'(lambda (foo)
                                   (+ (* 8 (get-field :conses foo))
                                      (* 24 (get-field :symbols foo))
                                      (get-field :others foo)))
                         data)))
        (push (cons (first phenomenon)
                    (pairlis (list :items :readingss
                                   :p-etasks :p-stasks
                                   :p-ftasks :edges
                                   :first :total :space)
                             (list items readingss
                                   (average etaskss) (average staskss)
                                   (average ftaskss) (average edgess)
                                   (average firsts) (average totals)
                                   (average space))))
              result)
        (setf itemtotal (+ itemtotal items))
        (setf readingstotal (+ readingstotal readingss))
        (setf etaskstotal (+ etaskstotal (sum etaskss)))
        (setf staskstotal (+ staskstotal (sum staskss)))
        (setf ftaskstotal (+ ftaskstotal (sum ftaskss)))
        (setf edgestotal (+ edgestotal (sum edgess)))
        (setf firsttotal (+ firsttotal (sum firsts)))
        (setf totaltotal (+ totaltotal (sum totals)))
        (setf spacetotal (+ spacetotal (sum space)))))
    (cons (cons :total
                (pairlis (list :items :readings
                               :p-etasks
                               :p-stasks
                               :p-ftasks
                               :edges
                               :first
                               :total
                               :space)
                         (list itemtotal readingstotal
                               (divide etaskstotal itemtotal)
                               (divide staskstotal itemtotal)
                               (divide ftaskstotal itemtotal)
                               (divide edgestotal itemtotal)
                               (divide firsttotal readingstotal)
                               (divide totaltotal itemtotal)
                               (divide spacetotal itemtotal))))
          result)))

(defun aggregate (language
                  &key (condition nil)
                       (restrictor nil)
                       (dimension :i-length)
                       (aggregate 2)
                       (lower 0)
                       (upper nil))
  (let* ((items (if (stringp language) (analyze condition language) language))
         (items (remove-if #'(lambda (foo)
                               (< (get-field dimension foo) lower))
                           items))
         (items (if upper
                  (remove-if #'(lambda (foo)
                                 (> (get-field dimension foo) upper))
                             items)
                  items))
         (restrictor (case restrictor
                       ((:wf :well-formed :grammatical :positive)
                        #'(lambda (foo) (not (equal (get-field :i-wf foo) 1))))
                       ((:if :ill-formed :ungrammatical :negative)
                        #'(lambda (foo) (not (equal (get-field :i-wf foo) 0))))
                       ((:parses :accepts :analyses)
                        #'(lambda (foo)
                            (not (> (get-field :readings foo) 0))))
                       (:rejects
                        #'(lambda (foo)
                            (not (equal (get-field :readings foo) 0))))))
         (items (if (and restrictor (functionp restrictor))
                  (remove-if restrictor items)
                  items))
         (values (map 'list #'(lambda (foo) (get-field dimension foo)) items)))
    (when values
      (let* ((minimum (apply #'min values))
             (aminimum (floor (/ minimum aggregate)))
             (maximum (apply #'max values))
             (amaximum (floor (/ maximum aggregate)))
             (storage (make-array (+ (- amaximum aminimum) 1)
                                  :initial-element nil))
             result)
        (dolist (item items)
          (let* ((value (get-field dimension item))
                 (class (- (floor (/ value aggregate)) aminimum)))
            (push item (aref storage class))))
        (dotimes (i (+ (- amaximum aminimum) 1) result)
          (print i)
          (let* ((class (* (+ i aminimum) aggregate))
                 (name (format 
                        nil 
                        "\\multicolumn{1}{|c|}~
                         {~d $\\leq$ {\\em ~(~a~)\\/} $<$ ~d}"
                        class dimension (+ class aggregate))))
            (push (cons class (cons name (aref storage i))) result)))))))

(defun analyze-performance (&optional (language *tsdb-data*)
                            &key restrictor file append)

  (let* ((items (if (stringp language) (analyze-phenomena language) language))
         (stream (create-output-stream file append))
         (averages
          (summarize-performance-parameters items :restrictor restrictor)))
    (format
     stream
     "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|@{}}~%  ~
      \\hline~%  ~
      \\multicolumn{8}{|c|}~%    {\\bf `{\\bf ~a}' Performance Profile~%     ~
      {\\bf [~a]}}\\\\~%  \\hline~%  \\hline~%  ~
      \\raisebox{-1.5ex}[0ex][0ex]{\\bf Phenomenon}~%    & {\\bf items} ~
        & {\\bf etasks} & {\\bf filter} & {\\bf edges}~%    ~
        & {\\bf first}  & {\\bf total} & {\\bf space}\\\\~%  ~
      & $\\sharp$ & $\\propto$ & \\% & $\\propto$~%    ~
        & $\\propto$ (s) & $\\propto$ (s) & $\\propto$ (kb)\\\\~%  ~
      \\hline~%  ~
      \\hline~%"
     (if (stringp language) language)
     (current-time :long t))
    (dolist (phenomenon items)
      (let* ((data (rest (assoc (first phenomenon) averages))))
        (when data
          (let* ((name (latexify-string (second phenomenon)))
                 (items (get-field :items data))
                 (etasks (round (get-field :p-etasks data)))
                 (ftasks (round (get-field :p-ftasks data)))
                 (filter (float (* 100 (divide ftasks (+ etasks ftasks)))))
                 (edges (round (get-field :edges data)))
                 (first (float (get-field :first data)))
                 (total (float (get-field :total data)))
                 (space (round (/ (get-field :space data) (expt 2 10)))))
            (format
             stream
             "  ~a~%    & ~d & ~d & ~,1f & ~d & ~,1f & ~,1f & ~d\\\\~%"
             name items etasks filter edges first total space)))))
    (format stream "  \\hline~%  \\hline~%")
    (let* ((data (rest (assoc :total averages)))
           (name "Total")
           (items (get-field :items data))
           (etasks (round (get-field :p-etasks data)))
           (ftasks (round (get-field :p-ftasks data)))
           (filter (float (* 100 (/ ftasks (+ etasks ftasks)))))
           (edges (round (get-field :edges data)))
           (first (float (get-field :first data)))
           (total (float (get-field :total data)))
           (space (round (/ (get-field :space data) (expt 2 10)))))
      (format
       stream
       "  {\\bf ~a} & {\\bf ~d}~%    ~
        & {\\bf ~d} & {\\bf ~,1f} & {\\bf ~d} & {\\bf ~,1f}~%    ~
        & {\\bf ~,1f} & {\\bf ~d}\\\\~%"
       name items etasks filter edges first total space))
    (format stream "  \\hline~%\\end{tabular}")
    (when (or (stringp file) (stringp append)) (close stream))))

(defun compare-performance (olanguage nlanguage 
                            &key (format :table)
                                 (olabel "old") (nlabel "new")
                                 (clabel "reduction")
                                 orestrictor nrestrictor restrictor
                                 file append)

  (let* ((oitems 
          (if (stringp olanguage) (analyze-phenomena olanguage) olanguage))
         (nitems 
          (if (stringp nlanguage) (analyze-phenomena nlanguage) nlanguage))
         (stream (create-output-stream file append))
         (oaverages (summarize-performance-parameters 
                     oitems :restrictor (or orestrictor restrictor)))
         (naverages (summarize-performance-parameters 
                     nitems :restrictor (or nrestrictor restrictor))))
    (case format
      (:table
       (format
        stream
        "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|c|c|@{}}~%  ~
         \\hline~%  ~
         & \\multicolumn{3}{|c|}{\\bf ~a}~%    ~
         & \\multicolumn{3}{|c|}{\\bf ~a}~%    ~
         & \\multicolumn{3}{|c|}{\\bf ~a}\\\\~%  ~
         {\\bf Phenomenon} ~
           & {\\bf tasks} & {\\bf time} & {\\bf space}~%    ~
           & {\\bf tasks} & {\\bf time} & {\\bf space}~%    ~
           & {\\bf tasks} & {\\bf time} & {\\bf space}\\\\~%  ~
         & $\\propto$ & $\\propto$ (s) & $\\propto$ (kb)~%   ~
         & $\\propto$ & $\\propto$ (s)& $\\propto$ (kb)~%   ~
           & $\\%$ & $\\%$ & $\\%$\\\\~%  ~
         \\hline~%  ~
         \\hline~%"
        olabel nlabel clabel)))
    (do* ((oitems oitems (rest oitems))
          (phenomenon (first oitems) (first oitems))
          (position (length oitems) (1- position)))
        ((null oitems))
      (let* ((odata (rest (assoc (first phenomenon) oaverages)))
             (ndata (rest (assoc (first phenomenon) naverages)))
             (name (latexify-string (second phenomenon)))
             (oetasks (round (get-field :p-etasks odata)))
             (otime (float (get-field :total odata)))
             (ospace (round (/ (get-field :space odata) (expt 2 10))))
             (otimepertask (divide otime oetasks))
             (ospacepertask (divide ospace oetasks))
             (netasks (round (get-field :p-etasks ndata)))
             (ntime (float (get-field :total ndata)))
             (nspace (round (/ (get-field :space ndata) (expt 2 10))))
             (ntimepertask (divide ntime netasks))
             (nspacepertask (divide nspace netasks))
             (taskreduction 
              (float (* 100 (divide (- oetasks netasks) oetasks))))
             (timereduction 
              (float (* 100 (divide (- otime ntime) otime))))
             (spacereduction 
              (float (* 100 (divide (- ospace nspace) ospace)))))
        (case format
          (:table
           (format
            stream
            "  ~a~%     & ~d & ~,1f & ~d ~
             & ~d & ~,1f & ~d ~
             & ~,1f & ~,1f & ~,1f\\\\~%"
            name oetasks otime ospace netasks ntime nspace
            taskreduction timereduction spacereduction))
          (:graph
           (format
            stream
            "    ~,3f ~f~%    ~,3f ~f~%"
            (divide (- otimepertask ntimepertask) otimepertask) 
            (+ position 0.8)
            (divide (- ospacepertask nspacepertask) ospacepertask)
            (+ position 0.4)
;            (/ taskreduction 100) (+ position 0.77)
;            (/ timereduction 100) (+ position 0.5)
;            (/ spacereduction 100) (+ position 0.23))          
           )))))
           
    (let* ((odata (rest (assoc :total oaverages)))
           (ndata (rest (assoc :total naverages)))
           (name "Total")
           (oetasks (round (get-field :p-etasks odata)))
           (otime (float (get-field :total odata)))
           (ospace (round (/ (get-field :space odata) (expt 2 10))))
           (otimepertask (divide otime oetasks))
           (ospacepertask (divide ospace oetasks))
           (netasks (round (get-field :p-etasks ndata)))
           (ntime (float (get-field :total ndata)))
           (nspace (round (/ (get-field :space ndata) (expt 2 10))))
           (ntimepertask (divide ntime netasks))
           (nspacepertask (divide nspace netasks))
           (taskreduction 
            (float (* 100 (divide (- oetasks netasks) oetasks))))
           (timereduction 
            (float (* 100 (divide (- otime ntime) otime))))
           (spacereduction 
            (float (* 100 (divide (- ospace nspace) ospace)))))
      (case format
        (:table
         (format
          stream 
          "  \\hline~%  \\hline~%  ~
           {\\bf ~a} & {\\bf ~d} & {\\bf ~,1f} & {\\bf ~d}~%    ~
           & {\\bf ~d} & {\\bf ~,1f} & {\\bf ~d}~%    ~
           & {\\bf ~,1f} & {\\bf ~,1f} & {\\bf ~,1f}\\\\~%"
          name oetasks otime ospace netasks ntime nspace
          taskreduction timereduction spacereduction)
         (format stream "  \\hline~%\\end{tabular}"))
        (:graph
         (format
          stream
          "    ~,3f ~f~%    ~,3f ~f~%"
          (divide (- otimepertask ntimepertask) otimepertask) 0.8
          (divide (- ospacepertask nspacepertask) ospacepertask) 0.4
;          (/ taskreduction 100) 0.77
;          (/ timereduction 100) 0.5
;          (/ spacereduction 100) 0.23)          
           ))))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun graph-words (data &key file append)
  (let* ((stream (create-output-stream file append))
         (mwords 
          (do ((data data (rest data))
               (wordss (list (get-field :words (first data)))
                       (cons (get-field :words (first data)) wordss)))
              ((null data) (apply #'max wordss))))
         (all (make-array (+ mwords 1) :initial-element 0))
         (parses (make-array (+ mwords 1) :initial-element 0)))
    (multiple-value-bind (mall mparses)
        (do* ((data data (rest data))
              (current (first data) (first data))
              (words (get-field :words current) (get-field :words current))
              (mall 0 mall)
              (mparses 0 mparses))
            ((null data) (values mall mparses))
          (setf mall (max mall (incf (aref all words))))
          (when (> (get-field :readings current) 0)
            (setf mparses (max mparses (incf (aref parses words))))))
      ;;;
      ;;; plot words histogram
      ;;;
      (let ((breadth (* 0.7 (/ *statistics-plot-width* mwords))))
        (format stream "\\beginpicture~%")
        (format stream 
                "  \\setcoordinatesystem units <~,2fmm,~,2fmm>~%"
                (/ *statistics-plot-width* mwords)
                (/ *statistics-plot-height* mall))
        (format stream 
                "  \\setplotarea x from 0 to ~d, y from 0 to ~d~%"
                mwords mall)
        (format stream 
                "  \\axis bottom ticks numbered from 0 to ~d by 5 /~%"
                mwords)
        (format stream 
                "  \\axis left ticks numbered from 0 to ~d by 50 /~%" 
                mall)
        (format stream 
                "  \\setbars breadth <~,2fmm> baseline at y = 0~%"
                breadth)
        (format stream "  \\plot~%")
        (do ((i 1 (+ i 1)))
            ((> i mwords))
          (when (> (aref all i) 0)
            (format stream "    ~3d ~4d~%" i (aref all i))))
        (format stream "  /~%")
        (format stream "  \\linethickness=~,2fmm~%" breadth)
        (format stream "  \\setbars breadth <0mm> baseline at y = 0~%")
        (format stream "  \\plot~%")
        (do ((i 1 (+ i 1)))
            ((> i mwords))
          (when (> (aref parses i) 0)
            (format stream "    ~3d ~4d~%" i (aref parses i))))
        (format stream "  /~%")
        (format stream "\\endpicture~%")))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun plot-words-first (data &key file append)
  (let* ((stream (create-output-stream file append))
         (mwords 
          (do ((data data (rest data))
               (wordss (list (get-field :words (first data)))
                       (cons (get-field :words (first data)) wordss)))
              ((null data) (apply #'max wordss))))
         (firsts (make-array (+ mwords 1) :initial-element nil))
         (totals (make-array (+ mwords 1) :initial-element nil)))
    (multiple-value-bind (mfirst mtotal)
        (do* ((data data (rest data))
              (current (first data) (first data))
              (words (get-field :words current) (get-field :words current))
              (first (get-field :first current) (get-field :first current))
              (mfirst 0 (if first (max mfirst first) mfirst))
              (total (get-field :total current) (get-field :total current))
              (mtotal 0 (if total (max mtotal total) mtotal)))
            ((null data) (values (/ mfirst 10) (/ mtotal 10)))
          (push (/ first 10) (aref firsts words))
          (push (/ total 10) (aref totals words)))
      ;;;
      ;;; plot words # first graph
      ;;;
      (format stream "\\beginpicture~%")
      (format stream 
              "  \\setcoordinatesystem units <~,2fmm,~,2fmm>~%"
              (/ *statistics-plot-width* mwords)
              (/ *statistics-plot-height* (ceiling mfirst)))
      (format stream 
              "  \\setplotarea x from 0 to ~d, y from 0 to ~d~%"
              mwords (ceiling mfirst))
      (format stream "  \\setplotsymbol({\\rule{.4pt}{.4pt}})~%")
      (format stream "  \\setlinear~%")
      (format stream 
              "  \\axis bottom ticks numbered from 0 to ~d by 5 /~%"
              mwords)
      (format stream 
              "  \\axis left ticks numbered from 0 to ~d by 50 /~%" 
              (ceiling mfirst))
      (format stream "  \\multiput{\\tiny$\\circ$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (do ((times (aref firsts i) (rest times)))
          ((null times))
          (format stream "    ~4d ~8,2f~%" i (first times))))
      (format stream "  /~%")
      (format stream "  \\multiput{\\large$\\circ$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref firsts i)
          (format stream "    ~4d ~8,2f~%" i (average (aref firsts i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref firsts i)
          (format stream "    ~4d ~8,2f~%" i (average (aref firsts i)))))
      (format stream "  /~%")
      (format stream "\\endpicture~%"))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun plot-words-total (data &key file append
                                   (threshold 1))
  (let* ((stream (create-output-stream file append))
         (mwords 
          (do ((data data (rest data))
               (wordss (list (get-field :words (first data)))
                       (cons (get-field :words (first data)) wordss)))
              ((null data) (apply #'max wordss))))
         (firsts (make-array (+ mwords 1) :initial-element nil))
         (totals (make-array (+ mwords 1) :initial-element nil)))
    (multiple-value-bind (mfirst mtotal)
        (do* ((data data (rest data))
              (current (first data) (first data))
              (words (get-field :words current) (get-field :words current))
              (first (get-field :first current) (get-field :first current))
              (mfirst 0 (if first (max mfirst first) mfirst))
              (total (get-field :total current) (get-field :total current))
              (mtotal 0 (if total (max mtotal total) mtotal)))
            ((null data) (values (/ mfirst 10) (/ mtotal 10)))
          (push (/ first 10) (aref firsts words))
          (push (/ total 10) (aref totals words)))
      ;;;
      ;;; plot words # total graph (with words # first overlay)
      ;;;
      (format stream "\\beginpicture~%")
      (format stream 
              "  \\setcoordinatesystem units <~,2fmm,~,2fmm>~%"
              (/ *statistics-plot-width* mwords)
              (/ *statistics-plot-height* (ceiling mtotal)))
      (format stream 
              "  \\setplotarea x from 0 to ~d, y from 0 to ~d~%"
              mwords (ceiling mtotal))
      (format stream "  \\setplotsymbol({\\rule{.4pt}{.4pt}})~%")
      (format stream "  \\setlinear~%")
      (format stream 
              "  \\axis bottom ticks numbered from 0 to ~d by 5 /~%"
              mwords)
      (format stream 
              "  \\axis left ticks numbered from 0 to ~d by 10 /~%" 
              (ceiling mtotal))
      (format stream "  \\multiput{\\tiny$\\bullet$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (do ((times (aref totals i) (rest times)))
          ((null times))
          (format stream "    ~4d ~8,2f~%" i (first times))))
      (format stream "  /~%")
      (format stream "  \\multiput{\\large$\\circ$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref firsts i)
          (format stream "    ~4d ~8,2f~%" i (average (aref firsts i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref firsts i)
          (format stream "    ~4d ~8,2f~%" i (average (aref firsts i)))))
      (format stream "  /~%")
      (format stream "  \\multiput{\\large$\\bullet$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref totals i)
          (format stream "    ~4d ~8,2f~%" i (average (aref totals i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref totals i)
          (format stream "    ~4d ~8,2f~%" i (average (aref totals i)))))
      (format stream "  /~%")
      (format stream "\\endpicture~%"))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun plot-tasks-total (data &key file append)
  (let* ((stream (create-output-stream file append))
         (data 
          (remove -1 data :key #'(lambda (foo) (get-field :p-etasks foo))))
         (data 
          (remove -1 data :key #'(lambda (foo) (get-field :first foo))))
         (data 
          (remove -1 data :key #'(lambda (foo) (get-field :total foo))))
         (data 
          (remove -1 data :key #'(lambda (foo) (get-field :readings foo))))
         (mtasks
          (apply #'max (map 'list #'(lambda (foo) 
                                      (get-field :p-etasks foo))
                            data)))
         (firsts (make-array (+ (ceiling (/ mtasks 50)) 1)
                             :initial-element nil))
         (totals (make-array (+ (ceiling (/ mtasks 50)) 1)
                             :initial-element nil)))
    (multiple-value-bind (mfirst mtotal)
        (do* ((data data (rest data))
              (current (first data) (first data))
              (tasks (get-field :p-etasks current)
                     (get-field :p-etasks current))
              (first (get-field :first current) (get-field :first current))
              (mfirst 0 (if first (max mfirst first) mfirst))
              (total (get-field :total current) (get-field :total current))
              (mtotal 0 (if total (max mtotal total) mtotal)))
            ((null data) (values (/ mfirst 10) (/ mtotal 10)))
          (push (/ first 10) (aref firsts (floor (/ tasks 50))))
          (push (/ total 10) (aref totals (floor (/ tasks 50)))))
      ;;;
      ;;; plot tasks # total graph (with tasks # first overlay)
      ;;;
      (format stream "\\beginpicture~%")
      (format stream 
              "  \\setcoordinatesystem units <~,2fmm,~,2fmm>~%"
              (/ *statistics-plot-width* (ceiling (/ mtasks 50)))
              (/ *statistics-plot-height* (ceiling mtotal)))
      (format stream 
              "  \\setplotarea x from 0 to ~d, y from 0 to ~d~%"
              (ceiling (/ mtasks 50)) (ceiling mtotal))
      (format stream "  \\setplotsymbol({\\rule{.4pt}{.4pt}})~%")
      (format stream "  \\setlinear~%")
      (format stream 
              "  \\axis bottom ticks numbered from 0 to ~d by 50 /~%"
              (ceiling (/ mtasks 50)))
      (format stream 
              "  \\axis left ticks numbered from 0 to ~d by 10 /~%" 
              (ceiling mtotal))
      (format stream "  \\multiput{\\tiny$\\bullet$} at~%")
      (do ((i 0 (+ i 1)))
          ((> i (floor (/ mtasks 50))))
        (do ((times (aref totals i) (rest times)))
            ((null times))
          (format stream "    ~4d ~8,2f~%" i (first times))))
      (format stream "  /~%")
      (format stream "  \\multiput{\\large$\\circ$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i (floor (/ mtasks 50))))
        (when (aref firsts i)
          (format stream "    ~4d ~8,2f~%" i (average (aref firsts i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i (floor (/ mtasks 50))))
        (when (aref firsts i)
          (format stream "    ~4d ~8,2f~%" i (average (aref firsts i)))))
      (format stream "  /~%")
      (format stream "  \\multiput{\\large$\\bullet$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i (floor (/ mtasks 50))))
        (when (aref totals i)
          (format stream "    ~4d ~8,2f~%" i (average (aref totals i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i (floor (/ mtasks 50))))
        (when (aref totals i)
          (format stream "    ~4d ~8,2f~%" i (average (aref totals i)))))
      (format stream "  /~%")
      (format stream "\\endpicture~%"))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun plot-words-etasks-stasks-ftasks (data &key file append
                                                  (threshold 1))
  (let* ((stream (create-output-stream file append))
         (mwords 
          (do ((data data (rest data))
               (wordss (list (get-field :words (first data)))
                       (cons (get-field :words (first data)) wordss)))
              ((null data) (apply #'max wordss))))
         (etaskss (make-array (+ mwords 1) :initial-element nil))
         (staskss (make-array (+ mwords 1) :initial-element nil))
         (ftaskss (make-array (+ mwords 1) :initial-element nil))
         (aetasks 0)
         (astasks 0)
         (aftasks 0))
    (multiple-value-bind (metasks mstasks mftasks)
        (do* ((data data (rest data))
              (current (first data) (first data))
              (words (get-field :words current) (get-field :words current))
              (etasks (get-field :p-etasks current)
                      (get-field :p-etasks current))
              (stasks (get-field :p-stasks current)
                      (get-field :p-stasks current))
              (ftasks (get-field :p-ftasks current)
                      (get-field :p-ftasks current))
              (metasks 0 (if etasks (max metasks etasks) metasks))
              (mstasks 0 (if stasks (max mstasks stasks) mstasks))
              (mftasks 0 (if ftasks (max mftasks ftasks) mftasks)))
            ((null data) (values metasks mstasks mftasks))
          (when etasks
            (push etasks (aref etaskss words)))
          (when stasks
            (push stasks (aref staskss words)))
          (when ftasks
            (push ftasks (aref ftaskss words))))
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (let ((etasks (aref etaskss i))
              (stasks (aref staskss i))
              (ftasks (aref ftaskss i)))
          (when etasks
            (setf aetasks (max aetasks (average etasks))))
          (when stasks
            (setf astasks (max astasks (average stasks))))
          (when ftasks
            (setf aftasks (max aftasks (average ftasks))))))
      ;;;
      ;;; plot words # etasks graph (with words # stasks overlay)
      ;;;
      (format stream "\\beginpicture~%")
      (format stream 
              "  \\setcoordinatesystem units <~,4fmm,~,4fmm>~%"
              (/ *statistics-plot-width* mwords)
              (/ *statistics-plot-height* (ceiling aetasks)))
      (format stream 
              "  \\setplotarea x from 0 to ~d, y from 0 to ~d~%"
              mwords (ceiling aetasks))
      (format stream "  \\setplotsymbol({\\rule{.4pt}{.4pt}})~%")
      (format stream "  \\setlinear~%")
      (format stream 
              "  \\axis bottom ticks numbered from 0 to ~d by 5 /~%"
              mwords)
      (format stream 
              "  \\axis left ticks numbered from 0 to ~d by 100 /~%" 
              (ceiling aetasks))
      (format stream "  \\multiput{\\large$\\bullet$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref etaskss i)
          (format stream "    ~4d ~8,2f~%" i (average (aref etaskss i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref etaskss i)
          (format stream "    ~4d ~8,2f~%" i (average (aref etaskss i)))))
      (format stream "  /~%")
      (format stream "  \\multiput{\\large$\\circ$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref staskss i)
          (format stream "    ~4d ~8,2f~%" i (average (aref staskss i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref staskss i)
          (format stream "    ~4d ~8,2f~%" i (average (aref staskss i)))))
      (format stream "  /~%")
      (format stream "\\endpicture~%"))
    (when (or (stringp file) (stringp append)) (close stream))))
