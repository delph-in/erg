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

(defun latexify-string (string)
  (if (and (stringp string) (>= (length string) 1))
    (let ((prefix (elt string 0)))
      (if (equal prefix #\_)
        (concatenate 'string "\\_" (latexify-string (subseq string 1)))
        (concatenate 
            'string (string prefix) (latexify-string (subseq string 1)))))
    string))

(defmacro average (values)
  `(/ (apply #'+ ,values) (length ,values)))

(defun analyze (condition &optional (language *tsdb-data*))
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
                        "parse" condition language))
         (item (select (list "i-id" "i-length" "i-wf")
                        (list :integer :integer :integer)
                        "item" condition language))
         (all (join parse item :i-id))
         (successful (remove-if #'(lambda (result)
                                    (<= (get-field :readings result) 0))
                                all)))
    (values all successful)))

(defun analyze-coverage (condition &optional (language *tsdb-data*))
  (let ((items (if (stringp language) nil language))
        (ttotal 0)
        (twf nil)
        (tnwf 0)
        (tparses nil)
        (tnparses 0)
        (treadingss nil))
    (unless items
      (dolist (phenomenon (reverse *english-phenomena*))
        (let* ((condition (format nil "p-name ~~ \"^~a\"" phenomenon))
               (data (analyze condition language)))
          (push (cons (intern (string-upcase phenomenon) "KEYWORD")
                      (cons phenomenon data))
                items))))
    (format
     *tsdb-io*
     "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|@{}}~%  ~
      \\hline~%  ~
      & {\\bf  total} & {\\bf positive} & {\\bf word} & {\\bf lexical}~%    ~
        & {\\bf parser} & {\\bf total} & {\\bf overall}\\\\~%  ~
      {\\bf Phenomenon} & {\\bf items} & {\\bf items} & {\\bf string}~%    ~
        & {\\bf items} & {\\bf analyses} & {\\bf results}~%    ~
        & {\\bf coverage}\\\\~%  ~
      & $\\sharp$ & $\\sharp$ & $\\propto$ & $\\propto$ & $\\propto$~%    ~
        & $\\sharp$ & $\\%$\\\\~%  ~
      \\hline~%  ~
      \\hline~%")
    (dolist (phenomenon items)
      (let* ((data (rest (rest phenomenon)))
             (total (length data))
             (wf (remove-if-not #'(lambda (foo)
                                    (= (get-field :i-wf foo) 1)) 
                                data))
             (nwf (length wf))
             (parses (remove-if-not #'(lambda (foo)
                                    (>= (get-field :readings foo) 1))
                                    wf))
             (nparses (length parses))
             (lengths (map 'list #'(lambda (foo)
                                     (get-field :i-length foo))
                           wf))
             (alength (if lengths (average lengths) 0))
             (wordss (map 'list #'(lambda (foo)
                                    (get-field :words foo))
                          wf))
             (awords (if wordss (average wordss) 0))
             (readingss (map 'list #'(lambda (foo)
                                       (get-field :readings foo))
                             wf))
             (readingss (remove-if #'zerop readingss))
             (areadings (if readingss (average readingss) 0))
             (coverage
              (if (or (zerop nparses) (zerop nwf)) 0 (/ nparses nwf))))
        (format
         *tsdb-io*
         "  ~a & ~d & ~d & ~,2f & ~,2f & ~,2f & ~d & ~,1f\\\\~%"
         (latexify-string (second phenomenon))
         total
         nwf
         alength awords areadings
         nparses (* coverage 100))
        (setf ttotal (+ ttotal total))
        (setf tnwf (+ tnwf nwf))
        (setf tnparses (+ tnparses nparses))
        (setf twf (append wf twf))
        (setf tparses (append parses tparses))
        (setf treadingss (append readingss treadingss))))
    (let* ((tlengths (map 'list #'(lambda (foo)
                                    (get-field :i-length foo))
                          twf))
           (talength (if tlengths (average tlengths) 0))
           (twordss (map 'list #'(lambda (foo)
                                   (get-field :words foo))
                         twf))
           (tawords (if twordss (average twordss) 0))
           (tareadings (if treadingss (average treadingss) 0))
           (tcoverage
            (if (or (zerop tnparses) (zerop tnwf)) 0 (/ tnparses tnwf))))
      (format *tsdb-io* "  \\hline~%  \\hline~%")
      (format
       *tsdb-io*
       "  {\\bf ~a} & {\\bf ~d} & {\\bf ~d} & {\\bf ~,2f} & {\\bf ~,2f} ~%    ~
        & {\\bf ~,2f} & {\\bf ~d} & {\\bf ~,1f}\\\\~%"
       "Total"
       ttotal
       tnwf
       talength tawords tareadings
       tnparses (* 100 tcoverage))
      (format *tsdb-io* "  \\hline~%\\end{tabular}"))
    (when (stringp language) items)))

(defun analyze-performance (&optional (olanguage *tsdb-data*)
                                      (nlanguage *tsdb-data*))
  (let ((oitems (if (stringp olanguage) nil olanguage))
        (nitems (if (stringp nlanguage) nil nlanguage))
        (total 0)
        (outotal 0)
        (ottotal 0)
        (nutotal 0)
        (nttotal 0))
    (unless oitems
      (dolist (phenomenon (reverse *english-phenomena*))
        (let* ((condition (format nil "p-name ~~ \"^~a\"" phenomenon))
               (data (analyze condition olanguage)))
          (push (cons (intern (string-upcase phenomenon) "KEYWORD") data)
                oitems))))
    (unless nitems
      (dolist (phenomenon (reverse *english-phenomena*))
        (let* ((condition (format nil "p-name ~~ \"^~a\"" phenomenon))
               (data (analyze condition nlanguage)))
          (push (cons (intern (string-upcase phenomenon) "KEYWORD") data)
                nitems))))
    (format
     *tsdb-io*
     "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|@{}}~%  ~
      \\hline~%  ~
      & {\\bf  total} ~
      & \\multicolumn{2}{|c|}{\\bf without filter} ~
      & \\multicolumn{2}{|c|}{\\bf with filter} ~
      & \\multicolumn{2}{|c|}{\\bf improvement}\\\\~% ~
      {\\bf Phenomenon} & {\\bf items} ~
        & {\\bf unifications} & {\\bf tasks}~%    ~
        & {\\bf unifications} & {\\bf tasks} ~
        & {\\bf unifications} & {\\bf tasks}\\\\~%  ~
      & $\\sharp$ & $\\propto$ & $\\propto$ & $\\propto$ & $\\propto$~%    ~
        & $\\%$ & $\\%$\\\\~%  ~
      \\hline~%  ~
      \\hline~%")
    (do ((oitems oitems (rest oitems))
         (nitems nitems (rest nitems)))
        ((null oitems))
      (let* ((odata (rest (first oitems)))
             (ototal (length odata))
             (ounifications (map 'list #'(lambda (foo)
                                           (get-field :unifications foo))
                                 odata))
             (ouaverage (average ounifications))
             (otasks (map 'list #'(lambda (foo)
                                    (get-field :p-etasks foo))
                          odata))
             (otaverage (average otasks))
             (otime (map 'list #'(lambda (foo)
                                   (get-field :total foo))
                         odata))
             (otimeaverage (average otime))
             (ndata (rest (first nitems)))
             (nunifications (map 'list #'(lambda (foo)
                                           (get-field :unifications foo))
                                 ndata))
             (nuaverage (average nunifications))
             (ntasks (map 'list #'(lambda (foo)
                                    (get-field :p-etasks foo))
                          ndata))
             (ntaverage (average ntasks))
             (ntime (map 'list #'(lambda (foo)
                                   (get-field :total foo))
                         ndata))
             (ntimeaverage (average ntime)))
        (format
         *tsdb-io*
         "~a & ~d & ~d & ~d & ~d & ~d & ~,1f & ~,1f\\\\~%"
         (first (first oitems))
         ototal
         (round ouaverage) (round otaverage)
         (round nuaverage) (round ntaverage)
         (* (- 1 (/ nuaverage ouaverage)) 100)
         (* (- 1 (/ ntaverage otaverage)) 100))
        (setf total (+ total ototal))
        (setf outotal (+ outotal (reduce #'+ ounifications)))
        (setf ottotal (+ ottotal (reduce #'+ otasks)))
        (setf nutotal (+ nutotal (reduce #'+ nunifications)))
        (setf nttotal (+ nttotal (reduce #'+ ntasks)))))
    (format *tsdb-io* "\\hline~%\\hline~%")
    (format
     *tsdb-io*
     "{\\bf ~a} & {\\bf ~d} & {\\bf ~d} & {\\bf ~d} ~
      & {\\bf ~d} & {\\bf ~d} & {\\bf ~,1f} & {\\bf ~,1f}\\\\~%"
     "Total"
     total
     (round (/ outotal total))
     (round (/ ottotal total))
     (round (/ nutotal total))
     (round (/ nttotal total))
     (* (- 1 (/ nutotal outotal)) 100)
     (* (- 1 (/ nttotal ottotal)) 100))
    (format *tsdb-io* "  \\hline~%\\end{tabular}")
    (list oitems nitems)))

(defun analyze-performance-time-space (&optional (olanguage *tsdb-data*)
                                                 (nlanguage *tsdb-data*))
  (let ((oitems (if (stringp olanguage) nil olanguage))
        (nitems (if (stringp nlanguage) nil nlanguage))
        (total 0)
        (ostotal 0)
        (ottotal 0)
        (nstotal 0)
        (nttotal 0))
    (unless oitems
      (dolist (phenomenon (reverse *english-phenomena*))
        (let* ((condition (format nil "p-name ~~ \"^~a\"" phenomenon))
               (data (analyze condition olanguage)))
          (push (cons (intern (string-upcase phenomenon) "KEYWORD") data)
                oitems))))
    (unless nitems
      (dolist (phenomenon (reverse *english-phenomena*))
        (let* ((condition (format nil "p-name ~~ \"^~a\"" phenomenon))
               (data (analyze condition nlanguage)))
          (push (cons (intern (string-upcase phenomenon) "KEYWORD") data)
                nitems))))
    (format
     *tsdb-io*
     "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|@{}}~%  ~
      \\hline~%  ~
      & {\\bf  total} ~
      & \\multicolumn{2}{|c|}{\\bf old} ~
      & \\multicolumn{2}{|c|}{\\bf new ~
      & \\multicolumn{2}{|c|}{\\bf improvement}\\\\~% ~
      {\\bf Phenomenon} & {\\bf items} ~
        & {\\bf time} & {\\bf space}~%    ~
        & {\\bf time} & {\\bf space} ~
        & {\\bf time} & {\\bf space}\\\\~%  ~
      & $\\sharp$ & $\\propto$ & $\\propto$ & $\\propto$ & $\\propto$~%    ~
        & $\\%$ & $\\%$\\\\~%  ~
      \\hline~%  ~
      \\hline~%")
    (do ((oitems oitems (rest oitems))
         (nitems nitems (rest nitems)))
        ((null oitems))
      (let* ((odata (rest (rest (first oitems))))
             (ototal (length odata))
             (ospace (map 'list #'(lambda (foo)
                                    (+ (* 8 (get-field :conses foo))
                                       (* 24 (get-field :symbols foo))
                                       (get-field :others foo)))
                          odata))
             (osaverage (average ospace))
             (otime (map 'list #'(lambda (foo)
                                   (get-field :total foo))
                         odata))
             (otimeaverage (average otime))
             (ndata (rest (rest (first nitems))))
             (nspace (map 'list #'(lambda (foo)
                                    (+ (* 8 (get-field :conses foo))
                                       (* 24 (get-field :symbols foo))
                                       (get-field :others foo)))
                          ndata))
             (nsaverage (average nspace))
             (ntime (map 'list #'(lambda (foo)
                                   (get-field :total foo))
                         ndata))
             (ntimeaverage (average ntime)))
        (format
         *tsdb-io*
         "~a & ~d & ~d & ~d & ~d & ~d & ~,1f & ~,1f\\\\~%"
         (second (first oitems))
         ototal
         (round otimeaverage) (round osaverage)
         (round ntimeaverage) (round nsaverage)
         (* (- 1 (/ ntimeaverage otimeaverage)) 100)
         (* (- 1 (/ nsaverage osaverage)) 100))
        (setf total (+ total ototal))
        (setf ottotal (+ ottotal (reduce #'+ otime)))
        (setf ostotal (+ ostotal (reduce #'+ ospace)))
        (setf nttotal (+ nttotal (reduce #'+ ntime)))
        (setf nstotal (+ nstotal (reduce #'+ nspace)))))
    (format *tsdb-io* "\\hline~%\\hline~%")
    (format
     *tsdb-io*
     "{\\bf ~a} & {\\bf ~d} & {\\bf ~d} & {\\bf ~d} ~
      & {\\bf ~d} & {\\bf ~d} & {\\bf ~,1f} & {\\bf ~,1f}\\\\~%"
     "Total"
     total
     (round (/ ottotal total))
     (round (/ ostotal total))
     (round (/ nttotal total))
     (round (/ nstotal total))
     (* (- 1 (/ nttotal ottotal)) 100)
     (* (- 1 (/ nstotal ostotal)) 100))
    (format *tsdb-io* "  \\hline~%\\end{tabular}")
    (list oitems nitems)))

(defun graph-words (data &key (stream *tsdb-io*))
  (let* ((mwords 
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
        (format stream "\\endpicture~%")))))

(defun plot-words-first (data &key (stream *tsdb-io*))
  (let* ((mwords 
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
      (format stream "\\endpicture~%"))))

(defun plot-words-total (data &key (stream *tsdb-io*)
                                   (threshold 1))
  (let* ((mwords 
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
      (format stream "\\endpicture~%"))))

(defun plot-words-etasks-stasks-ftasks (data &key (stream *tsdb-io*)
                                                  (threshold 1))
  (let* ((mwords 
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
      (format stream "\\endpicture~%"))))
