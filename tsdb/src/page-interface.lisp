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
  *tsdb-morphology-protocol*
  '((main::*scanner* main::*morphology* :start-fw)
    ((main::*morphology* :parse) main::*standalone* main::*scanner*)))

(defparameter
  *tsdb-parser-protocol*
  '((main::*scanner* main::*morphology* :start-fw)
    ((main::*morphology* :parse) main::*lexicon* main::*scanner*)
    (main::*lexicon* main::*parser* main::*morphology*)
    (main::*parser* main::*standalone* main::*lexicon*)))

(defparameter *page-controller* main::*paradice*)    

(defmacro current-parser ()
  (if (equal (elt make::*page-version* 0) #\2)
    '(pg::get-parser :syntax)
    'pg::*global-parser*))

(defun run-protocol (protocol input &key (trace nil))
  (let* ((foo (open "/dev/null" :direction :output :if-exists :overwrite))
         (main::*standard-disco-output* 
          (if trace main::*standard-disco-output* foo))
         printers streams)
    (unless trace
      (dolist (component (main::internal-protocol-rep protocol))
        (push (main::printer-status (main::p-caller component)) printers)
        (setf (main::printer-status (main::p-caller component)) nil)
        (push (stream (main::p-caller component)) streams)
        (setf (stream (main::p-caller component)) foo)))
    (multiple-value-bind (result condition)
        (ignore-errors
         (catch 'main::stop
           (main::eval-proto *page-controller* protocol input)))
      (close foo)
      (unless trace
        (do ((protocol (main::internal-protocol-rep protocol)
                       (rest protocol))
             (printers printers (rest printers))
             (streams streams (rest streams)))
            ((null protocol))
          (setf 
            (main::printer-status (main::p-caller (first protocol)))
            (first printers))
          (setf (stream (main::p-caller (first protocol))) (first streams))))
      (values result condition))))

(defun morphologically-analyze-word (word &key (trace nil))
  (setf (main::output-stream main::*lexicon*) nil)
  (multiple-value-bind (result condition)
      (run-protocol *tsdb-morphology-protocol* word :trace trace)
    (unless condition
      (main::output-stream main::*lexicon*))))

(defun size-of-type-hierarchy ()
  (let* ((avms (tdl::get-global :avms))
         (avms (when (hash-table-p avms) (hash-table-count avms)))
         (sorts (tdl::get-global :sorts))
         (sorts (when (hash-table-p sorts) (hash-table-count sorts)))
         (templates (tdl::get-global :templates))
         (templates
          (when (hash-table-p templates) (hash-table-count templates))))
    (append (and avms (list (cons :avms avms)))
            (and sorts (list (cons :sorts sorts)))
            (and templates (list (cons :templates templates))))))

(defmacro get-item-type (item)
  `(get-fs-type (lex::cfs-fs (pg::combo-item-cfs ,item))))

(defun remove-terminals (derivation)
  (let ((daughters (pg::pnode-daughters derivation)))
    (if daughters
      (append (list (pg::pnode-name derivation) 
                    (pg::pnode-start derivation)
                    (pg::pnode-end derivation))
              (remove nil (map 'list #'remove-terminals daughters)))
      nil)))
       
(defun extract-preterminals (derivation &optional (offset 0))
  (let ((daughters (pg::pnode-daughters derivation)))
    (if daughters
      (let* ((offset (+ offset (pg::pnode-start derivation))))
        (mapcan #'(lambda (daughter)
                    (extract-preterminals daughter offset))
                daughters))
      (list (list (pg::pnode-name derivation) 
                  (+ offset (pg::pnode-start derivation))
                  (+ offset (pg::pnode-end derivation)))))))

(defparameter *lexical-oracle* nil)

(defun install-derivation-oracle (derivations)
  (setf *lexical-oracle* nil)
  (when derivations
    (let* ((derivations (map 'list #'remove-terminals derivations))
           (preterminals (mapcan #'extract-preterminals derivations)))
      (setf *lexical-oracle* 
        (make-array (+ 1 (apply #'max (map 'list 
                                        #'pg::pnode-start preterminals)))))
      (map nil 
        #'(lambda (preterminal) 
            (push (intern (string-upcase (pg::pnode-name preterminal)) 
                          lex::*lex-package*)
                  (aref *lexical-oracle* (pg::pnode-start preterminal))))
        preterminals))
    (setf (pg::parser-task-priority-fn (pg::get-parser :lexicon))
      #'lexical-priority-oracle)))


(defun lexical-priority-oracle (rule daughter tasktype parser)
  (declare (ignore parser))
  #|
  (format t "~a: ~a [~a ~a] # ~a [~a ~a]~%" 
          tasktype 
          (when rule (get-item-type rule))
          (when rule (pg::item-start rule))
          (when rule (pg::item-end rule))
          (when daughter (get-item-type daughter))
          (when daughter (pg::item-start daughter))
          (when daughter (pg::item-end daughter)))
  |#
  (if (and *lexical-oracle*
           rule daughter
           (eq tasktype :cf-rule)
           (eq (pg::combo-item-itype rule) :lex-entry)
           (eq (pg::combo-item-itype daughter) :morph))
    (let ((start (pg::item-start daughter))
          (type (get-item-type rule)))
      (when (member type (aref *lexical-oracle* start)) 600))
    600))

(in-package "PARSING")

;;;
;;; this is code that started out in the CSLI `tuneup.lisp' a while or two
;;; ago (that night when people had had a bottle of champagne while admiring
;;; the half moon bay sunset |:-); since it is truly tsdb(1) specific, it seems
;;; plausible to move it here.                           (31-oct-97  -  oe)
;;;

(defun total-number-of-items (&optional (parser (pg::get-parser :syntax)))
  (let* ((chart (parser-chart parser))
         (passive-items (when chart (chart-passive-items chart)))
         (active-items-starting-at (when chart
                                     (chart-active-items-starting-at chart)))
         (active-items-ending-at (when chart
                                   (chart-active-items-ending-at chart)))
         (total 0))
    (when (and chart
             passive-items
             active-items-starting-at active-items-ending-at)
      (let ((n (array-dimension passive-items 0))
            (m (array-dimension passive-items 1)))
        (dotimes (i n)
          (dotimes (j m)
            (setf total (+ total (length (aref passive-items i j)))))))
      (let ((n (array-dimension active-items-starting-at 0)))
        (dotimes (i n)
          (setf total (+ total (length (aref active-items-starting-at i))))))
      (let ((n (array-dimension active-items-ending-at 0)))
        (dotimes (i n)
          (setf total (+ total (length (aref active-items-ending-at i)))))))
    total))

(defparameter *current-number-of-tasks* 0)
(defparameter *maximal-number-of-tasks* 0)
(defparameter *maximal-number-of-tasks-exceeded-p* nil)

(defun maximal-number-of-tasks-exceeded-p (parser)
  (setf *maximal-number-of-tasks-exceeded-p*
    (>= (incf *current-number-of-tasks*) *maximal-number-of-tasks*)))

