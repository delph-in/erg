;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: MAIN -*-

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

(in-package "MAIN")

(define-command
  '(:tsdb
    &nec query &optional language
    &doc "Forward QUERY to the test suite database (for LANGUAGE).")
  #'tsdb:tsdb)

(define-command
  '(:retrieve
    &optional condition language
    &doc "Retrieve TSDB test items (optionally matching CONDITION).")
  #'tsdb:retrieve)

(define-command
  '(:retrieve-and-process
    &optional condition run language comment
    &key verbose gc
    &doc 
    "Retrieve TSDB test items (optionally matching CONDITION), feed them into 
  the parser and store results in the `parse' relation (as RUN with COMMENT).")
  #'tsdb:retrieve-and-process)

(define-command
  '(:vocabulary
    &optional condition language load verbose
    &doc "Retrieve TSDB test suite vocabulary (optionally matching CONDITION);
  if LOAD is not null(), the necessary vocabulary is dynamically loaded;
  VERBOSE controls the tracing level (one of `:full', `:fair', or `:none').")
  #'tsdb:vocabulary)
