;;;
;;; this file should be `Index.lisp' and reside in the directory containing the
;;; tsdb(1) test suite skeleton databases (typically a subdirectory `skeletons'
;;; in the tsdb(1) database root directory `*tsdb-home*').
;;;
;;; the file should contain a single un-quote()d Common-Lisp list enumerating
;;; the available skeletons, e.g.
;;;
;;;   (((:path . "english") (:content . "English TSNLP test suite"))
;;;    ((:path . "csli") (:content . "CSLI (ERGO) test suite"))
;;;    ((:path . "vm") (:content . "English VerbMobil data")))
;;;
;;; where the individual entries are assoc() lists with at least two elements:
;;;
;;;   - :path --- the (relative) directory name containing the skeleton;
;;;   - :content --- a descriptive comment.
;;;
;;; the order of entries is irrelevant as the `tsdb :skeletons' command sorts
;;; the list lexicographically before output.
;;;

;;;
;;; to make MaxEnt experimentation across data sets (and generally reference) a
;;; little easier, item identifiers need to be globally unique.  to accomodate
;;; existing data sets in this scheme, dan and i came up with a break-down of
;;; identifiers as follows
;;;
;;; [                1 --             1,999] --- various test suites
;;; [            2,000 --             9,999] --- The Cathedral and the Bazaar
;;; [           10,000 --            29,999] --- Conan Doyle Corpus (CDC)
;;; [          102,000 --           116,000] --- Le Petit Prince (LPP)
;;; [        1,000,000 --         1,999,999] --- VerbMobil
;;; [        2,000,000 --         2,999,999] --- ecommerce
;;; [        3,000,000 --         3,999,999] --- LOGON
;;; [        4,000,000 --         4,999,999] --- SciBorg
;;; [        5,000,000 --         5,099,999] --- BioScope
;;; [       10,000,000 --        19,999,999] --- WeScience
;;; [       20,000,000 --        29,999,999] --- PTB WSJ
;;; [       30,000,000 --        30,999,999] --- Hinoki
;;; [       40,000,000 --        49,999,999] --- Brown Corpus
;;; [      120,000,000 --     1,099,999,999] --- GENIA
;;; [  100,000,000,000 --   299,999,999,999] --- WeSearch Data Collection
;;; [1,000,010,000,000 -- 1,128,928,700,510] --- WikiWoods
;;; 
;;; _fix_me_
;;; some of the old skeletons (and existing treebanks, where applicable) still
;;; need to be converted.  this will require a re-numbering script and close
;;; synchronization with dan.                                   (8-nov-08; oe)
;;;
(((:path . "verbmobil") (:content . "Scheduling Dialogues (VerbMobil)"))
 ((:path . "yy") (:content . "ECommerce Email (YY)"))
 ((:path . "logon") (:content . "Tourism Brochures (LOGON)"))
 #+:null
 ((:path . "handon") (:content . "Tourism Web Sites (HandOn)"))
 ((:path . "wescience") (:content . "Scholarly Literature (WeScience)"))
 ((:path . "wdc") (:content . "WeSearch Data Collection"))
 ((:path . "brown") (:content . "(Parts of) Brown Corpus"))
 ((:path . "wsj") (:content . "Wall Street Journal"))
 ((:path . "ptb") (:content . "Penn Treebank (WSJ)"))
 ((:path . "semcor") (:content . "SemCor Sampler (via Melbourne)"))
 ((:path . "genia") (:content . "GENIA Treebank"))
 #+:null
 ((:path . "bioscope") (:content . "Bio-Medical Texts (BioScope)"))
 #+:null
 ((:path . "starsem") (:content . "2012 *SEM Shared Task on Negation"))
 ((:path . "english") (:content . "TSNLP English Test Suite"))
 ((:path . "csli") (:content . "CSLI (HP) Test Suite"))
 ((:path . "mrs") (:content . "DELPH-IN MRS Test Suite"))
 ((:path . "esd") (:content . "ERG Semantic Documentation Test Suite"))
 ((:path . "cb") (:content . "The Cathedral and the Bazaar"))
 ((:path . "trec") (:content . "TREC QA Questions (Ninth Conference)"))
 ((:path . "fracas") (:content . "FraCaS Semantics Test Suite"))
 ((:path . "peted") (:content . "Evaluation By Textual Entailment (Development)"))
 ((:path . "petet") (:content . "Evaluation By Textual Entailment (Test)"))
 #+:null
 ((:path . "tanaka") (:content . "Tanaka Corpus (English Translations)"))
 ((:path . "lpp") (:content . "Le Petit Prince"))
 ((:path . "redwoods") (:content . "Core Redwoods (JHPSTG & WeScience)")))
