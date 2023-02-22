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
;;; [        6,000,000 --         6,099,999] --- SemCore
;;; [        7,000,000 --         7,099,999] --- Open Multilingual WordNet
;;; [       10,000,000 --        19,999,999] --- WeScience
;;; [       20,000,000 --        29,999,999] --- PTB WSJ
;;; [       30,000,000 --        30,999,999] --- Hinoki
;;; [       40,000,000 --        49,999,999] --- Brown Corpus
;;; [       60,000,000 --        69,999,999] --- Sherlock Holmes
;;; [       70,000,000 --        79,999,999] --- Open English WordNet
;;; [      120,000,000 --     1,099,999,999] --- GENIA
;;; [    2,000,000,000 --     2,999,999,999] --- NTU CLE
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
 ((:path . "tanaka") (:content . "Tanaka Corpus (English Translations)"))
 ((:path . "lpp") (:content . "Le Petit Prince"))
 ((:path . "bcs") (:content . "Brown Corpus Sampler (SDP 2015 Task)"))
 ((:path . "scm") (:content . "SemCor Melbourne Sampler (Disjoint from BCS)"))
 ((:path . "wrev1819") (:content . "Writing Reviews 10/18-01/19"))
 ((:path . "redwoods") (:content . "Core Redwoods (JHPSTG & WeScience)"))
 ((:path . "educ") (:content . "Educational Composition"))
 ((:path . "repsol") (:content . "Alexandre Rademaker mining data"))
 ((:path . "ccs") (:content . "Collab Compuational Semantics"))
 ((:path . "control") (:content . "Control examples from literature"))
 ((:path . "handp12") (:content . "Huddleston and Pullum Ch.12"))
 ((:path . "sh-spec") (:content . "Sherlock Holmes Speckled Band"))
 ((:path . "sh-danc") (:content . "Sherlock Holmes Dancing Man"))
 ((:path . "ntucle") (:content . "NTU CLE learner English corpus"))
 ((:path . "omw") (:content . "Open Multilingual WordNet sample definitions"))
 ((:path . "marco300") (:content . "First set of 300 Marco user queries"))
 ((:path . "marco2nd300") (:content . "Second 300 Marco user queries"))
 ((:path . "marco3rd300") (:content . "Third 300 Marco user queries"))
 ((:path . "marcodrdev") (:content . "MSMarco Document Rank dev queries"))
 ((:path . "marcodrtrain") (:content . "MSMarco Document Rank train queries"))
 ((:path . "marcotrainwk") (:content . "MSMarco DR train queries in Wikipedia"))
 ((:path . "singlish") (:content . "Singlish examples"))
 ((:path . "ersa") (:content . "ERS Algebra examples"))
 ((:path . "test") (:content . "Test"))
)
