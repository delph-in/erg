(in-package "MRS")

(setf *value-feats* `(                        ,(vsym "YEAR") 
                                              ,(vsym "SEASON") 
                                              ,(vsym "MONTH") 
                                              ,(vsym "DAY") 
                                              ,(vsym "HOUR") 
                                               ,(vsym "MINUTE")        
                                               ;; ,(vsym "ORD") 
                                               ,(vsym "CONST_VALUE") 
                                               ,(vsym "NAMED") 
                                               ,(vsym "TITLE") 
                                               ,(vsym "EXCL")))



(setf *feat-priority-list*  
  `( ,(vsym "TOP") ,(vsym "HANDEL") ,(vsym "INDEX") ,(vsym "EVENT") 
     ,(vsym "INST") ,(vsym "ACT") ,(vsym "BV") ,(vsym "RESTR") 
     ,(vsym "SOA") ,(vsym "SCOPE") ,(vsym "QUANT") ,(vsym "XARG") 
     ,(vsym "ARG") ,(vsym "PREP") ,(vsym "ARGX") ,(vsym "CONST_VALUE")))
                                 
(setf *ignored-sem-features* `(,(vsym "PARAMS") ,(vsym "LABEL")))

(setf *ignored-extra-features* 
  (cons (vsym "SORT") *ignored-extra-features*))

(setf *top-level-rel-types* 
  `(,(vsym "pron_rel") ,(vsym "refl_pro_rel")
    ,(vsym "mofy_rel") ,(vsym "the_afternoon_rel")
    ,(vsym "the_morning_rel") ,(vsym "the_evening_rel")
    ,(vsym "numbered_hour_rel") ,(vsym "minute_rel") ,(vsym "dofw_rel")
    ,(vsym "holiday_rel")
    ,(vsym "ctime_rel") ,(vsym "_hour_rel") ,(vsym "_minute_rel") 
    ,(vsym "dim_rel") ,(vsym "unspec_rel") ,(vsym "recip_pro_rel") 
    ,(vsym "_the_day_after_rel") ,(vsym "dofm_rel")
    ,(vsym "_abroad_rel") ,(vsym "_afterward_rel") 
    ,(vsym "_afterwards_rel") ,(vsym "_ahead_rel") 
    ,(vsym "_all_day_rel") ,(vsym "_anytime_rel") 
    ,(vsym "_as_soon_as_possible_rel") ,(vsym "_aside_rel")
    ,(vsym "_astray_rel") ,(vsym "_away_rel") 
    ,(vsym "_back_adv_rel") ,(vsym "_backward_rel") 
    ,(vsym "_backwards_rel") ,(vsym "_beforehand_rel") 
    ,(vsym "_forth_rel") ,(vsym "_forward_rel") 
    ,(vsym "_forwards_rel") ,(vsym "_here_rel") 
    ,(vsym "_hither_rel") ,(vsym "_home_loc_rel") 
    ,(vsym "_last_time_rel") ,(vsym "_maximum_adv_rel") 
    ,(vsym "_nearby_rel") ,(vsym "_now_rel") 
    ,(vsym "_right_away_rel") 
    ,(vsym "_right_now_rel") ,(vsym "_sometime_rel") 
    ,(vsym "_somewhere_rel") ,(vsym "_then_temp_rel") 
    ,(vsym "_there_rel") ,(vsym "_thereabouts_rel") 
    ,(vsym "_upstairs_rel")
    ,(vsym "_and_rel") ,(vsym "_pm_rel") ,(vsym "_am_rel")
    ))

;;; features for extracting semantics from expanded lexical entries

(setf *dummy-relations* `(,(vsym "NO_REL") ,(vsym "MESSAGE")))

(setf *main-semantics-path* 
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT") 
    ,(vsym "LISZT") ,(vsym "LIST")))

(setf *construction-semantics-path*
  `(,(vsym "C-CONT") ,(vsym "LISZT") ,(vsym "LIST")))

(setf *top-semantics-type* 
  (vsym "RELATION"))

(setf *root-path* `(,(vsym "ROOT")))

(setf *false-type* (vsym "-"))

(setf *true-type* (vsym "+"))



