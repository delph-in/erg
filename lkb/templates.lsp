#|
satp ($head=head) := synsem &
  [ LOCAL local &
	  [ CONJ cnil,
	    CAT [ HEAD $head,
		  VALENCE [ SUBJ < >,
			    COMPS *olist*,
			    SPR *olist* ] ] ] ].

|#

(make-tdl-template 'satp '((head (local cat head)))
                   '((() synsem)
                     ((local) local)
                     ((local conj) cnil)
                     ((local cat head) head)
                     ((local cat valence subj) *null*)
                     ((local cat valence comps) *olist*)
                     ((local cat valence spr) *olist*)))

#|

cp ($vform=vform) := synsem & @satp($head=verbal) &		     
  [ LOCAL.CAT [ HEAD [ VFORM $vform,
		       MOOD ind_or_mod_subj,
		       INV -  ],
	          ROOT bool ] ].


|#

(make-tdl-template 'cp '((head (local cat head))
                         (vform (local cat head vform)))
                   '((() synsem)
                     ((local) local)
                     ((local conj) cnil)
                     ((local cat head) verbal)
                     ((local cat valence subj) *null*)
                     ((local cat valence comps) *olist*)
                     ((local cat valence spr) *olist*)
                     ((local cat head vform) vform)
                     ((local cat head mood) ind_or_mod_subj)
                     ((local cat head inv) -)
                     ((local cat root) bool)))

#|

nomp ($case=case,$cont=mrs) := @satp($head=nominal) &
  [ LOCAL [ CAT [ HEAD strict_head &
		       [ CASE $case ],
		  ROOT na ],
	    CONT $cont ] ].


|#

(make-tdl-template 'nomp '((head (local cat head))
                           (case (local cat head case))
                           (cont (local cont)))
                   '(((local) local)
                     ((local conj) cnil)
                     ((local cat head) strict_nominal)
                     ((local cat valence subj) *null*)
                     ((local cat valence comps) *olist*)
                     ((local cat valence spr) *olist*)
                     ((local cat head case) case)
                     ((local cat root) na)
                     ((local cont) mrs)))
#|

np ($case=case,$cont=mrs) := @satp($head=supnoun) &
  [ LOCAL [ CAT.HEAD strict_head &
		     [ CASE $case ],
	    CONT $cont ] ].

|#

(make-tdl-template 'np '((head (local cat head))
                           (case (local cat head case))
                           (cont (local cont)))
                   '(((local cat head case) case)
                     ((local cont) mrs)
                     ((local cat head) strict_supnoun)
                     ((local conj) cnil)
                     ((local cat valence subj) *null*)
                     ((local cat valence comps) *null*)
                     ((local cat valence spr) *null*)))

#|

; Only used as MOD value for relative clauses, so type LOCAL, not SYNSEM
nbar ($cont=nom-obj) := local &
  [ CAT [ HEAD noun &
	       [ MOD no-mod ],
	  VALENCE [ SUBJ < >,
		    COMPS *olist*,
		    SPR < synsem > ]],
    CONT $cont,
    ROOT na ].


|#

(make-tdl-template 'nbar '((cont (cont)))
                   '((() local)
                     ((cont) nom-obj)
                     ((cat head) noun)
                     ((cat head mod) no-mod)
                     ((cat valence subj) *null*)
                     ((cat valence comps) *olist*)
                     ((cat valence spr first) synsem)
                     ((cat valence spr rest) *null*)
                     ((cat root) na)))

#|
third-sg-np ( ) := @nomp ( ) & 
  [ LOCAL.CONT.INDEX.PNG 3sg ].


|#

(make-tdl-template 'third-sg-np '((head (local cat head))
                           (case (local cat head case))
                           (cont (local cont)))
                   '(((local) local)
                     ((local conj) cnil)
                     ((local cat head) strict_nominal)
                     ((local cat valence subj) *null*)
                     ((local cat valence comps) *olist*)
                     ((local cat valence spr) *olist*)
                     ((local cat head case) case)
                     ((local cat root) na)
                     ((local cont) mrs)
                     ((local cont index png) 3sg)))


#|
prd ($subj=synsem) :=
  [ LOCAL local &
	  [ CAT [ HEAD [ PRD + ],
		  VALENCE [ SUBJ < $subj >,
			    SPR *olist*,
			    COMPS *olist* ],
		  ROOT na ] ] ].

|#

(make-tdl-template 'prd '((subj (local cat valence subj)))
                   '(((local cat valence subj first) synsem)
                     ((local) local)
                     ((local cat head prd) +)
                     ((local cat valence spr) *olist*)
                     ((local cat valence comps) *olist*)
                     ((local cat root) na)))

#|

vp ($vform=vform) :=
  [ LOCAL local &
	  [ CONJ cnil,
	    CAT [ HEAD verbal & strict_head &
		     [ VFORM $vform,
		       INV - ],
		  VALENCE [ SUBJ < synsem >,
			    COMPS *olist* ] ] ] ].


|#

(make-tdl-template 'vp '((vform (local cat head vform)))
                   '(((local cat head vform) vform)
                     ((local) local)
                     ((local conj) cnil)
                     ((local cat head) verb_or_comp)
                     ((local cat head inv) -)
                     ((local cat valence subj first) synsem)
                     ((local cat valence comps) *olist*)))
#|

pp () := synsem &
  [ LOCAL local &
	  [ CAT [ HEAD prep & [ PRD - ],
		  VALENCE [ SPR *olist*,
			    COMPS *olist* ],
		  ROOT na ] ] ].

|#

(make-tdl-template 'pp nil
                   '((() synsem)
                     ((local) local)
                     ((local cat head) prep)
                     ((local cat head prd) -)
                     ((local cat valence spr) *olist*)
                     ((local cat valence comps) *olist*)
                     ((local cat root) na)))

nil
