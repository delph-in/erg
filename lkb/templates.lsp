#|
satp ($head=head) := synsem &
  [ LOCAL local &
	  [ CONJ cnil,
	    CAT [ HEAD $head,
		  VAL [ SUBJ < >,
			    COMPS *olist*,
			    SPR *olist* ] ] ] ].

|#

(make-tdl-template 'satp '((head (local cat head)))
                   '((() synsem)
                     ((local) local)
                     ((local conj) cnil)
                     ((local cat head) head)
                     ((local cat val subj) *null*)
                     ((local cat val comps) *olist*)
                     ((local cat val spr) *olist*)))

#|

cp ($vform=vform) := synsem & @satp($head=verbal) &		     
  [ LOCAL.CAT [ HEAD [ VFORM $vform,
		       MOOD ind_or_mod_subj,
		       INV -  ],
	          MC bool ] ].


|#

(make-tdl-template 'cp '((head (local cat head))
                         (vform (local cat head vform)))
                   '((() synsem)
                     ((local) local)
                     ((local conj) cnil)
                     ((local cat head) verbal)
                     ((local cat val subj) *null*)
                     ((local cat val comps) *olist*)
                     ((local cat val spr) *olist*)
                     ((local cat head vform) vform)
                     ((local cat head mood) ind_or_mod_subj)
                     ((local cat head inv) -)
                     ((local cat mc) -)))

#|

nomp ($case=case,$cont=mrs) := @satp($head=nominal) &
  [ LOCAL [ CAT [ HEAD strict_head &
		       [ CASE $case ],
		  MC na ],
	    CONT $cont ] ].


|#

(make-tdl-template 'nomp '((head (local cat head))
                           (case (local cat head case))
                           (cont (local cont)))
                   '(((local) local)
                     ((local conj) cnil)
                     ((local cat head) strict_nominal)
                     ((local cat val subj) *null*)
                     ((local cat val comps) *olist*)
                     ((local cat val spr) *olist*)
                     ((local cat head case) case)
                     ((local cat mc) na)
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
                     ((local cat val subj) *null*)
                     ((local cat val comps) *null*)
                     ((local cat val spr) *null*)))

#|

; Only used as MOD value for relative clauses, so type LOCAL, not SYNSEM
nbar ($cont=nom-obj) := local &
  [ CAT [ HEAD noun &
	       [ MOD no-mod ],
	  VAL [ SUBJ < >,
		    COMPS *olist*,
		    SPR < synsem > ]],
    CONT $cont,
    MC na ].


|#

(make-tdl-template 'nbar '((cont (cont)))
                   '((() local)
                     ((cont) nom-obj)
                     ((cat head) noun)
                     ((cat head mod) no-mod)
                     ((cat val subj) *null*)
                     ((cat val comps) *olist*)
                     ((cat val spr first) synsem)
                     ((cat val spr rest) *null*)
                     ((cat mc) na)))

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
                     ((local cat val subj) *null*)
                     ((local cat val comps) *olist*)
                     ((local cat val spr) *olist*)
                     ((local cat head case) case)
                     ((local cat mc) na)
                     ((local cont) mrs)
                     ((local cont index png) 3sg)))


#|
prd ($subj=synsem) :=
  [ LOCAL local &
	  [ CAT [ HEAD [ PRD + ],
		  VAL [ SUBJ < $subj >,
			    SPR *olist*,
			    COMPS *olist* ],
		  MC na ] ] ].

|#

(make-tdl-template 'prd '((subj (local cat val subj)))
                   '(((local cat val subj first) synsem)
                     ((local) local)
                     ((local cat head prd) +)
                     ((local cat val spr) *olist*)
                     ((local cat val comps) *olist*)
                     ((local cat mc) na)))

#|

vp ($vform=vform) :=
  [ LOCAL local &
	  [ CONJ cnil,
	    CAT [ HEAD verbal & strict_head &
		     [ VFORM $vform,
		       INV - ],
		  VAL [ SUBJ < synsem >,
			    COMPS *olist* ] ] ] ].


|#

(make-tdl-template 'vp '((vform (local cat head vform)))
                   '(((local cat head vform) vform)
                     ((local) local)
                     ((local conj) cnil)
                     ((local cat head) verb_or_comp)
                     ((local cat head inv) -)
                     ((local cat val subj first) synsem)
                     ((local cat val comps) *olist*)))
#|

pp () := synsem &
  [ LOCAL local &
	  [ CAT [ HEAD prep & [ PRD - ],
		  VAL [ SPR *olist*,
			    COMPS *olist* ],
		  MC na ] ] ].

|#

(make-tdl-template 'pp nil
                   '((() synsem)
                     ((local) local)
                     ((local cat head) prep)
                     ((local cat head prd) -)
                     ((local cat val spr) *olist*)
                     ((local cat val comps) *olist*)
                     ((local cat mc) na)))

nil
