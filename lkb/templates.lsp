#|
satp ($head=head) := synsem &
  [ LOCAL local_basic &
	  [ CONJ cnil,
	    CAT [ HEAD $head,
		  VAL [ SUBJ < >,
			    COMPS *olist*,
			    SPR *olist* ] ] ] ].

|#

(make-tdl-template 'satp '((head (local cat head)))
                   '((() synsem)
                     ((local) local_basic)
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
                     ((local) local_basic)
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

; DPF (28-Jul-98) Added MOD < > to exclude non-finite relative clauses, which
; are otherwise not distinguishable from nomp's.  Maybe also needed to exclude 
; the second (modifier) entries for temporal NPs like "Tuesday".

nomp ($cont=mrs) := @satp($head=nominal) &
  [ LOCAL [ CAT [ HEAD strict_type &
                       [ MOD < > ],
		  MC na ],
	    CONT $cont ] ].


|#

(make-tdl-template 'nomp '((head (local cat head))
                           (cont (local cont)))
                   '(((local) local_basic)
                     ((local conj) cnil)
                     ((local cat head) strict_nominal)
                     ((local cat head mod) *null*)
                     ((local cat val subj) *null*)
                     ((local cat val comps) *olist*)
                     ((local cat val spr) *olist*)
                     ((local cat mc) na)
                     ((local cont) mrs_min)))

; DPF (8-Jan-99) Added accusative and nominative subtypes of nomp, since 
; nominative ones are not 'mobile' - see fundamentals.tdl

#|
nomp_acc ($cont=mrs) := nomp &
  [ LOCAL [ CAT.HEAD noun_acc,
	    CONT $cont ] ].
|#

(make-tdl-template 'nomp_acc '((head (local cat head))
                           (case (local cat head case))
                           (cont (local cont)))
                   '(((local) local_basic)
                     ((local conj) cnil)
                     ((local cat head) strict_nominal)
                     ((local cat val subj) *null*)
                     ((local cat val comps) *olist*)
                     ((local cat val spr) *olist*)
                     ((local cat head case) acc)
                     ((local cat mc) na)
                     ((local cont) mrs_min)))

#|
nomp_nom ($cont=mrs) := nomp &
  [ LOCAL [ CAT.HEAD noun_nom,
	    CONT $cont ] ].
|#

(make-tdl-template 'nomp_nom '((head (local cat head))
                           (case (local cat head case))
                           (cont (local cont)))
                   '(((local) local_basic)
                     ((local conj) cnil)
                     ((local cat head) strict_nominal)
                     ((local cat val subj) *null*)
                     ((local cat val comps) *olist*)
                     ((local cat val spr) *olist*)
                     ((local cat head case) nom)
                     ((local cat mc) na)
                     ((local cont) mrs_min)))


#|
third-sg-np ( ) := @nomp ( ) & 
  [ LOCAL.CONT.INDEX.PNG 3sg ].


|#

(make-tdl-template 'third-sg-np '((head (local cat head))
                           (case (local cat head case))
                           (cont (local cont)))
                   '(((local) local_basic)
                     ((local conj) cnil)
                     ((local cat head) strict_nominal)
                     ((local cat val subj) *null*)
                     ((local cat val comps) *olist*)
                     ((local cat val spr) *olist*)
                     ((local cat head case) case)
                     ((local cat mc) na)
                     ((local cont) mrs_min)
                     ((local cont index png) 3sg)))


#|
prd ($subj=synsem) :=
  [ LOCAL local_basic &
	  [ CAT [ HEAD [ PRD + ],
		  VAL [ SUBJ < $subj >,
			    SPR *olist*,
			    COMPS *olist* ],
		  MC na ] ] ].

|#

(make-tdl-template 'prd '((subj (local cat val subj)))
                   '(((local cat val subj first) synsem)
                     ((local) local_basic)
                     ((local cat head prd) +)
                     ((local cat val spr) *olist*)
                     ((local cat val comps) *olist*)
                     ((local cat mc) na)))

#|

vp ($vform=vform) :=
  [ LOCAL local_basic &
	  [ CONJ cnil,
	    CAT [ HEAD verbal & strict_type &
		     [ VFORM $vform,
		       INV - ],
		  VAL [ SUBJ < synsem >,
			    COMPS *olist* ] ] ] ].


|#

(make-tdl-template 'vp '((vform (local cat head vform)))
                   '(((local cat head vform) vform)
                     ((local) local_basic)
                     ((local conj) cnil)
                     ((local cat head) verb_or_comp)
                     ((local cat head inv) -)
                     ((local cat val subj first) synsem)
                     ((local cat val comps) *olist*)))
#|

pp () := synsem &
  [ LOCAL local_basic &
	  [ CAT [ HEAD prep & [ PRD - ],
		  VAL [ SPR *olist*,
			    COMPS *olist* ],
		  MC na ] ] ].

|#

(make-tdl-template 'pp nil
                   '((() synsem)
                     ((local) local_basic)
                     ((local cat head) prep)
                     ((local cat head prd) -)
                     ((local cat val spr) *olist*)
                     ((local cat val comps) *olist*)
                     ((local cat mc) na)))

nil
