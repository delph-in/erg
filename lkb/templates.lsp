;;; AAC - templates from Uli, but with mrs replaced with mrs_min
;;; since this seems intentional, and updated comments


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
		       INV - ],
		MC - ] ].

|#

(make-tdl-template 'cp '((vform (local cat head vform)))
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

nomp ($cont=mrs) := @satp($head=nominal) &
  [ LOCAL [ CAT [ HEAD strict_type &
		       [ MOD < > ],
		  MC na ],
                  CONT $cont ] ].
                  
|#

(make-tdl-template 'nomp '((cont (local cont)))
                   '(((local) local_basic)
                     ((local conj) cnil)
                     ((local cat head) strict_type)
                     ((local cat head) nominal)
                     ((local cat head mod) *null*)
                     ((local cat val subj) *null*)
                     ((local cat val comps) *olist*)
                     ((local cat val spr) *olist*)
                     ((local cat mc) na)
                     ((local cont) mrs_min)))

#|
nomp_acc ($cont=mrs) := @nomp() &
  [ LOCAL [ CAT.HEAD mobile & [ CASE acc ],
	    CONT $cont ] ].
|#

(make-tdl-template 'nomp_acc '((cont (local cont)))
                   '(((local) local_basic)
                     ((local conj) cnil)
                     ((local cat head) mobile)
                     ((local cat head) strict_type)
                     ((local cat head) nominal)
                     ((local cat head case) acc)
                     ((local cat val subj) *null*)
                     ((local cat val comps) *olist*)
                     ((local cat val spr) *olist*)
                     ((local cat mc) na)
                     ((local cont) mrs_min)
                     ((local cat head mod) *null*)))

#|
nomp_nom ($cont=mrs) := @nomp() &
  [ LOCAL [ CAT.HEAD.CASE nom,
	    CONT $cont ] ].
|#

(make-tdl-template 'nomp_nom '((cont (local cont)))
                   '(((local) local_basic)
                     ((local conj) cnil)
                     ((local cat head) nominal)
                     ((local cat head) strict_type)
                     ((local cat val subj) *null*)
                     ((local cat val comps) *olist*)
                     ((local cat val spr) *olist*)
                     ((local cat head case) nom)
                     ((local cat mc) na)
                     ((local cont) mrs_min)
                     ((local cat head mod) *null*)))

#|

third-sg-np ( ) := @nomp ( ) & 
  [ LOCAL.CONT.INDEX.PNG 3sg ].

|#

(make-tdl-template 'third-sg-np nil
                   '(((local) local_basic)
                     ((local conj) cnil)
                     ((local cat head) strict_type)
                     ((local cat head) nominal)
                     ((local cat val subj) *null*)
                     ((local cat val comps) *olist*)
                     ((local cat val spr) *olist*)
                     ((local cat mc) na)
                     ((local cont) mrs_min)
                     ((local cont index png) 3sg)
                     ((local cat head mod) *null*)))


#|
prd ($subj=synsem) :=
  [ LOCAL local_basic &
	  [ CAT [ HEAD [ PRD + ],
		  VAL [ SUBJ < $subj >,
			    SPR *olist*,
			    COMPS *olist* ],
                  MC na ] ] ].
                            
|#

(make-tdl-template 'prd '((subj (local cat val subj first)))
                   '(((local cat val subj first) synsem)
                     ((local cat val subj rest) *null*)
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
                     ((local cat head) verbal)
                     ((local cat head) strict_type)
                     ((local cat head inv) -)
                     ((local cat val subj first) synsem)
                     ((local cat val subj rest) *null*)
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


