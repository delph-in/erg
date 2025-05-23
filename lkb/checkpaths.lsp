#|
Check paths created from execution of
  (WITH-CHECK-PATH-LIST-COLLECTION "~/erg/lkb/checkpaths.lsp" (PARSE-SENTENCES "~/erg/lkb/checkpaths.items" T))
with grammar ERG2025 on 18-5-2025 (05:43:08)
|#
(CL:IN-PACKAGE #:LKB)
(DEFPARAMETER *CHECK-PATHS*
  '(((SYNSEM LOCAL CAT HEAD) . 1018284)
    ((SYNSEM LOCAL CAT VAL COMPS) . 219842)
    ((SYNSEM NONLOC SLASH LIST) . 140526)
    ((SYNSEM --MIN) . 66207)
    (NIL . 63075)
    ((SYNSEM) . 58334)
    ((SYNSEM LOCAL CAT MC) . 48205)
    ((SYNSEM PUNCT RPUNCT) . 34259)
    ((SYNSEM LOCAL CAT HEAD VFORM) . 30742)
    ((INFLECTD) . 21118)
    ((SYNSEM --SIND) . 15178)
    ((SYNSEM LOCAL CAT HEAD MOD FIRST) . 12258)
    ((SYNSEM LOCAL CAT HEAD PRD) . 10953)
    ((SYNSEM MODIFD) . 9349)
    ((SYNSEM NONLOC QUE LIST) . 9131)
    ((SYNSEM LOCAL CAT HEAD MOD) . 9020)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT HEAD) . 6687)
    ((SYNSEM LOCAL CONJ) . 6524)
    ((SYNSEM LOCAL CAT HEAD MINORS ALTMIN) . 6144)
    ((SYNSEM NONLOC SLASH LIST FIRST) . 5470)
    ((SYNSEM LOCAL CAT VAL SUBJ FIRST) . 4782)
    ((SYNSEM PUNCT LPUNCT) . 4025)
    ((SYNSEM LOCAL CAT VAL SPR FIRST) . 3794)
    ((SYNSEM LOCAL CAT --SLASHED BOOL) . 3302)
    ((SYNSEM LOCAL CAT VAL SPR) . 2979)
    ((SYNSEM LOCAL CAT VAL COMPS REST) . 2663)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT) . 2651)
    ((SYNSEM LOCAL AGR PNG PN) . 2250)
    ((ORTH RD) . 2224)
    ((SYNSEM LOCAL CAT HEAD MOD FIRST LOCAL) . 1944)
    ((SYNSEM LOCAL CAT HEAD INV) . 1861)
    ((SYNSEM NONLOC REL LIST) . 1443)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT HEAD MINORS MIN) . 1328)
    ((SYNSEM LOCAL CAT HEAD TAM) . 1282)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST OPT) . 1196)
    ((SYNSEM LOCAL CAT VAL SUBJ) . 1168)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST) . 933)
    ((SYNSEM LOCAL CAT HEAD AUX) . 835)
    ((SYNSEM PUNCT RCLSTR RPAREN) . 794)
    ((SYNSEM PUNCT RCLSTR RITAL) . 789)
    ((SYNSEM LOCAL CAT HEAD MOD FIRST LOCAL CAT HEAD) . 753)
    ((SYNSEM LEX) . 740)
    ((SYNSEM LOCAL CONT HOOK XARG) . 707)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST LOCAL CAT) . 673)
    ((SYNSEM LOCAL CAT VAL SPR FIRST --MIN) . 669)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT HEAD CASE) . 618)
    ((SYNSEM LOCAL CAT HEAD MINORS NORM) . 441)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST LOCAL CAT HEAD) . 432)
    ((SYNSEM LOCAL CAT VAL COMPS REST FIRST LOCAL CAT HEAD) . 411)
    ((SYNSEM LOCAL CAT HEAD CASE) . 408)
    ((SYNSEM LOCAL CONT HOOK INDEX SORT) . 389)
    ((SYNSEM LOCAL CAT VAL SPR FIRST LOCAL CAT HEAD) . 389)
    ((SYNSEM LOCAL CAT POSTHD) . 379)
    ((SYNSEM LOCAL CAT HEAD TAM MOOD) . 357)
    ((SYNSEM LOCAL CONT HOOK INDEX DIV) . 325)
    ((SYNSEM LOCAL CONT HOOK XARG SORT) . 321)
    ((SYNSEM LOCAL ARG-S REST FIRST) . 296)
    ((SYNSEM LOCAL AGR DIV) . 287)
    ((SYNSEM MODIFD LPERIPH) . 276)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT HEAD MOD FIRST --MIN) . 216)
    ((SYNSEM LOCAL CONJ CHEAD) . 214)
    ((ALTS CSAI) . 208)
    ((SYNSEM LOCAL ARG-S REST REST) . 205)
    ((SYNSEM LOCAL CAT VAL SUBJ FIRST --SIND) . 184)
    ((SYNSEM LOCAL) . 183)
    ((SYNSEM PUNCT PNCTPR) . 168)
    ((SYNSEM LOCAL CAT VAL SPR REST) . 163)
    ((SYNSEM LOCAL CAT HEAD TAM ASPECT PROGR) . 162)
    ((SYNSEM LOCAL CAT HEAD TAM TENSE) . 135)
    ((SYNSEM LOCAL CAT VAL COMPS REST FIRST LOCAL CAT VAL SUBJ) . 132)
    ((SYNSEM LOCAL CONT HOOK INDEX PNG PN) . 129)
    ((SYNSEM MODIFD RPERIPH) . 115)
    ((SYNSEM LOCAL CAT VAL COMPS REST FIRST) . 113)
    ((SYNSEM LOCAL ARG-S REST FIRST LOCAL CAT HEAD) . 108)
    ((SYNSEM LOCAL CAT HEAD MOD FIRST --MIN) . 107)
    ((SYNSEM PUNCT RPUNCT PSF) . 96)
    ((SYNSEM LOCAL CAT HEAD MOD FIRST PUNCT RPUNCT) . 90)
    ((SYNSEM LOCAL ARG-S REST FIRST --SIND) . 88)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST --MIN) . 86)
    ((SYNSEM LOCAL CAT HEAD --BARE) . 64)
    ((SYNSEM LOCAL CAT VAL SUBJ FIRST LOCAL CAT HEAD) . 58)
    ((SYNSEM LOCAL CONJ RPERNUM) . 53)
    ((SYNSEM LOCAL CAT VAL SPR FIRST LOCAL CAT HEAD MINORS MIN) . 51)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST NONLOC SLASH LIST) . 48)
    ((SYNSEM LOCAL CAT VAL COMPS REST FIRST LOCAL CAT) . 47)
    ((SYNSEM PHON ONSET) . 41)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT RSUBJHD MOD FIRST LOCAL CAT HEAD MINORS ALTMIN) . 40)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT HEAD PRD) . 39)
    ((SYNSEM LOCAL CONT HOOK INDEX SF) . 37)
    ((SYNSEM LOCAL CAT VAL SPEC) . 36)
    ((SYNSEM LOCAL CAT VAL SPR FIRST LOCAL AGR DIV) . 35)
    ((SYNSEM LKEYS --COMPKEY) . 33)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT HEAD MOD) . 32)
    ((C-CONT HOOK INDEX PNG PN) . 31)
    ((SYNSEM LOCAL CAT --SLASHED OR REST FIRST AND FIRST BOOL) . 31)
    ((SYNSEM LOCAL CAT VAL SUBJ FIRST NONLOC SLASH LIST) . 30)
    ((SYNSEM LOCAL CAT VAL SPR FIRST LOCAL CAT VAL SPR) . 29)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT HEAD MOD FIRST LOCAL CAT HEAD PRD) . 28)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST LOCAL CAT VAL SUBJ) . 28)
    ((SYNSEM NONLOC SLASH LIST FIRST CONT HOOK XARG SORT) . 25)
    ((SYNSEM LOCAL CAT VAL SPR FIRST NONLOC SLASH LIST) . 25)
    ((C-CONT HOOK XARG DIV) . 25)
    ((SYNSEM LOCAL ARG-S REST REST FIRST LOCAL CAT HEAD) . 24)
    ((SYNSEM LOCAL CAT VAL COMPS REST FIRST NONLOC SLASH LIST) . 23)
    ((SYNSEM LOCAL CAT VAL SPR FIRST OPT) . 23)
    ((SYNSEM NONLOC SLASH LIST FIRST AGR PNG PN) . 18)
    ((SYNSEM --SIND E ASPECT PROGR) . 17)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST LOCAL CONT HOOK XARG) . 16)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST LOCAL CAT HEAD MOD FIRST LOCAL CAT VAL SUBJ FIRST LOCAL CAT HEAD) . 16)
    ((SYNSEM NONLOC SLASH LIST FIRST CONT HOOK INDEX SORT) . 15)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST LOCAL CONT HOOK XARG SORT) . 14)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST LOCAL CAT HEAD MOD FIRST LOCAL CAT VAL SUBJ FIRST MODIFD LPERIPH) . 14)
    ((SYNSEM LOCAL CAT VAL SUBJ FIRST OPT) . 14)
    ((SYNSEM LOCAL CAT VAL KCMP --SIND SF) . 13)
    ((SYNSEM LOCAL CONT HOOK INDEX E TENSE) . 13)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST --SIND) . 12)
    ((C-CONT HOOK XARG PNG PN) . 12)
    ((SYNSEM LOCAL ARG-S REST REST FIRST) . 11)
    ((C-CONT HOOK XARG) . 11)
    ((SYNSEM LOCAL CAT HEAD MOD FIRST LOCAL CONT HOOK XARG) . 10)
    ((ALTS CPFRAG) . 10)
    ((SYNSEM LOCAL CONJ LINDIV) . 10)
    ((SYNSEM LOCAL AGR IND) . 10)
    ((SYNSEM LOCAL ARG-S REST FIRST OPT) . 9)
    ((SYNSEM LOCAL CTXT ACTIVATED) . 9)
    ((SYNSEM NONLOC SLASH LIST FIRST CONT HOOK INDEX E TENSE) . 9)
    ((SYNSEM LOCAL CAT VAL COMPS REST FIRST --MIN) . 9)
    ((SYNSEM LOCAL CAT HEAD MOD FIRST MODIFD RPERIPH) . 8)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST NONLOC SLASH LIST FIRST CAT) . 8)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT RSUBJHD MOD FIRST LOCAL CAT HEAD MOD) . 8)
    ((SYNSEM NONLOC SLASH LIST FIRST CONT HOOK XARG PNG PN) . 8)
    ((SYNSEM PUNCT PAIRED) . 8)
    ((SYNSEM LOCAL CAT VAL SPEC FIRST LOCAL CONT HOOK INDEX) . 8)
    ((SYNSEM LOCAL CAT HEAD MOD FIRST --SIND) . 8)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST LOCAL CONT HOOK INDEX) . 8)
    ((ALTS DATIVE) . 7)
    ((ALTS NPPART) . 7)
    ((SYNSEM LOCAL CAT HEAD MOD FIRST --SIND SORT) . 7)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT HEAD MOD FIRST LOCAL CAT HEAD MINORS ALTMIN) . 7)
    ((SYNSEM LOCAL CAT HEAD --TITLE-CPD) . 6)
    ((SYNSEM LOCAL CAT) . 6)
    ((SYNSEM NONLOC QUE LIST FIRST) . 6)
    ((SYNSEM LOCAL CAT VAL SPR FIRST LOCAL CAT VAL SPEC FIRST LOCAL CAT VAL COMPS) . 6)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST LOCAL CAT HEAD MINORS NORM) . 5)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT HEAD MOD FIRST LOCAL CAT HEAD MOD FIRST --MIN) . 5)
    ((SYNSEM LOCAL CONJ CREL ARG2 PNG PN) . 5)
    ((ALTS VPELLIP) . 5)
    ((SYNSEM LOCAL CAT VAL SUBJ FIRST --SIND SORT) . 5)
    ((SYNSEM LOCAL CAT HEAD MOD FIRST LOCAL CAT HEAD AUX) . 4)
    ((SYNSEM LOCAL CAT HEAD MOD FIRST LOCAL CAT HEAD MINORS ALTMIN) . 4)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT HEAD MOD FIRST LOCAL CAT HEAD MOD FIRST --SIND SORT) . 4)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST LOCAL CAT RSUBJHD) . 4)
    ((SYNSEM LOCAL CAT HEAD MOD FIRST LOCAL CAT MC) . 4)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST LOCAL CONT HOOK XARG PNG PN) . 4)
    ((SYNSEM LOCAL CAT VAL COMPS REST REST FIRST LOCAL CAT VAL SUBJ) . 4)
    ((SYNSEM LOCAL CAT HEAD MOD FIRST OPT) . 4)
    ((SYNSEM LOCAL ARG-S REST FIRST LOCAL CAT) . 4)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT HEAD VFORM) . 3)
    ((SYNSEM LOCAL CAT VAL SPCMPS) . 3)
    ((SYNSEM NONLOC SLASH LIST FIRST CAT RSUBJHD) . 3)
    ((SYNSEM PUNCT) . 3)
    ((ORTH CLASS) . 3)
    ((SYNSEM LOCAL CONT HOOK INDEX E MOOD) . 3)
    ((ALTS TOUGH) . 2)
    ((SYNSEM LOCAL CAT HEAD POSS) . 2)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST NONLOC SLASH LIST FIRST CONT HOOK INDEX PNG PN) . 2)
    ((SYNSEM LOCAL ARG-S REST FIRST LOCAL CONT HOOK INDEX) . 2)
    ((SYNSEM LKEYS --OCOMPKEY) . 2)
    ((SYNSEM LOCAL CONJ RINDIV) . 2)
    ((SYNSEM LOCAL ARG-S REST REST FIRST LOCAL CONT HOOK INDEX) . 2)
    ((SYNSEM NONLOC SLASH LIST FIRST CONT HOOK XARG PT) . 2)
    ((SYNSEM LOCAL CAT VAL SPEC FIRST LOCAL CAT HEAD) . 2)
    ((SYNSEM LOCAL ARG-S REST REST FIRST --MIN) . 2)
    ((SYNSEM LOCAL CONJ CPRED) . 2)
    ((SYNSEM LOCAL CONJ CREL PRED) . 2)
    ((SYNSEM LOCAL CAT VAL COMPS REST REST) . 2)
    ((SYNSEM LOCAL CAT VAL SUBJ FIRST LOCAL CONT HOOK INDEX) . 2)
    ((SYNSEM --SIND E ASPECT PRF) . 2)
    ((SYNSEM LOCAL CAT --SLASHED OR FIRST BOOL) . 2)
    ((SYNSEM NONLOC SLASH LIST FIRST CONT HOOK INDEX) . 2)
    ((SYNSEM LOCAL CAT VAL COMPS FIRST NONLOC SLASH LIST FIRST CONT HOOK INDEX SORT) . 1)
    ((SYNSEM LOCAL CAT VAL KCMP LOCAL CAT HEAD VFORM) . 1)
    ((SYNSEM LOCAL CAT VAL SPEC FIRST LOCAL CONT HOOK INDEX PNG PN) . 1)
    ((SYNSEM LOCAL CAT VAL KCMP LOCAL CAT HEAD) . 1)
    ((SYNSEM LOCAL AGR PNG GEN) . 1)
    ((C-CONT HOOK INDEX DIV) . 1)
    ((SYNSEM LOCAL CAT RSUBJHD) . 1)))
