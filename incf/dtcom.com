c**********************************************************************
c**
c**    NAME         :  dtcom.com 
c**
c**    CONTAINS:
c**          data tablet common
c**
c**    MODULE NAME AND RELEASE LEVEL 
c**       dtcom.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:31
c**
c*********************************************************************
      common /dtcomc/ dtline(72), comrec
      common /dtcomi/ gerrno, vwd, gsklun
      common /dtcoml/ more, curson, vergeo, immsw, cross, ldum1,
     x                nocoma, cntlin, modal, modfrs, modset

      character*1 dtline
      character*40 comrec
      integer*2 gerrno, gsklun, skpsyn
      logical more, curson, vergeo, immsw, cross, ldum1
      logical nocoma, cntlin, modal, modfrs, modset

      integer*2 curno, cinpt, parssw, coutpt, tokerr, matchn
      integer*2 scpnt, vwd(20)
      equivalence (curno, ifl(173)), (skpsyn, ifl(175))
      equivalence (cinpt, ifl(177))
      equivalence (parssw, ifl(178)), (coutpt, ifl(179))
      equivalence (scpnt, ifl(180))
      equivalence (tokerr, ifl(181)), (matchn, ifl(182))

