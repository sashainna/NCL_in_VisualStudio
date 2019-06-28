c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       strwf1.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:46
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1988 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: strwf1                                            **
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C          Store a wireframe entity in the ranfile.
C          Routine labwf must have been called previously to set
C          up all variables except key.
C
C **********************************************************************
C **********************************************************************
 
      subroutine strwf1(nclkey)
 
      include 'com4a.com'
 
      integer*4 nclkey

      integer*2 irest(4)
      equivalence (rest,irest)
      integer*2 numwds(9)
      data numwds /0,1,3,3,6,4,11,1,1/

      if (unklbl) goto 99
      call ptdesc(nclkey,idst,rest)
      if (idst.le.9) then
        irest(3) = numwds(idst)
      else
        irest(3) = 0
      endif
      call vstore
 
99    return
      end
