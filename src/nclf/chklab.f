c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       chklab.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:39
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1988 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: chklab                                            **
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C          Call vstchk to find out if the label is previously used
C          if so set ifound to 1 and also check the CANON status. 
C **********************************************************************
C **********************************************************************
 
      subroutine chklab(label, nclkey, isub, ifound, ifl41)
 
      include 'com4a.com'
 
      integer*2 ietype, nwds,  ifound
      character*64 label
      integer*2 ifl41
      integer*4 isub, nclkey

      ifound = 0
      token2 = label
      ivxsub = isub
      call vstchk
      ifl41 = ifl(41)
      if (ist .eq. 1) then
         nclkey = 0
         goto 99
      endif
      if (ist.ne.1) ifound = 1
      call gtdesc(tv, nclkey, nwds,ietype)

 
99    return
      end
