C*********************************************************************
C*    NAME         :  srfpre.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       srfpre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:44
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine srfpre
c*       depending on the info in sc(10), decides which surface
C*       routine to call, and calls it.
c*                                                                   
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
       subroutine srfpre

      include 'com8a.com'

      integer*2 maxpt, maxwd
      parameter (maxpt = 50)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))

c
c..... w-array is the work area for 4 defining geometric entities
c
      common/wblok/w(4*(maxwd+20))

      real*8 asn, w
      real*4 aw(8*(maxwd+20))
      integer*2 ksn(4),lsc(400),ient(4)
      logical noeras

      equivalence (asn,ksn),(sc,lsc),(sc(53),ient),(aw,w)

      noeras=.false.
      ASN=SC(10)

      IF (KSN(2).NE.8) GOTO 80
      CALL FILPRE

      IF (IFL(2).GT.0) GOTO 99999
      ranpts=.true.
      GOTO 100

80    if(ksn(2).ne.4) goto 90
      call cylpre
      goto 95

90    if (ksn(3).ne.2) goto 100
c
c          ******* rldsrf ******    ( sf/cv,cv )
c
      call rldpre

95    if (ifl(2).lt.1) call rldgen
      goto 99999

100   call srfdef
      ranpts = .false.
c
c..... if error flag set, exit now    
c
      if(ifl(2).gt.0) goto 99999
c
c..... call patgen to generate a panel of patches
c
      call patgen
      if (ifl(2).gt.0) goto 99999
      call rectfy
      if (.not.geodsp) go to 99999
      cout=' '

      do 8643 i=15,17
8643       call putmsg (cout,10,i,noeras)

99999 if(ifl(2).gt.0) err=.true.
      return
      end
