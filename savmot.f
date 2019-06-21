C*********************************************************************
C*    NAME         :  savmot.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       savmot.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:39
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine savmot (tdat,tfwd)
c*         saves canonical data of points and
c*         vectors created by motion in ranfile.
C*    PARAMETERS   
C*       INPUT  : 
c*          tdat contains point and toolaxis vec*tor
c*          tfwd contains fwd vec*tor
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine savmot (tdat,tfwd)

      include 'com8a.com'
      include 'mocom.com'

      real*8 tdat(6), tfwd(3)

      integer*2 igen,iminit,impg,imel,npts
      equivalence (ifl(246),igen),(ifl(247),iminit)
      equivalence (ifl(248),impg),(ifl(249),imel),(ifl(250),npts)

      real*8 a(12)
      logical redef

      redef=.true.

      if (impg.eq.0) then
c                        the first point in this motion statement
        impg=ifl(4)+30
        if (lcmm) impg=impg+10
        imel=1
        npts=0
c                    if modify & second or subsequent
c                    motion statement, its the .001 modify start
c                    which we don't want to save
        if (ifl(104).eq.1.and.iminit.eq.0) goto 99999
      endif

      npts=npts+1
c      if (npts.gt.500) goto 99999
c
c...GENPTS
c
      ish  = 0
      if (igen .eq. 10) ish = 3
c
c......get GT point
c
      a(1)=tdat(1)
      a(2)=tdat(2)
      a(3)=tdat(3)
      l=3

      if (igen.eq.1) goto 200
c
c......get TV vector
c
      if (igen .eq. 3 .or. igen .eq. 8) goto 120
c
      a(4+ish)=tdat(4)
      a(5+ish)=tdat(5)
      a(6+ish)=tdat(6)
      l=6

      if (igen .eq. 2 .or. igen .eq. 7) goto 200
c
c......get FWD vector
c
120   a(l+1-ish)=tfwd(1)
      a(l+2-ish)=tfwd(2)
      a(l+3-ish)=tfwd(3)
      l=l+3
c
c...GENPTS are igen = 1 thru 4 and 7 thru 10
c
      if (igen .lt. 5 .or. igen .gt. 6) goto 200
c
c...CHKPOS
c
      l=12

      if (igen.eq.6) goto 140

      a(10)=s(1,1)
      a(11)=s(2,1)
      a(12)=s(3,1)
      goto 200

140   a(10)=s(1,2)
      a(11)=s(2,2)
      a(12)=s(3,2)

200   continue

      if (imel.gt.1 .and. imel+l.lt.36) goto 210

      i=impg
      if (imel.gt.1) i=i+1
      call putran (jb,i)

210   call putent (a,l,impg,imel,redef,0)

      imel=imel+l
      if (imel.lt.36.) goto 220
        impg=impg+1
        imel=imel-35

220   continue

c      if (.not.debug) goto 99999
c
c      write(cout,9010) a(1),a(2),a(3),npts,imel,impg
c9010  format(' savmot ',3f9.2,3i4)
c      call putmsg(cout,80,23,0)

99999 return
      end
