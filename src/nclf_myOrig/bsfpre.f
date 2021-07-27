C*********************************************************************
C*    NAME         :  bsfpre.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       bsfpre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:38
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine bsfpre
C*       Define a B-spline surface.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine bsfpre

      include 'com8a.com'
c      implicit undefined (a-z)
c      include '../incf/com.com'

      common/pblok/p
      real*8 p(500)
      integer*4 ids(1000)
      equivalence (p,ids)

      common/keycom/keyold
      integer*4 keyold

      integer*2 i, j, jx, nents, ncrvs, ifit, ipg, nwds, ietype
      integer*4 nclkey
      real*8 bj(36)
      real*8 asn
      integer*2 ksn(4)
      equivalence (asn,ksn),(jb,bj)
      logical noslp

      asn=sc(10)
      nents = ksn(3)
      ifit = ksn(4)
      if (ifit.eq.0 .and. nents-nents/2*2.ne.0) goto 9277
      ncrvs = nents/2
      if (ifit.eq.1) ncrvs = nents
      noslp = ifit.eq.1 .or. nents.eq.2
      ipg = ifl(4) + 1
      jx = 36
c...
c    Load keys of entities that will define surf.
c...
      do 20 i = 1,nents
        if (i .lt. 13) then
          call gtdesc (sc(10+i),nclkey,nwds,ietype)
        else
          jx = jx + 1
          if (jx .gt. 35) then
            call getran(bj,ipg)
            ipg = ipg + 1
            jx = 1
          endif
          call gtdesc (bj(jx),nclkey,nwds,ietype)
        endif
        if (noslp) then
          ids(i) = nclkey
          ids(i+nents) = 0
        else
          j = (i+1)/2
          if (j*2-i .eq. 1) then
            ids(j) = nclkey
          else
            ids(j+ncrvs) = nclkey
          endif
        endif
20    continue
c
c... If 2 curves with 0 slope, set ncrvs=1 so ruled surf will be created
c
      if (nents .eq. 4 .and. ids(3).eq.0 .and. ids(4).eq.0) ncrvs = 1
      nclkey = keyold
      defwf = .true.
      call bsfdef(ncrvs, ids, ids(ncrvs+1), nclkey, ifit)
      if (nclkey .eq. 0) goto 9051
      ietype = 9
      call ptdesc (nclkey, ietype, tv)
      call srfcls (nclkey)
      rest = tv

99    return
c                   Error 51 - conditions too severe.
9051  ifl(2) = 51
      goto 9999
c                   Error 277 - must hve even number of items in sf def
9277  ifl(2) = 277
9999  err = .true.
      goto 99
      end
