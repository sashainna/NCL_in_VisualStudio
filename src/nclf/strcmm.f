C*********************************************************************
C*    NAME         :  strcmm.f
C*       CONTAINS:
C*    COPYRIGHT 1992 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       strcmm.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:45
C********************************************************************/
C **
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: strcmm
C **
C **  PURPOSE OF PROGRAM: Analyze, weed & store cmm points.
C **
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE strcmm(npts)
 
c      include 'com8.com'
c      implicit undefined (a-z)
      include 'com.com'
      include 'cmm.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      integer*2 npts
c
      real*8 tb1(12), tb2(12), tb3(12), cmax, cmtol, comax, dst, d1, d2
      real*8 co, si, crwn, tdat(120), ndot, ndist
      integer*2 lnk, jptk, isubcl, ihld, ix1, ix2, ix3, numitm
      integer*2 ntk, istpg
      equivalence (ifl(79),ntk)

      real*8 RADIAN
      parameter (RADIAN=57.2957795D0)
      integer*2 i5000
      data i5000 /5000/

      if (ifl(2).gt.0) goto 500

c Set lcmm false so savmot will work correctly.
      lcmm = .false.
      ntk = 0
      lnk = 1
      jptk = 0
      isubcl = 5
      comax = dcos(cmmmxa/RADIAN)
      cmax = cmmmxd
      cmtol = cmmtol
      ihld = 0
      ix1 = 1
      dst = 0.0
      istpg = ifl(4)+40
      call gtcmmp (ix1, istpg, tb1)
      if (cmmnpt.eq.0) call outcmm (tdat, tb1, lnk, jptk, isubcl)
      do 200, ix3 = 2,npts
      ix2 = ix3-1
      call gtcmmp (ix2, istpg, tb2)
      call gtcmmp (ix3, istpg, tb3)
10    continue
      d1 = ndist(tb3,tb2)
      dst = dst+d1
      if (dst.ge.cmax) goto 12
      co = ndot(tb3(7),tb1(7))
      si = 1.0-co**2
      if (si.lt.0.0) si = 0.0
      si = dsqrt(si)
      d2 = ndist(tb3,tb1)
      crwn = d2*si/8.0
      if (crwn.gt.cmtol) goto 12
      if (co.lt.comax) goto 13
      if (d1.gt.cmax) goto 13
      co = ndot(tb3(7),tb2(7))
      if (co.lt.comax) goto 13
      ihld = 1
      goto 200
12    if (ihld.eq.0) goto 14
      call outcmm (tdat, tb2, lnk, jptk, isubcl)
      ihld = 0
      ix1 = ix3-1
      call gtcmmp (ix1, istpg, tb1)
      dst = 0.0
      goto 10
13    ihld = 0
14    ix1 = ix3
      call outcmm (tdat, tb3, lnk, jptk, isubcl)
      call gtcmmp (ix1, istpg, tb1)
      dst = 0.0
200   continue

      if (ihld.eq.1) call outcmm (tdat, tb3, lnk, jptk, isubcl)

      token2 = '@N'
      ivxsub=0
      call vstchk
      cmmnpt = cmmnpt + jptk
      rest = cmmnpt
      idst = 2
      savid2 = token2
      isvsub = ivxsub
      keyold = keyhld
      istold = ist
      ifl(9) = ifl(11)
      ifl(10) = ifl(12)
      call vstore

      if (ifl(42).eq.0 .and. ntk.gt.0) then
        numitm = ntk/(3*ifl(82)+3)
        call putcl (i5000, isubcl, numitm, tdat)
      endif

500   continue
      lcmm = .true.

999   RETURN
      END
C********************************************************************/
      subroutine gtcmmp(ix, istpg, tb)

      include 'com.com'

      integer*2 ix, istpg
      real*8 tb(12)

      integer*2 itmp, ipg, iel, izro, i12
      data izro /0/
      data i12 /12/

      itmp = (ix-1)*12
      ipg = istpg+itmp/35
      iel = itmp-itmp/35*35+1
      if (iel.gt.35) then
        iel = iel-35
        ipg = ipg+1
      endif

      call getent (tb,i12,ipg,iel,izro)

      return
      end
C********************************************************************/
      function ndot (v1,v2)

      include 'com.com'

      real*8 ndot, v1(3), v2(3)

      ndot = v1(1)*v2(1)+v1(2)*v2(2)+v1(3)*v2(3)

      return
      end
C********************************************************************/
      function ndist (p1,p2)

      include 'com.com'

      real*8 ndist, p1(3), p2(3)

      ndist = dsqrt((p1(1)-p2(1))**2+(p1(2)-p2(2))**2+(p1(3)-p2(3))**2)

      return
      end
C********************************************************************/
C
      subroutine outcmm (tdat, tb, lnk, jptk, isubcl)

      include 'com.com'
      include 'cmm.com'

      real*8 tdat(120), tb(*)
      integer*2 lnk, jptk, isubcl
c
c     common /cmmcom/ cmmtol, cmmmxd, cmmmxa, cmmmnm, cmminm, cmmnpt
c     real*8 cmmtol, cmmmxd, cmmmxa, cmmmnm
c     integer*2 cmminm, cmmnpt
c
      integer*2 i, n, numitm, ix
      character*64 pnam,v1nam,v2nam,v3nam
      data pnam /'@P'/, v1nam /'@V1'/, v2nam /'@V2'/, v3nam /'@V3'/
      integer*2 ntk
      character*64  cmmcnm
      equivalence (cmmmnm,cmmcnm)
      equivalence (ntk,ifl(79))
      integer*2 I3, I4,I5000
      integer*4 sub
      logical LFLSE
      data I3 /3/, I4 /4/, I5000 /5000/
      data LFLSE /.false./
c
      if (ifl(42).eq.0) then
cc        call plotm (tb,LFLSE)
cc        ifl(270)=0
cc        call motend
        if (cmmcnm .eq.' ') then
          n = 3+ifl(82)*3
          do 10 i=1,n
10        tdat(ntk+i)=tb(i)
          ntk = ntk+n
          if (ntk.eq.120) then
            numitm=ntk/n
            call putcl(I5000,isubcl,numitm,tdat(1))
            ntk = 0
            isubcl = 6
          endif
        endif
      endif
      if (ifl(154).eq.1) then
        ix = jptk
        call tdsply(lnk,tb,ix)
      endif
      if (ifl(246).gt.0) call savmot(tb,tb(7))

      jptk = jptk+1
      ix = cmmnpt+jptk
c                                   store point
      sub = ix
      call storit (tb,pnam,sub,I3,1)
      if (ifl(2).ne.0) goto 999
c                                   store ta vec
      call storit (tb(4),v1nam,sub,I4,1)
      if (ifl(2).ne.0) goto 999
c                                   store fwd vec
      call storit (tb(7),v2nam,sub,I4,1)
      if (ifl(2).ne.0) goto 999
c                                   store normal vec
      call storit (tb(10),v3nam,sub,I4,1)

999   return
      end
