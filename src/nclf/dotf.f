c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dotf.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:57
c**
c*****************************************************
C ***  FILE NAME: DOTF
C**
C** COPYRIGHT (C) 1986 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **  SUBROUTINE NAME: DOTF
C **
C **  LAST REVISION:
C **
C **  PURPOSE OF SUBROUTINE: TO COMPUTE THE DOT FUNCTION OF 2 VECTORS 
C **   (I.E. THE COSINE OF THE TRUE ANGLE BETWEEN THEN) AND STORE 
C **   IN VARIABLE 'TV' FOR SUBROUTINE 'EXPRES' TO USE.
C **
C **********************************************************************
 
      subroutine dotf
 
      include 'com8a.com'

      common /dotcom/ idx
      integer*2 idx

      real*8 a(3,20),b(3,20), w(20)
      integer*4 nclkey
      integer*2 nwds,ietype
      integer*2 izro /0/
      logical trflg

      trflg = .false.
      idx=idx+1
      if (idx.gt.20) goto 9059
      call parser
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      if (ifl(2).gt.0) goto 999
      if (ityp.eq.2.and.ist.eq.1) goto 9009
      if (ityp.ne.2) goto 9011
      if (ist.eq.9) then
        call gtdesc(tv,nclkey,nwds,ietype)
        call ncl_get_sf_primtyp(nclkey,ietype)
        if (ietype.ne.3) goto 9011
        call gtplt(tv, izro, w)
      else
        if (ist.ne.4.and.ist.ne.6 .and. ist.ne.21) goto 9011
        call gtentt(tv,trflg,nclkey,ietype,w)
      endif
      ix     = 0
      if (ist .eq. 21) ix = 3
      a(1,idx) = w(1+ix)
      a(2,idx) = w(2+ix)
      a(3,idx) = w(3+ix)

      call parser
      if (ityp.ne.5.or.ist.ne.9) goto 9311
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      if (ifl(2).gt.0) goto 999
      if (ityp.eq.2.and.ist.eq.1) goto 9009
      if (ityp.ne.2) goto 9011
      if (ist.eq.9) then
        call gtdesc(tv,nclkey,nwds,ietype)
        call ncl_get_sf_primtyp(nclkey,ietype)
        if (ietype.ne.3) goto 9011
        call gtplt(tv, izro, w)
      else
        if (ist.ne.4.and.ist.ne.6 .and. ist .ne. 21) goto 9011
        call gtentt(tv,trflg,nclkey,ietype,w)
      endif
      ix     = 0
      if (ist .eq. 21) ix = 3
      b(1,idx) = w(1+ix)
      b(2,idx) = w(2+ix)
      b(3,idx) = w(3+ix)

      call parser
      if (ityp.ne.5.or.ist.ne.7) goto 9310

      dis=dsqrt(a(1,idx)**2+a(2,idx)**2+a(3,idx)**2)
      if (abs(dis).lt.1.d-6) goto 9049
      a(1,idx)=a(1,idx)/dis
      a(2,idx)=a(2,idx)/dis
      a(3,idx)=a(3,idx)/dis

      dis=dsqrt(b(1,idx)**2+b(2,idx)**2+b(3,idx)**2)
      if (abs(dis).lt.1.d-6) goto 9049
      b(1,idx)=b(1,idx)/dis
      b(2,idx)=b(2,idx)/dis
      b(3,idx)=b(3,idx)/dis

      tv=a(1,idx)*b(1,idx)+a(2,idx)*b(2,idx)+a(3,idx)*b(3,idx)
      goto 999

c                   identifier not previously defined
9009  ifl(2)=9
      goto 999
c                   vector expected
9011  ifl(2)=11
      goto 999
c                   bad input vector (zero length)
9049  ifl(2)=49
      goto 999
c                   expression too long
9059  ifl(2)=59
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999
c                   comma expected
9311  ifl(2)=311
      isvinx=inx
      goto 999

999   idx=idx-1
      return
      end
