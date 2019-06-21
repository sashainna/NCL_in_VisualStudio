c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       atan2f.f , 25.1
c**     DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:37
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1986 MILLS DATA SYSTEMS CORPORATION
C**
C*************************************************************************
C
C   COMPUTE THE ATAN2 FUNCTION OF 2 SCALARS AND RETURN IN VARIABLE 'TV'
C   FOR SUBROUTINE 'EXPRES'.
C
C*************************************************************************
 
      subroutine atang2f
 
      include 'com8a.com'

      common /at2com/ iax
      integer*2 iax

      real*8 a1(20),a2(20)

      iax=iax+1
      if (iax.gt.20) goto 9059
      call parser
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      call exprs2
      if (ifl(2).ne.0) goto 999
      if (ityp.ne.3.and.ityp.ne.4.and.(ityp.ne.2.or.ist.ne.2)) goto 9007
      a1(iax)=tv
      call parser
      if (ityp.ne.5.or.ist.ne.9) goto 9311
      call parser
      call exprs2
      if (ifl(2).ne.0) goto 999
      if (ityp.ne.3.and.ityp.ne.4.and.(ityp.ne.2.or.ist.ne.2)) goto 9007
      a2(iax)=tv
      call parser
      if (ityp.ne.5.or.ist.ne.7) goto 9310

      tv=datan2(a1(iax),a2(iax))*57.295779513d0
      goto 999

c                   number or scalar expected
9007  ifl(2)=7
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

999   iax=iax-1
      return
      end
