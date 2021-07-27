C*********************************************************************
C*    NAME         :  dspmsh.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dspmsh.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:59
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dspmsh (nclkey, buf)
C*       display mesh surface on plotter or graphics terminal.
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey       - key of surface to display.
C*       OUTPUT :  
C*          buf          - last point displayed (used to label net surf)
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine dspmsh (nclkey, buf)

      include 'com4a.com'
      include 'mocom.com'

      integer*4 nclkey
      real*8 buf(3)

      real*4 ad(50),p1(3),ahdr(4)
      integer*2 isc11(4),ihdr(12),ipat,mmax,nmax
      equivalence (d(50),ahdr,ihdr),(d(52),ad),(sc(11),isc11)

c
c                         get U and V values from unibase
c
      call gtgeo(nclkey, d(50))
      mmax=ihdr(3)
      nmax=ihdr(4)
      nu=ihdr(9)
      nv=ihdr(10)
      du=1./nu
      dv=1./nv
c...                   set these based sf disply values
c      if(sc(12).lt.2.)sc(12)=2.
c      if(sc(13).lt.2.)sc(13)=2.
c      du=1./sc(12)
c      dv=1./sc(13)
c      nu=sc(12)
c      nv=sc(13)
      istps=5
      ipat=0
c...
c      write(cout,9005) nclkey
c9005  format(' calling gtgeo for header, key='i4)
c      call putmsg(cout, 80, 1, 0)
c...
c                      get header
c      call gtgeo(nclkey, d(50))
c      mmax=ihdr(3)
c      nmax=ihdr(4)
      off=0.
      if (ihdr(5).eq.7)off=ahdr(4)
      
      do 600 n=1,nmax

        do 500 m=1,mmax
c...
c      write(cout,9010) n,m
c9010  format(' calling gtmpat for patch, row='i4' col='i4)
c      call putmsg(cout, 80, 1, 0)
c...
          ipat=ipat+1
          call gtmpat(nclkey, ipat, d(52))

140       d(50)=d(52)
          d(51)=d(53)
          d(52)=d(54)
          ad(4)=0.
          ad(5)=0.
          ad(6)=0.

          if (n.lt.nmax) goto 150
c                                     start at top right corner for last row
          CALL DSPMEV(AD(49),AD(37),AD(25),AD(13),
     X                AD(46),AD(34),AD(22),AD(10),
     X                 0.,OFF,P1)
          goto 160
c                                     otherwise start at top left corner
150       CALL DSPMEV(AD(13),AD(10),AD(7),AD(4),
     X                AD(25),AD(22),AD(19),AD(16),
     X                 0.,OFF,P1)

160       BUF(1)=P1(1)+D(50)
          BUF(2)=P1(2)+D(51)
          BUF(3)=P1(3)+D(52)

          call gmova3(buf(1), buf(2), buf(3))

170     if (n.lt.nmax) goto 210
c                                     do 'top' edge for last row

        U=0.
        DO 200 I=1,NU
          U=U+DU
          CALL DSPMEV(AD(49),AD(37),AD(25),AD(13),
     X                AD(46),AD(34),AD(22),AD(10),
     X                 U,OFF,P1)

          DO 190 K=1,3
190         BUF(K)=P1(K)+D(49+K)
200       call glina3(buf(1), buf(2), buf(3))
C                                        DO LEFT EDGE
210       U=0.
          DO 240 I=1,NV
            U=U+DV
            CALL DSPMEV(AD(13),AD(10),AD(7),AD(4),
     X                AD(25),AD(22),AD(19),AD(16),
     X                 U,OFF,P1)
            DO 230 K=1,3
230           BUF(K)=P1(K)+D(49+K)
            call glina3(buf(1), buf(2), buf(3))
240       CONTINUE
C                                  DO 'BOTTOM' EDGE
          U=0.
          DO 270 I=1,NU
            U=U+DU
            CALL DSPMEV(AD(4),AD(16),AD(28),AD(40),
     X                AD(7),AD(19),AD(31),AD(43),
     X                 U,OFF,P1)
            DO 260 K=1,3
260           BUF(K)=P1(K)+D(49+K)
            call glina3(buf(1), buf(2), buf(3))
270       CONTINUE

          IF (M.LT.MMAX) GOTO 500
C                                    DO RIGHT EDGE FOR LAST PATCH IN ROW
          U=0.
          DO 300 I=1,NV
            U=U+DV
            CALL DSPMEV(AD(40),AD(43),AD(46),AD(49),
     X                AD(28),AD(31),AD(34),AD(37),
     X                 U,OFF,P1)
            DO 290 K=1,3
290           BUF(K)=P1(K)+D(49+K)
            call glina3(buf(1), buf(2), buf(3))
300       CONTINUE
c RAH: added call to set intr flag - effective on SGI ONLY
          call ckintr(ifl(86),ifl(35))
          if (ifl(86).eq.0) goto 500
          ifl(2)=-212
          go to 999
500     continue
600   continue
      call gdraw
      call drwlab(buf(1), buf(2), buf(3), nclkey)

999   return
      end
