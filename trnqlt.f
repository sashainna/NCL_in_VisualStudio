c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       trnqlt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:48
c**
c*****************************************************
c**
c** copyright (c) 1986 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  program name: trnqlt                                            **
c **                                                                  **
c **  last revision:                                                  **
c **                                                                  **
c **  purpose of program: transform a quilt surface patch thru a
c **    matrix.
c **
c **       buf     patch to be transformed
c **       rmx     matrix to be used
c **
c **********************************************************************
c **********************************************************************

      subroutine trnqlt (buf,rmx)

      include 'com4a.com'


      real*4 buf(80)
      real*8 rmx(12)
      integer*2 i
      real*8 p(3),xo,yo,zo

c                              save origin point
      xo=buf(3)
      yo=buf(4)
      zo=buf(5)
c                              transform delta offsets (vectors) from origin
      do 10,i=1,25
        p(1)=xo+buf(i+5)
        p(2)=yo+buf(i+30)
        p(3)=zo+buf(i+55)
        call conent(p,rmx,3)
        if (i.eq.1) then
            buf(3)=int(p(1))
            buf(4)=int(p(2))
            buf(5)=int(p(3))
        endif
        buf(i+5)=p(1)-buf(3)
        buf(i+30)=p(2)-buf(4)
        buf(i+55)=p(3)-buf(5)
10    continue
99999 return
      end
