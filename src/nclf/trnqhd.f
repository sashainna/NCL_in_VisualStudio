c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       trnqhd.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:48
c**
c*****************************************************
c**
c** copyright (c) 1986 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  program name: trnqhd                                            **
c **                                                                  **
c **  last revision:                                                  **
c **                                                                  **
c **  purpose of program: transform a quilt surface header thru a
c **    matrix.
c **
c **       buf     data to be transformed
c **       rmx     matrix to be used
c **
c **********************************************************************
c **********************************************************************

      subroutine trnqhd (buf,rmx)

      include 'com4a.com'

      real*4 buf(40)
      real*8 rmx(12)
      integer*2 i,ix,npat,i2(12)
      real*8 u8(3)
      real*4 u4(6)
      equivalence (u8,u4,i2)
      u4(1)=buf(1)
      npat=i2(2)
      do 10,i=1,npat
        ix=i*3+2
        u8(1)=buf(ix)
        u8(2)=buf(ix+1)
        u8(3)=buf(ix+2)
        call conent(u8,rmx,3)
        buf(ix)=u8(1)
        buf(ix+1)=u8(2)
        buf(ix+2)=u8(3)
10    continue
99999 return
      end
