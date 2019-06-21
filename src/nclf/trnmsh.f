c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       trnmsh.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:48
c**
c*****************************************************
c**
c** copyright (c) 1986 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  program name: trnmsh                                            **
c **                                                                  **
c **  last revision:                                                  **
c **                                                                  **
c **  purpose of program: transform a mesh surface patch thru a
c **    matrix.
c **
c **       buf     patch to be transformed
c **       rmx     matrix to be used
c **
c **********************************************************************
c **********************************************************************

      subroutine trnmsh (buf,rmx)

      include 'com4a.com'

      real*8 buf(26),rmx(12)
      integer*2 i,ix,it,jst
c                              transform first point
      call conent(buf,rmx,3)
c                              treat remaining points as vectors & transform
      ix=4
      jst=0
      do 100 i=1,15
        it=41+jst
        call conent(buf(ix),rmx,it)
        ix=ix+1+jst
        jst=1-jst
100   continue
99999 return
      end
