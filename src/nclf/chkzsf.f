c **********************************************************************
c **********************************************************************
c **  subroutine name: chkzsf
c **                                                                  **
c **  purpose of subroutine: to project point onto zsurf plane
c **                                                                  **
c **   MODULE NAME AND RELEASE LEVEL 
c **     chkzsf.f , 25.1
c **  DATE AND TIME OF LAST  MODIFICATION
c **     04/29/15 , 15:09:40
c **********************************************************************
c **********************************************************************
c**
c** copyright (c) 1987 mills data systems corporation
c**
 
      subroutine chkzsf(rbuff)
 
      include 'com8a.com'
 
      real*8 rbuff(3),zs(4)
      if(ifl(55).eq.1) then
c               if zsurf on, load zs-tbl from sc(37,)
          do 910 i=1,4
910       zs(i)=sc(i+36)
c              if refsys, convert zsurf pl
          if(ifl(72).eq.1) call transf(zs,sc(68),4,6)
c             zsurf on. check for valid pl
          if (abs(zs(3)).lt.1.e-6) then
c                 error. zsurf pl ng.
              ifl(2)=276
              goto 99999
          endif
          rbuff(3)=(zs(4)-zs(1)*rbuff(1)-zs(2)*rbuff(2))/zs(3)
      endif
 
99999 return
      end
