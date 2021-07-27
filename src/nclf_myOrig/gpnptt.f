c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       gpnptt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:09
c**
c*****************************************************
c*
c*   COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
c*
c**********************************************************************
c*
c*                           gtpnpt
c*                           ------
c*
c*         this subroutine extracts an individual point out of a 
c*     specific patern.
c*
c**********************************************************************

      subroutine gpnptt(rbuff, key, ptnum, trans)

      include 'com8a.com'
      include 'wrksys.com'


      real*8 rbuff(6)
      integer*4 key, ipntyp
      integer*2 ptnum
      logical trans

      real*8 umx(12)
      data umx /25.4d0,  0.0d0,  0.0d0, 0.0d0,
     y           0.0d0, 25.4d0,  0.0d0, 0.0d0,
     z           0.0d0,  0.0d0, 25.4d0, 0.0d0/

      call gtpnpt(rbuff,ipntyp,key,ptnum)
c
c...working system
c...vp3.26.93 added vector transf for PV patern type
c
      if (lwrk) then
          call conent (rbuff,invwrk,POINT)
          if (ipntyp.eq.2) call conent (rbuff(4),invwrk,VECTOR)
      end if
      if (ifl(264).eq.1) then
          call conent (rbuff,umx,POINT)
          if (ipntyp.eq.2) call conent (rbuff(4),umx,VECTOR)
      end if

c                  refsys
      if (ifl(72).eq.1.and.trans) then
          call conent (rbuff,sc(68),POINT)
          if (ipntyp.eq.2) call conent (rbuff(4),sc(68),VECTOR)
      end if

99999 return
      end
