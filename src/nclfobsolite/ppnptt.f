c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       ppnptt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:27
c**
c*****************************************************
c*
c*   COPYRIGHT (c) 1987 MILLS DATA SYSTEMS CORPORATION
c*
c**********************************************************************
c*
c*                           ppnptt
c*                           ------
c*
c*         this subroutine puts an individual point into a 
c*     specific patern.
c*
c**********************************************************************

      subroutine ppnptt(rbuff, ipntyp, key, ptnum, trans)

      include 'com8a.com'
      include 'wrksys.com'

      real*8 rbuff(6)
      integer*4 ipntyp, key
      integer*2 ptnum
      logical trans

      real*8 umx(12)
      data umx /0.0d0, 0.0d0, 0.0d0, 0.0d0,
     y          0.0d0, 0.0d0, 0.0d0, 0.0d0,
     z          0.0d0, 0.0d0, 0.0d0, 0.0d0/
c
c...call transf if refsys for the point
c...vp3.26.93 added vector transf for pointvector
c
      if (ifl(72).eq.1.and.trans) then
          call transf(rbuff,sc(56),3,3)
          if (ipntyp .eq. 2) call transf(rbuff(4),sc(56),3,4)
      end if

c                     work system, transform entity with work mx
      if (ifl(264).eq.1) then 
        umx(1)  = 1.0d0/25.4d0
        umx(6)  = 1.0d0/25.4d0
        umx(11) = 1.0d0/25.4d0
        call conent (rbuff,umx,POINT)
        if (ipntyp .eq. 2) call conent (rbuff(4),umx,VECTOR)
      endif
      if (lwrk) then
          call conent (rbuff,wrkmx,POINT)
          if (ipntyp .eq. 2) call conent (rbuff(4),wrkmx,VECTOR)
      endif

      call ptpnpt(rbuff,key,ptnum)

99999 return
      end
