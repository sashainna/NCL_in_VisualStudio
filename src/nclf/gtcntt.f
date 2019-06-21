c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       gtcntt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:09
c**
c*****************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtcntt(nclkey, ix, buf, ietype, ierr)
C*       Get composite curve element from unibase.
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey - key of composite curve.
C*          ix    -  Index of element in composite curve.
C*       OUTPUT :  
C*          buf    - real*8 array
C*          ietype - Type of entity.
C*          ierr   - 0 iff no error
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine gtcntt (nclkey, ix,  keyout, buf, ietype, imfl, ierr)


c      include 'com8.com'
      include 'com.com'
      include 'wrksys.com'

      integer*4 nclkey, keyout
      integer*2 ix, ietype, ierr,imfl
      real*8 buf(14)

      real*8 umx(12)
      data umx /25.4d0,  0.0d0,  0.0d0, 0.0d0,
     y           0.0d0, 25.4d0,  0.0d0, 0.0d0,
     z           0.0d0,  0.0d0, 25.4d0, 0.0d0/

      integer*2 it
      real*8 sec

      call gtcent (nclkey, ix, keyout, buf, ietype, ierr)

      if (ierr.ne.0) goto 99999
      if (.not.lwrk .and. ifl(264).eq.0) goto 99999
      it = 3
      if (ietype.ne.LINE) goto 20 
      if (lwrk) then
        call conent (buf,invwrk,it)
        call conent (buf(4),invwrk,it)
      endif
      if (ifl(264).eq.1 .and. imfl .eq. 1) then
        call conent (buf,umx,it)
        call conent (buf(4),umx,it)
      endif
      goto 99999

20    if (ietype.ne.CIRCLE) goto 99999 
      if (lwrk) then
        call conent (buf,invwrk,it)
        call conent (buf(4),invwrk,it)
        call conent (buf(7),invwrk,it)
        it = 4
        call conent (buf(10),invwrk,it)
        sec = dsqrt(buf(10)**2+buf(11)**2+buf(12)**2)
        if (sec.eq.0.0.or.sec.eq.1.0) goto 30
        buf(10) = buf(10)/sec
        buf(11) = buf(11)/sec
        buf(12) = buf(12)/sec
30      buf(13) = buf(13)/wrkscl
      endif
      if (ifl(264).eq.1 .and. imfl .eq. 1) then
        it = 3
        call conent (buf,umx,it)
        call conent (buf(4),umx,it)
        call conent (buf(7),umx,it)
        buf(13) = buf(13)*25.4d0
      endif

99999 return
      end
