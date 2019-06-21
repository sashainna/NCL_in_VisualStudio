c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       gtref.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:11
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1994 NCCS Inc. 
C**
C **********************************************************************
C **  SUBROUTINE:  gtref (tfmat,i72)                                  **
C **                                                                  **
C **  FUNCTION: Get REFSYS matrix defined in FORTRAN and convert to
C **            CADD format used in unibase.
C **
C **********************************************************************
 
      subroutine gtref (tfmat,i72)
c 
      include 'com.com'
c
      real*8 tfmat(3,4)
      integer*2 i72
      real*8 ref(12), buf(3), umx (12)
      data umx /12*0.0d0/
c
      equivalence (ref,sc(56)) 
c
      i72    = ifl(72)
      if (i72 .eq. 0) go to 8000 
      k  = 0
      do 225 i=1,3
         do 125 j=1,4
            k   = k + 1
            tfmat(i,j) = ref(k)
  125    continue
         buf(i) = tfmat(i,4)
  225 continue
      if (ifl(264) .eq. 1) then
          umx(1) = 1.d0/25.4d0
          umx(6) = 1.d0/25.4d0
          umx(11) = 1.d0/25.4d0
          call conent (buf, umx, 4)
          tfmat(1,4) = buf(1)
          tfmat(2,4) = buf(2)
          tfmat(3,4) = buf(3)
      end if
 
 8000 return
      end
C
C **********************************************************************
C **  SUBROUTINE:  gtmod (tfmat,i72)                                  **
C **                                                                  **
C **  FUNCTION: Get REFSYS matrix defined in FORTRAN and convert to
C **            CADD format used in unibase.
C **
C **********************************************************************
 
      subroutine gtmod (tfmat,iflg)
c 
      include 'com.com'
      include 'wrksys.com'
c
      integer*2 iflg
c
      real*8 tfmat(3,4)
      integer*2 i,k
c
      iflg = 0 
      if (lwrk) then 
         iflg = 1
         k    = 0
         do 225 i=1,3
            do 125 j=1,4
               k   = k + 1
               tfmat(i,j) = wrkmx(k)
  125       continue
  225    continue
      end if
c 
 8000 return
      end
