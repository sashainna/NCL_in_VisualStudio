C*********************************************************************
C*    NAME         :  support.f
C*       CONTAINS:
C*					ncdate  exit  getmcr  trmmsg  ftim  gerror
C*
C*    COPYRIGHT 2000 (c) Numerical Control Computer Sceinces Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       support.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:13:29
C********************************************************************/
C
c
c***********************************************************************
c
c   SUBROUTINE:  ncdate (cd)
c
c   FUNCTION:  This routine returns the current date in the form:
c              dd-mmm-yyyy.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1  -  Character string to receive the current
c                               date.
c
c***********************************************************************
c
      subroutine ncdate (ld)
c
      integer*4 id(3)
      character*12 ld
      character*3 cd(12)
      character*12 rc(3)
      integer vals(8)
c
      data cd /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1         'SEP','OCT','NOV','DEC'/
c
=HPX,SUN
      call idate (id)
=IBM,IRS,VMS
      call idate (id(2),id(1),id(3))
=WNT,W2K
      call date_and_time (rc(1),rc(2),rc(3),vals)
      id(1) = vals(3)
      id(2) = vals(2)
      id(3) = vals(1)
=ALL
      if (id(3) .lt. 1900) id(3) = id(3) + 1900
      if (id(3) .lt. 1970) id(3) = id(3) + 100
      write (ld,1) id(1),cd(id(2)),id(3)
    1 format (i2,'-',a3,'-',i4)
      return
      end
=ALL
c
c***********************************************************************
c
c   SUBROUTINE:  ftim (cd)
c
c   FUNCTION:  This routine returns the current time in the form:
c              hh:mm:ss.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1  -  Character string to receive the current
c                               time.
c
c***********************************************************************
c
      subroutine ftim (cd)
c
      character*8 cd
      integer*4 it(3)
      integer vals(8)
      character*12 rc(3)
=VMS
      call time (cd)
=IRS,HPX,SUN,IBM
      call itime (it(1),it(2),it(3))
=WNT,W2K
      call date_and_time (rc(1),rc(2),rc(3),vals)
      it(1) = vals(5)
      it(2) = vals(6)
      it(3) = vals(7)
=UNX,W2K
      write (cd,10) it(1),it(2),it(3)
   10 format (i2,':',i2.2,':',i2.2)
=ALL
      return
      end
c
c***********************************************************************
c
C   SUBROUTINE:  gerror (cbuf)
c
c   FUNCTION:  This routine returns an IBM error message string.
c
c   INPUT:  cbuf    C*n  D1  -  Character string to receive the error
c                               message text.
c
c   OUTPUT: none.
c
c***********************************************************************
c
=UNX,W2K
      subroutine gerror (cbuf)
c
      include 'toolib.com'
c
      character*(*) cbuf
c
c...Don't know how to get the error message
c
      write (cbuf,10) IBMERR
   10 format ('Fortran Error #',i4)
      return
      end
=ALL
      subroutine getvers (vdate, vernum)

      include 'toolib.com'
      character*12 vdate
      character*3 cd(12)
      integer*4 vernum, num1, num2, knum1, knum2, kerr
      data cd /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1         'SEP','OCT','NOV','DEC'/
c
c...Compared version date REVDAT
c
c
c...compared year first
c
      call ctoi(vdate(8:11),knum1,kerr)
      if (kerr.ne.0) goto 9000
      call ctoi(REVDAT(8:11),knum2,kerr)
      if (kerr.ne.0) goto 9000
      if (knum1.lt.knum2) then
          vernum = 0
          goto 8000
      endif
c
c...compare month
c
      do 100 num1 = 1, 12
          if (cd(num1).eq. vdate(4:6)) then
              goto 150
          endif
  100 continue
  150 do 200 num2 = 1, 12
          if (cd(num2).eq. vdate(4:6)) then
              goto 250
          endif
  200 continue
  250 if (num1.lt.num2) then
          vernum = 0
          goto 8000
      endif
c
c...compared day
c 
      call ctoi(vdate(1:2),knum1,kerr)
      if (kerr.ne.0) goto 9000
      call ctoi(REVDAT(1:2),knum2,kerr)
      if (kerr.ne.0) goto 9000
      if (knum1.lt.knum2) then
          vernum = 0
          goto 8000
      endif
      vernum = 1
 8000 return
 9000 vernum = 0
      return
      end 
