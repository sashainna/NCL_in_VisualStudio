C*********************************************************************
C*    NAME         :  varfmt.f
C*       CONTAINS:
C*    COPYRIGHT 1989 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*        varfmt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:52
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine varfmt
c*       This routine creates a run time format for a floating point
c*       number.  It is used when a routine wants to write a floating 
c*       point number out but it is not sure how many places to the 
c*       left of the decimal places are needed.
C*    PARAMETERS   
C*       INPUT  : 
C*          fnum   - variable containing floating point number to be printed.
c*          numdec - variable containing number of digits to the right of the
c*                   decimal point to be printed.
C*       OUTPUT :  
c*          fmtchr - character string containing generated format statement.
c*                   (must be 9 characters long)
c*          len    - total number of spaces the Fxx.xx format will print.  
c*                   e.g. if a F12.4 format is generated, len will equal 12.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine varfmt (fnum, numdec, fmtchr, len)

      include 'com4a.com'

      integer*2 numdec, len
      real*8 fnum, fnumtmp, afnum, lim
      character*9 fmtchr
c
c.....Added to correct format error.
c.....Paul. 04/02/92
c
      fnumtmp = dnint(fnum*10.0D0**numdec) / 10.0D0**numdec
      
      afnum = dabs(fnumtmp)
      lim = 1
      do 100, i = 1, 7
          lim = lim * 10.
          if (afnum .ge. lim) goto 100
              len = i + numdec + 1
              if (fnumtmp .lt. 0) len = len +1
              goto 200
100   continue
      call error (366)
      goto 99999

200   write (fmtchr, 1010) len, numdec
1010  format (' (f', i2, '.', i2, ')')

99999 return
      end
