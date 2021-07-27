C*********************************************************************
C*    NAME         :  getas.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*        getas.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:07
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getas
C*       Open apt source file
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine getas(fname)

      include 'com4a.com'
      include 'mocom.com'

      character*80 clemsg,temp,rfile,fname
      character*1 atemp(80)
      equivalence (atemp(1), temp)

      data clemsg/'i/o error occured opening cl file '/

      temp = fname
      do 50 j=1,80
50        if (atemp(j) .eq. ' ') go to 100
100   asfnam = temp(1:j-1) // '@'

      if (ifl(69).eq.0) then
        temp='tmp.cl@'
        call flname(6, temp, rfile)
        call flopnw(cllun, rfile, 'DIRECT', 'UNFORMATTED', 288,
     x   'NULL', ioerr)

        if(ioerr .ne. 0) then
          call putmsg (clemsg,80,4,0)
          go to 99999
        endif
        ifl(69) = 2
        istat(1) = 1
        call ncl_zroptr(imotp)
      endif
      if (asfnam .eq. '.as@' .or. 
     x    asfnam .eq. '@') asfnam = 'no_name.as@'
      iaptix = -1
      ifl(88) = 1
      aptcom = .true.

99999 return
      end
