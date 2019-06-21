C*********************************************************************
C*    NAME         :getclf.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*        getclf.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:07
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getclf
C*       Open cl file
C*    PARAMETERS   
C*       INPUT  : 
C*          fname -  name of cl file
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
 
      subroutine getclf(fname)

      include 'com8a.com'
      include 'mocom.com'

      character*(*) fname
c
      integer*4 inc,nrndex
c
      character*(MAX_PATH) clname,rfile
      character*80 clemsg
      character*1 lasnam(MAX_PATH)
      equivalence (lasnam,clfnam)

      data clemsg/'i/o error occured opening cl file '/

      clfnam = fname
      inc = nrndex(clfnam,' ')
      lasnam(inc+1) = '{'
      if (clfnam .eq. '.cl{' .or.
     x    clfnam .eq. '.CL{' .or.
     x    clfnam .eq. '{') then 
          clname = 'no_name.cl{'
          clfnam = clname
      else
          clname = clfnam
      endif
      if (ifl(69).ne.0) close(cllun)
      call flname(6, clname, rfile)
      call flopnw(cllun, rfile, 'DIRECT', 'UNFORMATTED', 288,
     x'NULL', ioerr)

      if(ioerr .ne. 0) then
        call putmsg (clemsg,80,4,0)
        go to 99999
      endif

      ifl(69) = 1
      istat(1) = 1
      call ncl_zroptr(imotp)

99999 return
      end
