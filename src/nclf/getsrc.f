C*********************************************************************
C*    NAME         :  getsrc.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       getsrc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:08
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getsrc(cmsg,nc,irecno,icmt)
C*     this routine reads a record from the
C*     part program source file
C*
C*    PARAMETERS
C*       INPUT  :
C*          irecno   =  the number of the record to be read
C*          icmt     =  1 = Return comment characters at end of line
C*       OUTPUT :
C*          cmsg     =  the array into which the data is to be read
C*          nc       =  number of chars in 'cmsg'
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine getsrc(cmsg,nc,irecno,icmt)
 
      include 'com8a.com'
 
      character*(MAX_LEN) cmsg
      integer*2 icmt
      integer*4 irecno, nc

      integer*4 ioerr,nclf_getsrc
c
c...Read the requested record
c
      ioerr = nclf_getsrc(irecno,cmsg,nc,IRCTYP,icmt)
      if (ioerr.ne.0) go to 88888
      if ((irecno+1).gt.nhline) then
           nhline = irecno+1
      endif
      ncline = irecno+1
cc      if (cmsg(1:nc) .eq. 'eof') go to 88888
      srceof=.false.
cc      lincld = (IRCTYP .eq. 1)
cc      lincfi = (IRCTYP .eq. 3)
      go to 99999
88888 srceof=.true.
cc      lincld=.false.
      irctyp = 0
99999 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getsrcc(cmsg,nc,irecno,fend)
C*     this routine reads a record from the
C*     part program source file
C*
C*    PARAMETERS
C*       INPUT  :
C*      irecno   =  the number of the record to be read
C*       OUTPUT :
C*      cmsg     =  the array into which the data is to be read
C*      nc:      = length of data
C*      fend      = file end
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine getsrcc(cmsg,nc,irecno,fend)
 
      include 'com8a.com'
 
      character*(MAX_LEN) cmsg
      integer*4 irecno,nc,fend
c
c...Read the requested record
c
      call getsrc (cmsg,nc,irecno,0)
      fend = 0
      if (srceof) fend = 1
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : rstrct()
C*     this routine resets the record type (Std, Include, Macro, etc.).
C*
C*    PARAMETERS
C*       INPUT  : none
C*       OUTPUT : none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine rstrct
 
      include 'com8a.com'
c
      irctyp = 0
      return
      end
