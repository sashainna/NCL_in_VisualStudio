C*********************************************************************
C*    NAME         :  wrtpp.f
C*       CONTAINS:
C*         wrtpp
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       wrtpp.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:55
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine wrtpp (cmsg,knc,irecno,jtyp,insm)
c*       this routine writes a record to the
c*       part program source file
c*
C*    PARAMETERS
C*       INPUT  :
c*          cmsg     =  the array from which the data is to be written
c*          knc      =  Number of characters in 'cmsg'.
c*          irecno   =  the number of the record to be written
c*          jtyp     =  Type of record to write.
c*                         0 = Normal
c*                         1 = INCLUDE
c*                         2 = INCERR*
c*                         3 = INCFILE
c*                         4 = READIT*
c*                         5 = READINC
c*          insm     =  0 = Overwrite mode, 1 = Insert Mode.
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine wrtpp (cmsg,knc,irecno,jtyp,insm)
 
      include 'com8a.com'
 
      character*(*) cmsg
      integer*2 jtyp,insm
      integer*4 irecno,ioerr,nclf_putsrc,knc
 
      if (insm .eq. 1) ifl4(1) = ifl4(1) + 1
      if (irecno.ge.ifl4(1)) ifl4(1)=irecno+1
cc      if (lincld) ktyp = 1
cc      if (lincfi) ktyp = 3
c
c...exclude the first record which is a header
c
      ioerr = nclf_putsrc(irecno,cmsg,knc,jtyp,insm)
99999 return
      end
