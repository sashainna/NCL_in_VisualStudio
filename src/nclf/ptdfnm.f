C*********************************************************************
C*    NAME         :ptdfnm.f
C*       CONTAINS:
C*    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ptdfnm.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:30
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptdfnm(fname,nci)
C*       Save the default file name for pp, cl & as file opens.
C*    PARAMETERS   
C*       INPUT  : 
C*          fname   = name to save.
C*          nci     = Number of chars in 'fname'.
C*       OUTPUT :  
C*          fname   = Filename with extension.
C*          nci     = Number of chars in 'fname'.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine ptdfnm(fname,nci)

      include 'com4a.com'
      character*(MAX_PATH) fname
      integer*4 nci
      integer*2 i,ifn
C RAH: in case this is called with the file extension,
c                            remove extension, if any
      i=index(fname(1:nci),']')
      ifn=index(fname(i+1:nci),'.')
      if (ifn.gt.0) then
          fname(ifn+i:nci) = ' '
          nci = ifn + i - 1
      endif
      if (nci .lt. 0) nci = 0

      defnam = fname

      return
      end
