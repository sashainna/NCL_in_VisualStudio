C*********************************************************************
C*    NAME         :gtrtnm.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       gtrtnm.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:11
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtrtnm(pgmname)
C*       Return part program name without extension.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine gtrtnm(pgmnam,nco)

      include 'com4a.com'
 
      character*(MAX_PATH) pgmnam
      integer*2 i,ifn,j
      integer*4 nco,strlen1
 
      nco = strlen1(defnam)
      pgmnam(1:nco) = defnam(1:nco)
c                            remove extension, if any
      i=index(pgmnam(1:nco),']')
      ifn=index(pgmnam(i+1:nco),'.')
      if (ifn.gt.0) then
          pgmnam(ifn+i:nco) = ' '
          nco = ifn + i - 1
      endif
      return
      end
