C*********************************************************************
C*    NAME         : menu.com 
C*       CONTAINS:
C*    COPYRIGHT 1992 (c) NCCS  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       menu.com , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:07:31
C********************************************************************/
C
c
      common /kmenu / NESC  , IESCPT, ESCBUF
c
      integer*4 NESC, ESCBUF(5), IESCPT
