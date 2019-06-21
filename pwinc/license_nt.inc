c
c     Filename license_nt.inc
c      variables for nccs_license window
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        license_nt.inc , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:24
c
c
c...Please don't put repnum, addnum, BATCHFILE
c...in the seperate header file, for some reason
c...it will affect ghMainDlg , ghWndMain value
c...and get in problem
c...Yurong 1/30/00
c
      common ghMainDlg , ghWndMain
      integer*4 ghMainDlg , ghWndMain
      common repnum, addnum, BATCHFILE
      integer*4 repnum, addnum, BATCHFILE
