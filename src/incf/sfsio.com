c*********************************************************************
c**
c**    NAME         :  sfsio.com 
c**
c**    CONTAINS:
c**     Common block of data used by sfsio and declcv Fortran routines.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       sfsio.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:33
c*********************************************************************
c
      common /sfscom/ sclvar_lab,sclvar_inx,sclvar_def
c
      character*64 sclvar_lab
      integer*4 sclvar_inx
      logical sclvar_def
