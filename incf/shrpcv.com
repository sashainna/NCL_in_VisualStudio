c*********************************************************************
c**
c**    NAME         :  shrpcv.com
c**
c**    CONTAINS:  
c**      The common area for driving sharp-cornered curves
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       shrpcv.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:33
c*********************************************************************
 
      common /lsharp/ ishrp,zercor,wedged
      integer*2 ishrp,zercor
      logical wedged

      common /gsharp/fdshrp,ushrp
      real*4 fdshrp,ushrp

      common /vsharp/ptshrp,zshrp,vxshrp
      real*4 ptshrp(3),zshrp(3),vxshrp(3)
