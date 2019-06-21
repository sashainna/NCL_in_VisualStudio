C*********************************************************************
C*    NAME         :  convtk.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       convtk.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:44
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine convtk (xcoord,ycoord,inchr)
C*   purpose of program: tektronix conversion routines               
C*                                                                   
C*    PARAMETERS   
C*       INPUT  : 
C*          xcoord - (integer*2) contains convertes x co-ordinate 
C*          ycoord - (integer*2) contains converted y co-ordinate
C*          inchr - (character*5) contains input characters     
C*                                left justified               
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine convtk (xcoord,ycoord,inchr)

      include 'com4a.com'

      character*5 inchr
      integer*2 xcoord,ycoord,hi,lo,xextra,yextra
      integer   lchr(5)


c **********************************************************************
c          load up char equivalent array from input char array
c **********************************************************************
c
      do 10 i=1,5
         lchr(i) = ichar(inchr(i:i))
10    continue
c
c            y co-ordinate conversion
      hi=lchr(1)-32
      lo=lchr(3)-32
      yextra=lchr(2)/4-8
      ycoord=hi*128+lo*4+yextra

c            x co-ordinate conversion
      hi=lchr(4)-32
      lo=lchr(5)-32
      xextra=lchr(2)-(32+yextra)
      xcoord=hi*128+lo*4+xextra

      return
      end
