c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        post.inc , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        12/09/13 , 11:11:50
c
      common /kposmp/ KPOSMP
c
      integer*4 KPOSMP(6000)
c
      common /posmap/ POSMAP
c
      real*8 POSMAP(6000)
c
      common /cposmp/ CPOSMP
c
      character*1 CPOSMP(16000)
c
      common /tlccom/ TLNAME
c
      character*66 TLNAME(120)
c
      common /bsplinr/ PTVEBP, BSPCRV, BSPTPR, BSPMAP
      common /bsplink/ INVBP, KBSPMP
      common /cldat/ RCLDAT,RCLPT,COMBUF 
      common /clexp/ CLFWD,CLPTE1,CLPTE2,CLPTE3,CLPTE4 
c
      integer*4 INVBP(1000),KBSPMP(100)
      real*8 PTVEBP(10,1000), BSPCRV(3,3000), BSPTPR(3010),
     -       BSPMAP(50),RCLDAT(420),CLFWD(60),CLPTE1(60),
     -       CLPTE2(60),CLPTE3(60),CLPTE4(60),RCLPT(240),
     -       COMBUF(240)
