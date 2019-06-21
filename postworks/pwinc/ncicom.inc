c
c     COPYRIGHT 2009 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        ncicom.inc , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:24
c
      common /NCICOM/ INBUF , ICPT  , ICR   , IS1   , IS4
c
      integer*2 INBUF(964)
      integer*4 ICPT,ICR,IS1,IS4
c
      integer*2 IOBUF(960),MXC
      integer*4 JBUF(480)
c
      real*8 ROBUF(240)
c
      character*1920 TOBUF
c
      equivalence (INBUF(5),ROBUF,IOBUF,TOBUF), (INBUF(1),MXC,JBUF)
c
