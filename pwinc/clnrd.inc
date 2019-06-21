c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        clnrd.inc , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/12/15 , 17:38:49
c
      common /kcln  / VNMAJ , VAMAJ , VNMIN , VAMIN , NNMAJ , NNMIN ,
     -                CNMAJ , CAMAJ , CNMIN , CAMIN , CNNMAJ, CNNMIN,
     -                LSTIRC, LSTPTC, MRECS , MRECE , MDEF  , JMAC  ,
     -                JMNP  , MCMDNC, INCLNO, INCLIX, INCLFL, INCLNC,
     -                INCLCO, INCLPT, INCLMU
c
      integer*4 VNMAJ(14),VAMAJ(14),VNMIN(58),VAMIN(58),NNMAJ,NNMIN,
     -          CNMAJ(10),CAMAJ(10),CNMIN(42),CAMIN(42),CNNMAJ,
     -          CNNMIN,LSTIRC,LSTPTC,MRECS,MRECE,MDEF(50),JMAC,JMNP,
     -          MCMDNC,INCLNO,INCLIX(2,20),INCLFL,INCLNC(20),INCLCO,
     -          INCLPT(2),INCLMU(20)
c
      common /gcln  / RCLBUF,PNTVEC,MRDEF
c
      real*8 RCLBUF(404),PNTVEC(6),MRDEF(50)
c
      integer*2 ICLBUF(1616)
      integer*4 JCLBUF(808)
      character*3232 LCLBUF
c
      equivalence (RCLBUF,ICLBUF,JCLBUF,LCLBUF)
c
      common /ccln  / MPARM , MCMD  , INCLNM
c
      character*8 MPARM(8)
      character*80 MCMD
      character*132 INCLNM(20)
c
      common /cbfcom/ PTL, JCIRBF, ICNT, NREC, MULT, CIRTYP, ICYACT
      real*8 PTL(6)
      integer*4 JCIRBF(3000), ICNT, NREC, MULT, CIRTYP, ICYACT
