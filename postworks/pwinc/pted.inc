c     File: Pted.inc
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			pted.inc , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			09/11/13 , 12:58:25
c        
c
      common /pted/ MINTER, TBLOCK, PTEDSP, TBLAST, MILAST, PCNV_TYPE,
     1              SMSTOP, SMSTAT, DELSTK, NDELST
c
      integer*4 MINTER,TBLOCK,PTEDSP,TBLAST,MILAST,PCNV_TYPE,SMSTOP,
     1          SMSTAT,DELSTK(50),NDELST
c
      common /pconst/ PBLK_DWELL  , PBLK_FOFFSET, PBLK_GOHOME ,
     1                PBLK_MOTION , PBLK_TOFFSET, PBLK_UNKNOWN,
     2                PMOT_CIRCUL , PMOT_CYCLE  , PMOT_LINEAR ,
     3                PMOT_ROTARY , PBLK_POSTN  , PMOT_CYCOFF ,
     4                PBLK_AXSIZE , PCNV_APTSRC , PCNV_SIMUL  ,
     5                PMOT_CIR3AX , PBLK_XFORM  , PCNV_CONVERT,
     6                PMOT_CIRCEN
c
      integer*4 PBLK_DWELL,PBLK_FOFFSET,PBLK_GOHOME,PBLK_MOTION,
     1          PBLK_TOFFSET,PBLK_UNKNOWN,PMOT_CIRCUL,PMOT_CYCLE,
     2          PMOT_LINEAR,PMOT_ROTARY,PBLK_POSTN,PMOT_CYCOFF,
     3          PBLK_AXSIZE,PCNV_APTSRC,PCNV_SIMUL,PMOT_CIR3AX,
     4          PBLK_XFORM,PCNV_CONVERT,PMOT_CIRCEN
