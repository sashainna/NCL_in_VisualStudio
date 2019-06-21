c
c     Filename postworks_nt.inc
c     Variable for PostWorks
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        postworks_nt.inc , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 08:47:27
c

        common UU_BATCH
        common pwMainDlg , pwWndMain, pwFtxt, pwOpt, pwMtxt
		common Save_optint, save_option
		common wait_csr, arror_csr
		common SICLF,SIOPFL, SLOPFL, SOPTOVR
		common SPREPT, SPRENUM
		common SMCNSRC,SMAXWRN, SMAXPER, SMAXFAT, SPSTNAM, SUSROVR

        Integer*4 UU_BATCH
		integer*4 pwMainDlg , pwWndMain, pwFtxt, pwOpt, pwMtxt
      integer*4 save_optint
		character*200 Save_option
		integer*4 wait_csr, arror_csr
		integer*4 SOPTOVR(20), SPREPT, SPRENUM, SUSROVR(50)
		integer*4 SICLF,SIOPFL(20),SMCNSRC,SMAXWRN, SMAXPER, SMAXFAT
c
      character*40 SPSTNAM
		character*256 SLOPFL(20)
