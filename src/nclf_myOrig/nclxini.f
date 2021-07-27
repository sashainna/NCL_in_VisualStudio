c*********************************************************************
c*    NAME         :  nclxini.f
c*       CONTAINS:
c*				nclxini  comini  nclxpw
c*				subroutine nclxpw(cpw,kerr)
c*    COPYRIGHT 1997 (c) Numerical Control Computer Sciences Inc.
c*              All Rights Reserved.
c*    MODULE NAME AND RELEASE LEVEL
c*       nclxini.f , 25.1
c*    DATE AND TIME OF LAST  MODIFICATION
c*       04/29/15 , 15:10:20
c*********************************************************************
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nclxini (mmfl,kver)
c*       sets up all required start up variables
c*       and fields at initial startup and at restart time for
c*       the OML.
C*    PARAMETERS
C*       INPUT  :
C*          mmflg   =  0 = inches, 1 = millimeters.
C*          kver    =  NCL Version number to use in motion calculations.
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
c
      subroutine nclxini(mmflg,gver)
c
      include 'com.com'
      include 'comgt.com'
      include 'mocom.com'
c
      integer*4 mmflg
c
      real*8 gver
c
      integer*2 ksn(4)
c
      real*4 bsn(2)
      real*8 asn
c
      equivalence (asn,bsn,ksn)
c
c...Set certain array sizes
c
      NUMSC = 300
      NUMIFL = 500
      NUMLFL = 200
      NUMDTB = 200
      NUMIF4 = 200
c
c...Zero out common arrays
c
      call comini
c
c...Initialize logicals
c
      ldssvc = .false.
      lcssvc = .false.
      lexpcl = .true.
c
c...Initialize scratch file
c
      iscrln(1) = 13
      iscrln(2) = 14
      iscrln(3) = 15
      iscrln(4) = 16
      call scrini
c
c...   Initialize tool axis to 0,0,1
c
      sc(6) = 1.0d0
      FROMSV(6) = 1.0d0
c
c...Set machining library flag.
c
      ifl(330) = 1
c
c...Set CL output on
c
      ifl(69) = 1
c
c...Set MULTAX/ON
c
      ifl(82) = 1
c
c...Set AUTOST
c
      ifl(89) = 1
      autost = .true.
c
c...Set Circular Interpolation (on for now)
c
      ifl(94) = 1
      ifl(144) = 1
c
c...Set Version.
c
      sc(169) = gver
c
c...set autouv. Dassault MFGNC251
c
      auvset = gver .ge. 9.149
c
c...Initialize MAXDP
c
      sc(54) = 4.        ! DPMAX
      if (mmflg .eq. 1) sc(54) = 100.
      sc(105) = sc(54)
      ksn(1) = 10.       ! Maximum tries for AUTODP
      ksn(2) = 0
      bsn(2) = .01       ! DPMIN
      if (mmflg .eq. 1) bsn(2) = .01 * 25.
      sc(162) = asn
      sc(214) = sc(162)
      sc(80) = 5.        ! MAXANG
      sc(201) = sc(80)
c
c...Initialize units & TOLER
c
      if (mmflg .eq. 1) then
        ifl(362) = 1
        sc(27) = .025
        sc(167) = .025
        sc(168) = .0001
      else
        ifl(362) = 0
        sc(27) = .001
        sc(167) = .001
        sc(168) = .0001
      endif
      sc(91) = sc(27) * 2.
      sc(92) = dcos(3.141592654d0/180.d0)
c
c...Initialize NUMPTS
c
      ifl(91) = 100
      ifl(368) = ifl(91)
c
c...Initialize GOUGCK
c
      ifl(6) = 0
      idsgck = 0
      icsgck = 0
c
c...Free cvonsf list.
c
      call ncl_cvonsf_free
c
c...Initialize pocket list.
c
      call pklini
c
c...Initialize hex constants
c...Hex 0000 = 0
c...Hex ffff = 65535
c...Hex 00ff = 255
c...Hex ff00 = 65280
c...Hex 0ff0 = 4080
c
      x0000 = x'0000'
      xffff = x'ffff'
      xff00 = x'ff00'
      x00ff = x'00ff'
      xf00f = x'f00f'
      x0ff0 = x'0ff0'
c
c...End of routine
c
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine comini ()
c*       Zeros out the common arrays.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine comini
c
      include 'com.com'
      include 'cmm.com'
      include 'comgt.com'
      include 'cutter.com'
      include 'drvcom.com'
      include 'dspcom.com'
      include 'fillet.com'
      include 'gidcom.com'
      include 'ifcom.com'
      include 'mocom.com'
      include 'prfcom.com'
      include 'rrdm.com'
      include 'status.com'
      include 'suvcom.com'
      include 'vaxvoc.com'
      include 'wrksys.com'
c
      integer*4 i
c
c......COM.COM
c
      do 10 i=1,N_INCLMP,1
   10     INCLMP(i) = 0
c
      do 20 i=1,N_KNCLMP,1
   20     KNCLMP(i) = 0
c
      do 30 i=1,N_RNCLMP,1
   30     RNCLMP(i) = 0.
c
      do 40 i=1,N_GNCLMP,1
   40     GNCLMP(i) = 0.
c
      do 50 i=1,N_CNCLMP,1
   50     CNCLMP(i) = ' '
c
      do 60 i=1,32,1
   60     PTRS(i) = 0
c
c......CMM.COM
c
      do 110 i=1,N_ICMCOM,1
  110     ICMCOM(i) = 0
c
      do 120 i=1,N_GCMCOM,1
  120     GCMCOM(i) = 0.
c
      do 130 i=1,N_CCMCOM,1
  130     CCMCOM(i) = ' '
c
c......COMGT.COM
c
      do 210 i=1,N_MOTMAP,1
  210     MOTMAP(i) = 0
c
c......CUTTER.COM
c
      do 310 i=1,N_ICTCOM,1
  310     ICTCOM(i) = 0
c
      do 320 i=1,N_KCTCOM,1
  320     KCTCOM(i) = 0
c
      do 330 i=1,N_GCTCOM,1
  330     GCTCOM(i) = 0.
c
      do 340 i=1,N_CCTCOM,1
  340     CCTCOM(i) = ' '
c
c......DRVCOM.COM
c
      do 410 i=1,N_GSFCOM,1
  410     GSFCOM(i) = 0.
c
c......DSPCOM.COM
c
      do 510 i=1,150,1
  510     w(i) = 0.
c
c......FILLET.COM
c
      do 610 i=1,N_IFILET,1
  610     IFILET(i) = 0.
c
      do 620 i=1,N_KFILET,1
  620     KFILET(i) = 0.
c
      do 630 i=1,N_GFILET,1
  630     GFILET(i) = 0.
c
c......GIDCOM.COM
c
      do 710 i=1,N_IGIDCM,1
  710     IGIDCM(i) = 0
c
      do 720 i=1,N_RGIDCM,1
  720     RGIDCM(i) = 0.
c
      do 730 i=1,N_GGIDCM,1
  730     GGIDCM(i) = 0
c
c......IFCOM.COM
c
      do 810 i=1,N_IIFCOM,1
  810     IIFCOM(i) = 0
c
      do 820 i=1,N_KIFCOM,1
  820     KIFCOM(i) = 0
c
c......MOCOM.COM
c
      do 910 i=1,N_MOCCOM,1
  910     MOCCOM(i) = 0.
c
c......PRFCOM.COM
c
      do 1010 i=1,N_IPROF,1
 1010     IPROF(i) = 0
c
      do 1020 i=1,N_KPROF,1
 1020     KPROF(i) = 0
c
      do 1030 i=1,N_RPROF,1
 1030     RPROF(i) = 0.
c
      do 1040 i=1,N_GPROF,1
 1040     GPROF(i) = 0.
c
      do 1050 i=1,N_CPROF,1
 1050     CPROF(i) = ' '
c
c......RRDM.COM
c
      do 1110 i=1,N_IRRDM,1
 1110     IRRDM(i) = 0
c
      do 1120 i=1,N_KRRDM,1
 1120     KRRDM(i) = 0
c
      do 1130 i=1,N_RRRDM,1
 1130     RRRDM(i) = 0.
c
      do 1140 i=1,N_GRRDM,1
 1140     GRRDM(i) = 0.
c
      do 1150 i=1,N_CRRDM,1
 1150     CRRDM(i) = ' '
c
c......STATUS.COM
c
      do 1210 i=1,N_ISTMAP,1
 1210     ISTMAP(i) = 0
c
c......SUVCOM.COM
c
      do 1310 i=1,N_ISUVCM,1
 1310     ISUVCM(i) = 0
c
      do 1320 i=1,N_KSUVCM,1
 1320     KSUVCM(i) = 0
c
      do 1330 i=1,N_RSUVCM,1
 1330     RSUVCM(i) = 0.
c
c......VAXVOC.COM
c
      do 1410 i=1,N_VOC,1
          vvcnum(i) = 0
 1410     vvcnam(i) = ' '
c
c......WRKSYS.COM
c
      do 1510 i=1,N_WRKSYS,1
 1510     WRKSYS(i) = 0.
c
      do 1520 i=1,N_CWKSYS,1
 1520     CWKSYS(i) = ' '
c
c...End of routine
c
 8000 return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nclxpw (cpw,kerr)
c*       Validates the provided password against a list of registered
c*       users.
C*    PARAMETERS
C*       INPUT  :
C*          cpw     =  Password provided by calling routine.
C*       OUTPUT :
C*          kerr    =  Returns positive number of user when validated.
c*                     Returns 0 on error.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine nclxpw(cpw,kerr)
c
      PARAMETER (NUMUSR = 2)
c
      byte cpw(20)
c
      integer*4 kerr
c
      integer*4 ist(4),ien(4),PWD(4,NUMUSR),ipw(4)
c
      byte lc(20)
      character*4 lnum
      character*20 lpw
      equivalence (lpw,lc)
c
      data ist /1,6,11,16/, ien /4,9,14,19/
c
      data PWD /20191, 27532, 13855, 28750,
     1          21580, 25657, 6478, 11518/
c
c...Initialize routine
c
      kerr   = 0
      do 100 i=1,19,1
          lc(i) = cpw(i)
  100 continue
c
c...Convert password to numeric
c
      do 150 i=1,4,1
          lnum = lpw(ist(i):ien(i))
          read (lnum,10,err=8000) ipw(i)
  150 continue
   10 format (i4)
c
c...Verify password
c
      do 300 kerr=1,NUMUSR,1
          do 200 j=1,4,1
              if (ipw(j)*3-365 .ne. PWD(j,kerr)) go to 300
  200     continue
          go to 8000
  300 continue
c
c...Password did not validate
c
      kerr   = 0
c
c...End of routine
c
 8000 return
      end
