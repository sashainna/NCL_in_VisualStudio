c*********************************************************************
c    NAME:  PtdfFunc.f
c    Description:
c            Fortran functions for Pted.
c    CONTAINS:
c      ptd_init        ptd_savdef      ptdf_loddef    ptd_cparse
c      parsevalid      ptd_getseq      ptd_rmopt      ptdrmopt
c      ptdf_stoval     ptd_initregtab  ptd_cnvreg     ptd_icnvgmcode
c      ptd_ocnvgmcode  ptd_aptsrc      ptdf_mactoapt  ptdf_ftoc
c      ptdf_convrapto  ptdf_ledout  ptdf_seqctl
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c    MODULE NAME AND RELEASE LEVEL
c       ptdffunc.f , 24.2
c    DATE AND TIME OF LAST  MODIFICATION
c       12/09/13 , 11:54:07
*********************************************************************/
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_init
c
c   FUNCTION:  This routine initializes pted fortran commons.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine ptd_init
c
      include 'pted.inc'
      include 'ptedpost.inc'
      include 'menu.inc'
      include 'compile.inc'

      equivalence (AXSOUT,OPOSMAP(1340))
      equivalence (AXSSTO,OPOSMAP(1425))
      real*8 AXSOUT(10), AXSSTO(10)

      integer*4 ierr, i
      character*80 msg

      call pted_version
      call init
      call pstini
      call ptdf_copy_mdf (1)
      call loddat (0, msg, ierr)
c
      PBLK_UNKNOWN = 0
      PBLK_MOTION  = 1
      PBLK_DWELL   = 2
      PBLK_FOFFSET = 3
      PBLK_GOHOME  = 4
      PBLK_POSTN   = 5
      PBLK_TOFFSET = 6
      PBLK_AXSIZE  = 7
      PBLK_XFORM   = 8
c
      PMOT_LINEAR = 1
      PMOT_CIRCUL = 2
      PMOT_ROTARY = 3
      PMOT_CYCLE  = 4
      PMOT_CYCOFF = 5
      PMOT_CIR3AX = 6
      PMOT_CIRCEN = 7
c
      MINTER = PMOT_LINEAR
      TBLOCK = PBLK_UNKNOWN
c
      PCNV_APTSRC  = 1
      PCNV_SIMUL   = 2
      PCNV_CONVERT = 3
      PTD_RDBUF = 0
c
      do 100 i = 1, 10, 1
          AXSSTO(i) = AXSOUT(i)
  100 continue
c
      MAXTOK = 100
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_savdef
c
c   FUNCTION:  This routine saves default variables used by
c              aptsrc convert.
c
c   INPUT:  none
c
c   OUTPUT: none
c***********************************************************************
c
      subroutine ptd_savdef

      include 'post.inc'

      equivalence (AXSSTO,POSMAP(1425)), (AXSSAV ,POSMAP(4423))
c
      real*8 AXSSTO(10),AXSSAV(10)

      integer*4 i

      do 100 i=1,10,1
          AXSSAV(i) = AXSSTO(i)
  100 continue

      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:   loddef
c
c   FUNCTION:  This routine initializes variables used by
c              aptsrc convert.
c
c   INPUT:  none
c
c   OUTPUT: none
c***********************************************************************
c
      subroutine ptdf_loddef

      include 'pted.inc'
      include 'post.inc'
      include 'menu.inc'

      equivalence (ISN   ,KPOSMP(0001)), (ITYPE ,KPOSMP(0003))
      equivalence (ISUBT ,KPOSMP(0004)), (MXCL  ,KPOSMP(0005))
      equivalence (MULTAX,KPOSMP(0056)), (ICUCHG,KPOSMP(0068))
      equivalence (IOPSKP,KPOSMP(0092)), (ICYCSW,KPOSMP(0271))
      equivalence (IRTOUT,KPOSMP(0813)), (XFMFL ,KPOSMP(0921))
      equivalence (IRTACT,KPOSMP(1256)), (IRTDEF,KPOSMP(1485))
      equivalence (ITP   ,KPOSMP(1801)), (IRAP  ,KPOSMP(3199))
c
      integer*4 ISN,ITYPE,ISUBT,MXCL,IRTOUT(4),IRTACT(2),ITP,
     1          XFMFL(5),IOPSKP,MULTAX,ICYCSW(5),IRAP,ICUCHG,IRTDEF
c
      equivalence (AXSOUT,POSMAP(1340)), (VECSAV,POSMAP(1372))
      equivalence (AXSSTO,POSMAP(1425)), (FEED  ,POSMAP(3547))
      equivalence (SPIVEC,POSMAP(3583)), (AXSSAV,POSMAP(4423))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 AXSOUT(10),AXSSAV(10),VECSAV(3),SPIVEC(3),AXSSTO(10),
     1       FEED(4),ROTANG(20,2),ROTSTO(20,2)
c
      integer*4 i, kfl
c
      byte cfil1(MAX_PATH),cfil2(MAX_PATH)
      equivalence (cfil1,LCMPFI)
c
      do i=1,MAX_PATH,1
        cfil2(i) = cfil1(i)
      enddo
      kfl = 1
c
c...Loading the MDF file here
c...Will wipe out any changes the user made
c...Such as tool lengths
c...Bobby  -  3/25/05
c
cc      call ptd_load_mdf(cfil2,kfl,cmsg1,cmsg2,kerr)
c
c
c...Initialize certain variables with
c...each APT Source conversion
c
      ICYCSW(1) = 0
      IOPSKP = 2
      IRAP   = 0
      MULTAX = 0
      FEED(1) = 0.
c
      do 10 i=1,10
          AXSOUT(i) = AXSSAV(i)
          AXSSTO(i) = AXSSAV(i)
   10 continue
c
      IRTACT(1) = IRTDEF
      do 20 i=1,IRTDEF
          IRTOUT(i) = 1
   20 continue
c
      do 30 i=1,3,1
          VECSAV(i) = SPIVEC(i)
   30 continue
c
      PTEDSP = 0
      call rscini
      call rotini(i)
      call cpyrot (ROTSTO,ROTANG)
c
      ITP    = 1
      XFMFL(4) = 0
      XFMFL(5) = 0
c
      ICUCHG = 0
      NCLVER = 9.6
c
c...Call G$MAIN Macro
c
      ISN    = 1
      ITYPE  = 2000
      MXCL   = 0
      kfl    = 3999
      call ptdf_mactoapt (kfl)
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_cparse(instr, knc, mend, kerr)
c
c   FUNCTION:  This routine parse a non-block/block string and parse the valid N
c              character and string
c
c   INPUT:  instr:  C*n  D1  - string to parse
c           knc     I*4  D1  - length of instr
c
c   OUTPUT: mend:   I*4  D1  - end valid N/C character
c           kerr    I*4  D1  - non_zero iff error
c***********************************************************************
c NOT used anymore
      subroutine ptd_cparse(instr, knc, mend, kerr)
c
      integer*4 knc, mend, kerr
C WNT-START
      byte instr(256)
c
      integer*4 nc
      character*256 cinstr
C
      call pwdbtc(instr, cinstr, nc)
      call parsevalid(cinstr, knc, mend, kerr)
C WNT-END
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     character*256 instr
C     call parsevalid(instr, knc, mend, kerr)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  parsevalid(instr, knc, mend, kerr)
c
c   FUNCTION:  This routine parse a string and parse the valid N/C
c              character and string
c
c   INPUT:  instr:  C*n  D1  - string to parse
c           knc     I*4  D1  - length of instr
c
c   OUTPUT: mend:   I*4  D1  - end valid N/C character
c           kerr    I*4  D1  - non_zero iff error
c
c***********************************************************************
c
      subroutine parsevalid(instr, knc, mend, kerr)

      include 'post.inc'
c
      integer*4 mend, knc, kerr

      character*(*) instr
      equivalence (NCBMSG,KPOSMP(0843))
      equivalence (NCEMSG,KPOSMP(0844)), (NOPSKP,KPOSMP(1077))
c
      integer*4 NCBMSG,NCEMSG,NOPSKP
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      equivalence (MSGST ,CPOSMP(0976)), (LEDCHR,CPOSMP(0986))
      equivalence (TABCHR,CPOSMP(0987)), (MSGEN ,CPOSMP(1500))
      equivalence (LOPSKP,CPOSMP(1648))
c
      character*1 LEDCHR,TABCHR
      character*5 LOPSKP
      character*10 MSGST,MSGEN

      equivalence (LRWSTP,CPOSMP(2201)), (LEOT,CPOSMP(2211))
      equivalence (LBOT  ,CPOSMP(2515))
c
      character*10 LRWSTP
      character*20 LEOT,LBOT
      equivalence (NRWSTP,KPOSMP(1104))
      equivalence (NEOT  ,KPOSMP(1106))
      equivalence (NBOT  ,KPOSMP(1136))
c
      integer*4 NRWSTP,NEOT,NBOT

      integer*4 j, ist
      mend = 0
      ist = 0
      kerr = 0
  100 ist = ist + 1
      if (ist .gt. knc) then
          if (mend.eq.0) then
              kerr = 1
          else
              kerr = 0
          endif
          return
      endif
c
c...Check for Message block
c
      if ((ist+NCBMSG-1).gt.knc) goto 150
      if (instr(ist:ist+NCBMSG-1) .eq. MSGST(1:NCBMSG))
     -                              go to 500
c
c...check space
c
  150 if (instr(ist:ist).eq.' ') then
         mend = mend + 1
         goto 100
      endif
c
c...check for start of tape characters
c
      if (NBOT.eq.0) goto 200
      if (instr(ist:ist+NBOT-1) .eq. LBOT(1:NBOT)) then
         mend = ist + NBOT-1
         ist = ist + NBOT-1
         goto 100
      endif
c
c...check for end of tape characters
c
  200 if (NEOT.eq.0) goto 300
      if (instr(ist:ist+NEOT-1) .eq. LEOT(1:NEOT)) then
         mend = ist + NEOT-1
         ist = ist + NEOT-1
         goto 100
      endif

c
c...check for Rewind stop characters
c
  300 if (NRWSTP.eq.0) goto 400
      if (instr(ist:ist+NRWSTP-1) .eq. LRWSTP(1:NRWSTP)) then
         mend = ist + NRWSTP-1
         ist = ist + NRWSTP-1
         goto 100
      endif
c
c...check for Leader characters
c
  400 if (instr(ist:ist).eq.LEDCHR) then
         mend = mend + 1
         goto 100
      endif
c
c...till here and no valid, error
c
      if (mend.eq.0) kerr = 1
      return
  500 do 600 j = ist+NCBMSG, knc-NCEMSG+1
         if (instr(j:j+NCEMSG-1) .eq. MSGEN(1:NCEMSG)) then
             ist = j + NCEMSG-1
             mend = ist
             goto 100
         endif
  600 continue
      ist = j
      goto 100
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_getseq(regnum)
c
c   FUNCTION:  This routine returns the current value of the
c              sequence number register.
c
c   INPUT:  none
c
c   OUTPUT: regnum  I*4  D1  -  Sequence number
c
c***********************************************************************
c
      subroutine ptd_getseq(regnum)

      include 'post.inc'
      integer*4 regnum
c
c...Sequence number register
c
      equivalence (SEQCOD,KPOSMP(0847))
      integer*4 SEQCOD

      regnum = SEQCOD
      return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  ptd_rmopt(cstr)
c
c   FUNCTION:  This routine remove optional skip character from
c              a block string
c
c   INPUT:  cstr:   string to parse
c            knc:   number of chars in cstr
c
c   OUTPUT: cstr:
c            knc:   number of chars in cstr
c
c***********************************************************************
c Not used anymore
      subroutine  ptd_rmopt(cstr, knc)
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     character*256 cstr
C     integer*4 knc
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      integer*4 knc
      byte cstr(256)
      character*256 dstr
      call pwdbtc(cstr, dstr, knc)
      call ptdrmopt(dstr, knc)
      call pwdctb(dstr, cstr)
      return
C WNT-END
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     call ptdrmopt(cstr, knc)
C     return
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
      end

c***********************************************************************
c
c   SUBROUTINE:  ptdrmopt(cstr, knc)
c
c   FUNCTION:  This routine remove optional skip character from
c              a block string
c
c   INPUT:  cstr:   string to parse
c            knc:   number of chars in cstr
c
c   OUTPUT: cstr:
c            knc:   number of chars in cstr
c
c***********************************************************************
c Not used anymore
      subroutine  ptdrmopt(cstr, knc)
c
      include 'post.inc'
      include 'menu.inc'
      character*(*) cstr
      integer*4 knc

      equivalence (LOPSKP,CPOSMP(1648)), (NOPSKP,KPOSMP(1077))
      integer*4 NOPSKP
      character*5 LOPSKP
      character*256 tempbuf
c
      if (NOPSKP.gt.knc) return
      if (cstr(1:NOPSKP).eq.LOPSKP(1:NOPSKP)) then
          tempbuf = cstr(NOPSKP+1:)
          knc = knc - NOPSKP
          cstr(1:knc) = tempbuf(1:knc)
      endif
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  ptdf_stoval(regarray, valarray, tnum)
c
c   FUNCTION: This function store the value into the certain register
c
c
c   INPUT:  regarray:  array contain register
c           valarray:  array contain value
c           tnum:    number of values need store
c
c   OUTPUT: None
c
c***********************************************************************
c
      subroutine  ptdf_stoval(regarray, valarray, tnum)
c
      include 'post.inc'
      include 'menu.inc'
c
      integer*4 regarray(MAXFMT),tnum
c
      real*8 valarray(MAXFMT)
c
      equivalence (REGSTO, POSMAP(1032)), (REGVAL,POSMAP(5000))
c
      real*8 REGSTO(MAXFMT), REGVAL(MAXFMT)
c
      integer i, regnum

      do 100 i=1, tnum, 1
          regnum = regarray(i)
          REGSTO(regnum) = valarray(i)
          REGVAL(regnum) = valarray(i)
  100 continue
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  ptd_initregtab ()
c
c   FUNCTION: This function initializes the register lookup tables.
c
c   INPUT:  None
c
c   OUTPUT: None
c
c***********************************************************************
c
      subroutine  ptd_initregtab
c
      include 'menu.inc'
      include 'post.inc'
      include 'ptedpost.inc'

      equivalence (ICYCCOD,KPOSMP(0211)),(OCYCCOD,OKPOSMP(0211))
      equivalence (ICYCREG,KPOSMP(0231)),(OCYCREG,OKPOSMP(0231))
      equivalence (IMOTREG,KPOSMP(0381)),(OMOTREG,OKPOSMP(0381))
      equivalence (ISEQCOD,KPOSMP(0847)),(OSEQCOD,OKPOSMP(0847))
      equivalence (IALNCOD,KPOSMP(0848)),(OALNCOD,OKPOSMP(0848))
      equivalence (IOPSTCD,KPOSMP(1075)),(OOPSTCD,OKPOSMP(1075))
      equivalence (ISTOPCD,KPOSMP(1076)),(OSTOPCD,OKPOSMP(1076))
      equivalence (IUNTCOD,KPOSMP(1078)),(OUNTCOD,OKPOSMP(1078))
      equivalence (IREWCOD,KPOSMP(1080)),(OREWCOD,OKPOSMP(1080))
      equivalence (IFINCOD,KPOSMP(1105)),(OFINCOD,OKPOSMP(1105))
      equivalence (IICSMRG,KPOSMP(1132)),(OICSMRG,OKPOSMP(1132))
      equivalence (IMOTCOD,KPOSMP(1240)),(OMOTCOD,OKPOSMP(1240))
      equivalence (IABSCOD,KPOSMP(1241)),(OABSCOD,OKPOSMP(1241))
      equivalence (IINCCOD,KPOSMP(1242)),(OINCCOD,OKPOSMP(1242))
      equivalence (ICLMPCD,KPOSMP(1307)),(OCLMPCD,OKPOSMP(1307))
      equivalence (ISLWFL ,KPOSMP(1701)),(OSLWFL ,OKPOSMP(1701))
      equivalence (ISLWCD ,KPOSMP(1706)),(OSLWCD ,OKPOSMP(1706))
      equivalence (ITLCCD ,KPOSMP(1839)),(OTLCCD ,OKPOSMP(1839))
      equivalence (ITLOCD ,KPOSMP(1860)),(OTLOCD ,OKPOSMP(1860))
      equivalence (REGBNC ,KPOSMP(2001))
      equivalence (FMTDES ,KPOSMP(2133))
      equivalence (ISPNDCD,KPOSMP(3105)),(OSPNDCD,OKPOSMP(3105))
      equivalence (ICOOLCD,KPOSMP(3128)),(OCOOLCD,OKPOSMP(3128))
      equivalence (ISPCOCD,KPOSMP(3132)),(OSPCOCD,OKPOSMP(3132))
      equivalence (ISPNSCD,KPOSMP(3144)),(OSPNSCD,OKPOSMP(3144))
      equivalence (IFEDCD, KPOSMP(3162)),(OFEDCD, OKPOSMP(3162))
      equivalence (IFEDMCD,KPOSMP(3170)),(OFEDMCD,OKPOSMP(3170))
      equivalence (NRAPCD ,KPOSMP(3190))
      equivalence (IRAPCD ,KPOSMP(3191)),(ORAPCD,OKPOSMP(3191))
      equivalence (IFEDOCD,KPOSMP(3217)),(OFEDOCD,OKPOSMP(3217))
      equivalence (ICUTCCD,KPOSMP(3271)),(OCUTCCD,OKPOSMP(3271))
      equivalence (IDELYCD,KPOSMP(3351)),(ODELYCD,OKPOSMP(3351))
      equivalence (IHOMCOD,KPOSMP(3361)),(OHOMCOD,OKPOSMP(3361))
      equivalence (REGENC ,KPOSMP(3522))
      equivalence (IPLNCOD,KPOSMP(4222)),(OPLNCOD,OKPOSMP(4222))

      integer*4 ICYCCOD(20), OCYCCOD(20), ICYCREG(20), OCYCREG(20)
      integer*4 IMOTREG(24), OMOTREG(24), REGBNC(MAXFMT),REGENC(MAXFMT)
      integer*4 ISEQCOD,OSEQCOD, IALNCOD,OALNCOD
      integer*4 IOPSTCD,OOPSTCD,ISTOPCD,OSTOPCD
      integer*4 IUNTCOD(2),OUNTCOD(2), IREWCOD,OREWCOD
      integer*4 IFINCOD,OFINCOD
      integer*4 IICSMRG,OICSMRG, IMOTCOD,OMOTCOD, IABSCOD,OABSCOD
      integer*4 IINCCOD,OINCCOD
      integer*4 ICLMPCD(20),OCLMPCD(20)
      integer*4 ISLWFL(5),OSLWFL(5),ISLWCD(10),OSLWCD(10)
      integer*4 ITLCCD(20),OTLCCD(20)
      integer*4 ITLOCD( 8),OTLOCD( 8)
      integer*4 ISPNDCD(4),OSPNDCD(4)
      integer*4 ICOOLCD(4),OCOOLCD(4)
      integer*4 ISPCOCD(6),OSPCOCD(6),ISPNSCD(2),OSPNSCD(2)
      integer*4 IFEDCD(8), OFEDCD(8), IFEDMCD(8), OFEDMCD(8)
      integer*4 NRAPCD,IRAPCD(5),ORAPCD(5)
      integer*4 IFEDOCD(2), OFEDOCD(2), ICUTCCD(10),OCUTCCD(10)
      integer*4 IDELYCD,ODELYCD
      integer*4 IHOMCOD(4),OHOMCOD(4)
      integer*4 IPLNCOD(4),OPLNCOD(4)
      integer*2 FMTDES(10,MAXFMT)
c
      equivalence (IDELYVL,POSMAP(0165)),(ODELYVL,OPOSMAP(0165))
      equivalence (IHOMCDV,POSMAP(0167)),(OHOMCDV,OPOSMAP(0167))
      equivalence (IOPSTVL,POSMAP(1168)),(OOPSTVL,OPOSMAP(1168))
      equivalence (ISTOPVL,POSMAP(1169)),(OSTOPVL,OPOSMAP(1169))
      equivalence (IUNTCDV,POSMAP(1170)),(OUNTCDV,OPOSMAP(1170))
      equivalence (IREWCDV,POSMAP(1172)),(OREWCDV,OPOSMAP(1172))
      equivalence (IFINCDV,POSMAP(1203)),(OFINCDV,OPOSMAP(1203))
      equivalence (IMOTCDV,POSMAP(1284)),(OMOTCDV,OPOSMAP(1284))
      equivalence (IABSCDV,POSMAP(1285)),(OABSCDV,OPOSMAP(1285))
      equivalence (IINCCDV,POSMAP(1286)),(OINCCDV,OPOSMAP(1286))
      equivalence (ICLMPVL,POSMAP(1454)),(OCLMPVL,OPOSMAP(1454))
      equivalence (IPLNCDV,POSMAP(2208)),(OPLNCDV,OPOSMAP(2208))
      equivalence (ICUTCVL,POSMAP(2410)),(OCUTCVL,OPOSMAP(2410))
      equivalence (ISLWVL, POSMAP(2456)),(OSLWVL, OPOSMAP(2456))
      equivalence (ICYCCDV,POSMAP(2901)),(OCYCCDV,OPOSMAP(2901))
      equivalence (IFEDOVL,POSMAP(3001)),(OFEDOVL,OPOSMAP(3001))
      equivalence (ISPNDVL,POSMAP(3007)),(OSPNDVL,OPOSMAP(3007))
      equivalence (ICOOLVL,POSMAP(3294)),(OCOOLVL,OPOSMAP(3294))
      equivalence (ISPCOVL,POSMAP(3298)),(OSPCOVL,OPOSMAP(3298))
      equivalence (IFEDMVL,POSMAP(3320)),(OFEDMVL,OPOSMAP(3320))
      equivalence (IRAPVL, POSMAP(3577)),(ORAPVL, OPOSMAP(3577))
      equivalence (ITLCVL ,POSMAP(3963)),(OTLCVL ,OPOSMAP(3963))
      equivalence (ITLOVL ,POSMAP(3983)),(OTLOVL ,OPOSMAP(3983))
      real*8 IDELYVL, ODELYVL
      real*8 IHOMCDV(4), OHOMCDV(4)
      real*8 IOPSTVL, OOPSTVL
      real*8 ISTOPVL, OSTOPVL
      real*8 IUNTCDV(2), OUNTCDV(2)
      real*8 IREWCDV, OREWCDV
      real*8 IFINCDV,OFINCDV
      real*8 IMOTCDV,OMOTCDV, IABSCDV,OABSCDV, IINCCDV,OINCCDV
      real*8 ICLMPVL(20),OCLMPVL(20)
      real*8 IPLNCDV(4), OPLNCDV(4)
      real*8 ISLWVL(10), OSLWVL(10)
      real*8 ICYCCDV(20),OCYCCDV(20)
      real*8 IFEDOVL(2), OFEDOVL(2)
      real*8 ISPNDVL(4),OSPNDVL(4)
      real*8 ICOOLVL(4),OCOOLVL(4), ISPCOVL(6),OSPCOVL(6)
      real*8 IFEDMVL(8),OFEDMVL(8)
      real*8 IRAPVL(5),ORAPVL(5), ICUTCVL(10),OCUTCVL(10)
      real*8 ITLCVL(20),OTLCVL(20)
      real*8 ITLOVL( 8),OTLOVL( 8)
c
      equivalence (REGST ,CPOSMP(7011)), (REGEN ,CPOSMP(9219))
      character*24 REGST(MAXFMT),REGEN(MAXFMT)
c
      common/REGCOM/RGBEG, RGNUM, RGTAB, RGINC, REGCNV
      integer*4 RGBEG(100), RGNUM(100), RGTAB(MAXFMT), RGINC(MAXFMT)
      integer*4 REGCNV(MAXFMT)

      integer*4 i1, i2, j, ix, inum

      j = 0

      do 10 i1=1, 100
        RGNUM(i1) = 0
   10 continue

      do 100 i1=1, MAXFMT
        if (FMTDES(10,i1).gt.0.and.REGST(i1)(1:1).ne.' ') then
          ix = ichar(REGST(i1)(1:1))-32
          if (RGNUM(ix).eq.0) then
            inum = 1
            j = j+1
            RGTAB(j) = i1
            RGBEG(ix) = j
            do 20 i2=i1+1, MAXFMT
              if (ix.eq.ichar(REGST(i2)(1:1))-32) then
                inum = inum+1
                j = j+1
                RGTAB(j) = i2
              endif
   20       continue
            RGNUM(ix) = inum
          endif
        endif
        RGINC(i1) = 0
        if (i1.eq.IMOTREG(1).or.i1.eq.IMOTREG(3).or.
     x      i1.eq.IMOTREG(5).or.i1.eq.IMOTREG(7).or.
     x      i1.eq.IMOTREG(9).or.i1.eq.IMOTREG(11).or.
     x      i1.eq.IMOTREG(14).or.i1.eq.IMOTREG(17).or.
     x      i1.eq.IMOTREG(20).or.i1.eq.IMOTREG(23)) RGINC(i1) = 1
        if (i1.eq.IMOTREG(2).or.i1.eq.IMOTREG(4).or.
     x      i1.eq.IMOTREG(6).or.i1.eq.IMOTREG(8).or.
     x      i1.eq.IMOTREG(10).or.i1.eq.IMOTREG(12).or.
     x      i1.eq.IMOTREG(15).or.i1.eq.IMOTREG(18).or.
     x      i1.eq.IMOTREG(21).or.i1.eq.IMOTREG(24)) RGINC(i1) = 2

  100 continue

      do 200 i1=1, MAXFMT
       REGCNV(i1) = -1
  200 continue
c
c...Set up register convert table.
c
c...Axis registers
c
      do i1=1,24
        if (IMOTREG(i1).gt.0 .and. IMOTREG(i1).lt.67 .and.
     x      IMOTREG(i1).ne.OMOTREG(i1)) then
          REGCNV(IMOTREG(i1)) = OMOTREG(i1)
        endif
      enddo
c
c...Sequence number register.
c
      if (ISEQCOD.gt.0.and.ISEQCOD.lt.67.and.ISEQCOD.ne.OSEQCOD) then
        REGCNV(ISEQCOD) = OSEQCOD
      endif
c
c...Alignment block register.
c
      if (IALNCOD.gt.0.and.IALNCOD.lt.67.and.IALNCOD.ne.OALNCOD) then
        REGCNV(IALNCOD) = OALNCOD
      endif
c
c...Check sum register.
c
      if (IICSMRG.gt.0.and.IICSMRG.lt.67.and.IICSMRG.ne.OICSMRG) then
        REGCNV(IICSMRG) = OICSMRG
      endif
c
c...Spindl speed RPM register.
c
      if (ISPNSCD(1).gt.0.and.ISPNSCD(1).lt.67.and.
     x    ISPNSCD(1).ne.OSPNSCD(1)) then
        REGCNV(ISPNSCD(1)) = OSPNSCD(1)
      endif
c
c...Spindl speed SFM register.
c
      if (ISPNSCD(2).gt.0.and.ISPNSCD(2).lt.67.and.
     x    ISPNSCD(2).ne.OSPNSCD(2)) then
        REGCNV(ISPNSCD(2)) = OSPNSCD(2)
      endif
c
c...OPSTOP register.
c
c      if (IOPSTCD.gt.0.and.IOPSTCD.lt.67.and.IOPSTCD.ne.OOPSTCD) then
c        REGCNV(IOPSTCD) = OOPSTCD
c      endif
c
c...SELCTL register.
c
      if (ITLCCD(3).gt.0.and.ITLCCD(3).lt.67.and.
     x    ITLCCD(3).ne.OTLCCD(3)) then
        REGCNV(ITLCCD(3)) = OTLCCD(3)
      endif
c
c...LOADTL register.
c
      if (ITLCCD(4).gt.0.and.ITLCCD(4).lt.67.and.
     x    ITLCCD(4).ne.OTLCCD(4)) then
        REGCNV(ITLCCD(4)) = OTLCCD(4)
      endif
c
c...Cycle codes.
c
      do i1 = 1,16
        if (ICYCREG(i1).gt.0.and.ICYCREG(i1).lt.67.and.
     x      ICYCREG(i1).ne.OCYCREG(i1)) then
          REGCNV(ICYCREG(i1)) = OCYCREG(i1)
        endif
      enddo
c
c...FEDRAT codes.
c
      do i1 = 1,8
        if (IFEDCD(i1).gt.0.and.IFEDCD(i1).lt.67.and.
     x      IFEDCD(i1).ne.OFEDCD(i1)) then
          REGCNV(IFEDCD(i1)) = OFEDCD(i1)
        endif
      enddo
c
c...Set up G and M code conversion lists.
c
c
c...Linear, absolute and icremental motion codes
c
      call ptd_cnvreg (IMOTCOD,IMOTCDV,OMOTCOD,OMOTCDV)
      call ptd_cnvreg (IABSCOD,IABSCDV,OABSCOD,OABSCDV)
      call ptd_cnvreg (IINCCOD,IINCCDV,OINCCOD,OINCCDV)
c
c...Unit change codes
c
      call ptd_cnvreg (IUNTCOD(1),IUNTCDV(1),OUNTCOD(1),OUNTCDV(1))
      call ptd_cnvreg (IUNTCOD(2),IUNTCDV(2),OUNTCOD(2),OUNTCDV(2))
c
c...Tool change codes
c
      call ptd_cnvreg(ITLCCD(1),ITLCVL(1),OTLCCD(1),OTLCVL(1))
      call ptd_cnvreg(ITLCCD(2),ITLCVL(2),OTLCCD(2),OTLCVL(2))
      do i1=5,12
        call ptd_cnvreg(ITLCCD(i1),ITLCVL(i1),OTLCCD(i1),OTLCVL(i1))
      enddo
c
c...Tool length offset codes
c
      do i1=1,8
        call ptd_cnvreg(ITLOCD(i1),ITLOVL(i1),OTLOCD(i1),OTLOVL(i1))
      enddo
c
c...Coolnt codes
c
      do i1=1,4
        call ptd_cnvreg(ICOOLCD(i1),ICOOLVL(i1),OCOOLCD(i1),OCOOLVL(i1))
      enddo
c
c...Spindl codes
c
      do i1=1,6
        call ptd_cnvreg(ISPCOCD(i1),ISPCOVL(i1),OSPCOCD(i1),OSPCOVL(i1))
      enddo
c
c...Spindl direction codes
c
      do i1=1,4
        call ptd_cnvreg(ISPNDCD(i1),ISPNDVL(i1),OSPNDCD(i1),OSPNDVL(i1))
      enddo
c
c...Cycle codes
c
      do i1=1,14
        call ptd_cnvreg(ICYCCOD(i1),ICYCCDV(i1),OCYCCOD(i1),OCYCCDV(i1))
      enddo
c
c...Fedrat lock codes
c
      do  i1=1,2
        call ptd_cnvreg(IFEDOCD(i1),IFEDOVL(i1),OFEDOCD(i1),OFEDOVL(i1))
      enddo
c
c...Fedrat mode codes
c
      do  i1=1,8
        call ptd_cnvreg(IFEDMCD(i1),IFEDMVL(i1),OFEDMCD(i1),OFEDMVL(i1))
      enddo
c
c...Fedrat mode codes
c
      do  i1=1,8
        call ptd_cnvreg(IFEDMCD(i1),IFEDMVL(i1),OFEDMCD(i1),OFEDMVL(i1))
      enddo
c
c...Rapid codes
c
      do i1=1,NRAPCD
        call ptd_cnvreg (IRAPCD(i1),IRAPVL(i1),ORAPCD(i1),ORAPVL(i1))
      enddo
c
c...Cutcom codes.
c
      do i1=1,3
        call ptd_cnvreg(ICUTCCD(i1),ICUTCVL(i1),OCUTCCD(i1),OCUTCVL(i1))
      enddo
c
c...Plane selection codes
c
      do  i1=1,4
        call ptd_cnvreg(IPLNCOD(i1),IPLNCDV(i1),OPLNCOD(i1),OPLNCDV(i1))
      enddo
c
c...HOME codes
c
      do  i1=1,4
        call ptd_cnvreg(IHOMCOD(i1),IHOMCDV(i1),OHOMCOD(i1),OHOMCDV(i1))
      enddo
c
c...Slow down codes
c
      if (ISLWFL(1).eq.1) then
        do i1=1,ISLWFL(4)*3
          call ptd_cnvreg (ISLWCD(i1),ISLWVL(i1),OSLWCD(i1),OSLWVL(i1))
        enddo
      endif
c
c...Clamp codes
c
      do 240 i1=1,20
        call ptd_cnvreg(ICLMPCD(i1),ICLMPVL(i1),OCLMPCD(i1),OCLMPVL(i1))
  240 continue
c
c...Delay code
c
      call ptd_cnvreg (IDELYCD,IDELYVL,ODELYCD,ODELYVL)
c
c...Rewind register.
c
      call ptd_cnvreg (IREWCOD,IREWCDV,OREWCOD,OREWCDV)
c
c...OPSTOP code.
c
      call ptd_cnvreg (IOPSTCD,IOPSTVL,OOPSTCD,OOPSTVL)
c
c...STOP code.
c
      call ptd_cnvreg (ISTOPCD,ISTOPVL,OSTOPCD,OSTOPVL)
c
c...FINI
c
      call ptd_cnvreg(IFINCOD,IFINCDV,OFINCOD,OFINCDV)

      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_cnvreg (ireg1, gval1, ireg2, gval2)
c
c   FUNCTION: This function converts an input G or M register to
c             an output G or M register and stores it in the register
c             conversion list.
c
c   INPUT:  ireg1   I*4  D1  - register to be converted.
c           gval1   R*8  D1  - value of register.
c           ireg2   I*4  D1  - register to convert to.
c           gval2   R*8  D1  - value of register to convert to.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine ptd_cnvreg (ireg1, gval1, ireg2, gval2)
      include 'ptedpost.inc'

      integer*4 ireg1, ireg2
      real*8 gval1, gval2

      integer*4 ix, kreg1, kreg2

      if (ireg1.ne.0 .and. ireg2.ne.0) then
         kreg1 = ireg1
         kreg2 = ireg2
         if (ireg1.lt.0)
     x      call ptd_icnvgmcode (ireg1, gval1, kreg1)
         if (ireg2.lt.0)
     x      call ptd_ocnvgmcode (ireg2, gval2, kreg2)
         if (kreg1.ne.kreg2 .or. gval1.ne.gval2) then
            if (kreg1.gt.17.and.kreg1.lt.29) then
              ix = kreg1-17
            else if (kreg1.gt.36.and.kreg1.lt.48) then
              ix = kreg1-25
            else
              goto 8000
            endif
            call ptd_updreglist (ix, gval1, gval2, kreg2)
         endif
      endif

 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_icnvgmcode (inreg, gval, outreg)
c
c   FUNCTION: This function converts input logical G and M registers to
c             physical G and M registers.
c
c   INPUT:  inreg   I*4  D1  - register to be converted.
c           gval    R*8  D1  - value of register.
c
c   OUTPUT: outreg  I*4  D1  - corresponding physical register.
c
c***********************************************************************
c
      subroutine ptd_icnvgmcode (inreg, gval, outreg)
      include 'post.inc'

      integer*4 inreg, outreg
      real*8    gval
      integer*4 i, j

      equivalence (DUMMY, POSMAP(0003)),  (GRANGE,POSMAP(0801))
      equivalence (MRANGE,POSMAP(0955))
      real*8 DUMMY,GRANGE(14,11), MRANGE(7,11)

      if (inreg.eq.-1) then
c
c......G-codes
c
        do 200 i=1,11,1
          do 110 j=1,14,1
            if (GRANGE(j,i) .eq. DUMMY) go to 150
            if (GRANGE(j,i) .eq. gval) then
               outreg = 17 + i
               goto 8000
            endif
  110     continue
  150     continue
  200   continue
c
        outreg = 18
      else if (inreg.eq.-2) then
c
c...M-codes
c
        do 350 i=1,11,1
          do 300 j=1,7,1
            if (MRANGE(j,i) .eq. DUMMY) go to 320
            if (MRANGE(j,i) .eq. gval) then
              outreg = 36 + i
              goto 8000
            endif
  300     continue
  320     continue
  350   continue
        outreg = 37
      endif

 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_ocnvgmcode (inreg, gval, outreg)
c
c   FUNCTION: This function converts output logical G and M registers to
c             physical G and M registers.
c
c   INPUT:  inreg   I*4  D1  - register to be converted.
c           gval    R*8  D1  - value of register.
c
c   OUTPUT: outreg  I*4  D1  - corresponding physical register.
c
c***********************************************************************
c
      subroutine ptd_ocnvgmcode (inreg, gval, outreg)
      include 'ptedpost.inc'
c      include 'menu.inc'

      integer*4 inreg, outreg
      real*8    gval
      integer*4 i, j

      equivalence (DUMMY, OPOSMAP(0003)),  (GRANGE,OPOSMAP(0801))
      equivalence (MRANGE,OPOSMAP(0955))
      real*8 DUMMY,GRANGE(14,11), MRANGE(7,11)

      if (inreg.eq.-1) then
c
c......G-codes
c
        do 200 i=1,11,1
          do 110 j=1,14,1
            if (GRANGE(j,i) .eq. DUMMY) go to 150
            if (GRANGE(j,i) .eq. gval) then
               outreg = 17 + i
               goto 8000
            endif
  110     continue
  150     continue
  200   continue
c
        outreg = 18
      else if (inreg.eq.-2) then
c
c...M-codes
c
        do 350 i=1,11,1
          do 300 j=1,7,1
            if (MRANGE(j,i) .eq. DUMMY) go to 320
            if (MRANGE(j,i) .eq. gval) then
              outreg = 36 + i
              goto 8000
            endif
  300     continue
  320     continue
  350   continue
        outreg = 37
      endif

 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_aptsrc(indat, knc)
c
c   FUNCTION: Convert an NC block into apt source statements and add
c             them to the apt source list.
c
c   INPUT:  indat    C*n  D1  - NC block
c           knc     I*4  D1  - Length of block
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine  ptd_aptsrc(indat, knc)
c
      include 'pted.inc'
c
      byte indat(256)
      integer*4 knc
      character*256 cindat
      byte bindat(256)
      integer*4 i, lnc
      equivalence (bindat, cindat)

      lnc = knc
      if (lnc.gt.256) lnc = 256
      do 100 i = 1, lnc
          bindat(i) = indat(i)
  100 continue

      call ptdf_aptsrc(cindat,knc,0,PCNV_APTSRC)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptdf_mactoapt (imac)
c
c   FUNCTION: Call a macro and the add statements that it generates
c             to the apt source list.
c
c   INPUT:  imac    I*4  D1  - Vocab number of macro to call
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine ptdf_mactoapt (imac)

      include 'menu.inc'
      include 'post.inc'
      include 'pregen.inc'
      include 'pted.inc'

      integer*4 imac

      equivalence (ITYPE , KPOSMP(0003))
      equivalence (ISUBT , KPOSMP(0004))
      equivalence (MXCL  , KPOSMP(0005))
      equivalence (IPSTWD, KPOSMP(0006))
      equivalence (MULTAX, KPOSMP(0056))
      equivalence (NPT   , KPOSMP(0059))
c
      integer*4 ITYPE, ISUBT, MXCL, IPSTWD(50)
      integer*4 MULTAX, NPT
c
      equivalence (PSTWD,  POSMAP(0441))
      equivalence (CLPT,   POSMAP(0491))
      real*8 CLPT(240), PSTWD(50)
c
      equivalence (LPSTWD,CPOSMP(0217))
      character*512 LPSTWD
c
      character*80 aptdat
      byte bdat(80)
      equivalence (aptdat,bdat)
      character*80 cmsg
      character*1 sep
      integer*4 i, j, k, l1, ix, kfl, kerr, strlen1

      ITYPE = 2000
      ISUBT = imac
      call ppcall (kfl,cmsg,kerr)
      if (kfl.eq.0 .and. kerr.eq.0) then
        call precmp(cmsg,kerr)
        do while (IMACPT.gt.0 .and. kerr.eq.0)
          if (PCNV_TYPE .eq. PCNV_SIMUL) then
            call ptd_simmac
          else if (ITYPE .eq. 2000 .and. ISUBT .eq. 1106) then
              call force
          else if (PCNV_TYPE .eq. PCNV_APTSRC .and. ITYPE.eq.2000) then
            call getvwd (ISUBT,aptdat,l1,1,PSTWRD,PSTWVL,NPSTWD)
            if (ISUBT.ge.1043.and.ISUBT.le.1046) then
              aptdat(7:) = LPSTWD(1:66)
              l1 = strlen1(aptdat)
            else
              sep = '/'
              ix = 7
              do i=1,MXCL
                aptdat(ix:ix) = sep
                ix = ix+1
                if (i.eq.1) then
                  aptdat(ix:ix) = ' '
                  ix = ix+1
                  sep = ','
                endif
                if (IPSTWD(i).gt.0) then
                  call getvwd (IPSTWD(i),aptdat(ix:),j,2,PSTWRD,PSTWVL,
     -                         NPSTWD)
                else
                  call rtoc (PSTWD(i),aptdat(ix:),j)
                endif
                ix = ix+j
              enddo
              if (MXCL.gt.0) l1 = ix-1
            endif
            call ptd_addapt (bdat, l1)
            if (ISUBT.eq.1105) then
              if (IPSTWD(1).eq.72) then
                MULTAX = 0
                NPT = 3
              else
                MULTAX = 1
                NPT = 6
              endif
            endif
          else if (ITYPE.eq.5000) then
            do i=1,MXCL
              if (ISUBT.eq.3) then
                aptdat = 'FROM/'
              else
                aptdat = 'GOTO/'
              endif
              ix = 6
              do j=1,3
                call rtoc88(CLPT((i-1)*NPT+j), aptdat(ix:), k)
                ix = ix+k
                if (j.lt.3) then
                  aptdat(ix:ix) = ','
                  ix = ix+1
                endif
              enddo
              if (MULTAX.eq.1) then
                aptdat(ix:ix+1) = ',$'
                l1 = ix+1
                call ptd_addapt(bdat,l1)
                aptdat(1:8) = ' '
                ix = 6
                do j=4,6
                  call rtoc88(CLPT((i-1)*NPT+j), aptdat(ix:), k)
                  ix = ix+k
                  if (j.lt.6) then
                    aptdat(ix:ix) = ','
                    ix = ix+1
                  endif
                enddo
              endif
              l1 = ix-1
              call ptd_addapt(bdat,l1)
            enddo
          endif
          call precmp(cmsg,kerr)
        enddo
      endif
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptdf_ftoc (gnum,cbuf,knc,kfmt)
c
c   FUNCTION:  This routine converts a real number to a character
c              string using the acuracy of a predefined format array
c              but ignoring leading or trailing zero suppression.
c
c   INPUT:  gnum    R*8  D1  -  Real value to be converted to charac-
c                               ter string.
c
c           kfmt    I*2  D10 -  Format specifier.
c                               kfmt(1) = Floating point format.
c                                         1 = Leading zero suppression.
c                                         2 = Trailing zero suppression.
c                                         3 = Floating point with deci-
c                                             mal point in whole #'s.
c                                         4 = Floating point without
c                                             decimal point in whole #'s
c                               kfmt(2) = Plus sign flag.
c                               kfmt(3) = Minus sign flag.
c                               kfmt(4) = # of digits to left of decimal
c                                         point (INCH).
c                               kfmt(5) = # of digits to left of decimal
c                                         point (MM).
c                               kfmt(6) = # of digits to right of deci-
c                                         mal point (INCH).
c                               kfmt(7) = # of digits to right of deci-
c                                         mal point (MM).
c                               kfmt(8) = Min # of digits to left of
c                                         decimal point.
c                               kfmt(9) = Min # of digits to right of
c                                         decimal point.
c                               kfmt(10)  Not used.
c
c   OUTPUT: cbuf    C*n  D1  -  Character string from 'gnum'.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine ptdf_ftoc (gnum,cbuf,knc,kfmt)
c
      include 'post.inc'
c
      real*8 gnum
      character*(*) cbuf
      integer*4 knc
      integer*2 kfmt(10)
c
      integer*4 j
      integer*2 lfmt(10)
c
c...Save format in local array with type changed to floating
c
      do j=1,10
       lfmt(j) = kfmt(j)
      enddo
      if (lfmt(1).eq.1 .or. lfmt(1).eq.2) lfmt(1) = 3
c
c...Convert number
c
      call ftoc (gnum,cbuf,knc,lfmt)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptdf_convrapto (rapto)
c
c   FUNCTION:  This routine converts then rapto distance for pivot
c              and tool length.
c
c   INPUT:  rapto   R*8  D1  -  RAPTO distance.
c
c   OUTPUT: rapto   R*8  D1  -  RAPTO distance converted.
c
c***********************************************************************
c
      subroutine ptdf_convrapto (rapto)
c
      include 'post.inc'
c
      real*8 rapto
c
      equivalence (MCHNUM, POSMAP(1287)), (LINAXS, POSMAP(1299))
      equivalence (AXSOUT, POSMAP(1340)), (TLVEC , POSMAP(1369))

      equivalence (AXSSTO, POSMAP(1425)), (ROTANG, POSMAP(5173))
c
      real*8 MCHNUM(3,4), LINAXS(6), AXSOUT(10), AXSSTO(10), TLVEC(3)
      real*8 ROTANG(20,2)
c
      integer*4 i, j
      real*8 gmch(3,4), glin(6), grot(20,2), gvec(3)

      do i=1,4
         do j=1,3
            gmch(j,i) = MCHNUM(j,i)
         enddo
      enddo

      do i=1,6
        glin(i) = LINAXS(i)
      enddo

      call cpyrot (ROTANG,grot)

      do i=1,3
        gvec(i) = TLVEC(i)
      enddo

      gmch(3,3) = rapto
      call alladr (AXSOUT, glin, gmch, grot, gvec, 2, 1)
      rapto = gmch(3,1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptdf_ledout(iflag, nchrs, chr, len)
c
c   FUNCTION:  This routine calculates the number of characters in
c              leader or trailer of the punch tape.
c
c   INPUT:  iflag   I*4  D1  -  0 = leader, 1 = trailer
c
c   OUTPUT: nchrs   I*4  D1  -  Number of characters.
c           chr     B*1  D1  -  Leader character.
c           len     I*4  D1  -  Block length.
c
c***********************************************************************
c
      subroutine ptdf_ledout (iflag, nchrs, chr, len)
c
      include 'post.inc'
      include 'ptedpost.inc'
c
      integer*4 iflag, nchrs, len
      byte chr
c
      equivalence (MCHOPT, OKPOSMP(308)), (NEOB, OKPOSMP(841))
      integer*4 MCHOPT(20), NEOB
      equivalence (LEDBEG, OPOSMAP(1201)), (LEDEND, OPOSMAP(1202))
      real*8 LEDBEG, LEDEND
      equivalence (LEOB, OCPOSMP(971)),(LEDCHR, OCPOSMP(986))
      character*1 LEDCHR
      character*5 LEOB
c
      real*8 rnum
      byte bchr
      equivalence (LEDCHR,bchr)

      if (iflag.eq.0) then
        rnum = LEDBEG
      else
        rnum = LEDEND
      endif
      if (MCHOPT(2).eq.2) rnum = rnum/25.4
      nchrs = rnum*10
      chr = bchr
      len = MCHOPT(5)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_simhdr
c
c   FUNCTION: Write the simulation file header to the
c              simulation file.
c
c   INPUT:  klen     I*4  D1  - Length of PARTNO string.
c           pstr     C*n  D1  - PARTNO string.
c           fname    C*n  D1  - Simulation file name.
c
c   OUTPUT:
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ptd_simhdr(klen, pstr, fname, kerr)

      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME

      integer*4 klen, kerr
      byte pstr(80), fname(MAX_PATH)

      equivalence (LPARTN,BPARTN,CPOSMP(0067))
      character*66 LPARTN
      byte BPARTN(66)

      integer*4 i, nc, strlen1
      character*80 msg
      character*(MAX_PATH) fnam,cdev,csimfn
      character*(MAX_FILE) cext
      byte bsimfn(MAX_PATH)
      equivalence (csimfn,bsimfn)

      i = 1
      do while (i.lt.MAX_PATH .and. fname(i).ne.0)
        bsimfn(i) = fname(i)
        i = i+1
      enddo
      if (i.lt.MAX_PATH) then
        csimfn(i:) = ' '
        call fbreak (csimfn, cdev, fnam, cext)
        call adftyp (csimfn, fnam, '.sim')
      endif

      IOPFL(10) = 3
      PREPT     = 1
      PSTNAM(1) = LMNAME
      LCMPFI    = csimfn
      NCCMPF    = strlen1(LCMPFI)
      LOPFL(10) = fnam
      CLDATE    = LDATE
      CLTIME    = LTIME
      if (klen .gt. 0) then
        nc = klen
        if (nc.gt.66) nc = 66
        do i=1,nc
          BPARTN(i) = pstr(i)
        enddo
        if (nc.lt.66) LPARTN(nc+1:) = ' '
      else
        LPARTN = ' '
      endif
      call simopn(msg,kerr)
      if (kerr.eq.1) return
      call simhdr(msg,kerr)
c      fnam = 'cutter.dat'
c      nc = strlen1(fnam)
c      call tolfil(cdev,fnam,nc,ierr)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_nctosim(indat, knc)
c
c   FUNCTION: Convert an NC block into simulation file statements
c             and write them to the simulation file.
c
c   INPUT:  indat    C*n  D1  - NC block
c           knc      I*4  D1  - Length of block
c           knt      I*4  D1  - Punch file line number.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine  ptd_nctosim(indat, knc, knt)
c
      include 'pted.inc'
c
      byte indat(256)
      integer*4 knc, knt
      character*256 cindat
      byte bindat(256)
      integer*4 i, lnc
      equivalence (bindat, cindat)

      lnc = knc
      if (lnc.gt.256) lnc = 256
      do 100 i = 1, lnc
          bindat(i) = indat(i)
  100 continue

      call ptdf_aptsrc (cindat,knc,knt,PCNV_SIMUL)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE: ptd_toldo (cbuf, kerr, erlab)
c
c   FUNCTION:  This routine call ptdf_toldo to process a
c              cutter statement.
c
c   INPUT:  cbuf    C*80 D1  -  Cutter statement.
c           len     I*4  D1  -  Length of cutter statement.
c
c   OUTPUT: kerr    I*4  D1  -  Error status: = 0 - OK, > 0 - number of
c                               syntax errors in tool file, -1 - too
c                               many tools specified.
c
c           erlab   C*8  D1  -  Error label string.
c
c***********************************************************************
c
      subroutine ptd_toldo (cbuf, len, kerr)

      byte cbuf(80)
      integer*4 len, kerr

      integer*4 i, nc
      byte bdat(80)
      character*80 cdat
      equivalence (bdat, cdat)
      character*8 erlab

      nc = len
      if (nc.gt.80) nc = 80
      do i=1,nc
        bdat(i) = cbuf(i)
      enddo
      if (i.lt.80) cdat(i:) = ' '

      call ptdf_toldo(cdat, kerr, erlab)

      return
      end
c
c***********************************************************************
c
c   SUBROUTINE: ptdf_toldo (cbuf, kerr, erlab)
c
c   FUNCTION:  This routine parses and checks syntax of a
c              cutter statement and stores parameters in the
c              internal arrays.
c
c   INPUT:  cbuf    C*80 D1  -  Cutter statement.
c
c   OUTPUT: kerr    I*4  D1  -  Error status: = 0 - OK, > 0 - number of
c                               syntax errors in tool file, -1 - too
c                               many tools specified.
c
c           crlab   C*8  D1  -  Error label string.
c
c***********************************************************************
c
      subroutine ptdf_toldo (cbuf, kerr, erlab)
c
      include 'post.inc'
      include 'menu.inc'
      include 'compile.inc'
      include 'ptedtool.inc'
c
      character*80 cbuf
      integer*4 kerr
      character*8 erlab
c
      integer*4 nc,itol,ist,ien,ix,inc,i,jindex,lcom,ierr,strlen1,j,ip
c
      character*80 cbuf1,msg
c
c...Initialize routine
c
      kerr   = 0
      erlab  = ' '
      call remspc (cbuf,cbuf1,nc)
      if (nc .eq. 0) go to 8000
c
c...Parse tool parameters record
c
      ITXTPT = 1
      call stpars (cbuf1,nc,2,msg,ierr,lcom)
      if (lcom .eq. 1) go to 8000
      if (kerr .eq. 1) go to 9000
c
c...LOADTL
c...TURRET
c
      if (ICTYP(1) .eq. 1 .and. (RCSUB(1) .eq. 1055 .or.
     1    RCSUB(1) .eq. 1033)) then
          if (ICTYP(2) .ne. 2 .or. RCSUB(2) .ne. 7 .or.
     1        ICTYP(3) .ne. 3) go to 9000
          if (NTOK .ge. 5 .and. (ICTYP(5) .ne. 1 .or.
     1        (RCSUB(5) .ne. 9 .and. RCSUB(5) .ne. 23) .or.
     2        ICTYP(7) .ne. 3)) go to 9000
c
c......Check if tool number already exists,
c
          itol   = RCSUB(3)
          ITPX   = jindex (TLNUM,itol,NTOOL)
          if (ITPX .eq. 0) then
              NTOOL  = NTOOL + 1
              ITPX = NTOOL
              ISHKFL(ITPX) = 1
              do 100 i=1,3,1
                  ICTYPE(ITPX,i) = 0
                  CUSYM(ITPX,i) = ' '
                  NCUTR(ITPX) = 0
                  do 80 j=1,4,1
                      CUPARM(ITPX,j,i) = 0
   80             continue
  100         continue
          endif
          if (NTOOL .gt. MAXTL) go to 9100
c
c......Store tool number & length in arrays
c
          TLNUM(ITPX) = itol
          TLLEN(ITPX) = 0.
          if (NTOK .ge. 7) TLLEN(ITPX) = RCSUB(7)
c
c...CUTTER/parms
c
      else if (ICTYP(1) .eq. 1 .and. RCSUB(1) .eq. 4025) then
          if (ICTYP(2) .ne. 2 .or. RCSUB(2) .ne. 7) go to 9000
          if (ICTYP(3) .eq. 3 .or. (ICTYP(3) .eq. 1 .and.
     1        (RCSUB(3) .eq. 700 .or. RCSUB(3) .eq. 191))) then
              NCUTR(ITPX) = 0
              ist    = 3
              if (ICTYP(3) .eq. 1) then
                  NCUTR(ITPX) = NCUTR(ITPX) + 1
                  CUTR(ITPX,NCUTR(ITPX)) = RCSUB(3) - 10000
                  ist = 5
              endif
              do 200 i=ist,NTOK,2
                  if (ICTYP(i) .ne. 3) go to 9000
                  NCUTR(ITPX) = NCUTR(ITPX) + 1
                  CUTR(ITPX,NCUTR(ITPX)) = RCSUB(i)
  200         continue
              ICTYPE(ITPX,1) = 1
c
c...CUTTER/DISPLY [,SHANK-HOLDER], sym
c
          else if (ICTYP(3) .eq. 1 .and. RCSUB(3) .eq. 1021) then
              ix     = 1
              inc    = 5
c
c......CUTTER/DISPLY,SHANK-HOLDER
c
              if (ICTYP(5) .eq. 1 .and. (RCSUB(5) .eq. 157 .or.
     1            RCSUB(5) .eq. 192)) then
                  ix     = 2
                  if (RCSUB(5) .eq. 157) ix = 3
                  inc    = 7
              endif
c
c......CUTTER/DISPLY,sym
c
              ICTYPE(ITPX,ix) = 1
              if (ICTYP(inc) .eq. 4) then
                  ist = RCSUB(inc)
                  ien = ICNC(inc)
                  CUSYM(ITPX,ix) = LCTXT(ist:ien)
                  ICTYPE(ITPX,ix) = 2
                  inc    = inc    + 2
c
c......CUTTER/DISPLY,cv,pv
c
                  if (inc .le. NTOK .and. ICTYP(inc) .eq. 4) then
                      ist = RCSUB(inc)
                      ien = ICNC(inc)
                      nc  = strlen1(CUSYM(ITPX,ix))
                      CUSYM(ITPX,ix)(nc+1:) = ',' // LCTXT(ist:ien)
                      ICTYPE(ITPX,ix) = 2
                      inc    = inc    + 2
                  endif
              endif
c
c......CUTTER/DISPLY,parms
c
              ip     = 0
              do 300 i=inc,NTOK,2
                  if (ICTYP(i) .eq. 3) then
                      ip = ip + 1
                      CUPARM(ITPX,ip,ix) = RCSUB(i)
                  else if (ICTYP(i) .eq. 1 .and. (RCSUB(i) .eq. 4025
     1                     .or. RCSUB(i) .eq. 157) .and. ix .eq. 2) then
                      ISHKFL(ITPX) = 1
                      if (RCSUB(i) .eq. 4025) ISHKFL(ITPX) = 0
                  else if (ICTYP(i) .ne. 1 .or. RCSUB(i) .ne. 705) then
                      go to 9000
                  endif
  300         continue
c
c...Unrecognized command
c
          else if (ICTYP(3) .eq. 1 .and. RCSUB(3) .eq. 1021) then
              go to 9000
          endif
      endif
c
c...End of routine
c
 8000 return
c
c...Cutter description syntax error
c
 9000 erlab  = 'SYNERR'
      kerr   = kerr + 1
      go to 8000
c
c...Too many tools specified
c
 9100 kerr   = -1
      erlab  = 'NOTLSPC'
      NTOOL  = 120
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE: ptd_tolinit()
c
c   FUNCTION:  This routine initializes the cutter table.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine ptd_tolinit ()
c
      include 'ptedtool.inc'
c
      integer*4 i,j
c
c...Initialize the Cutter table
c
      NTOOL  = 0
      ITPX   = 1
      ISHKFL(1) = 1
      NCUTR(1) = 0
      TLNUM(1) = 0
      TLLEN(1) = 0.
      do 100 i=1,3,1
          ICTYPE(1,i) = 0
          CUSYM(1,i) = ' '
          do 80 j=1,4,1
             CUPARM(1,j,i) = 0
   80     continue
  100 continue
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE: ptd_simmac()
c
c   FUNCTION:  This routine convert macro output and write to sim file.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine ptd_simmac
c
      include 'pted.inc'
      include 'post.inc'
      include 'menu.inc'
c
      equivalence (ISN   , KPOSMP(0001))
      equivalence (ITYPE , KPOSMP(0003))
      equivalence (ISUBT , KPOSMP(0004))
      equivalence (MXCL  , KPOSMP(0005))
      equivalence (IPSTWD, KPOSMP(0006))
      equivalence (MULTAX, KPOSMP(0056))
      equivalence (NPT   , KPOSMP(0059))
      equivalence (NCUT  , KPOSMP(0062))
      equivalence (NCUTDS, KPOSMP(0063))
      equivalence (ITP   , KPOSMP(1801))
      equivalence (SPNFCD, KPOSMP(3118))
      equivalence (ICOLSW, KPOSMP(3146))
      equivalence (ICUTDO, KPOSMP(3301))
c
      integer*4 ISN, ITYPE, ISUBT, MXCL, IPSTWD(50), MULTAX, NPT, NCUT
      integer*4 NCUTDS, ITP, SPNFCD(3),ICOLSW, ICUTDO(15)
c
      equivalence (PSTWD,  POSMAP(0441))
      equivalence (CLPT,   POSMAP(0491))
      equivalence (RPM   ,POSMAP(3307)), (TL    ,POSMAP(3601))
      equivalence (TLNO  ,POSMAP(3841))
      real*8 CLPT(240), PSTWD(50), RPM, TL(120), TLNO(120)
c
      equivalence (LPSTWD,CPOSMP(0217))
      character*512 LPSTWD

      integer*4 j, k, nc, kerr
      character*256 lnum, cmsg

      integer*4 COOLNTV /1030/ ,LODTLV  /1055/
      integer*4 STOCKV  /321/  ,FIXTURV /898/
      integer*4 BOXV    /340/  ,CYLNDRV /4042/
      integer*4 LOADV   /1075/ ,STLV    /330/
      integer*4 CLONEV  /576/  ,MOVEV   /577/
      integer*4 REMOVEV /843/  ,MODIFYV /55/
      integer*4 OFFV    /72/   ,RPMV    /78/
      integer*4 SPINDLV /1031/

      if (IOPFL(10) .ne. 3) go to 8000
      MFKNC = 0
      if (ITYPE.eq.2000) then
        SMSTAT = 0
        SMSTOP = 0
        if (ISUBT.eq.COOLNTV) then
          j = 1
          if (IPSTWD(1).eq.OFFV) j = 2
          if (j .ne. ICOLSW) then
            ICOLSW = j
            SMSTAT = 1
          endif
        elseif (ISUBT.eq.LODTLV) then
          if (IPSTWD(1).eq.0) then
            if (ITP.lt.1 .or. ITP.gt.120) ITP = 1
            if (TLNO(ITP) .ne. PSTWD(1)) then
              TLNO(ITP) = PSTWD(1)
              SMSTAT = 1
            endif
          endif
        elseif (ISUBT.eq.SPINDLV) then
          j = 1
          if (IPSTWD(1).eq.RPMV) j = 2
          if (IPSTWD(j).eq.0) then
            RPM = PSTWD(j)
            SMSTAT = 1
          endif
        elseif (ISUBT.eq.STOCKV .or. ISUBT.eq.FIXTURV) then
          call getvwd (ISUBT,lnum,nc,2,PSTWRD,PSTWVL,NPSTWD)
          call simwrt (lnum,nc,cmsg,kerr)
          call itoc (ISN,lnum,nc,0)
          call simwrt (lnum,nc,cmsg,kerr)
          if (IPSTWD(1) .eq. BOXV) then
            if (MXCL.ne.9) goto 8000
            do j=2,9
              if (IPSTWD(j).ne.0) goto 8000
            enddo
            call itoc (BOXV,lnum,nc,0)
            call simwrt (lnum,nc,cmsg,kerr)
            k = PSTWD(3)
            call itoc (k,lnum,nc,0)
            call simwrt (lnum,nc,cmsg,kerr)
            k = PSTWD(2)
            call itoc (k,lnum,nc,0)
            call simwrt (lnum,nc,cmsg,kerr)
            do j=4,9
              call rtoc (PSTWD(j),lnum,nc)
              call simwrt (lnum,nc,cmsg,kerr)
            enddo
          elseif (IPSTWD(1) .eq. CYLNDRV) then
            if (MXCL.ne.11) goto 8000
            do j=2,11
              if (IPSTWD(j).ne.0) goto 8000
            enddo
            call itoc (CYLNDRV,lnum,nc,0)
            call simwrt (lnum,nc,cmsg,kerr)
            k = PSTWD(3)
            call itoc (k,lnum,nc,0)
            call simwrt (lnum,nc,cmsg,kerr)
            k = PSTWD(2)
            call itoc (k,lnum,nc,0)
            call simwrt (lnum,nc,cmsg,kerr)
            do j=4,11
              call rtoc (PSTWD(j),lnum,nc)
              call simwrt (lnum,nc,cmsg,kerr)
            enddo
          elseif (IPSTWD(1) .eq. LOADV) then
            if (MXCL.ne.2) goto 8000
            if (IPSTWD(2).ne.0) goto 8000
            k = PSTWD(2)
            call itoc (k,lnum,nc,0)
            call simwrt (lnum,nc,cmsg,kerr)
            j = 512
            do while (LPSTWD(j:j).eq.' ' .and. j.gt.1)
              j = j-1
            enddo
            call simwrt (LPSTWD,j,cmsg,kerr)
          elseif (IPSTWD(1) .eq. STLV) then
            if (MXCL.ne.3) goto 8000
            do j=2,MXCL
              if (IPSTWD(j).ne.0) goto 8000
              k = PSTWD(j)
              call itoc (k,lnum,nc,0)
              call simwrt (lnum,nc,cmsg,kerr)
            enddo
            j = 512
            do while (LPSTWD(j:j).eq.' ' .and. j.gt.1)
              j = j-1
            enddo
            call simwrt (LPSTWD,j,cmsg,kerr)
          elseif (IPSTWD(1) .eq. CLONEV) then
            if (MXCL.ne.3 .and. MXCL.ne.4) goto 8000
            do j=2,MXCL
              if (IPSTWD(j).ne.0) goto 8000
            enddo
            call itoc (CLONEV,lnum,nc,0)
            call simwrt (lnum,nc,cmsg,kerr)
            if (MXCL.eq.3) PSTWD(4) = 1
            do j=2,4
              k = PSTWD(j)
              call itoc (k,lnum,nc,0)
              call simwrt (lnum,nc,cmsg,kerr)
            enddo
          elseif (IPSTWD(1) .eq. MOVEV) then
            if (MXCL.lt.15) goto 8000
            call itoc (MOVEV,lnum,nc,0)
            call simwrt (lnum,nc,cmsg,kerr)
            do j=2,14
              if (IPSTWD(j).ne.0) goto 8000
              call rtoc (PSTWD(j),lnum,nc)
              call simwrt (lnum,nc,cmsg,kerr)
            enddo
            nc = 6
            call simwrt (LPSTWD,nc,cmsg,kerr)
            do j=15,MXCL
              if (IPSTWD(j).ne.0) goto 8000
              k = PSTWD(j)
              call itoc (k,lnum,nc,0)
              call simwrt (lnum,nc,cmsg,kerr)
            enddo
          elseif (IPSTWD(1) .eq. REMOVEV) then
            if (MXCL.lt.2) goto 8000
            call itoc (REMOVEV,lnum,nc,0)
            call simwrt (lnum,nc,cmsg,kerr)
            do j=2,MXCL
              if (IPSTWD(j).ne.0) goto 8000
              k = PSTWD(j)
              call itoc (k,lnum,nc,0)
              call simwrt (lnum,nc,cmsg,kerr)
            enddo
          elseif (IPSTWD(1) .eq. MODIFYV) then
            if (MXCL.lt.7) goto 8000
            call itoc (MODIFYV,lnum,nc,0)
            call simwrt (lnum,nc,cmsg,kerr)
            do j=2,MXCL
              if (IPSTWD(j).ne.0) goto 8000
              if (j.eq.4) then
                call rtoc (PSTWD(j),lnum,nc)
              else
                k = PSTWD(j)
                call itoc (k,lnum,nc,0)
              endif
              call simwrt (lnum,nc,cmsg,kerr)
            enddo

          endif


          call simwrt ('#EOL#',5,cmsg,kerr)
        endif
        if (SMSTAT .eq. 1) call simsta (SMSTOP,cmsg,kerr)

      else if (ITYPE.eq.5000) then
      endif

 8000 return
      end
c***********************************************************************
c
c   SUBROUTINE:  ptdf_seqctl(cdat, knc, kseqreg, kseqnum, kret)
c
c   FUNCTION: Check if the NC block is valid and if it is replace the sequence
c			number if there is a sequest register
c
c   INPUT:  cdat    C*n  D1  - NC block
c
c           knc     I*4  D1  - Length of block
c           kseqreg: sequence register
c		  kseqnum: seqence number to be replaced,
c				'if kseqnum=-1: don't reseqence the block, just check if the block
c								is valid
c				'if kseqnum=-1000: remove the sequence register from block, return 1
c
c   OUTPUT: cdat    C*1  Dn  - Updated NC Block.
c
c           knc     I*4  D1  - Length of NC block.
c           kret: -1: NC block is bad
c                0: NC block is good but not sequenced
c                1: NC block is good and sequenced
c                2: NC block is good but no register (only message, OPSKIP chara
c
c***********************************************************************
c
      subroutine ptdf_seqctl (cdat,knc, kseqreg, kseqnum, kret)
c
      include 'menu.inc'
      include 'pted.inc'
      include 'post.inc'
c
      integer*4 knc, kseqreg, kseqnum, kret
c
      character*256 cdat
c
      equivalence (ISN   ,KPOSMP(0001))
      equivalence (IOPSKP,KPOSMP(0092)), (REGSW ,KPOSMP(0405))
      equivalence (NEOB  ,KPOSMP(0841)), (NCBMSG,KPOSMP(0843))
      equivalence (NCEMSG,KPOSMP(0844)), (NOPSKP,KPOSMP(1077))
      equivalence (NRWSTP,KPOSMP(1104)), (NEOT  ,KPOSMP(1106))
      equivalence (ICSMRG,KPOSMP(1132)), (NBOT  ,KPOSMP(1136))
      equivalence (FMTDES,KPOSMP(2133)), (IROTFT,KPOSMP(3110))
      equivalence (IFDRAD,KPOSMP(3210)), (IFDRCD,KPOSMP(3213))
c
      integer*4 REGSW(MAXFMT),NEOB,NCBMSG,NCEMSG,NOPSKP,NEOT,NBOT,
     1          ICSMRG,IFDRAD,IFDRCD(4),IROTFT,IOPSKP,ISN,
     2          NRWSTP
      integer*2 FMTDES(10,MAXFMT)
c
      equivalence (DUMMY ,POSMAP(0003)), (REGSTO,POSMAP(1032))
c
      real*8 DUMMY,REGSTO(MAXFMT)
c
      equivalence (LEOB  ,CPOSMP(0971)), (MSGST ,CPOSMP(0976))
      equivalence (MSGEN ,CPOSMP(1500)), (LOPSKP,CPOSMP(1648))
      equivalence (LRWSTP,CPOSMP(2201))
      equivalence (LEOT  ,CPOSMP(2211)), (LBOT  ,CPOSMP(2515))
c
      character*5 LEOB, LOPSKP
      character*10 LRWSTP,MSGST,MSGEN
      character*20 LEOT, LBOT

      integer*4 i,ix,lnc,jnc,nregs,lpartno,reglst(MAXFMT),
     1          oreg,rnc
c
      real*8 regvals(MAXFMT), gval
c
      character*8 cpartno
      character*80 creg
c
      integer*4 displyv /1021/
      integer*4 vinsert /1046/
      integer*4 offv    /72/
      integer*4 onv     /71/
      integer*4 opskipv /1012/
      integer*4 partnov /1045/
      integer*4 pprintv /1044/
      integer*4 preapt  /4048/
      integer*4 pstapt  /4049/
c
c...Initialize routine
c
      kret = 2
      SMSTOP = 0
      SMSTAT = 0
      TBLAST = TBLOCK
      MILAST = MINTER
      TBLOCK = PBLK_UNKNOWN
      if (MINTER .eq. PMOT_ROTARY) MINTER = PMOT_LINEAR
      jnc    = knc
      do 20 i=1,MAXFMT
        REGSW(i) = 0
        if (FMTDES(10,i) .eq. 4) REGSTO(i) = 0.
   20 continue
c
c...Get past leading blanks.
c
      ix  = 1
      do while (cdat(ix:ix).eq.' ')
          ix = ix + 1
          if (ix .gt. knc) goto 8000
      enddo
      lnc = knc - ix + 1
c
c...PARTNO
c
      call getvwd (partnov,cpartno,lpartno,1,PSTWRD,PSTWVL,NPSTWD)
      if (cdat(ix:ix+lpartno-1) .eq. cpartno(1:lpartno)) then
          goto 8000
      endif
c
c...OPSKIP character(s)
c
      if (IOPSKP .eq. 0) then
          if (NOPSKP .gt. 0 .and. NOPSKP .le. lnc) then
              if (cdat(ix:ix+NOPSKP-1) .eq. LOPSKP(1:NOPSKP)) then
                  IOPSKP = 1
                  ix  = ix + NOPSKP
                  if (ix .gt. knc) goto 8000
              endif
          endif
      else
          if (NOPSKP .gt. 0 .and. NOPSKP .le. lnc) then
              if (cdat(ix:ix+NOPSKP-1) .eq. LOPSKP(1:NOPSKP)) then
                  ix  = ix + NOPSKP
                  if (ix .gt. knc) goto 8000
              else
                  IOPSKP = 0
              endif
          endif
      endif
      do while (cdat(ix:ix).eq.' ')
          ix = ix + 1
          if (ix .gt. knc) goto 8000
      enddo
c
c...EOB characters
c
      lnc = knc - ix + 1
      if (lnc .ge. NEOB) then
          if (NEOB.gt.0) then
              if (cdat(knc-NEOB+1:knc).eq.LEOB(1:NEOB)) then
                  lnc = lnc-NEOB
                  jnc = jnc-NEOB
              endif
          endif
      endif
c
c...Rewind stop code
c
      if (lnc .ge. NRWSTP .and. NRWSTP .ne. 0) then
          if (cdat(ix:ix+NRWSTP-1) .eq. LRWSTP(1:NRWSTP)) then
              ix     = ix     + NRWSTP
              lnc    = lnc    - NRWSTP
          endif
      endif
c
c...Parse input block
c
      if (lnc .eq. 0) go to 8000
      kret = 0
      if (kseqnum.ne.-1) then
          call seqblk (cdat(ix:),lnc,kseqnum, kret)
          if (kret .eq. -1) go to 9000
          if (kret.eq.0 .and. kseqnum.ne.-1000) then
              gval = kseqnum
              call fmtincod (kseqreg,gval,oreg, creg,rnc)
              cdat(rnc+1:) = cdat(1:)
              cdat(1:rnc) = creg(1:rnc)
              kret = 1
          endif
c
c...make sure the first is not blank space
c
          ix = 1
          do while (cdat(ix:ix).eq.' ')
              ix = ix + 1
          enddo
          cdat(1:) = cdat(ix:)
      else
          call setblk (cdat(ix:),lnc,TBLOCK,MINTER,reglst,regvals,
     1                 nregs,1)
          if (nregs .lt. 0) go to 9000
      endif
c
c...End of routine
c
 8000 return
c
c...Invalid tape block
c
 9000 kret = -1
      goto 8000
      end

c***********************************************************************
c
c   SUBROUTINE:  ptdf_seqctl(cdat, knc, kseqreg, kseqnum, kret)
c
c   FUNCTION: Check if the NC block is valid and if it is replace the sequence
c			number if there is a sequest register
c
c   INPUT:  cdat    C*n  D1  - NC block
c
c           knc     I*4  D1  - Length of block
c           kseqreg: sequence register
c		  kseqnum: seqence number to be replaced,
c				'if kseqnum=-1: don't reseqence the block, just check if the block
c								is valid
c				'if kseqnum=-1000: remove the sequence register from block, return 1
c
c   OUTPUT: cdat    C*1  Dn  - Updated NC Block.
c
c           knc     I*4  D1  - Length of NC block.
c           kret: -1: NC block is bad
c                0: NC block is good but not sequenced
c                1: NC block is good and sequenced
c                2: NC block is good but no register (only message, OPSKIP chara
c
c***********************************************************************
c
      subroutine ptd_getparse_word (msgbblk, nc1, msgeblk, nc2, eob,
     2                nc3, opskip, nc4, tab, nc5, rwd, nc6)
c
      include 'menu.inc'
      include 'pted.inc'
      include 'post.inc'
c
      integer*4 nc1, nc2, nc3, nc4, nc5, nc6
c
      character*20 msgbblk, msgeblk, eob,  opskip, tab, rwd
c
      equivalence (IOPSKP,KPOSMP(0092))
      equivalence (NEOB  ,KPOSMP(0841)), (NCBMSG,KPOSMP(0843))
      equivalence (NCEMSG,KPOSMP(0844)), (NOPSKP,KPOSMP(1077))
      equivalence (NRWSTP,KPOSMP(1104))
c
      integer*4 NEOB,NCBMSG,NCEMSG,NOPSKP,NRWSTP,
     1          IOPSKP
c
      equivalence (LEOB  ,CPOSMP(0971)), (MSGST ,CPOSMP(0976))
      equivalence (MSGEN ,CPOSMP(1500)), (LOPSKP,CPOSMP(1648))
      equivalence (TABCHR,CPOSMP(0987)), (LRWSTP,CPOSMP(2201))
c
      character*1 TABCHR
      character*5 LEOB, LOPSKP
      character*10 LRWSTP,MSGST,MSGEN

      msgbblk(1:) = MSGST(1:NCBMSG)
      nc1 = NCBMSG
      msgeblk(1:) = MSGEN(1:NCEMSG)
      nc2 = NCEMSG
      eob(1:) = LEOB(1:NEOB)
      nc3 = NEOB
      opskip(1:) = LOPSKP(1:IOPSKP)
      nc4 = IOPSKP
      tab(1:1) = TABCHR
      nc5 = 1
      rwd(1:) = LRWSTP(1:NRWSTP)
      nc6 = NRWSTP
      return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  ptdf_getvwd (cbuf,knc,knum,kflag)
c
c   FUNCTION:  This routine receives as input a character string and
c              to see if it matches (from beginning)
c              any major/minor word, the whole string should match the Minor/Maj
c              word except Text Command could be partial match (because it could
c
c   INPUT:  cbuf    C*n  D1  -  Character string containing
c                               vocabulary word.
c           knc:    I*4  D1  - cbuf length
c
c   OUTPUT: knum    I*4  D1  -  A pointer within the array 'ktab' that
c                               matches the (partial) vocabulary word
c                               supplied in 'cbuf'.  Returns 0 if a
c                               match was not found.
c           kflag   I*4  D1  -  1: major word.
c                               2: minor word
c
c
c***********************************************************************
c
      subroutine ptdf_getvwd (cbuf,knc,knum,kflag)
c
      include 'menu.inc'
      include 'pted.inc'
      include 'post.inc'
c
      integer*4 knc, kflag,knum
c
      character*256 cbuf
c
      integer*4 nc,nc1,i,strlen1
c
      character*24 str
c
      call touppr (cbuf,str)
      nc1    = strlen1(str)
      if (nc1 .eq. 0) go to 9000
      nc1 = knc
c
c...check for text command first ('INSERT', 'PARTNO',....)
c
      if (nc1.ge.6) then
          if ((str(1:6).eq."INSERT").or.(str(1:6).eq."PPRINT")
     1      .or.(str(1:6).eq."TPRINT").or.(str(1:6).eq."REMARK")
     2      .or.(str(1:6).eq."LETTER").or.(str(1:6).eq."PARTNO")) then
              knum = 6
              kflag = 1
              return
          endif
      endif
c
c...Check for match
c
      knum   = 0
      do 200 i=1,NPSTWD,1
		nc =  strlen1(PSTWRD(i))
          if (nc1 .eq. nc .and. str(1:nc) .eq. PSTWRD(i)(1:nc)) then
              knum = nc
              if (PSTWVL(i).gt.0) then
                  kflag = 2
              else
                  kflag = 1
              endif
              return
          endif
  200 continue
c
c...No match found
c
 9000 knum   = 0
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  ptd_clssim
c
c   FUNCTION: Close the simulation file
c
c   INPUT:  None
c
c   OUTPUT: None
c
c***********************************************************************
c
      subroutine ptd_clssim()
c
      include 'menu.inc'
c
      call clsfil (LUNSC3)
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  ptd_getunit (kunit)
c
c   FUNCTION: Get MDF unit number
c
c   INPUT:  None
c
c   OUTPUT: kunit: 1: INCH
c				2: MM
c
c***********************************************************************
c
      subroutine ptd_getunit (kunit)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0309))
c
      integer*4 IUNIT
c
      integer*4 kunit
c
      kunit = IUNIT
      return
      end
