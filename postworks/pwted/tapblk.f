c
c***********************************************************************
c
c   FILE NAME:  tapblk
c   CONTAINS:
c       ptd_tapblk  tapblk    ptdf_clrreg  ptdf_regout  ptdf_init_convert
c       chkreg      ckaxes    gticdreg     gtocdreg     ckmotcd  ckcircd
c       ckinccd     ckabscd
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c       tapblk.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c       12/09/13 , 11:56:11
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_tapblk(cdat, outdat, knc)
c
c   FUNCTION:  This routine convert a input block string to
c         output format
c
c   INPUT:    cdat    C*n  D1   -  input block string
c             knc     I*4  D1   -  Number of characters in 'cdat'.
c
c   OUTPUT:  outdat   C*n  D1   -  output block string
c            knc      I*4  D1   -  Number of characters in 'outdat'.
c
c***********************************************************************
c
      subroutine ptd_tapblk(indat, outdat, knc)

C WNT-START
      integer*4 knc
      byte indat(256), outdat(256)
      character*256 cindat, coutdat
      byte bindat(256), boutdat(256)
      equivalence (cindat,bindat),(boutdat,coutdat)
      integer*4 i
C
      if (knc.gt.256) knc = 256
      do i=1,knc
        bindat(i) = indat(i)
      enddo
C
      cindat(knc+1:) = ' '
      call tapblk (cindat, coutdat, knc)
C
      if (knc.gt.256) knc = 256
      do i=1,knc
        outdat(i) = boutdat(i)
      enddo
C WNT-END
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     integer*4 knc
C     character*256 indat
C     character*256 outdat
C     call tapblk(indat, outdat, knc)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  tapblk(cdat, outdat, knc)
c
c   FUNCTION:  This routine convert a input block string to output format.
c
c   INPUT:    cdat    C*n  D1   -  input block string
c
c             knc     I*4  D1   -  Number of characters in 'cdat'.
c
c   OUTPUT:   cout    C*n  D1   -  output block string
c
c             knc     I*4  D1   -  Number of characters in 'cout'.
c
c***********************************************************************
c
      subroutine tapblk (cdat,cout,knc)
c
      include 'ptedpost.inc'
      include 'post.inc'
      include 'pted.inc'
      include 'menu.inc'
c
      integer*4 knc
c
      character*(*) cdat,cout
c
      equivalence (REGFRC,KPOSMP(0603)), (NEOBI ,KPOSMP(0841))
c
      integer*4 NEOBI,REGFRC(MAXFMT)
c
      equivalence (LEOBI ,  CPOSMP(0971))
c
      character*5 LEOBI
c
      equivalence (REGSW ,OKPOSMP(0405)), (NEOB  ,OKPOSMP(0841))
      equivalence (NOUTSW,OKPOSMP(1074))
      equivalence (IPARTO,OKPOSMP(1102)), (PCHSPC,OKPOSMP(1103))
      equivalence (FMTDES,OKPOSMP(2133))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 IPARTO,PCHSPC,REGSW(MAXFMT),NOUTSW,NEOB
c
      equivalence (REGSTO,OPOSMAP(1032))
c
      real*8 REGSTO(MAXFMT)
c
      equivalence (LEOB  ,  OCPOSMP(0971))
c
      character*5 LEOB
c
      equivalence (MSGEOBI,KPOSMP(0302)),(MSGEOBO,OKPOSMP(0302))
      equivalence (NCBMSGI,KPOSMP(0843)),(NCBMSGO,OKPOSMP(0843))
      equivalence (NCEMSGI,KPOSMP(0844)),(NCEMSGO,OKPOSMP(0844))
      equivalence (NOPSKPI,KPOSMP(1077)),(NOPSKPO,OKPOSMP(1077))
      equivalence (NRWSTPI,KPOSMP(1104)),(NRWSTPO,OKPOSMP(1104))
      equivalence (NBOTI  ,KPOSMP(1136)),(NBOTO,  OKPOSMP(1136))
      equivalence (NEOTI  ,KPOSMP(1106)),(NEOTO,  OKPOSMP(1106))
c
      integer*4 MSGEOBI,MSGEOBO,NCBMSGI,NCBMSGO,NCEMSGI,NCEMSGO,NOPSKPI,
     1          NOPSKPO,NBOTI,NBOTO,NEOTI,NEOTO,NRWSTPI,NRWSTPO
c
      equivalence (MSGSTI, CPOSMP(0976)),(MSGSTO, OCPOSMP(0976))
      equivalence (MSGENI, CPOSMP(1500)),(MSGENO, OCPOSMP(1500))
      equivalence (LOPSKPI,CPOSMP(1648)),(LOPSKPO,OCPOSMP(1648))
      equivalence (LRWSTPI,CPOSMP(2201)),(LRWSTPO,OCPOSMP(2201))
      equivalence (LEOTI,  CPOSMP(2211)),(LEOTO,  OCPOSMP(2211))
      equivalence (LBOTI,  CPOSMP(2515)),(LBOTO,  OCPOSMP(2515))
c
      character*5 LOPSKPI, LOPSKPO
      character*10 LRWSTPI, LRWSTPO
      character*10 MSGSTI, MSGSTO, MSGENI, MSGENO
      character*20 LEOTI, LEOTO, LBOTI, LBOTO
c
      integer*4 ix,lpartno,nc,tnc,lnc,reglst(MAXFMT),jnc,i,
     1          nregs,iefl
c
      real*8 regvals(MAXFMT)
c
      character*8  cpartno
      character*256 sbuf
c
      integer*4 partnov /1045/
      integer*4 preapt  /4048/
      integer*4 pstapt  /4049/
c
c...Initialize routine
c
      PCNV_TYPE = PCNV_CONVERT
      TBLAST = TBLOCK
      MILAST = MINTER
      TBLOCK = PBLK_UNKNOWN
      if (MINTER .eq. PMOT_ROTARY) MINTER = PMOT_LINEAR
      jnc    = knc
      tnc    = 0
      nc     = 0
      iefl   = 0
      do 20 i=1,MAXFMT,1
          REGSW(i) = 0
          REGFRC(i) = 0
          if (FMTDES(10,i) .eq. 4) REGSTO(i) = 0.
   20 continue
      cout = ' '
c
c...Get past leading blanks
c
      ix     = 1
      do while (cdat(ix:ix) .eq. ' ')
          ix     = ix     + 1
          tnc    = tnc    + 1
          if (ix .gt. knc) go to 8000
      enddo
      lnc = knc - ix + 1
c
c...Beginning-of-tape string
c
      if (NBOTI .gt. 0 .and. NBOTI .le. knc .and.
     1    cdat(ix:ix+NBOTI-1) .eq. LBOTI(1:NBOTI)) then
          knc = tnc
          if (NBOTO .gt. 0) then
              cout(ix:) = LBOTO(1:NBOTO)
              knc = tnc + NBOTO
          endif
          goto 8000
      endif
c
c...End-of-tape string
c
      if (NEOTI .gt. 0 .and. NEOTI .le. knc .and.
     1    cdat(ix:ix+NEOTI-1) .eq. LEOTI(1:NEOTI)) then
          knc = tnc
          if (NEOTO .gt. 0) then
              cout(ix:) = LEOTO(1:NEOTO)
              knc = tnc + NEOTO
          endif
          goto 8000
      endif
c
c...PARTNO
c
      call getvwd (partnov,cpartno,lpartno,1,PSTWRD,PSTWVL,NPSTWD)
      if (cdat(ix:lpartno+ix-1) .eq. cpartno(1:lpartno)) then
          if (IPARTO .eq. 1) then
              knc = tnc + knc - ix + 1
              cout(tnc+1:knc) = cdat(ix:knc)
              tnc = tnc + knc
          else
              knc = 0
          endif
          goto 8000
      endif
c
c...Opskip
c
      if (NOPSKPI .gt. 0 .and. NOPSKPI .le. knc-ix+1) then
          if (cdat(ix:ix+NOPSKPI-1) .eq. LOPSKPI(1:NOPSKPI)) then
              if (NOPSKPO.gt.0) then
                  cout(tnc+1:tnc+NOPSKPO) = LOPSKPO(1:NOPSKPO)
                  tnc = tnc + NOPSKPO
                  if (PCHSPC .eq. 1) then
                      tnc = tnc + 1
                      cout(tnc:tnc) = " "
                  endif
              endif
              ix = ix + NOPSKPI
          endif
      endif
c
c...Rewind stop code
c
      if (NRWSTPI .gt. 0 .and. NRWSTPI .le. knc-ix+1) then
          if (cdat(ix:ix+NRWSTPI-1) .eq. LRWSTPI(1:NRWSTPI)) then
              if (NRWSTPO.gt.0) then
                  cout(tnc+1:tnc+NRWSTPO) = LRWSTPO(1:NRWSTPO)
                  tnc = tnc + NRWSTPO
              endif
              ix = ix + NRWSTPO
          endif
      endif
c
c...Remove EOB characters
c
      if (NEOBI .gt. 0 .and. NEOBI .le. knc-ix+1) then
          if (cdat(knc-NEOBI+1:knc) .eq. LEOBI(1:NEOBI))
     1            knc = knc - NEOBI
          iefl = 1
      endif
c
c...Parse input block
c
      lnc    = knc - ix + 1
      if (lnc .eq. 0) go to 8000
      call setblk (cdat(ix:),lnc,TBLOCK,MINTER,reglst,regvals,nregs,2)
      if (nregs .lt. 0) then
          cout(tnc+1:) = cdat(ix:)
          tnc = tnc + lnc
          go to 8000
      endif
c
c...Copy input registers to output registers
c
      do 500 i=1,nregs,1
          REGSW(reglst(i)) = 1
          REGSTO(reglst(i)) = regvals(i)
  500 continue
c
c...Call pre-processing macro
c
      call ptdf_mactoapt (preapt)
c
c...Determine Rapid mode
c
      call rapida (reglst,regvals,nregs)
c
c...Recalculate motion blocks
c
      if (TBLOCK .eq. PBLK_MOTION) call mottap (reglst,regvals,nregs)
c
c...Call post-processing macro
c
      call ptdf_mactoapt (pstapt)
c
c...Output tape block
c
      NOUTSW = nregs
      call ptdf_clrreg (reglst,nregs,sbuf,nc)
      if (nc .gt. 0) then
          cout(tnc+1:) = sbuf(1:nc)
          tnc   = tnc    + nc
      endif
c
c...End of routine
c...Add end of block
c
 8000 if (NEOB .ne. 0 .and. (nc .ne. 0 .or. iefl .eq. 1)) then
          cout(tnc+1:) = LEOB(1:NEOB)
          tnc = tnc + NEOB
      endif
      knc = tnc
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptdf_clrreg (kregs,knreg,cout,knco)
c
c   FUNCTION:  This routine sets up an entire tape block for output.  It
c              also determines which registers will be output depending
c              on which have been set and their control code.
c
c   INPUT:    kregs   I*4  Dn   -  List of registers in current block.
c
c             knreg   I*4  Dn   -  Number of registers in list.
c
c   OUTPUT:   cout    C*n  D1   -  Output block string
c
c             knco    I*4  D1   -  Number of characters in 'cout'.
c
c***********************************************************************
c
      subroutine ptdf_clrreg (kregs,knreg,cout,knco)
c
      include 'ptedpost.inc'
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 knco,kregs(MAXFMT),knreg
c
      character*(*) cout
c
      equivalence (REGFRC,KPOSMP(0603)), (PRTBNC,KPOSMP(1153))
c
      integer*4 PRTBNC,REGFRC(MAXFMT)
c
      equivalence (PRTBLK,CPOSMP(1653))
c
      character*512 PRTBLK
c
      equivalence (REGSW ,OKPOSMP(0405)), (IREGST,OKPOSMP(0497))
      equivalence (IRGOUT,OKPOSMP(0695)), (TABSEQ,OKPOSMP(0842))
      equivalence (NCBMSG,OKPOSMP(0843)), (NCEMSG,OKPOSMP(0844))
      equivalence (LSTREG,OKPOSMP(0857)), (MACPRG,OKPOSMP(0858))
      equivalence (NOUTSW,OKPOSMP(1074))
      equivalence (ICSMFL,OKPOSMP(1133)), (REGBNC,OKPOSMP(2001))
      equivalence (FMTDES,OKPOSMP(2133)), (REGENC,OKPOSMP(3522))
      equivalence (IREGVL,OKPOSMP(3614)), (REGORD,OKPOSMP(3707))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 REGBNC(MAXFMT),REGENC(MAXFMT),REGORD(MAXFMT),
     1          TABSEQ,IREGVL(MAXFMT),IREGST(MAXFMT),
     2          ICSMFL,IRGOUT(MAXFMT),LSTREG,MACPRG,REGSW(MAXFMT),
     3          NOUTSW,NCBMSG,NCEMSG
c
      equivalence (REGSTO,OPOSMAP(1032)), (REGVAL,OPOSMAP(5000))
c
      real*8 REGSTO(MAXFMT),REGVAL(MAXFMT)
c
      equivalence (MSGST ,OCPOSMP(0976)), (TABCHR,OCPOSMP(0987))
      equivalence (MSGEN ,OCPOSMP(1500))
c
      character*1 TABCHR
      character*10 MSGST,MSGEN
c
      integer*4 i,inc,irinc,irsav(MAXFMT),nc,isq,minc
c
      character*30 sbuf
c
c...If there is a message block to be output
c...Determine where it goes
c
      minc = 0
      do 100 i=1,knreg,1
          if (kregs(i) .eq. -1000) then
              if (i .eq. knreg) then
                  minc = -1
              else
                  minc = kregs(i+1)
              endif
          endif
  100 continue
c
c...Loop through register arrays
c...to determine which ones need
c...to be output
c
      knco  = 0
      cout  = ' '
      if (NOUTSW .eq. 0) go to 8000
      irinc = 0
      isq   = 0
      do 1000 i=1,MAXFMT,1
          inc    = REGORD(i)
          IRGOUT(inc) = 0
          REGVAL(inc) = REGSTO(inc)
          call codint (inc,REGVAL(inc),REGVAL(inc),IREGVL(inc))
c
c......NOT USED
c
          if (FMTDES(10,inc) .eq. 0) then
              REGFRC(inc) = 0
              go to 1000
          endif
c
c......ALWAYS
c
          if (FMTDES(10,inc) .eq. 1) REGSW(inc) = 1
c
c......CHANGED
c
          if (REGSW(inc) .eq. 1) then
              if (FMTDES(10,inc) .eq. 3) then
                  if (IREGVL(inc) .eq. IREGST(inc)) REGSW(inc) = 0
c
c......NON ZERO
c
              else if (FMTDES(10,inc) .eq. 4) then
                  if (IREGVL(inc) .eq. 0) REGSW(inc) = 0
c
c......MOTION
c
              else if (FMTDES(10,inc) .eq. 5 .and.
     1                 TBLOCK .ne. PBLK_MOTION) then
                  irinc  = irinc  + 1
                  irsav(irinc) = inc
                  REGSW(inc) = 0
              endif
          endif
c
c......Register is to be output
c......Add Tab character
c
          if (TABSEQ .ne. 1) call blkout (TABCHR,1,0)
c
c......Force register output
c
          if (REGSW(inc) .eq. 0) then
              if (REGFRC(inc) .eq. 3) then
                  if (TBLOCK .eq. PBLK_MOTION) then
                      call codint (inc,REGVAL(inc),REGVAL(inc),
     1                             IREGVL(inc))
                      REGSW(inc) = 1
                  endif
              else if (REGFRC(inc) .gt. 0) then
                  if (FMTDES(10,inc) .eq. 5 .and.
     1                TBLOCK .ne. PBLK_MOTION) go to 1000
                  call codint (inc,REGVAL(inc),REGVAL(inc),IREGVL(inc))
                  REGSW(inc) = 1
              endif
c
c......Suppress register output
c
          else
              if (REGFRC(inc) .eq. -1 .or. REGFRC(inc) .eq. -2)
     1                REGSW(inc) = 0
          endif
          if (REGFRC(inc) .eq. -1) REGFRC(inc) = 0
          if (REGSW(inc) .eq. 0) go to 1000
c
c......Do not output block if
c......only parameter definition
c......codes (SEQ #'s, etc) are being output
c
          if (REGFRC(inc) .ne. 4) isq = 1
c
c......Clear register force flag
c
          if (REGFRC(inc) .eq. 1 .or. REGFRC(inc) .eq. 3 .or.
     1        REGFRC(inc) .eq. 4 .or. REGFRC(inc) .eq. -1)
     2            REGFRC(inc) = 0
c
c......Output message block if it's time
c
          if (minc .eq. inc) then
              call ptdf_regout (cout,knco,MSGST,NCBMSG,1)
              call ptdf_regout (cout,knco,PRTBLK,PRTBNC,0)
              call ptdf_regout (cout,knco,MSGEN,NCEMSG,0)
              minc = 0
          endif
c
c......Format register for output and
c......Add this register to Control Tape
c
          call fmtocod (inc,REGVAL(inc),sbuf,nc)
          call ptdf_regout (cout,knco,sbuf,nc,1)
c
c......Store this registers values
c
          REGSTO(inc) = REGVAL(inc)
          IREGST(inc) = IREGVL(inc)
          REGSW(inc) = 0
          IRGOUT(inc) = 1
c
c......Output Macro Parameters if a
c......Macro call is being output and
c......this is the correct position
c
cc          if (inc .eq. MACPRG) call mbkout
 1000 continue
c
c......Output message block if it's not been output yet
c
      if (minc .ne. 0) then
          call ptdf_regout (cout,knco,MSGST,NCBMSG,1)
          call ptdf_regout (cout,knco,PRTBLK,PRTBNC,0)
          call ptdf_regout (cout,knco,MSGEN,NCEMSG,0)
          minc = 0
      endif
      if (knco .eq. 0) go to 1200
c
c...Output checksum code
c
cc      if (ICSMFL .eq. 1) call chksum
c
c...Sequence number is only code being output
c...Suppress the output
c
      if (isq .eq. 0) knco = 0
c
c...Reset Motion only registers
c
 1200 do 1300 i=1,irinc,1
          REGSW(irsav(i)) = 1
 1300 continue
      NOUTSW = irinc
cc      MACPRG = -1
      LSTREG = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptdf_regout (cout,knco,cin,knc,kspc)
c
c   FUNCTION:  This routine places the input register string into the
c              output tape block and formats it according to the output
c              options (spaces, etc.).
c
c   INPUT:    cin     C*n  D1   -  Input register string.
c
c             knc     I*4  Dn   -  Number of characters in 'cin'.
c
c             kspc    I*4  Dn   -  1 = Add separating space if supported
c                                  in MDF file.
c
c   OUTPUT:   cout    C*n  D1   -  Output block string
c
c             knc     I*4  D1   -  Number of characters in 'cout'.
c
c***********************************************************************
c
      subroutine ptdf_regout (cout,knco,cin,knc,kspc)
c
      include 'ptedpost.inc'
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 knco,knc,kspc
c
      character*(*) cout,cin
c
      equivalence (PCHSPC,OKPOSMP(1103))
c
      integer*4 PCHSPC
c
      equivalence (LEDCHR,OCPOSMP(0986)), (TABCHR,OCPOSMP(0987))
c
      character*1 TABCHR,LEDCHR
c
c...If string is empty, do nothing
c
      if (knc .eq. 0) go to 8000
c
c...Add space to output block
c...if necessary
c
      if (PCHSPC .eq. 1 .and. knco .ne. 0 .and. kspc .eq. 1 .and.
     1    cout(knco:knco) .ne. TABCHR .and.
     2    cout(knco:knco) .ne. LEDCHR .and.
     3    cin(1:1) .ne. TABCHR .and. cin(1:1) .ne. LEDCHR) then
          knco   = knco   + 1
          cout(knco:knco) = LEDCHR
      endif
c
c...Append register string
c
      cout(knco+1:) = cin
      knco   = knco   + knc
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptdf_init_convert
c
c   FUNCTION:  This routine initializes the variables required for
c              tape-to-tape conversions.
c
c   INPUT:    none
c
c   OUTPUT:   none
c
c***********************************************************************
c
      subroutine ptdf_init_convert
c
      include 'menu.inc'
      include 'ptedpost.inc'
c
      equivalence (IDUMMY,OKPOSMP(0088)), (IREGST,OKPOSMP(0497))
c
      integer*4 IREGST(MAXFMT),IDUMMY
c
      integer*4 i
c
c...Initialize initial register settings
c
      do 100 i=1,MAXFMT,1
          IREGST(i) = IDUMMY
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   E-FUNCTION:   chkreg(i, gval, regstr, knc, bcase, kerr)
c
c   FUNCTION:  This routine check if register is a correct register
c
c   INPUT:  gval    C*n  D1  -  register string
c
c   OUTPUT: strlen  I*4  D1  -  Length of 'cstr'.
c
c***********************************************************************
c
      subroutine  chkreg(inreg, gval, regstr, knc, bcase, kerr)
      include 'post.inc'
      include 'pted.inc'
      include 'menu.inc'
c
      integer*4 kerr, knc, inreg, bcase
      character*(*) regstr
      real*8 gval
c
      equivalence (FMTDES,KPOSMP(2133))
c
      integer*2 FMTDES(10,MAXFMT)

      equivalence (INCR  ,KPOSMP(1226))
      equivalence (CIRCDV,POSMAP(2206)), (CIRCOD,KPOSMP(1378))
      equivalence (MOTCOD,KPOSMP(1240)), (ABSCOD,KPOSMP(1241))
      equivalence (INCCOD,KPOSMP(1242)), (MOTCDV,POSMAP(1284))
      equivalence (ABSCDV,POSMAP(1285)), (INCCDV,POSMAP(1286))
      equivalence (MOTREG,KPOSMP(0381)), (IJKREG,KPOSMP(0827))
      equivalence (LTPREG,KPOSMP(4201)), (LTCREG,KPOSMP(4195))
      equivalence (NUMLIN,KPOSMP(1202)), (IJKROT,KPOSMP(1739))

      equivalence (LTHPCL,KPOSMP(1899)), (MACHTP,KPOSMP(1201))

      integer*4 MOTREG(24), LTCREG(3), LTPREG(2), IJKREG(3)
      integer*4 NUMLIN(3),IJKROT, MACHTP, LTHPCL(2)
      integer*4 MOTCOD,ABSCOD,INCCOD, CIRCOD, INCR
      integer*4 inc(24),isub(12), outreg, kreg, choice

      real*8 CIRCDV, MOTCDV,ABSCDV,INCCDV

      data inc  /1,1,1,1, 2,2,2,2, 3,3,3,3, 1,1,1, 2,2,2, 3,3,3, 4,4,4/
      data isub /1,1,2,2, 1,1,2,2, 1,1,2,2/

      kerr = 1
      kreg = inreg
c
c...Register "NOT USED"
c
      if (FMTDES(10,inreg).eq.0) then
        kerr = -1
        return
      endif
c
c...check for special register which include
c..."Linear, Circular, Absolute anc Incremental Codes'
c
      call ckmotcd(regstr, outreg, knc, bcase, kerr)
      if (kerr.eq.0) then
        MINTER = PMOT_LINEAR
        inreg = outreg
        kerr = 0
        return
      endif

      call ckabscd(regstr, outreg, knc, bcase, kerr)
      if (kerr.eq.0) then
        INCR = 1
        inreg = outreg
        kerr = 0
        return
      endif

      call ckinccd(regstr, outreg, knc, bcase, kerr)
      if (kerr.eq.0) then
        INCR = 2
        inreg = outreg
        kerr = 0
        return
      endif

      call ckcircd(regstr, outreg, knc, bcase, choice, kerr)
      if (kerr.eq.0) then
          MINTER = PMOT_CIRCUL
          inreg = outreg
          kerr = 0
          return
      endif

c
c...see if register matched correct linear/rotary
c...axes register
c

      call ckaxes(regstr, kreg, gval, knc, bcase, choice, kerr)
      if (kerr.eq.0) then
          TBLOCK = PBLK_MOTION
          inreg = kreg
          kerr = 0
          return
      endif

      kerr = 1
      return
      end
c***********************************************************************
c
c   E-FUNCTION:   ckaxes(regstr, outreg, gval, knc, bcase, choice, kerr)
c
c   FUNCTION:  This routine check if register is a axes register
c
c   INPUT:  gval    C*n  D1  -  register string
c
c   OUTPUT: strlen  I*4  D1  -  Length of 'cstr'.
c
c***********************************************************************
c
      subroutine  ckaxes(regstr, inreg, ingval, knc, bcase,
     1                     choice, kerr)
      include 'post.inc'
      include 'pted.inc'
      include 'menu.inc'
c
      integer*4 kerr, knc, inreg, bcase, choice
      character*(*) regstr
      real*8 ingval
      equivalence (INCR  ,KPOSMP(1226))
      equivalence (MOTREG,KPOSMP(0381)), (IJKREG,KPOSMP(0827))
      equivalence (LTPREG,KPOSMP(4201)), (LTCREG,KPOSMP(4195))
      equivalence (NUMLIN,KPOSMP(1202)), (IRTDEF,KPOSMP(1485))
      equivalence (IJKROT,KPOSMP(1739))
      equivalence (LTHPCL,KPOSMP(1899)), (MACHTP,KPOSMP(1201))
      equivalence (AXSOUT,POSMAP(1340))

      real*8 AXSOUT(10),  gval
      integer*4 MOTREG(24), LTCREG(3), LTPREG(2), IJKREG(3)
      integer*4 NUMLIN(3),IRTDEF, IJKROT, MACHTP, LTHPCL(2)
      integer*4 INCR
      integer*4 iwrn,i,inc(24),isub(12), kreg

      data inc  /1,1,1,1, 2,2,2,2, 3,3,3,3, 1,1,1, 2,2,2, 3,3,3, 4,4,4/
      data isub /1,1,2,2, 1,1,2,2, 1,1,2,2/

      kreg = inreg
      gval = ingval
c
c...see if register matched correct linear/rotary
c...axes register
c
      if (INCR .eq. 1) then
          i = 1
c
c......Verify that this machine has this axis
c
  110     iwrn   = 0
          if (i .le. 12) then
              if (NUMLIN(inc(i)) .lt. isub(i)) iwrn = 1
          else
              if (inc(i) .gt. IRTDEF) iwrn = 1
              if (i .ne. (10+inc(i)*3) .and. IJKROT .eq. 1) iwrn = 1
          endif
          if ((iwrn .ne. 1).and.(MOTREG(i).eq.kreg)) then
              TBLOCK = PBLK_MOTION
              choice = i
              kerr = 0
c
c....save this axis value
c
              if (i.eq.1) then
                  AXSOUT(1) = gval
              else if (i.eq.3) then
                  AXSOUT(2) = gval
              else if (i.eq.5) then
                  AXSOUT(3) = gval
              else if (i.eq.7) then
                  AXSOUT(4) = gval
              else if (i.eq.9) then
                  AXSOUT(5) = gval
              else if (i.eq.11) then
                  AXSOUT(6) = gval
              else if (i.eq.14) then
                  AXSOUT(7) = gval
              else if (i.eq.17) then
                  AXSOUT(8) = gval
              else if (i.eq.20) then
                  AXSOUT(9) = gval
              else if (i.eq.23) then
                  AXSOUT(10) = gval
              endif
              return
          endif
          if (i.le.10) then
              i = i + 2
              goto 110
          else if (i.le.23) then
              i = i + 3
              goto 110
          endif

          if (MACHTP .ne. 4 .or. LTHPCL(1) .ne. 1) then
              goto 200
          else if (LTPREG(1).eq.kreg) then
              TBLOCK = PBLK_MOTION
              choice = 28
              kerr = 0
              return
          endif

  200     if (MACHTP .ne. 4 .or. LTHPCL(2) .ne. 1) then
             goto 400
          else if (LTCREG(2).eq.kreg) then
             TBLOCK = PBLK_MOTION
             choice = 31
             kerr = 0
             return
         endif
      endif


      if (INCR .eq. 2) then

            i = 2
c
c......Verify that this machine has this axis
c
  210     iwrn   = 0
          if (i .le. 12) then
              if (NUMLIN(inc(i)) .lt. isub(i)) iwrn = 1
          else
              if (inc(i) .gt. IRTDEF) iwrn = 1
              if (i .ne. (10+inc(i)*3) .and. IJKROT .eq. 1) then
                  iwrn = 1
              endif
          endif
          if ((iwrn .ne. 1).and.(MOTREG(i).eq.kreg)) then
              TBLOCK = PBLK_MOTION
              choice = i
              kerr = 0
c
c....save this axis value
c
              if (i.eq.2) then
                  AXSOUT(1) = AXSOUT(1) + gval
              else if (i.eq.4) then
                  AXSOUT(2) = AXSOUT(2) + gval
              else if (i.eq.6) then
                  AXSOUT(3) = AXSOUT(3) + gval
              else if (i.eq.8) then
                  AXSOUT(4) = AXSOUT(4) + gval
              else if (i.eq.10) then
                  AXSOUT(5) = AXSOUT(5) + gval
              else if (i.eq.12) then
                  AXSOUT(6) = AXSOUT(6) + gval
              else if (i.eq.15) then
                  AXSOUT(7) = AXSOUT(7) + gval
              else if (i.eq.18) then
                  AXSOUT(8) = AXSOUT(8) + gval
              else if (i.eq.21) then
                  AXSOUT(9) = AXSOUT(9) + gval
              else if (i.eq.24) then
                  AXSOUT(10) = AXSOUT(10) + gval
              endif
              return
          endif
          if (i.le.11) then
              i = i + 2
              goto 210
          else if (i.le.23) then
              i = i + 3
              goto 210
          endif

          if (MACHTP .ne. 4 .or. LTHPCL(1) .ne. 1) then
             goto 300
          else if (LTPREG(2).eq.kreg) then
             TBLOCK = PBLK_MOTION
             choice = 29
             kerr = 0
             return
          endif

  300     if (MACHTP .ne. 4 .or. LTHPCL(2) .ne. 1) then
             goto 400
         else if (LTCREG(3).eq.kreg) then
             TBLOCK = PBLK_MOTION
             choice = 32
             kerr = 0
             return
         endif
      endif

  400 do 500 i=1, 3, 1
          if ((IJKROT .ne. 2) .and. (IJKREG(i).eq.kreg)) then
              TBLOCK = PBLK_MOTION
              choice = 24 + i
              kerr = 0
              return
          endif
  500 continue
      kerr = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gticdreg(kcod,codval, outreg, codstr, knc)
c
c   FUNCTION:  This routine search a register in the G, M...code group
c         and find a correct register and formated followed input format
c
c   INPUT:  kcod     I*4  D1  -  -1 = G-code, -2 = M-code,
c                                -3 = (EOB), -4-12 = XUYVZWABC@, -13 =
c                                F.
c
c           gval     R*8  D1  -  Code value.
c
c   OUTPUT: codstr   C*n  D1  -  formated letter addess
c
c           knc      I*4  D1  -  Number of chars in 'codstr'.
c           outreg:  I*4  D1  -  correct register number
c
c***********************************************************************
c
      subroutine gticdreg(kcod, codval, outreg, codstr, knc)

      include 'post.inc'
      include 'menu.inc'

      integer*4   kcod, outreg, knc
      real*8      codval
      character*(*) codstr
      integer*4 i, j
      character*60 obuf

      equivalence (DUMMY, POSMAP(0003)),  (GRANGE,POSMAP(0801))
      equivalence (MRANGE,POSMAP(0955))
      real*8 DUMMY,GRANGE(14,11), MRANGE(7,11)

      equivalence (REGBNC,KPOSMP(2001))
      equivalence (FMTDES,KPOSMP(2133)), (REGENC,KPOSMP(3522))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 REGBNC(MAXFMT),REGENC(MAXFMT)
c
      equivalence (REGST ,CPOSMP(7011)), (REGEN ,CPOSMP(9219))
c
      character*24 REGST(MAXFMT),REGEN(MAXFMT)

      if (kcod .eq. -1) goto 100
      if (kcod .eq. -2) goto 250

  100 do 200 i=1,11,1
c
c......G-codes
c
          do 110 j=1,14,1
            if (GRANGE(j,i) .eq. DUMMY) go to 150
            if (GRANGE(j,i) .eq. codval) then
               outreg = 17 + i
               goto 800
            endif
  110    continue
  150    outreg = -1
  200 continue
c
c...if can't find this register in G-code group
c...use G0
c
      outreg = 17
      goto 800
  250 do 350 i=1,11,1
c
c...M-codes
c
          do 300 j=1,7,1
             if (MRANGE(j,i) .eq. DUMMY) go to 320
             if (MRANGE(j,i) .eq. codval) then
                outreg = 36 + i
                goto 800
             endif
  300    continue
  320    outreg = -1
  350 continue
c
c...if can't find this register in M-code group
c...use M0
c
      outreg = 36
  800 if (codval.ne.DUMMY) then
          call ftoc (codval,codstr,knc,FMTDES(1,outreg))
      else
          knc = 0
      endif
      if (REGBNC(outreg) .ne. 0) then
          if (knc .ne. 0) then
             obuf   = REGST(outreg)(1:REGBNC(outreg)) // codstr(1:knc)
          else
             obuf   = REGST(outreg)(1:REGBNC(outreg))
          end if
          codstr(1:30)   = obuf
          knc    = knc    + REGBNC(outreg)
      endif
c
      if (REGENC(outreg) .ne. 0) then
          if (knc .ne. 0) then
             obuf   = codstr(1:knc) // REGEN(outreg)(1:REGENC(outreg))
          else
             obuf   = REGEN(outreg)(1:REGENC(outreg))
          end if
          knc    = knc    + REGENC(outreg)
          codstr(1:30)   = obuf
      endif

      return
      end


c
c***********************************************************************
c
c   SUBROUTINE:  gtocdreg (kcod,codval,outreg)
c
c   FUNCTION:  This routine search a register in the G, M...code group
c              and find a correct register.
c
c
c   INPUT:  kcod    I*4  D1  -  -1 = G-code, -2 = M-code,
c                               -3 = (EOB), -4-12 = XUYVZWABC@, -13 =
c                               F.
c
c           gval    R*8  D1  -  Code value.
c
c   OUTPUT: outreg  I*4  D1  -  correct register number
c
c***********************************************************************
c
      subroutine gtocdreg (kcod,codval,outreg)

      include 'ptedpost.inc'
      include 'menu.inc'

      integer*4 kcod, outreg
      real*8 codval
      integer*4 i, j

      equivalence (DUMMY, OPOSMAP(0003)),  (GRANGE,OPOSMAP(0801))
      equivalence (MRANGE,OPOSMAP(0955))
      real*8 DUMMY,GRANGE(14,11), MRANGE(7,11)

      equivalence (REGBNC,OKPOSMP(2001))
      equivalence (FMTDES,OKPOSMP(2133)), (REGENC,OKPOSMP(3522))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 REGBNC(MAXFMT),REGENC(MAXFMT)
c
      equivalence (REGST ,OCPOSMP(4147)), (REGEN ,OCPOSMP(5067))
c
      character*10 REGST(MAXFMT),REGEN(MAXFMT)

      if (kcod .eq. -1) goto 100
      if (kcod .eq. -2) goto 250
      outreg = -1
      go to 8000
c
  100 do 200 i=1,11,1
c
c......G-codes
c
          do 110 j=1,14,1
            if (GRANGE(j,i) .eq. DUMMY) go to 150
            if (GRANGE(j,i) .eq. codval) then
               outreg = 17 + i
               goto 8000
            endif
  110    continue
  150    outreg = -1
  200 continue
c
c...if can't find this register in G-code group
c...use G0
c
      outreg = 17
      goto 8000
  250 do 350 i=1,11,1
c
c...M-codes
c
          do 300 j=1,14,1
             if (MRANGE(j,i) .eq. DUMMY) go to 320
             if (MRANGE(j,i) .eq. codval) then
                outreg = 36 + i
                goto 8000
             endif
  300     continue
  320     outreg = -1
  350 continue
c
c...if can't find this register in M-code group
c...use M0
c
      outreg = 36

 8000 return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  ckmotcd(incod, cdreg, knc, bcase, kerr)
c
c   FUNCTION:  This routine check if a letter address is a Linear
c                motion code, and return correct register number
c
c
c   INPUT:  incod   C*n  D1   -  input letter address to be checked
c
c           bcase    R*8  D1  -  if consider case sensitive
c           knc               -  number of chars in 'incod'
c
c   OUTPUT: cdreg             - correct register number for set
c                               Linear motion mode
c
c           kerr     I*4  D1   - 0. it is motion code
c
c***********************************************************************
c
      subroutine ckmotcd(incod, cdreg, knc, bcase, kerr)

      include 'post.inc'
      include 'menu.inc'

      character*(*) incod
      integer*4 cdreg, knc, bcase, kerr
      character*80 codstr

      equivalence (MOTCOD,KPOSMP(1240)), (MOTCDV,POSMAP(1284))
      integer*4 MOTCOD, matched
      real*8 MOTCDV

      integer*4 rnc
      integer*4 flstrcmp, flstrcmpi

      call fmtincod(MOTCOD,MOTCDV, cdreg, codstr, rnc)
      if (knc.eq.rnc) then
      if (bcase.eq.1) then
         matched = flstrcmp(incod, codstr,knc)
      else
         matched = flstrcmpi(incod, codstr, knc)
      endif
      if (matched.eq.1) then
         kerr = 0
         return
      endif
      endif

      kerr = 1
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  ckcircd(incod, cdreg, knc, bcase, choice, kerr)
c
c   FUNCTION:  This routine check if a letter address is a Circular
c              interplolation code, and return correct register number
c
c   INPUT:  incod    C*n  D1   -  input letter address to be checked
c
c           bcase    R*8  D1   -  if consider case sensitive
c           knc                -  number of chars in 'incod'
c
c   OUTPUT: cdreg              - correct register number for set
c                                Circular motion mode
c           kerr     I*4  D1   - 0. it is motion code
c
c***********************************************************************
c
      subroutine ckcircd(incod, cdreg, knc, bcase, choice, kerr)

      include 'post.inc'
      include 'menu.inc'


      character*(*) incod
      integer*4 cdreg, knc, bcase, kerr, choice

      character*80 codstr

      equivalence (MACHTP,KPOSMP(1201)), (ICIRFL,KPOSMP(1367))
      equivalence (CIRCOD,KPOSMP(1378)), (LTHPCL,KPOSMP(1899))
      equivalence (LFIRCD,KPOSMP(4112)), (LDIRCD,KPOSMP(4118))
      equivalence (ICRHEL,KPOSMP(4250))
c
      integer*4 ICIRFL,CIRCOD(6),MACHTP,LFIRCD(6),LDIRCD(6),
     1          LTHPCL(2),ICRHEL(10)
c
      equivalence (DUMMY ,POSMAP(0003)), (CIRCDV,POSMAP(2206))
      equivalence (CFRCDV,POSMAP(4595))
      equivalence (CDRCDV,POSMAP(4597)), (HELCDV,POSMAP(2242))
c
      real*8 DUMMY,CIRCDV(2),CFRCDV(2),
     1       CDRCDV(2),HELCDV(2)

      integer*4 rnc
      integer*4 flstrcmp, flstrcmpi, matched
      integer*4 iwrn,inum,ist,i,inc,n,ix
      real*8 rnum

c
c...Get Circular direction codes
c
      ist    = 3
      inc    = ist    - 3
      if (ICIRFL .ne. 1) then
          kerr = 1
          return
      endif
      do 200 i=ist,10,1
          n      = inc / 4
          ix     = n + 1
          inc    = inc    + 1
          n      = inc - 4 * n
          iwrn   = 0
          if (n .gt. 1 .and. n .lt. 4 .and. (MACHTP .eq. 4 .and.
     -          LTHPCL(n-1) .ne. 1 .or. MACHTP .ne. 4)) iwrn = 1
          if (n .eq. 4 .and. ICRHEL(1) .ne. 1) iwrn = 1
          if (iwrn .eq. 1) goto 200
          if (n .eq. 1) then
              inum   = CIRCOD(ix)
              rnum   = CIRCDV(ix)
          else if (n .eq. 2) then
              inum   = LFIRCD(ix)
              rnum   = CFRCDV(ix)
          else if (n .eq. 3) then
              inum   = LDIRCD(ix)
              rnum   = CDRCDV(ix)
          else
              inum   = ICRHEL(3+ix)
              rnum   = HELCDV(ix)
          endif

          call fmtincod(inum, rnum, cdreg, codstr, rnc)

          if (knc.eq.rnc) then
              if (bcase.eq.1) then
                  matched = flstrcmp(incod, codstr,knc)
              else
                  matched = flstrcmpi(incod, codstr, knc)
              endif
              if (matched.eq.1) then
                  choice = i
                  kerr = 0
                  return
              endif
          endif
  200 continue

      kerr = 1
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  ckinccd(incod, cdreg, knc, bcase, kerr)
c
c   FUNCTION:  This routine check if a letter address is a Incremental
c              position code, and return correct register number
c
c
c   INPUT:  incod    C*n  D1   -  input letter address to be checked
c
c           bcase    R*8  D1   -  if consider case sensitive
c           knc                -  number of chars in 'incod'
c
c   OUTPUT: cdreg              - correct register number for set
c                                Incremental mode
c
c           kerr     I*4  D1   - 0. it is Incremental code
c
c***********************************************************************
c
      subroutine ckinccd(incod, cdreg, knc, bcase, kerr)

      include 'post.inc'
      include 'menu.inc'


      character*(*) incod
      integer*4 cdreg, knc, bcase, kerr
      character*80 codstr

      equivalence (INCCOD,KPOSMP(1242)), (INCCDV,POSMAP(1286))

      integer*4 INCCOD, matched
      real*8 INCCDV

      integer*4 rnc
      integer*4 flstrcmp, flstrcmpi

      call fmtincod(INCCOD,INCCDV, cdreg, codstr, rnc)

      if (knc.eq.rnc) then
      if (bcase.eq.1) then
         matched = flstrcmp(incod, codstr,knc)
      else
         matched = flstrcmpi(incod, codstr, knc)
      endif
         if (matched.eq.1) then
            kerr = 0
            return
         endif
      endif

      kerr = 1
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  ckabscd(incod, cdreg, knc, bcase, kerr)
c
c   FUNCTION:  This routine check if a letter address is a Absolute
c              position code, and return correct register number
c
c   INPUT:  incod   C*n  D1    -  input letter address to be checked
c
c           bcase    R*8  D1   -  if consider case sensitive
c           knc                   number of chars in 'incod'
c
c   OUTPUT: cdreg              - correct register number for set
c                                Absolute mode
c           kerr     I*4  D1   - 0. it is Incremental code
c
c***********************************************************************
c
      subroutine ckabscd(incod, cdreg, knc, bcase, kerr)

      include 'post.inc'
      include 'menu.inc'


      character*(*) incod
      integer*4 cdreg, knc, bcase, kerr
      character*80 codstr

      equivalence (ABSCDV,POSMAP(1285)), (ABSCOD,KPOSMP(1241))

      integer*4 ABSCOD, matched
      real*8 ABSCDV

      integer*4 rnc
      integer*4 flstrcmp, flstrcmpi

      call fmtincod(ABSCOD,ABSCDV, cdreg, codstr, rnc)

      if (knc.eq.rnc) then
      if (bcase.eq.1) then
         matched = flstrcmp(incod, codstr,knc)
      else
         matched = flstrcmpi(incod, codstr, knc)
      endif
         if (matched.eq.1) then
            kerr = 0
            return
         endif
      endif

      kerr = 1
      return
      end
