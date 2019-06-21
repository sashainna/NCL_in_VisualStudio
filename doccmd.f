c
c***********************************************************************
c
c   FILE NAME:  doccmd.for
c   CONTAINS:
c               doccmd
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        doccmd.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 08:37:58
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  doccmd (cmsg,kerr)
c
c   FUNCTION:  This routine outputs the automatic documentation Command
c              Summary pages.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine doccmd (cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCREG,KPOSMP(0231))
      equivalence (MCHOPT,KPOSMP(0308)), (PODACD,KPOSMP(0372))
      equivalence (PODCCD,KPOSMP(0379)), (PODSCD,KPOSMP(0818))
      equivalence (NCBMSG,KPOSMP(0843)), (NCEMSG,KPOSMP(0844))
      equivalence (SEQCOD,KPOSMP(0847)), (ALNCOD,KPOSMP(0848))
      equivalence (AL1BLK,KPOSMP(0849)), (AL2BLK,KPOSMP(0850))
      equivalence (OPSTCD,KPOSMP(1075)), (STOPCD,KPOSMP(1076))
      equivalence (NOPSKP,KPOSMP(1077))
      equivalence (REWCOD,KPOSMP(1080)), (NRWSTP,KPOSMP(1104))
      equivalence (ICSMRG,KPOSMP(1132)), (ICSMFL,KPOSMP(1133))
      equivalence (BRKOP ,KPOSMP(1137)), (MACHTP,KPOSMP(1201))
      equivalence (NUMLIN,KPOSMP(1202)), (ACTLIN,KPOSMP(1205))
      equivalence (INCR  ,KPOSMP(1226)), (IRTNUM,KPOSMP(1243))
      equivalence (LNRADJ,KPOSMP(1277)), (LRTRCT,KPOSMP(1278))
      equivalence (IRTCLM,KPOSMP(1306)), (RSIZCD,KPOSMP(1328))
      equivalence (LRTTAD,KPOSMP(1335)), (ILINRP,KPOSMP(1348))
      equivalence (ICIRFL,KPOSMP(1367)), (IRTSHF,KPOSMP(1374))
      equivalence (NUMINF,KPOSMP(1396)), (IRTDUP,KPOSMP(1413))
      equivalence (IRTINC,KPOSMP(1461)), (IRDEAD,KPOSMP(1465))
      equivalence (IRTDEF,KPOSMP(1485)), (IRTYPE,KPOSMP(1486))
      equivalence (ACHEAD,KPOSMP(1645))
      equivalence (SLWFL ,KPOSMP(1701)), (IACCFL,KPOSMP(1745))
      equivalence (TOOLFL,KPOSMP(1804)), (TURFL ,KPOSMP(1824))
      equivalence (IJKROT,KPOSMP(1739)), (TLCCD ,KPOSMP(1839))
      equivalence (TLOCD ,KPOSMP(1860)), (TLOSGN,KPOSMP(1869))
      equivalence (LTHPCL,KPOSMP(1899)), (ICYLFL,KPOSMP(1901))
      equivalence (CYLCOD,KPOSMP(1921)), (CYLREG,KPOSMP(1946))
      equivalence (NSPRG ,KPOSMP(3101)), (SPNDCD,KPOSMP(3105))
      equivalence (SPNBCD,KPOSMP(3109))
      equivalence (IFDSUP,KPOSMP(3111)), (SPNOCD,KPOSMP(3121))
      equivalence (COOLCD,KPOSMP(3128))
      equivalence (IFITYP,KPOSMP(3150)), (FEDOCD,KPOSMP(3217))
      equivalence (CUTCFL,KPOSMP(3251)), (CUTCCD,KPOSMP(3271))
      equivalence (DELYCD,KPOSMP(3351)), (HOMCOD,KPOSMP(3361))
      equivalence (PSTNCD,KPOSMP(3376)), (PSTNRG,KPOSMP(3381))
      equivalence (ALNCUT,KPOSMP(4040)), (ALNMOV,KPOSMP(4045))
      equivalence (BSPLFL,KPOSMP(4088))
      equivalence (MTPDYN,KPOSMP(4126)), (SRTNUM,KPOSMP(4140))
      equivalence (ALNMOD,KPOSMP(4049)), (MODCYL,KPOSMP(4204))
c
      integer*4 ICIRFL,BRKOP(10),ICSMRG,ICSMFL,NUMLIN(3),IRTNUM,
     1          NUMINF,IRTCLM,MACHTP,CUTCFL(20),CUTCCD(30),COOLCD(4),
     2          ICYCFL(30),CYCREG(20),ICYLFL(20),CYLCOD(25),DELYCD(5),
     3          CYLREG(25),NCBMSG,NCEMSG,IFDSUP(4),IFITYP,FEDOCD(2),
     4          HOMCOD(5),TOOLFL(20),MCHOPT(20),IRDEAD(20),INCR,
     5          ACTLIN(3),TLOCD(8),PSTNRG(15),OPSTCD,NOPSKP,PSTNCD(5)
      integer*4 LRTRCT,LRTTAD,REWCOD,RSIZCD(5),IRTYPE(20),SEQCOD,NSPRG,
     1          SPNDCD(4),SPNBCD,SPNOCD(2),STOPCD,SLWFL(5),ALNCOD,
     2          AL1BLK,AL2BLK,NRWSTP,TLOSGN,TURFL(2),TLCCD(20),SRTNUM,
     3          PODACD(7),PODCCD(2),PODSCD(6),ALNCUT,ALNMOV,LTHPCL(2),
     4          MTPDYN,BSPLFL,MODCYL,IJKROT,ALNMOD,IRTDUP(4),
     5          IACCFL(2),IRTSHF,ACHEAD,LNRADJ,ILINRP,IRTDEF,IRTINC(4)
c
      equivalence (DUMMY ,POSMAP(0003)), (TLATOL,POSMAP(0057))
      equivalence (CLRPLN,POSMAP(0161)), (BRKVR ,POSMAP(1205))
      equivalence (CIRTOL,POSMAP(2201))
      equivalence (LNRTOL,POSMAP(2251)), (RETDIS,POSMAP(2279))
      equivalence (LNRATL,POSMAP(2286))
      equivalence (RETFED,POSMAP(3239)), (MAXIPM,POSMAP(3544))
      equivalence (MAXDPM,POSMAP(3545))
      equivalence (SPIVEC,POSMAP(3583)), (BLDELT,POSMAP(3593))
      equivalence (ALNDIS,POSMAP(4444)), (ALNMPS,POSMAP(4461))
      equivalence (SMORAN,POSMAP(4464)), (SMODIS,POSMAP(4465))
      equivalence (VTOLER,POSMAP(4911)), (RTSHFD,POSMAP(4947))
c
      real*8 BRKVR(10),DUMMY,CLRPLN(4),LNRTOL,MAXDPM,SPIVEC(3),
     1       CIRTOL(5),MAXIPM,RETDIS,RETFED(3),BLDELT,ALNMPS(3),
     2       ALNDIS(3),SMORAN,SMODIS,VTOLER,TLATOL,RTSHFD,LNRATL(5)
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ilev(10),nlev,ncon,ncoff,ncnow,ncnxt,nco,ncm,ncmin(20),
     1          strlen1,inc,ipt1,ipt2,nct,ncaxs(4),i,ncmod,inc1,inc2,
     2          ncb,isub(10),inc3,inc4,inum,ncm1,itab,ihed,ifl1,iout,
     3          ifl,jindex,ncs,ncauto,nmod(6),ifl2,is1,is2,is3
c
      character*1 lxyz(3)
      character*8 lon,loff,lnow,lnext,laxs(4),lmod,lauto,lxmod
      character*24 lmaj,lmaj1
      character*80 obuf,tbuf,lmin(10),lspac,sbuf
      character*512 bbuf,bbuf1
c
      data lspac /' '/, nmod /11,12,13,20,21,22/
      data lxyz /'x','y','z'/
c
c...Initialize routine
c
      DLIN   = 10000
      NDHED  = 6
c
c...Get Header line
c
      ilev(1) = 5
      nlev    = 1
      call docprm (DHED(1),NCHED(1),ilev,nlev)
      call dcenhd (DHED(1),NCHED(1),DHED(1),NCHED(1))
      nlev   = 2
c
c...Get standard words
c
      call getvwd (71,lon,ncon,2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (72,loff,ncoff,2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (88,lauto,ncauto,2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (161,lnow,ncnow,2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (162,lnext,ncnxt,2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (84,laxs(1),ncaxs(1),2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (85,laxs(2),ncaxs(2),2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (86,laxs(3),ncaxs(3),2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (132,laxs(4),ncaxs(4),2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (55,lmod,ncmod,2,PSTWRD,PSTWVL,NPSTWD)
c
c...Check for Mill Turn
c
      if (MACHTP .eq. 4) then
      end if
c
c...ALIGN/AXIS
c
      if (MACHTP .eq. 1 .and. IRTNUM .gt. 1 .and. ACHEAD .ne. 2) then
          call getvwd (1076,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
c
          obuf   = lmaj(1:ncm) // '/' // laxs(4)(1:ncaxs(4)) // ' [,' //
     1             loff(1:6) // ']'
          nco    = ncm    + ncaxs(4) + 6      + 5
          call docout (obuf,nco,1,cmsg,kerr)
c
          nct    = ncm    + ncaxs(4) + 4
          call rtoc (TLATOL,lmin(1),ncmin(1))
          call getvwd (1037,lmin(2),ncmin(2),1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (113,lmin(3),ncmin(3),2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lspac(1:nct) // lauto(1:6) // ' [,' //
     1             lmin(1)(1:ncmin(1)) // '] [,' // lnext(1:6) //
     2             ' ] [,' // lmin(2)(1:6) // '] [,' //
     3             lmin(3)(1:6) // ']'
          nco    = strlen1(obuf)
          call docout (obuf,nco,0,cmsg,kerr)
c
          call getvwd (5,lmin(2),ncmin(2),1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (1078,lmin(3),ncmin(3),1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (112,lmin(4),ncmin(4),2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lspac(1:nct) // lmin(2)(1:ncmin(2)) //
     1             lspac(1:6+ncmin(1)+2) // lmin(3)(1:6) //
     2             '     ' // lnow(1:6) // '    ' //
     3             lmin(4)(1:ncmin(4))
          nco    = strlen1(obuf)
          call docout (obuf,nco,0,cmsg,kerr)
      endif
c
c...ALIGN/CUT
c
      if (MACHTP .eq. 3) then
          call getvwd (1076,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (803,tbuf,nct,1,PSTWRD,PSTWVL,NPSTWD)
          inum = 0
          do 105 i=1,3
              call rtoc (ALNDIS(i),lmin(i),ncmin(i))
              inum = inum + ncmin(i)
  105     continue
          lmin(4) = ' [,' // lmin(1)(1:ncmin(1)) // ',' //
     1              lmin(2)(1:ncmin(2)) // ',' //
     2              lmin(3)(1:ncmin(3)) // ']'
          inum    = inum + 6
          if (ALNCUT .eq. 0) then
              obuf  = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1                 loff(1:ncoff)
              i     = ncoff
          else
              obuf  = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1                lon(1:ncon) // lmin(4)(1:inum)
              i     = ncon + inum
          end if
          nco    = ncm    + nct    + i + 2
          call docout (obuf,nco,1,cmsg,kerr)
c
          nct    = ncm    + nct    + 2
          call getvwd (176,tbuf,i,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lspac(1:nct) // tbuf(1:i)
          nco    = nct    + i
          call docout (obuf,nco,0,cmsg,kerr)
c
          call getvwd (162,tbuf,i,2,PSTWRD,PSTWVL,NPSTWD)
          nco    = nct    + i
          obuf(nct+1:80) = tbuf(1:i)
          call docout (obuf,nco,0,cmsg,kerr)
c
          if (ALNCUT .eq. 0) then
              obuf(nct+1:80) = lon(1:ncon) // lmin(4)(1:inum)
              nco = nct + ncon + inum
          else
              obuf(nct+1:80) = loff(1:ncoff)
              nco = nct + ncoff
          end if
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...ALIGN/MOVE
c
      if (MACHTP .eq. 3) then
          call getvwd (1076,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (577,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          inum = 0
          do 115 i=1,3
              call rtoc (ALNMPS(i),lmin(i),ncmin(i))
              inum = inum + ncmin(i)
  115     continue
          lmin(4) = ' [,' // lmin(1)(1:ncmin(1)) // ',' //
     1              lmin(2)(1:ncmin(2)) // ',' //
     2              lmin(3)(1:ncmin(3)) // ']'
          inum    = inum + 6
          if (ALNMOV .eq. 0) then
              obuf  = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1                 loff(1:ncoff)
              i     = ncoff
          else
              obuf  = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1                lon(1:ncon) // lmin(4)(1:inum)
              i     = ncon + inum
          end if
          nco    = ncm    + nct    + i + 2
          call docout (obuf,nco,1,cmsg,kerr)
c
          nct    = ncm    + nct    + 2
          call getvwd (176,tbuf,i,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lspac(1:nct) // tbuf(1:i)
          nco    = nct    + i
          call docout (obuf,nco,0,cmsg,kerr)
c
          call getvwd (162,tbuf,i,2,PSTWRD,PSTWVL,NPSTWD)
          nco    = nct    + i
          obuf(nct+1:80) = tbuf(1:i)
          call docout (obuf,nco,0,cmsg,kerr)
c
          if (ALNMOV .eq. 0) then
              obuf(nct+1:80) = lon(1:ncon) // lmin(4)(1:inum)
              nco = nct + ncon + inum
          else
              obuf(nct+1:80) = loff(1:ncoff)
              nco = nct + ncoff
          end if
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...ALIGN/OPTION
c
      if (MACHTP .eq. 3) then
          call getvwd (1076,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (144,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          call itoc (ALNMOD,lmin(1),ncmin(1),0)
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1             lmin(1)(1:ncmin(1))
          nco    = ncm    + nct    + ncmin(1) + 2
          call docout (obuf,nco,1,cmsg,kerr)
      endif
c
c...ARCSLP/LINCIR
c
      if (ICIRFL .eq. 1) then
          call getvwd (1029,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (95,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ',' //
     1             lon(1:ncon)
          nco    = ncm    + ncmin(1) + ncon   + 2
          call docout (obuf,nco,1,cmsg,kerr)
          nco    = ncm    + ncmin(1) + 2
          obuf   = lspac(1:nco) // loff(1:ncoff)
          nco    = nco    + ncoff
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...ARCSLP/BSPLIN
c
      if (BSPLFL .eq. 1) then
          call getvwd (1029,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (628,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ',' //
     1             lon(1:ncon)
          nco    = ncm    + ncmin(1) + ncon   + 2
          call docout (obuf,nco,1,cmsg,kerr)
          nco    = ncm    + ncmin(1) + 2
          obuf   = lspac(1:nco) // loff(1:ncoff)
          nco    = nco    + ncoff
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...AUXFUN
c
      call getvwd (1022,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lmaj(1:ncm) // '/n [,' // lnow(1:6) // ']'
      nco    = ncm    + 12
      call docout (obuf,nco,1,cmsg,kerr)
      nco    = ncm    + 5
      obuf   = lspac(1:nco) // lnext(1:ncnxt)
      nco    = nco    + ncnxt
      call docout (obuf,nco,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...BREAK
c
      call getvwd (16,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      call docout (lmaj,ncm,1,cmsg,kerr)
c
      obuf   = lmaj(1:ncm) // '/' // loff(1:ncoff)
      nco    = ncm    + ncoff  + 1
      call docout (obuf,nco,1,cmsg,kerr)
c
      call getvwd (28,lmin(3),ncmin(3),2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (188,lmin(4),ncmin(4),2,PSTWRD,PSTWVL,NPSTWD)
      if (BRKOP(1) .eq. 1 .or. BRKOP(1) .eq. 2) then
          inc = 1
          call rtoc (BRKVR(1),lmin(5),ncmin(5))
          if (BRKOP(1) .eq. 2) then
              lmin(1) = lauto
              ncmin(1) = ncauto
              inc    = 2
          endif
      else if (BRKOP(1) .eq. 3) then
          call rtoc (BRKVR(2),lmin(5),ncmin(5))
          lmin(1) = lmin(3)
          ncmin(1) = ncmin(3)
          inc    = 3
      else if (BRKOP(1) .eq. 4) then
          call rtoc (BRKVR(3),lmin(5),ncmin(5))
          lmin(1) = lmin(4)
          ncmin(1) = ncmin(4)
          inc    = 4
      else
          inc    = 1
          lmin(5) = 'n'
          ncmin(5) = 1
      endif
      lmin(inc) = lon
      ncmin(inc) = ncon
      call getvwd (280,lmin(6),ncmin(6),2,PSTWRD,PSTWVL,NPSTWD)
      call rtoc (BRKVR(4),lmin(7),ncmin(7))
      call getvwd (281,lmin(8),ncmin(8),2,PSTWRD,PSTWVL,NPSTWD)
      call rtoc (BRKVR(5),lmin(9),ncmin(9))
      obuf   = lmaj(1:ncm) // '/ [' // lmin(1)(1:6) // '] [,' //
     1         lmin(5)(1:ncmin(5)) // '] [,' // lmin(6)(1:ncmin(6)) //
     2         ',' // lmin(7)(1:ncmin(7)) // '] [,' //
     3         lmin(8)(1:ncmin(8)) // ',' // lmin(9)(1:ncmin(9)) //
     4         ']'
      nco    = strlen1(obuf)
      call docout (obuf,nco,1,cmsg,kerr)
      obuf   = lspac(1:ncm+3) // lauto(1:ncauto)
      nco    = ncm    + ncauto + 3
      call docout (obuf,nco,0,cmsg,kerr)
      obuf   = lspac(1:ncm+3) // lmin(3)(1:ncmin(3))
      nco    = ncm    + ncmin(3) + 3
      call docout (obuf,nco,0,cmsg,kerr)
      obuf   = lspac(1:ncm+3) // lmin(4)(1:ncmin(4))
      nco    = ncm    + ncmin(4) + 3
      call docout (obuf,nco,0,cmsg,kerr)
c
c...CHECK/LENGTH
c
      call getvwd (1023,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      if (ICSMRG .ne. 0) then
          lmin(2) = lon
          ncmin(2) = ncon
          lmin(3) = loff
          ncmin(3) = ncoff
          call getvwd (9,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          if (ICSMRG .eq. 1) then
              ipt1   = 2
              ipt2   = 3
          else
              ipt1   = 3
              ipt2   = 2
          endif
          obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ',' //
     1             lmin(ipt1)(1:ncmin(ipt1))
          nco    = ncm    + ncmin(1) + ncmin(ipt1) + 2
          call docout (obuf,nco,1,cmsg,kerr)
          inc    = ncm    + ncmin(1) + 2
          obuf   = lspac(1:inc) // lmin(ipt2)(1:ncmin(ipt2))
          nco    = inc    + ncmin(ipt2)
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...CHECK/OUT
c
      iout   = 1
      call getvwd (49,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
      ilev(nlev) = 1
      call docprm (lmin(2),ncmin(2),ilev,nlev)
      ilev(nlev) = 2
      call docprm (lmin(3),ncmin(3),ilev,nlev)
      obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ',m'
      nco    = ncm    + ncmin(1) + 3
c
      do 500 i=1,3,1
          if (NUMLIN(i) .ne. 0) then
              tbuf   = ' [,' // laxs(i)
              nct    = ncaxs(i) + 3
              if (NUMLIN(i) .eq. 2) then
                  tbuf(nct+1:) = '[,n]'
                  nct    = nct    + 4
              endif
              tbuf(nct+1:) = '[,' // lmin(2)(1:ncmin(2)) //
     1                 ',' // lmin(3)(1:ncmin(3)) // ']]'
              nct    = strlen1(tbuf)
              if (nco+nct .gt. 78) then
                  obuf(nco+1:) = ' $'
                  call docout (obuf,nco+2,iout,cmsg,kerr)
                  nco    = ncm    + 4
                  obuf   = lspac(1:nco)
                  iout   = 0
              endif
              obuf(nco+1:) = tbuf(1:nct)
              nco    = nco    + nct
          endif
  500 continue
c
      if (IRTNUM .ne. 0) then
          tbuf   = ' [,' // laxs(4)
          nct    = ncaxs(4) + 3
          if (IRTNUM .gt. 1) then
              tbuf(nct+1:) = '[,n]'
              nct    = nct    + 4
          endif
          tbuf(nct+1:) = '[,' // lmin(2)(1:ncmin(2)) //
     1             ',' // lmin(3)(1:ncmin(3)) // ']]'
          nct    = strlen1(tbuf)
          if (nco+nct .gt. 78) then
              obuf(nco+1:) = ' $'
              call docout (obuf,nco+2,iout,cmsg,kerr)
              nco    = ncm    + 4
              obuf   = lspac(1:nco)
              iout   = 0
          endif
          obuf(nco+1:) = tbuf(1:nct)
          nco    = nco    + nct
      endif
c
      call docout (obuf,nco,iout,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...CHECK/AXIS
c
      iout   = 1
      obuf   = lmaj(1:ncm) // '/'
      nco    = ncm    + 1
c
      do 600 i=1,3,1
          if (NUMLIN(i) .ne. 0) then
              tbuf   = ' [,' // laxs(i)
              nct    = ncaxs(i) + 3
              if (NUMLIN(i) .eq. 2) then
                  tbuf(nct+1:) = '[,n]'
                  nct    = nct    + 4
              endif
              tbuf(nct+1:) = '[,' // lmin(2)(1:ncmin(2)) //
     1                 ',' // lmin(3)(1:ncmin(3)) // ']]'
              nct    = strlen1(tbuf)
              if (nco+nct .gt. 78) then
                  obuf(nco+1:)   = ' $'
                  call docout (obuf,nco+2,iout,cmsg,kerr)
                  nco    = ncm    + 4
                  obuf   = lspac(1:nco)
                  iout   = 0
              endif
              obuf(nco+1:) = tbuf(1:nct)
              nco    = nco    + nct
          endif
  600 continue
c
      if (IRTNUM .ne. 0) then
          tbuf   = ' [,' // laxs(4)
          nct    = ncaxs(4) + 3
          if (IRTNUM .gt. 1) then
              tbuf(nct+1:) = '[,n]'
              nct    = nct    + 4
          endif
          tbuf(nct+1:) = '[,' // lmin(2)(1:ncmin(2)) //
     1             ',' // lmin(3)(1:ncmin(3)) // ']]'
          nct    = strlen1(tbuf)
          if (nco+nct .gt. 78) then
              obuf(nco+1:) = ' $'
              call docout (obuf,nco+2,iout,cmsg,kerr)
              nco    = ncm    + 4
              obuf   = lspac(1:nco)
              iout   = 0
          endif
          obuf(nco+1:) = tbuf(1:nct)
          nco    = nco    + nct
      endif
c
      call docout (obuf,nco,iout,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...CLAMP
c
      if (IRTNUM .ne. 0 .and. IRTCLM .ne. 1) then
          call getvwd (1060,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // laxs(4)(1:ncaxs(4)) // ','
          nco    = ncm    + ncaxs(4) + 2
          if (IRTNUM .gt. 1) then
              call itoc (IRTNUM,lmin(1),ncmin(1),0)
              obuf(nco+1:) = 'n1 [...] n' // lmin(1)(1:ncmin(1)) // ','
              nco    = nco    + ncmin(1) + 11
          endif
          inc    = nco
          obuf(nco+1:) = lon(1:ncon)
          nco    = nco    + ncon
          call docout (obuf,nco,1,cmsg,kerr)
          obuf   = lspac(1:inc) // loff(1:ncoff)
          nco    = inc    + ncoff
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c...CLAMP/AUTO
c
          obuf   = lmaj(1:ncm) // '/' // lauto(1:ncauto) // ',n'
          nco    = ncm    + ncauto + 3
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...CLRSRF/AVOID
c
      call getvwd (1057,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      if (MACHTP .ne. 2) then
          call getvwd (187,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) //
     1             ', [i,j,k,] d'
          nco    = ncm    + ncmin(1) + 13
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...CLRSRF/NORMAL
c
      call getvwd (111,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
      if (CLRPLN(1) .eq. DUMMY) then
          obuf   = lmaj(1:ncm) // '/ [' // lmin(1)(1:ncmin(1)) //
     1             ',] [i,j,k,] d'
      else
          do 1000 i=1,4,1
              call rtoc (CLRPLN(i),lmin(i+1),ncmin(i+1))
 1000     continue
          obuf   = lmaj(1:ncm) // '/ [' // lmin(1)(1:ncmin(1)) //
     1             ',] [' // lmin(2)(1:ncmin(2)) // ',' //
     2             lmin(3)(1:ncmin(3)) // ',' // lmin(4)(1:ncmin(4)) //
     3             ',] ' // lmin(5)(1:ncmin(5))
      endif
      nco    = strlen1(obuf)
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...CLRSRF/TOOL
c
      call getvwd (617,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
      if (CLRPLN(1) .ne. DUMMY) then
          obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) //
     1             ',d'
      else
          call rtoc (CLRPLN(2),lmin(2),ncmin(2))
          obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) //
     1             ',' // lmin(2)(1:ncmin(2))
      endif
      nco    = strlen1(obuf)
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...prepst CLRSRC/START
c...etc
c
      call getvwd (57,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (2,lmin(2),ncmin(2),1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (53,lmin(3),ncmin(3),2,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lmaj(1:ncm) // '/' // lmin(1)
      call docout (obuf,ncm+ncmin(1)+1,1,cmsg,kerr)
      obuf   = lspac(1:ncm+1) // lmin(2)
      call docout (obuf,ncm+ncmin(2)+1,0,cmsg,kerr)
      obuf   = lspac(1:ncm+1) // lmin(3)
      call docout (obuf,ncm+ncmin(3)+1,0,cmsg,kerr)
      lmin(1) = 'a,b,c,d, '
      lmin(2) = 'z,       '
      ncs     = ncm + 10
      do 1025 i=1,6
         call getvwd (nmod(i),lxmod,inc2,2,PSTWRD,PSTWVL,NPSTWD)
         if (i .lt. 3) then
            obuf = lspac(1:ncm+1) // lmin(i)(1:9) // lxmod(1:inc2)
         else
            obuf = lspac(1:ncs) // lxmod(1:inc2)
         end if
         call docout (obuf,ncs+inc2,0,cmsg,kerr)
         if (kerr .ne. 0) go to 8000
 1025 continue
c
c...COOLNT
c
      if (COOLCD(1) .ne. 0 .or. COOLCD(2) .ne. 0 .or. COOLCD(3) .ne. 0)
     1        then
          call getvwd (1030,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // lon(1:ncon)
          nco    = ncm    + ncon   + 1
          call docout (obuf,nco,1,cmsg,kerr)
          inc    = ncm    + 1
          isub(1) = 89
          isub(2) = 90
          isub(3) = 1011
          isub(4) = 72
          do 2000 i=1,4,1
              if (COOLCD(i) .ne. 0) then
                  call getvwd (isub(i),tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
                  obuf   = lspac(1:inc) // tbuf(1:nct)
                  nco    = inc    + nct
                  call docout (obuf,nco,0,cmsg,kerr)
              endif
 2000     continue
          if (kerr .ne. 0) go to 8000
      endif
c
c...COUPLE
c
      if (MACHTP .ne. 2) then
          call getvwd (1049,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // loff(1:ncoff)
          nco    = ncm + ncoff + 1
          call docout (obuf,nco,1,cmsg,kerr)
          call getvwd (1,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lspac(1:ncm+1) // 'n,' // lmin(1)(1:ncmin(1)) // ',a'
          nco    = ncm    + ncmin(1) + 5
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c...COUPLE/AUTO
c
          obuf   = lmaj(1:ncm) // '/' // lauto(1:ncauto) // ',' //
     1             lon(1:ncon)
          nco    = ncm    + ncauto + ncon   + 2
          call docout (obuf,nco,1,cmsg,kerr)
          nco    = ncm    + ncauto + 2
          obuf   = lspac(1:nco) // loff(1:ncoff)
          nco    = nco    + ncoff
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...CUTCOM/ADJUST
c
      call getvwd (1007,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (159,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
      if (CUTCCD(17) .ne. 0 .or. CUTCCD(18) .ne. 0) then
          obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ',' //
     1             lon(1:ncon)
          nco    = ncm    + ncmin(1) + ncon   + 2
          call docout (obuf,nco,1,cmsg,kerr)
          inc    = ncm    + ncmin(1) + 2
          obuf   = lspac(1:inc) // loff(1:ncoff)
          nco    = inc    + ncoff
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
      if (CUTCCD(16) .ne. 0 .or. CUTCCD(17) .ne. 0 .or.
     1    CUTCCD(19) .ne. 0 .or. CUTCCD(20) .ne. 0) then
          obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ',n'
          nco    = ncm    + ncmin(1) + 3
          ifl    = 0
          if (CUTCCD(19) .ne. 0 .or. CUTCCD(20) .ne. 0 .or.
     1        CUTCFL(11) .eq. 1) then
              call getvwd (19,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(nco+1:) = ' [,' // tbuf(1:6) // ']'
              inc    = nco    + 3
              nco    = nco    + 10
              ifl    = 1
          endif
          ilev(nlev) = 3
          call docprm (lmin(2),ncmin(2),ilev,nlev)
          do 1500 i=1,3,1
              if (NUMLIN(i) .ne. 0 .and. CUTCCD(20+i) .ne. 0) then
                  tbuf   = ' [,' // laxs(i)(1:ncaxs(i)) // '[,' //
     1                     lmin(2)(1:ncmin(2)) // ']]'
                  nct    = strlen1(tbuf)
                  obuf(nco+1:) = tbuf(1:nct)
                  nco    = nco    + nct
              endif
 1500     continue
          call docout (obuf,nco,1,cmsg,kerr)
          if (ifl .eq. 1) then
              call getvwd (10,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf   = lspac(1:inc) // tbuf(1:nct)
              nco    = inc    + nct
              call docout (obuf,nco,0,cmsg,kerr)
          endif
          if (kerr .ne. 0) go to 8000
      endif
c
c...CUTCOM/dir
c
      if (CUTCCD(1) .ne. 0 .or. CUTCCD(2) .ne. 0 .or. CUTCFL(1) .ne. 0)
     1        then
          call getvwd (8,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          bbuf   = lmaj(1:ncm) // '/' // tbuf(1:6)
          ncb    = ncm    + 7
          inc1   = 0
          inc2   = 0
c
          if (CUTCFL(2) .eq. 1 .and. MACHTP .ne. 2) then
              call getvwd (33,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              inc1   = ncb    + 3
              bbuf(ncb+1:) = ' [,' // tbuf(1:6) // ']'
              ncb    = ncb    + 10
          endif
c
          if (CUTCCD(4) .ne. 0) then
              bbuf(ncb+1:) = ' [,d]'
              ncb    = ncb    + 5
          endif
c
          if (CUTCFL(1) .ne. 0) then
              call getvwd (111,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              inc2   = ncb    + 3
              bbuf(ncb+1:) = ' [,' // tbuf(1:6) // ']'
              ncb    = ncb    + 10
          endif
c
          if (CUTCFL(3) .eq. 1) then
              bbuf(ncb+1:) = ' [,' // lmod(1:ncmod) // ',n,v]'
              ncb    = ncb    + ncmod  + 8
          endif
c
          if (CUTCFL(1) .ne. 0) then
              call getvwd (158,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              bbuf(ncb+1:) = ' [,' // tbuf(1:nct)
              ncb    = ncb    + nct    + 3
              do 1700 i=1,3,1
                  if (NUMLIN(i) .ne. 0) then
                      tbuf   = ' [,' // laxs(i)(1:ncaxs(1)) // ',' //
     1                         lxyz(i) // ']'
                      nct    = strlen1(tbuf)
                      bbuf(ncb+1:) = tbuf(1:nct)
                      ncb    = ncb    + nct
                  endif
 1700         continue
              ncb    = ncb    + 1
              bbuf(ncb:ncb) = ']'
          endif
c
          iout   = 1
          if (ncb .gt. 80) then
              do 1800 i=80,1,-1
                  if (bbuf(i:i) .eq. ' ') then
                      obuf   = bbuf(1:i) // '$'
                      nco    = i      + 1
                      call docout (obuf,nco,iout,cmsg,kerr)
                      tbuf   = bbuf(i+1:ncb)
                      inc    = ncm    + 4
                      bbuf   = lspac(1:inc) // tbuf
                      ncb    = ncb     - i + ncm    + 4
                      iout   = 0
                      go to 1850
                  endif
 1800         continue
          else
              call docout (bbuf,ncb,iout,cmsg,kerr)
              ncb    = 0
              iout   = 0
          endif
c
 1850     call getvwd (24,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          inc    = ncm    + 1
          obuf   = lspac(1:inc) // tbuf(1:nct)
          nco    = inc    + nct
          if (inc1 .ne. 0) then
              call getvwd (41,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc1+1:) = tbuf(1:nct)
              nco    = inc1   + nct
          endif
          if (inc2 .ne. 0) then
              call getvwd (18,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc2+1:) = tbuf(1:nct)
              nco    = inc2   + nct
          endif
          call docout (obuf,nco,iout,cmsg,kerr)
c
          if (inc1 .ne. 0) then
              call getvwd (45,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf   = lspac(1:inc1) // tbuf(1:nct)
              nco    = inc1   + nct
              call docout (obuf,nco,0,cmsg,kerr)
          endif
c
          if (ncb .ne. 0) then
              call docout (' ',0,0,cmsg,kerr)
              call docout (bbuf,ncb,0,cmsg,kerr)
          endif
          if (kerr .ne. 0) go to 8000
c
c...CUTCOM/ENDPT
c
      if (CUTCFL(17) .eq. 1) then
          call getvwd (664,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          bbuf   = lmaj(1:ncm) // '/' // tbuf(1:6)
          ncb    = ncm    + 7
          inc1   = 0
c
          call getvwd (728,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          inc1   = ncb    + 2
          bbuf(ncb+1:) = ', ' // tbuf(1:6)
          ncb    = ncb    + 8
c
          if (CUTCCD(4) .ne. 0) then
              bbuf(ncb+1:) = ' [,d]'
              ncb    = ncb    + 5
          endif
c
          call getvwd (617,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ' [,' // tbuf(1:nct) // ',rad,cr]'
          ncb    = ncb    + nct    + 11
c
          call getvwd (96,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ' [,' // tbuf(1:nct) // ',f]'
          ncb    = ncb    + nct    + 6
c
          call getvwd (68,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ' [,' // tbuf(1:nct) // ',n]'
          ncb    = ncb    + nct    + 6
c
          call getvwd (158,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ' [,' // tbuf(1:nct)
          ncb    = ncb    + nct    + 3
          do 1900 i=1,3,1
              tbuf   = ' [,' // laxs(i)(1:ncaxs(1)) // ',' //
     1                 lxyz(i) // ']'
              nct    = strlen1(tbuf)
              bbuf(ncb+1:) = tbuf(1:nct)
              ncb    = ncb    + nct
 1900     continue
          ncb    = ncb    + 1
          bbuf(ncb:ncb) = ']'
c
          iout   = 1
          if (ncb .gt. 80) then
              do 1920 i=80,1,-1
                  if (bbuf(i:i) .eq. ' ') then
                      obuf   = bbuf(1:i) // '$'
                      nco    = i      + 1
                      call docout (obuf,nco,iout,cmsg,kerr)
                      tbuf   = bbuf(i+1:ncb)
                      inc    = ncm    + 4
                      bbuf   = lspac(1:inc) // tbuf
                      ncb    = ncb     - i + ncm    + 4
                      iout   = 0
                      go to 1950
                  endif
 1920         continue
          else
              call docout (bbuf,ncb,iout,cmsg,kerr)
              ncb    = 0
              iout   = 0
          endif
c
 1950     call getvwd (729,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lspac(1:inc1) // tbuf(1:nct)
          nco    = inc1   + nct
          call docout (obuf,nco,iout,cmsg,kerr)
c
          if (ncb .ne. 0) then
              call docout (' ',0,0,cmsg,kerr)
              call docout (bbuf,ncb,0,cmsg,kerr)
          endif
          if (kerr .ne. 0) go to 8000
      endif
c
c...CUTCOM/ON
c
          obuf   = lmaj(1:ncm) // '/' // lon(1:ncon)
          nco    = ncm    + ncon    + 1
          call docout (obuf,nco,1,cmsg,kerr)
          inc    = ncm    + 1
          obuf   = lspac(1:inc) // loff(1:ncoff)
          nco    = inc    + ncoff
          if (CUTCFL(3) .eq. 1) then
              obuf(nco+1:) = ' [,' // lmod(1:ncmod) // ',n,v]'
              nco    = nco    + ncmod  + 8
          endif
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Mill CYCLE
c
      if (MACHTP .ne. 2) then
          call getvwd (1054,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          ilev(nlev) = 4
          call docprm (lmin(1),ncmin(1),ilev,nlev)
          call getvwd (281,lmin(2),ncmin(2),2,PSTWRD,PSTWVL,NPSTWD)
          ilev(nlev) = 5
          call docprm (lmin(3),ncmin(3),ilev,nlev)
          bbuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ', ' //
     1             lmin(2)(1:ncmin(2)) // ',' // lmin(3)(1:ncmin(3))
          inc1   = ncm    + ncmin(1) + ncmin(2) + 4
          ncb    = inc1   + ncmin(3)
          ilev(nlev) = 12
          call docprm (lmin(5),ncmin(5),ilev,nlev)
          if (ncmin(5) .gt. ncmin(3)) ncb = ncb + (ncmin(5)-ncmin(3))
c
          inc2   = 0
          if (ICYCFL(1) .ne. 1 .or. ICYCFL(14) .eq. 1) then
              call getvwd (73,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              ilev(nlev) = 6
              call docprm (lmin(2),ncmin(2),ilev,nlev)
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:6) // ',' //
     1                       lmin(2)(1:ncmin(2)) // ']'
              inc2   = ncb    + 3
              ncb    = inc2   + ncmin(2) + 9
          endif
c
          if (ICYCFL(1) .ne. 1 .or. CYCREG(2) .ne. 0) then
              call getvwd (280,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',r]'
              ncb    = ncb    + ncmin(1) + 6
          endif
c
          if (ICYCFL(1) .ne. 1 .or. CYCREG(4) .ne. 0) then
              call getvwd (279,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              ilev(nlev) = 7
              call docprm (lmin(2),ncmin(2),ilev,nlev)
              ncmin(2) = ncmin(2) + 1
              lmin(2)(ncmin(2):ncmin(2)) = '1'
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',' //
     1                       lmin(2)(1:ncmin(2))
              ncb    = ncb    + ncmin(1) + ncmin(2) + 4
              if (ICYCFL(1) .ne. 1 .or. CYCREG(5) .ne. 0) then
                  lmin(2)(ncmin(2):ncmin(2)) = '2'
                  bbuf(ncb+1:) = '[,' // lmin(2)(1:ncmin(2)) // ']'
                  ncb    = ncb    + ncmin(2) + 3
              endif
              ncb    = ncb    + 1
              bbuf(ncb:) = ']'
          endif
c
          if (ICYCFL(1) .ne. 1 .or. CYCREG(6) .ne. 0) then
              call getvwd (92,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              ilev(nlev) = 8
              call docprm (lmin(2),ncmin(2),ilev,nlev)
              ncmin(2) = ncmin(2) + 1
              lmin(2)(ncmin(2):ncmin(2)) = '1'
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',' //
     1                       lmin(2)(1:ncmin(2))
              ncb    = ncb    + ncmin(1) + ncmin(2) + 4
              if (ICYCFL(1) .ne. 1 .or. CYCREG(7) .ne. 0) then
                  lmin(2)(ncmin(2):ncmin(2)) = '2'
                  bbuf(ncb+1:) = '[,' // lmin(2)(1:ncmin(2)) // ']'
                  ncb    = ncb    + ncmin(2) + 3
              endif
              ncb    = ncb    + 1
              bbuf(ncb:) = ']'
          endif
c
          if (ICYCFL(1) .ne. 1 .or. CYCREG(8) .ne. 0 .or.
     1        CYCREG(9) .ne. 0) then
              call getvwd (705,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              ilev(nlev) = 9
              call docprm (lmin(2),ncmin(2),ilev,nlev)
              ncmin(2) = ncmin(2) + 1
              lmin(2)(ncmin(2):ncmin(2)) = '1'
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',' //
     1                       lmin(2)(1:ncmin(2))
              ncb    = ncb    + ncmin(1) + ncmin(2) + 4
              if (ICYCFL(1) .ne. 1 .or. CYCREG(10) .ne. 0) then
                  lmin(2)(ncmin(2):ncmin(2)) = '2'
                  bbuf(ncb+1:) = '[,' // lmin(2)(1:ncmin(2)) // ']'
                  ncb    = ncb    + ncmin(2) + 3
              endif
              ncb    = ncb    + 1
              bbuf(ncb:) = ']'
          endif
c
          if (ICYCFL(1) .ne. 1 .or. CYCREG(12) .ne. 0) then
              call getvwd (295,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              ilev(nlev) = 10
              call docprm (lmin(2),ncmin(2),ilev,nlev)
              ncmin(2) = ncmin(2) + 1
              lmin(2)(ncmin(2):ncmin(2)) = '1'
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',' //
     1                       lmin(2)(1:ncmin(2))
              ncb    = ncb    + ncmin(1) + ncmin(2) + 4
              if (ICYCFL(1) .ne. 1 .or. CYCREG(13) .ne. 0) then
                  lmin(2)(ncmin(2):ncmin(2)) = '2'
                  bbuf(ncb+1:) = '[,' // lmin(2)(1:ncmin(2)) // ']'
                  ncb    = ncb    + ncmin(2) + 3
              endif
              ncb    = ncb    + 1
              bbuf(ncb:) = ']'
          endif
c
          if (ICYCFL(1) .eq. 1 .and. CYCREG(14) .ne. 0) then
              call getvwd (1083,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',n]'
              ncb    = ncb    + ncmin(1) + 6
          endif
c
          if (ICYCFL(30) .eq. 1) then
              call getvwd (934,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              ilev(nlev) = 21
              call docprm (lmin(2),ncmin(2),ilev,nlev)
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) //
     1                       lmin(2)(1:ncmin(2)) // ']'
              ncb    = ncb    + ncmin(1) + ncmin(2) + 4
          endif
c
          call getvwd (57,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          ilev(nlev) = 11
          call docprm (lmin(2),ncmin(2),ilev,nlev)
          bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) //
     1                   lmin(2)(1:ncmin(2))
          ncb    = ncb    + ncmin(1) + ncmin(2) + 3
c
          iout   = 1
 2500     if (ncb .gt. 80) then
              do 2600 i=80,1,-1
                  if (bbuf(i:i) .eq. ' ') then
                      obuf   = bbuf(1:i) // '$'
                      nco    = i      + 1
                      call docout (obuf,nco,iout,cmsg,kerr)
                      bbuf1  = bbuf(i+1:ncb)
                      inc    = ncm    + 4
                      bbuf   = lspac(1:inc) // bbuf1
                      ncb    = ncb     - i + ncm    + 4
                      iout   = 0
                      go to 2650
                  endif
 2600         continue
          else
              call docout (bbuf,ncb,iout,cmsg,kerr)
              ncb    = 0
              iout   = 0
          endif
c
 2650     obuf   = ' '
          nco    = 0
          if (inc1 .ne. 0) then
              obuf   = lspac(1:inc1) // lmin(5)(1:ncmin(5))
              nco    = inc1   + ncmin(5)
          endif
c
          if (inc2 .ne. 0) then
              isub(1) = 74
              isub(2) = 315
              isub(3) = 316
              isub(4) = 143
              do 2700 i=1,4,1
                  call getvwd (isub(i),tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
                  obuf(inc2+1:) = tbuf(1:nct)
                  nco    = inc2   + nct
                  call docout (obuf,nco,0,cmsg,kerr)
                  obuf   = ' '
 2700         continue
              call docout (' ',0,0,cmsg,kerr)
              inc1   = 0
              inc2   = 0
          else if (inc1 .ne. 0) then
              call docout (obuf,nco,0,cmsg,kerr)
              call docout (' ',0,0,cmsg,kerr)
              inc1   = 0
          endif
          if (ncb .gt. 0) go to 2500
          if (kerr .ne. 0) go to 8000
      endif
c
c...Lathe CYCLE
c
      if (MTPDYN .eq. 2) then
          call getvwd (1054,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          ilev(nlev) = 4
          call docprm (lmin(1),ncmin(1),ilev,nlev)
          call getvwd (86,lmin(2),ncmin(2),2,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (84,lmin(3),ncmin(3),2,PSTWRD,PSTWVL,NPSTWD)
          bbuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ' [,' //
     1             lmin(2)(1:ncmin(2)) // ',z] [,' //
     2             lmin(3)(1:ncmin(3)) // ',x]'
          inc1   = ncm    + ncmin(1) + 4
          inc2   = inc1   + ncmin(2) + 6
          ncb    = inc2   + ncmin(3) + 3
c
          call getvwd (281,lmin(2),ncmin(2),2,PSTWRD,PSTWVL,NPSTWD)
          ilev(nlev) = 5
          call docprm (lmin(3),ncmin(3),ilev,nlev)
          bbuf(ncb+1:) = ' [,' // lmin(2)(1:ncmin(2)) // ',' //
     1                   lmin(3)(1:ncmin(3)) // ']'
          ncb    = ncb    + ncmin(2) + ncmin(3) + 5
c
          inc3   = 0
          call getvwd (73,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          ilev(nlev) = 6
          call docprm (lmin(2),ncmin(2),ilev,nlev)
          bbuf(ncb+1:) = ' [,' // lmin(1)(1:6) // ',' //
     1                   lmin(2)(1:ncmin(2)) // ']'
          inc3   = ncb    + 3
          inc4   = inc3
          ncb    = inc3   + ncmin(2) + 8
c
          if (ICYLFL(1) .ne. 1 .or. CYLREG(1) .ne. 0) then
              call getvwd (143,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              ilev(nlev) = 13
              call docprm (lmin(2),ncmin(2),ilev,nlev)
              ncmin(2) = ncmin(2) + 1
              lmin(2)(ncmin(2):ncmin(2)) = 'k'
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',' //
     1                       lmin(2)(1:ncmin(2))
              ncb    = ncb    + ncmin(1) + ncmin(2) + 4
              if (ICYLFL(1) .ne. 1 .or. CYLREG(2) .ne. 0) then
                  lmin(2)(ncmin(2):ncmin(2)) = 'i'
                  bbuf(ncb+1:) = '[,' // lmin(2)(1:ncmin(2)) // ']'
                  ncb    = ncb    + ncmin(2) + 3
              endif
              ncb    = ncb    + 1
              bbuf(ncb:) = ']'
          endif
c
          if (ICYLFL(1) .ne. 1 .or. CYLREG(10) .ne. 0) then
              call getvwd (280,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',r]'
              ncb    = ncb    + ncmin(1) + 6
          endif
c
          if (ICYLFL(1) .ne. 1 .or. CYLREG(14) .ne. 0) then
              call getvwd (92,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              ilev(nlev) = 8
              call docprm (lmin(2),ncmin(2),ilev,nlev)
              ncmin(2) = ncmin(2) + 1
              lmin(2)(ncmin(2):ncmin(2)) = '1'
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',' //
     1                       lmin(2)(1:ncmin(2))
              ncb    = ncb    + ncmin(1) + ncmin(2) + 4
              if (ICYLFL(1) .ne. 1 .or. CYLREG(15) .ne. 0) then
                  lmin(2)(ncmin(2):ncmin(2)) = '2'
                  bbuf(ncb+1:) = '[,' // lmin(2)(1:ncmin(2)) // ']'
                  ncb    = ncb    + ncmin(2) + 3
              endif
              ncb    = ncb    + 1
              bbuf(ncb:) = ']'
          endif
c
          if (ICYLFL(1) .ne. 1 .or. CYLREG(8) .ne. 0) then
              call getvwd (705,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              ilev(nlev) = 9
              call docprm (tbuf,nct,ilev,nlev)
              ncmin(2) = nct    + 1
              lmin(2) = 'z' // tbuf(1:nct)
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',' //
     1                       lmin(2)(1:ncmin(2))
              ncb    = ncb    + ncmin(1) + ncmin(2) + 4
              if (ICYLFL(1) .ne. 1 .or. CYLREG(9) .ne. 0) then
                  lmin(2)(1:1) = 'x'
                  bbuf(ncb+1:) = '[,' // lmin(2)(1:ncmin(2)) // ']'
                  ncb    = ncb    + ncmin(2) + 3
              endif
              ncb    = ncb    + 1
              bbuf(ncb:) = ']'
          endif
c
          if (ICYLFL(1) .ne. 1 .or. CYLREG(12) .ne. 0) then
              call getvwd (295,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              ilev(nlev) = 10
              call docprm (lmin(2),ncmin(2),ilev,nlev)
              ncmin(2) = ncmin(2) + 1
              lmin(2)(ncmin(2):ncmin(2)) = '1'
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',' //
     1                       lmin(2)(1:ncmin(2))
              ncb    = ncb    + ncmin(1) + ncmin(2) + 4
              if (ICYLFL(1) .ne. 1 .or. CYLREG(13) .ne. 0) then
                  lmin(2)(ncmin(2):ncmin(2)) = '2'
                  bbuf(ncb+1:) = '[,' // lmin(2)(1:ncmin(2)) // ']'
                  ncb    = ncb    + ncmin(2) + 3
              endif
              ncb    = ncb    + 1
              bbuf(ncb:) = ']'
          endif
c
          if (ICYLFL(1) .ne. 1 .or. CYLREG(16) .ne. 0) then
              call getvwd (617,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',t1'
              ncb    = ncb    + ncmin(1) + 6
              if (ICYLFL(1) .ne. 1 .or. CYLREG(17) .ne. 0) then
                  bbuf(ncb+1:) = '[,t2]'
                  ncb    = ncb    + 5
              endif
              ncb    = ncb    + 1
              bbuf(ncb:) = ']'
          endif
c
          if (ICYLFL(1) .ne. 1 .or. CYLREG(11) .ne. 0) then
              call getvwd (1083,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',n]'
              ncb    = ncb    + ncmin(1) + 6
          endif
c
          if (ICYLFL(1) .ne. 1 .or. CYLCOD(24) .ne. 0 .or.
     1        CYLCOD(25) .ne. 0) then
              bbuf(ncb+1:) = ' [,' // lon(1:6) // ']'
              inc4   = ncb    + 3
              ncb    = ncb    + 10
          endif
c
          iout   = 1
 3000     if (ncb .gt. 80) then
              do 3100 i=80,1,-1
                  if (bbuf(i:i) .eq. ' ') then
                      obuf   = bbuf(1:i) // '$'
                      nco    = i      + 1
                      call docout (obuf,nco,iout,cmsg,kerr)
                      bbuf1  = bbuf(i+1:ncb)
                      inc    = ncm    + 4
                      bbuf   = lspac(1:inc) // bbuf1
                      ncb    = ncb    - i + ncm    + 4
                      inc4   = inc4   - nco    + ncm    + 5
                      iout   = 0
                      go to 3150
                  endif
 3100         continue
          else
              call docout (bbuf,ncb,iout,cmsg,kerr)
              ncb    = 0
              iout   = 0
          endif
c
 3150     obuf   = ' '
          nco    = 0
          if (inc1 .ne. 0) then
              call getvwd (118,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf   = lspac(1:inc1) // tbuf(1:nct)
              nco    = inc1   + nct
          endif
c
          if (inc2 .ne. 0) then
              call getvwd (116,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc2+1:) = tbuf(1:nct)
              nco    = inc2   + nct
          endif
c
          if (inc3 .ne. 0) then
              isub(1) = 74
              isub(2) = 315
              isub(3) = 316
              do 3200 i=1,3,1
                  call getvwd (isub(i),tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
                  obuf(inc3+1:) = tbuf(1:nct)
                  nco    = inc3   + nct
                  call docout (obuf,nco,0,cmsg,kerr)
                  obuf   = ' '
 3200         continue
              call docout (' ',0,0,cmsg,kerr)
              inc1   = 0
              inc2   = 0
              inc3   = 0
          else if (inc1 .ne. 0) then
              call docout (obuf,nco,0,cmsg,kerr)
              call docout (' ',0,0,cmsg,kerr)
              inc1   = 0
              inc2   = 0
          endif
          if (ncb .gt. 0) go to 3000
c
          if (inc4 .gt. 0) then
              obuf   = lspac(1:inc4) // loff(1:ncoff)
              nco    = inc4   + ncoff
              call docout (obuf,nco,0,cmsg,kerr)
          endif
c
          if (kerr .ne. 0) go to 8000
      endif
c
c...CYCLE/AUTO
c
      if ((MTPDYN .eq. 2 .and. ICYLFL(1) .eq. 1) .or. ICYCFL(1) .eq. 1)
     1        then
          obuf   = lmaj(1:ncm) // '/' // lauto(1:ncauto) // ',' //
     1             lon(1:ncon)
          nco    = ncm    + ncauto + ncon   + 2
          call docout (obuf,nco,1,cmsg,kerr)
          nco    = ncm    + ncauto + 2
          obuf   = lspac(1:nco) // loff(1:ncoff)
          nco    = nco    + ncoff
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...CYCLE/AVOID
c
      if (MACHTP .ne. 2) then
          call getvwd (187,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1))
          nco    = ncm    + ncmin(1) + 1
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...CYCLE/ON
c
      obuf   = lmaj(1:ncm) // '/' // lon(1:ncon)
      nco    = ncm    + ncon   + 1
      call docout (obuf,nco,1,cmsg,kerr)
      obuf   = lspac(1:ncm+1) // loff(1:ncoff)
      nco    = ncm    + ncoff  + 1
      call docout (obuf,nco,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...CYCLE/CIRCUL
c
      if (MACHTP .ne. 2) then
          call getvwd (75,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          bbuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1))
          ncb    = ncm    + ncmin(1) + 1
c
          call getvwd (510,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ', ' // lmin(1)(1:ncmin(1)) // ',z'
          ncb    = ncb    + ncmin(1) + 4
c
          call getvwd (23,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ', ' // lmin(1)(1:ncmin(1)) // ',d,' //
     1                   lon(1:ncon)
          inc1   = ncb    + ncmin(1) + 6
          ncb    = ncb    + ncmin(1) + 6 + 3
c
          call getvwd (617,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          ilev(nlev) = 22
          call docprm (lmin(2),ncmin(2),ilev,nlev)
          bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',' //
     1                   lmin(2)(1:ncmin(2)) // ']'
          ncb    = ncb    + ncmin(1) + ncmin(2) + 5
c
          call getvwd (92,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ', ' // lmin(1)(1:ncmin(1)) // ',z'
          ncb    = ncb    + ncmin(1) + 4
c
          call getvwd (73,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1))
          inc2   = ncb    + 4
          ncb    = ncb    + 6 + 3
          bbuf(ncb+1:) = ',f]'
          ncb    = ncb    + 3
c
          call getvwd (280,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',r]'
          ncb    = ncb    + ncmin(1) + 6
c
          call getvwd (1,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1)) // ',a]'
          ncb    = ncb    + ncmin(1) + 6
c
          call getvwd (113,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1))
          inc3   = ncb    + 4
          ncb    = ncb    + 6 + 3
          bbuf(ncb+1:) = ']'
          ncb    = ncb    + 1
c
          call getvwd (59,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ' [,' // lmin(1)(1:ncmin(1))
          inc4   = ncb    + 4
          ncb    = ncb    + 6 + 3
          bbuf(ncb+1:) = ']'
          ncb    = ncb    + 1
c
          iout   = 1
 3300     ifl    = 0
          ifl1   = 0
          ifl2   = 0
          if (ncb .gt. 80) then
              do 3350 i=80,1,-1
                  if (bbuf(i:i) .eq. ' ') then
                      obuf   = bbuf(1:i) // '$'
                      nco    = i      + 1
                      call docout (obuf,nco,iout,cmsg,kerr)
                      bbuf1  = bbuf(i+1:ncb)
                      inc    = ncm    + 4
                      bbuf   = lspac(1:inc) // bbuf1
                      ncb    = ncb    - i + ncm    + 4
                      if (inc2 .gt. i) then
                          inc2   = inc2   - nco    + ncm    + 5
                          ifl    = 1
                      endif
                      if (inc3 .gt. i) then
                          inc3   = inc3   - nco    + ncm    + 5
                          ifl1   = 1
                      endif
                      if (inc4 .gt. i) then
                          inc4   = inc4   - nco    + ncm    + 5
                          ifl2   = 1
                      endif
                      iout   = 0
                      go to 3360
                  endif
 3350         continue
          else
              call docout (bbuf,ncb,iout,cmsg,kerr)
              ncb    = 0
          endif
c
 3360     obuf   = ' '
          nco    = 0
          if (inc1 .ne. 0) then
              call getvwd (48,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc1:) = tbuf(1:nct)
              nco    = inc1   + nct    - 1
              inc1   = 0
          endif
c
          if (inc2 .ne. 0 .and. ifl .eq. 0) then
              call getvwd (74,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc2:) = tbuf(1:nct)
              nco    = inc2   + nct    - 1
              inc2   = 0
          endif
c
          if (inc3 .ne. 0 .and. ifl1 .eq. 0) then
              call getvwd (112,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc3:) = tbuf(1:nct)
              nco    = inc3   + nct    - 1
              inc3   = 0
          endif
c
          if (inc4 .ne. 0 .and. ifl2 .eq. 0) then
              call getvwd (60,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc4:) = tbuf(1:nct)
              nco    = inc4   + nct    - 1
              inc4   = 0
          endif
c
          if (nco .ne. 0) then
              call docout (obuf,nco,0,cmsg,kerr)
          endif
c
          if (ncb .gt. 0) then
              call docout (' ',0,0,cmsg,kerr)
              go to 3300
          endif
          if (kerr .ne. 0) go to 8000
      endif
c
c...DELAY
c
      if (DELYCD(1) .ne. 0 .or. DELYCD(2) .ne. 0 .or. DELYCD(3) .ne. 0)
     1        then
          call getvwd (1010,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (97,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/n [,' // lmin(1)(1:ncmin(1)) // ']'
          nco    = ncm    + ncmin(1) + 6
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...DISPLY
c
      if (NCBMSG .ne. 0 .or. NCEMSG .ne. 0) then
          call getvwd (1021,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // lon(1:ncon)
          nco    = ncm    + ncon   + 1
          call docout (obuf,nco,1,cmsg,kerr)
          obuf   = lspac(1:ncm+1) // loff(1:ncoff)
          nco    = ncm    + ncoff  + 1
          call docout (obuf,nco,0,cmsg,kerr)
          obuf   = lspac(1:ncm+1) // lauto(1:ncauto)
          nco    = ncm    + ncauto + 1
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
       endif
c
c...END
c
      call getvwd (1,obuf,nco,1,PSTWRD,PSTWVL,NPSTWD)
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...FEDRAT/mode
c
      isub(1) = 73
      isub(2) = 74
      isub(3) = 315
      isub(4) = 316
      isub(5) = 6
      inc    = IFITYP
      if (inc .eq. 4) inc = 5
      isub(6) = isub(inc)
      isub(inc) = isub(1)
      isub(1) = isub(6)
      call getvwd (1009,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (isub(1),tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lmaj(1:ncm) // '/[' // tbuf(1:6) // ',] [n]'
      nco    = ncm    + 14
      call docout (obuf,nco,1,cmsg,kerr)
      inc    = 5
      if (IFDSUP(4) .ne. 1) inc = 4
      do 3500 i=2,inc,1
          call getvwd (isub(i),tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lspac(1:ncm+2) // tbuf(1:nct)
          nco    = ncm    + nct    + 2
          call docout (obuf,nco,0,cmsg,kerr)
 3500 continue
      if (kerr .ne. 0) go to 8000
c
c...FEDRAT/LENGTH
c
      if (IRTNUM .ne. 0) then
          call getvwd (9,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:6) // ',n'
          nco    = ncm    + 9
          call docout (obuf,nco,1,cmsg,kerr)
          call getvwd (25,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lspac(1:ncm+1) // tbuf(1:nct)
          nco    = ncm    + nct    + 1
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...FEDRAT/LOCK
c
      if (FEDOCD(1) .ne. 0 .or. FEDOCD(2) .ne. 0) then
          call getvwd (114,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1             lon(1:ncon)
          nco    = ncm    + nct    + ncon   + 2
          call docout (obuf,nco,1,cmsg,kerr)
          inc    = ncm    + nct    + 2
          obuf   = lspac(1:inc) // loff(1:ncoff)
          nco    = inc    + ncoff
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...FEDRAT/MAXIPM
c
      if (IRTNUM .ne. 0) then
          call getvwd (96,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          call rtoc (MAXIPM,lmin(1),ncmin(1))
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1             lmin(1)(1:ncmin(1))
          nco    = ncm    + nct   + ncmin(1) + 2
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...FINI
c
      call getvwd (4012,obuf,nco,1,PSTWRD,PSTWVL,NPSTWD)
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...FROM
c
      call getvwd (4019,tbuf,nct,1,PSTWRD,PSTWVL,NPSTWD)
      if (MACHTP .eq. 2) then
          obuf   = tbuf(1:nct) // '/z,x'
          nco    = nct    + 4
      else if (IRTNUM .gt. 0) then
          obuf   = tbuf(1:nct) // '/x,y,z,i,j,k'
          nco    = nct    + 12
      else
          obuf   = tbuf(1:nct) // '/' // 'x,y,z'
          nco    = nct    + 6
      endif
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...GOHOME
c
      call getvwd (17,obuf,nco,1,PSTWRD,PSTWVL,NPSTWD)
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...GOHOME/mode
c
      if (HOMCOD(1) .ne. 0 .or. HOMCOD(2) .ne. 0 .or. HOMCOD(3) .ne. 0
     1    .or. HOMCOD(4) .ne. 0) then
          inc    = 0
          isub(1) = 1023
          isub(2) = 88
          isub(3) = 4019
          isub(4) = 162
          do 4000 i=1,4,1
              if (HOMCOD(i) .ne. 0) then
                  inc    = inc    + 1
                  call getvwd (isub(i),lmin(inc),ncmin(inc),2,PSTWRD,
     1                         PSTWVL,NPSTWD)
              endif
 4000     continue
c
          call getvwd (17,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          ilev(nlev) = 3
          call docprm (lmin(5),ncmin(5),ilev,nlev)
          bbuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1))
          ncb    = ncm    + ncmin(1) + 1
c
          do 4100 i=1,3,1
              if (NUMLIN(i) .ne. 0) then
                  bbuf(ncb+1:) = ' [,' // laxs(i)
                  ncb    = ncb    + ncaxs(i) + 3
                  if (NUMLIN(i) .eq. 2) then
                      bbuf(ncb+1:)  = '[,n]'
                      ncb    = ncb    + 4
                  endif
                  bbuf(ncb+1:) = ',' // lmin(5)(1:ncmin(5)) // ']'
                  ncb    = ncb    + ncmin(5) + 2
              endif
 4100     continue
c
          if (IRTNUM .ne. 0) then
              bbuf(ncb+1:)  = ' [,' // laxs(4)
              ncb    = ncb    + ncaxs(4) + 3
              if (IRTNUM .gt. 1) then
                  bbuf(ncb+1:) = '[,n]'
                  ncb    = ncb    + 4
              endif
              bbuf(ncb+1:)   = ',' // lmin(5)(1:ncmin(5)) // ']'
              ncb    = ncb    + ncmin(5) + 2
          endif
c
          iout   = 1
 4200     if (ncb .gt. 80) then
              do 4250 i=80,1,-1
                  if (bbuf(i:i) .eq. ' ') then
                      obuf   = bbuf(1:i) // '$'
                      nco    = i      + 1
                      call docout (obuf,nco,iout,cmsg,kerr)
                      bbuf1  = bbuf(i+1:ncb)
                      inc1   = ncm    + 4
                      bbuf   = lspac(1:inc1) // bbuf1
                      ncb    = ncb    - i + ncm    + 4
                      iout   = 0
                      go to 4270
                  endif
 4250         continue
          else
              call docout (bbuf,ncb,iout,cmsg,kerr)
              ncb    = 0
          endif
c
 4270     obuf   = ' '
          nco    = 0
          if (inc .gt. 1) then
              do 4300 i=2,inc,1
                  obuf   = lspac(1:ncm+1) // lmin(i)(1:ncmin(i))
                  nco    = ncm    + ncmin(i) + 1
                  call docout (obuf,nco,0,cmsg,kerr)
 4300         continue
              if (ncb .gt. 0) call docout (' ',0,0,cmsg,kerr)
          endif
          if (ncb .gt. 0) go to 4200
          if (kerr .ne. 0) go to 8000
      endif
c
c...HEAD/ZIGZAG
c
      if (MACHTP .eq. 3) then
          call getvwd (1002,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (170,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1             lon(1:ncon)
          nco    = ncm    + nct    + ncon + 2
          call docout (obuf,nco,1,cmsg,kerr)
          inc    = ncm    + nct    + 2
          obuf   = lspac(1:inc) // loff(1:ncoff)
          nco    = inc    + ncoff
          call docout (obuf,nco,0,cmsg,kerr)
          obuf   = lspac(1:inc) // 'h,d'
          nco    = inc    + 3
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...HEAD/m [,n]
c
      if (MACHTP .eq. 5) then
          call getvwd (1002,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (3001,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/m [,n] [,' //
     1             tbuf(1:6) // '        ]'
          nco    = ncm    + 10 + 6 + 9
          call docout (obuf,nco,1,cmsg,kerr)
          inc    = ncm    + 10
          call getvwd (9,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lspac(1:inc) // tbuf(1:6) // ',tl1,tl2'
          nco    = inc    + 6 + 8
          call docout (obuf,nco,0,cmsg,kerr)
      endif
c
c...INSERT
c
      call getvwd (1046,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      ilev(nlev) = 14
      call docprm (tbuf,nct,ilev,nlev)
      obuf   = lmaj(1:ncm) // ' ' // tbuf(1:nct)
      nco    = ncm    + nct    + 1
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...LEADER
c
      call getvwd (1013,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lmaj(1:ncm) // '/n'
      nco    = ncm    + 2
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...LINTOL/n
c
      if (IRTNUM .gt. 0 .or. IJKROT .eq. 1) then
          call getvwd (1067,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call rtoc (LNRTOL,tbuf,nct)
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct)
          nco    = ncm    + nct    + 1
          call docout (obuf,nco,1,cmsg,kerr)
          obuf   = lspac(1:ncm+1) // lon(1:ncon)
          nco    = ncm    + ncon   + 1
          call docout (obuf,nco,0,cmsg,kerr)
          obuf   = lspac(1:ncm+1) // loff(1:ncoff)
          nco    = ncm    + ncoff  + 1
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c...LINTOL/ADJUST,tol
c
          if (IJKROT .ne. 1) then
              call getvwd (1067,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
              call getvwd (159,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
              if (LNRADJ .eq. 1) then
                  lmin(2) = lon
                  ncmin(2) = ncon
                  tbuf   = loff
                  nct    = ncoff
              else
                  lmin(2) = loff
                  ncmin(2) = ncoff
                  tbuf   = lon
                  nct    = ncon
              endif
              if (nct .gt. ncmin(2)) ncmin(2) = nct
              call rtoc (LNRATL(1),lmin(3),ncmin(3))
              call getvwd (1,lmin(4),ncmin(4),2,PSTWRD,PSTWVL,NPSTWD)
              call rtoc (LNRATL(2),lmin(5),ncmin(5))
              call getvwd (188,lmin(6),ncmin(6),2,PSTWRD,PSTWVL,NPSTWD)
              call rtoc (LNRATL(3),lmin(7),ncmin(7))
              call getvwd (9,lmin(8),ncmin(8),2,PSTWRD,PSTWVL,NPSTWD)
              call rtoc (LNRATL(4),lmin(9),ncmin(9))
              obuf = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ',' //
     1               lmin(2)(1:ncmin(2)) // ',' // lmin(3)(1:ncmin(3))
     2               // ' [,' // lmin(4)(1:ncmin(4)) // ',' //
     3               lmin(5)(1:ncmin(5)) // '] [,' //
     4               lmin(6)(1:ncmin(6)) // ',' // lmin(7)(1:ncmin(7))
     5               // ']' // ' [,' // lmin(8)(1:ncmin(8)) // ',' //
     6               lmin(9)(1:ncmin(9)) // ']'
              nco    = strlen1(obuf)
              call docout (obuf,nco,1,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
c
              obuf   = lspac(1:ncm+ncmin(1)+2) // tbuf(1:nct)
              nco    = ncm    + ncmin(1) + nct + 2
              call docout (obuf,nco,0,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
c
c...LINTOL/AXIS,n
c
              call rtoc (VTOLER,tbuf,nct)
              obuf   = lmaj(1:ncm) // '/' // laxs(4)(1:ncaxs(4)) //
     -              ',' // tbuf(1:nct)
              nco    = ncm    + ncaxs(4) + nct + 2
              call docout (obuf,nco,1,cmsg,kerr)
          endif
      endif
c
c...prepst LINTOL/LINEAR,delta
c...etc
c
      call getvwd (1067,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      isub(1) = 76
      isub(2) = 731
      call getvwd (isub(1),lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (isub(2),lmin(2),ncmin(2),2,PSTWRD,PSTWVL,NPSTWD)
      nct    = ncmin(1)
      if (ncmin(2)  .gt. nct) nct = ncmin(2)
      tbuf   = lmin(1)(1:ncmin(1))
      if (nct .gt. ncmin(1)) tbuf(ncmin(1)+1:nct) = ' '
      obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ', delta'
      nco    = ncm    + nct + 8
      call docout (obuf,nco,1,cmsg,kerr)
      tbuf   = lmin(2)(1:ncmin(2))
      if (nct .gt. ncmin(2)) tbuf(ncmin(2)+1:nct) = ' '
      obuf   = lspac(1:ncm+1) // tbuf(1:nct) // ', tol'
      nco    = ncm    + nct + 6
      call docout (obuf,nco,0,cmsg,kerr)
      obuf   = lspac(1:ncm+nct+3) // loff(1:ncoff)
      nco    = ncm    + nct + ncoff + 3
      call docout (obuf,nco,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...LINTOL/RAPID
c
      if (IRTNUM .gt. 0 .or. IJKROT .eq. 1) then
          call getvwd (1067,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (5,lmin(1),ncmin(1),1,PSTWRD,PSTWVL,NPSTWD)
          if (ILINRP .eq. 1) then
              lmin(2) = lon
              ncmin(2) = ncon
              tbuf   = loff
              nct    = ncoff
          else
              lmin(2) = loff
              ncmin(2) = ncoff
              tbuf   = lon
              nct    = ncon
          endif
          if (nct .gt. ncmin(2)) ncmin(2) = nct
          obuf = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ',' //
     1           lmin(2)(1:ncmin(2))
          nco    = strlen1(obuf)
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
          obuf   = lspac(1:ncm+ncmin(1)+2) // tbuf(1:nct)
          nco    = ncm    + ncmin(1) + nct + 2
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...LOADTL
c
      if (MACHTP .ne. 2) then
          call getvwd (1055,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (9,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/[tn [[,' // tbuf(1:nct) // '], tl]]'
          nco    = strlen1(obuf)
c
          if (TLOCD(1) .ne. 0 .or. CUTCCD(4) .ne. 0) then
              call getvwd (705,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(nco+1:) = ' [,' // tbuf(1:nct) // ',h'
              nco    = nco    + nct    + 5
              if (CUTCCD(4) .ne. 0) then
                  obuf(nco+1:) = '[,d]'
                  nco    = nco    + 4
              endif
              nco    = nco    + 1
              obuf(nco:nco) = ']'
          endif
c
          inc1   = 0
          if (TOOLFL(2) .eq. 1) then
              call getvwd (7,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              inc1   = nco    + 4
              obuf(nco+1:) = ' [,' // tbuf(1:6) // ']'
              nco    = nco    + 10
          endif
c
          inc2   = 0
          if (TOOLFL(3) .eq. 1) then
              call getvwd (62,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              inc2   = nco    + 4
              obuf(nco+1:) = ' [,' // tbuf(1:6) // ']'
              nco    = nco    + 10
          endif
c
          inc3   = nco    + 4
          obuf(nco+1:) = ' [,' // lauto(1:6) // ']'
          nco    = nco    + 10
c
          call docout (obuf,nco,1,cmsg,kerr)
          obuf   = ' '
          if (inc1 .ne. 0) then
              call getvwd (26,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc1:) = tbuf(1:nct)
          endif
          if (inc2 .ne. 0) then
              call getvwd (63,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc2:) = tbuf(1:nct)
          endif
          nco    = inc3   + ncmod  - 1
          obuf(inc3:) = lmod(1:ncmod)
          call docout (obuf,nco,0,cmsg,kerr)
          obuf   = ' '
          call getvwd (158,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(inc3:) = tbuf(1:nct)
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...MACHIN
c
      call getvwd (1015,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (144,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
      nct    = strlen1(LMNAME)
      obuf   = lmaj(1:ncm) // '/PWORKS,' // LMNAME(1:nct) // ',' //
     1         lmin(1)(1:ncmin(1))
      nco    = strlen1(obuf)
      do 4500 i=1,6,1
          call itoc (i,tbuf,nct,0)
          inum   = MCHOPT(i)
          call itoc (inum,lmin(1),ncmin(1),0)
          if (i .eq. 4 .and. PSTNRG(11) .ne. 0) then
              obuf(nco+1:) = ', 4,n'
              nco    = nco    + 5
          else
              obuf(nco+1:) = ', ' // tbuf(1:nct) // ',' //
     1                       lmin(1)(1:ncmin(1))
              nco    = nco    + nct    + ncmin(1) + 3
          endif
 4500 continue
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...MAXDPM
c
      if (IRTNUM .ne. 0) then
          call getvwd (1062,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call rtoc (MAXDPM,tbuf,nct)
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct)
          nco    = ncm    + nct    + 1
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...MCHTOL
c
      if (ICIRFL .eq. 1) then
          call getvwd (1016,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call rtoc (CIRTOL(1),tbuf,nct)
          call rtoc (CIRTOL(2),lmin(1),ncmin(1))
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ' [,' //
     1             lmin(1)(1:ncmin(1)) // ']'
          nco    = ncm    + nct    + ncmin(1) + 5
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...MODE/AXIS
c
      call getvwd (1003,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      if (IRTNUM .gt. 2) then
          obuf   = lmaj(1:ncm) // '/' // laxs(4)(1:ncaxs(4))
          nco    = ncm    + ncaxs(4) + 1
          do 5000 i=1,IRTNUM,1
              if (IRDEAD(i) .eq. 0) then
                  call itoc (i,tbuf,nct,0)
                  obuf(nco+1:) = ',' // tbuf(1:nct)
                  nco    = nco    + nct    + 1
              endif
 5000     continue
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...MODE/BLADE
c
      if (MACHTP .eq. 3) then
          call getvwd (191,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1             lon(1:ncon)
          nco    = ncm    + nct    + ncon + 2
          call docout (obuf,nco,1,cmsg,kerr)
          nco    = ncm    + nct    + 2
          obuf   = lspac(1:nco) // loff(1:ncoff)
          nco    = nco    + ncoff
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...MODE/INCR
c
      call getvwd (66,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      if (INCR .eq. 1) then
          lmin(1) = lon
          ncmin(1) = ncon
          lmin(2) = loff
          ncmin(2) = ncoff
      else
          lmin(1) = loff
          ncmin(1) = ncoff
          lmin(2) = lon
          ncmin(2) = ncon
      endif
      obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1         lmin(1)(1:ncmin(1))
      nco    = ncm    + nct    + ncmin(1) + 2
      call docout (obuf,nco,1,cmsg,kerr)
      nco    = ncm    + nct    + 2
      obuf   = lspac(1:nco) // lmin(2)(1:ncmin(2))
      nco    = nco    + ncmin(2)
      call docout (obuf,nco,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...MODE/LATHE
c...     MILL,AUTO
c
      if (MACHTP .eq. 4) then
          call getvwd (700,tbuf,nct,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (33,lmin(2),ncmin(2),1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (41,lmin(3),ncmin(3),1,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) //  ' [,' //
     -             lmin(2)(1:ncmin(2)) // ']'
          lmin(1) = obuf(1:ncm+1)
          nco    = ncm   + nct   +  ncmin(2) + 5
          call docout (obuf,nco,1,cmsg,kerr)
          obuf   = lspac(1:ncm+nct+4) // lmin(3)(1:ncmin(3))
          nco    = ncm + nct + ncmin(3) + 4
          call docout (obuf,nco,0,cmsg,kerr)
c
          call getvwd (151,tbuf,ncb,1,PSTWRD,PSTWVL,NPSTWD)
          inc1   = ncm   + ncb + 1
          sbuf   = lmin(1)(1:ncm+1) // tbuf(1:ncb)
          call getvwd (88,tbuf,inum,1,PSTWRD,PSTWVL,NPSTWD)
          obuf   = sbuf(1:inc1) // ',' // tbuf(1:inum) // ' [,' //
     -             lmin(2)(1:ncmin(2)) // ']'
          lmin(1) = obuf
          nco    = inc1  +  inum + ncmin(2) + 5
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          obuf   = lspac(1:inc1+inum+4) // lmin(3)(1:ncmin(3))
          nco    = inc1 + inum + ncmin(3) + 4
          call docout (obuf,nco,0,cmsg,kerr)
c
c......MILL,DIAMTR
c           FACE
c
          sbuf(1:7) = ' [,tol]'
          if (LTHPCL(2) .eq. 1) then
             call getvwd (205,tbuf,inum,1,PSTWRD,PSTWVL,NPSTWD)
             if (MODCYL .gt. 0) then
                call doc_cyl1 (lmin(2),ncmin(2),lmin(3),ncmin(3))
                tbuf(inum+1:) = ' [,' // lmin(2)(1:ncmin(2)) // ']'
                inc2 = inc1 + 4 + inum
                inum = inum + ncmin(2) + 4
             end if
             obuf = lmin(1)(1:inc1+1) // tbuf(1:inum) // sbuf(1:7)
             nco  = inc1 + inum + 8
             call docout (obuf,nco,1,cmsg,kerr)
             if (MODCYL .gt. 0) then
                obuf = lspac(1:inc2) // lmin(3)(1:ncmin(3))
                nco  = inc2 + ncmin(3)
                call docout (obuf,nco,0,cmsg,kerr)
             end if
          end if
          if (LTHPCL(1) .eq. 1) then
             call getvwd (81,tbuf,inum,1,PSTWRD,PSTWVL,NPSTWD)
             obuf = lmin(1)(1:inc1+1) // tbuf(1:inum) // sbuf(1:7)
             nco  = inc1 + inum + 8
             call docout (obuf,nco,1,cmsg,kerr)
          end if
      end if
c
c...MODE/ROTATE
c
      if (jindex(IRTYPE,1,IRTNUM) .ne. 0) then
          call getvwd (1066,tbuf,nct,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (88,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          inc1   = ncm    + nct  + 2
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1             loff(1:ncoff)
          nco    = inc1   + ncoff
          call docout (obuf,nco,1,cmsg,kerr)
          inc2   = ncon
          if (ncon .lt. ncmin(1)) inc2 = ncmin(1)
          ncb    = ncmin(1) - ncon + 1
          if (ncb .le. 0) ncb = 1
          obuf   = lspac(1:inc1) // lon(1:ncon) // lspac(1:ncb) //
     1             '[,n]'
          nco    = inc1   + inc2 + 5
          call docout (obuf,nco,0,cmsg,kerr)
          obuf   = lspac(1:inc1) // lmin(1)(1:ncmin(1))
          nco    = inc1   + ncmin(1)
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...MODE/TOOL
c
      if (MACHTP .ne. 2) then
          call getvwd (617,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (33,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1             lmin(1)(1:ncmin(1))
          nco    = ncm    + nct    + ncmin(1) + 2
          call docout (obuf,nco,1,cmsg,kerr)
c
          inc    = ncm    + nct    + 2
          call getvwd (41,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lspac(1:inc) // tbuf(1:nct)
          nco    = inc    + nct
          call docout (obuf,nco,0,cmsg,kerr)
c
          call getvwd (37,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lspac(1:inc) // tbuf(1:nct)
          nco    = inc    + nct
          call docout (obuf,nco,0,cmsg,kerr)
c
          call getr8s (SPIVEC,3,tbuf,nct)
          obuf   = lspac(1:inc) // tbuf(1:nct)
          nco    = inc    + nct
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...MODE/-AXIS
c
      if (NUMLIN(1) .eq. 2 .or. NUMLIN(2) .eq. 2 .or. NUMLIN(3) .eq. 2)
     1        then
          obuf   = lmaj(1:ncm) // '/'
          nco    = ncm    + 1
          iout   = 1
          do 5300,i=1,3,1
              if (NUMLIN(i) .eq. 2) then
                  call itoc (ACTLIN(i),tbuf,nct,0)
                  obuf(nco+1:) = laxs(i)(1:ncaxs(i)) // ',' //
     1                           tbuf(1:nct)
                  nco    = nco    + ncaxs(i) + nct + 1
                  call docout (obuf,nco,iout,cmsg,kerr)
                  obuf   = ' '
                  nco    = ncm    + 1
                  iout   = 0
              endif
 5300     continue
          if (kerr .ne. 0) go to 8000
      endif
c
c...MULTAX
c
      if (IRTNUM .gt. 0) then
          call getvwd (1105,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // lon(1:ncon)
          nco    = ncm    + ncon   + 1
          call docout (obuf,nco,1,cmsg,kerr)
          obuf   = lspac(1:ncm+1) // loff(1:ncoff)
          nco    = ncm    + ncoff  + 1
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...OPSKIP
c
      if (NOPSKP .gt. 0) then
          call getvwd (1012,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // lon(1:ncon) //
     1             '  [,n1 [...] n9]'
          nco    = strlen1(obuf)
          call docout (obuf,nco,1,cmsg,kerr)
          obuf   = lspac(1:ncm+1) // loff(1:ncoff)
          nco    = ncm    + ncoff  + 1
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...OPSTOP
c
      if (OPSTCD .ne. 0) then
          call getvwd (3,obuf,nco,1,PSTWRD,PSTWVL,NPSTWD)
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...ORIGIN
c
      call getvwd (1027,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      if (MACHTP .eq. 2) then
          obuf   = lmaj(1:ncm) // '/z,x'
          nco    = ncm    + 4
      else
          obuf   = lmaj(1:ncm) // '/x,y [,z]'
          nco    = ncm    + 9
      endif
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...PARTNO
c
      call getvwd (1045,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      ilev(nlev) = 14
      call docprm (tbuf,nct,ilev,nlev)
      obuf   = lmaj(1:ncm) // ' ' // tbuf(1:nct)
      nco    = ncm    + nct    + 1
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...PLUNGE
c
      call getvwd (1001,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      ilev(nlev) = 15
      call docprm (tbuf,nct,ilev,nlev)
      obuf   = lmaj(1:ncm) // ' [/' // tbuf(1:nct) // ']'
      nco    = ncm    + nct    + 4
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...POD/AIR
c
      if (MACHTP .eq. 3) then
          call getvwd (1088,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          if (PODACD(1) .ne. 0 .or. PODACD(2) .ne. 0 .or.
     1        PODACD(5) .ne. 0 .or. PODACD(6) .ne. 0) then
              call getvwd (1011,sbuf,ncs,2,PSTWRD,PSTWVL,NPSTWD)
              obuf   = lmaj(1:ncm) // '/' // sbuf(1:ncs) // ','
              nco    = ncm    + ncs    + 2
              inc    = nco    + 1
              isub(1) = 71
              isub(2) = 72
              isub(3) = 63
              isub(4) = 62
              isub(5) = 53
              isub(6) = 5
              isub(7) = 6
              isub(8) = 1
              isub(9) = 2
              isub(10) = 7
              iout   = 1
              do 5200 i=1,5,1
                  if (PODACD(isub(i+5)) .ne. 0) then
                      call getvwd (isub(i),tbuf,nct,2,PSTWRD,PSTWVL,
     1                             NPSTWD)
                      obuf(inc:) = tbuf(1:nct)
                      nco    = inc    + nct    - 1
                      call docout (obuf,nco,iout,cmsg,kerr)
                      obuf   = ' '
                      iout   = 0
                  endif
 5200         continue
              if (kerr .ne. 0) go to 8000
          endif
c
c...POD/AIR,LOCK
c
          if (PODACD(3) .ne. 0) then
              call getvwd (1011,sbuf,ncs,2,PSTWRD,PSTWVL,NPSTWD)
              call getvwd (114,sbuf,ncs,2,PSTWRD,PSTWVL,NPSTWD)
              obuf   = lmaj(1:ncm) // '/' // sbuf(1:ncs) // ',' //
     1                 lon(1:ncon)
              nco    = ncm    + ncs    + ncon   + 2
              call docout (obuf,nco,1,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
c
c...POD/DOWN
c
          if (PODACD(4) .ne. 0) then
              call getvwd (113,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct)
              nco    = ncm    + nct    + 1
              call docout (obuf,nco,1,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
c
c...POD/row,col,AIR,OFF
c
          if (PODCCD(1) .ne. 0 .or. PODCCD(2) .ne. 0) then
              ilev(nlev) = 19
              call docprm (lmin(1),ncmin(1),ilev,nlev)
              ilev(nlev) = 20
              call docprm (lmin(2),ncmin(2),ilev,nlev)
              sbuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) //
     1                 ',' // lmin(2)(1:ncmin(2)) // ','
              ncs    = ncm    + ncmin(1) + ncmin(2) + 3
              if (PODSCD(6) .ne. 0) then
                  call getvwd (1011,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
                  obuf   = sbuf(1:ncs) // tbuf(1:nct) // ',' //
     1                     loff(1:ncoff)
                  nco    = ncs    + nct    + ncoff  + 1
                  call docout (obuf,nco,1,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              endif
c
c...POD/row,col,CLAMP
c
              if (PODSCD(4) .ne. 0 .or. PODSCD(5) .ne. 0) then
                  call getvwd (1060,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
                  obuf   = sbuf(1:ncs) // tbuf(1:nct) // ',' //
     1                     lon(1:ncon)
                  nco    = ncs    + nct    + ncon   + 1
                  call docout (obuf,nco,1,cmsg,kerr)
                  inc    = ncs    + nct    + 1
                  obuf   = lspac(1:inc) // loff(1:ncoff)
                  nco    = inc    + ncoff
                  call docout (obuf,nco,0,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              endif
c
c...POD/row,col,UP
c
              if (PODSCD(2) .ne. 0 .or. PODSCD(3) .ne. 0) then
                  call getvwd (112,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
                  obuf   = sbuf(1:ncs) // tbuf(1:nct)
                  nco    = ncs    + nct
                  if (PODSCD(3) .ne. 0) then
                      call getvwd (1060,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
                      obuf(nco+1:) = ' [,' // tbuf(1:nct) // ']'
                      nco    = nco    + nct    + 4
                  endif
                  call docout (obuf,nco,1,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              endif
          endif
      endif
c
c...POSITN
c
      call getvwd (1072,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      ilev(nlev) = 3
      call docprm (lmin(5),ncmin(5),ilev,nlev)
      obuf   = lmaj(1:ncm) // '/'
      nco    = ncm    + 1
c
      do 5500 i=1,3,1
          if (NUMLIN(i) .ne. 0) then
              obuf(nco+1:) = ' [,' // laxs(i)(1:ncaxs(i))
              nco    = nco    + ncaxs(i) + 3
              if (NUMLIN(i) .eq. 2) then
                  obuf(nco+1:)  = '[,n]'
                  nco    = nco    + 4
              endif
              obuf(nco+1:) = ',' // lmin(5)(1:ncmin(5)) // ']'
              nco    = nco    + ncmin(5) + 2
          endif
 5500  continue
c
      if (IRTNUM .ne. 0) then
          obuf(nco+1:)  = ' [,' // laxs(4)(1:ncaxs(4))
          nco    = nco    + ncaxs(4) + 3
          if (IRTNUM .gt. 1) then
              obuf(nco+1:) = '[,n]'
              nco    = nco    + 4
          endif
          obuf(nco+1:)   = ',' // lmin(5)(1:ncmin(5)) // ']'
          nco    = nco    + ncmin(5) + 2
      endif
c
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...POSTN/n
c
      call getvwd (1024,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      if (PSTNRG(11) .ne. 0) then
          obuf   = lmaj(1:ncm) // '/n'
          nco    = ncm    + 2
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...POSTN/NORMAL
c
      inc    = 0
      bbuf   = lmaj(1:ncm) // '/'
      ncb    = ncm    + 1
      if (PSTNCD(2) .ne. 0) then
          call getvwd (111,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (66,lmin(2),ncmin(2),2,PSTWRD,PSTWVL,NPSTWD)
          inc    = ncb    + 2
          bbuf(ncb+1:) = ' [' // lmin(1)(1:ncmin(1)) // ']'
          ncb    = ncb    + ncmin(1) + 3
      endif
c
      ilev(nlev) = 3
      call docprm (lmin(5),ncmin(5),ilev,nlev)
c
      do 5800 i=1,3,1
          if (NUMLIN(i) .ne. 0) then
              bbuf(ncb+1:) = ' [,' // laxs(i)
              ncb    = ncb    + ncaxs(i) + 3
              if (NUMLIN(i) .eq. 2) then
                  bbuf(ncb+1:)  = '[,n]'
                  ncb    = ncb    + 4
              endif
              bbuf(ncb+1:) = ',' // lmin(5)(1:ncmin(5)) // ']'
              ncb    = ncb    + ncmin(5) + 2
          endif
 5800 continue
c
      if (IRTNUM .ne. 0) then
          bbuf(ncb+1:)  = ' [,' // laxs(4)
          ncb    = ncb    + ncaxs(4) + 3
          if (IRTNUM .gt. 1) then
              bbuf(ncb+1:) = '[,n]'
              ncb    = ncb    + 4
          endif
          bbuf(ncb+1:)   = ',' // lmin(5)(1:ncmin(5)) // ']'
          ncb    = ncb    + ncmin(5) + 2
      endif
c
      iout   = 1
 5900 if (ncb .gt. 80) then
          do 5950 i=80,1,-1
              if (bbuf(i:i) .eq. ' ') then
                  obuf   = bbuf(1:i) // '$'
                  nco    = i      + 1
                  call docout (obuf,nco,iout,cmsg,kerr)
                  bbuf1  = bbuf(i+1:ncb)
                  inc1   = ncm    + 4
                  bbuf   = lspac(1:inc1) // bbuf1
                  ncb    = ncb    - i + ncm    + 4
                  iout   = 0
                  go to 5970
              endif
 5950     continue
      else
          call docout (bbuf,ncb,iout,cmsg,kerr)
          ncb    = 0
      endif
c
 5970 if (inc .gt. 0) then
          obuf   = lspac(1:inc) // lmin(2)(1:ncmin(2))
          nco    = inc    + ncmin(2)
          call docout (obuf,nco,0,cmsg,kerr)
          if (ncb .gt. 0) call docout (' ',0,0,cmsg,kerr)
      endif
c
      if (ncb .gt. 0) go to 5900
      if (kerr .ne. 0) go to 8000
c
c...POSTN/OFF
c
      if (PSTNCD(3) .ne. 0) then
          obuf   = lmaj(1:ncm) // '/' // loff(1:ncoff)
          nco    = ncm    + ncoff  + 1
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...POSTN/XYPLAN
c
      if (PSTNCD(1) .ne. 0) then
          call getvwd (33,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct)
          nco    = ncm    + nct    + 1
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...PPRINT
c
      call getvwd (1044,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      ilev(nlev) = 14
      call docprm (tbuf,nct,ilev,nlev)
      obuf   = lmaj(1:ncm) // ' ' // tbuf(1:nct)
      nco    = ncm    + nct    + 1
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...PPTOL
c
      call getvwd (1068,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      ilev(nlev) = 16
      call docprm (lmin(5),ncmin(5),ilev,nlev)
c
      obuf   = lmaj(1:ncm) // '/' // lmin(5)(1:ncmin(5))
      nco    = ncm    + ncmin(5) + 1
      call docout (obuf,nco,1,cmsg,kerr)
c
      obuf   = ' '
      nco    = ncm
c
      do 6500 i=1,3,1
          if (NUMLIN(i) .ne. 0) then
              obuf(nco+1:) = ' [,' // laxs(i)(1:ncaxs(i))
              nco    = nco    + ncaxs(i) + 3
              if (NUMLIN(i) .eq. 2) then
                  obuf(nco+1:)  = '[,n]'
                  nco    = nco    + 4
              endif
              obuf(nco+1:) = ',' // lmin(5)(1:ncmin(5)) // ']'
              nco    = nco    + ncmin(5) + 2
          endif
 6500  continue
c
      if (IRTNUM .ne. 0) then
          obuf(nco+1:)  = ' [,' // laxs(4)(1:ncaxs(4))
          nco    = nco    + ncaxs(4) + 3
          if (IRTNUM .gt. 1) then
              obuf(nco+1:) = '[,n]'
              nco    = nco    + 4
          endif
          obuf(nco+1:)   = ',' // lmin(5)(1:ncmin(5)) // ']'
          nco    = nco    + ncmin(5) + 2
      endif
c
      call docout (obuf,nco,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...PREFUN
c
      call getvwd (1048,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lmaj(1:ncm) // '/n [,' // lnow(1:6) // ']'
      nco    = ncm    + 12
      call docout (obuf,nco,1,cmsg,kerr)
      nco    = ncm    + 5
      obuf   = lspac(1:nco) // lnext(1:ncnxt)
      nco    = nco    + ncnxt
      call docout (obuf,nco,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...QAXIS/qval [,feed]
c
      if (MACHTP .eq. 5) then
          call getvwd (3005,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/qval [,feed]'
          nco    = ncm    + strlen1(obuf)
          call docout (obuf,nco,1,cmsg,kerr)
      endif
c
c...RAPID
c
      call getvwd (5,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (55,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (295,sbuf,ncs,2,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lmaj(1:ncm) // ' [/] [' // tbuf(1:nct) // '] [,] [' //
     1         laxs(1)(1:6) // '] [,] [' // sbuf(1:ncs) // '] [,] [' //
     2         lon(1:6) // ']'
      nco    = strlen1(obuf)
      call docout (obuf,nco,1,cmsg,kerr)
c
      inc1   = ncm    + nct    + 14
      inc2   = inc1   + ncs    + 20
      obuf   = ' '
      if (MACHTP .ne. 2) then
          obuf(inc1:) = laxs(2)(1:ncaxs(2))
      else
          obuf(inc1:) = laxs(3)(1:ncaxs(3))
      endif
      obuf(inc2:) = loff(1:ncoff)
      nco    = inc2   + ncoff  - 1
      call docout (obuf,nco,0,cmsg,kerr)
c
      if (MACHTP .ne. 2) then
          obuf(inc1:)  = laxs(3)(1:ncaxs(3))
          nco    = inc1   + ncaxs(3) - 1
          call docout (obuf,nco,0,cmsg,kerr)
c
          if (IRTNUM .gt. 0) then
              call getvwd (617,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc1:)  = tbuf(1:nct)
              nco    = inc1   + nct    - 1
              call docout (obuf,nco,0,cmsg,kerr)
          endif
      endif
      if (kerr .ne. 0) go to 8000
c
c...RAPID/FEDTO
c
      call getvwd (281,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (9,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1         lmin(1)(1:ncmin(1)) // ' [,n]'
      nco    = ncm    + nct    + ncmin(1) + 7
      call docout (obuf,nco,1,cmsg,kerr)
c
      call getvwd (25,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lspac(1:ncm+1) // tbuf(1:nct)
      nco    = ncm    + nct    + 1
      call docout (obuf,nco,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Old retract
c
cc      call getvwd (7,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
cc      ilev(nlev) = 15
cc      call docprm (tbuf,nct,ilev,nlev)
cc      obuf   = lmaj(1:ncm) // ' [/' // tbuf(1:nct) // ']'
cc      nco    = ncm    + nct    + 4
cc      call docout (obuf,nco,1,cmsg,kerr)
cc      if (kerr .ne. 0) go to 8000
c
c...RETRCT/feed
c
      call getvwd (7,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (70,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      ilev(nlev) = 15
      call docprm (sbuf,ncs,ilev,nlev)
      obuf   = lmaj(1:ncm) // ' [/] [' // tbuf(1:nct) // '] [,] [' //
     1         laxs(1)(1:6) // '] [,] [' // sbuf(1:ncs) // ']'
      nco    = strlen1(obuf)
      call docout (obuf,nco,1,cmsg,kerr)
c
      inc1   = ncm    + 7
      inc2   = ncm    + nct    + 14
      obuf   = ' '
      call getvwd (69,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      obuf(inc1:) = tbuf(1:nct)
      if (MACHTP .ne. 2) then
          obuf(inc2:) = laxs(2)(1:ncaxs(2))
      else
          obuf(inc2:) = laxs(3)(1:ncaxs(3))
      endif
      nco    = strlen1(obuf)
      call docout (obuf,nco,0,cmsg,kerr)
c
      if (MACHTP .ne. 2) then
          obuf   = ' '
          obuf(inc2:)  = laxs(3)(1:ncaxs(3))
          nco    = inc2   + ncaxs(3) - 1
          call docout (obuf,nco,0,cmsg,kerr)
c
          if (IRTNUM .gt. 0) then
              call getvwd (617,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc2:)  = tbuf(1:nct)
              nco    = inc2   + nct    - 1
              call docout (obuf,nco,0,cmsg,kerr)
          endif
      endif
      if (kerr .ne. 0) go to 8000
c
c...RETRCT/ON
c
      if (MACHTP .ne. 2 .and. ICYCFL(9) .eq. 1) then
          obuf   = lmaj(1:ncm) // '/' // lon(1:ncon)
          nco    = ncm    + ncon   + 1
          call docout (obuf,nco,1,cmsg,kerr)
          obuf   = lspac(1:ncm+1) // loff(1:ncoff)
          nco    = ncm    + ncoff  + 1
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...RETRCT/TOOL
c
      if (IRTNUM .gt. 0) then
          call getvwd (617,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          bbuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ' [,'
          ncb    = ncm    + nct    + 4
c
          inc1   = ncb    + 1
          if (LRTRCT .eq. 1) then
              bbuf(ncb+1:) = lon(1:6) // ']'
              ncb    = ncb    + 7
              lmin(9) = loff
              ncmin(9) = ncoff
          else
              bbuf(ncb+1:) = loff(1:6) // ']'
              ncb    = ncb    + 7
              lmin(9) = lon
              ncmin(9) = ncon
          endif
c
          call getvwd (9,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ' [,' // tbuf(1:nct) // '[,'
          ncb    = ncb    + nct    + 5
          inc2   = ncb    + 1
          if (LRTTAD .eq. 1) then
              bbuf(ncb+1:) = lon(1:6) // ']'
              ncb    = ncb    + 7
              lmin(10) = loff
              ncmin(10) = ncoff
          else
              bbuf(ncb+1:) = loff(1:6) // ']'
              ncb    = ncb    + 7
              lmin(10) = lon
              ncmin(10) = ncon
          endif
          call rtoc (RETDIS,tbuf,nct)
          bbuf(ncb+1:) = '[,' // tbuf(1:nct) // ']]'
          ncb    = ncb    + nct    + 4
c
          call getvwd (705,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ' [,' // tbuf(1:nct) // ','
          ncb    = ncb    + nct    + 4
          inc3   = ncb    + 1
          call getvwd (112,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (8,lmin(2),ncmin(2),2,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (24,lmin(3),ncmin(3),2,PSTWRD,PSTWVL,NPSTWD)
          if (IRTSHF .eq. 1) then
              is1    = 1
              is2    = 2
              is3    = 3
          else if (IRTSHF .eq. 2) then
              is1    = 2
              is2    = 3
              is3    = 1
          else
              is1    = 3
              is2    = 2
              is3    = 1
          endif
          bbuf(ncb+1:) = lmin(is1)(1:6)
          ncb    = ncb    + 6
          lmin(7) = lmin(is2)
          ncmin(7) = ncmin(is2)
          lmin(8) = lmin(is3)
          ncmin(8) = ncmin(is3)
          call rtoc (RTSHFD,tbuf,nct)
          bbuf(ncb+1:) = '[,' // tbuf(1:nct) // ']]'
          ncb    = ncb    + nct    + 4
c
          if (MACHTP .eq. 3) then
              call getvwd (1,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              call rtoc (BLDELT,lmin(1),ncmin(1))
              bbuf(ncb+1:) = ' [,' // tbuf(1:nct) // ',' //
     1                       lmin(1)(1:ncmin(1)) // ']'
              ncb    = ncb    + nct    + ncmin(1) + 5
          endif
c
c
          call getvwd (219,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          call rtoc (RETFED(1),lmin(1),ncmin(1))
          call rtoc (RETFED(2),lmin(2),ncmin(2))
          call rtoc (RETFED(4),lmin(3),ncmin(3))
          bbuf(ncb+1:) = ' [,' // tbuf(1:nct) // ',' //
     1                   lmin(1)(1:ncmin(1)) // '[,' //
     2                   lmin(2)(1:ncmin(2)) // '[,' //
     3                   lmin(3)(1:ncmin(3)) // ']]]'
          ncb    = ncb    + nct    + ncmin(1) + ncmin(2) + 14
c
          iout   = 1
 7200     if (ncb .gt. 80) then
              do 7250 i=80,1,-1
                  if (bbuf(i:i) .eq. ' ') then
                      obuf   = bbuf(1:i) // '$'
                      nco    = i      + 1
                      call docout (obuf,nco,iout,cmsg,kerr)
                      bbuf1  = bbuf(i+1:ncb)
                      inc4   = ncm    + 4
                      bbuf   = lspac(1:inc4) // bbuf1
                      ncb    = ncb    - i + ncm    + 4
                      iout   = 0
                      go to 7270
                  endif
 7250         continue
          else
              call docout (bbuf,ncb,iout,cmsg,kerr)
              ncb    = 0
          endif
c
 7270     obuf   = ' '
          if (inc1 .ne. 0) then
              obuf(inc1:) = lmin(9)(1:ncmin(9))
              obuf(inc2:) = lmin(10)(1:ncmin(10))
              obuf(inc3:) = lmin(7)(1:ncmin(7))
              nco    = inc3   + ncmin(7) - 1
              inc1   = 0
              call docout (obuf,nco,0,cmsg,kerr)
          endif
c
          obuf   = ' '
          if (inc3 .ne. 0) then
              obuf(inc3:) = lmin(8)(1:ncmin(8))
              nco    = inc3   + ncmin(8) - 1
              inc3   = 0
              call docout (obuf,nco,0,cmsg,kerr)
          endif
c
          if (ncb .gt. 0) then
              call docout (' ',0,0,cmsg,kerr)
              go to 7200
          endif
          if (kerr .ne. 0) go to 8000
      endif
c
c...REWIND
c
      if (REWCOD .ne. 0) then
          call getvwd (1006,obuf,nco,1,PSTWRD,PSTWVL,NPSTWD)
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...ROTABL/ATANGL
c
      if (IRTDEF .gt. 0) then
          do 7500 i=1,IRTDEF,1
              if (IRTYPE(IRTINC(i)) .eq. 1) itab = 1
              if (IRTYPE(IRTINC(i)) .eq. 2) ihed = 1
 7500     continue
          ncm1   = 0
          if (itab .eq. 1) then
              call getvwd (1026,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
              if (ihed .eq. 1)
     1            call getvwd (1035,lmaj1,ncm1,1,PSTWRD,PSTWVL,NPSTWD)
          else
              call getvwd (1035,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          endif
          obuf   = lmaj(1:ncm) // '/'
          nco    = 7
c
          if (IRTNUM .gt. 1) then
              obuf(nco+1:) = ' [' // laxs(4)(1:ncaxs(4)) // ',n,] '
              nco    = nco    + ncaxs(4) + 7
          endif
c
          call getvwd (1,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          ilev(nlev) = 17
          call docprm (tbuf,nct,ilev,nlev)
          obuf(nco+1:) = lmin(1)(1:ncmin(1)) // ',' // tbuf(1:nct)
          nco    = nco    + ncmin(1) + nct + 1
c
          inc1   = nco    + 4
          call getvwd (166,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(nco+1:) = ' [,' // tbuf(1:6) // ']'
          nco    = nco    + 10
c
          inc2   = nco    + 4
          call getvwd (73,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(nco+1:) = ' [,' // tbuf(1:6) // ',f]'
          nco    = nco    + 12
c
          inc3   = nco    + 4
          call getvwd (54,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(nco+1:) = ' [,' // tbuf(1:6) // ']'
          nco    = nco    + 10
c
          inc4   = nco    + 4
          obuf(nco+1:) = ' [,' // lnow(1:6) // ']'
          nco    = nco    + 10
c
          call docout (obuf,nco,1,cmsg,kerr)
c
          obuf   = ' '
          if (ncm1 .ne. 0) obuf = lmaj1(1:6)
          call getvwd (246,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(inc1:) = tbuf(1:nct)
          call getvwd (315,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(inc2:) = tbuf(1:nct)
          call getvwd (68,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(inc3:) = tbuf(1:nct)
          obuf(inc4:) = lnext(1:ncnxt)
          nco    = inc4   + ncnxt  - 1
          call docout (obuf,nco,0,cmsg,kerr)
c
          obuf   = ' '
          call getvwd (60,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(inc1:) = tbuf(1:nct)
          nco    = inc1   + nct    - 1
          call docout (obuf,nco,0,cmsg,kerr)
c
          call getvwd (59,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(inc1:) = tbuf(1:nct)
          nco    = inc1   + nct    - 1
          call docout (obuf,nco,0,cmsg,kerr)
c
          call getvwd (66,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(inc1:) = tbuf(1:nct)
          nco    = inc1   + nct    - 1
          call docout (obuf,nco,0,cmsg,kerr)
c
c...ROTABL/ADJUST
c
          call getvwd (159,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          if (itab .eq. 1) then
              call getvwd (177,lmin(2),ncmin(2),2,PSTWRD,PSTWVL,NPSTWD)
              call getvwd (1002,lmin(3),ncmin(3),2,PSTWRD,PSTWVL,NPSTWD)
          else
              call getvwd (1002,lmin(2),ncmin(2),2,PSTWRD,PSTWVL,NPSTWD)
          endif
          obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ',' //
     1             lmin(2)(1:6) // ',' // lon(1:ncon)
          nco    = ncm    + ncmin(1) + 6 + ncon + 3
          call docout (obuf,nco,1,cmsg,kerr)
c
          if (itab .eq. 1 .and. ihed .eq. 1) then
              obuf   = lmaj1(1:ncm1)
              inc    = ncm    + ncmin(1) + 3
              obuf(inc:) = lmin(3)(1:6)
          else
              obuf   = ' '
          endif
          inc    = nco    - ncon   + 1
          obuf(inc:) = loff(1:ncoff)
          nco    = inc    + ncoff  - 1
          call docout (obuf,nco,0,cmsg,kerr)
c
c...ROTABL/NEXT
c
          obuf   = lmaj(1:ncm) // '/' // lnext(1:ncnxt)
          nco    = ncnxt  + ncm + 1
c
          if (IRTNUM .gt. 1) then
              obuf(nco+1:) = ' [,' // laxs(4)(1:ncaxs(4)) // ',n] '
              nco    = nco    + ncaxs(4) + 7
          endif
c
          call getvwd (60,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          inc1   = nco    + 2
          obuf(nco+1:) = ' ,' // tbuf(1:nct)
          nco    = nco    + nct    + 2
          call docout (obuf,nco,1,cmsg,kerr)
c
          obuf   = ' '
          if (ncm1 .ne. 0) obuf = lmaj1(1:ncm1)
          call getvwd (59,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(inc1+1:) = tbuf(1:nct)
          nco    = inc1   + nct
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c...ROTABL/ON
c
          ifl    = 0
          do 7600 i=1,IRTNUM,1
              if (IRTDUP(i) .gt. 1) ifl = 1
 7600     continue
          if (ifl .eq. 1) then
              obuf   = lmaj(1:6) // '/' // lon(1:ncon)
              nco    = ncon   + 7
              if (IRTNUM .gt. 1) then
                  obuf(nco+1:) = ' [,' // laxs(4)(1:ncaxs(4)) // ',n] '
                  nco    = nco    + ncaxs(4) + 7
              endif

              obuf(nco+1:) = ',m1,...,mn'
              nco    = nco    + 10
              call docout (obuf,nco,1,cmsg,kerr)
c
              if (ncm1 .ne. 0) then
                  obuf   = lmaj1(1:6) // ' '
              else
                  obuf   = '       '
              endif
              obuf(8:) = loff(1:ncoff)
              nco    = ncoff  + 7
              call docout (obuf,nco,0,cmsg,kerr)
c
          endif
c
c...ROTABL/ORIGIN
c
          call getvwd (1027,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:6) // '/' // tbuf(1:nct)
          nco    = nct    + 7
c
          if (IRTNUM .gt. 1) then
              obuf(nco+1:) = ' [,' // laxs(4)(1:ncaxs(4)) // ',n] '
              nco    = nco    + ncaxs(4) + 7
          endif
c
          obuf(nco+1:) = ',x,y,z'
          nco    = nco    + 6
          call docout (obuf,nco,1,cmsg,kerr)
c
          if (ncm1 .ne. 0) then
              obuf   = lmaj1(1:6)
              call docout (obuf,nco,0,cmsg,kerr)
          endif
          if (kerr .ne. 0) go to 8000
c
c...ROTABL/SHORT
c
          if (IRTNUM .gt. 1) then
              call getvwd (851,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              call getvwd (1071,sbuf,ncs,2,PSTWRD,PSTWVL,NPSTWD)
              obuf   = lmaj(1:6) // '/' // tbuf(1:nct) // ',' //
     1                 sbuf(1:6) // ' [,' // lon(1:6) // ']'
              inc1   = nct    + 9
              inc2   = nct    + 9 + 9
              nco    = strlen1(obuf)
              call docout (obuf,nco,1,cmsg,kerr)
c
              obuf   = ' '
              if (ncm1 .ne. 0) obuf = lmaj1(1:6)
              call getvwd (7,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc1:) = tbuf(1:nct)
              obuf(inc2:) = lnext(1:ncnxt)
              nco    = inc2   + ncnxt - 1
              call docout (obuf,nco,0,cmsg,kerr)
c
              obuf   = ' '
              call getvwd (19,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc1:) = tbuf(1:nct)
              nco    = inc1   + nct    - 1
              call docout (obuf,nco,0,cmsg,kerr)
c
              obuf   = ' '
              obuf(nco+1:) = ' [,' // laxs(4)(1:ncaxs(4)) // ',n] '
              obuf(inc1:) = laxs(4)(1:ncaxs(4)) // ',n'
              nco    = inc1   + ncaxs(4) + 1
              call docout (obuf,nco,0,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
      endif
c
c...ROTABL/SIZE
c
          if (RSIZCD(1) .ne. 0) then
              call getvwd (196,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf   = lmaj(1:6) // '/' // tbuf(1:nct)
              nco    = nct    + 7
c
              if (IRTNUM .gt. 1) then
                  obuf(nco+1:) = ' [,' // laxs(4)(1:ncaxs(4)) // ',n] '
                  nco    = nco    + ncaxs(4) + 7
              endif
c
              obuf(nco+1:) = ',c'
              nco    = nco    + 2
              call docout (obuf,nco,1,cmsg,kerr)
c
              if (ncm1 .ne. 0) then
                  obuf   = lmaj1(1:6)
                  call docout (obuf,nco,0,cmsg,kerr)
              endif
              if (kerr .ne. 0) go to 8000
          endif
c
c...SELCTL
c
      if (MACHTP .ne. 2 .and. TOOLFL(1) .eq. 1) then
          call getvwd (1056,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (9,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/[tn    [[,' // tbuf(1:nct) //
     1             '], tl]]'
          nco    = strlen1(obuf)
          inc    = ncm    + 3
c
          inc1   = 0
          if (TOOLFL(2) .eq. 1) then
              call getvwd (7,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              inc1   = nco    + 4
              obuf(nco+1:) = ' [,' // tbuf(1:6) // ']'
              nco    = nco    + 10
          endif
c
          inc2   = 0
          if (TOOLFL(3) .eq. 1) then
              call getvwd (62,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              inc2   = nco    + 4
              obuf(nco+1:) = ' [,' // tbuf(1:6) // ']'
              nco    = nco    + 10
          endif
c
          inc3   = nco    + 4
          obuf(nco+1:) = ' [,' // lmod(1:6) // ']'
          nco    = nco    + 10
c
          call docout (obuf,nco,1,cmsg,kerr)
c
          obuf   = ' '
          obuf(inc:) = lauto(1:ncauto) // ' [,lastn]'
c
          if (inc1 .ne. 0) then
              call getvwd (26,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc1:) = tbuf(1:nct)
          endif
          if (inc2 .ne. 0) then
              call getvwd (63,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc2:) = tbuf(1:nct)
          endif
          obuf(inc3:) = lnow(1:ncnow)
          nco    = inc3   + ncnow  - 1
          call docout (obuf,nco,0,cmsg,kerr)
          obuf   = ' '
          obuf(inc3:) = lnext(1:ncnxt) // ',tm'
          nco    = inc3   + ncnxt  + 2
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...SEQNO
c
      if (SEQCOD .ne. 0) then
          call getvwd (1019,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // lon(1:ncon)
          nco    = ncm    + ncon   + 1
          call docout (obuf,nco,1,cmsg,kerr)
          obuf   = lspac(1:ncm+1) // loff(1:ncoff)
          nco    = ncm    + ncoff  + 1
          call docout (obuf,nco,0,cmsg,kerr)
          call getvwd (66,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lspac(1:ncm+1) // 'n [,' // tbuf(1:nct) // ',i[,m]]'
          nco    = ncm    + nct    + 12
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...prepst SET/LINEAR
c...etc.
c
      call getvwd (1087,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (76,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (95,lmin(2),ncmin(2),2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (51,lmin(3),ncmin(3),2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1071,lmin(4),ncmin(4),2,PSTWRD,PSTWVL,NPSTWD)
      nct    = ncmin(1)
      if (ncmin(2)  .gt. nct) nct = ncmin(2)
      obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1))
      nco    = ncm    + ncmin(1) + 1
      call docout (obuf,nco,1,cmsg,kerr)
      tbuf   = lmin(2)(1:ncmin(2))
      if (nct .gt. ncmin(2)) tbuf(ncmin(2)+1:nct) = ' '
      sbuf   = lspac(1:ncm+1) // tbuf(1:nct) // ' [, tol] [,'
      ncs    = ncm + nct + 12
      inc1   = ncmin(3)
      if (ncmin(4) .gt. inc1) inc1 = ncmin(4)
      tbuf   = lmin(3)
      if (inc1 .gt. ncmin(3)) tbuf(ncmin(3)+1:inc1) = ' '
      tbuf(inc1+1:inc1+1) = ']'
      obuf   = sbuf(1:ncs) // tbuf(1:inc1+1)
      call docout (obuf,ncs+inc1+1,0,cmsg,kerr)
      obuf   = lspac(1:ncs) // lmin(4)(1:ncmin(4))
      call docout (obuf,ncs+ncmin(4),0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...SLOWDN
c
      call getvwd (1063,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lmaj(1:ncm) // '/' // loff(1:ncoff)
      nco    = ncm    + ncoff  + 1
      call docout (obuf,nco,1,cmsg,kerr)
      obuf   = lspac(1:ncm+1) // lon(1:6)
      nco    = ncm    + 7
      if (SLWFL(1) .eq. 1 .and. SLWFL(4) .gt. 1) then
          obuf(nco+1:) = ' [,n]'
          nco    = nco    + 5
      endif
      inc    = nco    + 3
      obuf(nco+1:) = ' [,' // lnext(1:ncnxt) // ']'
      nco    = nco    + ncnxt  + 4
      call docout (obuf,nco,0,cmsg,kerr)
      obuf   = lspac(1:inc) // lauto(1:ncauto)
      nco    = inc    + ncauto
      call docout (obuf,nco,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...SLOWDN/RAMP
c
      if (IACCFL(1) .eq. 1) then
          call getvwd (1063,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (854,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (1078,lmin(2),ncmin(2),2,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (92,lmin(3),ncmin(3),2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ',' //
     1             loff(1:ncoff)
          nco    = ncm    + ncmin(1) + ncoff  + 2
          call docout (obuf,nco,1,cmsg,kerr)
          obuf   = lspac(1:ncm+ncmin(1)+2) // lon(1:ncon) // ' [,' //
     1             lmin(2)(1:ncmin(2)) // ',maxvel,axsvel] [,' //
     2             lmin(3)(1:ncmin(3)) // ',min,max]'
          nco    = strlen1(obuf)
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...SMOOTH/ON
c
      if (MACHTP .eq. 3) then
          call getvwd (1085,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          obuf  = lmaj(1:ncm) // '/' // loff(1:ncoff)
          nco   = ncm    + ncoff + 1
          call docout (obuf,nco,1,cmsg,kerr)
c
          call getvwd (92,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
          lmin(3) = ' [,' // lmin(1)(1:ncmin(1)) // ',max [,npts]]'
          ncmin(3) = strlen1(lmin(3))
c
          obuf(1:ncm+1) = lspac(1:ncm+1)
          obuf(ncm+2:)  = lon(1:ncon) // lmin(3)(1:ncmin(3))
          nco    = ncm    + ncon   + ncmin(3) + 1
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c...SMOOTH/ATANGL,
c
          call getvwd (1,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf  = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     -            loff(1:ncoff)
          inc1  = ncm    + nct + 2
          nco   = inc1   + ncoff
          call docout (obuf,nco,1,cmsg,kerr)
c
          obuf(1:inc1) = lspac(1:1)
          call rtoc (SMORAN,lmin(1),ncmin(1))
          call rtoc (SMODIS,lmin(2),ncmin(2))
          obuf(inc1+1:)  = '[' // lmin(1)(1:ncmin(1)) // ',' //
     -                            lmin(2)(1:ncmin(2)) // ']' //
     2                            lmin(3)(1:ncmin(3))
          nco    = inc1   + ncmin(1) + ncmin(2) + ncmin(3) + 3
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c
c...SPINDL/ON
c
      call getvwd (1031,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lmaj(1:ncm) // '/' // lon(1:ncon)
      nco    = ncm    + ncon   + 1
      call docout (obuf,nco,1,cmsg,kerr)
      obuf   = lspac(1:ncm+1) // loff(1:ncoff)
      nco    = ncm    + ncoff  + 1
      call docout (obuf,nco,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (SPNDCD(4) .ne. 0) then
          call getvwd (246,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lspac(1:ncm+1) // tbuf(1:nct)
          nco    = ncm    + nct    + 1
          call docout (obuf,nco,0,cmsg,kerr)
      endif
      if (kerr .ne. 0) go to 8000
c
c...SPINDL/LOCK
c
      if (SPNOCD(1) .ne. 0 .or. SPNOCD(2) .ne. 0) then
          call getvwd (114,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',' //
     1             lon(1:ncon)
          nco    = ncm    + nct    + ncon   + 2
          call docout (obuf,nco,1,cmsg,kerr)
          inc    = ncm    + nct    + 2
          obuf   = lspac(1:inc) // loff(1:ncoff)
          nco    = inc    + ncoff
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...SPINDL/MAXRPM
c
      call getvwd (79,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ',n'
      nco    = ncm    + nct    + 3
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...SPINDL/RPM
c
      call getvwd (78,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (60,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lmaj(1:ncm) // '/ [' // tbuf(1:6) // '] [,n] [,' //
     1         lmin(1)(1:6) // ']'
      inc1   = ncm    + 4
      inc2   = ncm    + 19
      nco    = strlen1(obuf)
c
      inc3   = 0
      if (NSPRG .gt. 1) then
          inc3   = nco    + 4
          call getvwd (63,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(nco+1:) = ' [,' // tbuf(1:6) // ']'
          nco    = nco    + 10
      endif
c
      inc4   = nco    + 4
      call getvwd (23,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      obuf(nco+1:) = ' [,' // tbuf(1:6) // ',r]'
      nco    = nco    + 12
c
      call docout (obuf,nco,1,cmsg,kerr)
c
      obuf   = ' '
      call getvwd (115,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      obuf(inc1:) = tbuf(1:nct)
      call getvwd (59,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      obuf(inc2:) = tbuf(1:nct)
      if (inc3 .ne. 0) then
          if (NSPRG .eq. 2) then
              call getvwd (62,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc3:) = tbuf(1:nct)
              inc3   = 0
          else
              call getvwd (61,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc3:) = tbuf(1:nct)
          endif
      endif
      call getvwd (205,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      obuf(inc4:) = tbuf(1:nct)
      nco    = inc4   + nct    - 1
      call docout (obuf,nco,0,cmsg,kerr)
c
      obuf   = ' '
      nco    = 0
      if (SPNBCD .ne. 0) then
          call getvwd (83,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(inc2:) = tbuf(1:nct)
          nco    = inc2   + nct    - 1
      endif
c
      if (inc3 .ne. 0) then
          call getvwd (62,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(inc3:) = tbuf(1:nct)
          nco    = inc3   + nct    - 1
          call docout (obuf,nco,0,cmsg,kerr)
c
          obuf(inc3:) = lauto(1:ncauto)
          nco    = inc1   + ncauto - 1
      endif
      if (nco .ne. 0) call docout (obuf,nco,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...STOP
c
      if (STOPCD .ne. 0) then
          call getvwd (2,obuf,nco,1,PSTWRD,PSTWVL,NPSTWD)
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...THREAD
c
      if (MTPDYN .eq. 2 .and. (CYLCOD(1) .ne. 0 .or. CYLCOD(2) .ne. 0
     1    .or. CYLCOD(3) .ne. 0)) then
          call getvwd (1036,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/'
          nco    = ncm    + 1
          ilev(nlev) = 13
          call docprm (tbuf,nct,ilev,nlev)
c
          if (CYLREG(1) .ne. 0) then
              obuf(nco+1:) = tbuf(1:nct) // 'k'
              nco    = nco    + nct    + 1
          endif
c
          if (CYLREG(2) .ne. 0) then
              obuf(nco+1:) = ' [,' // tbuf(1:nct) // 'i]'
              nco    = nco    + nct    + 5
          endif
c
          inc1   = nco    + 4
          obuf(nco+1:) = ' [,' // laxs(3)(1:6) // ',z]'
          nco    = nco    + 12
c
          inc2   = nco    + 4
          obuf(nco+1:) = ' [,' // laxs(1)(1:6) // ',z]'
          nco    = nco    + 12
c
          if (CYLCOD(2) .ne. 0 .or. CYLREG(3) .ne. 0) then
              inc3   = nco    + 4
              call getvwd (66,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(nco+1:) = ' [,' // tbuf(1:6) // ',k]'
              nco    = nco    + 12
          endif
c
          obuf(nco+1:) = ' [,' // lauto(1:ncauto) // ']'
          nco    = nco    + ncauto + 4
c
          call docout (obuf,nco,1,cmsg,kerr)
          obuf   = ' '
c
          call getvwd (118,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(inc1:) = tbuf(1:nct)
          call getvwd (116,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf(inc2:) = tbuf(1:nct)
          nco    = inc2   + nct    - 1
c
          if (inc3 .ne. 0) then
              call getvwd (65,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc3:) = tbuf(1:nct)
              nco    = inc3   + nct    - 1
          endif
c
          call docout (obuf,nco,0,cmsg,kerr)
c
c...THREAD/OFF
c
          obuf   = lmaj(1:ncm) // '/' // loff(1:ncoff)
          nco    = ncm    + ncoff  + 1
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...TMARK/n
c
      call getvwd (1005,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      if (ALNCOD .ne. 0 .or. AL1BLK .ne. 0 .or. AL2BLK .ne. 0) then
          obuf   = lmaj(1:ncm) // ' [/] [n] [,] [' // lnow(1:ncnow) //
     1             ']'
          nco    = strlen1(obuf)
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...TMARK/AUTO
c
      if (NRWSTP .ne. 0) then
          obuf   = lmaj(1:ncm) // '/' // lauto(1:ncauto)
          nco    = ncm    + ncauto + 1
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif

c
c...TOOLNO/ADJUST,ON
c
      call getvwd (1025,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (159,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
      if (TLOCD(2) .ne. 0 .or. TLOCD(3) .ne. 0) then
          obuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ',' //
     1             lon(1:ncon)
          nco    = ncm    + ncmin(1) + ncon   + 2
          call docout (obuf,nco,1,cmsg,kerr)
          inc    = ncm    + ncmin(1) + 2
          obuf   = lspac(1:inc) // loff(1:ncoff)
          nco    = inc    + ncoff
          call docout (obuf,nco,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...TOOLNO/ADJUST,n
c
      if (TLOCD(1) .ne. 0 .or. TLOCD(2) .ne. 0 .or.
     1    TLOCD(3) .ne. 0 .or. TLOCD(4) .ne. 0 .or.
     2    TLOCD(5) .ne. 0) then
          bbuf   = lmaj(1:ncm) // '/' // lmin(1)(1:ncmin(1)) // ',n'
          ncb    = ncm    + ncmin(1) + 3
c
          if (CUTCCD(4) .ne. 0) then
              bbuf(ncb+1:) = ' [,d]'
              ncb    = ncb    + 5
          endif
c
          inc1   = 0
          if (TLOCD(4) .ne. 0 .or. CUTCCD(5) .ne. 0 .or.
     1        TLOSGN .eq. 1) then
              call getvwd (19,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              bbuf(ncb+1:) = ' [,' // tbuf(1:6) // ']'
              inc1   = ncb    + 4
              ncb    = ncb    + 10
          endif
          ilev(nlev) = 3
          call docprm (lmin(2),ncmin(2),ilev,nlev)
          do 9000 i=1,3,1
              if (NUMLIN(i) .ne. 0 .and. TLOCD(5+i) .ne. 0) then
                  tbuf   = ' [,' // laxs(i)(1:ncaxs(i)) // '[,' //
     1                     lmin(2)(1:ncmin(2)) // ']]'
                  nct    = strlen1(tbuf)
                  bbuf(ncb+1:) = tbuf(1:nct)
                  ncb    = ncb    + nct
              endif
 9000     continue
c
          inc2   = 0
          if (MACHTP .ne. 2) then
              call getvwd (33,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              bbuf(ncb+1:) = ' [,' // tbuf(1:6) // ']'
              inc2   = ncb    + 4
              ncb    = ncb    + 10
          endif
c
          inc3    = ncb    + 4
          bbuf(ncb+1:) = ' [,' // lnow(1:6) // ']'
          ncb    = ncb    + 10
c
          iout   = 1
 9100     ifl    = 0
          ifl1   = 0
          if (ncb .gt. 80) then
              do 9200 i=80,1,-1
                  if (bbuf(i:i) .eq. ' ') then
                      obuf   = bbuf(1:i) // '$'
                      nco    = i      + 1
                      call docout (obuf,nco,iout,cmsg,kerr)
                      bbuf1  = bbuf(i+1:ncb)
                      inc    = ncm    + 4
                      bbuf   = lspac(1:inc) // bbuf1
                      ncb    = ncb    - i + ncm    + 4
                      if (inc2 .gt. i) then
                          inc2   = inc2   - nco    + ncm    + 5
                          ifl    = 1
                      endif
                      if (inc3 .gt. i) then
                          inc3   = inc3   - nco    + ncm    + 5
                          ifl1   = 1
                      endif
                      iout   = 0
                      go to 9250
                  endif
 9200         continue
          else
              call docout (bbuf,ncb,iout,cmsg,kerr)
              ncb    = 0
          endif
c
 9250     obuf   = ' '
          nco    = 0
          if (inc1 .ne. 0) then
              call getvwd (10,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc1:) = tbuf(1:nct)
              nco    = inc1   + nct    - 1
              inc1   = 0
          endif
c
          inc4   = 0
          if (inc2 .ne. 0 .and. ifl .eq. 0) then
              call getvwd (37,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc2:) = tbuf(1:nct)
              nco    = inc2   + nct    - 1
              inc4   = inc2
              inc2   = 0
          endif
c
          if (inc3 .ne. 0 .and. ifl1 .eq. 0) then
              obuf(inc3:) = lnext(1:ncnxt)
              nco    = inc3   + ncnxt  - 1
              inc3   = 0
          endif
c
          if (nco .ne. 0) then
              call docout (obuf,nco,0,cmsg,kerr)
          endif
c
          if (inc4 .ne. 0) then
              call getvwd (41,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf   = ' '
              obuf(inc4:) = tbuf(1:nct)
              nco    = inc4   + nct    - 1
              call docout (obuf,nco,0,cmsg,kerr)
          endif
          if (ncb .gt. 0) then
              call docout (' ',0,0,cmsg,kerr)
              go to 9100
          endif
          if (kerr .ne. 0) go to 8000
      endif
c
c...TRANS/x,y,z
c
      call getvwd (1037,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (25,lmin(1),ncmin(1),2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (52,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lmaj(1:ncm) // '/ [' // lmin(1)(1:ncmin(1)) //
     1          ',] n     [,' // tbuf(1:nct) // ']'
      nco    = strlen1(obuf)
      call docout (obuf,nco,1,cmsg,kerr)
      inc    = ncm    + ncmin(1) + 6
      if (MACHTP .eq. 2) then
          obuf   = lspac(1:inc) // 'z,x'
          nco    = inc    + 3
      else
          obuf   = lspac(1:inc) // 'x,y,z'
          nco    = inc    + 5
      endif
      call docout (obuf,nco,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...TRANS/mx
c
      obuf   = lmaj(1:ncm) // '/i1,j1,k1,d1, i2,j2,k2,d2, i3,j3,k3,d3'
      nco    = strlen1(obuf)
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...TRANS/TOOL,VECTOR,i,j,k
c
      call getvwd (617,lmin(2),ncmin(2),2,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (604,lmin(3),ncmin(3),2,PSTWRD,PSTWVL,NPSTWD)
      obuf   = lmaj(1:ncm) // '/ [' // lmin(2)(1:ncmin(2)) //
     1         '] [,] [' // lmin(3)(1:ncmin(3)) // ' [,i,j,k]]'
      nco    = strlen1(obuf)
      call docout (obuf,nco,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...TRANS/AXIS
c
      obuf   = lmaj(1:ncm) // '/ [' // lmin(1)(1:ncmin(1)) // ',]'
      nco    = ncm    + ncmin(1) + 5
      ilev(nlev) = 18
      call docprm (lmin(2),ncmin(2),ilev,nlev)
c
      iout   = 1
      do 9600 i=1,3,1
          if (NUMLIN(i) .ne. 0) then
              tbuf   = ' [,' // laxs(i)
              nct    = ncaxs(i) + 3
              if (NUMLIN(i) .eq. 2) then
                  tbuf(nct+1:) = '[,n]'
                  nct    = nct    + 4
              endif
              tbuf(nct+1:) = ',' // lmin(2)(1:ncmin(2)) //
     1                 ']'
              nct    = strlen1(tbuf)
              if (nco+nct .gt. 78) then
                  obuf(nco+1:) = ' $'
                  call docout (obuf,nco+2,iout,cmsg,kerr)
                  nco    = ncm    + 4
                  obuf   = lspac(1:nco)
                  iout   = 0
              endif
              obuf(nco+1:) = tbuf(1:nct)
              nco    = nco    + nct
          endif
 9600 continue
c
      if (IRTNUM .ne. 0) then
          tbuf   = ' [,' // laxs(4)
          nct    = ncaxs(4) + 3
          if (IRTNUM .gt. 1) then
              tbuf(nct+1:) = '[,n]'
              nct    = nct    + 4
          endif
          tbuf(nct+1:) = ',' // lmin(2)(1:ncmin(2)) //
     1             ']'
          nct    = strlen1(tbuf)
          if (nco+nct .gt. 78) then
              obuf(nco+1:) = ' $'
              call docout (obuf,nco+2,iout,cmsg,kerr)
              nco    = ncm    + 4
              obuf   = lspac(1:nco)
              iout   = 0
          endif
          obuf(nco+1:) = tbuf(1:nct)
          nco    = nco    + nct
      endif
c
      call docout (obuf,nco,iout,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...TURRET
c
      if (MTPDYN .eq. 2) then
          call getvwd (1033,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (23,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          bbuf   = lmaj(1:ncm) // '/[tn [[,' // tbuf(1:nct) // '], tr]]'
          ncb    = strlen1(bbuf)
c
          if (TLOCD(1) .ne. 0 .or. CUTCCD(4) .ne. 0) then
              call getvwd (705,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              bbuf(ncb+1:) = ' [,' // tbuf(1:nct) // ',h'
              ncb    = ncb    + nct    + 5
              if (CUTCCD(4) .ne. 0) then
                  bbuf(ncb+1:) = '[,d]'
                  ncb    = ncb    + 4
              endif
              ncb    = ncb    + 1
              bbuf(ncb:ncb) = ']'
          endif
c
          call getvwd (159,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          bbuf(ncb+1:) = ' [,' // tbuf(1:nct) // ',z,x]'
          ncb    = ncb    + nct    + 8
c
          inc1   = 0
          if (TURFL(1) .gt. 1) then
              if (TURFL(2) .eq. 1) then
                  call getvwd (148,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
                  call getvwd (149,lmin(5),ncmin(5),2,PSTWRD,PSTWVL,
     1                         NPSTWD)
              else
                  call getvwd (149,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
                  call getvwd (148,lmin(5),ncmin(5),2,PSTWRD,PSTWVL,
     1                         NPSTWD)
              endif
              inc1   = ncb    + 4
              bbuf(ncb+1:) = ' [,' // tbuf(1:6) // ']'
              ncb    = ncb    + 10
          endif
c
          call getvwd (60,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          inc2   = ncb    + 4
          bbuf(ncb+1:) = ' [,' // tbuf(1:6) // ']'
          ncb    = ncb    + 10
c
          inc3   = ncb    + 4
          bbuf(ncb+1:) = ' [,' // lauto(1:6) // ']'
          ncb    = ncb    + 10
c
          iout   = 1
 9800     ifl    = 0
          ifl1   = 0
          if (ncb .gt. 80) then
              do 9900 i=80,1,-1
                  if (bbuf(i:i) .eq. ' ') then
                      obuf   = bbuf(1:i) // '$'
                      nco    = i      + 1
                      call docout (obuf,nco,iout,cmsg,kerr)
                      bbuf1  = bbuf(i+1:ncb)
                      inc    = ncm    + 4
                      bbuf   = lspac(1:inc) // bbuf1
                      ncb    = ncb    - i + ncm    + 4
                      if (inc2 .gt. i) then
                          inc2   = inc2   - nco    + ncm    + 5
                          ifl    = 1
                      endif
                      if (inc3 .gt. i) then
                          inc3   = inc3   - nco    + ncm    + 5
                          ifl1   = 1
                      endif
                      iout   = 0
                      go to 9950
                  endif
 9900         continue
          else
              call docout (bbuf,ncb,iout,cmsg,kerr)
              ncb    = 0
          endif
c
 9950     obuf   = ' '
          nco    = 0
          if (inc1 .ne. 0) then
              obuf(inc1:) = lmin(5)(1:ncmin(5))
              nco    = inc1   + ncmin(5) - 1
              inc1   = 0
          endif
c
          if (inc2 .ne. 0 .and. ifl .eq. 0) then
              call getvwd (59,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf(inc2:) = tbuf(1:nct)
              nco    = inc2   + nct    - 1
              inc2   = 0
          endif
c
          inc4   = 0
          if (inc3 .ne. 0 .and. ifl1 .eq. 0) then
              obuf(inc3:) = lmod(1:ncmod)
              nco    = inc3   + ncmod  - 1
              inc4   = inc3
              inc3   = 0
          endif
c
          if (nco .ne. 0) then
              call docout (obuf,nco,0,cmsg,kerr)
          endif
c
          if (inc4 .ne. 0) then
              call getvwd (158,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
              obuf   = ' '
              obuf(inc4:) = tbuf(1:nct)
              nco    = inc4   + nct    - 1
              call docout (obuf,nco,0,cmsg,kerr)
          endif
          if (ncb .gt. 0) then
              call docout (' ',0,0,cmsg,kerr)
              go to 9800
          endif
          if (kerr .ne. 0) go to 8000
      endif
c
c...UNLOAD
c
      if (MACHTP .ne. 2 .and. TOOLFL(1) .eq. 1 .and.
     1    (TLCCD(7) .ne. 0 .or. TLCCD(8) .ne. 0 .or.
     2    TLCCD(9) .ne. 0 .or. TLCCD(10) .ne. 0)) then
          call getvwd (10,lmaj,ncm,1,PSTWRD,PSTWVL,NPSTWD)
          call getvwd (617,tbuf,nct,2,PSTWRD,PSTWVL,NPSTWD)
          obuf   = lmaj(1:ncm) // '/' // tbuf(1:nct) // ' [,tn]'
          nco    = ncm    + nct    + 7
          call docout (obuf,nco,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
      call docclr (cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c***********************************************************************
c
c   SUBROUTINE:  doc_cyl1 (cmsg,knum)
c
c   FUNCTION:  This routine formats the cylindrical interpolation plane
c              definition.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Plane definition string.
c
c           knum    I*4  D1  -  Number of characters in cmsg.
c
c***********************************************************************

      subroutine doc_cyl1 (cmsg,knum,cmsg1,knum1)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      character*80 cmsg,cmsg1
      integer*4 knum,knum1
c
      equivalence (IRTNUM,KPOSMP(1243)), (IRTWRK,KPOSMP(1506))
      equivalence (NCPLAN,KPOSMP(4226))
c
      integer*4 NCPLAN(3),IRTNUM,IRTWRK(20)
c
      character*80 bufa,bufb,lspac
      integer*4 na,nb,nax,i,nas,jindex,iax(4)
c
      data iax /2,3,1,2/
      data lspac  /' '/
c
c...get table rotary axis
c
      nax    = NCPLAN(2)
      i      = jindex(IRTWRK,nax,IRTNUM)
      nax    = IRTWRK(i)
      call getvwd (132,bufa,na,2,PSTWRD,PSTWVL,NPSTWD)
      call itoc (i,bufb,nb,0)
      bufa(na+1:) = ',' // bufb(1:nb) // ','
      na     = na + nb + 2
      nas    = na
c
c...get first axis of plan
c
      i      = iax(nax) + 83
      call getvwd (i,bufa(na+1:),nb,2,PSTWRD,PSTWVL,NPSTWD)
      na     = na + nb
      cmsg   = bufa(1:na)
      knum   = na
c
c...get second axis of plan
c
      i      = iax(nax+1) + 83
      call getvwd (i,bufa,na,2,PSTWRD,PSTWVL,NPSTWD)
      cmsg1  = lspac(1:nas) // bufa(1:na)
      knum1  = na + nas
c
      return
      end

