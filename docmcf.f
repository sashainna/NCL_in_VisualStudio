c
c***********************************************************************
c
c   FILE NAME:  docmcf.for
c   CONTAINS:
c               docmcf  docpos
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        docmcf.f , 26.2
c     DATE AND TIME OF LAST  MODIFICATION
c        07/23/18 , 13:45:59
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  docmcf (cmsg,kerr)
c
c   FUNCTION:  This routine outputs the automatic documentation Machine
c              Configuration pages.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine docmcf (cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087)), (IDUMMY,KPOSMP(0088))
      equivalence (MOTREG,KPOSMP(0381)), (NEOB  ,KPOSMP(0841))
      equivalence (IJKREG,KPOSMP(0827)), (SGMREG,KPOSMP(0951))
      equivalence (MACHTP,KPOSMP(1201)), (NUMLIN,KPOSMP(1202))
      equivalence (LNCALC,KPOSMP(1208)), (IZSWIV,KPOSMP(1224))
      equivalence (LTHXY ,KPOSMP(1225)), (LTHDIA,KPOSMP(1228))
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (IJKROT,KPOSMP(1739)), (NOPIVS,KPOSMP(1282))
      equivalence (NOTABS,KPOSMP(1283)), (IRTMOD,KPOSMP(1284))
      equivalence (IRTSCL,KPOSMP(1288)), (IRTCLW,KPOSMP(1292))
      equivalence (MROTTV,KPOSMP(1334)), (LTHXD ,KPOSMP(1339))
      equivalence (NCONTB,KPOSMP(1347)), (ISCWRK,KPOSMP(1453))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTYPE,KPOSMP(1486))
      equivalence (IRTWRK,KPOSMP(1506)), (IACCFL,KPOSMP(1745))
      equivalence (TURFL ,KPOSMP(1824)), (LTHPCL,KPOSMP(1899))
      equivalence (NSPRG ,KPOSMP(3101)), (SPNSFM,KPOSMP(3103))
      equivalence (NUMINF,KPOSMP(1396)), (POSFDF,KPOSMP(3208))
      equivalence (EXCLAX,KPOSMP(3401)), (EXCLCO,KPOSMP(3417))
      equivalence (EXCLPS,KPOSMP(3421)), (EXCLNE,KPOSMP(3469))
      equivalence (NUMEXC,KPOSMP(3517)), (EXCLNM,KPOSMP(3518))
      equivalence (MTPDYN,KPOSMP(4126))
c
      integer*4 IUNIT,MACHTP,MOTREG(24),NUMLIN(3),LNCALC(3),IZSWIV,
     1          IRTNUM,IRTYPE(20),IRTWRK(20),IRTMOD(4),IRTSCL(4),
     2          IRTCLW(4),POSFDF,IRDEAD(20),NUMINF,NOPIVS,NOTABS,
     3          MROTTV,LTHXY,LTHDIA(2),IDUMMY,EXCLAX(4,4),EXCLCO(4),
     4          EXCLPS(12,4),EXCLNE(12,4),NUMEXC,EXCLNM(4),NEOB,NSPRG,
     5          SPNSFM,TURFL(2),LTHXD,NCONTB,MTPDYN,LTHPCL(2),IJKROT,
     6          IJKREG(3),IACCFL(2),ISCWRK(2,4),SGMREG(6,3)
c
      equivalence (DUMMY ,POSMAP(0003)), (INFZON,POSMAP(0071))
      equivalence (HOME  ,POSMAP(0151))
      equivalence (LNDIST,POSMAP(1251)), (LIMITS,POSMAP(1254))
      equivalence (PPTOLR,POSMAP(1274)), (TRANAX,POSMAP(1320))
c....      equivalence (TABORG,POSMAP(1375)), (AXSSTO,POSMAP(1425))
      equivalence (AXSSTO,POSMAP(1425))
      equivalence (ROTCRM,POSMAP(1439)), (PPINCR,POSMAP(1443))
      equivalence (PPMAXD,POSMAP(1584)), (SCROT ,POSMAP(2119))
      equivalence (SPNLMT,POSMAP(3309)), (FEDLMT,POSMAP(3528))
      equivalence (RAPLMT,POSMAP(3567)), (SPIVEC,POSMAP(3583))
      equivalence (BLDVEC,POSMAP(3589)), (BLDELT,POSMAP(3593))
      equivalence (TURDIS,POSMAP(3961)), (MAXVEL,POSMAP(4293))
      equivalence (AXSVEL,POSMAP(4294)), (ACCSTP,POSMAP(4295))
      equivalence (SPMLMT,POSMAP(4930)), (SGMLMT,POSMAP(5137))
      equivalence (TABORG,POSMAP(5374))
c
      real*8 LNDIST(3),TABORG(3,20),ROTCRM(4),SPIVEC(3),LIMITS(2,10),
     1       DUMMY,INFZON(2,10,4),HOME(10),BLDVEC(3),BLDELT,AXSSTO(10),
     2       TRANAX(20),PPTOLR(10),PPINCR(10),PPMAXD(10),FEDLMT(10),
     3       RAPLMT(10),SPNLMT(2,4),TURDIS,SPMLMT(2,3),MAXVEL,AXSVEL,
     4       ACCSTP(2),SCROT(2,4),SGMLMT(2,6,3)
c
      equivalence (LEOB  ,CPOSMP(0971))
c
      character*5 LEOB
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ilev(10),nlev,nc,nc1,nco,ireg,imr(6),i,ist,inc,njax(3),
     1          ncs,ncs1,ncs2,nct,nct1,nct2,strlen1,ifl,ima(4),iprev,
     2          ncax(10),j,iout,isw(6),itab,ipiv,k,jindex,imx(10),n
c
      real*8 rnum,rsub(2)
c
      character*8 mlab(5),munit(2),axis(6),lsec(2),lrt(3),lra(2),
     1            lcon(2),lsca(2),lspn(3),lsup(2)
      character*24 lax(10),jax(3)
      character*1 laxs(3)
      character*5 cijk(3)
      character*80 obuf,sbuf,s1buf,s2buf,tbuf,t1buf,t2buf
c
      data imr /1,5,9, 3,7,11/, ima /14,17,20,23/
      data imx /1,4,2,5,3,6,7,8,9,10/
      data mlab /'MILL','LATHE','BLADE','MILLTURN','STRINGER'/
      data munit /'INCH','MM'/
      data axis /'XAXIS','YAXIS','ZAXIS','ROTAXS1','ROTAXS2','ROTAXS3'/
      data lsec /'SEPARAT','CONNECT'/
      data lrt /'STAND','CARRIER','RIDER'/, lra /'TABLE','HEAD'/
      data lcon /'CONTOUR','POSITN'/, lsca /'LINEAR','ROTARY'/
      data lspn /'LOW','MEDIUM','HIGH'/
      data lsup /'SUPPORT','NOTSUPRT'/
      data cijk /'IAXIS','JAXIS','KAXIS'/
      data laxs /'X','Y','Z'/
c
c...Initialize routine
c
      DPAG   = 0
      DLIN   = 10000
      NDHED  = 6
c
c...Get Header line
c
      ilev(1) = 1
      nlev   = 1
      call docprm (DHED(1),NCHED(1),ilev,nlev)
      call dcenhd (DHED(1),NCHED(1),DHED(1),NCHED(1))
      nlev   = 2
c
c...Machine Type
c
      ilev(2) = 1
      call docprm (sbuf,nc,ilev,nlev)
      call docsap (mlab(MACHTP),tbuf,nc1)
      obuf   = sbuf(1:nc) // ' ' // tbuf
      nc     = nc     + nc1    + 1
      call docout (obuf,nc,1,cmsg,kerr)
c
c...Units
c
      ilev(2) = 2
      call docprm (sbuf,nc,ilev,nlev)
      call docsap (munit(IUNIT),tbuf,nc1)
      obuf   = sbuf(1:nc) // ' ' // tbuf
      nc     = nc     + nc1    + 1
      call docout (obuf,nc,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Axes description
c
      ilev(nlev) = 3
      call docprm (sbuf,nc,ilev,nlev)
      call docout (sbuf,nc,1,cmsg,kerr)
      nlev   = 3
c
c......Linear axes
c
      ilev(nlev) = 1
      call docprm (sbuf,ncs,ilev,nlev)
      call docout (' ',0,0,cmsg,kerr)
      ifl    = 0
      do 590 i=1,10,1
          lax(i) = ' '
          ncax(i) = 0
  590 continue
      do 600 i=1,3,1
c
c.........Primary axes
c
          if (NUMLIN(i) .ne. 0) then
              ireg   = MOTREG(imr(i))
              ilev(nlev) = 1
              call docrgf (ireg,0.d0,lax(i),ncax(i),1)
              call docsap (axis(i),s2buf,ncs2)
              obuf   = '   ' // lax(i)(1:ncax(i)) // ' - ' //
     1                          sbuf(1:ncs)
              call errstr (obuf,s2buf,1)
              nco    = strlen1(obuf)
c
c.........Secondary axes
c
              if (NUMLIN(i) .eq. 2) then
                  ifl    = 1
                  ilev(nlev) = 2
                  call docprm (s1buf,ncs1,ilev,nlev)
                  ireg   = MOTREG(imr(i+3))
                  call docrgf (ireg,0.d0,lax(i+3),ncax(i+3),1)
                  inc    = LNCALC(i)
                  if (inc .gt. 2) inc = 1
                  call docsap (lsec(inc),t2buf,nct2)
                  call errstr (s1buf,t2buf,0)
                  call errstr (s1buf,s2buf,1)
                  t1buf   = lax(i+3)(1:ncax(i+3)) // ' - ' // s1buf
                  ist    = 36
                  if (nco .gt. ist) ist = nco + 5
                  if (ist .lt. 80) then
                      obuf(ist:) = t1buf
                      nco    = strlen1(obuf)
                  endif
              endif
              call docout (obuf,nco,0,cmsg,kerr)
          endif
  600 continue
c
c.........Z-axis moves with rotary head
c
      iout   = 1
      if (IZSWIV .eq. 1) then
          ilev(nlev) = 3
          call docprm (sbuf,ncs,ilev,nlev)
          obuf   = '   ' // sbuf
          nco    = ncs    + 3
          call docout (obuf,nco,iout,cmsg,kerr)
          iout   = 0
      endif
c
c.........Secondary axes distances
c
      if (ifl .eq. 1) then
          ilev(nlev) = 6
          call docprm (sbuf,ncs,ilev,nlev)
          do 700 i=1,3,1
              if (NUMLIN(i) .eq. 2 .and. LNCALC(i) .eq. 1) then
                  obuf  = '   ' // sbuf
                  call errstr (obuf,lax(i+3),0)
                  call rtoc (LNDIST(i),tbuf,nct)
                  call errstr (obuf,tbuf,1)
                  nco    = strlen1(obuf)
                  call docout (obuf,nco,iout,cmsg,kerr)
                  iout   = 0
              endif
  700     continue
      endif
c
c.........Distance between turrets
c
      if (MTPDYN .eq. 2 .and. TURFL(1) .eq. 2) then
          ilev(nlev) = 9
          call docprm (sbuf,ncs,ilev,nlev)
          call rtoc (TURDIS,tbuf,nct)
          obuf  = '   ' // sbuf(1:ncs) // ' ' // tbuf(1:nct)
          nco    = ncs    + nct    + 4
          call docout (obuf,nco,iout,cmsg,kerr)
          iout   = 0
      endif
c
c......Rotary axes
c
      ilev(nlev) = 5
      iprev   = 0
      do 1100 i=1,IRTNUM,1
          ireg   = MOTREG(ima(i))
          call docrgf (ireg,0.d0,lax(i+6),ncax(i+6),1)
          if (IJKROT .eq. 1) then
             lax(i+6) = '*'
             ncax(i+6) = 1
          end if
          call docprm (sbuf,ncs,ilev,nlev)
          obuf   = '   ' // lax(i+6)(1:ncax(i+6)) // ' - ' // sbuf
          inc    = 1
c
c.........Table
c
          if (IRTYPE(i) .eq. 1) then
              if (IRTNUM .lt. 2 .or. IRTYPE(2) .ne. 1 .or.
     1            NCONTB .ne. 2) then
                  if (iprev .eq. 1) then
                      inc    = 2
                  else if (i .lt. IRTNUM .and. IRTYPE(i+1) .eq. 1) then
                      inc    = 3
                  endif
              endif
c
c.........Head
c
          else
              if (i .eq. IRTNUM .and. iprev .eq. 2) then
                  inc    = 3
              else if (i .lt. IRTNUM .and. IRTYPE(i+1) .eq. 2) then
                  inc    = 2
              endif
          endif
c
c.........Output Rotary axis type
c
          iprev  = IRTYPE(i)
          call docsap (lrt(inc),t1buf,nct1)
          call errstr (obuf,t1buf,0)
          call docsap (lra(IRTYPE(i)),t1buf,nct1)
          call errstr (obuf,t1buf,1)
          nco    = strlen1(obuf)
          call docout (obuf,nco,1,cmsg,kerr)
c
c.........Rotation vector
c
          nlev   = nlev   + 1
          ilev(nlev) = 1
          call docprm (sbuf,ncs,ilev,nlev)
          if (IRTWRK(i) .eq. 1) then
              tbuf   = '1,0,0'
          else if (IRTWRK(i) .eq. 2) then
              tbuf   = '0,1,0'
          else
              tbuf   = '0,0,1'
          endif
          nct     = 5
          obuf   = '          ' // sbuf(1:ncs) // ' ' // tbuf(1:nct)
          nco    = ncs    + nct    + 11
          call docout (obuf,nco,0,cmsg,kerr)
c
c........Center of rotation
c
          ilev(nlev) = IRTYPE(i) + 1
          call docprm (sbuf,ncs,ilev,nlev)
          call getr8s (TABORG(1,i),3,tbuf,nct)
          obuf   = '          ' // sbuf(1:ncs) // ' ' // tbuf(1:nct)
          nco    = ncs    + nct    + 11
          call docout (obuf,nco,0,cmsg,kerr)
c
c.........Initial orientation
c
          if (ISCWRK(1,i) .ne. 0) then
              ilev(nlev) = 9
              call docprm (sbuf,ncs,ilev,nlev)
              call getr8s (SCROT(1,i),1,tbuf,nct)
              sbuf(ncs+2:) = laxs(ISCWRK(1,i)) // tbuf(1:nct)
              ncs    = ncs    + nct    + 2
              if (ISCWRK(2,i) .ne. 0) then
                  call getr8s (SCROT(2,i),1,tbuf,nct)
                  sbuf(ncs+1:) = ',' // laxs(ISCWRK(2,i)) // tbuf(1:nct)
                  ncs    = ncs    + nct    + 2
              endif
              obuf   = '          ' // sbuf(1:ncs)
              nco    = ncs    + 11
              call docout (obuf,nco,0,cmsg,kerr)
          endif
c
c.........Contouring/Positioning axis
c
          if (IJKROT .eq. 1) go to 950
          ilev(nlev) = 4
          call docprm (sbuf,ncs,ilev,nlev)
          call docsap (lcon(IRTMOD(i)),tbuf,nct)
          call errstr (sbuf,tbuf,0)
          call docsap (lsca(IRTSCL(i)),tbuf,nct)
          call errstr (sbuf,tbuf,0)
          ncs    = strlen1(sbuf)
          if (IRTMOD(i) .eq. 1 .or. POSFDF .eq. 1) then
              obuf   = '          ' // sbuf(1:ncs) // '.'
              nco    = ncs    + 11
          else
              ilev(nlev) = 5
              call docprm (tbuf,nct,ilev,nlev)
              obuf   = '          ' // sbuf(1:ncs) // ' ' //
     1                 tbuf(1:nct)
              nco    = ncs    + nct    + 11
          endif
          call docout (obuf,nco,0,cmsg,kerr)
c
c.........Circumference of axis
c
          if (ROTCRM(i) .ne. 1.) then
              ilev(nlev) = 6
              call docprm (sbuf,ncs,ilev,nlev)
              rnum   = ROTCRM(i) * 360.
              call rtoc (rnum,tbuf,nct)
              obuf   = '          ' // sbuf(1:ncs) // ' ' //
     1                 tbuf(1:nct)
              nco    = ncs    + nct    + 11
              call docout (obuf,nco,0,cmsg,kerr)
          endif
c
c.........CLW direction
c
          if (IRTCLW(inc) .eq. 2) then
              ilev(nlev) = 7
              call docprm (sbuf,ncs,ilev,nlev)
              obuf   = '          ' // sbuf(1:ncs)
              nco    = ncs    + 10
              call docout (obuf,nco,0,cmsg,kerr)
          endif
  950     nlev   = nlev   - 1
 1100 continue
c
c...Tool axis vector output
c
      if (IJKROT .eq. 1) then
         iout = 1
         nlev = nlev + 1
         ilev(nlev) = 8
         do 1150 i=1,3
            jax(i) = ' '
            njax(i) = 0
            call docrgf (IJKREG(i),0.d0,jax(i),njax(i),1)
            call docprm (sbuf,ncs,ilev,nlev)
            obuf = '   ' // jax(i)(1:njax(i)) // ' - ' // sbuf
            call docsap (cijk(i),tbuf,nct)
            call errstr (obuf,tbuf,0)
            nco    = strlen1(obuf)
            call docout (obuf,nco,iout,cmsg,kerr)
            iout = 0
 1150    continue
         nlev = nlev - 1
      end if
c
c......Blade axes
c
      if (MACHTP .eq. 3) then
          ilev(nlev) = 8
          ireg   = MOTREG(ima(4))
          call docrgf (ireg,0.d0,lax(10),ncax(10),1)
          call docprm (sbuf,ncs,ilev,nlev)
          obuf   = '   ' // lax(10)(1:ncax(10)) // ' - ' // sbuf
          inc    = 1
          nco    = strlen1(obuf)
          call docout (obuf,nco,1,cmsg,kerr)
c
c.........Direction vector
c
          nlev   = nlev   + 1
          ilev(nlev) = 1
          call docprm (sbuf,ncs,ilev,nlev)
          call getr8s (BLDVEC,3,tbuf,nct)
          obuf   = '          ' // sbuf(1:ncs) // ' ' // tbuf(1:nct)
          nco    = ncs    + nct    + 11
          call docout (obuf,nco,0,cmsg,kerr)
c
c.........Maximum direction change
c
          ilev(nlev) = 2
          call docprm (sbuf,ncs,ilev,nlev)
          call rtoc (BLDELT,tbuf,nct)
          obuf   = '          ' // sbuf(1:ncs) // ' ' // tbuf(1:nct)
          nco    = ncs    + nct    + 11
          call docout (obuf,nco,0,cmsg,kerr)
c
c.........Contouring/Positioning axis
c
          ilev(nlev-1) = 5
          ilev(nlev) = 4
          call docprm (sbuf,ncs,ilev,nlev)
          call docsap (lcon(1),tbuf,nct)
          call errstr (sbuf,tbuf,0)
          call docsap (lsca(IRTSCL(4)),tbuf,nct)
          call errstr (sbuf,tbuf,0)
          ncs    = strlen1(sbuf)
          obuf   = '          ' // sbuf(1:ncs) // '.'
          nco    = ncs    + 11
          call docout (obuf,nco,0,cmsg,kerr)
          nlev   = nlev   - 1
      endif
c
c.........Spindle axis
c
      iout   = 1
      if (MACHTP .ne. 2) then
          ilev(nlev) = 6
          call docprm (sbuf,ncs,ilev,nlev)
          call getr8s (SPIVEC,3,tbuf,nct)
          obuf   = '   ' // sbuf(1:ncs) // ' ' // tbuf(1:nct)
          nco    = ncs    + nct    + 4
          call docout (obuf,nco,iout,cmsg,kerr)
          iout   = 0
      endif
c
c........Mill Turn Polar/Cylindrical interpolation
c
      if (MACHTP .eq. 4) then
         if (LTHPCL(1) .eq. 1) then
             iout   = 1
             ilev(nlev) = 9
             call docprm (sbuf,nct,ilev,nlev)
             obuf = '   ' // sbuf(1:nct)
             nco  = nct + 3
             call docout (obuf,nco,iout,cmsg,kerr)
             iout   = 0
         end if
         if (LTHPCL(2) .eq. 1) then
             ilev(nlev) = 10
             call docprm (sbuf,nct,ilev,nlev)
             obuf = '   ' // sbuf(1:nct)
             nco  = nct + 3
             call docout (obuf,nco,iout,cmsg,kerr)
         end if
      end if
c
c.........Active rotary axes
c
      if (IRTNUM .gt. 2) then
          ilev(nlev) = 7
          call docprm (sbuf,ncs,ilev,nlev)
          obuf   = '   ' // sbuf(1:ncs)
          do 1200 i=1,IRTNUM,1
              if (IRDEAD(i) .eq. 0) then
                  call errstr (obuf,lax(i+6),1)
              endif
 1200     continue
          call errstr (obuf,' ',0)
          nco    = strlen1(obuf)
          call docout (obuf,nco,iout,cmsg,kerr)
      endif
      if (kerr .ne. 0) go to 8000
c
c......Stringer Machine axes
c
      if (MACHTP .eq. 5) then
c
c.........Loop through and output all axes
c
          n      = 6
          ilev(nlev) = 1
          do 1260 j=1,3,1
              call docsap ('HEAD',sbuf,ncs)
              call itoc (j+1,tbuf,nct,0)
              obuf   = ' ' // sbuf(1:ncs) // ' ' // tbuf(1:nct) // ':'
              nco    = 1 + ncs + 1 + nct + 1
              call docout (' ',0,0,cmsg,kerr)
              call docout (obuf,nco,iout,cmsg,kerr)
              call docprm (sbuf,ncs,ilev,nlev)
              do 1250 i=1,n,1
                  if (SGMREG(i,j) .ne. 0) then
                      if (i .eq. 4) then
                          ilev(nlev) = 13
                          call docprm (sbuf,ncs,ilev,nlev)
                      endif
                      ireg   = SGMREG(i,j)
                      ilev(nlev) = 1
                      call docrgf (ireg,0.d0,tbuf,nct,1)
                      call docsap (axis(i),s2buf,ncs2)
                      obuf   = '   ' // tbuf(1:nct) // ' - ' //
     1                                  sbuf(1:ncs)
                      call errstr (obuf,s2buf,1)
                      nco    = strlen1(obuf)
                      call docout (obuf,nco,0,cmsg,kerr)
                  endif
 1250         continue
              ilev(nlev) = 12
              n      = 3
 1260     continue
      endif
c
c...Special considerations
c......Do we need to output this section
c
      ifl    = 0
      do 1300 i=1,5,1
          isw(i) = 0
 1300 continue
      if (MTPDYN .eq. 2) then
          if (LTHXY .eq. 2) then
              isw(1) = 1
              ifl    = 1
          endif
          if (LTHDIA(1) .eq. 2) then
              isw(2) = 1
              ifl    = 1
          endif
          if (LTHDIA(2) .eq. 1) then
              isw(3) = 1
              ifl    = 1
          endif
          if (LTHXD .eq. 1) then
              isw(4) = 1
              ifl    = 1
          endif
      else
          if (MROTTV .ne. 1) then
              isw(5) = 1
              ifl    = 1
          endif
          itab   = 0
          ipiv   = 0
          do 1400 i=1,IRTNUM,1
              if (IRTYPE(i) .eq. 1 .and. NOTABS .eq. 2) itab = 1
              if (IRTYPE(i) .eq. 2 .and. NOPIVS .eq. 2) ipiv = 1
 1400     continue
          if (itab .eq. 1 .or. ipiv .eq. 1) then
              isw(6) = 1
              ifl    = 1
          endif
      endif
c
c......Yes we do
c
      if (ifl .eq. 1) then
          nlev   = 2
          ilev(nlev) = 8
          call docprm (sbuf,ncs,ilev,nlev)
          call docout (sbuf,ncs,1,cmsg,kerr)
          call docout (' ',0,0,cmsg,kerr)
          nlev   = 3
c
          do 1450 i=1,6,1
              if (isw(i) .eq. 1) then
                  ilev(nlev) = i
                  call docprm (sbuf,ncs,ilev,nlev)
                  obuf   = '   ' // sbuf(1:ncs)
                  nco    = ncs    + 3
                  if (i .eq. 6) then
                      if (itab .eq. 1 .and. ipiv .eq. 1) then
                          call docsap ('AXES',tbuf,nct)
                      else if (itab .eq. 1) then
                          call docsap ('TABLE',tbuf,nct)
                      else
                          call docsap ('HEAD',tbuf,nct)
                      endif
                      obuf(nco+1:) = ' ' // tbuf(1:nct) // '.'
                      nco    = nco     + nct    + 2
                  endif
                  call docout (obuf,nco,0,cmsg,kerr)
              endif
 1450     continue
      endif
c
c...Machine limits
c
      nlev   = 2
      ilev(nlev) = 4
      call docprm (sbuf,ncs,ilev,nlev)
      call docout (sbuf,ncs,1,cmsg,kerr)
      call docout (' ',0,0,cmsg,kerr)
      call docsap ('AXIS',sbuf,ncs)
      do 2000 i=1,3,1
c
c......Primary linear axes
c
          if (NUMLIN(i) .ne. 0) then
              call getr8s (LIMITS(1,i*2-1),2,tbuf,nct)
              obuf   = '   ' // lax(i)(1:ncax(i)) // sbuf(1:ncs) //
     1                 ': ' // tbuf(1:nct)
              nco    = strlen1(obuf)
c
c......Secondary linear axes
c
              if (NUMLIN(i) .eq. 2) then
                  call getr8s (LIMITS(1,i*2),2,tbuf,nct)
                  t1buf   = lax(i+3)(1:ncax(i+3)) // sbuf(1:ncs) //
     1                      ': ' // tbuf(1:nct)
                  ist    = 41
                  if (nco .gt. ist) ist = nco + 5
                  if (ist .lt. 80) then
                      obuf(ist:) = t1buf
                      nco    = strlen1(obuf)
                  endif
              endif
              call docout (obuf,nco,0,cmsg,kerr)
          endif
 2000 continue
      do 2100 i=1,2,1
c
c......1st & 2nd rotary axes
c
          nco    = 0
          if (i .le. IRTNUM) then
              call getr8s (LIMITS(1,i+6),2,tbuf,nct)
              obuf   = '   ' // lax(i+6)(1:ncax(i+6)) // sbuf(1:ncs) //
     1                 ': ' // tbuf(1:nct)
              nco    = strlen1(obuf)
          endif
c
c......3rd & 4th rotary axes
c
          if (i+2 .le. IRTNUM .or. (MACHTP .eq. 3 .and. i .eq. 2)) then
              call getr8s (LIMITS(1,i+8),2,tbuf,nct)
              t1buf   = lax(i+8)(1:ncax(i+8)) // sbuf(1:ncs) //
     1                  ': ' // tbuf(1:nct)
              ist    = 41
              if (IRTNUM .eq. 0) ist = 4
              if (nco .gt. ist) ist = nco + 5
              if (ist .lt. 80) then
                  obuf(ist:) = t1buf
                  nco    = strlen1(obuf)
              endif
          endif
          if (nco .ne. 0) call docout (obuf,nco,0,cmsg,kerr)
 2100 continue
      if (kerr .ne. 0) go to 8000
c
c......Stringer Machine axes
c
      if (MACHTP .eq. 5) then
c
c.........Loop through and output all axes
c
          n      = 6
          do 2160 j=1,3,1
              call docsap ('HEAD',s2buf,ncs2)
              call itoc (j+1,tbuf,nct,0)
              obuf   = ' ' // s2buf(1:ncs2) // ' ' // tbuf(1:nct) // ':'
              nco    = 1 + ncs + 1 + nct + 1
              call docout (' ',0,0,cmsg,kerr)
              call docout (obuf,nco,iout,cmsg,kerr)
              do 2150 i=1,n,1
                  if (SGMREG(i,j) .ne. 0) then
                      ireg   = SGMREG(i,j)
                      call docrgf (ireg,0.d0,tbuf,nct,1)
                      call getr8s (SGMLMT(1,i,j),2,s2buf,ncs2)
                      obuf   = '   ' // tbuf(1:nct) // sbuf(1:ncs) //
     1                    ' ' // s2buf(1:ncs2)
                      nco    = strlen1(obuf)
                      call docout (obuf,nco,0,cmsg,kerr)
                  endif
 2150         continue
              n      = 3
 2160     continue
      endif
c
c...Interference zones
c
      if (NUMINF .ne. 0) then
          ilev(nlev) = 5
          call docprm (sbuf,ncs,ilev,nlev)
          call docout (sbuf,ncs,1,cmsg,kerr)
          nlev   = nlev   + 1
          ilev(nlev) = 1
          call docsap ('AXIS',sbuf,ncs)
          do 2500 i=1,NUMINF,1
              call docprm (s1buf,ncs1,ilev,nlev)
              call itoc (i,tbuf,nct,0)
              call errstr (s1buf,tbuf,1)
              ncs1   = strlen1(s1buf)
              obuf   = '   ' // s1buf
              nco    = ncs1   + 3
              iout   = 1
              do 2200 j=1,3,1
c
c......Primary linear axes
c
                  if (NUMLIN(j) .ne. 0) then
                      rsub(1) = INFZON(1,j*2-1,i)
                      rsub(2) = INFZON(2,j*2-1,i)
                      if (rsub(1) .ne. DUMMY) then
                          inc    = 1
                          if (rsub(2) .ne. DUMMY) inc = 2
                          call getr8s (rsub,inc,tbuf,nct)
                          obuf(nco+1:) = ' ' // lax(j)(1:ncax(j)) //
     1                                   sbuf(1:ncs) // ': ' //
     2                                   tbuf(1:nct)
                          nco    = strlen1(obuf)
                      endif
c
c......Secondary linear axes
c
                      if (NUMLIN(i) .eq. 2) then
                          rnum   = rsub(1)
                          rsub(1) = INFZON(1,j*2,i)
                          rsub(2) = INFZON(2,j*2,i)
                          if (rsub(1) .ne. DUMMY) then
                              inc    = 1
                              if (rsub(2) .ne. DUMMY) inc = 2
                              call getr8s (rsub,inc,tbuf,nct)
                              if (rnum .eq. DUMMY) then
                                  nco    = nco    + 1
                              else
                                  if (nco .gt. 40) then
                                      nco    = nco    + 5
                                  else
                                      nco    = 40
                                  endif
                              endif
                              obuf(nco+1:) = lax(j+3)(1:ncax(j+3)) //
     1                                       sbuf(1:ncs) // ': ' //
     2                                       tbuf(1:nct)
                              nco    = strlen1(obuf)
                          endif
                      endif
                      if (nco .gt. ncs1+3) then
                          call docout (obuf,nco,iout,cmsg,kerr)
                          iout   = 0
                          obuf   = ' '
                      endif
                  endif
                  nco    = ncs1   + 3
 2200         continue
c
              do 2300 j=1,2,1
c
c......1st & 2nd rotary axes
c
                  if (i .le. IRTNUM) then
                      rsub(1) = INFZON(1,j+6,i)
                      rsub(2) = INFZON(2,j+6,i)
                      if (rsub(1) .ne. DUMMY) then
                          inc    = 1
                          if (rsub(2) .ne. DUMMY) inc = 2
                          call getr8s (rsub,inc,tbuf,nct)
                          obuf(nco+1:) = ' ' //
     1                                   lax(j+6)(1:ncax(j+6)) //
     1                                   sbuf(1:ncs) // ': ' //
     2                                   tbuf(1:nct)
                          nco    = strlen1(obuf)
                      endif
c
c......3rd & 4th rotary axes
c
                      if (i+2 .le. IRTNUM) then
                          rnum   = rsub(1)
                          rsub(1) = INFZON(1,j+8,i)
                          rsub(2) = INFZON(2,j+8,i)
                          if (rsub(1) .ne. DUMMY) then
                              inc    = 1
                              if (rsub(2) .ne. DUMMY) inc = 2
                              call getr8s (rsub,inc,tbuf,nct)
                              if (rnum .eq. DUMMY) then
                                  nco    = nco    + 1
                              else
                                  if (nco .gt. 40) then
                                      nco    = nco    + 5
                                  else
                                      nco    = 40
                                  endif
                              endif
                              obuf(nco+1:) = lax(j+8)(1:ncax(j+8)) //
     1                                       sbuf(1:ncs) // ': ' //
     2                                       tbuf(1:nct)
                              nco    = strlen1(obuf)
                          endif
                      endif
                      if (nco .gt. ncs1+3) then
                          call docout (obuf,nco,iout,cmsg,kerr)
                          iout   = 0
                          obuf   = ' '
                      endif
                  endif
                  nco    = ncs1   + 3
 2300         continue
 2500     continue
      nlev   = nlev   - 1
      endif
      if (kerr .ne. 0) go to 8000
c
c...Mutually exclusive axes
c
      if (MACHTP .ne. 2 .and. NUMEXC .gt. 0) then
          nlev   = 2
          ilev(nlev) = 14
          call docprm (sbuf,ncs,ilev,nlev)
          call docout (sbuf,ncs,1,cmsg,kerr)
          nlev   = 3
          do 2540 i=1,NUMEXC,1
              ilev(nlev) = 1
              call docprm (sbuf,ncs,ilev,nlev)
              tbuf   = ' '
              nct    = 0
              do 2510 j=1,EXCLNM(i),1
                  if (j .ne. 1) then
                      tbuf(nct+1:) = ' & '
                      nct    = nct    + 3
                  endif
                  inc    = imx(EXCLAX(j,i))
                  tbuf(nct+1:) = lax(inc)(1:ncax(inc))
                  nct    = nct    + ncax(inc)
 2510         continue
              obuf   = '   ' // tbuf(1:nct) // ' ' // sbuf(1:ncs)
              nco    = nct    + ncs     + 4
              call docout (obuf,nco,1,cmsg,kerr)
c
              do 2530 k=1,2,1
                  call docout (' ',0,0,cmsg,kerr)
                  ilev(nlev) = k      + 1
                  call docprm (tbuf,nct,ilev,nlev)
                  inc    = imx(EXCLCO(i))
                  call errstr (tbuf,lax(inc)(1:ncax(inc)),1)
                  sbuf   = '      ' // tbuf
                  ncs    = strlen1(sbuf)
                  if (k .eq. 1) then
                      nc     = jindex(EXCLPS(1,i),IDUMMY,12) - 1
                  else
                      nc     = jindex(EXCLNE(1,i),IDUMMY,12) - 1
                  endif
                  if (nc .eq. -1) nc = 12
                  obuf   = sbuf
                  nco    = ncs    + 1
                  do 2520 j=1,nc,1
                      if (k .eq. 1) then
                          inc    = EXCLPS(j,i)
                      else
                          inc    = EXCLNE(j,i)
                      endif
                      if (inc .eq. -1) then
                          obuf(nco+1:) = ' ' // LEOB(1:NEOB)
                          nco    = nco    + NEOB   + 1
                          call docout (obuf,nco,0,cmsg,kerr)
                          obuf   = ' '
                          nco    = ncs    + 1
                      else
                          inc    = imx(inc)
                          obuf(nco+1:) = ' ' // lax(inc)(1:ncax(inc)) //
     1                                   '..'
                          nco    = nco    + ncax(inc) + 3
                      endif
 2520             continue
                  if (nco .gt. ncs+1) call docout (obuf,nco,0,cmsg,kerr)
 2530         continue
 2540     continue
      endif
c
c...Current position
c......Verify a Current position has been defined
c
      ifl    = 0
      do 2600 i=1,3,1
          if (NUMLIN(i) .gt. 0) then
              if (AXSSTO(i*2-1) .ne. 0) ifl = 1
              if (NUMLIN(i) .eq. 2) then
                  if (AXSSTO(i*2) .ne. 0) ifl = 1
              endif
          endif
 2600 continue
c
      do 2700 i=1,IRTNUM,1
          if (AXSSTO(i+6) .ne. 0) ifl = 1
 2700 continue
c
c......Output Current position
c
      if (ifl .eq. 1) then
          nlev   = 2
          ilev(nlev) = 7
          call docprm (sbuf,ncs,ilev,nlev)
          call docout (sbuf,ncs,1,cmsg,kerr)
          call docout (' ',0,0,cmsg,kerr)
          call docpos (AXSSTO,lax,ncax,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Home position
c......Verify a Home position has been defined
c
      ifl    = 0
      do 2800 i=1,3,1
          if (NUMLIN(i) .gt. 0) then
              if (HOME(i*2-1) .ne. 0) ifl = 1
              if (NUMLIN(i) .eq. 2) then
                  if (HOME(i*2) .ne. 0) ifl = 1
              endif
          endif
 2800 continue
c
      do 2900 i=1,IRTNUM,1
          if (HOME(i+6) .ne. 0) ifl = 1
 2900 continue
c
c......Output Home position
c
      if (ifl .eq. 1) then
          nlev   = 2
          ilev(nlev) = 6
          call docprm (sbuf,ncs,ilev,nlev)
          call docout (sbuf,ncs,1,cmsg,kerr)
          call docout (' ',0,0,cmsg,kerr)
          call docpos (HOME,lax,ncax,IJKROT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Minimum axis increment
c......Verify an increment has been defined
c
      ifl    = 0
      do 3000 i=1,3,1
          if (NUMLIN(i) .gt. 0) then
              if (PPINCR(i*2-1) .ne. 0) ifl = 1
              if (NUMLIN(i) .eq. 2) then
                  if (PPINCR(i*2) .ne. 0) ifl = 1
              endif
          endif
 3000 continue
c
      do 3100 i=1,IRTNUM,1
          if (PPINCR(i+6) .ne. 0) ifl = 1
 3100 continue
c
c......Output axis increments
c
      if (ifl .eq. 1) then
          nlev   = 2
          ilev(nlev) = 9
          call docprm (sbuf,ncs,ilev,nlev)
          call docout (sbuf,ncs,1,cmsg,kerr)
          call docout (' ',0,0,cmsg,kerr)
          call docpos (PPINCR,lax,ncax,IJKROT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Maximum axis departure
c......Verify a departure has been defined
c
      ifl    = 0
      do 3200 i=1,3,1
          if (NUMLIN(i) .gt. 0) then
              if (PPMAXD(i*2-1) .lt. 10000.) ifl = 1
              if (NUMLIN(i) .eq. 2) then
                  if (PPMAXD(i*2) .lt. 10000.) ifl = 1
              endif
          endif
 3200 continue
c
      do 3300 i=1,IRTNUM,1
          if (PPMAXD(i+6) .lt. 10000.) ifl = 1
 3300 continue
c
c......Output axis departures
c
      if (ifl .eq. 1) then
          nlev   = 2
          ilev(nlev) = 10
          call docprm (sbuf,ncs,ilev,nlev)
          call docout (sbuf,ncs,1,cmsg,kerr)
          call docout (' ',0,0,cmsg,kerr)
          call docpos (PPMAXD,lax,ncax,IJKROT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Output tolerance
c......Verify an output tolerance has been defined
c
      ifl    = 0
      do 3400 i=1,3,1
          if (NUMLIN(i) .gt. 0) then
              call mininc (MOTREG(imr(i)),rnum)
              if (PPTOLR(i*2-1) .gt. rnum) ifl = 1
              if (NUMLIN(i) .eq. 2) then
                  call mininc (MOTREG(imr(i+3)),rnum)
                  if (PPTOLR(i*2) .gt. rnum) ifl = 1
              endif
          endif
 3400 continue
c
      do 3500 i=1,IRTNUM,1
          call mininc (MOTREG(ima(i)),rnum)
          if (PPTOLR(i+6) .gt. rnum) ifl = 1
 3500 continue
c
c......Output output tolerance
c
      if (ifl .eq. 1) then
          nlev   = 2
          ilev(nlev) = 11
          call docprm (sbuf,ncs,ilev,nlev)
          call docout (sbuf,ncs,1,cmsg,kerr)
          call docout (' ',0,0,cmsg,kerr)
          call docpos (PPTOLR,lax,ncax,IJKROT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...TRANS/AXIS
c......Verify a TRANS/AXIS has been defined
c
      ifl    = 0
      do 3600 i=1,3,1
          if (NUMLIN(i) .gt. 0) then
              if (TRANAX(i*2-1) .ne. 0) ifl = 1
              if (NUMLIN(i) .eq. 2) then
                  if (TRANAX(i*2) .ne. 0) ifl = 1
              endif
          endif
 3600 continue
c
      do 3700 i=1,IRTNUM,1
          if (TRANAX(i+6) .ne. 0) ifl = 1
 3700 continue
c
c......Output TRANS/AXIS
c
      if (ifl .eq. 1) then
          nlev   = 2
          ilev(nlev) = 12
          call docprm (sbuf,ncs,ilev,nlev)
          call docout (sbuf,ncs,1,cmsg,kerr)
          call docout (' ',0,0,cmsg,kerr)
          call docpos (TRANAX,lax,ncax,IJKROT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...TRANS/SCALE
c......Verify a TRANS/SCALE has been defined
c
      ifl    = 0
      do 3800 i=1,3,1
          if (NUMLIN(i) .gt. 0) then
              if (TRANAX(i*2-1+10) .ne. 1.) ifl = 1
              if (NUMLIN(i) .eq. 2) then
                  if (TRANAX(i*2+10) .ne. 1.) ifl = 1
              endif
          endif
 3800 continue
c
      do 3900 i=1,IRTNUM,1
          if (TRANAX(i+6+10) .ne. 1.) ifl = 1
 3900 continue
c
c......Output TRANS/SCALE
c
      if (ifl .eq. 1) then
          nlev   = 2
          ilev(nlev) = 13
          call docprm (sbuf,ncs,ilev,nlev)
          call docout (sbuf,ncs,1,cmsg,kerr)
          call docout (' ',0,0,cmsg,kerr)
          call docpos (TRANAX(11),lax,ncax,IJKROT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Spindle ranges
c......Single range
c
      nlev   = 2
      ilev(nlev) = 15
      if (NSPRG .eq. 1) then
          call docprm (sbuf,ncs,ilev,nlev)
          call docprm (sbuf,ncs,ilev,nlev)
          call getr8s (SPNLMT(1,2),2,tbuf,nct)
          obuf   = sbuf(1:ncs) // ' ' // tbuf(1:nct)
          nco    = ncs    + nct    + 1
          call docout (obuf,nco,1,cmsg,kerr)
c
c......Multiple ranges
c
      else
          nlev   = 3
          ilev(nlev) = 1
          call docprm (sbuf,ncs,ilev,nlev)
          ifl    = 1
          do 4500 i=1,3,1
              if (NSPRG .eq. 3 .or. (NSPRG .eq. 2 .and. i .ne. 2)) then
                  call docsap (lspn(i),s1buf,nc1)
                  call getr8s (SPNLMT(1,i),2,tbuf,nct)
                  obuf   = s1buf(1:nc1) // ' ' // sbuf(1:ncs) // ' ' //
     1                     tbuf(1:nct)
                  nco    = nc1    + ncs    + nct    + 2
                  call docout (obuf,nco,ifl,cmsg,kerr)
                  ifl    = 0
             endif
 4500    continue
      endif
c
c......SFM range
c
      if (SPNSFM .eq. 1) then
          nlev   = 3
          ilev(nlev) = 2
          call docprm (sbuf,ncs,ilev,nlev)
          call getr8s (SPNLMT(1,4),2,tbuf,nct)
          obuf   = sbuf(1:ncs) // ' ' // tbuf(1:nct)
          nco    = ncs    + nct    + 1
          call docout (obuf,nco,0,cmsg,kerr)
      endif
      if (kerr .ne. 0) go to 8000
c
c...Mill attachment spindle ranges
c......Single range
c
      if (MACHTP .eq. 4) then
          if (NSPRG .eq. 1) then
              nlev   = 3
              ilev(nlev) = 3
              call docprm (sbuf,ncs,ilev,nlev)
              call getr8s (SPMLMT(1,2),2,tbuf,nct)
              obuf   = sbuf(1:ncs) // ' ' // tbuf(1:nct)
              nco    = ncs    + nct    + 1
              call docout (obuf,nco,1,cmsg,kerr)
c
c......Multiple ranges
c
          else
              nlev   = 3
              ilev(nlev) = 4
              call docprm (sbuf,ncs,ilev,nlev)
              ifl    = 1
              do 4600 i=1,3,1
                  if (NSPRG .eq. 3 .or. (NSPRG .eq. 2 .and. i .ne. 2))
     1                    then
                      call docsap (lspn(i),s1buf,nc1)
                      call getr8s (SPMLMT(1,i),2,tbuf,nct)
                      obuf   = s1buf(1:nc1) // ' ' // sbuf(1:ncs) //
     1                         ' ' // tbuf(1:nct)
                      nco    = nc1    + ncs    + nct    + 2
                      call docout (obuf,nco,ifl,cmsg,kerr)
                      ifl    = 0
                 endif
 4600        continue
          endif
      endif
c
c...Maximum feed rates
c
      nlev   = 2
      ilev(nlev) = 16
      call docprm (sbuf,ncs,ilev,nlev)
      call docout (sbuf,ncs,1,cmsg,kerr)
      call docout (' ',0,0,cmsg,kerr)
      call docpos (FEDLMT,lax,ncax,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Rapid rates
c
      nlev   = 2
      ilev(nlev) = 17
      call docprm (sbuf,ncs,ilev,nlev)
      call docout (sbuf,ncs,1,cmsg,kerr)
      call docout (' ',0,0,cmsg,kerr)
      call docpos (RAPLMT,lax,ncax,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Acceleration blocks
c
      if (IACCFL(1) .eq. 1) then
          nlev   = 2
          ilev(nlev) = 18
          call docprm (sbuf,ncs,ilev,nlev)
          call docout (sbuf,ncs,1,cmsg,kerr)
          call docout (' ',0,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          nlev   = 3
c
          ilev(nlev) = 1
          call docprm (sbuf,ncs,ilev,nlev)
          call getr8s (MAXVEL,1,tbuf,nct)
          obuf   = '   ' // sbuf(1:ncs) // ' ' // tbuf(1:nct)
          nco    = ncs    + nct    + 11
          call docout (obuf,nco,0,cmsg,kerr)
c
          ilev(nlev) = 2
          call docprm (sbuf,ncs,ilev,nlev)
          call getr8s (AXSVEL,1,tbuf,nct)
          obuf   = '   ' // sbuf(1:ncs) // ' ' // tbuf(1:nct)
          nco    = ncs    + nct    + 11
          call docout (obuf,nco,0,cmsg,kerr)
c
          do 4700 i=1,2,1
              ilev(nlev) = i     + 2
              call docprm (sbuf,ncs,ilev,nlev)
              call getr8s (ACCSTP(i),1,tbuf,nct)
              obuf   = '   ' // sbuf(1:ncs) // ' ' // tbuf(1:nct)
              nco    = ncs    + nct    + 11
              call docout (obuf,nco,0,cmsg,kerr)
 4700     continue
          if (kerr .ne. 0) go to 8000
      endif
c
c...Clear output buffer
c
      call docclr (cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  docpos (gpos,cax,kncax,cmsg,kerr)
c
c   FUNCTION:  This routine outputs machine position type blocks in the
c              Machine Configuration section of the automatic documen-
c              tation.  For example, Home Position, Current Position,
c              etc.  The output will have the following format.
c
c                 X1.0        U2.0
c                 Y3.0        V4.0
c                 Z5.0        W6.0
c                 A7.0        B8.0
c                 C9.0        D10.0
c
c   INPUT:  gpos    R*8  D10 -  Array containing the position values to
c                               output.  Only the supported axes for
c                               this MDF file will be output.
c
c           cax    C*24  D10 -  Register beginning and ending text
c                               labels.
c
c           kncax  I*4   D10 -  Number of characters in 'cax'.
c
c           krot   I*4   D1  -  1 - rotary axes active, 2 - rotary not used
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine docpos (gpos,cax,kncax,krot,cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (NUMLIN,KPOSMP(1202))
      equivalence (IRTNUM,KPOSMP(1243))
c
      integer*4 MACHTP,NUMLIN(3),IRTNUM
c
      integer*4 kerr,kncax(10),krot
c
      real*8 gpos(10)
c
      character*(*) cmsg
      character*24 cax(10)
c
      integer*4 i,ist,nco,nct,strlen1,ncs
c
      character*80 obuf,tbuf,t1buf,sbuf
c
c...Output position
c
      call docsap ('AXIS',sbuf,ncs)
      do 500 i=1,3,1
c
c.........Primary linear axes
c
          if (NUMLIN(i) .ne. 0) then
              call rtoc (gpos(i*2-1),tbuf,nct)
              obuf   = '   ' // cax(i)(1:kncax(i)) // sbuf(1:ncs) //
     1                 ': ' // tbuf(1:nct)
              nco    = strlen1(obuf)
c
c.........Secondary linear axes
c
              if (NUMLIN(i) .eq. 2) then
                  call rtoc (gpos(i*2),tbuf,nct)
                  t1buf   = cax(i+3)(1:kncax(i+3)) // sbuf(1:ncs) //
     1                      ': ' // tbuf(1:nct)
                  ist    = 41
                  if (nco .gt. ist) ist = nco + 5
                  if (ist .lt. 80) then
                      obuf(ist:) = t1buf
                      nco    = strlen1(obuf)
                  endif
              endif
              call docout (obuf,nco,0,cmsg,kerr)
          endif
  500 continue
c
c......1st & 2nd rotary axes
c
      do 700 i=1,2,1
          nco    = 0
          if (i .le. IRTNUM .and. krot .eq. 1) then
              call rtoc (gpos(i+6),tbuf,nct)
              obuf   = '   ' // cax(i+6)(1:kncax(i+6)) //
     1                 sbuf(1:ncs) // ': ' // tbuf(1:nct)
              nco    = strlen1(obuf)
          endif
c
c......3rd & 4th rotary axes
c
          if (i+2 .le. IRTNUM .and. krot .eq. 1 .or.
     1                 (MACHTP .eq. 3 .and. i .eq. 2)) then
              call rtoc (gpos(i+8),tbuf,nct)
              t1buf   = cax(i+8)(1:kncax(i+8)) //
     1                  sbuf(1:ncs) // ': ' // tbuf(1:nct)
              ist    = 41
              if (IRTNUM .eq. 0) ist = 4
              if (nco .gt. ist) ist = nco + 5
              if (ist .lt. 80) then
                  obuf(ist:) = t1buf
                  nco    = strlen1(obuf)
              endif
          endif
          if (nco .gt. 0) call docout (obuf,nco,0,cmsg,kerr)
  700 continue
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
