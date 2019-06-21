c
c***********************************************************************
c
c   FILE NAME:  pstwah
c   CONTAINS:
c               align  arcslp  check   clamp   clrsrf  coolnt  couple
c               delay  dlyout  disply  head    getpl
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pstwah.f , 25.3
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 08:18:26
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  align
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 ALIGN/AXIS
c                 ALIGN/CUT
c                 ALIGN/MOVE
c                 ALIGN/OPTION
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine align
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (IRTACT,KPOSMP(1256))
      equivalence (IRTINC,KPOSMP(1461)), (IRTYPE,KPOSMP(1486))
      equivalence (ACHEAD,KPOSMP(1645))
      equivalence (IACHFL,KPOSMP(1646)), (IACFLG,KPOSMP(1648))
      equivalence (MODBLD,KPOSMP(3984)), (ALNCUT,KPOSMP(4040))
      equivalence (ALNCSK,KPOSMP(4041)), (ALNMOV,KPOSMP(4045))
      equivalence (ALNMSK,KPOSMP(4046)), (ALNMRP,KPOSMP(4047))
      equivalence (ALNMOD,KPOSMP(4049))
c
      integer*4 MXCL,IPSTWD(50),MODBLD,ALNCUT,ALNCSK,ALNMOV,
     1          ALNMSK,ALNMRP,ALNMOD,IRTYPE(20),
     2          ACHEAD,IACHFL,IACFLG(5),IRTACT(2),IRTINC(4)
c
      equivalence (TLATOL,POSMAP(0057)), (PSTWD ,POSMAP(0441))
      equivalence (ALNDIS,POSMAP(4444)), (ALNMPS,POSMAP(4461))
      equivalence (TABORG,POSMAP(5374))
c
      real*8 PSTWD(20),ALNDIS(3),ALNMPS(3),TLATOL,TABORG(3,20)
c
      integer*4 n,inc,mod,nctp,ir1,ir2,ip1
c
      real*8 rprm(3),nmag
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))
c
      if (MXCL .lt. 2 .or. MXCL .gt. 5) go to 9400
c
      inc    = 1
      ip1    = IRTINC(ir1)
      if (IPSTWD(inc) .eq. 803) then
         mod = 1
      else if (IPSTWD(inc) .eq. 577) then
         mod = 2
c
c...ALIGN/OPTION,n
c
      else if (IPSTWD(inc) .eq. 144) then
         if (MODBLD .eq. 0) go to 9500
         if (IPSTWD(inc+1) .ne. 0) go to 9100
         mod = PSTWD(inc+1)
         if (mod .lt. 1 .or. mod .gt. 3) go to 9200
         ALNMOD = mod
         go to 8000
c
c...ALIGN/AXIS
c
      else if (IPSTWD(inc) .eq. 132) then
          inc    = inc    + 1
          if (inc .gt. MXCL) go to 9400
c
c......ALIGN/AXIS,OFF
c
          if (IPSTWD(inc) .eq. 72) then
              IACFLG(1) = 1
              IACHFL = 0
              TLATOL = 0.

c
c......ALIGN/AXIS,tol
c
          else if (ACHEAD .eq. 1) then
  100         if (IPSTWD(inc) .eq. 0) then
                  TLATOL = PSTWD(inc)
c
c......ALIGN/AXIS,AUTO
c
              else if (IPSTWD(inc) .eq. 88) then
                  IACFLG(1) = 2
                  IACHFL = 0
c
c......ALIGN/AXIS,RAPID
c
              else if (IPSTWD(inc) .eq. 5) then
                  IACFLG(1) = 3
                  IACHFL = 0
c
c......ALIGN/AXIS,NEXT
c
              else if (IPSTWD(inc) .eq. 162) then
                  IACFLG(2) = 1
c
c......ALIGN/AXIS,LIMIT
c
              else if (IPSTWD(inc) .eq. 1078) then
                  IACFLG(2) = 2
c
c......ALIGN/AXIS,TRANS
c
              else if (IPSTWD(inc) .eq. 1037) then
                  IACFLG(3) = 1
c
c......ALIGN/AXIS,NOW
c
              else if (IPSTWD(inc) .eq. 161) then
                  IACFLG(3) = 2
c
c......ALIGN/AXIS,DOWN
c
              else if (IPSTWD(inc) .eq. 113) then
                  IACFLG(4) = 1
c
c......ALIGN/AXIS,UP
c
              else if (IPSTWD(inc) .eq. 112) then
                  IACFLG(4) = 2
c
c......Unrecognized minor word
c
              else
                  go to 9600
              endif
c
c......Get next word
c
              inc    = inc    + 1
              if (inc .le. MXCL) go to 100
c
c......Enable correct style of look ahead
c......Normal for AC-style head
c......During RAPID moves only for all other configurations
c
              if (IACFLG(1) .eq. 1) IACFLG(1) = 2
              if (IACFLG(1) .eq. 2 .and. (IRTYPE(ip1) .eq. 1 .or.
     1            nmag(TABORG(1,ip1)) .ne. 0.)) IACFLG(1) = 3
          endif
          go to 8000
c
c...Invalid minor word
c
      else
         go to 9600
      end if
c
c...ALIGN/*,OFF
c...        OMIT
c
      if (MODBLD .eq. 0) go to 9500
      inc    = 2
      if (IPSTWD(inc) .eq. 72) then
         nctp = 1
      else if (IPSTWD(inc) .eq. 176) then
         nctp = 2
c
c...ALIGN/*,ON [,....]
c...        NEXT [,....]
c
      else if (IPSTWD(inc) .eq. 71) then
         nctp = 3
      else if (IPSTWD(inc) .eq. 162) then
         nctp = 4
      else
         go to 9600
      end if
c
c...set flags for OFF or OMIT
c
      if (nctp .lt. 3) then
         if (mod .eq. 1) then
             if (nctp .eq. 2) ALNCSK = ALNCUT
             ALNCUT = 0
         else
             if (nctp .eq. 2) ALNMSK = ALNMOV
             ALNMOV = 0
         end if
         if (MXCL .gt. inc) go to 9500
c
c...Get parameters if any
c
      else
         n   = 0
         do 115 inc=inc+1,MXCL,1
            if (IPSTWD(inc) .ne. 0) go to 9100
            n   = n + 1
            rprm(n) = PSTWD(inc)
            if (rprm(n) .lt. 0.) go to 9200
  115    continue
         if (n .lt. 3 .and. n .ne. 0) go to 9500
c
c...Set flag and parameters for NEXT or ON
c
         if (mod .eq. 1) then
             ALNCUT = nctp - 2
             if (n .ne. 0) then
                 if (rprm(2) .lt. rprm(1)) go to 9200
                 call copyn (rprm,ALNDIS,n)
             end if
         else
             ALNMRP = 4 - nctp
             ALNMOV = 0
             if (nctp .eq. 4) ALNMOV = 2
             if (n .ne. 0) call copyn (rprm,ALNMPS,n)
         end if
      end if
c
 8000 return
c
c...Errors
c...Number expected
c
 9100 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9200 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Invalid command
c
 9300 call psterr (2,'NOTVALID',' ',inc)
      go to 8000
c
c...Invalid command syntax
c
 9400 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Invalid command syntax
c
 9500 call psterr (2,'INVPSYNW',' ',MXCL+1)
      go to 8000
c
c...Invalid minor word
c
 9600 call psterr (2,'INVMINOR',' ',MXCL+1)
      go to 8000
c
      end
c
c***********************************************************************
c
c   SUBROUTINE:  arcslp
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 ARCSLP/LINCIR,ON
c                               OFF
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine arcslp
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (ICIRFL,KPOSMP(1367)), (ICIRSW,KPOSMP(1391))
      equivalence (BSPLSW,KPOSMP(4084)), (BSPLFL,KPOSMP(4088))
      equivalence (NBSCDS,KPOSMP(4230)), (BSTCOD,KPOSMP(4231))
      equivalence (NBECDS,KPOSMP(4236)), (BNDCOD,KPOSMP(4237))
c
      integer*4 MXCL,IPSTWD(50),ICIRFL,ICIRSW,BSPLSW,BSPLFL
      integer*4 NBSCDS,BSTCOD(5),NBECDS,BNDCOD(5)
c
      equivalence (DUMMY ,POSMAP(0003)), (BSPCDV,POSMAP(4901))
      equivalence (BSTVAL,POSMAP(4936)), (BNDVAL,POSMAP(4941))
c
      real*8 DUMMY,BSPCDV,BSTVAL(5),BNDVAL(5)
c
      integer*4 i, inc
c
c...ARCSLP/LINCIR
c
      inc    = 1
      if (MXCL .lt. 2) go to 9000
      if (IPSTWD(1) .eq. 95) then
          inc    = 2
c
c......ARCSLP/LINCIR,ON
c
          if (IPSTWD(inc) .eq. 71) then
              if (IOPFL(10) .ne. 3) ICIRSW = ICIRFL
c
c......ARCSLP/LINCIR,OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              ICIRSW = 2
c
c......Unrecognized minor word
c
          else
              if (IPSTWD(inc) .eq. 0) go to 9200
              go to 9500
          endif
      else if (IPSTWD(1) .eq. 628) then
          inc    = 2
c
c......ARCSLP/BSPLIN,ON
c
          if (IPSTWD(inc) .eq. 71) then
              BSPLSW = BSPLFL
              if (BSPCDV .eq. DUMMY) BSPCDV = 0.
              if (NBSCDS.gt.0) then
                do i=1,NBSCDS
                   call codout (BSTCOD(i),BSTVAL(i))
                enddo
                call clrbuf
              endif
c
c......ARCSLP/BSPLIN,OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              BSPLSW = 2
              if (NBECDS.gt.0) then
                do i=1,NBECDS
                   call codout (BNDCOD(i),BNDVAL(i))
                enddo
                call clrbuf
              endif
c
c......Unrecognized minor word
c
          else
              if (IPSTWD(inc) .eq. 0) go to 9200
              go to 9500
          endif
c
      else
          if (IPSTWD(inc) .eq. 0) go to 9200
          go to 9500
      endif
      if (inc .gt. MXCL) go to 9700
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Minor word expected
c
 9200 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
c
c...Invalid minor word
c
 9500 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9700 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  check
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 CHECK/-AXIS,(n),la,ha, ...
c
c                 CHECK/LENGTH,ON
c                              OFF
c
c                 CHECK/OUT,m1,(n),la,ha, ...
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine check
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (ICSMFL,KPOSMP(1133)), (NUMINF,KPOSMP(1396))
c
      integer*4 MXCL,IPSTWD(50),ICSMFL,NUMINF
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (METCNV,POSMAP(0004)), (INFZON,POSMAP(0071))
      equivalence (PSTWD ,POSMAP(0441)), (LIMITS,POSMAP(1254))
c
      real*8 METCNV,PSTWD(50),LIMITS(2,10),INFZON(2,10,4),DUMMY
c
      integer*4 ist,inc,i,nprm,isub,ifl
c
      real*8 rzon(2,10)
c
c...CHECK/LENGTH
c
      if (MXCL .lt. 1) go to 9400
      if (IPSTWD(1) .eq. 9) then
          inc    = 2
          if (MXCL .lt. 2) go to 9400
c
c...CHECK/LENGTH,ON
c
          if (IPSTWD(2) .eq. 71) then
              ICSMFL = 1
c
c......CHECK/LENGTH,OFF
c
          else if (IPSTWD(2) .eq. 72) then
              ICSMFL = 2
c
c......Invalid minor word
c
          else
              if (IPSTWD(2) .eq. 0) go to 9000
              go to 9300
          endif
          if (MXCL .gt. inc) go to 9500
c
c...CHECK/(OUT,m,)-AXIS,...
c
      else
          if (IPSTWD(1) .eq. 49) then
              if (MXCL .lt. 5) go to 9400
              inc    = 2
              if (IPSTWD(inc) .ne. 0) go to 9100
              if (PSTWD(inc) .lt. 1 .or. PSTWD(inc) .gt. 4) go to 9200
              ifl    = PSTWD(inc)
              inc    = inc    + 1
              do 80 i=1,10,1
                  rzon(1,i) = DUMMY
                  rzon(2,i) = DUMMY
   80         continue
          else
              if (MXCL .lt. 3) go to 9400
              inc    = 1
              ifl    = 0
          endif
          ist    = inc
c
c......Get next parameter section
c
  100     inc    = ist
          if (IPSTWD(ist) .eq. 0) go to 9000
          inc    = inc    + 1
          do 200 i=inc,MXCL,1
              if (IPSTWD(i) .ne. 0) go to 250
  200     continue
          i      = MXCL   + 1
  250     nprm   = i      - inc
c
c.........Invalid number of parameters
c
          if (nprm .lt. 2) then
              inc    = i
              go to 9100
          else if (nprm .gt. 3) then
              inc    = inc    + 3
              go to 9000
          endif
c
c.........Check parameter values
c
          isub   = 1
          if (nprm .eq. 3) then
              isub   = PSTWD(inc)
              inc    = inc    + 1
          endif
c
          if (PSTWD(inc) .gt. PSTWD(inc+1)) go to 9200
c
c......Set limits
c
          if (IPSTWD(ist) .eq. 84) then
              if (isub .ne. 1 .and. isub .ne. 2) then
                  inc    = inc    - 1
                  go to 9200
              endif
              ist    = 0
          else if (IPSTWD(ist) .eq. 85) then
              if (isub .ne. 1 .and. isub .ne. 2) then
                  inc    = inc    - 1
                  go to 9200
              endif
              ist    = 2
          else if (IPSTWD(ist) .eq. 86) then
              if (isub .ne. 1 .and. isub .ne. 2) then
                  inc    = inc    - 1
                  go to 9200
              endif
              ist    = 4
          else if (IPSTWD(ist) .eq. 132) then
              if (isub .lt. 1 .or. isub .gt. 4) then
                  inc    = inc    - 1
                  go to 9200
              endif
              ist    = 6
          else
              go to 9300
          endif
          ist    = ist    + isub
          if (ifl .eq. 0) then
              LIMITS(1,ist) = PSTWD(inc)
              if (ist .le. 6) LIMITS(1,ist) = LIMITS(1,ist) * METCNV
              LIMITS(2,ist) = PSTWD(inc+1)
              if (ist .le. 6) LIMITS(2,ist) = LIMITS(2,ist) * METCNV
          else
              rzon(1,ist) = PSTWD(inc) * METCNV
              if (ist .le. 6) rzon(1,ist) = rzon(1,ist) * METCNV
              rzon(2,ist) = PSTWD(inc+1) * METCNV
              if (ist .le. 6) rzon(2,ist) = rzon(2,ist) * METCNV
          endif
          ist    = inc    + 2
          if (ist .le. MXCL) go to 100
c
c......Set number of interference zones
c
          if (ifl .ne. 0) then
              do 600 i=1,10,1
                  INFZON(1,i,ifl) = rzon(1,i)
                  INFZON(2,i,ifl) = rzon(2,i)
  600         continue
              if (ifl .gt. NUMINF) NUMINF = ifl
          endif
      endif
c
c...End of routine
c
 8000 return
c
c...Minor word expected
c
 9000 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
c
c...Number expected
c
 9100 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9200 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Invalid minor word
c
 9300 call psterr (2,'INVMINOR',' ',ist)
      go to 8000
c
c...Invalid command syntax
c
 9400 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Too many parameters
c
 9500 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clamp
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 CLAMP/AXIS,n1(,...n4),ON
c                                       OFF
c
c                 CLAMP/AUTO,n
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clamp
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (IRTOUT,KPOSMP(0813))
      equivalence (IRTCLM,KPOSMP(1306)), (CLMPCD,KPOSMP(1307))
      equivalence (CLMPBK,KPOSMP(1327)), (CLMPUB,KPOSMP(1757))
c
      integer*4 MXCL,IPSTWD(50),CLMPCD(2,10),CLMPBK,CLMPUB(2,10),
     -          IRTOUT(4),IRTCLM
c
      equivalence (PSTWD ,POSMAP(0441)), (CLMPVL,POSMAP(1454))
c
      real*8 PSTWD(50),CLMPVL(2,10)
c
      integer*4 inc,i,iaxs(6),is,ifrc
c
c...CLAMP/AXIS
c
      if (MXCL .eq. 0) go to 9000
      inc    = 1
      if (IPSTWD(1) .eq. 132) then
          if (MXCL .lt. 3 .or. MXCL .gt. 6) go to 9000
          iaxs(1) = 0
          iaxs(2) = 0
          iaxs(3) = 0
          iaxs(4) = 0
c
          do 500 inc=inc+1,MXCL-1,1
              if (IPSTWD(inc) .ne. 0) go to 9300
              if (PSTWD(inc) .le. 0 .or. PSTWD(inc) .gt. 4)
     1                go to 9400
              is     = PSTWD(inc)
              if (IRTOUT(is) .ne. 1) go to 9400
              iaxs(is) = 1
  500     continue
          inc    = MXCL
c
c......CLAMP/AXIS,ON
c
          if (IPSTWD(inc) .eq. 71) then
              do 600 i=1,4,1
                  if (iaxs(i) .eq. 1) then
                      call codout (CLMPCD(1,i+6),CLMPVL(1,i+6))
                      if (CLMPUB(1,i) .ne. 0)
     1                    call frcblk(CLMPUB(1,i),ifrc,1)
                  endif
  600         continue
c
c......CLAMP/AXIS,OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              do 650 i=1,4,1
                  if (iaxs(i) .eq. 1) then
                      call codout (CLMPCD(2,i+6),CLMPVL(2,i+6))
                      if (CLMPUB(2,i) .ne. 0)
     1                    call frcblk(CLMPUB(2,i),ifrc,1)
                  endif
  650         continue
c
c......Unrecognized minor word
c
          else
              if (IPSTWD(inc) .eq. 0) go to 9500
              go to 9100
          endif
c
c......Clear output buffer
c
          if (CLMPBK .eq. 1) call clrbuf
c
c...CLAMP/AUTO,n
c
      else if (IPSTWD(1) .eq. 88) then
          if (MXCL .lt. 2) go to 9000
          inc    = 2
          is     = PSTWD(inc)
          if (is .lt. 1 .or. is .gt. 5) go to 9400
          IRTCLM = is
c
c...Unrecognized Command
c
      else
           if (IPSTWD(1) .ne. 0) go to 9500
           go to 9100
      endif
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Invalid minor word
c
 9100 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Number expected
c
 9300 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9400 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Minor word expected
c
 9500 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  CLRSRF
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 CLRSRF/(NORMAL,)(i,j,k,)d
c
c                 CLRSRF/TOOL,d
c
c                 CLRSRF/AVOID(,i,j,k),d
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clrsrf
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (ICLIPS,KPOSMP(0112)), (NPLANE,KPOSMP(0113))
      equivalence (CPLNTR,KPOSMP(0115)), (CLRTFL,KPOSMP(0116))
      equivalence (CLRTCP,KPOSMP(0121))
c
      integer*4 MXCL,IPSTWD(50),ICLIPS,NPLANE,CPLNTR,CLRTFL(5),CLRTCP
c
      equivalence (DUMMY ,POSMAP(0003)), (METCNV,POSMAP(0004))
      equivalence (CLRPLN,POSMAP(0161)), (CLSAV ,POSMAP(0201))
      equivalence (CPLANE,POSMAP(0253))
      equivalence (CPLNFD,POSMAP(0390)), (CLRTPL,POSMAP(0391))
      equivalence (CLRTFD,POSMAP(0403)), (CLRTDS,POSMAP(0406))
      equivalence (PSTWD ,POSMAP(0441)), (CYCPLN,POSMAP(2964))
      equivalence (SPIVEC,POSMAP(3583)), (FUZZ4 ,POSMAP(4912))
c
      real*8 PSTWD(20),METCNV,CLRPLN(4),CYCPLN(4),SPIVEC(3),CLRTDS,
     1       CPLANE(5,10),DUMMY,FUZZ4,CPLNFD,CLRTPL(4,3),CLRTFD(3),
     2       CLSAV(21)
c
      integer*4 inc,ipt,njndex,imod,ist,ifl,nout
c
      real*8 rdis,p(5),nmag,rfed(30)
c
c...CLRSRF/(NORMAL,)d
c
      inc    = 1
      if (MXCL .lt. 1) go to 9000
      if ((IPSTWD(1) .eq. 0 .or. IPSTWD(1) .eq. 111) .and.
     -                               IPSTWD(MXCL) .eq. 0) then
          if (IPSTWD(1) .eq. 111) then
              if (MXCL .lt. 2) go to 9000
              inc    = 2
          endif
          if (MXCL .eq. inc) then
              CLRPLN(1) = SPIVEC(1)
              CLRPLN(2) = SPIVEC(2)
              CLRPLN(3) = SPIVEC(3)
              CLRPLN(4) = PSTWD(inc) * METCNV
c
c......CLRSRF/i,j,k,d
c
          else if (MXCL .ge. inc+3) then
              ipt    = inc
              do 100 inc=ipt,ipt+3,1
                  if (IPSTWD(inc) .ne. 0) go to 9300
  100         continue
              inc    = ipt    + 2
              rdis   = dsqrt(PSTWD(ipt)**2 + PSTWD(ipt+1)**2 +
     1                       PSTWD(ipt+2)**2)
              if (rdis .eq. 0.) go to 9400
              CLRPLN(1) = PSTWD(ipt) / rdis
              CLRPLN(2) = PSTWD(ipt+1) / rdis
              CLRPLN(3) = PSTWD(ipt+2) / rdis
              CLRPLN(4) = PSTWD(ipt+3) * METCNV
              if (MXCL .gt. ipt+3) go to 9700
c
c......Invalid syntax
c
          else
              go to 9000
          endif
c
c...CLRSRF/TOOL,d
c
      else if (IPSTWD(1) .eq. 617) then
          if (MXCL .lt. 2) go to 9000
          inc    = 2
          if (IPSTWD(inc) .ne. 0) go to 9300
          CLRPLN(1) = DUMMY
          CLRPLN(2) = PSTWD(inc)
          if (MXCL .gt. 2) go to 9700
c
c...CLRSRF/STOP
c
      else if (IPSTWD(inc) .eq. 2) then
          if (CLRTFL(4) .eq. 1) then
              nout   = 1
              call pre_retrct (CLSAV,CLSAV,rfed,nout,1)
          endif
          CLRTFL(4) = 0
          if (NPLANE .le. 0) go to 9500
          ICLIPS = 2
          if (MXCL .ne. 1) go to 9700
c
c...CLRSRF/NOMORE
c
      else if (IPSTWD(inc) .eq. 53) then
          if (CLRTFL(4) .eq. 1) then
              nout   = 1
              call pre_retrct (CLSAV,CLSAV,rfed,nout,1)
          endif
          CLRTFL(4) = 0
          ICLIPS = 0
          NPLANE = 0
          if (MXCL .ne. 1) go to 9700
c
c...CLRSRF/START
c
      else if (IPSTWD(inc) .eq. 57) then
          if (NPLANE .le. 0) go to 9500
          CPLNTR = 2
          CPLNFD = -1.
          CLRTDS = 0.
          CLRTFL(1) = 0
          CLRTFL(2) = 0
          CLRTFL(3) = 0
          CLRTFL(4) = 0
          do while (inc .lt. MXCL)
              inc    = inc    + 1
c
c......CLRSRF/START,TRFORM
c
              if (IPSTWD(inc) .eq. 110) then
                  if (inc .eq. MXCL) go to 9100
                  inc    = inc    + 1
                  if (IPSTWD(inc) .ne. 71 .and. IPSTWD(inc) .ne. 72)
     1                go to 9100
                  CPLNTR = IPSTWD(inc) - 70
c
c......CLRSRF/START,IPM
c
              else if (IPSTWD(inc) .eq. 73 .or. IPSTWD(inc) .eq. 315)
     1                then
                  if (inc .eq. MXCL) go to 9300
                  inc    = inc    + 1
                  if (IPSTWD(inc) .ne. 0) go to 9300
                  if (PSTWD(inc) .lt. 0.) go to 9400
                  CPLNFD = PSTWD(inc) * METCNV
c
c......CLRSRF/START,RTRCTO
c
              else if (IPSTWD(inc) .eq. 295) then
                  if (inc .eq. MXCL) go to 9300
                  inc    = inc    + 1
                  if (IPSTWD(inc) .eq. 72) then
                      CLRTFL(1) = 0
                  else
                      call getpl (inc,CLRTPL(1,1),ifl)
                      if (ifl .eq. 0) go to 9800
                      CLRTFL(1) = ifl
                  endif
                  CLRTFD(1) = CPLNFD
                  CLRTFD(2) = CPLNFD
                  CLRTFD(3) = CPLNFD
c
                  ist    = 1
                  do while (inc .lt. MXCL)
                      inc    = inc    + 1
                      if (inc .eq. MXCL) go to 9300
c
c.........CLRSRF/START,RTRCTO,IPM
c
                      if (IPSTWD(inc) .eq. 73 .or. IPSTWD(inc) .eq. 315)
     1                        then
                          inc    = inc    + 1
                          if (IPSTWD(inc) .ne. 0) go to 9300
                          if (PSTWD(inc) .lt. 0.) go to 9400
                          CLRTFD(ist) = PSTWD(inc) * METCNV
c
c.........CLRSRF/START,RTRCTO,DELTA
c
                      else if (IPSTWD(inc) .eq. 188) then
                          inc    = inc    + 1
                          if (IPSTWD(inc) .ne. 0) go to 9300
                          if (PSTWD(inc) .lt. 0.) go to 9400
                          CLRTDS = PSTWD(inc) * METCNV
c
c.........CLRSRF/START,RTRCTO,LAST
c
                      else if (IPSTWD(inc) .eq. 52) then
                          CLRTFL(4) = 1
c
c.........CLRSRF/START,RTRCTO,RAPTO
c
                      else if (IPSTWD(inc) .eq. 280) then
                          inc    = inc    + 1
                          if (IPSTWD(inc) .eq. 0) then
                              call getpl (inc,CLRTPL(1,2),ifl)
                              if (ifl .eq. 0) go to 9800
                              CLRTFL(2) = ifl
                          else
                              inc    = inc    - 1
                              call copyn (CLRTPL(1,1),CLRTPL(1,2),4)
                              CLRTFL(2) = CLRTFL(1)
                          endif
                          ist    = 2
c
c.........CLRSRF/START,RTRCTO,PLUNGE
c
                      else if (IPSTWD(inc) .eq. 1001) then
                          inc    = inc    + 1
                          call getpl (inc,CLRTPL(1,3),ifl)
                          if (ifl .eq. 0) go to 9800
                          CLRTFL(3) = ifl
                          ist    = 3
                      else
                          go to 9100
                      endif
                  enddo
              else
                  go to 9100
              endif
          enddo
          ICLIPS = 1
      else if (IPSTWD(MXCL) .ne. 0) then
c
c...CLRSRF/d,modifier
c
         if (MXCL .gt. 5) go to 9700
         if (MXCL .eq. 2) then
            if (IPSTWD(1) .ne. 0) go to 9300
            p(1) = 0.
            p(2) = 0.
            p(3) = 1.
            inc  = 2
c
c...CLRSRF/a,b,c,d,modifier
c
         else if (MXCL .eq. 5) then
            inc  = njndex(IPSTWD(1),0,4)
            if (inc .ne. 0) go to 9000
            inc  = 5
            if (nmag(PSTWD(1)) .lt. FUZZ4) go to 9400
            call unitvc (PSTWD(1),p(1))
         else
            go to 9000
         end if
         p(4) = PSTWD(inc-1) * METCNV
         imod = IPSTWD(inc)
         if (imod .lt. 11 .or. imod .gt. 22) go to 9100
         if (imod .lt. 20 .and. imod .gt. 13) go to 9100
         p(5) = 1.d0
         if (imod .gt. 13) then
            ipt = imod - 19
            if (p(ipt) .lt. .0) p(5) = -1.d0
         else
            ipt = imod - 10
            if (p(ipt) .gt. .0) p(5) = -1.d0
         end if
         if (dabs(p(ipt)) .lt. FUZZ4) go to 9150
         if (NPLANE .lt. 10) then
            NPLANE = NPLANE + 1
            call copyn (p,CPLANE(1,NPLANE),5)
         else
            go to 9600
         end if
c
c...CLRSRF/AVOID
c
      else
          if (MXCL .lt. 2) go to 9000
          if (IPSTWD(1) .ne. 187) go to 9100
c
c......CLRSRF/AVOID,d
c
          if (MXCL .eq. 2) then
              inc    = 2
              if (IPSTWD(2) .ne. 0) go to 9300
              CYCPLN(1) = SPIVEC(1)
              CYCPLN(2) = SPIVEC(2)
              CYCPLN(3) = SPIVEC(3)
              CYCPLN(4) = PSTWD(2) * METCNV
c
c......CLRSRF/AVOID,i,j,k,d
c
          else if (MXCL .ge. 5) then
              do 600 inc=2,5,1
                  if (IPSTWD(inc) .ne. 0) go to 9300
  600         continue
              inc    = 4
              rdis   = dsqrt(PSTWD(2)**2 + PSTWD(3)**2 + PSTWD(4)**2)
              if (rdis .eq. 0.) go to 9400
              CYCPLN(1) = PSTWD(2) / rdis
              CYCPLN(2) = PSTWD(3) / rdis
              CYCPLN(3) = PSTWD(4) / rdis
              CYCPLN(4) = PSTWD(5) * METCNV
              if (MXCL .gt. 5) go to 9700
c
c......Invalid syntax
c
          else
              go to 9000
          endif
      endif
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Invalid minor word
c
 9100 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Invalid minor word
c
 9150 call psterr (2,'CLSMOD','INVMINOR',inc)
      go to 8000
c
c...Number expected
c
 9300 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9400 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Clip planes not defined
c
 9500 call psterr (2,'CLSINV',' ',inc)
      go to 8000
c
c...Too many clip planes defined
c
 9600 call psterr (2,'CLSEXCC',' ',MXCL)
      go to 8000
c
c...Too many parameters
c
 9700 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
c
c...Plane expected
c
 9800 call psterr (1,'PLNEXP',' ',inc+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  coolnt
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 COOLNT/ON
c                        OFF
c                        FLOOD
c                        MIST
c                        AIR
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine coolnt
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (REGSW ,KPOSMP(0405)), (SGMACT,KPOSMP(0996))
      equivalence (SGMCOD,KPOSMP(1018)), (MACHTP,KPOSMP(1201))
      equivalence (COOLCD,KPOSMP(3128))
      equivalence (SPCOCD,KPOSMP(3132)), (COLBLK,KPOSMP(3138))
      equivalence (LSTCOL,KPOSMP(3140)), (SPNDIR,KPOSMP(3141))
      equivalence (ICOLSW,KPOSMP(3146))
c
      integer*4 MXCL,IPSTWD(50),REGSW(MAXFMT),COOLCD(4),SPCOCD(6),
     1          COLBLK(2),LSTCOL,SPNDIR,ICOLSW,SGMACT(2),SGMCOD(10),
     2          MACHTP
c
      equivalence (COOLVL,POSMAP(3294)), (SPCOVL,POSMAP(3298))
      equivalence (RPM   ,POSMAP(3307)), (SGMCDV,POSMAP(5552))
c
      real*8 COOLVL(4),SPCOVL(6),RPM,SGMCDV(10)
c
      integer*4 inc,ion,icod,ierr
c
      real*8 rval
c
      character*80 lmsg
c
      if (MXCL .eq. 0) go to 9000
      inc    = 1
c
c...COOLNT/ON
c
      if (IPSTWD(1) .eq. 71) then
          if (MACHTP .eq. 5 .and. SGMACT(1) .gt. 1) then
              call codout (SGMCOD(4),SGMCDV(4))
              call clrbuf
              ion    = 4
          else
              ion    = LSTCOL
          endif
c
c...COOLNT/OFF
c
      else if (IPSTWD(1) .eq. 72) then
          if (MACHTP .eq. 5 .and. SGMACT(1) .gt. 1) then
              ion    = 4
              call codout (SGMCOD(5),SGMCDV(5))
              call clrbuf
          else
              ion    = 4
              call codout (COOLCD(4),COOLVL(4))
              if (COLBLK(2) .eq. 1) call clrbuf
          endif
c
c...COOLNT/MIST
c
      else if (IPSTWD(1) .eq. 90) then
          ion    = 1
c
c...COOLNT/FLOOD
c
      else if (IPSTWD(1) .eq. 89) then
          ion    = 2
c
c...COOLNT/AIR
c
      else if (IPSTWD(1) .eq. 1011) then
          ion    = 3
c
c...Unrecognized minor word
c
      else
          if (IPSTWD(inc) .eq. 0) go to 9500
          go to 9100
      endif
c
c...Coolant is turned on
c...Output proper code
c
      ICOLSW = 2
      if (ion .ne. 4) then
          ICOLSW = 1
          LSTCOL = ion
          icod   = COOLCD(ion)
          rval   = COOLVL(ion)
          call spncol (icod,rval,2)
          call codout (icod,rval)
          if (COLBLK(1) .eq. 1) call clrbuf
      endif
c
c...Output Simulation record
c
      call simsta (0,lmsg,ierr)
      if (MXCL .gt. 1) go to 9200
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Invalid minor word
c
 9100 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9200 call psterr (1,'INVPSYNW',' ',MXCL+1)
      go to 8000
c
c...Minor word expected
c
 9500 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  couple
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 COUPLE/n,ATANGL,a
c                                 AUTO
c
c                 COUPLE/AUTO,ON
c                             OFF
c
c                 COUPLE/OFF
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine couple
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (IHELTP,KPOSMP(1280))
      equivalence (IHELIX,KPOSMP(1392)), (ICRHEL,KPOSMP(4250))
c
      integer*4 MXCL,IPSTWD(50),IHELIX,ICRHEL(10),IHELTP
c
      equivalence (METCNV,POSMAP(0004))
      equivalence (PSTWD ,POSMAP(0441)), (HELANG,POSMAP(2213))
c
      real*8 PSTWD(20),HELANG(2),METCNV
c
      integer*4 inc
c
c...COUPLE/AUTO
c
      inc    = 1
      if (MXCL .lt. 1) go to 9000
      if (IPSTWD(1) .eq. 88) then
          if (MXCL .lt. 2) go to 9000
          inc    = MXCL
c
c......COUPLE/AUTO,ON
c
          if (IPSTWD(inc) .eq. 71) then
              ICRHEL(1) = 1
c
c......COUPLE/AUTO,OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              ICRHEL(1) = 2
c
c......Unrecognized minor word
c
          else
              if (IPSTWD(inc) .eq. 0) go to 9200
              go to 9500
          endif
          if (MXCL .gt. inc) go to 9700
c
c...COUPLE/OFF
c
      else if (IPSTWD(1) .eq. 72) then
          IHELIX = 0
          if (MXCL .gt. inc) go to 9700
c
c...COUPLE/n,ATANGL,a
c
      else
          if (MXCL .lt. 3) go to 9000
          if (IPSTWD(1) .ne. 0) go to 9300
          if (PSTWD(1) .eq. 0.) go to 9400
c
          inc    = 2
          if (IPSTWD(inc) .ne. 1) then
              if (IPSTWD(inc) .eq. 0) go to 9200
              go to 9500
          endif
c
          inc    = 3

          if (IPSTWD(inc) .ne. 0) go to 9300
          if (PSTWD(3) .eq. 0) then
              IHELTP = 2
              HELANG(1) = (PSTWD(1)*METCNV)
          else
              IHELTP = 1
              HELANG(1) = (PSTWD(1)*METCNV) / PSTWD(3)
          endif
c
          IHELIX = ICRHEL(1)
          if (MXCL .gt. inc) go to 9700
      endif
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Minor word expected
c
 9200 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
c
c...Number expected
c
 9300 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9400 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Invalid minor word
c
 9500 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9700 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  delay
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 DELAY/n(,REV)
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine delay
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
c
      integer*4 MXCL,IPSTWD(50)
c
      equivalence (PSTWD ,POSMAP(0441)), (RPM   ,POSMAP(3307))
c
      real*8 PSTWD(20),RPM
c
      integer*4 inc,ifl
c
      real*8 dwl
c
c...DELAY/n
c
      inc    = 1
      if (MXCL .lt. 1) go to 9000
      if (IPSTWD(1) .ne. 0) go to 9300
      if (PSTWD(1) .le. 0.) go to 9400
      dwl    = PSTWD(1)
      ifl    = 1
c
c......DELAY/n,REV
c
      if (MXCL .ge. 2) then
          inc    = 2
          if (IPSTWD(2) .eq. 0) go to 9200
          if (IPSTWD(2) .ne. 97) go to 9500
          ifl    = 2
      endif
c
c......Output dwell
c
      call dlyout (dwl,ifl)
      if (MXCL .gt. inc) go to 9700
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Minor word expected
c
 9200 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
c
c...Number expected
c
 9300 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9400 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Invalid minor word
c
 9500 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9700 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  dlyout (gtim,kfl)
c
c   FUNCTION:  This routine outputs a delay block.
c
c   INPUT:  gtim    R*8  D1  - Amount of time in seconds or number of
c                              spindle revolutions to delay.
c
c           kfl     I*4  D1  - 1 = 'gtim' is specified in seconds. 2 =
c                              'gtim' is specified in REV's.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine dlyout (gtim,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGFRC,KPOSMP(0603)), (ITP   ,KPOSMP(1801))
      equivalence (IFITYP,KPOSMP(3150)), (DELYCD,KPOSMP(3351))
      equivalence (DELYFL,KPOSMP(3356))
c
      integer*4 IFITYP,DELYCD(5),DELYFL(5),ITP,REGFRC(MAXFMT)
c
      equivalence (DELYVL,POSMAP(0165)), (DELYTM,POSMAP(0166))
      equivalence (RPM   ,POSMAP(3307))
c
      real*8 DELYVL,DELYTM,RPM
c
      integer*4 kfl
c
      real*8 gtim
c
      integer*4 inum,i,ireg
c
      real*8 rnum,rtim,rrev
c
c...Calculate delay time &
c...spindle revolutions to delay
c
      if (kfl .eq. 1) then
          rtim   = gtim
          rrev   = gtim / 60. * RPM
      else
          if (RPM .eq. 0.) go to 9000
          rtim   = gtim   / RPM * 60.
          rrev   = gtim
      endif
c
c...Output delay block(s)
c
      call clrbuf
c
c.....Multiple delay blocks
c
      if (DELYFL(1) .eq. 5) then
          rnum   = rtim   / DELYTM + .5
          inum   = rnum
          if (inum .eq. 0) inum = 1
          do 100 i=1,inum,1
              if (DELYFL(3) .eq. 1) then
                  ireg   = -4
                  call regtyp (ireg,rnum)
                  if (ireg .gt. 0) REGFRC(ireg) = 1
              endif
              call codout (DELYCD(1),DELYVL)
              call clrbuf
  100     continue
c
c......Delay in spindle revolutions
c
      else if (DELYFL(1) .eq. 2 .or.
     1         (DELYFL(1) .eq. 3 .and. IFITYP .eq. 2) .or.
     2         (DELYFL(1) .eq. 4 .and. kfl .eq. 2)) then
          if (rrev .eq. 0.) go to 9000
          if (DELYFL(3) .eq. 1) then
              ireg   = -4
              call regtyp (ireg,rnum)
              if (ireg .gt. 0) REGFRC(ireg) = 1
          endif
          call codout (DELYCD(1),DELYVL)
          call codout (DELYCD(3),rrev)
          call clrbuf
c
c......Delay in time
c
      else
          if (DELYFL(2) .eq. 1) then
              rnum   = rtim   * 1000.
          else if (DELYFL(2) .eq. 2) then
              rnum   = rtim
          else
              rnum   = rtim   / 60.
          endif
          if (DELYFL(1) .eq. 6 .and. rnum .ne. 0) rnum = 1. / rnum
          if (DELYFL(3) .eq. 1) then
              ireg   = -4
              call regtyp (ireg,rnum)
              if (ireg .gt. 0) REGFRC(ireg) = 1
          endif
          call codout (DELYCD(1),DELYVL)
          call codout (DELYCD(2),rnum)
          call clrbuf
      endif
c
c...Force feed rate on next block
c
      if (DELYFL(4) .eq. 1) then
          ireg   = -14
          call regtyp (ireg,rnum)
          if (ireg .gt. 0) REGFRC(ireg) = 1
      endif
c
c...Accumulate machining time
c
      rnum   = rtim   / 60.
      call addtim (rnum)
c
c...End of routine
c
 8000 return
c
c...Delay per rev programmed
c...with spindle off
c
 9000 call psterr (2,'DLYOFF',' ',-1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  disply
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 DISPLY/ON
c                        OFF
c                        AUTO
c
c                 DISPLY/ALIGN,n
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine disply
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (IDSPLY,KPOSMP(0091)), (MSGALN,KPOSMP(0349))
c
      integer*4 MXCL,IPSTWD(50),IDSPLY,MSGALN
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      integer*4 nc,strlen1,inc,ierr
c
      character*80 ldat,lmsg
c
      if (MXCL .eq. 0) go to 9000
      inc    = 1
c
c...DISPLY/ON
c
      if (IPSTWD(1) .eq. 71) then
          IDSPLY = 1
c
c...DISPLY/OFF
c
      else if (IPSTWD(1) .eq. 72) then
          IDSPLY = 2
c
c...DISPLY/AUTO
c
      else if (IPSTWD(1) .eq. 88) then
          ldat   = 'RUNDATE: ' // LDATE // '   ' // LTIME
          nc     = strlen1(ldat)
          call msgout (ldat,nc)
c
c......Output simulation record
c
          call simdis (ldat,nc,lmsg,ierr)
c
c......DISPLY/ALIGN
c
      else if (IPSTWD(1) .eq. 1076) then
          inc    = 2
          if (MXCL .ne. 2 .or. IPSTWD(2) .ne. 0) go to 9600
          if (PSTWD(2) .lt. 0 .or. PSTWD(2) .gt. 66) go to 9700
          MSGALN = PSTWD(2)
c
c...Unrecognized minor word
c
      else
          if (IPSTWD(inc) .eq. 0) go to 9500
          go to 9100
      endif
      if (MXCL .gt. inc) go to 9200
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Invalid minor word
c
 9100 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9200 call psterr (1,'INVPSYNW',' ',MXCL+1)
      go to 8000
c
c...Minor word expected
c
 9500 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
c
c...Value expected
c
 9600 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Value out of range
c
 9700 call psterr (2,'INPRNG',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  head
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 HEAD/ZIGZAG,ON
c                             OFF
c                             h,dis
c
c                 HEAD/m [,n] [,HOME]
c                             [,LENGTH,r [s]]
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine head
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (SIMACT,KPOSMP(0174))
      equivalence (IZIGON,KPOSMP(0370)), (SGMACT,KPOSMP(0996))
      equivalence (ITP   ,KPOSMP(1801)), (MXTOOL,KPOSMP(1803))
c
      integer*4 MXCL,IPSTWD(50),IZIGON,SGMACT(2),ITP,SIMACT,MXTOOL
c
      equivalence (METCNV,POSMAP(0004))
      equivalence (PSTWD ,POSMAP(0441)), (ZIGHIG,POSMAP(0187))
      equivalence (ZIGDIS,POSMAP(0188)), (ZIGSLP,POSMAP(0189))
      equivalence (ZIGDEP,POSMAP(0190)), (ZIGHSV,POSMAP(0191))
      equivalence (TL    ,POSMAP(3601)), (TLNO  ,POSMAP(3841))
      equivalence (SGMTL ,POSMAP(5550))
c
      real*8 METCNV,PSTWD(50),ZIGHSV,ZIGHIG,ZIGDIS,ZIGSLP,
     -       ZIGDEP,SGMTL(2),TL(120),TLNO(120)
c
      integer*4 inc,iact(2),ihom,i,ierr
c
      character*80 msg
c
      inc    = 1
      ihom   = 0
c
c...HEAD/m [,n]
c
      if (IPSTWD(1) .eq. 0) then
          if (MXCL .ge. 2 .and. IPSTWD(2) .eq. 0) then
              inc    = 2
              if (PSTWD(1) .ne. 3 .or. PSTWD(2) .ne. 4) go to 9400
              iact(1) = PSTWD(1)
              iact(2) = PSTWD(2)
              inc    = 3
          else
              if (PSTWD(1) .lt. 1 .or. PSTWD(1) .gt. 4) go to 9400
              iact(1) = PSTWD(1)
              iact(2) = 0.
              inc    = 2
          endif
c
c......HEAD/m,HOME
c
          if (inc .le. MXCL) then
              if (IPSTWD(inc) .eq. 790) then
                  ihom   = 1
c
c......HEAD/m,LENGTH
c
              else if (IPSTWD(inc) .eq. 9) then
                  inc    = inc    + 1
                  if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
                  SGMTL(iact(1)-2) = PSTWD(inc) * METCNV
                  if (inc .lt. MXCL) then
                      inc    = inc    + 1
                      if (iact(2) .eq. 0) go to 9600
                      if (IPSTWD(inc) .ne. 0) go to 9300
                      SGMTL(iact(2)-2) = PSTWD(inc) * METCNV
                  endif
c
c.........Simulate LOADTL command
c............Check for previously loaded tool
c
                  if (SIMACT .eq. 1) then
                      do 300 i=1,MXTOOL,1
                          if (TLNO(i) .eq. 0) go to 310
  300                 continue
c
c............Use next available slot
c
                      if (MXTOOL .ge. 120) then
                          call psterr (1,'TOOLOVR1','TOOLOVR2',1)
                          i      = MXTOOL
                      else
                          MXTOOL = MXTOOL + 1
                          i      = MXTOOL
                      endif
 310                  ITP    = i
                      TL(ITP) = SGMTL(iact(1)-2)
c
c............Output tool length to simulation file
c
                      call simsta (3,msg,ierr)
                  endif
c
c......Invalid minor word
c
              else
                  go to 9100
              endif
          endif
c
c......Setup active head
c
          call smdset (iact,ihom)
c
c...HEAD/ZIGZAG,...
c
      else
          if (MXCL .lt. 2) go to 9000
          if (IPSTWD(inc) .ne. 170) go to 9100
          IZIGON = 1
          inc    = 2
c
c......ON
c
          if (IPSTWD(inc) .ne. 0) then
              if (MXCL .gt. inc) go to 9200
              if (IPSTWD(inc) .eq. 71) then
                  if (ZIGHSV .eq. 0.0) go to 9300
                  ZIGHIG = ZIGHSV
                  go to 500
c
c......OFF
c
              else if (IPSTWD(inc) .eq. 72) then
                  ZIGHSV = ZIGHIG
                  PSTWD(inc) = 0.0
              else
                  go to 9100
              end if
          end if
c
c......hig,
c
          ZIGHIG = PSTWD(inc) * METCNV
c
c......ZIGZAG off - reset zigis if set before
c
          if (ZIGHIG .eq. 0.0) then
              if (MXCL .gt. 2) go to 9000
              if (IZIGON .ne. 0) then
                  IZIGON = 0
                  if (ZIGDEP .ne. 0.0) IZIGON = 2
              end if
              go to 8000
          end if
c
c......,dis
c
          inc    = 3
          if (MXCL .lt. inc) go to 9000
          if (IPSTWD(inc) .ne. 0) go to 9300
          ZIGDIS = PSTWD(inc) * METCNV
c
  500     if (ZIGDIS .ne. 0.0) ZIGSLP = ZIGHIG/ZIGDIS
          if (MXCL .gt. inc) go to 9200
          ZIGDEP = 0.0
      endif
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Invalid minor word
c
 9100 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9200 call psterr (1,'INVPSYNW',' ',MXCL+1)
      go to 8000
c
c...Number expected
c
 9300 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9400 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Minor word expected
c
 9500 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
c
c...Minor word expected
c
 9600 call psterr (2,'INVNUMPR',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getpl (kinc,gpl,kfl)
c
c   FUNCTION:  This routine parses a portion of a post command for a
c              plane or a distance.
c
c   INPUT:  kinc    I*4  D1  - Position within PSTWD arrays to start
c                              parsing at.
c
c   OUTPUT: kinc    I*4  D1  - Position at the last word parsed.
c
c           gpl     R*8  D4  - Plane or distance parsed.
c
c           kfl     I*4  D1  - 0 = Parsing error, 1 = a plane is provided,
c                              2 = a distance is provided.
c
c***********************************************************************
c
      subroutine getpl (kinc,gpl,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
c
      integer*4 MXCL,IPSTWD(50)
c
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
c
      real*8 METCNV,PSTWD(50)
c
      integer*4 kinc,kfl
c
      real*8 gpl(4)
c
      integer*4 i
c
c...Initialize routine
c
      kfl    = 0
      if (kinc .gt. MXCL .or. IPSTWD(kinc) .ne. 0) go to 8000
c
c...Distance provided
c
      if (kinc .eq. MXCL .or. IPSTWD(kinc+1) .ne. 0) then
          gpl(1) = PSTWD(kinc) * METCNV
          kfl    = 2
c
c...Plane provided
c
      else
          if (kinc+3 .gt. MXCL .or. IPSTWD(kinc+1) .ne. 0 .or.
     1        IPSTWD(kinc+2) .ne. 0 .or. IPSTWD(kinc+3) .ne. 0)
     2            go to 8000
          do 100 i=1,4,1
              gpl(i) = PSTWD(kinc)
              kinc   = kinc   + 1
  100     continue
          gpl(4) = gpl(4) * METCNV
          kinc   = kinc   - 1
          kfl    = 1
      endif
c
c...End of routine
c
 8000 return
      end
