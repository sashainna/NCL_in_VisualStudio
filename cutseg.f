C*********************************************************************
C*    NAME         :  cutseg.f
C*       CONTAINS:
C*           cutseg  cutdis  cuthld  cutprs  cutrst  gcutlb
C*           cutset_flag     cutget_flag
C*
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cutseg.f , 25.3
C*    DATE AND TIME OF LAST  MODIFICATION
C*       11/22/17 , 11:10:30
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cutseg
C*          this routine entered on cutter/ command.
C*
C          CHG:  IF NWDS=5 OR 6 THIS IS BARREL CUTTER,  CALL BARSEG.  
C                OTHERWISE SET IFL(282)=0 FOR NON-BARREL TOOL.    17-DEC-87
C*
C*          sto data in tool( ) tbl
C*              tool(1)  dia
C*                  (2)  rad
C*                  (3)  hgt
C*                  (4)  h ps lookpt1
C*                  (5)  " ds   "
C*                  (6)  e
C*                  (7)  effective r for cs cleard calcs   9-15-81
C*                  (8)  sin(alfa) in ta/atangl,alfa,ps
C*                  (9)  cos  "    "   "     "    "
C*                  (10) hin in  ta/tanto,ds,hin
C*                  (11) l/r contact  "   "   "
C*
C*                  (12) drgt in modfy
C*                  (13) dfwd "    "
C*                  (14) dup  "    "
C*                  (15) tsys  i-value in modfy
C*                  (16)  "    j   "   "    "
C*                  (17)  "    k   "   "    "
C*             tool (18) dx      ]
C*                  (19) dy      ]   vector in ta/ , ,perpto,v1 commands
C*                  (20) dz      ]
C*
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine cutseg
      include 'com8a.com'
      include 'cutter.com'
      include 'mocom.com'

      real*4 asc(400)
      integer*2 ksn(4)
      equivalence (asn,ksn),(asc,sc)

      integer*2 iclw(80),maxind,ttyp,tins,sind,ttype,tinsm
      integer*4 ksn3,kerr,outval,lerr,tnc,tline,trec
      real*8 rclw(20)
      character*20 aclw
      equivalence (rclw,iclw,aclw)
      character*(MAX_LEN) mytemp,ttemp
      character*80 ctemp

      integer*2 i, ierr, i6, i6000
      data i6 /6/, i6000 /6000/

      integer*2 BLADE,SHADE,SMOOTH,LATHE,OFFSET,SHANK,CUTTER,HOLDER,
     1          PSEUDO
      parameter (BLADE=191)
      parameter (LATHE=700)
      parameter (SHADE=578)
      parameter (SMOOTH=1085)
      parameter (OFFSET=666)
      parameter (SHANK=192)
      parameter (CUTTER=716)
      parameter (HOLDER=157)
      parameter (PSEUDO=762)
c
c...Initialize routine
c
      radian = 57.29578d0
      asn    = sc(10)
      idis   = 0
      maxind = 0
      ind = 1
c
c...CUTTER/TOOL
c...CUTTER/READ added - Andrew 2/14/13
c
      if ((sc(11) .eq. (617-10000)).or.(sc(11) .eq. (876-10000))) then
          if (sc(11) .eq. (876-10000)) then
              call ncl_setcut_outfl
              fromt = .true.
              maxind = ifl(123)
              do 10 ind=1,maxind,1
                  call nclf_getw2(ind,cimage,nccimg,IRCTYP)
                  mytemp = cimage(1:nccimg)
                  cimage = '$$' // mytemp
                  nccimg = nccimg + 2
                  call nclf_putw2 (ind,cimage,nccimg,IRCTYP)
10            continue
          else
              call ncl_resetcut_outfl
          endif
          call cutool (ictent,ctbuf,ictnc)
          if (ictent .eq. -1) ictent = 0
          go to 99
c
c...CUTTER/DISPLY
c
      else if (sc(11) .eq. (1021-10000)) then
          call cutdis (idis,ksn(3),ierr)
          if (ierr .ne. 0) go to 98
          if (idis .eq. 0) go to 99
c
c...CUTTER/PROFIL,"lib-name"
c
      else if (sc(11) .eq. (761-10000)) then
          if (sc(12) .ne. -10000) then
              call error(145)
              goto 98
          endif
          call ncl_setcut_proffl
          call nclf_open_cutprof(lsc(2),lerr)
          if (lerr .ne. 0) then
              call error(145)
              goto 98
          endif
          goto 99
c
c...CUTTER/PSEUDO,dia,rad, ...
c
      else if (sc(11) .eq. (PSEUDO-10000)) then
          idis   = 3
          do 75 i=11,17,1
             sc(i) = sc(i+1)
   75     continue
          ksn(3) = ksn(3) - 1
      endif
c
c...Blade cutter
c
      lblade = .false.
      lthbit = 0
      if (sc(11) .eq. BLADE - 10000) then
        if (ksn(3).ne.5) goto 97
        sc(28) = 0.0
        sc(29) = 0.0
        sc(30) = sc(14)
        tool(1) = 0.0
        tool(2) = 0.0
        tool(3) = sc(14)
        asc(68) = sc(12)
        asc(305) = dsin(sc(15)/radian)
        sc(15)   = asc(305)
        asc(307) = sc(13)
        lblade = .true.
        i = ksn(3)+1
        call putcl (i6000,i6,i,sc(11))
        if (idis .ne. 3) call cutrst (idis)
        goto 99
      endif
c
c...Lathe bit
c
      if (sc(11) .eq. LATHE - 10000) then
        if (ksn(3) .lt. 4 .or. ksn(3) .gt. 6) goto 97
        if (sc(15) .lt. 0. .or. sc(15) .gt. 90.) then
            ifl(2) = -1
            go to 98
        endif
        if (ksn(3) .lt. 6) sc(16) = 0.
        if (ksn(3) .lt. 5) sc(15) = 0.
        sc(28) = sc(12) * 2.
        sc(29) = 0.0
        sc(30) = sc(14)
        tool(1) = sc(28)
        tool(2) = 0.0
        tool(3) = sc(14)
        asc(68) = sc(16)
        asc(305) = sc(15)
        asc(307) = sc(13)
c
        sc(31) = 0.
        asc(63) = 0.
        asc(64) = 1.
        asc(65) = sc(12)
        asc(66) = 0.
        tool(4) = .005
        tool(5) = sc(30) / 2.
        tool(6) = sc(12)
        efr = dsqrt(.25*(sc(28)**2+sc(30)**2))
        tool(7) = efr
        ifl(282) = 0
c
        lthbit = 2
        if (sc(15) .eq. 90.) lthbit = 1
        if (sc(15) .eq. 60) lthbit = 3
        if (sc(15) .eq. 0.) then
            lthbit = 4
            if (sc(13) .ne. 0.) lthbit = 5
        endif
        i = ksn(3)+1
        call putcl (i6000,i6,i,sc(11))
        if (idis .ne. 3) call cutrst (idis)
        goto 99
      endif
C               BARREL CUTTER DEF IF NWDS EQ. 5 OR 6        17-NOV-87
c
c... Implement 'APTSRC,CUTTER' options
c... Sharon - 05Aug91
c
c... PPRINT, SO DON'T CHECK ARGUMENTS
c      IF(IFL(307).EQ.2) GOTO 94
c... NCL WITH LE 4 ARGUMENTS
c
c... If its not a barrel cutter, adjust count to ignore trailing zeroes
c... that the tool library may have added. Should really be fixed in the
c... tool library code.
c
      nc     = ksn(3)
      if (nc.gt.4 .and. nc.lt.7 .and. sc(15).eq.0.) then
        ksn(3) = 1
        do 8 i=1,nc,1
          if (sc(i+10) .ne. 0.) ksn(3) = i
    8   continue
      endif
      IF(KSN(3).LE.4.OR.KSN(3).GT.6) GOTO 5
c... APT AND NCL BARREL CUTTER
      CALL BARSEG (idis,sc(11),dcutr,ksn(3))
      IF(IFL(2).GT.0)GOTO 98
      if (idis .ne. 3) call cutrst (idis)
      GOTO 94
5     CONTINUE
c
c... Error if ncl with gt 6 arguments
c... Sharon - 05Aug91
c
      if (ksn(3).gt.7) goto 97
c
c...Seven parameter cutter 
c
      if (ksn(3).eq.7) then
        ksn3 = ksn(3)
        call claptc (sc(11),ksn3,kerr)
        if (kerr .eq. 1) then
          call error(-119)
          goto 98
        endif
        ksn(3) = ksn3
      endif
c*********
c
      nw=ksn(3)
      disk=0.
c              zero the sc-list after last input param.
      jst=11+nw
      do 11 i=jst,17
11    sc(i)=0.
      dia=sc(11)
      rad=sc(12)
      hgt=sc(13)
      bet=0.
      hsidtn=rad
      si=0.
      co=1.
      cutn=dia/2.
      cute=cutn-rad
      if(nw-4)18,12,98
c              side angle(beta) given
12    bet=sc(14)
      if(bet.gt.89..or.bet.lt.-89.)goto 98
      si=dsin(bet/radian)
      co=dcos(bet/radian)
      dia=(dia/2.-rad+((hgt-rad)*si+rad)/co)*2.
      if(dia.lt.0.)goto 98
      hsidtn=rad*(1.-si)
      cutn=co*cute+hsidtn
18    if(nw.ge.3)goto 20
      if(nw.eq.1)rad=0.
      hgt=rad
c          check input.  dia, rad, hgt may not be negative
20    if(dia.lt.0..or.rad.lt.0..or.hgt.lt.0.)goto 98
c          if hgt.lt.hsidtn, this is disk case.  calc new beta,etc.
      if(hgt.ge.hsidtn-.001.or.rad.le.0.)goto 28
      disk=1.
      si=(rad-hgt)/rad
      bet=dasin(si)
      co=dcos(bet)
      bet=bet*radian
      dia=(cute+co*rad)*2.
      cutn=cute*co-rad*si+rad
28    if(cute+.001.lt.0.) goto 98
c          move input data to cutter sto area  sc(28),, and tool-tbl
      if (idis .eq. 0 .or. idis .eq. 3) then
          sc(28)=sc(11)
          sc(29)=rad
          sc(30)=hgt
          sc(31)=bet
          asc(63)=si
          asc(64)=co
          asc(65)=cutn
          asc(66)=disk
          tool(1)=dia
          tool(2)=rad
          tool(3)=hgt
c          1st look pts
          tool(4)=rad+.005
          tool(5)=(rad+hgt)/2.
c          if disk cutter, ds 1st lookpt = hgt        19-jun-84
          if(disk.eq.1.)tool(5)=hgt
          tool(6)=cute
c          cs 1st lookpt  h-val
          efr=dsqrt(.25*(dia**2+hgt**2))
          tool(7)=efr
c                       IFL(282)=0 MEANS NON-BARREL TOOL    17-NOV-87
          IFL(282)=0
          if (idis .ne. 3) call cutrst (idis)
      else if (idis .eq. 1) then
          dcutr(1) = sc(11)
          dcutr(2) = rad
          dcutr(3) = hgt
          dcutr(4) = bet
          dcutr(9) = 0
          icutfl(1) = 1
          icutfl(5) = 0
          icutfl(6) = 0
          cutsym(1) = ' '
          cutkey(1) = 0
      endif
c          set up cutter circle command for vt125 graphics
c      idia=dia*50.*sc(106)
c      write (cir125,1010) idia
c1010  format ('c[+',i4,']')

c           zero ifl(251) to make subroutine cdraw re-calculate cutter disply
      ifl(251)=0

c*****
c          the following patch outputs the cutter record to
c          the clfile. type = 6000 , subtype = 6 , sc(28) = diameter
C
C
C... put variable number of parameters into cl file
C... Sharon - 05Aug91
C
   94 if (idis .eq. 0) then
          call putcl (6000,6,ksn(3)+1,sc(11))
      else if (idis .eq. 1) then
          call putcl (7100,1,ksn(3)+1,sc(11))
      endif
C
      goto 99
c
   97 ifl(2) = 4
c          error exit
98    err=.true.
c          illegal cutter shape
      if (ifl(2).lt.1) ifl(2)=119
99    return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cutdis (kcut,knc,kerr)
c
c   FUNCTION:  Parses the CUTTER/DISPLY commands, sets the appropriate
c              flags, and outputs the clfile records.
c
c   INPUT:  knc     I*2   D1  -  Number of parameters in command.
c
c   OUTPUT: kcut    I*2   D1  -  1 = CUTTER/DISPLY,parms command needs to
c                                be processed by the calling routine.
c                                The DISPLY parameter has been removed
c                                from the command.
c
c   INPUT:  knc     I*2   D1  -  Updated number of parameters in command
c                                for CUTTER/DISPLY,dia style command.
c
c           kerr    I*2   D1  -  1 = An error occurred parsing the
c                                command.
c
c***********************************************************************
c
      subroutine cutdis (kcut,knc,kerr)
c
      include 'com8a.com'
      include 'cutter.com'
      include 'mocom.com'
c
      integer*2 kcut,knc,kerr
c
      integer*2 iclw(80)
      real*8 rclw(20)
      character*80 aclw
      equivalence (rclw,iclw), (rclw(4),aclw)
c
      integer*2 i,ierr,is,nval,isym
      integer*4 key,nc,strlen1,cutdsp
c
      real*8 rval(10)
c
      character*80 lsym
c
      integer*2 OFFSET,SHADE,SMOOTH,CUTTER,HOLDER,ON,OFF
      parameter (CUTTER=716)
      parameter (HOLDER=157)
      parameter (OFFSET=666)
      parameter (SHADE=578)
      parameter (SHANK=192)
      parameter (SMOOTH=1085)
      parameter (ON=71)
      parameter (OFF=72)
c
c...Initialize routine
c
      kcut   = 0
      kerr   = 0
      asn    = sc(10)
      np     = knc + 10
c
c...CUTTER/DISPLY,PART
c...              ALL
c
      if (sc(12) .eq. (260-10000) .or. sc(12) .eq. (816-10000)) then
          if (knc .ne. 2) then
              ifl(2) = 4
              goto 9000
          endif
          icutfl(2) = 0
          if (sc(12) .eq. (816-10000)) icutfl(2) = 1
          iclw(1) = 0
          iclw(4) = icutfl(2)
          call putcl (7100,3,2,rclw)
c
c...CUTTER/DISPLY,MOVE
c
      else if (sc(12) .eq. (577-10000)) then
          if (knc .lt. 3 .or. (sc(13) .ne. (71-10000) .and.
     1        sc(13) .ne. (72-10000))) then
              ifl(2) = 56
              goto 9000
          endif
          icutfl(3) = 0
          if (sc(13) .eq. (71-10000)) icutfl(3) = 1
c
c...CUTTER/DISPLY,SHADE
c
      else if (sc(12) .eq. (SHADE-10000)) then
          if (knc .lt. 3 .or. (sc(13) .ne. (71-10000) .and.
     1        sc(13) .ne. (72-10000) .and.
     2        sc(13) .ne. (SMOOTH-10000))) then
              ifl(2) = 56
              goto 9000
          endif
          inc    = 0
          if (sc(13) .eq. (71-10000)) inc = 1
          if (knc .eq. 4) then
              if (sc(14) .eq. (SHANK-10000)) then
                  icutfl(7) = inc
              else if (sc(14) .eq. (HOLDER-10000)) then
                  icutfl(8) = inc
              else if (sc(14) .eq. (CUTTER-10000)) then
                  icutfl(4) = inc
              endif
          else
              icutfl(4) = inc
              icutfl(7) = inc
              icutfl(8) = inc
          endif
c
c...CUTTER/DISPLY,HOLDER
c
      else if (sc(12) .eq. (HOLDER-10000)) then
          is     = 13
          call cuthld (is,2,ierr)
c
c...CUTTER/DISPLY,SHANK
c
      else if (sc(12) .eq. (SHANK-10000)) then
          is     = 13
          call cuthld (is,1,ierr)
c
c...CUTTER/DISPLY,ON
c
      else if (sc(12) .eq. (ON-10000)) then
          cutdsp = 1
          call ncl_set_cutdisp(cutdsp)
c
c...CUTTER/DISPLY,OFF
c
      else if (sc(12) .eq. (OFF-10000)) then
          cutdsp = 0
          call ncl_set_cutdisp(cutdsp)
c
c...CUTTER/DISPLY,symbol
c
      else if (sc(12) .lt. -7000) then
          is     = 12
          call cutprs (is,isym,rval,nval,cutlib,lsym,key,ifl(2))
          if (ifl(2) .ne. 0) go to 9000
c
c......Old style CUTTER/disply,holder command
c
          icut    = 1
          dcutr(19) = 0.
          dcutr(20) = 0.
          if (is .le. np) then
              if (isym .ge. 2 .and. isym .le. 5) then
                  if (sc(is) .eq. OFFSET-10000) then
                      is     = is     + 1
                      if (is+1 .ne. np) go to 9100
                      if (sc(is) .lt. -7000 .or. sc(is+1) .lt. -7000)
     1                        then
                          ifl(2) = 3
                          goto 9000
                      endif
                      dcutr(19) = sc(is)
                      dcutr(20) = sc(is+1)
                  else
                      is     = 12
                      call cuthld (is,2,ierr)
                      if (ierr .ne. 0) go to 9000
                      icut    = 2
                  endif
              else
                  go to 9300
              endif
          endif
c
c......Store clfile record
c
     
          if (icut .eq. 1) then
              cutsym(1) = lsym
              cutkey(1) = key
              icutfl(1) = isym
              call cutrst (isym)
              aclw = cutsym(1)
              iclw(2) = icutfl(1)
              rclw(2) = dcutr(19)
              rclw(3) = dcutr(20)
              nc = strlen1(cutsym(1))
              iclw(1) = nc
              nval = 3 + (nc+7) / 8 + 1
              call putcl (7100,7,nval,rclw)
          endif
c
c......CUTTER/DISPLAY,dia,rad, ...
c
      else
          if (knc .lt. 2) go to 9400
          kcut   = 1
          dcutr(15) = 0
          do 7 i=11,17,1
             sc(i) = sc(i+1)
    7     continue
          knc = knc - 1
          sc(10) = asn
      endif
c
c...End of routine
c
 8000 return
c
c...Error parsing statement
c
 9000 kerr   = 1
      go to 8000
c
c...Incorrect number of parameters
c
 9100 ifl(2) = 4
      kerr   = 1
      go to 8000
c
c...Identifier not previously defined
c
 9200 ifl(2) = 9
      go to 8000
c
c...Invalid cutter definition
c
 9300 ifl(2) = 61
      go to 8000
c
c...Number or scalar expected
c
 9400 ifl(2) = 7
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cuthld (kst,ktyp,kerr)
c
c   FUNCTION:  Parses the CUTTER/DISPLY,HOLDER-SHANK command.
c
c   INPUT:  kst     I*2   D1  -  Position within command to begin parsing.
c
c           ktyp    I*2   D1  -  Defines either 1 = Tool shank or
c                                2 = Tool holder.
c
c   OUTPUT: kerr    I*2   D1  -  1 = An error occurred parsing the
c                                command.
c
c***********************************************************************
c
      subroutine cuthld (kst,ktyp,kerr)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*2 kst,kerr,ktyp
c
      integer*2 ksn(4)
      equivalence (asn,ksn)
c
      integer*2 nval,isym,np,ics,ids
      integer*4 key,nc,strlen1
c
      logical lsav
c
      real*8 rval(10)
c
      character*80 lsym
c
      integer*2 iclw(80)
      real*8 rclw(20)
      character*80 aclw
      equivalence (rclw,iclw), (rclw(6),aclw)

c
      integer*2 CUTTER,HOLDER,OFFSET
      parameter (CUTTER=716)
      parameter (HOLDER=157)
      parameter (OFFSET=666)
c
c...Initialize routine
c
      if (ktyp .eq. 1) then
          ics    = 5
          ids    = 14
          dcutr(18) = 1
      else
          ics    = 6
          ids    = 10
      endif
      asn    = sc(10)
      np     = ksn(3) + 10
      lsav   = lblade
      lblade = .false.
      kerr   = 0
c
c...Get shank shape
c
      call cutprs (kst,isym,rval,nval,cutlib,lsym,key,ifl(2))
      if (ifl(2) .ne. 0) go to 9000
c
c...CUTTER/DISPLY,dia,height
c
      if (isym .eq. 1) then
          if ((lthbit .eq. 0 .and. (nval .lt. 2 .or. nval .gt. 4)) .or.
     2        (lthbit .ne. 0 .and. nval .ne. 3 .and. nval .ne. 4)) then
              ifl(2) = 61
              goto 9000
          endif
          dcutr(ids) = 0.
          dcutr(ids+1) = 0.
          dcutr(ids+2) = 0.
          dcutr(ids+3) = 0.
          call conv8_8 (rval,dcutr(ids),nval)
c
c...CUTTER/DISPLY,symbol
c
      else if (isym .ge. 2) then
          if (kst .le. np) then
              if (sc(kst) .gt. -7000) then
                  dcutr(ids) = sc(kst)
                  kst   = kst   + 1
                  if (kst .le. np .and. sc(kst) .gt. -7000) then
                      dcutr(ids+1) = sc(kst)
                      kst   = kst   + 1
                  endif
              else
                  dcutr(ids) = 0.
                  dcutr(ids+1) = 0.
              endif
          else
              dcutr(ids) = 0.
              dcutr(ids+1) = 0.
          endif
c
c......CUTTER/DISPLY,...,OFFSET,zmin,zmax
c
          if (kst .le. np) then
              if (sc(kst) .eq. OFFSET-10000) then
                  kst   = kst   + 1
                  if (kst+1 .gt. np) goto 9100
                  if (sc(kst) .lt. -7000 .or.
     1                sc(kst+1) .lt. -7000) then
                      ifl(2) = 3
                      goto 9000
                  endif
                  dcutr(ids+2) = sc(kst)
                  dcutr(ids+3) = sc(kst+1)
                  kst    = kst   + 2
              else if (sc(kst) .ne. CUTTER-10000 .and.
     1                 sc(kst) .ne. HOLDER-10000) then
                  ifl(2) = 61
                  goto 9000
              endif
          else
              dcutr(ids+2) = 0.
              dcutr(ids+3) = 0.
          endif
      endif
c
c...CUTTER/DISPLY,SHANK,CUTTER
c...                    HOLDER
c
      if (kst .le. np) then
          if (ktyp .eq. 2) then
              ifl(2) = 61
              go to 9000
          else
              if (sc(kst) .eq. (CUTTER-10000)) then
                  dcutr(18) = 0
              else if (sc(kst) .eq. (HOLDER-10000)) then
                  dcutr(18) = 1
              else
                  ifl(2) = 7
                  go to 9000
              endif
          endif
      endif
      icutfl(ics) = isym
      cutsym(ktyp+1) = lsym
      cutkey(ktyp+1) = key
c
c...Store clfile record
c
      iclw(2) = isym
      if (ktyp .eq. 1) then
          iclw(3) = dcutr(18)
      else
          iclw(3) = 2
      endif
      aclw   = lsym
      call conv8_8 (dcutr(ids),rclw(2),4)
      nc = strlen1(lsym)
      iclw(1) = nc
      nval = 5 + (nc+7) / 8 + 1
c
c......Output record
c
      call putcl (7100,8,nval,rclw)
c
c...End of routine
c
 8000 lblade = lsav
      return
c
c...Error parsing command
c
 9000 kerr   = 1
      go to 8000
c
c...Invalid cutter specification
c
 9100 ifl(2) = 4
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cutprs (kst,ktyp,gval,knval,csymlb,csym,key,kerr)
c
c   FUNCTION:  Parses the CUTTER/DISPLY shape type (parameters, symbol,
c              curve, surface, file).  If a symbol or point list is
c              referenced, then this routine stores the data globally so
c              that it is ready to use.  A point list is defined by a
c              curve, surface, and point list file.
c
c   INPUT:  kst     I*2   D1  -  Position within command to begin parsing.
c
c   OUTPUT: kst     I*2   D1  -  Next position after cutter shape parameters
c                                to continue parsing.
c
c           ktyp    I*2   D1  -  Type of cutter shape defined.  0 = Unknown,
c                                1 = Parameters, 2 = Symbol,
c                                3 = Curve/Surf/Solid,
c                                4 = From external tool file.
c                                5 = Revolved Surf/Solid for lathe tool.
c
c           gval    R*8   Dn  -  Array to receive the parameter values
c                                when 'ktyp' = 1.
c
c           knval   I*2   D1  -  Number of parameters in 'gval'.
c
c           csymlb  C*n   D1  -  Symbol library when 'ktyp' = 2.
c
c           csym    C*n   D1  -  Symbol name when 'ktyp' = 2.  Entity label
c                                when 'ktyp' = 3.
c
c           key     I*4   D1  -  Unibase key of symbol when 'ktyp' = 3.
c                                0 when 'ktyp' != 3.
c
c           kerr    I*2   D1  -  Non-zero = An error occurred parsing the
c                                command.  Contains the appropriate error
c                                number.
c
c***********************************************************************
c
      subroutine cutprs (kst,ktyp,gval,knval,csymlb,csym,key,kerr)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*2 kst,ktyp,kerr,knval
      integer*4 key
c
      real*8 gval(*)
c
      character*(*) csymlb,csym
c
      integer*2 ksn(4)
      equivalence (asn,ksn)
c
      integer*2 i,np,idata(1680),il,len
      integer*4 isfl,nc,strlen1,itsk,istyp,ictyp,npts,jdata(840),
     1          iary(3),styp,nparms,nc1,nc2
c
      real*8 pts(500),vecs(500),rdata(420),params(20)
c
      character*80 msym,ldata
      character*256 tstr1, tstr2,txt,fulldir
c
      equivalence (rdata,idata,jdata), (rdata(2),ldata)
c
c...Initialize routine
c
      ktyp   = 0
      knval  = 0
      asn    = sc(10)
      np     = ksn(3) + 10
      key    = 0
      kerr   = 0
      ictyp  = 1
      if (lthbit .ne. 0 .or. lblade) ictyp = 2
c
c...Allow symbol library with solid,curve
c...since it can be output from CUTTER/TOOL statement
c
      if (sc(kst) .eq. -10000 .and. sc(kst+1) .lt. -10000 .and.
     1    ictent .gt. 0) kst = kst + 1
c
c...Determine type of cutter shape
c......CUTTER/DISPLY,cv
c
      if (sc(kst) .eq. -10008 .or. sc(kst) .eq. -10005 .or.
     1         sc(kst) .eq. -10007 .or. sc(kst) .eq. -10023) then
          if (np .gt. kst .and. sc(kst+1) .eq. -10021) then
              nc     = strlen1(lsc(kst-10))
              msym   = lsc(kst-10)
              csym   = lsc(kst-10)(1:nc) // '&' // lsc(kst-9)
              kst    = kst    + 2
              ktyp   = 3
          else
              msym   = lsc(kst-10)
              csym   = lsc(kst-10)
              kst    = kst    + 1
              ktyp   = 3
          endif
          call getkey (msym,key)
c
c......CUTTER/DISPLY,sf
c......Make sure it is a surface of revolution
c
      else if (sc(kst) .eq. -10009) then
          csym   = lsc(kst-10)
          kst    = kst    + 1
          ktyp   = 3
          call nclf_is_revsf (csym,isfl)
          if (isfl .ne. 1) go to 9100
          call getkey (csym,key)
          if (ictyp .eq. 2) ktyp = 5
c
c......CUTTER/DISPLY,solid
c
      else if (sc(kst) .eq. -10033) then
          csym   = lsc(kst-10)
          kst    = kst    + 1
          ktyp   = 3
          call getkey (csym,key)
          call nclf_get_solid (key,styp,params,nparms)
          if (ictyp .eq. 1 .and. (styp .eq. 1 .or. styp .eq. 6 .or.
     1        styp .eq. 7)) go to 9200
          if (ictyp .eq. 2 .and. (styp .eq. 2 .or. styp .eq. 3 .or.
     2        styp .eq. 4 .or. styp .eq. 5 .or. styp .eq. 8)) ktyp = 5
c
c...CUTTER/DISPLY,params
c
      else if (sc(kst) .gt. -7000) then
          do 500 i=kst,np,1
              if (sc(i) .gt. -7000) then
                  knval  = knval  + 1
                  gval(knval) = sc(i)
              else
                  go to 590
              endif
  500     continue
  590     kst    = kst    + knval
          ktyp   = 1
          csym   = ' '
c
c......CUTTER/DISPLY,symlib,symbol
c
      else
          if (np .gt. kst .and. sc(kst+1) .eq. -10000) then
              csymlb = lsc(kst-10)
              if (csymlb(78:79).eq.'&&') then
                  il = kst-10
                  call nclf_getlsc(il, txt, len) 
                  txt = txt(1:len) // '_S'
                  len = len + 2
                  call ulf_get_full_dir(txt,fulldir,len)
                  csymlb = fulldir
              endif
              csym   = lsc(kst-9)
              kst    = kst    + 1
              ktyp   = 2
c
c......CUTTER/DISPLY,symbol
c
          else
              csymlb = cutlib
              csym   = lsc(kst-10)
          endif
c
c......Verify cutter symbol exists
c
c
c.....if csym have a path, replace the csymlb path, the csym should only
c.....have a label, no path
c
          nc = strlen1(csym)
          call ulf_break_fname(csym,nc,tstr1,nc1,tstr2,nc2)
          if (nc1.gt.1) then
              csymlb = tstr1
              csym = tstr2
          endif
          call ldcsym (csym,icutfl(4),ictyp,ierr)
          if (ierr .eq. 0) then
              kst    = kst    + 1
              ktyp   = 2
c
c......Not a valid symbol
c......CUTTER/DISPLY,tool
c
          else
              itsk   = 0
              call nclf_load_cutprof (csym,ierr)
              if (ierr .ne. 0) go to 9000
              kst    = kst   + 1
              ktyp   = 4
          endif
      endif
c
c...Create cutter geometry
c
      if (ktyp .ne. 1) then
          istyp  = ktyp
          iary(1) = icutfl(4)
          iary(2) = icutfl(7)
          iary(3) = icutfl(8)
          call nclf_cutter_get_cutsym(csym,key,iary,istyp,ictyp,
     1                                pts,vecs,npts)
          ktyp   = istyp
c
c......Store geometry in clfile
c
          if (npts .ne. 0) then
              ldata  = csym
              nc = strlen1(csym)
              idata(1) = nc
              jdata(2) = npts
              idata(2) = ictyp
              inc    = 1 + (nc+7) / 8
              inc1   = 0
              if (ictyp .eq. 2) inc1 = 1
              do 800 i=1,npts,1
                  if (inc .eq. 420) then
                      call putcl (7120,2,421,rdata)
                      inc    = 0
                  endif
                  rdata(inc+1) = pts(i*3-2)
                  rdata(inc+2) = pts(i*3-inc1)
                  inc    = inc    + 2
                  rdata(inc+1) = vecs(i*3-2)
                  rdata(inc+2) = vecs(i*3-inc1)
                  inc    = inc    + 2
  800         continue
              if (inc .ne. 0) call putcl (7120,1,inc+1,rdata)
          endif
      endif
c
c...End of routine
c
 8000 return
c
c...Could not determine cutter display type
c
 9000 kerr   = 9
      go to 8000
c
c...Surface of revolution expected
c
 9100 kerr   = 480
      go to 8000
c
c...Invalid solid for cutter part
c
 9200 kerr   = 533
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cutrst (ksym)
c
c   FUNCTION:  Resets certain CUTTER variables when processing a new
c              cutter statement, for example the SHANK and HOLDER variables
c              are reset to not be displayed.
c
c   INPUT:  ksym    I*2   D1  -  Type of cutter being defined.
c                                0 = Cutter Parameters, 1 = Disply Parameters,
c                                2 = Symbol, 3 = CV/SF, 4 = Point-list,
c                                5 = Revolved SF/SO for lathe tool.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine cutrst (ksym)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*2 ksym
c
c...Set appropriate cutter flags when
c...New cutter definition is processed
c......Set cutter display type
c......Reset cutter symbol if parameters
c......Reset SHANK and HOLDER display
c
      icutfl(1) = ksym
      icutfl(5) = 0
      icutfl(6) = 0
      if (ksym .lt. 2) then
          cutsym(1) = ' '
          cutkey(1) = 0
      endif
      cutsym(2) = ' '
      cutkey(2) = 0
      cutsym(3) = ' '
      cutkey(3) = 0
c
c...End of routine
c
 8000 return
      end

c***********************************************************************
c
c   SUBROUTINE:  gcutlb (clib)
c
c   FUNCTION:  Interface to C routines which returns the name of the
c              cutter symbol library.
c
c   INPUT:  none.
c
c   OUTPUT: clib    C*20 D1  -  Name of cutter symbol library.  Usually
c                               "symlib".
c
c***********************************************************************
c
      subroutine gcutlb (clib)
c
      include 'com.com'
      include 'cutter.com'
c
      character*256 clib
c
      clib = cutlib
      return
      end


c
c***********************************************************************
c
c   SUBROUTINE:  cutset_flag (cutfl)
c
c   FUNCTION:  Interface to C routines which set the cutter display flag
c              
c   INPUT:  cutfl: flag to be set.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cutset_flag (cutfl)
c
      include 'com.com'
      include 'cutter.com'

      integer*4 cutfl(10)

      do 100 i=1,10,1
          icutfl(i) = cutfl(i) 
  100 continue
      return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  cutget_flag (cutfl)
c
c   FUNCTION:  Interface to C routines which get the cutter display flag
c              
c   INPUT:  none
c
c   OUTPUT: cutfl: flag to be get.
c
c***********************************************************************
c
      subroutine cutget_flag (cutfl)
c
      include 'com.com'
      include 'cutter.com'

      integer*4 cutfl(10)

      do 100 i=1,10,1
          cutfl(i) = icutfl(i) 
  100 continue
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  scutlb (clib)
c
c   FUNCTION:  Interface to C routines which set the name of the
c              cutter symbol library.
c
c   INPUT:  clib. C*256 D1  -  Name of cutter symbol library.  Usually
c                               "symlib".
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine scutlb (clib)
c
      include 'com.com'
      include 'cutter.com'
c
      character*256 clib
c
      cutlib = clib
      return
      end
