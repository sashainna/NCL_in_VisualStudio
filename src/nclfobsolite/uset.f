C*********************************************************************
C*    NAME         :  uset.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*        uset.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:51
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine uset (set)
C*       description
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine uset (set)

      include 'com8a.com'
      include 'const.com'
      include 'suvcom.com'

      real*4 tapmax,tapnow,tapeq(2)
      equivalence (tapmax,sc(55)),(tapnow,tapeq(2)),(sc(55),tapeq(1))
      equivalence (iquit,quit)
      character*6 tkn,qimage,tknsv
      character*2 tkn2
      equivalence (tkn2,token2)
      character*1 aimage(80)
      integer*2 indtyp, indall, indsep
      equivalence (indall, ifl(230)), (indsep, ifl(231))
      integer*2 ifini(2)
      integer*2 iset
      integer*4 i4fini(2),nup,nupt,nvp,nvpt,intv
      equivalence (token2,tkn),(aimage,cimage),(qimage,cin)
      equivalence (fini,ifini,i4fini)
      logical set
      logical lpt,lve,lln,lci,lcv,lsf,lsh,lpl,lpn,lpv,lmx,lan,lsy,lso
c     temporary storage
      integer*4 itemp(5),lo
      real*4 hldu
      real*8 r, r1, r2
      integer*4 nclkey,choice
      integer*2 nwds,ietype,inoaut
      integer*2 DEFALT, OFF, ON, BASE, FACE, OMIT, RETAIN
      parameter (DEFALT=903,OFF=72,ON=71,BASE=868,FACE=81)
      parameter (OMIT=172,RETAIN=329)

      iset=0
      if (set) iset=1

      if (.not.(tkn.eq.'AUTOST'.and.length.eq.6)) go to 199
        if (nextyp.eq.11) then
          sc(91) = sc(27)*2.0d0
          sc(92) = dcos(1.0d0/RADIAN)
          ifl(363) = 0
        else
          r1     = sc(91)
          r2     = sc(92)
          inoaut = ifl(363)
          ifl(44) = 0
          call parsit
          if (ityp.ne.5 .or. ist.ne.9) then
            if (.not.scalar) goto 9053
            r1 = tv
            if (r1.lt.1.d-6) r1 = 1.d-6
            if (nextyp.ne.11) call parsit
          endif
          if (nextyp.ne.11) then
            call parsit
            if (ityp.ne.5 .or. ist.ne.9) then
              if (.not.scalar) goto 9053
              if (tv.lt..1) tv = .1
              r2 = dcos(tv/RADIAN)
              if (nextyp.ne.11) call parsit
            endif
          endif
          if (nextyp.ne.11) then
            call parsit
            if (ityp.ne.5 .or. ist.ne.9) then
              if (.not.vocab.or.ist.ne.OMIT.and.ist.ne.RETAIN)goto 9053
              inoaut = 0
              if (ist.eq.OMIT) inoaut = 1
            endif
          endif
          if (nextyp.ne.11) goto 9004
          sc(91)   = r1
          sc(92)   = r2
          ifl(363) = inoaut
        endif
        autost=set
        go to 2299

c          *autol1 controls the display of part program statements
c          to line 1 of the terminal during interactive processing
199   if (.not.(tkn .eq. 'AUTOL1' .and. length .eq. 6)) go to 219
          if (nextyp .eq. 11) then
              autol1 = set
          else
              go to 44444
          endif
          go to 2299
c
c...Added check for NCL-VT mode
c...Paul  -  11/04/91
c
219   if (.not.(tkn .eq. 'ECHO' .and. length .eq. 4)) go to 299
          if (nextyp .eq. 11) then
              echo = set
          else
              go to 44444
          endif  
          go to 2299


299   if (.not.(tkn.eq.'APTCOM'.and.length.eq.6)) go to 399
          if (nextyp.eq.11) then
              aptcom=set
          else
              go to 44444
          endif
          go to 2299
399   if (.not.(tkn.eq.'DEBUG'.and.length.eq.5)) go to 499
          if (nextyp.eq.11) then
              ifl(125)=0
              debug = set
          else
              go to 44444
          endif
          go to 2299
499   if (.not.(tkn.eq.'KSR'.and.length.eq.3)) go to 599
          if (nextyp.eq.11) then
              ksr = set
          else
              go to 44444
          endif
          go to 2299
599   if (.not.(tkn.eq.'MOTION'.and.length.eq.6)) go to 699
          if (nextyp.eq.11) then
              motdfl=set
          else
              go to 44444
          endif
          go to 2299
699   if (.not.((tkn.eq.'ELIMIT'.or.tkn.eq.'WLIMIT').and.
     1          length.eq.6)) go to 799
          if (.not. set) then
              ifl(2)=25
              go to 99999
          endif
          i=32
          if (tkn.eq.'ELIMIT') i=31
          call parsit
          if (nextyp.eq.11) then
              if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.
     1             ityp.eq.4) then
                  ifl(i)=itv
              else
                  ifl(2)=53
                  go to 99999
              endif
          else
              go to 44444
          endif
          go to 2299
799   if (.not.(tkn.eq.'DEBUGM'.and.length.eq.6)) go to 899
          if (nextyp.eq.11) then
              if (set) then
                   ifl(85) = 1
              else
                   ifl(85) = 0
              endif
          else
              go to 44444
          endif
          go to 2299
899   if (.not.(tkn.eq.'DEBUGG'.and.length.eq.6)) go to 999
          if (nextyp.eq.11) then
              geodsp=set
          else
              go to 44444
          endif
          go to 2299
999   if (.not.(tkn.eq.'PLOT'.and.length.eq.4)) go to 1099
          call setplt(set)
          go to 2299
c                                                          ***  screen
1099  if (.not.(tkn.eq.'SCREEN'.and.length.eq.6)) go to 1199
          call parsit
          if (nextyp.ne.11) go to 44444
          if (tkn.eq.'NORMAL'.and.length.eq.6) then
 
c                      force domode to write line 5
              ifl(48)=0
              ifl(131)=0
c
              if (ifl(35).eq.0) call putmsg('***************************
     1  INTERACTIVE PROCESSING  ***************************'
     2            ,80,14,1)
c 
c...Added check for NCL-VT mode
c...Paul  -  10/3/91 
c...Old version was: 
c   there was no old version. 
c 
c 
c...VX
c
              if (ifl(35).eq.2 .and. ifl(322) .eq. 0) then
                 ifl(303) = 2
                 call statln                             
              endif
c
          else if (tkn.eq.'GRAPH'.and.length.eq.5) then
c 
c...Added check for NCL-VT mode 
c...Paul  -  10/3/91  
c...Old version was:  
c   if (ifl(35).eq.0) call ersw3 (5,1)
c
              if (ifl(35).eq.0 .or. ifl(35) .eq. 2) call ersw3 (5,1)
              ifl(131)=1
          else
              call error (220)
          endif
          go to 2299
 
c                                                          ***  verify
1199  continue
c1199  if (.not.(tkn .eq. 'VERIFY' .and. length .eq. 6)) go to 1299
c          if (nextyp .ne. 11) go to 44444
c          ifl(191) = iset
c          go to 2299
 
c                                                          ***  indent
1299  if (.not.(tkn .eq. 'INDENT' .and. length .eq. 6)) go to 1399
          if (.not. set) then
              if (nextyp .ne. 11) then
                  go to 44444
              else
                  indall = 0
                  indsep = 0
                  ifl(360) = 0
                  go to 7100
              endif
          endif
          call parsit
          if (tkn .eq. 'ALL   ') then
              indtyp = 1
          else if (tkn .eq. 'SEP   ') then
              indtyp = 2
          else if (tkn .eq. 'OFF   ') then
              if (nextyp .eq. 11) then
                  indall = 0
                  indsep = 0
                  ifl(360) = 0
                  go to 7100
              else
                  go to 44444
              endif
          endif
          if (nextyp .ne. 11) then
              call parsit
              if ((ityp .eq. 2 .and. ist .eq. 2) .or.
     x             ityp .eq. 3 .or.
     x             ityp .eq. 4) then
                  if (nextyp .eq. 11) then
                      if (indtyp .eq. 1) indall = tv
                      if (indtyp .eq. 2) indsep = tv
                      ifl(360) = 1
                  else
                      go to 44444
                  endif
              else
                  ifl(2)=7
              endif
          else
              ifl(2) = 7
          endif
7100      continue
          go to 2299

c                                                          ***  tapeft
1399  if (.not.(tkn.eq.'TAPEFT'.and.length.eq.6)) go to 1499
          if (set) then
              call parsit
               if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.
     1             ityp.eq.4) then
                  if (nextyp.eq.11) then
                      tapmax=tv
                      ifl(99)=1
                  else
                      go to 44444
                  endif
              else
                  ifl(2)=7
              endif
          else
              tapnow=0
              ifl(99)=0
          endif
          go to 2299
c   added the flag (ifl=277) for patern on: 3/15/88 by: kathy
c   also added a call to a C routine called namset to set the
c   geometry name to start at itv number.
c
1499  if (.not.((ityp.eq.1.) .and. ((ist.gt.602.and.ist.lt.611).or.
     1    ist .eq. 636 .or. ist .eq. 616 .or. ist .eq. 123 .or.
     2    ist .eq. 602 .or. ist .eq. 877 .or.ist .eq. 613))) go to 1599
          if (ist.gt.602.and.ist.lt.611) then
            idst=ist-600
          else if (ist.eq.636) then
            idst=20
          else if (ist .eq. 616) then
            idst = VANOTE
          else if (ist .eq. 123) then
            idst = VSOLID
          else if (ist .eq. 602) then
            idst = SHAPE
          else if (ist .eq. 877) then
            idst = VSYMBOL
          else if (ist .eq. 613) then
            idst = PNTVEC
          endif
          call parsit
          if (ityp.ne.3) then
              ifl(2)=53
              go to 99999
          else if (nextyp.ne.11) then
              go to 44444
          endif
          if (tv .gt. 999999) then
              ifl(2) = 381
              go to 99999
          endif
          intv = itv
          call namset(idst,intv)
          if (idst.gt. 2 .and. idst .lt. 11) then
              ifl(10+idst)=itv-1
          else if (idst .eq. PATERN) then
              ifl(277)=itv-1
          else if (idst .eq. VANOTE) then
              ifl(373) = itv - 1
          else if (idst .eq. VSOLID) then
              ifl(382) = itv - 1
          else if (idst .eq. SHAPE) then
              ifl(143) = itv - 1
          else if (idst .eq. VSYMBOL) then
              ifl(380) = itv - 1
          else if (idst .eq. PNTVEC) then
              ifl(309) = itv - 1
          endif
          go to 2299
1599  if (.not.((tkn.eq.'CANON'.and.length.eq.5) .or.
     1         (tkn2.eq.'CA'.and.length.eq.2))) go to 1699
          if (set) then
              ifl(103)=ifl(41)
              ifl(41)=1
          else
              ifl(41)=ifl(103)
          endif
          go to 2299
1699  if (.not.((tkn.eq.'NOWARN'.and.length.eq.6) .or.
     1         (tkn2.eq.'NW'.and.length.eq.2))) go to 1799
          if (nextyp.eq.11) then
              nowarn=set
          else
              go to 44444
          endif
          go to 2299

1799  if (.not.((tkn.eq.'ADISPL'.and.length.eq.6) .or.
     x         (tkn2.eq.'AD'.and.length.eq.2))) go to 1899
          lo  = 3
          if (sc(169) .gt. 8.3999999) lo = 0 
          if (nextyp.eq.11) then
              if (set) then
                  ifl(136)=50
c                 ifl(137)=10
c                 ifl(138)=10
                  call gtsfdp (nup,nupt,nvp,nvpt)
                  ifl(184)=1
              else
c                     ifl(184) is implemented as the adisply switch.
c                     0=off, 1=on.   epm  5-21-84
                  ifl(184)=0
              endif
          else if (set) then
              isv = INX              
              call parsit
c
c.../ADISPL,TOLER,d
c
              if (ityp .eq. 1 .and. ist .eq. 731) then
                  call parsit
                  if (SCALAR) then
                     r = TV
                     if (ifl(264) .eq. 1) r = r/25.4
                     if (r .lt. .0001) then
                        ifl(2) = 464
                        go to 99999
                     else if (r .gt. .5) then
                        ifl(2) = 358
                        go to 99999
                     else
                        if (.not.nxteos) goto 9004
                        sc(175) = r
                        go to 99999
                     end if
                  else
                     ifl(2) = 7
                     go to 99999       
                  end if 
              else
                  INX = isv
              end if
c
c.../ADISPL,n1,n2,...
c
              do 505 i=1,5
                  call parsit
                  if ((ityp.eq.2.and.ist.eq.1.and.itv.ge.0) .or.
     x                (ityp.eq.3.and.itv.ge.0)) then
                      itemp(i)=itv
                  else 
                      ifl(2)=224
                      go to 99999
                  endif
                  if (nextyp .eq. 11) go to 520
  505         continue
c
c...Added the bound check on: 3/14/88 by: kathy
c 
  520         if (i .eq. 1 .or. i .eq. 3 .or. i .eq. 5) then
                 if (itemp(1).lt.lo) then
                     ifl(2)=356
                     go to 99999
                 else if (itemp(1).gt.500) then
                     ifl(2)=357
                     go to 99999
                 else
                     ifl(136)=itemp(1)
                 endif
              end if
              if (i .gt. 1) then
                 nx    = 0
                 if (i .eq. 3 .or. i .eq. 5) nx = 1 
c
c......Separate the logic for *SET/ADISPL,u,v & up,ul,vp,vl
c......Bobby  -  5/28/96
c
                 if (i .le. 3) then
                     if (itemp(nx+1).lt.2) then
                         ifl(2)=358
                         go to 99999
                     end if
                     if (itemp(nx+1).gt.500.or.itemp(nx+2).gt.500) then
                         ifl(2)=359
                         go to 99999
                     else
                         nup  = itemp(nx+1)
                         nvp  = itemp(nx+2)
                         nvpt = itemp(nx+1)
                         nupt = itemp(nx+2)
                         if (sc(169) .gt. 8.3999999) then
                            nvpt = 0
                            nupt = 0
                         end if
                     end if
                 else
                     if (itemp(nx+2).lt.2.or.itemp(nx+4).lt.2 .or.
     -                   itemp(nx+1).lt.lo.or.itemp(nx+3).lt.lo) then
                        ifl(2)=358
                        go to 99999
                     end if
                     if (itemp(nx+1).gt.500.or.itemp(nx+2).gt.500 .or.
     -                   itemp(nx+3).gt.500.or.itemp(nx+4).gt.500) then
                        ifl(2)=359
                        go to 99999
                     else
                        nupt = itemp(nx+3)
                        nup  = itemp(nx+4)
                        nvpt = itemp(nx+1)
                        nvp  = itemp(nx+2)
                     end if
                 endif
                 call ptsfdp (nup,nupt,nvp,nvpt)
                 call pttess (nupt,nvpt)
                 ifl(184)=1
              end if
              if (nextyp.ne.11) ifl(2)=4
          else
              ifl(2)=4
          endif
          go to 2299
1899  if (.not.(tkn.eq.'LABEL'.and.length.eq.5)) go to 1999
          if (nextyp.eq.11) then
              lablpt = set
              lablve = set
              lablln = set
              lablpl = set
              lablci = set
              lablcv = set
              lablsf = set
              lablsh = set
              lablpn = set
              lablpv = set
              lablmx = set
              lablan = set
              lablsy = set
              lablso = set
          else
            lpt=.false.
            lve=.false.
            lln=.false.
            lci=.false.
            lcv=.false.
            lsf=.false.
            lsh=.false.
            lpl=.false.
            lpn=.false.
            lpv=.false.
            lmx=.false.
            lan=.false.
            lsy=.false.
            lso=.false.
            ifl(44)=9

5010        if (nextyp.ne.9) then
              ifl(2)=57
              goto 99999
            endif
            call parsit
            if (ist.eq.603) then
              lpt=.true.
            else if (ist.eq.604) then
              lve=.true.
            else if (ist.eq.605) then
              lln=.true.
            else if (ist.eq.607) then
              lci=.true.
            else if (ist.eq.608) then
              lcv=.true.
            else if (ist.eq.609) then
              lsf=.true.
            else if (ist.eq.602) then
              lsh=.true.
            else if (ist.eq.606) then
              lpl=.true.
            else if (ist.eq.636) then
              lpn=.true.
            else if (ist.eq.610) then
              lmx=.true.
            else if (ist.eq.616) then
              lan=.true.
            else if (ist.eq.877) then
              lsy=.true.
            else if (ist.eq.123) then
              lso=.true.
            else if (ist.eq.816) then
              lpt=.true.
              lve=.true.
              lln=.true.
              lci=.true.
              lcv=.true.
              lsf=.true.
              lsh=.true.
              lpl=.true.
              lpn=.true.
              lmx=.true.
              lan=.true.
              lsy=.true.
              lso=.true.
c
c...Added by Paul for *SET/LABEL statements.
c
            else if(ist .eq.  732) then
              if (set)       choice = 1
              if (.not. set) choice = 2
              call labset(choice)
            else if(ist .eq. 951) then
              call labalt
            else if(ist .eq. 903) then
              call labdef
c
            else
              ifl(2)=1
              goto 99999
            endif
            if (nextyp.ne.11) goto 5010
            if (lpt) lablpt=set
            if (lve) lablve=set
            if (lln) lablln=set
            if (lci) lablci=set
            if (lcv) lablcv=set
            if (lsf) lablsf=set
            if (lsh) lablsh=set
            if (lpl) lablpl=set
            if (lpn) lablpn=set
            if (lpv) lablpv=set
            if (lmx) lablmx=set
            if (lan) lablan=set
            if (lsy) lablsy=set
            if (lso) lablso=set
          endif
          go to 2299
1999  if (.not.(tkn.eq.'DISPLY')) go to 2099
          if (nextyp.eq.11) then
            dsplpt=set
            dsplve=set
            dsplln=set
            dsplci=set
            dsplcv=set
            ldspsf=set
            ldspsh=set
            dsplpl=set
            dsplmx=set
            ldsppn=set
            ldsppv=set
            dsplan=set
            dsplsy=set
            dsplso=set
          else
            lpt=.false.
            lve=.false.
            lln=.false.
            lci=.false.
            lcv=.false.
            lsf=.false.
            lsh=.false.
            lpl=.false.
            lpn=.false.
            lpv=.false.
            lmx=.false.
            lan=.false.
            lsy=.false.
            lso=.false.
            ifl(44)=9
5110        if (nextyp.ne.9) then
              ifl(2)=57
              goto 99999
            endif
            call parsit
            if (ist.eq.603) then
              lpt=.true.
            else if (ist.eq.604) then
              lve=.true.
            else if (ist.eq.605) then
              lln=.true.
            else if (ist.eq.607) then
              lci=.true.
            else if (ist.eq.608) then
              lcv=.true.
            else if (ist.eq.609) then
              lsf=.true.
            else if (ist.eq.602) then
              lsh=.true.
            else if (ist.eq.606) then
              lpl=.true.
            else if (ist.eq.636) then
              lpn=.true.
            else if (ist.eq.613) then
              lpv=.true.
            else if (ist.eq.610) then
              lmx=.true.
            else if (ist.eq.616) then
              lan=.true.
            else if (ist.eq.817) then
              lsy=.true.
            else if (ist.eq.123) then
              lso=.true.
            else
              ifl(2)=1
              goto 99999
            endif
            if (nextyp.ne.11) goto 5110
            if (lpt) dsplpt=set
            if (lve) dsplve=set
            if (lln) dsplln=set
            if (lci) dsplci=set
            if (lcv) dsplcv=set
            if (lsf) ldspsf=set
            if (lsh) ldspsh=set
            if (lpl) dsplpl=set
            if (lpn) ldsppn=set
            if (lpv) ldsppv=set
            if (lmx) dsplmx=set
            if (lan) dsplan=set
            if (lsy) dsplsy=set
            if (lso) dsplso=set
          endif
          go to 2299
2099  if (.not.(tkn.eq.'CIRAPT')) go to 2149
      if (nextyp.ne.11) goto 44444
          ifl(261)=iset
      go to 2299
2149  if (.not.(tkn.eq.'STATLN')) go to 2199
      if (nextyp.ne.11) goto 44444
c
c...Update the status line
c...for *SET/STATLN just in case it changed
c...while it was turned off
c...Bobby  -  3/16/92
c
          if (iset .eq. 0) then
              ifl(303) = 1
          else
              ifl(303) = 0
              ifl(301) = 0
          endif
          go to 2299
c
c... Add APTSRC Control Command with one of four arguments
c... 1 - CIRCUL same as CIRAPT
c... 2 - CUTTER specifying APT, NCL or PPRINT format
c...     Added precision setting to CUTTER command
c... 3 - REMARK same as APTCOM
c... 4 - VERIFY to indicate Standard or Vericut circular statements
c... Sharon - 05Aug91
c
2199  if (.not.(tkn.eq.'APTSRC'.and.length.eq.6)) go to 2249
      if (nextyp.eq.11) goto 9034
2210  call parsit
      ifl(390) = 0
      if (tkn .eq. 'CIRCUL' .and. length .eq. 6) then
          ifl(261) = iset
      else if (tkn .eq. 'CUTTER' .and. length .eq. 6) then
          if (nextyp.ne.11) then
              call parsit
              if (tkn.eq.'APT'.and.length.eq.3) then
                 ifl(307) = 0
              else if (tkn.eq.'NCL'.and.length.eq.3) then
                 ifl(307) = 1
              else if (tkn.eq.'PPRINT'.and.length.eq.6) then
                 ifl(307) = 2
c
c... Added for precision setting. Andrew - 11Jul12                 
c
              else if (ityp.eq.3) then
                 if (tv.lt.0. .or.tv.gt.8.) goto 9445
                 ifl(390) = int(tv)
              else
                 goto 9034
              endif
          else
              goto 9034
          endif
      else if (tkn .eq. 'REMARK' .and. length .eq. 6) then
          if (nextyp .eq. 11) then
              aptcom=set
          else
              call parsit
              if (tkn .eq. 'ACTIVE' .and. length .eq. 6) then
                  aptrem = set
              else
                  goto 9034
              endif
          endif
      else if (tkn .eq. 'DATA' .and. length .eq. 4) then
          aptmark=set
      else if (tkn .eq. 'VERIFY' .and. length .eq. 6) then
          ifl(308)=iset
      else if (tkn .eq. 'IPV' .and. length .eq. 3) then
          ifl(308)=iset
          if (set) ifl(308) = 2
      else if (tkn .eq. 'IPVCOM' .and. length .eq. 6) then
          ipvcom=set
      else if (tkn .eq. 'LOW' .and. length .eq. 3) then
          ifl(319) = 0
      else if (tkn .eq. 'HIGH' .and. length .eq. 4) then
          ifl(319) = 1
      else if (tkn .eq. 'TRACUT' .and. length .eq. 6) then
          ifl(353) = iset
          if (set) then
              if (nextyp .ne. 11) then
                  call parsit
                  if (ityp .ne. 2 .or. ist .ne. 10) goto 9094
                  call gtentt (tv,0,nclkey,ietype,sc(183))
              else if (sc(183) .eq. 0. .and.  sc(184) .eq. 0. .and.
     1                 sc(185) .eq. 0.) then
                  goto 9094
              endif
          endif
      else if (tkn .eq. 'REAL' .and. length .eq. 4) then
          ifl(386) = iset
      else
          goto 9025
      endif
c
c......SET/APTSRC,CIRCUL,PLANAR
c......           ...    ALL
c
      if (tkn .eq. 'CIRCUL' .or. tkn .eq. 'VERIFY' .or. tkn .eq. 'IPV')
     1        then
          if (nextyp .ne. 11) then
              call parsit
              if (tkn .eq. 'PLANAR') then
                  ifl(371) = 0
              else if (tkn .eq. 'ALL') then
                  ifl(371) = 1
              else
                  go to 9034
              endif
          endif
      endif
      if (nextyp .ne. 11) go to 2210
      go to 2299
c
c...*RESET/CALL
c...Bobby  -  6/28/91
c
2249  continue
      if (tkn .eq. 'CALL') then
          if (set) then
              ifl(2)=25
              go to 99999
          endif
          call parsit
          if (ityp .eq. 7) then
              i = 1000
          else if (nextyp.eq.11) then
              if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.
     1             ityp.eq.4) then
                  i = itv
              else
                  ifl(2)=53
                  go to 99999
              endif
          else
              go to 44444
          endif
          if (i .le. 0) then
              call error (37)
              go to 2299
          endif
          call calrst (i)
          go to 2299
      endif
c
      if (tkn.eq.'LFL') then
        call parsit
        if (nextyp.ne.11) goto 44444
        if (.not.scalar) goto 9053
c
c aak 25-nov-1997:
c        if (itv.gt.100) itv=100
c
        if (itv.gt.200) itv=200
        if (itv.lt.1) itv=1
        lfl(itv)=set
        goto 99999
      endif
      if (tkn.eq.'PSUV') then
        call parsit
        if (token2(1:6).eq.'DSUV') then
          if (.not.nxteos) goto 44444
          psu=dsu
          psv=dsv
          psuv=.true.
          goto 99999
        endif
        if (token2(1:6).eq.'CSUV') then
          if (.not.nxteos) goto 44444
          psu=csu
          psv=csv
          psuv=.true.
          goto 99999
        endif
        if (ityp.eq.5.and.ist.eq.9) then
          tv=.5
        else if (.not.scalar) then
          goto 9053
        endif
        if (tv.gt.1.) tv=1.
        if (tv.lt.0.) tv=0.
        hldu = tv
        if (nxteos) then
          tv=.5
        else
          call parsit
          if (.not.nxteos) goto 44444
          if (.not.scalar) goto 9053
        endif
        if (tv.gt.1.) tv=1.
        if (tv.lt.0.) tv=0.
        psu = hldu
        psv = tv
        psuv=.true.
        goto 99999
      endif
      if (tkn.eq.'DSUV') then
        call parsit
        if (token2(1:6).eq.'PSUV') then
          if (.not.nxteos) goto 44444
          dsu=psu
          dsv=psv
          dsuv=.true.
          goto 99999
        endif
        if (token2(1:6).eq.'CSUV') then
          if (.not.nxteos) goto 44444
          dsu=csu
          dsv=csv
          dsuv=.true.
          goto 99999
        endif
        if (ityp.eq.5.and.ist.eq.9) then
          tv=.5
        else if (.not.scalar) then
          goto 9053
        endif
        if (tv.gt.1.) tv=1.
        if (tv.lt.0.) tv=0.
        hldu = tv
        if (nxteos) then
          tv=.5
        else
          call parsit
          if (.not.nxteos) goto 44444
          if (.not.scalar) goto 9053
        endif
        if (tv.gt.1.) tv=1.
        if (tv.lt.0.) tv=0.
        dsuv=.true.
        dsu = hldu
        dsv = tv
        goto 99999
      endif
      if (tkn.eq.'CSUV') then
        call parsit
        if (token2(1:6).eq.'PSUV') then
          if (.not.nxteos) goto 44444
          csu=psu
          csv=psv
          csuv=.true.
          goto 99999
        endif
        if (token2(1:6).eq.'DSUV') then
          if (.not.nxteos) goto 44444
          csu=dsu
          csv=dsv
          csuv=.true.
          goto 99999
        endif
        if (ityp.eq.5.and.ist.eq.9) then
          tv=.5
        else if (.not.scalar) then
          goto 9053
        endif
        if (tv.gt.1.) tv=1.
        if (tv.lt.0.) tv=0.
        hldu = tv
        if (nxteos) then
          tv=.5
        else
          call parsit
          if (.not.scalar) goto 9053
          if (.not.nxteos) goto 44444
        endif
        if (tv.gt.1.) tv=1.
        if (tv.lt.0.) tv=0.
        csuv=.true.
        csu = hldu
        csv = tv
        goto 99999
      endif
      if (tkn.eq.'CLOSED') then
        call parsit
        if (ityp.ne.2.or.ist.ne.8.and.ist.ne.9) goto 9025
        call gtdesc(tv,nclkey,nwds,ietype)
        iflg = 0
        if (ietype.eq.9) then
          call parsit
          if (.not.scalar) goto 9053
          if (itv.ne.0.and.itv.ne.1) goto 9170
          iflg = itv
        endif
        if (.not.nxteos) goto 44444
        call ptclsd (nclkey,iflg,iset)
        goto 99999
      endif
      if (tkn.eq.'AUTOUV') then
        if (.not.nxteos) goto 44444
        autouv=set
        auvset=set
        goto 99999
      endif
      if (tkn.eq.'VER') then
        call parsit
        if (.not.scalar) goto 9053
        if (.not.nxteos) goto 44444
        if (tv .lt. 8.0999 .or. tv .gt. sc(119)) goto 9061
        sc(169) = tv
c
c..... Added by Eduard to change the default value of ifl(346) from
c..... zero to one for versions 9.049 and below.
c
        ifl(346)=0
        if (sc(169).lt.9.050) ifl(346)=1-ifl(346)
        goto 99999
      endif
      if (tkn.eq.'EXPCL') then
        if (.not.nxteos) goto 9004
        lexpcl = set
        goto 99999
      endif
      if (token2(1:7).eq.'TRIMMED') then
        call parsit
        if (.not.vocab) goto 9469
        if (ist.ne.DEFALT.and.ist.ne.FACE.and.ist.ne.BASE) goto 9469
        if (.not.nxteos) goto 9004
        if (ist.eq.DEFALT) then
          ifl(340) = 0
        else if (ist.eq.FACE) then
          ifl(340) = 1
        else if (ist.eq.BASE) then
          ifl(340) = 2
        endif
        goto 99999
      endif
c
c..... Added by Eduard for *SET/SCHECK, *RESET/SCHECK statements.
c
      if (tkn .eq. 'SCHECK' .and. length .eq. 6) then     
        if (set) then
          ifl(346)=0
        else
          ifl(346)=1
        endif
        goto 99999
      endif

c
c...(RE)SET/STOP
c
      if (tkn .eq. 'STOP' .and. length .eq. 4) then
          onstop = set
          goto 99999
      endif
c
c...(RE)SET/PAUSE
c
      if (tkn .eq. 'PAUSE' .and. length .eq. 5) then
          onpaus = set
          goto 99999
      endif
c
c...(RE)SET/CASE
c
      if (tkn .eq. 'CASE') then
          ltcase = set
          goto 99999
      endif
c
c...SET/RAPID
c
      if (tkn .eq. 'RAPID') then
          if (.not. set) then
              ifl(2)=25
          else
              call parsit
              if (.not.scalar) goto 9053
              if (.not.nxteos) goto 44444
              if (tv .lt. 0.) goto 9061
              call ulf_ipv_set_rapid(tv)
              sc(169) = tv
          endif
          goto 99999
      endif
c
c...(RE)SET/LANG
c
      if (tkn .eq. 'LANG') then
        if (nextyp .eq.11) goto 9025
        call parsit
        if (nextyp .ne.11) goto 9004
        if (tkn .ne. 'CADRA') goto 9025
        ifl(374) = iset
        goto 99999
      endif
c
c...SET/CMDLEN,CMDCOM
c
      if (tkn .eq. 'CMDLEN' .or. tkn .eq. 'CMDCOM') then
          if (.not. set) then
              ifl(2)=25
          else
              tknsv = tkn
              call parsit
              if (.not.scalar) goto 9053
              if (.not.nxteos) goto 44444
              if (tknsv .eq. 'CMDLEN') then
                  if (itv .lt. 40 .or. itv .gt. 1024) goto 9061
                  ifl(106) = itv
              else
                  if (itv .lt. 0 .or. itv .gt. 1024) goto 9061
                  ifl(387) = itv
              endif
              call ulf_set_line_length(ifl(106),ifl(387))
          endif
          goto 99999
      endif
c
c...(RE)SET/RUNCMD
c......Set to enter command mode at stopping condition for *RUN
c......Example: *RUN/MACRO
c
      if (tkn .eq. 'RUNCMD' .and. length .eq. 6) then
          if(set) then
            ifl(392) = 1
          else
            ifl(392) = 0
          endif
          goto 99999
      endif
c
c...(RE)SET/STPCMD
c......Set to enter command mode at *STOP
c
      if (tkn .eq. 'STPCMD' .and. length .eq. 6) then
          if(set) then
            ifl(393) = 1
          else
            ifl(393) = 0
          endif
          goto 99999
      endif
c                         --- Invalid control option specification.
9025  ifl(2) = 25
      goto 99999

44444 continue
c                         --- End of statement expected
9004  ifl(2) = 4
      goto 99999
c
c... Invalid number of arguments
c... Sharon - 05Aug91
c
9034  ifl(2) = 34
      goto 99999

9053  ifl(2) = 53
      goto 99999

9061  ifl(2) = 61
      goto 99999

9094  ifl(2) = 94
      goto 99999

9170  ifl(2) = 170
      goto 99999
C                         --- DEFALT, BASE or FACE expected

c
c... Scalar given is out of range
c
9445  ifl(2) = 445
      goto 99999
      
9469  ifl(2) = 469
      goto 99999    

2299  continue

99999 if (ifl(2).ne.0) then
          ist=99
          call error(ifl(2))
      endif

      return
      end
