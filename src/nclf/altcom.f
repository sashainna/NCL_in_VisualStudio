C*********************************************************************
C*    NAME         :  altcom.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*        altcom.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:09:36
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine altcom(iwhich)
C*       purpose of subroutine: to parse all commands not handled in the 
C*       driver.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine altcom(iwhich)

c      include 'com8.com'
      include 'com.com'
      include 'mocom.com'
      include 'wrksys.com'

      integer*2 iwhich

      integer*2 irest(4)
      integer*2 icl,isubcl,inum
      equivalence (irest,rest)
      integer*2 i,j,isv,numcpy,ipos,ipos4
      integer*2 iuni,nuni
      real*8 copdat(2)
      character*1 ctemp(9)
      character*2 ct
      character*3 csegno
      character*4 ct6
      character*64 toksav
      equivalence (copdat,leq2),(leq2,ctemp,ct),(ctemp(6),ct6)
      equivalence (ifl(244), leq3), (leq3, csegno)
      logical mxtr,leq2, leq3, ltmp1, ltmp2, trflg
      real*8 copymx(16), rsetmx(12), tmpmx(12), rte(6), tempmx(12)
      integer*2 nocop
      real*4 asc162(2)
      equivalence (sc(162),asc162)
c temp storage for ifl flag
      integer*2 isifl
      integer*2 izro /0/

      integer*4 nclkey,kbeg,kend,nci,strlen1,jsub,isub
      integer*4 svrec(2),ipg,iel
      integer*2 nwds,ietype, ierr
      character*(MAX_LEN) temp
      character*(MAX_PATH) temp2
      integer*2 CHKPT, MODSYS, IFTOL, CONTCT, SRFVCT, ONV, READV,
     1          STOCKV,FIXTV,SAVEV
      parameter (CHKPT=669)
      parameter (MODSYS=668)
      parameter (IFTOL=751)
      parameter (CONTCT=856)
      parameter (SRFVCT=857)
      parameter (FIXTV=898)
      parameter (ONV=71)
      parameter (READV=876)
      parameter (SAVEV=582)
      parameter (STOCKV=321)

      data rsetmx /1.0, 0.0, 0.0, 0.0,
     y             0.0, 1.0, 0.0, 0.0,
     z             0.0, 0.0, 1.0, 0.0/
      data trflg /.false./
c
c    
      if (.not.(ityp.eq.1)) go to 900

c                                                     **** redef
          if (.not.(ist.eq.840)) go to 12
               call redef(iwhich)
               go to 890

c                                                     **** remove
12        if (.not.(ist .eq. 843)) go to 20
              call remov
              go to 890

c                                                     **** obtain
20        if (.not.(ist.eq.661)) go to 25
              call obtain
              go to 99999

25        IF (.not.(IST.EQ.849)) go to 30
C                                                     **** FORMAT
              IF (NEXTYP.NE.5) THEN
                  ISVINX=INX
                  CALL ERROR(22)
                  GO TO 99999
              ENDIF
              CALL PARSIT
              IF (ITYP.EQ.1.AND.IST.EQ.851) THEN
C                                     **** SHORT
                  IFL(294) = 0
              ELSE IF (ITYP.EQ.1.AND.IST.EQ.850) THEN
C                                     **** LONG
                  IFL(294) = 1
              ELSE 
                  CALL ERROR(360)
                  GO TO 99999
              ENDIF
              go to 890

c                                                     **** canon
30        if (.not.(ist.eq.645)) go to 40
              if (.not.(nextyp.ne.5)) go to 50
22                isvinx=inx
                  call error(22)
                  go to 99999
50            continue
              call parsit
              if (ist.eq.71) then
                  isifl=ifl(41)
                  ifl(41)=1
                  if((isifl.ne.ifl(41)).and.(ifl(35).eq.0))call upstat()
              else if (ist.eq.72) then
                  isifl=ifl(41)
                  ifl(41)=0
                  if((isifl.ne.ifl(41)).and.(ifl(35).eq.0))call upstat()
              else
                  call error(56)
                  go to 99999
              endif
              go to 890
40        if (.not.(ist.eq.841)) go to 60
c                                                     **** units 
              if (nextyp.ne.5) go to 22
              call parsit
              if (ityp.eq.1.and.(ist.eq.296.or.ist.eq.303)) then
                  if (ist.eq.296) then
c                                      *** millimeters
                     call millim 
c
c...Added UNITS output to clfile
c...Bobby  -  3/25/93
c
                     call putcl (7300,2,1,sc)
                  else
c                                      *** inches
                     call inches
c
c...Added UNITS output to clfile
c...Bobby  -  3/25/93
c
                     call putcl (7300,1,1,sc)
                  endif
              else
                  call error(318)
                  go to 99999
              endif
              go to 890

c                                                     **** gougck  epm  11-7-84
60        if (.not.(ist.eq.838)) go to 70
                  if (nextyp.ne.5) go to 22
                  ifl(44)=9
                  call parsit
                  if (ityp.eq.1) then
                    if (ist.eq.71) then
                      ifl(6)=1
                      if (nextyp.ne.11) then
                        call parsit
                        if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3
     x                                 .and.ityp.ne.4.or.itv.lt.0) then
                          call error(224)
                          goto 99999
                        endif
                        ifl(6)=itv
                      endif
                      idsgck=ifl(6)
                      icsgck=ifl(6)
                    else if (ist.eq.72) then
                      ifl(6)=0
                      idsgck=ifl(6)
                      icsgck=ifl(6)
                    else if (ist.eq.728) then
                      call parsit
                      if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3
     x                                 .and.ityp.ne.4.or.itv.lt.0) then
                        call error(224)
                        goto 99999
                      endif
                      ifl(6) = itv
                      call parsit
                      if (ityp.ne.1 .or. ist.ne.729) then
                        call error(5)
                        goto 99999
                      endif
                      call parsit
                      if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3
     x                                 .and.ityp.ne.4.or.itv.lt.0) then
                        call error(224)
                        goto 99999
                      endif
                      idsgck = itv
                      call parsit
                      if (ityp.ne.1 .or. ist.ne.753) then
                        call error(5)
                        goto 99999
                      endif
                      call parsit
                      if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3
     x                                 .and.ityp.ne.4.or.itv.lt.0) then
                        call error(224)
                        goto 99999
                      endif
                      icsgck = itv
                    else
                      call error(5)
                      goto 99999
                    endif
                  else
                      call error(56)
                      go to 99999
                  endif

                  if (ifl(6).gt.3) ifl(6) = 3
                  if (idsgck.gt.4) idsgck = 4
                  if (icsgck.gt.4) icsgck = 4

                  go to 890

c                                                     **** multax
c                    multax can be turned off.  epm 10-1-84
70        if (.not.(ist.eq.815)) go to 80 
			  if (nextyp.eq.11) then 
c                      multax/on is default
                   ifl(82)=1
              else
                  if (nextyp.ne.5) go to 22
                  call parsit
                  if (ityp.eq.1.and.ist.eq.72) then
c                                               **** multax/off
                      ifl(82)=0
                  else if (ityp.eq.1.and.ist.eq.71) then
c                                               **** multax/on
                      ifl(82)=1
                  else
                      call error(56)
                      go to 99999
                  endif
              endif
              if (ifl(82).eq.0) then
                   i=1
              else
                   i=0
              endif
              call putcl(9000,i,1,tv)
              go to 890

c                                                     **** cut
80        if (.not.(ist.eq.803)) go to 90
c
c                         if we are in an implied check surface range, cut
c                         is invalid.  call error and  reset nline to the
c                         last motion statement.     epm   1-29-86
              if (ifl(215).ne.0) then
                   call error(302)
c
c...Added check for NCL-VT mode
c...Paul  -  10/2/91
c
                   if (ifl(35).eq.0 .or. ifl(35) .eq. 2) then
                        call nclf_src_rec_to_line(ifl4(13),nline)
                        ifl(215) = 4
                   endif
                   go to 99999
              endif
              if (ifl(42).eq.0) go to 99999 
              ifl(42)=0
c                  cut needs to call mocntl to output last point
              isc10(1)=803
              ifl(1)=2
c                  allow cut to preceed a statement (cut,...)  
              if (nextyp .eq. 9) then
                  ncsfl(3) = 1
                  inx = inx + 1
                  go to 99999
              endif
              go to 890

c                                                     **** dntcut
90        if (.not.(ist.eq.802)) go to 101
c
c                         if we are in an implied check surface range, dntcut
c                         is invalid.  call error and  reset nline to the
c                         last motion statement.     epm   1-29-86
              if (ifl(215).ne.0) then
                   call error(302)
c
c...Added check for NCL-VT mode
c...Paul  -  10/2/91
c
                   if (ifl(35).eq.0 .or. ifl(35) .eq. 2) then
                        call nclf_src_rec_to_line(ifl4(13),nline)
                        ifl(215) = 4
                   endif
                   go to 99999
              endif
              sc(141)=sc(1)
              sc(142)=sc(2)
              sc(143)=sc(3)
              if (nextyp.eq.11) then
                  ifl(42)=1
              else
c                  allow dntcut to preceed a statement (dntcut,...)
                  if (nextyp.eq.9) then
                      ifl(42) = 1
                      ncsfl(3) = 1
                      inx = inx + 1
                      go to 99999
                  else
                      if (nextyp.ne.5) go to 22
                  endif
                  call parsit
                  if (ist.ne.53) then
                      call error(157)
                      go to 99999
                  endif
                  ifl(42)=0
              endif
              go to 890

c                                                     **** loopst
C          else if (ist.eq.808) then
C             call loopst
C NEW STUFF
101       if (.not.(ist.eq.808.or.ist.eq.844)) go to 110
c                                                     **** loopst or do
              if (ist.eq.808.and.ifl(286).gt.0) then
                  call error(349)
                  go to 99999
              endif
              call loopst
              go to 890

c                                                     **** jumpto
110       if (.not.(ist.eq.826)) go to 120 
              call jumpto
              go to 890

c                                                     **** if
120       if (.not.(ist.eq.825)) go to 130
              call condif
              go to 99999

c                                                     **** reserv
130       if (.not.(ist.eq.805)) go to 140
              if (nextyp.ne.5) go to 22
              ifl(44)=9
10            call parsit
              if (ityp.eq.2) then
                  if (ist.eq.1) then
                      if (ivxsub.eq.0) then
                          savid2=token2
                          isvsub = 0
                          ifl(9)=ifl(11)
                          ifl(10)=ifl(12)
                      else
                          call error(86)
                          go to 99999
                      endif
                  else
                      call error(8)
                      go to 99999
                  endif
              else
                  call error(87)
                  go to 99999
              endif
              call parsit
              if (ityp.eq.3.or.(ityp.eq.2.and.ist.eq.2)) then
                  if (itv.lt.0.or.itv.gt.1000000) then
                      call error(85)
                      go to 99999
                  endif

                  idst=14
                  rest=0
                  irest(3)=itv
                  irest(4)=14
                  call vstore
              else
                  call error(53)
                  go to 99999
              endif
              if (nextyp.ne.11) go to 10
              go to 890

c                                                     **** refsys
140       if (.not.(ist.eq.644)) go to 150
              if (nextyp.ne.5) go to 22
              ifl(44)=9
              call parsit

c                                               ***  nomore
              if (ist.eq.53) then
                  ifl(72)=0
                  call mxcolc (nclkey,1,0)
                  rsname = '        '

c                     reset working plane
cc                  if (ifl(35).eq.0) call stwpmx (rsetmx)
                  call stwpmx (rsetmx)

c                                               ***  matrix
              else if (ityp.eq.2.and.ist.eq.10) then
                  call expnm2 (token2, ivxsub, i, rsname)
                  call gtgeom(tv,sc(56),nclkey,nwds,ietype)
c                                          invert and store in sc(68)-79
                  call invmx (sc(56), sc(68))
                  ifl(72) = 1
                  call mxcolc (nclkey,1,1)
cc                  if (ifl(35).eq.0) then
c                     change working plane to reflect new refsys matrix
                    call gtgeo(nclkey,tmpmx)
                    call stwpmx (tmpmx)
cc                  endif

c                 error if token is undefined name
              else if (ityp.eq.2.and.ist.eq.1) then
                  call error(9)
                  go to 99999
              else
                  call error(94)
                  go to 99999
              endif
              go to 890

c                                                     **** zsurf
150       if (.not.(ist.eq.647)) go to 160
              if (nextyp.ne.5) go to 22
              ifl(44)=9
              call parsit
              if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4)
     1            then
c                           zsurf/n plus refsys is ng    9-feb-83
                  if(ifl(72).eq.0) goto 232
                  call error(206)
                  goto 99999
232                continue
                  sc(37)=0
                  sc(38)=0
                  sc(39)=1
                  sc(40)=tv
                  ifl(55)=1
              else if (ityp.eq.2.and.(ist.eq.PLANE.or.ist.eq.SURF)) then
c                            zsurf/pl is always in part sys    9-feb-83
                  if (ist.eq.SURF) then
                    call gtdesc(tv,nclkey,nwds,ietype)
                    call ncl_get_sf_primtyp(nclkey,ietype)
                    if (ietype.ne.3) then
                      call error(281)
                      go to 99999
                    endif
                  endif
                  call gtplt(tv, izro, sc(37))
                  ifl(55)=1
              else if (ist.eq.53) then
                  ifl(55)=0
c              else if (ityp.eq.2.and.ist.eq.1) then
c                  call error(9)
c                  go to 99999
              else
                  call error(281)
                  go to 99999
              endif
              go to 890

c                                                     **** tracut
c
c...TRACUT is now output to the clfile
c...Bobby  -  12/1/92
c
160       if (.not.(ist.eq.812)) go to 170
              if (nextyp.ne.5) go to 22
              ifl(44)=9
              call parsit

c                                               *** nomore
              if (ist.eq.53) then
                  ifl(73)=0
                  tcname = '        '
                  call mxcolc (nclkey,2,0)
                  icl    = 7000
                  isubcl = 2
                  inum   = 1
                  call putcl (icl,isubcl,inum,sc(41))
              else if (ityp.eq.2.and.ist.eq.10) then
                  ifl(73)=1
                  call expnm2 (token2, ivxsub, i, tcname)
                  call gtgeom(tv,sc(41),nclkey,nwds,ietype)
                  call mxcolc (nclkey,2,1)
                  icl    = 7000
                  isubcl = 1
                  inum   = 13
                  call putcl (icl,isubcl,inum,sc(41))
                  call invmx (sc(41),sc(93))
              else if (ityp.eq.2.and.ist.eq.1) then
                  call error(9)
                  go to 99999
c                                               *** tracut/last
              else if (ist.eq.52) then
                  if (ifl(73) .eq. 0) then
                    call error(437)
                    go to 99999
                  endif
                  call parsit
                  if (ityp.eq.2.and.ist.eq.10) then
                    call gtgeom(tv,tempmx,nclkey,nwds,ietype)
                    if (ifl(377) .eq. 0) then
                      do 168 i=0,11
                        sc(202+i) = sc(41+i)
  168                 continue
                    endif
                    ifl(377) = 1
                    call mxmult(sc(202),tempmx,sc(41))
                    icl    = 7000
                    isubcl = 1
                    inum   = 13
                    call putcl (icl,isubcl,inum,sc(41))
                    call invmx (sc(41),sc(93))
c                                               *** tracut/last,nomore
                  else if (ist.eq.53) then
                    if (ifl(377) .ne. 0) then
                      do 169 i=0,11
                        sc(41+i) = sc(202+i)
  169                 continue
                    endif
                    ifl(377) = 0
                    icl    = 7000
                    isubcl = 1
                    inum   = 13
                    call putcl (icl,isubcl,inum,sc(41))
                    call invmx (sc(41),sc(93))
                  else
                    call error(94)
                    go to 99999
                  endif
              else
                  call error(94)
                  go to 99999
              endif
              go to 890

c                                                     **** index
170       if (.not.(ist.eq.810)) go to 180
              call indx
              go to 890

c                                                     **** copy
c
c...Copy is now allowed even if not
c...creating a clfile
c...Bobby  -  12/1/92
c
180       if (.not.(ist.eq.811)) go to 185
c              if (ifl(69).eq.1) then
                  call copy1 (copdat,mxtr,copymx,numcpy,nocop)
c
c...vp 4/20/98 safe 'if' to avoid NAN bit combination in copdat
c...created on VAX/VMS running euro_04.pp
c
                  if (nocop.eq.1) then
                      rte(1) = sc(1)
                      rte(2) = sc(2)
                      rte(3) = sc(3)
                      rte(4) = sc(4)
                      rte(5) = sc(5)
                      rte(6) = sc(6)
                      call ncl_setptr(imotp,svrec)
                      call copy2 (copdat,mxtr,copymx,numcpy)
                      call dismot (svrec,imotp,rte)
                  endif
c              else
c                  call error (-210)
c                  go to 99999
c              endif
              go to 890
c
c                                                      ****rename
c...Adding a new function to rename.
c
185       if (.not.(ist.eq.869)) go to 190
             call renam
             go to 99999

190       if (.not.(ist.eq.742)) go to 199
c                                                     **** erase
              if (nextyp.ne.5) go to 22
c
c...Added check for NCL-VT mode
c...Paul  -  10/2/91
c
ccccccc    if (ifl(35).eq.1 .or. ifl(35) .eq. 2) go to 99999
c
c...Added check for NCL501+ mode
c...Paul  -  02/11/92
c...Old version was:
c...if (ifl(35).eq.1 .or. ifl(35) .eq. 2) go to 99999
c
c
c...VX
c
      if (ifl(35).eq.1.or.(ifl(35).eq.2.and.ifl(350).eq.0) .or.
     1    ifl(322) .eq. 1) goto 99999

c
c         same action as invisible. kathy
c
              isv = inx
			  call delgeo

c              call parsit
c              if (nextyp.ne.11) then
c                if (ityp.eq.2.and.((ist.ge.2.and.ist.le.9).or.
c     1              ist.eq.18.or.ist.eq.20)) then
c					goto 890
c                else
c                   call error(4)
c                   goto 99999
c                endif
c              endif
c                                               *** /motion
              if (ist.eq.836) then
c                                delete the motion segment.  epm  6-19-86
                   call motdel

              else if (ityp.eq.1.and.ist.eq.132) then
c                                          erase/axis
                  call delaxs

c              else if (.not.(ityp.eq.1.and.((ist.ge.602.and.ist.le.609)
c     1                  .or.ist.eq.636.or.ist.eq.816))) then
c                if (.not.(ityp.eq.2.and.((ist.ge.2.and.ist.le.9).or.
c     1              ist.eq.18.or.ist.eq.20))) then
c                        call error(5)
c                   endif
              endif
              go to 890
c
c        added for invisible. kathy
c
199       if (.not.(ist.eq.1096)) go to 201
c                                                     **** invisible
              if (nextyp.ne.5) go to 22
c
c...Added check for NCL-VT mode
c...Paul  -  10/2/91
c
ccccccc    if (ifl(35).eq.1 .or. ifl(35) .eq. 2) go to 99999
c
c...Added check for NCL501+ mode
c...Paul  -  02/11/92
c...Old version was:
c...if (ifl(35).eq.1 .or. ifl(35) .eq. 2) go to 99999
c
c
c...VX
c
      if (ifl(35).eq.1.or.(ifl(35).eq.2.and.ifl(350).eq.0) .or.
     1    ifl(322) .eq. 1) goto 99999

              isv=inx
              call delgeo
              go to 890

c                                                     **** fini
201       if (.not.(ist.eq.801)) go to 210
              if (ifl(69).eq.1) call putcl (14000,0,1,tv)
c                  if batch mode then quit
              if (ifl(35).eq.1) then
                  quit=.true.
c                  if (ifl(69).eq.1) then
c                      call clput (i4stat(2),clbuff)
c                  endif
              endif
c
              if (ifl(38).eq.0.and.ifl(45).eq.0) then
                  pmode=0
              else
                  call error(84)
                  go to 99999
              endif
              go to 890

c                                                     **** genpts
210       if (.not.(ist.eq.743)) go to 220
            call genpts
            go to 890

c                                                     **** includ
220       if (.not.(ist.eq.744) .and. .not.(ist.eq.READV)) go to 230
              if (ist .eq. READV) then
                  temp = cimage(1:nccimg)
                  ipos = index(temp,',')
411               ipos = ipos + 1
                  if (ipos.lt.MAX_LINE .and. 
     x                temp(ipos:ipos).eq.' ') goto 411
                  inx = ipos
              endif
              call includ (kbeg,kend)
              goto 99999

230       if (.not.(ist.eq.842)) go to 240
c                                                     **** tralst
              if (nextyp.ne.5) go to 22
              call parsit
              if (ist.eq.71) then
                  ifl(267)=1
              else if (ist.eq.72) then
                  ifl(267)=0
              else
                  call error(56)
                  go to 99999
              endif
              go to 890
240       if (.not.(ist.eq.847)) go to 250
c                                                     **** loadu
c
c...loadu not allowed in loop or macro
c
             if (ifl(38).ne.0 .or. ifl(45).ne.0) then
               call error(84)
               goto 99999
             endif
             iuni = 841 
             nuni = ifl(264)
             ldtext = .true.
             call parsit
             ldtext = .false.
             if (lstrng) then
               j = 0
               call gttext(temp2,j)
               ipos = 1
               do while (ipos.lt.j .and. temp2(ipos:ipos).eq.' ')
                 ipos = ipos+1
               enddo
               temp = temp2(ipos:)
               if (nxteos) goto 425
               call parsit
             else
c
c...vp 12-jul-94 set limit to 72 otherwise doesn't work in includ
c...files since cimage(72:79) = 'include'
c
               err  = .false.
               temp = cimage(1:nccimg)
               ipos = index (temp, '/')
245            ipos = ipos+1
               if (ipos.lt.nccimg.and.temp(ipos:ipos).eq.' ') goto 245
               ipos4 = index (temp, ',')
               if (ipos4 .gt. ipos) then
                  temp = cimage(ipos:ipos4-1)
               else
                  temp = cimage(ipos:nccimg)
               end if
               if (ipos4 .eq. 0) go to 425 
               inx  = ipos4 + 1
             end if
c
c...Check if units specified with the unibase name
c
             idtype = -1
             call parsit
             if (ityp .ne. 1) then
                call error (232)
                go to 99999
             end if
             iuni = ist 
c
c...make sure INCH is active before loading unibase
c...since every unibase is in inches.
c
  425        if (nuni .ne. 0) call inches 
             call fparse (temp,temp,' ','.u')
             call flname (5,temp,temp2)
             nci = strlen1(temp2)
             call loadu (temp2, nci, ifl(35), i)
c
c rah: loadu now check version number and machine type of Unibase
c
c bad version number
c
c...Output the error messages here
c...when in batch
c...Otherwise NCL stays in an infinite loop
c...Bobby  -  3/12/92
c
             if (i.eq.2) then
                errcom='USE VERSION CONVERTER'
c               if (ifl(35).eq.1) then
c                  ifl(2) = 406
c                  err=.true.
c               else
                   call error(406)
c               endif
c wrong machine type
             else if (i.eq.3) then
                errcom='USE PLATFORM CONVERTER'
c               if (ifl(35).eq.1) then
c                  ifl(2) = 407
c                  err=.true.
c               else
                    call error(407)
c               endif
c other errors - missing file?
             else if (i.ne.0) then
c               if (ifl(35).eq.1) then
c                  ifl(2) = 145
c                  err=.true.
c               else
                    call error(145)
c               endif
             endif
c
c...process INCHES, MM, SAME (as unibase),
c...UNITS or default - no action
c
             if (iuni .eq. 303) then
                 call inches 
             else if (iuni .eq. 296) then
                 call millim
             else if (iuni .eq. 730) then
                 call unbuni
             else if (iuni .eq. 841) then
                 if (nuni .eq. 1) call millim 
             else 
                 call error (192)
             end if
             go to 99999 
c
250       if (.not.(ist.eq.848)) go to 260
c                                                     **** saveu
c...vp 12-jul-94 set limit to 72 otherwise doesn't work in includ
c...files since cimage(72:79) = 'include'
c
              ldtext = .true.
              call parsit
              ldtext = .false.
              if (lstrng) then
                j = 0
                call gttext(temp2,j)
                ipos = 1
                do while (ipos.lt.j .and. temp2(ipos:ipos).eq.' ')
                  ipos = ipos+1
                enddo
                temp = temp2(ipos:)
              else
                err  = .false.
                temp = cimage(1:nccimg)
                ipos = index (temp, '/')
420             ipos = ipos+1
                if (ipos.lt.nccimg.and.temp(ipos:ipos).eq.' ') 
     x                 goto 420
                temp = cimage(ipos:nccimg)
              endif
              call fparse (temp,temp,' ','.u')
              call flname (5,temp,temp2)
              nci = strlen1(temp2)
              call saveu (temp2, nci, i)
              if (i.ne.0) call error(145)
              goto 99999
c                                                     **** chkpt
260       if (ist.ne.CHKPT) goto 270
            call chkpts (iwhich)
            goto 99999
c                                                     **** wrksys
270       if (ist.ne.MODSYS) goto 280
              if (nextyp.ne.5) go to 22
              ifl(44)=9
              call parsit
              toksav = token2
              isub = ivxsub
c                                               *** nomore
              if (ist.eq.53) then
c                     reset working plane
                  if (.not.nxteos) then
                    call error(4)
                    goto 99999
                  endif
c                 if (ifl(35).eq.0) call stmdmx (rsetmx)
                  call stmdmx (rsetmx)
                  lwrk = .false.
                  ifl(72)=0
                  rsname = '        '
                  call mxcolc (nclkey,3,0)
c                     reset working plane
cc                  if (ifl(35).eq.0) call stwpmx (rsetmx)
                  call stwpmx (rsetmx)
              else if (ityp.eq.2.and.ist.eq.10) then
                  if (.not.nxteos) then
                    call error(4)
                    goto 99999
                  endif
                  call gtdesc(tv,nclkey,nwds,ietype)
                  ifl4(14) = nclkey
                  call gtgeo (nclkey,wrkmx)
                  wrkscl = dsqrt(wrkmx(1)**2+wrkmx(2)**2+wrkmx(3)**2)
                  call invmx (wrkmx, invwrk)
                  call mxcolc (nclkey,3,1)
c                     change model coord sys to reflect new wrksys matrix
c                 if (ifl(35).eq.0) call stmdmx (wrkmx)
                  call stmdmx (wrkmx)
                  lwrk = .true.
c                     reset refsys.
                  ifl(72)=0
                  call mxcolc (nclkey,1,0)
                  rsname = '        '
cc                  if (ifl(35).eq.0) call stwpmx (rsetmx)
                  call stwpmx (rsetmx)                 
                  modlab = ' '
                  call expnm2 (toksav, isub, i, modlab)
              else if (ityp.eq.2.and.ist.eq.1) then
                  call error(9)
                  go to 99999
              else
                  call error(94)
                  go to 99999
              endif
              call vxlfst
              call vxlnxt(token2, jsub, nclkey, nwds, ietype, ipg, iel)
              do while (ietype.ne.1)
                if (ietype .eq. MATRIX .and. nclkey .ne. ifl4(14)) then
                  call dspent(nclkey,ietype)
                endif
                call vxlnxt(token2, jsub, nclkey, nwds, ietype,ipg,iel)
              enddo
              go to 99999
c.....
c..... New SYN/NEW_NAME,OLD_NAME[,...,...] command. Paul 08/11/92
c.....
280       if (.not.(ist.eq.804)) go to 290
c                                                     **** syn
          call syncom
          goto 99999
c.....
c..... END OF THE CODE FOR THE "SYN" COMMAND !!!!!!!!!!!!!!!!!!!!!!
c.....
  290     if (IST .ne. 747) go to 300
c
c...PODDEF/
c
              call poddef 
              go to 99999
  300     if (IST .ne. 746) go to 310
c
c...PODPTS/.....
c
              call podpts (iwhich)
              go to 99999
  310     if (IST .ne. 899) go to 320
c
c...DEFNAM/....
c
              call modnam
              go to 99999 
  320     if (IST .eq. IFTOL) then
c
c...                                                  **** IFTOL
c
          call parsit
          if (.not.scalar) then
            call error(7)
            goto 99999
          endif
          if (.not.nxteos) then
            call error(4)
            goto 99999
          endif
          sc(170) = dabs(tv)
          goto 890
          end if
          if (IST .eq. CONTCT) then
c
c...                                                  **** CONTCT
c
            if (nxteos) then
              lcntct = .true.
            else
              if (nextyp.ne.5) go to 22
              call parsit
              if (ityp.eq.1.and.ist.eq.72) then
c                                               **** contct/off
                lcntct = .false.
              else if (ityp.eq.1.and.ist.eq.71) then
c                                               **** contct/on
                lcntct = .true.
              else
                call error(56)
                go to 99999
              endif
            endif
            goto 890
          endif
c
c...                                                  **** SRFVCT
c
          if (IST .eq. SRFVCT) then
            ltmp1 = .false.
            ltmp2 = .false.
            ifl(44) = 9
C
C...   kludged because parsit does not allow a comma followed by a minus sign,
C
            if (ain(inx) .eq. ',') then
              inx=inx+1
            else
              call gtvect (trflg, sc(11), ierr)
              if (ierr.gt.0) goto 9888
              ltmp1 = .true.
            endif
            if (.not.nxteos) then
              call gtvect (trflg, sc(14), ierr)
              if (ierr.gt.0) goto 9888
              ltmp2 = .true.
            endif
            if (.not.nxteos) goto 88888
            ldssvc = ltmp1
            dssfvc(1) = sc(11)
            dssfvc(2) = sc(12)
            dssfvc(3) = sc(13)
            lcssvc = ltmp2
            cssfvc(1) = sc(14)
            cssfvc(2) = sc(15)
            cssfvc(3) = sc(16)
            goto 99999
          endif
c
c...                                                  **** ON/ERROR
c
          if (IST .eq. ONV) then
            call onerr
            goto 99999
          endif
c
c...                                                  **** STOCK/sym
c
          if (IST .eq. STOCKV .or. IST .eq. FIXTV) then
            call crestk (IST)
            goto 99999
          endif
c
c...                                                  **** SAVE
c
          if (IST .eq. SAVEV) then
            call savecm
            goto 99999
          endif

c
885       continue
              call error(5)
              go to 99999
890       continue
          go to 1000
900   continue
      call error(13)
      go to 99999
1000  continue

c          don't check for end of statement if ifl(135) is 1 since the
c          rest of the input statement will be handled by the disply routine
88888 if (.not.err .and. ifl(135).eq.0) then
          call parsit
          if (ityp.ne.7) call error(4)
      endif

99999 continue
cc99999 call uw_newline_flush()
      return
C                               Error
9888  call error(ierr)
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine millim
C*       FUNCTION: to set units MM in CAD and CAM. 
C*       driver.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine millim
c
      include 'com.com'
      include 'const.com'
      include 'wrksys.com'
c
      real*4 asc162(2)
      equivalence (sc(162), asc162)
c
      real*8 rsetmx(12)
c
      data rsetmx /1.0, 0.0, 0.0, 0.0,
     y             0.0, 1.0, 0.0, 0.0,
     z             0.0, 0.0, 1.0, 0.0/
c
c.....    Millimeters
c
      if (ifl(264).eq.0) then
          lwrk = .false.
c         if (ifl(35).eq.0) call stmdmx (rsetmx)
          call stmdmx (rsetmx)
          ifl(72) = 0
          rsname = '        '
cc          if (ifl(35).eq.0) call stwpmx (rsetmx)
          if (ifl(35).eq.0) call stwpmx (rsetmx)         
      endif
      ifl(264) = 1
      sc(27) = .025
      sc(91) = sc(27) * 2.0d0
      sc(92) = dcos(1.0d0/RADIAN)
      sc(54) = 100.
      sc(105) = 100.
      asc162(2)=.25
      sc(214) = sc(162)
      sc(167) = .025
      sc(168) = .0001
c      sc(168) = .0025
c      if (sc(169) .lt. 9.199) sc(168) = .0001
c      sc(171) = .25
      call setmm
c
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine inches
C*       FUNCTION: to set units INCH in CAD and CAM.
C*       driver.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine inches
c
      include 'com.com'
      include 'const.com'
c
      character*64 rsname
      real*4 asc162(2)
      equivalence (sc(162),asc162)
      equivalence (sc163, rsname)
c
      real*8 rsetmx(12)
c
      data rsetmx /1.0, 0.0, 0.0, 0.0,
     y             0.0, 1.0, 0.0, 0.0,
     z             0.0, 0.0, 1.0, 0.0/
c
c....     Inches
c
      if (ifl(264).eq.1) then
          lwrk = .false.
c         if (ifl(35).eq.0) call stmdmx (rsetmx)
          call stmdmx (rsetmx)
          ifl(72) = 0
          rsname = '        '
cc          if (ifl(35).eq.0) call stwpmx (rsetmx)
          call stwpmx (rsetmx)
      endif
      ifl(264) = 0
      sc(27) = .001
      sc(91) = sc(27) * 2.0d0
      sc(92) = dcos(1.0d0/RADIAN)
      sc(54) = 4.
      sc(105) = 4.
      asc162(2)=.01
      sc(214) = sc(162)
      sc(167) = .001
      sc(168) = .0001
c      sc(171) = .01
      call setin
c
      return
      end
