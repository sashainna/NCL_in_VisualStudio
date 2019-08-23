C*********************************************************************
C*    NAME         :  postp.for
C*       CONTAINS:
C*         postp   pstwfr  gtfedp  stclix
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        postp.for , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*        07/13/15 , 09:23:34
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine postp
c*       this routine parses post processor commands.  it puts
c*       the parsed command in the sc(10) holding area for putcl.
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
C
      subroutine postp
 
      include 'com8a.com'
      include 'comgt.com'
      include 'cutter.com'
      include 'fillet.com'
      include 'mocom.com'
 
      real*4 tapnow,tapeq(2)
      equivalence (tapeq(1),sc(55)),(tapeq(2),tapnow)
      real*8 temp,rletx
      integer*2 itemp(4),i2sc18(4),i2sc15(4),xsv125,ysv125,acnt
      integer*4 jtemp(2)
      equivalence (xsv125,ifl(132)),(ysv125,ifl(133))
 
      integer*2 ktv(4)
      equivalence (tv,ktv)
      integer*4 i4sc17(2)
      equivalence (temp,jtemp,itemp),(i2sc18,sc(108)),(i4sc17,sc(107))
      equivalence (lastx,i2sc15,sc(115)),(lasty,i2sc15(2))
      character*2 dt,lastpn
      character*3 pu,pd
      character*5 c5temp
      character*(MAX_LEN) ctemp
      character*1 ltemp(90),cletx(4),c5tmp1(5)
      equivalence (c1temp,c5temp,c5tmp1)
      logical leqpt1,leqpt4,first,letx(4),ldig(5),c1temp(5)
      equivalence (letx(1),leqpt4),(leqpt4,rletx),(leqpt4,cletx)
      equivalence (letx(1),dt), (letx(3),etx)
      equivalence (ctemp,ltemp),(ldig,ltemp(3))
      equivalence (leqpt1,i2sc18(3)),(leqpt1,lastpn)
      real*8 a(10)
      character*(MAX_LEN) ach
      equivalence (a,ach)
      character*1 aain(MAX_LEN)
      equivalence (aain,cimage)
      integer*2 icl,isubcl,numr8s,etx,lgth,savinx,savist,savitp
      character*5 int1,int2
      character*1 clesc,clgs,clus
      CHARACTER*(MAX_LEN) TCIN
      CHARACTER*30 CTMP
      INTEGER*2 IX1,IX2,IX3
 
      integer*2 ipb,maxipb
      equivalence (ifl(185),ipb),(ifl(186),maxipb)
      logical lsc122
      character*1 lstplt(5)
      character*5 lplt5
      equivalence (sc(122),lsc122),(lsc122,lstplt,lplt5)
      real*4 r4133(2)
      integer*2 isc133(4)
      equivalence (sc(133),isc133),(sc(133),r4133)
 
      logical trflg
      integer*4 nclkey,strlen1,nc
      integer*2 nw, ietype, primtyp
      logical leq1
      real*8 req1(5),FEEDR
      equivalence (token2,leq1),(leq1,req1),(FEEDR,MOTMAP(24))
      character*64 pstnam
      logical lv90, tflwrn, tlfed, tflcom, tflsam, lres
 
      real*8 tflrad, tfltol, tmang
      integer*2 numdec,len,is1,is4,iclf,iflg,jerr
      integer*4 irec(2),svrec(2),iclw(6)
      character*9 fltfmt
      character*256 str
 
      integer*2 WARN,NOWRN,FEDRAT,ARCSLP,FILLET
      parameter (WARN=852,NOWRN=853,FEDRAT=1009,ARCSLP=1029,FILLET=402)
 
      data int1/'w(i1)'/
      data int2/'w(i2)'/
      data is1/4/, is4/1/
 
      lv90  = sc(169).lt.9.04999
      trflg = .true.
      temp=0
      clesc = char(27)
      clgs   = char(29)
      clus   = char(31)
      pu='pu;'
      pd='pd;'
      etx=xetx
      dt='dt'
      icl=2000
      isubcl=ist
      if (nextyp.ne.5.and.nextyp.ne.9) then
          if (nextyp.ne.11.and.nextyp.ne.0) then
              if (ityp.ne.1.or.ist.lt.1043.or.(ist.gt.1046 .and.
     1            ist .ne. 1091)) then
                  isvinx=inx-1
                  call error(22)
                  go to 99999
              endif
          endif
      else
          ifl(44)=9
      endif
c
c    for rapid color and line type. kathy
c
      rpfron = .false.
      if (ist.eq.5) then
          rpfron=.true.
          ifl(323) = 1
      end if
 
      if (ist.eq.16) tapnow=0
 
      if (ist.lt.1043.or.(ist.gt.1046 .and. ist.ne.1091)) go to 3099
c              handle partno, insert, letter, pprint commands
c
c       if INSERT or PPRINT do not start at column 1 give error exit. kathy
          call convrt(cimage,cout,6)
          if (.not.(cout(1:6).eq.'INSERT'.or.
     x        cout(1:6).eq.'LETTER'.or.
     x        cout(1:6).eq.'PPRINT'.or.
     x        cout(1:6).eq.'PARTNO'.or.
     x        cout(1:6).eq.'REMARK'))then
                call error(405)
                goto 99999
          endif
 
c
c...IT'S A PPRINT, INSERT, REMARK. LOOK FOR SCALARS PRECEDED BY @
c
          IF ((IST.GE.1044.AND.IST.LE.1046) .or. ist .eq. 1091) THEN
              IX2 = 1
              IX3 = 7
              tcin=' '
              cimage(nccimg+1:) = ' '
              ldtext = .true.
              mxc = ifl(387) - 1
              if (mxc .le. 0) mxc = ifl(106)
              DO 49,IX1=7,nccimg,1
c
c...Check for buffer overflow
c...Bobby  -  6/5/92
c
                  if (ix2 .gt. mxc) go to 491
                  TCIN(IX2:IX2)=AAIN(IX3)
                  IF (AAIN(IX3).EQ.'@') THEN
                      INX=IX3+1
                      CALL PARSIT
                      if (ifl(2).gt.0) then
                        ldtext = .false.
                        call error(ifl(2))
                        goto 99999
                      endif
                      IX3=INX-1
                      IF (ITYP.EQ.2.AND.IST.EQ.2) THEN
C                                IT'S A SCALAR TO BE CONVERTED TO TEXT
                          IF (IFL(294).EQ.0) THEN
                              numdec=4
                          ELSE
                              numdec=6
                          endif
c                                create a variable length floating point format
c                                then write the variable's value to the
c                                temporary area.
                          if (tv .lt. 10000000. .and.
     x                        tv .gt. -10000000.) then
                              call varfmt(tv,numdec,fltfmt,len)
                              WRITE (CTMP,fltfmt)TV
c
c...Check for buffer overflow
c...Bobby  -  6/5/92
c
                              if (ix2+len .gt. mxc)
     x                             len = mxc - ix2 + 1
                              TCIN(IX2:IX2+len-1)=CTMP
                              IX2=IX2+len-1
                          else
                              ldtext = .false.
                              call error(366)
                              goto 99999
                          endif
c
c... Substitute text variable
c
                      else if (lstrng) then
                        len = 0
                        call gttext(str,len)
                        if (ifl(2).gt.0) then
                          ldtext = .false.
                          call error(ifl(2))
                          goto 99999
                        endif
                        if (ix2+len .gt. mxc)
     x                      len = mxc - ix2 + 1
                        TCIN(IX2:IX2+len-1)=str
                        ix2 = ix2+len-1
                        ix3 = inx-1
                      ELSE
                          IX3=ISVINX-1
                      ENDIF
                  ENDIF
                  if (ix3 .ge. mxc) go to 491
                  IX3=IX3+1
                  IX2=IX2+1
49            CONTINUE
491           CIMAGE(7:) = TCIN(1:mxc-6)
              nccimg = strlen1(cimage)
              ldtext = .false.
          else if (ist.eq.1043) then
              j = 4
              k=5
 
c                  if plotting to tek allow 1 more space for delimiters
              if (ifl(127).gt.2 .and. ifl(127).lt.7) k=4
 
c                  if we are plotting to a tek term & no motion seg open,
c                  open one
              if(ifl(242).eq.0.and.ifl(127).ge.4.and.ifl(127).le.5)then
                ltemp(1)='s'
                ltemp(2)='o'
c               call tkconv(ifl(169),ifl(169),ltemp(3),2)
c               call pltout(3,ctemp,5)
                ifl(242)=ifl(169)
                ifl(169)=ifl(169)+1
              endif
              do 50 i=8,nccimg,1
                  ltemp(i-k)=aain(i)
50                if (aain(i).ne.' ') j = i-3
c             if (ifl(189).eq.0) call pltout (2,pu,3)
              if (ifl(127).eq.0) then
c                 if (ifl(189).eq.0) call pltout (2,cletx,4)
                  ltemp(1)='l'
                  ltemp(2)='b'
                  ltemp(j-1)=cletx(3)
                  ltemp(j)=cletx(4)
c                 if (ifl(189).eq.0) call pltout (2,ctemp,j)
              else if (ifl(127).eq.1) then
c                 if (ifl(189).eq.0) call pltout (3,int1,5)
                  j=j-1
                  ltemp(1)='t'
                  ltemp(2)=''''
 
c                      check for apostrophies or double quotes that require
c                      special handling
                  acnt=3
54                if (acnt.ge.j) go to 56
                  hi=j-1
                  if (ltemp(acnt).eq.'''') then
                      do 57 k=hi,acnt,-1
57                        ltemp(k+1)=ltemp(k)
                      j=j+1
                      acnt=acnt+1
                  endif
55                acnt=acnt+1
                  go to 54
 
56                ltemp(j)=''''
c                 if (ifl(189).eq.0) call pltout (3,ctemp,j)
c                 if (ifl(189).eq.0) call pltout (3,int2,5)
 
c                  tektronix
              else if (ifl(127).gt.2 .and. ifl(127).lt.7) then
                  letrng = .true.
                  ltemp(1)='l'
                  ltemp(2)='t'
                  lgth=j-4
                  c5temp = ' '
c                 call tkconv (lgth,lgth,c5temp,2)
                  if (lgth.lt.16) then
                      j=j-1
                      ldig(1)=c1temp(1)
                  else
 
c                        shift text string 1 place higher to make place
c                        for 2 byte length code
                      do 58 i=j,4,-1
58                        ltemp(i)=ltemp(i-1)
                      ldig(1)=c1temp(1)
                      ldig(2)=c1temp(2)
                  endif
c                 if (ifl(189).eq.0) call pltout (3,ctemp,j)
              else if (ifl(127).eq.2) then
                  letrng = .true.
                  ltemp(1) = clesc
                  ltemp(2) = clus
c                 if (ifl(189).eq.0) call pltout (3,ctemp,j-2)
              endif
              if (ifl(127).eq.0) then
                  write (ctemp,1060) lastx,lasty,lastpn
1060              format ('pa',i6,',',i6,';',a2,';')
c                 if (ifl(189).eq.0) call pltout (2,ctemp,19)
              else if (ifl(127).eq.1) then
                  write (ctemp,1061) xsv125,ysv125
1061              format ('p[',i6,',',i6,']')
c                 if (ifl(189).eq.0) call pltout (3,ctemp,16)
              else if (ifl(127).gt.1 .and. ifl(127).lt.7) then
c                 call tkconv (lastx,lasty,lplt5,1)
                  if (ifl(127).eq.2) then
                    lstplt(2)=lstplt(3)
                    lstplt(3)=lstplt(4)
                    lstplt(4)=lstplt(5)
                  endif
                  lmpu=.true.
              endif
              if (lastpn.eq.'pd') ifl(128)=1
              i2sc18(4)=0
              ifl(189)=0
          endif
c               Don't use equivalence of 'a' to 'cimage' because of
c               alignment problems on SGI.            ijd 31-jan-89
c
c...Don't allow for more than 66 chars for text statement
c...due to PPRINTs in INCLUD files
c...Bobby - 7/8/03
c
          ach(1:66)=cimage(7:nccimg)
          ach(67:72) = ' '
          call putcl (icl,isubcl,10,a(1))
          if (ist.eq.1043) then
              call putcl (2500,5,1,sc(1))
              if (lastpn.eq.'pd') call putcl (2000,130,1,temp)
              if (lastpn.eq.'pu') call putcl (2000,129,1,temp)
          endif
          go to 99999
3099  if (.not.(ist.eq.1087)) go to 3199
c                                                        *** set/mode
          isv=inx
          call parsit
          if (ist.ne.1003) then
c                                    it's not 'mode - go put it in the cl file
              inx=isv
              nextyp=9
              go to 99
          endif
          call parsit
          if (ist.eq.75.or.ist.eq.76) then
              if (ist.eq.75) then
                  itemp(1)=1
              else
                  itemp(1)=0
              endif
          else
              inx=isv
              nextyp=9
              go to 99
          endif
          if (nextyp.ne.11) then
              call parsit
              if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3.or.ityp.eq.4) then
                  itemp(2)=1
                  if (itv.ne.0.and.itv.ne.1) then
                      call error(170)
                      go to 99999
                  endif
              else
                  call error(7)
                  go to 99999
              endif
         else
             itemp(2)=1
         endif
         ifl(144)=itemp(2)
         ifl(94)=itemp(1)
c
c         Do not put the set/mode,.. command in the clfile. kathy
c
         if (ist.eq.75.or.ist.eq.76) goto 99999
         temp=0
         inx=isv
         nextyp=9
         go to 99
 
c                                                    *** end
3199  if (.not.(ist.eq.499)) go to 3299
          call putcl(icl,1,1,sc(11))
          go to 3899
c                                                    *** at
3299  if (.not.(ist.eq.189)) go to 3399
          call error(13)
          go to 3899
c                                                    *** penup
3399  if (.not.(ist.eq.129)) go to 3499
          if (.not.(nextyp.eq.11)) go to 2099
              call putcl (2000,129,1,temp)
              ctemp='pu;'
              lastpn='pu'
              ifl(128)=0
              i2sc18(2)=1
300           continue
              if (ifl(268).eq.1) call pltout (2,ctemp,3)
              go to 2199
2099      continue
310           call error (4)
              go to 99999
2199      continue
          go to 3899
c                                                    *** pendwn
3499  if (.not.(ist.eq.130)) go to 3599
          if (nextyp.eq.11) then
              call putcl(2000,130,1,temp)
              ctemp='pd;'
              lastpn='pd'
              ifl(128)=1
              i2sc18(2)=1
              go to 300
          else
              go to 310
          endif
          go to 3899
c          scale, xyplan, yzplan, zxplan, xyzpln, nomore, xaxis, yaxis
c          zaxis, xcoord, ycoord, zcoord, solid, dash, dotted, ctrlin,
c          ditto, pen, penup, pendwn, lintyp, thru,  letsiz,
c          letdir or letorg are invalid as standalone commands
3599  if (.not.(ist.eq.25.or.(ist.ge.33.and.ist.le.45).or.ist.eq.53.or.
     1        (ist.ge.84.and.ist.le.86).or.(ist.ge.116.and.ist.le.131)
     2         .or.ist.eq.152.or.((ist.ge.1040.and.ist.le.1042).or.
     3         ist .eq. 1091)))
     3         go to 3699
          call error (182)
          go to 99999
3699  if (.not.(ist.eq.1009)) go to 3749
 
c                                        *****    feed rate
          nextsv=nextyp
          call parsit
          if ((ityp.eq.2.and.ist.eq.2.or.ityp.eq.3.or.ityp.eq.4).or.
     1         (ityp.eq.1.and.ist.eq.73.or.ist.eq.74)) then
                if (ityp.eq.1) then
                  itemp(is4)=ist
                  d(51)=temp
                  isc133(1)=2
                  isc133(2)=ist
                else
                  d(51) = tv
                  r4133(2) = tv
                  isc133(1) = 0
                  if (ifl(215).gt.0) isc133(1) = 1
                endif
                numr8s = 2
                if (nextyp.ne.11) then
 
c                        check for ipr or ipm  (aii format)
                    call parsit
                    if (ityp .eq. 1 .and.isc133(1).ne.1.and.
     x                 (ist .eq. 73 .or. ist .eq. 74)) then
                        itemp(is4) = ist
                        d(52) = temp
                        numr8s = 3
                        isc133(2) = ist
                    else if ((ityp.eq.2.and.ist.eq.2.or.(ityp.eq.3.or.
     1                      ityp.eq.4)).and.isc133(1).eq.2) then
                        if (ifl(215) .eq. 0) isc133(1) = 0
                        numr8s = 3
                        d(52) = tv
                        r4133(2) = tv
                    else
                        call error (4)
                        go to 99999
                    endif
                else if (isc133(1).eq.1) then
                    isc133(2) = 0
                else if (isc133(1).eq.2) then
                    call error(7)
                    go to 99999
                endif
                if (ifl(215).eq.0) then
c                       not an implied check surface range. out put fedrat
                    call putcl (icl, isubcl, numr8s, d(51))
 
c                       save fedrat as primary in sc(123)   epm 1-22-85
                    sc(123) = r4133(2)
                    call setfed (sc(123))
                    FEEDR = sc(123)
                endif
          else if (ityp .eq. 1 .and. (ist .eq. 189 .or.
     -             ist .eq. 9 .or. ist .eq. 653 .or. ist .eq. 325)) then
c                           fedrat/at
                call pstwfr (i)
                if (i .ne. 0) then
                   inx=isvinx
                   nextyp=nextsv
                   go to 99
                endif
c
C               isc10(1)=1009
C               call parsit
C               if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4)
C    1                then
C                    sc(11)=tv
C               else
C                    call error (7)
C                    go to 99999
C               endif
C               if (nextyp.ne.11) then
C                   if (tv.le.0) then
C                       call error(272)
C                       go to 99999
C                   endif
C                   call parsit
C                   if (ityp.eq.1.and.ist.eq.25) then
C                       isc10(2)=1
C                       call parsit
C                   endif
C                   if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp
C    1                  .eq.4) then
C                       sc(12)=tv
C                   else
C                       call error(7)
C                       go to 99999
C                   endif
C                   if (nextyp.ne.11) then
C                       call parsit
C                       if (ityp.eq.1.and.ist.eq.325) then
C                           isc10(3)=1
C                           if (nextyp.ne.11) then
C                              call error (4)
C                              go to 99999
C                           endif
C                       else
C                           call error(4)
C                           go to 99999
C                       endif
C                   endif
C              else
C                   if (sc(11).ne.0) then
C                       call error(273)
C                       go to 99999
C                   endif
C              endif
          else
               inx=isvinx
               nextyp=nextsv
               go to 99
          endif
          go to 3899
c
c...TOOLPN
c
 3749 if (ist .eq. 1053) then
          call svpars
          do 3750 i=1,9,1
              call parsit
              if (.not. scalar) then
                  call error (7)
                  go to 3899
              endif
 3750     continue
          if (nextyp .ne. 11) then
              call parsit
              call error (4)
              go to 3899
          endif
          call rtpars
          go to 3849
      endif
c             handle all other postprocessor commands
3849  continue
c                 if we are in an implied check surface range, post
c                 commands are not allowed.   epm  1-29-86
          if (ifl(215).ne.0) then
               call error(302)
c
c...Added check for NCL-VT mode
c...Paul  -  10/3/91
c...Old version was:
c   if (ifl(35).eq.0) then
c
               if (ifl(35).eq.0 .or. ifl(35) .eq. 2) then
                   call nclf_src_rec_to_line (ifl4(13),nline)
                   ifl(215) = 4
               endif
               go to 99999
          endif
          if (nextyp.eq.9) then
              ncsfl(3)=1
              inx=inx+1
              call putcl (icl,isubcl,1,sc(11))
              go to 99999
          endif
99        first=.true.
 
c              this section was changed from using the sc table to using
c              the dtbl in the common block mocom so that up to 100
c              postprocessor minor words could be handled.
c                  m. gump 8-8-83
          numr8s = 1
          pstnam = ' '
          do 100,i=51,101
              if (nextyp.eq.11) then
                  call putcl (icl,isubcl,numr8s,d(51))
                  if (isubcl.eq.1015.and.pstnam.ne.' ') then
                      nc     = strlen1(pstnam)
                      call lpstnm (pstnam,nc)
                      if (pstnam .eq. 'PWORKS' .and. numr8s .ge. 3)
     1                    pstnum = d(52)
                  endif
                  go to 99999
              endif
              if ((nextyp.ne.11).and. (i.eq.101)) then
                  call error(237)
                  go to 99999
              endif
 
c                  if there is a comma or
c                  there is a slash and this is the first minor word
              idtype = -1
              if (nextyp.eq.9) then
                  call parsit
              else if (nextyp.eq.5 .and. first) then
                  savinx=inx
                  savitp = ityp
                  savist = ist
                  call parsit
C
C...If this is ARCSLP/FILLET we need to set a flag and
C...set the fillet radius and the fillet tolerance.
C...JLS 4/14/99
C
                  if (savitp.eq.1.and.savist.eq.ARCSLP.and.
     x                ityp.eq.1.and.ist.eq.FILLET.and.
     x                ifl(351).eq.1) then
                     call parsit
                     lres = .false.
c
c......ARCSLP/FILLET,SAVE
c
                     if (ityp .eq. 1 .and. ist .eq. 582) then
                       sflrad = FILRAD
                       sfltol = FILTOL
                       sflwrn = LFLWRN
                       sflcom = LFLCOM
                       sflsam = LFLSAM
                       sflang = FILANG
                       sfldco = IFEDCO
                       sfldhi = FEDHIG
                       sfldlo = FEDLOW
                       sfl347 = ifl(347)
                       go to 99999
c
c......ARCSLP/FILLET,RESTOR
c
                     else if (ityp .eq. 1 .and. ist .eq. 583) then
                       tflrad = sflrad
                       tfltol = sfltol
                       tflwrn = sflwrn
                       tflcom = sflcom
                       tflsam = sflsam
                       tmang  = sflang
                       IFEDCO = sfldco
                       tlfed = .false.
                       if (sfldco .ne. 0) tlfed = .true.
                       FEDHIG = sfldhi
                       FEDLOW = sfldlo
                       if (sfl347 .eq. 0) then
                           ifl(347) = 0
                           ifl(348) = 0
                           go to 99999
                       endif
                       lres = .true.
                       scalar = .true.
                       tv = sflrad
                     endif
c
c......ARCSLP/FILLET,rad
c
                     if (.not.scalar) then
                       call error(7)
                       goto 99999
                     endif
                     if (tv.lt.0.0d0) then
                       call error(112)
                       goto 99999
                     endif
C
C...if tv is not equal to 0 then there is a fillet radius
C...and the flag should be set.
C
                     if(tv.gt.0.0) then
cc                        motdfl = .false.
                        if (.not. lres) then
                          tflrad = tv
                          tfltol = sc(27)
                          tflwrn = lflwrn
                          tlfed  = .false.
                          tflcom = .false.
                          tflsam = .false.
                          tmang  = dcos(2.0d0/57.2957795d0)
                        endif
C
C...Parse optional parameter portion of command
C
                        if (nextyp.ne.11 .and. .not. lres) then
                           call filprs (tflrad,tfltol,tlfed,tflwrn,
     1                       tflcom,tflsam,tmang)
                           if (ifl(2) .ne. 0) then
                               call error (ifl(2))
                               go to 99999
                           endif
                        endif
C
c...No error, store final values.
C...If we have already been making fillets and the fillet values are just
C...being changed, leave the value in ifl(347) as is
C
                        if (ifl(347) .eq. 0) then
                            ifl(347)=1
c                            irec = i4stat(2)
                            call ncl_setptr(imotp,irec)
                            iclw(3) = 0
                            iclf = 0
                            call ncl_tstptr(irec,iflg)
                            do while (iclw(3) .ne. 5000 .and.
     1                                iclw(3) .ne. 5200 .and.
     2                                iflg .ne. 0 .and. jerr .ne. 1)
c                                svrec = irec
                                call ncl_setptr(irec,svrec)
                                call clprev (iclf,irec,iclw,dclbuf,jerr)
                                call ncl_tstptr(irec,iflg)
                            enddo
c                            if (iclw(3) .eq. 5000 .or.
c     1                          iclw(3) .eq. 5200) isrec = irec
                            if (iclw(3) .eq. 5000 .or.
     1                          iclw(3) .eq. 5200)
     2                                   call ncl_setptr(irec,isrec)
                        endif
                        lcount = nline
                        filrad = tflrad
                        filtol = tfltol
                        lflwrn = tflwrn
                        lflcom = tflcom
                        lflsam = tflsam
                        filang = tmang
                        FILFLG(1) = 0
                        if (.not.tlfed) IFEDCO = 0
                        goto 99999
C
C...The ARCSLP/FILLET command was found but the radius is zero, so
C...set flag back to zero.
C
                     else
cc                        if (ifl(347).ne.0) then
cc                           call motend
cc                        endif
                        ifl(347)=0
                        ifl(348)=0
                        filrad = 0.
                        FILFLG(1) = 0
cc                        motdfl = .true.
                     endif
C
C...Reset inx to finish processing the command.
C...We don't want the actual command to go into the clfile so
C...return back and start on the next entry.
C
                     goto 99999
                  endif
 
                  first=.false.
                  if ((ityp.eq.1 .or. ityp.eq.2) .and.
     1                 isubcl.eq.1015) then
                      ityp=4
                      pstnam=token2
                      nc     = strlen1(pstnam)
                      if (nc .gt. 8) then
                          ie     = (nc+7)/8 - 1
                          jtemp(1) = -1
                          jtemp(2) = nc
                          d(50+numr8s) = temp
                          numr8s = numr8s + 1
                          do 75 j=1,ie,1
                              d(50+numr8s)=req1(j)
                              numr8s = numr8s + 1
   75                     continue
                          tv = req1(ie+1)
                      else
                          tv=req1(1)
                      endif
                  endif
              else
                  isvinx=inx
                  if (first) then
                      call error (22)
                  else
                      call error (57)
                  endif
                  go to 99999
              endif
c                      its a vocab word. it needs to be stored in the
c                      low order integer*2 for the post to be able to
c                      distinguish it from a scalar value
              if (ityp.eq.1) then
                  itemp(is4)=ist
                  d(50+numr8s)=temp
              else if ((ityp.eq.2 .and. ist.eq.2) .or.
     1                  ityp.eq.3 .or. ityp.eq.4) then
                  d(50+numr8s)=tv
c                     for "clrsrf/" commands:
c                      expand a plane id to its 4 canonical parameters
              else if (isubcl .eq. 1057 .and.
     x                 ityp.eq.2 .and. (ist.eq.6 .or. ist.eq.9)) then
                  if (ist.eq.SURF) then
                    call gtdesc(tv,nclkey,nw,ietype)
                    call ncl_get_sf_primtyp(nclkey,primtyp)
                    if (primtyp.ne.3) then
                      call error(19)
                      go to 99999
                    endif
                  endif
                  call gtplt (tv, ifl(72), d(50+numr8s))
                  numr8s = numr8s + 3
              else
                  call error(7)
                  go to 99999
              endif
              numr8s = numr8s + 1
100       continue
          call error(237)
3899  continue
88888 continue
99999 continue
 
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pstwfr (ierr)
c*       this routine parses feed rate command with AT and OUT
C*       qualifiers.  These are not post processor commands.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          ierr  - 0 = ncl FR/AT command processed (OK or not is
C*                  meaningless), 1 = assumed pp command.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
c**********************************************************************
      subroutine pstwfr (ierr)
c
      include 'com8a.com'
      include 'comgt.com'
c
      integer*4 nsw,iscal,idw(4),is,i,nss
      integer*2 isv,i1,i2,ierr
c
      real*8 r(4)
c
      if (ityp .ne. 1) go to 8000
      i1     = ifl(314)
      i2     = ifl(315)
      ierr   = 0
c
c...FEDRAT/AT...
c
      is     = -1
      nsw    = 0
      nss    = 0
      do 55 i=1,4
         idw(i) = 0
   55 continue
c
c...Get key word of command
c
   90 if (is .ge. 0) call parsit
      if (ityp .ne. 1) go to 9000
      iscal  = 0
      if (ist .eq. 189) then
          nsw   = 1
      else if (ist .eq. 653) then
          nsw   = 2
      else if (ist .eq. 325) then
          nsw   = 3
      else if (ist .eq. 9) then
          nsw   = 4
      else
          go to 9000
      end if
      if (idw(nsw) .eq. 1) go to 9100
      idw(nsw) = 1
c
c...AT or OUT must be the first minor word of the valid NCL command,
c...otherwise it is postprocessor command
c
      if (is .lt. 0 .and. idw(1)+idw(2) .eq. 0) go to 9990
      is     = 0
c
c...Get parameters
c......for AT or OUT
c
      go to (100,100,300,400) nsw
  100 if (nextyp .eq. 11) then
          if (nsw .eq. 2) go to 190
          go to 9300
      endif
  110 isv   = inx
      call parsit
      is    = is + 1
c
c......,distance
c
      if (scalar) then
         r(is) = tv
c
c......,[SCALE],feed
c
      else if (is .eq. 2) then
         if (ityp .eq. 1) then
            is     = is - 1
            if (ist .eq. 25) then
               iscal  = 1
            else
               go to 190
            end if
         else if (scalar) then
            r(is) = tv
         else
            go to 9200
         end if
      else if (ityp .eq. 1) then
         is    = is - 1
         if (nextyp .eq. 11) nextyp = 12
         go to 190
      end if
      if (is .gt. 3) go to 9400
      if (nextyp .ne. 11) go to 110
c
c......End of AT (or OUT) section,
c......store parameters in common area
c
  190 if (is .lt. 1) then
          if (nsw .eq. 2 .and. nss .eq. 1) then
              FEDIS(2) = FEDIS(1)
              lfl(85) = lfl(84)
              ifl(315) = ifl(314)
              FEEDC(2+3*iscal) = r(2)
c
              if (nss .eq. 1) nss = nsw
              idw(3) = 0
              is     = 0
              if (nextyp .eq. 11) go to 7000
              inx    = isv
              go to 90
          else
              go to 9200
          endif
      endif
c
      if (is .eq. 1) then
         if (r(is) .ne. 0.0) go to 9300
         ifl(313+nsw) = 0
      else
         if (r(1) .lt. 0.0) go to 9500
         FEDIS(nsw) = r(1)
         if (r(2) .lt. 0.0) go to 9600
         lfl(83+nsw) = iscal .eq. 1
         ifl(313+nsw) = 1 + idw(3)
c        if (r(1) .eq. 0.0) ifl(313+nsw) = 0
         FEEDC(nsw+3*iscal) = r(2)
         if (is .eq. 3) then
             if (r(3) .lt. 0.0) go to 9600
             sc(123) = r(3)
             call fedmut (sc(123))
         end if
      end if
c
c......This part is for version < 8.209
c
      if (nsw .eq. 1) then
         isc10(1) = 1009
         sc(11) = r(1)
         sc(12) = r(2)
         if (iscal .eq. 1) isc10(2) = 1
         if (idw(3) .eq. 1) isc10(3) = 1
      end if
c
c......Prepare for tagged ONCE
c
      if (idw(3) .eq. 0) nss = nsw
      idw(3) = 0
c
      is     = 0
      if (nextyp .eq. 11) go to 7000
      inx    = isv
      go to 90
c
c......ONCE
c......apply for parsed AT or OUT
c
  300 if (nss .eq. 1) then
          isc10(3) = 1
          ifl(314) = 2
          idw(3) = 0
      else if (nss .eq. 2) then
          ifl(315) = 2
          idw(3) = 0
      end if
c
c......or save it for following AT or OUT
c
      nss    = 0
      if (nextyp .eq. 11) go to 7000
      go to 90
c
c......LENGTH,hig
c
  400 if (nextyp .eq. 11) go to 9300
      call parsit
      if (scalar) then
         FHIGT = tv
      else
         go to 9200
      end if
      if (nextyp .eq. 11) go to 7000
      go to 90
c
c...Set common values
c
 7000 if (sclat) FEEDC(1) = FEEDC(4) * sc(123)
      if (sclout) FEEDC(2) = FEEDC(5) * sc(123)
      FEEDC(3) = sc(123)
c
c...Save parameters for after 'once'
c
      if (ifl(314) .eq. 1) then
         FEEDS(1) = FEEDC(1)
         FEEDS(4) = FEEDC(4)
         FEDIS(3) = FEDIS(1)
         sclats = sclat
      else if (ifl(314) .eq. 2 .and. i1 .ne. 2) then
         ifl(320) = i1
      end if
      if (ifl(315) .eq. 1) then
         FEEDS(2) = FEEDC(2)
         FEEDS(5) = FEEDC(5)
         FEDIS(4) = FEDIS(2)
         sclous = sclout
      else if (ifl(315) .eq. 2 .and. i2 .ne. 2) then
         ifl(321) = i2
      end if
c
c...End of routine
c
 8000 return
c
c...Errors
c
 9000 ifl(2) = 61
      go to 9900
c
 9100 ifl(2) = 8
      go to 9900
c
 9200 ifl(2) =  7
      go to 9900
c
 9300 ifl(2) = 273
      go to 9900
c
 9400 ifl(2) = 4
      go to 9900
c
 9500 ifl(2) = 272
      go to 9900
c
 9600 ifl(2) = 112
 9900 call error (ifl(2))
      go to 8000
c
c...Output as pp command
c
 9990 ierr = 1
      go to 8000
c
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtfedp (gfed,gdis,ghi,nfl,nscl)
c*       this routine passes feed rate parameters to C routines.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
c**********************************************************************
      subroutine gtfedp (gfed,gdis,ghi,nfl,nscl)
c
      include 'com8a.com'
      include 'comgt.com'
c
      real*8 gfed(6),gdis(2),ghi
      integer*2 nfl(2),nscl(2)
c
      do 55 i=1,6
         gfed(i) = FEEDC(i)
  55  continue
      gdis(1) = FEDIS(1)
      gdis(2) = FEDIS(2)
      nfl(1)  = ifl(314)
      nfl(2)  = ifl(315)
      nscl(1) = 0
      nscl(2) = 0
      if (sclat) nscl(1) = 1
      if (sclout) nscl(2) = 1
      ghi     = FHIGT
c
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine stclix (is1,is4)
c*       This routine sets the indices for clfile post word values.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          is1    - first index.
C*          is4    - second index.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
c**********************************************************************
      subroutine stclix (is1,is4)
c
      include 'com8a.com'
c
      integer*2 is1, is4
c
      is1=4
      is4=1
c
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine stclix (is1,is4)
c*       Parses the REMARK statement and determines if it is a
C*       REMARK/ON or REMARK/OFF command.
C*    PARAMETERS
C*       INPUT  :
C*          cstr   - REMARK text string, including REMARK word (cin).
C*       OUTPUT :
C*          kfl    - 0 = Standard REMARK command, 1 = REMARK/ON,
C*                   2 = REMARK/OFF.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C**********************************************************************
C
      subroutine prsrem (cstr,kfl)
c
      include 'com8a.com'
c
      integer*2 kfl
c
      character*(*) cstr
c
      integer*4 nc,strlen1
c
      character*72 lstr
c
c...Remove spaces and convert to upper case
c
      call remspc (cstr(1:72),lstr)
      call touppr (lstr,lstr)
c
c...Check for REMARK/ON,OFF
c
      nc = strlen1(lstr)
      if (nc .eq. 9 .and. lstr(7:9) .eq. '/ON') then
          kfl = 1
      else if (nc .eq. 10 .and. lstr(7:10) .eq. '/OFF') then
          kfl = 2
      else
          kfl = 0
      endif
c
c...End of routine
c
 8000 return
      end
