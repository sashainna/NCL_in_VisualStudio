C*********************************************************************
C*    NAME         :  driver.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) Numerical Control Computer Sciences Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*        driver.f , 25.1
C*     DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:09:58
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine driver
C*     this program is the main driver.  it's functions include:
C*         all user interface                                        
C*         syntax checking of user input commands                    
C*         task dissemination to sub programs                        
C*         source file maintenance                                   
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
c...Added check for NCL-VT mode (for *START command)
c...Paul  -  10/25/91
c...Old version was:
c   subroutine driver  
c
      subroutine driver(jquit)

      include 'com8a.com'
      include 'comgt.com'
      include 'mocom.com'
      include 'status.com'

      common/dstcom/keydt, ixdt, nxthld, nxtret
      integer*4 keydt
      integer*2 ixdt, nxthld, nxtret

c       Shared between driver and linchk.f for check surface 
c          (in case of error). Kathy
      common /fl217/ isvfl
      integer*2 i2sc15(4),lastx,lasty,endx,endy,stat,isc11(4)
      equivalence (i2sc15,lastx,sc(115)),(lasty,i2sc15(2))
      equivalence (endx,i2sc15(3)),(endy,i2sc15(4))
      equivalence (isc11,sc(11))
      equivalence (isvmod,ifl(48))
      integer*2 found1,ieqtv(4),skpsyn,isvf
      equivalence(ifl(120),found1),(ieqtv,tv),(skpsyn,ifl(175))

      integer*2 ipb,maxipb,iwhich
      equivalence (ifl(185),ipb),(ifl(186),maxipb)
      logical lsc122(8)
      character*1 lstplt(5)
      integer*4 cinx155,cinx159,nlin6,nlin5
      character*64 csc155,csc159,mcname
      character*(MAX_LEN) tcin
      equivalence (sc155,csc155),(sc159,csc159),(sc165,mcname)
      equivalence (inx155,cinx155), (inx159,cinx159)
      character*5 lplt5
      equivalence (sc(122),lsc122),(lsc122,lstplt,lplt5)
      integer*2 isc133(4),nr8s,iferr
      real*4 r4133(2),isvpm
      real*8 FEEDR,rte(9)
c
c...add flag to check if a macro started
c...yurong
c
      integer*2 macstr
      equivalence (sc(133),isc133),(sc(133),r4133),(FEEDR,MOTMAP(24))
      integer*4 nclkey,jgrec(2),jst(2),ncc
      character*64 c8buf,lmsg
      integer*2 nwds,ietype, isvfl,i,j,k, iid, idum, inot
      logical lgeo,lv92
      integer*2 IFV,THENV,ELSEIFV,ELSEV,ENDIFV,MOVEV,REVERSV, DATAV
      integer*2 SMILL,PROFIL,COMBIN,PMILL
      parameter (MOVEV   = 577)
      parameter (REVERSV = 1008)
      parameter (IFV     = 825)
      parameter (THENV   = 938)
      parameter (ELSEIFV = 939)
      parameter (ELSEV   = 940)
      parameter (ENDIFV  = 941)
      parameter (SMILL   = 756)
      parameter (PROFIL  = 761)
      parameter (COMBIN  = 1071)
      parameter (VMPOCK  = 765)
      parameter (DATAV   = 858)
      parameter (PMILL   = 1051)


c          ifl(1)=0   first entry
c                =1   ret from geogn2
c                =2   ret from mocntl

      isvfl = 0
      pmode=0
      isvmod=0
      jquit=0
      echo=.true.
      stat = 0
      macstr = 0
      nhline = 1
      ncline = 1
c
c...Start of driver loop
c
   10 lv92 = sc(169) .lt. 9.250          
      call resetmacpt
      call ud_send_nclinfo
c
c...update time
c
	call uz_acttime

      if (((ifl(35).eq.0 .and. ifl(350) .ne. 2)
     x    .or. ifl(35).eq.2) .and. .not.ksr .and.
     x    (isvmod.ne.ifl(37) .or. ifl(152).ne.ifl(153))) then
          call domode
      endif
c
       if (cimage(1:7) .ne. '*return' .and.
     x .not. err .and.
     x ifl(2) .ge. 0 .and.
     x ((ifl(35).eq.0.and.ifl(350).ne.2).or.ifl(35).eq.2)) call statln
 
          if (ifl(35).eq.1) then
c              batch mode, check error and warning limits
          if (ifl(34).ge.ifl(32)) then
              call error(184)
              go to 20
          else if (ifl(33).ge.ifl(31)) then
              call error(183)
              go to 20
          endif
      endif
c     if (ifl(100).eq.1) then
c              save ending plot coordinates for interactive plotting
c         endx=lastx
c         endy=lasty
c     endif
c           flush the plotting buffer
      if (ipb.gt.0) call flshbf
      lmlt = .false.

c         To correct the line numbering in *skip/to,no.
      lskip = .false.
c
c...   Turn off data statement & inclusive thru flags
c
      irapid = 0
      ifl(324) = 0
      ifl(325) = 0
      ifl(388) = 1

      ifl(105)=0
      thrlbl = ' '
      ifl(107)=0
      ifl(192)=0
      ifl(86)=0
      ifl(296) = 0
      defwf = .false.
      isvf=0
      isvpm = pmode
      ifl(378) = 0
      if (stat.eq.0) then
          svll=nline
          ifl(253) = 1
          ifl(257) = 1
c
c...Flush the buffer every time an NCL command is executed (VISTA)
c
          if (ifl(35) .eq. 0) call nclf_flush_buffer()
          call getmsg(jquit)
c
c...Added check for NCL-VT mode
c...Paul  -  10/25/91
c
          if (jquit.eq.2 .and. ifl(35) .eq. 2) goto 99999

          if (jquit.eq.1) then
c
c.....Added for *SET/RUNCMD to enter command mode at the end of 
c.....file or when line number to run to is reached 
c          
            if (ifl(392).eq.1.and.isvpm.ne.pmode) then
              call ncl_cmd_mode()
            endif
            goto 99999
          endif
          ifl(253) = 0
          ifl(257) = 0
      else
          stat=0
      endif
c
c...Increased the number of input lines
c...and buffer size
c...Bobby  -  3/1/94
c
      if (ifl(123).gt.49.or.inx.gt.1535.or.
     1    (ifl(343) .eq. 1 .and. inx .gt. 1520)) then
          call error(209)
          go to 10
      endif
c
c..... expand text variables in curly braces
c
      call substx

c      if (ifl(38).ne.1.and.ifl(45).ne.1) then
c             it's not defining a loop or a macro so look for nested geo
          ifl(111)=1
          found1=0
cccccccccccccccccccccccccccccccccccccccc
          call chknst(found1)
cccccccccccccccccccccccccccccccccccccccc
1099      if (.not.(err.or.ifl(2).ne.0)) go to 1199
              if (ifl(35).eq.1.and.ncsfl(6).eq.0.and..not.srceof)
     x            call prtput
              if (err) go to 50000
1199      continue
c      endif
      ifl(111)=0
      inx=1
18    if (.not.(ifl(35).eq.1.and.srceof)) go to 3099
20        ifl(1)=99
          go to 99999
3099  continue
35    if (ain(inx).eq.';') then
        inx = inx+1
        goto 35
      endif
      ifl(44)=5
      nxtret = 0
      ldtflg = .true.
      ldtext = .true.
      call parsit
      ldtflg = .false.
      ldtext = .false.
      if (ncsfl(3).ne.0) then
          ncsfl(3)=0
          lmlt = .true.
c
c... The following statement was added for IF-THEN-ELSE implementation
c... but it breaks multiple statements in a DATA statement e.g. RP,GD/..
c          ifl(324) = 0
      else
          if (ifl(35).eq.1.and.ncsfl(6).eq.0.and..not.srceof)
     1        call prtput
      endif
c
c...REMARK
c...Check for REMARK/ON
c...REMARK/OFF is handled in 'parser'
c
      if (ist .eq. 1091) then
          call prsrem (cin,i)
          if (i .eq. 1) then
              ifl(383) = 1
              if (pgindx .lt. 30) pgindx = pgindx + 1
              pgmode(pgindx) = 5
          endif
          if (.not. aptrem) go to 5499
      endif

c         the (nextyp .eq. 7 and ain(inx+1) .ne. '=') test is to ensure a 
c         subscripted geometry or scalar name found at the beginning of a 
c         statement during the definition processing of a looping region 
c         does not get mistaken for a statement label that is terminated 
c         with a closing parenthesis.  ")"
c             M. Gump 8-16-89
c
c...   if LANG= CADRA then a variable of vocab word can be declared as a label
c
      if ((nextyp.eq.13.or.(nextyp.eq.7.and.ain(inx+1).ne.'=')).and.
     1    ((ist.eq.1.or.ist.eq.13).or.ityp.eq.3.or.
     2     (ifl(374) .eq. 0 .and.(ityp.eq.1.or.ityp.eq.2))))  then
c                                                    *******  label
          if (ifl(38).eq.0.and.ifl(45).eq.0) then
              ist=13
              call error(79)
              go to 10
          endif
c                 if we are executing a do loop and this label was the
c                 terminating label for the previous do loop, it's an
c                 overlapping error.
          if (ifl(286).gt.1.and.
     1         ((csc159.eq.token2).and.(cinx155.eq.ivxsub))
     2         .and. ((csc159.ne.csc155).or.(cinx159.ne.cinx155)))
     3              then
              call error(350)
              go to 10
          endif
          if (ityp.eq.3) call vstchk
          call declbl
          if (err) go to 10
          if ((token2.eq.csc155).and.(ivxsub.eq.cinx155)) then
c                   this is a label that ends a do loop
              isvf=1
          endif
          inx=inx+1
          ilblf = 1
          call parsit
c          if (ncsfl(6) .eq. 0) call prtput
      endif
      if (.not.(ityp.eq.5.and.(ist.eq.4.or.ist.eq.10))) go to 4059

c                                        if it's a '*' command,
c                                        call utilty and return
          stat=0
          call utilty (stat)

c              stat=4 means it wasn't a valid utility command type
c              statement so it was stripped of the '*' and is now
c              ready to be processed as a non-utility type statement
c              but not put in the part program file
          if (stat.eq.4) then
              stat=0
c
c...reparser the statement, we need reset those value
c
              wildnum = 0
              geomnum = 0
              ifl4(107)=0
              go to 18
          endif
c
c...Don't get new source line
c...if '*' command is on a DO loop
c...terminating label
c...Bobby  -  7/9/91
c
c
c...VX
c
          if ((stat .eq. 0 .and. ist .ne. 10 .and. isvf .ne. 1) .and.
     1        (ifl(35) .ne. 2 .or. ifl(322) .ne. 1)) then
c
              if (.not.lskip) then
                  nline = svll
              endif
c
c... If the line in the source file is the same as the control command just 
c... executed, update nline to the next line. Deblank and uppercase in case
c... source formatting is in effect.
c
              if (.not. err) then
                call getsrc (cin, ncc, nline-1,0)
                if (.not. srceof) then
                  j = 0
                  do i=1,ncc,1
                    if (cin(i:i).ne.' ') then
                      j = j+1
                      cin(j:j) = cin(i:i)
                    endif
                  enddo
                  k = 0
                  call nclf_getw2 (1,w2,ncw2,irctyp)
                  do i=1,ncw2,1
                    if (w2(i:i).ne.' ') then
                      k = k+1
                      tcin(k:k) = w2(i:i)
                    endif
                  enddo
                  if (j.eq.k) then
                    call nupper(cin,1,j)
                    call nupper(tcin,1,j)
                    if (cin(1:j).eq.tcin(1:j)) 
     x                    nline = nline+ifl(123)
                  endif
                endif
              endif
          endif
          if (stat .eq. 999) then
             stat = 0
             goto 99999
         endif

c
c...Added isvf to 'stat .eq. 2' & 'fromt'
c...to allow a '*' command to be entered
c...with a DO Loop terminating label
c...Bobby  -  7/1/91
c
          if (stat.eq.2 .and. isvf .ne. 1) go to 10
          if (stat.eq.3) then
              stat=0
              go to 50000
          endif
          if (fromt .and. isvf .ne. 1) go to 10
          go to 5499
c                                  is it a 'prompt' statement?
4059  if (.not.(ist .eq. 935)) go to 4099
          call prompt2
          goto 5499
c                                  is it a 'termac' statement?
4099  if (.not.(ist .eq. 807)) go to 4199
c
c...set macro flag to 0
c...yurong
c
          macstr = 0
          if (ifl(38) .eq. 1) then
              call termac
          else if (ifl(38) .eq. 2) then
              call termac
c  
c...Added check for NCL-VT mode   
c...Paul  -  10/3/91   
c...Old version was:   
c   if (ifl(35).eq.0) call putw2(svll+ifl(123)-1)
c
c...And changed for NCL501+ mode
c...Paul - 03/17/92
c
              if ((ifl(35).eq.0.and.ifl(350).ne.2) .or. ifl(35).eq.2) 
     x        call putw2(svll+ifl(123)-1)
c
              ifl4(10) = 0
              call domode
              go to 10
          else
              call error(55)
              go to 10
          endif
          go to 5499
c
c...Changed to 4250
c...Bobby  -  6/27/91
c
4199  if (ist.eq.809) then
c                                                            ***** loopnd
          if (ifl(286).gt.0) then
              call error(348)
          else
              iwhich=1
              call loopnd(iwhich)
          endif
          go to 5499
      endif
c                                                      ***** if-then-else
      if (ist.eq.IFV.or.ist.eq.ELSEIFV .or.ist.eq.ELSEV.or.
     x    ist.eq.ENDIFV) then
c
c...  allow execution of one line IF statement outside loops/macros
c
        if (ifl(38).eq.0 .and. ifl(45).eq.0) ifl(378) = 1
        j = 0
        idum = 0
        if (ifl(38).eq.1.or.ifl(45).eq.1) idum = 1
c...using the beginning line number oof the statement for if/else jump
c
c...        call ifelse(idum, nline, j, inot)
        call ifelse(idum, bline, j, inot)
        nline = bline
        if (inot.eq.1) goto 4250
        go to 5499
      endif
c
c...FINI is not allowed in a loop or macro
c...Bobby  -  6/27/91
c
4250  if (ist .eq. 801 .and. (ifl(38) .ne. 0 .or. ifl(45) .ne. 0)) then
          if (ifl(38) .ne. 0) then
              token2 = mcname
              ivxsub = 0
          else
              call lrdata (token2,ivxsub,c8buf,tmp,i,j,k,i,j,c8buf,
     1                     tmp,1)
          endif
          call error (421)
          go to 5499
      endif


4299  if (.not.(ifl(38).eq.1.or.ifl(45).eq.1)) go to 4399
c                  we are declaring a macro or a loop
c
c...If there are macro start, it does not allow loopst and loopnd
c...error(84): STATEMENT NOT ALLOWED IN LOOP OR MACRO
c...yurong
c
      if ((macstr.eq.1).and.((ist.eq.808).or.(ist.eq.809))) then
         call error (84)
         goto 10
      endif
      go to 50000
4399  if (.not.(ist.eq.845)) go to 4499

c                                                            ***** undo
          call nclf_src_rec_to_line (ifl4(6),nlin6)
          call nclf_src_rec_to_line (ifl4(5),nlin5)
          if (ifl(286).lt.1) then
              call error(347)
          else if (nline.gt.nlin6.or.nline.lt.nlin5) then
              call error(347)
          else
              iwhich=2
              call loopnd(iwhich)
          endif
          go to 5499
c
c....Ignore CONTIN or TITLES
c
4499  if (ist .eq. 846 .or. ist .eq. 1092) go to 5499
c
c...CALL
c
4599  if (ist .eq. 824) then
          if (ifl(192).ne.0) then
c                 an unnamed nested item is in the line  -  error
              isvinx=ifl(192)
              call error(265)
          else if (isvf.ne.0) then
              isvf=0
              call error(352)
          else
              call callit
          endif
          if (.not. err) go to 10
          go to 5499
      endif
c
c...CALLS
c
      if (ist .eq. 867) then
c
c......Unnamed nested arguments not allowed
c
          if (ifl(192).ne.0) then
              isvinx=ifl(192)
              call error(265)
c
c......Open/NCL System call
c
          else
              call ncl_setptr (imotp,jgrec)
              do 4601 i=1,9,1
                   rte(i) = sc(i)
 4601         continue
              call syscal
              call dismot (jgrec,imotp,rte)
              go to 5499
          endif
          if (.not. err) go to 10
          go to 5499
      endif
c                                                            ***** print/geo
4699  if (.not.(ist.eq.817)) go to 4749
          call prtgeo
          go to 5499
c
c... jingrong 12/29/99                    ***** analyz/sf1,sf2,thru,sfn
c
4749  if (ist.ne.874) goto 4799
          call sfprim
          go to 5499
c
c...Added NURB surfaces
c...Bobby  3/5/92
c
4799  if (ityp .ne. 2) then
           call vctoid (ist,iid,idum)
      else
          iid = ist
      end if
      call ifidge (iid,lgeo)
c
c...Adding ist equal to 615 to the following statement so that
C...the command NSHAPE is processed.
c
      if (.not.(ityp .eq. 2 .or. lgeo .or. ist .eq. 576 .or.
     -    ist .eq. 577 .or. ist .eq. 1008.or.ist.eq.615)) go to 4899
c           if it's a variable symbol it must be an assignment stmt.  if
c           it's a geometry type, it must be auto name generation.
          if (lmlt.and.vocab.and.ist.ne.MOVEV.and.ist.ne.REVERSV) then
c
c...Give error if auto name generation for multiple statement
c...(statement following IF or semi-colon)
c
            call error (425)
            goto 10
          endif
          call assgn
          if (idst.eq.11)  macstr = 1
          if (err) go to 7099
          if (ifl(2).lt.0 .and. sc(169).ge.9.15d0) then
            err = .true.
            if (ityp.eq.2 .and. ist.eq.1) ist = 22
          endif
            if (ifl(262) .eq. 0) then
                  if (idst.gt.6.and.idst.lt.10.or.idst.eq.18.or.
     1                isc10(1).eq.575) then
                      if (isc10(1).eq.575) then
                         call mocntl 
                      else
                         call geogn2 
                      endif
                  else if ((idst.gt.2.and.idst.lt.7).or.idst.eq.10) then
                      call geogn1
                  elseif(idst.eq.1008.and.ifl(300).eq.1)then
                      goto 5499
                  endif
              else
                  ifl(262) = 0
              endif
80            if (.not.err) go to 6099
              isvinx=0
              call error(ifl(2))
cc              if (.not.err) go to 82
              if (ifl(2) .le. 0) go to 82
              go to 6199
6099          continue
82            if (isc10(1).eq.575.and.isc10(2).eq.1) then
                  rest=sc(13)
                  sc(10)=0
              endif
              if (.not.defwf) call vstore

c                      display defined geometry if auto display is set on
c                      unless it is a scalar or matrix
c
c...Added for NCL501+ mode
c...Paul - 02/11/92
c...Old version was:if (idst.ne.2.and.idst.ne.10.and.ifl(35).eq.0) then
c
      if (idst.ne.2 .and. ifl(329) .eq. 0 .and. (ifl(35).eq.0 .or.
     x   (ifl(35) .eq. 2 .and. ifl(350) .eq. 1))) then
                      call gtdesc(rest,nclkey,nwds,ietype)
                      call ifidge (ietype,lgeo)
                      if (lgeo) then
c
c                      if ((ietype.gt.2.and.ietype.lt.10).or.ietype.eq.18
c     x                     .or.ietype .eq. 20) then
                          call dspent(nclkey,ietype)
                      endif
                  endif
6199          continue
7099      continue
          go to 5499

c                                                            POKMOD
4899  if (ist .eq. 749) then
          call pokmod
          go to 5499
      endif
c                                                            VMPMOD
      if (ist .eq. 764) then
          call vmodsy
          go to 5499
      endif    
c                                                            DATAV 
      if (ist .eq. DATAV) then
          call assgn
          go to 5499
      endif   

      if (.not.(ityp.eq.1.and. ((ist.gt.700.and.ist.lt.742) .or.
     x                          ist.eq.745 .or.
     x                          ist.eq.748 .or.
     x                          ist.eq.750 .or.
     x                          ist.eq.SMILL .or.
     x                          ist.eq.PROFIL .or.
     x                          ist.eq.VMPOCK .or.
     x                          ist.eq.873 .or.
     x                          ist.eq.1021 .or.
     x                          ist.eq.1095 .or.
     x                          ist.eq.1090 .or.
     x                          ist.eq.PMILL))) go to 4999
c
c          * if it's a motion statement or a tlrgt,tllft or tlon followed
c          * by a motion statement, or a motion statement modifier, call
c          * the motion syntax section.  if it's a motion statement and
c          * no error has occurred, call the motion generation section.
c
c
c...Save current clfile record number for motion display
c...and current tool location for GENPTS and motion display
c
c85        jgrec = i4stat(2)
85        call ncl_setptr(imotp,jgrec)
          do 87 i=1,9,1
              rte(i) = sc(i)
   87     continue

          call mosyn
c
c       used for check surface (in case of error). Kathy
          isvfl = ifl(217)
          if (.not.(.not.err.and.sc(10).ne.0.and.ifl(215).ne.1))
     x           go to 8099
c                   only call mocntl for 501 if disply stmt.   ijd 14-sep-1987
c
c...Added check for NCL-VT mode
c...Paul  -  10/21/91
c...Old version was:
c      if(isc10(1).ne.1021.and.isc10(1).ne.1095.or.ifl(268).eq.1)
c
c
              if(isc10(1).ne.1021.and.isc10(1).ne.1095.and.isc10(1).ne.0
     x            .or.(ifl(268).eq.1 .and. ifl(35) .ne. 2)) then
C
C...Save the appropriate record number to be used to make fillets. isrec is 
C...the record to obtain the oldpts in pfillet, isrec2 and isrec3 are
C...the two records for the motions to make the fillets between.
C...If rapid, we want don't want to make a fillet.
C...JLS 4/14/99
C
                if (rpfron) then
                   irapid =1
                   ifl(348)=0
                endif
                call pfilpt (imotp,isrec,isrec2,isrec3)
c                jst = i4stat(2)
                call ncl_setptr(imotp,jst)
                call mocntl
              endif
90            if (err) then
91                isvinx=0
                  call error(ifl(2))
                  if (ifl(215).ne.0) then
c
c...We are doing an implied check surface and a generation
c...error occured.
c...Set nline back to the statement
c...that initiated this particular move and reset the
c...implicit check surface range specifier to 4 so that
c...gettrm will read the source for the statement pointed
c...to by nline.
c
            if (ifl(35).eq.0.or.(ifl(35).eq.2.and.ifl(350).eq.1)) then
                call nclf_src_rec_to_line (ifl4(13),nline)
                ifl(215)=4
            else
                ifl(215)=0
            endif
            endif
c                        turn off genpts flags
                  ifl(248)=0
                  ifl(250)=0
               else
c
c...Create GENPTS
c
                  if (isc10(1) .ne. 1021.and. isc10(1) .ne. 1095 .and.
     1                 isc10(1).ne.0) then
                      if (ifl(246) .gt. 0 .and .ifl(250) .gt. 0 .and.
     1                        (lv92 .or. lcmm)) then
                          call strmot
                      else if (ifl(246) .gt. 0 .and. ifl(372) .eq. 0
     1                         .and. .not. (lv92 .or. lcmm) ) then
                          call strgpt (jst,imotp,rte)
                      endif
                  endif
c
c...Display any generated motion
c
c
c.....We had a feedrate that was delayed
c.....Output to clfile
c
                  if (isc133(1).ne.0) then
                      tv=0
                      ieqtv(4) = isc133(2)
                      nr8s = 3
                      if (isc133(1).eq.1) then
                          d(51) = r4133(2)
                          if (isc133(2).eq.0) then
                              nr8s = 2
                          else
                              d(52) = tv
                          endif
                      else
                          d(52) = r4133(2)
                          d(51) = tv
                      endif
                      call putcl(2000,1009,nr8s,d(51))
                      sc(123)=r4133(2)
                      sc(133)=0
                      FEEDR = sc(123)
                      call setfed (sc(123))
                  endif
                  if (ifl(215).eq.2.or.ifl(215).eq.1) then
c                      if (sc(223) .eq. -10000) goto 99999
c                         in the middle of an implied c/s range.
c                         if it was a gr,gf,gl or gb, update the
c                         thick, and tool relationship and save nline in case
c                         of error.
                      ifl(21)=ifl(219)
                      sc(23)=sc(130)
                      sc(24)=sc(131)
                      sc(25)=sc(132)
                      call nclf_src_line_to_rec (nline,ifl4(13))
                  else if (ifl(215).eq.3) then
c                        this is the end of an implied c/s range.  call
c                        mocntl again to complete the last move and reset
c                        all indicators to normal motion state.
c                      if (sc(223) .eq. -10000) goto 99999
                      ifl(21)=ifl(219)
                      ifl(388) = 1
                      sc(23)=sc(130)
                      sc(24)=sc(131)
                      sc(25)=sc(132)
                      ifl(215)=0
                      sc(11)=sc(13)
                      sc(12)=sc(161)
                      sc(13)=sc(129)
                      isc10(1)=ifl(217)
                      call nclf_src_line_to_rec (nline,ifl4(13))
cc                      call pfilpt (i4stat(2),isrec,isrec2,isrec3)
                      if (ifl(42).eq.0 .or. ifl(372) .eq. 1) then
                        if (ifl(347).eq.1) then
c                           isrec2 = i4stat(2)
                           call ncl_setptr(imotp,isrec2)
                        else if (ifl(347).ge.2) then
c                           isrec3 = i4stat(2)
                           call ncl_setptr(imotp,isrec3)
                        else if(isc10(1).ge.701.and.isc10(1).le.710)then
c                           isrec = i4stat(2)
                           call ncl_setptr(imotp,isrec)
                        endif
                      endif
c                      jst = i4stat(2)
                      call ncl_setptr(imotp,jst)
                      call mocntl
                      go to 90
                  endif
              endif
c
c...Display any generated motion
c
                  call dismot (jgrec,imotp,rte)
c
c...Store CHKPTS
c
                  if (isc10(1) .ne. 1021.and. isc10(1) .ne. 1095 .and.
     1                 isc10(1).ne.0) then
                    if (ifl(246) .gt. 0 .and. lcmm .and. .not. lv92)
     1                    then
                      call strchk(jst,imotp,rte)
                    endif
                  endif
8099      continue
c
c         RAPID is a non-modal command. Therefore initialize it to
c         false every time. kathy
          rpfron = .false.
          go to 5499

c
c...SEQUNC/label
c
 4999 if (ist .eq. 818) then
          call sequnc(0,'')
          go to 5499
      endif

      if (.not.(ist.eq.700)) go to 5099
          call lathes
          if (.not.err) then
c              jst = i4stat(2)
              call ncl_setptr(imotp,jst)
              call mocntl
              go to 90
          endif
          go to 5499
5099  if (.not.(ist.eq.827)) go to 5199
          call dbcall(2)
          go to 5499
5199  if (.not.(ist.eq.828)) go to 5299
          call dbcall(3)
          go to 5499
5299  if (.not.(ist.eq.829)) go to 5349
          ifl(316) = 1
          call dbcall(1)
          go to 5499
5349  if (.not.(ist.eq.862)) go to 5399
          ifl(316) = 2
          call dbcall(1)
          go to 5499
5399  if (.not.(ityp.ne.7)) go to 5499
c******************************* blank line or %
c
          if (ityp.eq.1 .and. ist.ne.1096 .and. ist.ne.899 .and.
     x        ist .ne. 71 .and. ist .ne. 321 .and. ist .ne. 898 .and.
     x       (ist.gt.1000.or.ist.lt.500)) then
            if (ist.eq.1059) then
              call draft
            else
              sc(10)=0
              call postp

c                 call mocntl if it was fedrat/at....  epm 1-23-85
              if (isc10(1).eq.1009.and..not.err) then
                  call mocntl
                  if (ifl(2).ne.0) then
                      isvinx=0
                      call error(ifl(2))
                  endif
              endif
            endif
          else
              ifl(1)=0
              iwhich = 0
c              jgrec = i4stat(2)
              call ncl_setptr(imotp,jgrec)
              call altcom(iwhich)
              if (iwhich.eq.3) goto 10
c
C Commented out again after modifying REMOVE.F to always call error if a
C valid error is encountered.  9-12-90 rah
C
C            if (err) then
C               call error (ifl(2))
C            else
c... Add checks to err in case of error in redef
c
                if ((iwhich.eq.1).and. (.not.err)) then
c                            if iwhich = 1 then its a redef of a line.
                   call geogn1
                   go to 80
                else if ((iwhich.eq.2).and.(.not.err)) then
c                            if iwhich = 2 then its a redef of a circle.
                   call geogn2
                   go to 80
                endif
C             endif
c                  an erase command has been found, go to disply and
c                  erase the geometry
              if (ifl(135).eq.1) go to 85
              if (ifl(1).eq.2) then
                  if (isc10(1).eq.803)
     1                call pfilpt (imotp,isrec,isrec2,isrec3)
                  call mocntl
                  go to 90
              endif 
          endif
5499  continue
50000 if (isvf.eq.1) then
c                        this statement ended a do loop.
50001     iwhich=0
          call loopnd(iwhich)
          if (iwhich.eq.3) go to 50001
          isvf=0
      endif
      if (ncsfl(3).ne.0) then
c            don't read the next record; reprocess from inx
          go to 18
      endif
      if (.not.err .and. ifl4(10).eq.0) then
          call putsrc
          call nclf_getw2 (ifl(123),cimage,nccimg,irctyp)
c
c
c...Added check for NCL-VT mode
c...Paul  -  10/2/91
c...Old variant was:
c   if (ifl(47).eq.0.and.echo.and.ifl(35).eq.0)
c
c...And changed for NCL501+ mode
c...Paul - 03/17/92
c
          if (ifl(47).eq.0.and.echo.and.
     x    ((ifl(35).eq.0.and.ifl(350).ne.2) 
     x    .or. ifl(35) .eq. 2 ))
     x       call putw2(svll+ifl(123)-1)
c
      endif
      compos = .false.

c          if this statement was preceeded by a '*' that means the
c          statement was operated on but not put in the part program
c          file so nline must be reset back to the statement number
c          that it was at the beginning of processing the statement
      if (ifl4(10).ne.0) nline = ifl4(10)
      ifl4(10)=0
      ifl(47)=0
      isvtyp=0
c          if tablet menu entry was displayed, erase it now
      if (.not.err .and. ifl(172).eq.1) then
          ifl(172)=0
      endif
c
c...Added check for NCL-VT mode
c...Paul  -  10/9/91
c...Old version was:
c   if (.not.err.or.ifl(35).eq.1.or.ifl(268).eq.1) go to 10
c
c
c...Added check for NCL501+ mode
c...Paul  -  02/18/92
c...Old version was:
c    if (.not.err .or. ifl(35) .eq. 1 .or. ifl(268) .eq. 1
c   x.or.(ifl(35).eq.2 .and. pmode .eq.0)) go to 10
c
C
C...If ARCSLP/FILLET is active then call pfillet.
C...If ifl(347) is not equal to 3 then we don't have
C...enough information to call pfillet and need to get
C...another motion.  JLS 4/14/99
C
      if (ifl(347).ge.3) then
         if (irapid.ne.1) then
            call pfillet(isrec2,isrec3,isrec,iferr)
            if (ifl(2).ne.0) then
               call error(ifl(2))
            endif
c
c...Set clfile pointer when
c...fillet is skipped due to rapid motion
c
         else
            call ncl_setptr(imotp,isrec3)
         endif
C
C...Set up ifl(347) and isrec and isrec2 for next fillet.
C
         ifl(347) = 2
c         isrec=isrec2
c         isrec2=isrec3
         call ncl_setptr(isrec2,isrec)
         call ncl_setptr(isrec3,isrec2)
      endif
      if (.not.err .or. ifl(35) .eq. 1 .or. ifl(268) .eq. 1 
     x.or.(ifl(35).eq.2.and.pmode.eq.0.and.ifl(350).eq.0)) go to 10

99999 continue
      ifl(388) = 0

      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine chknst
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
c
      subroutine chknst(found1)
c
      include 'com8a.com'
c
      integer*2 found1,it
      integer*4 nclkey
c
      ldtflg = .true.
19    call getnst(found1,it)
      if (.not.(found1.ne.0.and..not.err)) go to 1000
          if (ifl(262) .eq. 0) then
              if (idst .gt. 6 .and. idst .lt. 10) then
                  call geogn2
              else if (isc10(1).eq.611.and.isc10(2).eq.11) then
                  isc10(1)=575 
                  isc10(2)=2 
                  call motimm
              else if ((idst .gt. 2. and .idst. lt .7) .or.
     x                     idst .eq. 10) then
                  call geogn1  
              endif
           else    
              ifl(262) = 0
           endif
70         if (err) then
              isvinx=0
              call error(ifl(2))
              if (err) go to 1000
           endif
           if (.not.defwf) call vstore
           defwf = .false.
c                                                check if need to disply
           call gtdesc(rest,nclkey,nwds,ietype)
           if (it.eq.1) then
c                                                unnamed - never display
               call nevdsp(nclkey)
           else if (it.eq.2) then
c                                                named - display
               call dspent(nclkey,ietype)
           endif
           go to 19
c
 1000  continue
c
c...   Turn off data statement flag
c
       ifl(324) = 0
       return
       end

C*********************************************************************
C*    E_SUBROUTINE     : getbatchinfo(ppfile, macro, kcurrent, khigh,
C*				        klines, kwarn, kerror)
C*        Get current NCL batch information            
C*                                
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          ppfile: current program name
C*          klen1: length of ppfile
C*          macro: name of active Macro
C*          klen2: length of macro
C*          kcurrent: current line number being processing
C*          khigh: highest number NCL reached
C*          klines: total line of part program file
C*          kwarn:  warning have so far 
C*          kerror: error have so far
C*          
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine getbatchinfo (ppfile, klen1, macro, klen2, kcurrent,
     1 		        khigh, klines, kwarn, kerror)
c
      include 'com8a.com'
      include 'comgt.com'
      include 'mocom.com'
c
      character*256 ppfile
      character*64  macro
      integer*4 kcurrent, khigh, klines, kwarn, kerror
      integer*4 klen1, klen2
c 
      integer*4 len, strlen1
      character*64 mcname
      equivalence (sc165,mcname)
      integer*2 ifnd
c
      kcurrent = ncline
      len = strlen1(ppfnam)
      if (len.le.255) then
          ppfile(1:255) = ppfnam(1:len)
          klen1 = len
      else
          ppfile(1:255) = ppfnam(len-255:len)
          klen1 = 255
      endif
c
c.....check if macro being called
c
      klen2 = strlen1(mcname)
      ifnd = 0
      if (klen2.gt.0) then
          call lrfind (mcname, ifnd)
      endif
      if (ifnd.eq.1) then
          macro(1:64) = mcname(1:64)
      else
          do 50 i=1,64
             macro(i:i) = ' '
50        continue
      endif
      klen2 = strlen1(macro)
      khigh = nhline

      klines = ifl4(1)
      kerror = ifl(33)
      kwarn = ifl(34)
c
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : resetmacpt()
C*        Reset macro pointer to current mcname (because the macro pointer
c*		could be interrupt by vxchk routine or parser routine)          
C*                                
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*          
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine resetmacpt()
c
      include 'com8a.com'
      include 'comgt.com'
      include 'mocom.com'
c 
      integer*4 len, strlen1
      character*64 mcname
      equivalence (sc165,mcname)
      integer*2 ifnd
c
c......we need the current mcname be define.
c
      len = strlen1(mcname)
c...
c...lrfind function will find if the mcname is called, if not it will define it
c...
      ifnd = 0
      if (len.gt.0) then
          call lrfind (mcname, ifnd)
      endif
      return
      end




