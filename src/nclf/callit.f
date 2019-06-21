C*********************************************************************
C*    NAME         :  callit.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       callit.f , 25.9
C*    DATE AND TIME OF LAST MODIFICATION
C*       05/01/17 , 13:06:50
C********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine callit
C*    purpose of subroutine: to set up for execution of a             
C*      macro/termac section of code.                                 
C*                                                                    
C*    a successful call will generate an entry in the variable symbol 
C*    table (vst) with the following format:                          
C*                                                                    
C*     name record ('a' record)  8 bytes                              
C*                     6 bytes                  2 bytes              
C*        ************************************************           
C*        *           macro name            *   >ffff    *           
C*        ************************************************            
C*                                                                    
C*     data record ('b' record)  8 bytes                             
C*           2 bytes     2 bytes     2 bytes     2 bytes              
C*        ************************************************            
C*        *    pg     *    el     *   line    *   dummy  *            
C*        ************************************************            
C*          where:                                                   
C*            pg - the page number of the call entry in the vst of  
C*                 the previously higher level call.                
C*                 (zero if this is the highest level call)         
C*            el - the element number of the call entry in the vst of 
C*                 the previously higher level call.                 
C*                 (zero if this is the highest level call)         
C*            line - the line number of the next line after this   
C*                 call.                                          
C*            dummy - not used.                                  
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
      subroutine callit

      include 'com8a.com'
      include 'status.com'
      include 'cutter.com'

      common /svmac/ svnam
      common /tdatcom/ tmp_dkeys, data_n
      
      character*64 svnam

      real*8 savtv
      integer*2 head(12)
      integer*2 mode
      integer*4 maclin,termln,next,callin,smod,ifl410,ictwas
      equivalence (mode,head(7))
      character*64 macnam
      character*64 mcname
      equivalence (sc165, mcname)
      integer*2 ktv(4)
      equivalence (ktv,tv)
      integer*2 melem
      equivalence (melem,ifl(39))
      integer*2 frstpg,pg,el,max, numprm, data_n
      integer*2 isv38,isav,ixsv,ifl123,trctyp,ifl37,ifl25
      integer*2 ifnd,i,j,savist,savityp
      integer*4 strlen1,kbeg,kend,nlsav,isub,nc,tmp,tncw2
      integer*4 nccimg_sav, nccin_sav, tmp_dkeys(50), key
      character*64 lname
      character*(MAX_LEN) cinsv,cimsv,buf,tw2
      logical*2 saveos, sfromt, in_chg
      data lname /' '/

      if (nextyp.ne.5) then
          call error(22)
          go to 99999
      endif
      in_chg = 0
      ifl(44)=9
      max=10
c          get macro name
      call parsit
      ldtext = .true.
c
c...Automatically include Macro if it does not
c...already exist
c...Bobby  -  1/19/00
c
      if (ityp .eq. 2 .and. ist .eq. 1 .and. token2 .ne. svmaci) then
          smod =pmode
          call savecmd
          saveos = nxteos
          savityp = ityp
          savist = ist
          savtv = tv
          sfromt = fromt
          
          svmaci = token2
          cinsv = cin(1:nccin)
          cimsv = cimage(1:nccimg)
          cin = '*INCLUD/' // token2(1:strlen1(token2)) // '.MAC'
          nccin = strlen1(cin)
          call nclf_getw2 (1,tw2,tncw2,trctyp)
          i = index(tw2(1:tncw2),'/')
          j = index(tw2(1:tncw2),',')
          if (j .eq. 0) j = tncw2 + 1
          buf = '*INCLUD/' // tw2(i+1:j-1) // '.mac'
          cimage = buf
          nccimg = strlen1(buf)
          isav = nextyp
          ixsv = inx
          nextyp = 5
          inx   = 9
          ist   = 744
          ncsfl(9) = 1
          nlsav = nline
          nline = nline - ifl(123) + 1
          call includ (kbeg,kend)
          nextyp = isav
          inx    = ixsv
          cin = cinsv
          nccin = strlen1(cin)
          cimage = cimsv
          nccimg = strlen1(buf)
          ncsfl(9) = 0
          if (err) go to 99999
c
c...I think we need run the include macro to define it
c
          cinsv = cin(1:nccin)
          cimsv = cimage(1:nccimg)
          isav = nextyp
          ixsv = inx
          ifl123 = ifl(123)
          ifl410 = ifl4(10)
          ifl4(10) = 0
          ictwas = ictent
          ictent = 0
          ifl25 = ifl(25)
          ifl37 = ifl(37)
          ifl(25) = 0
          ifl(37) = 3
c
c...if *call/ we don't call this command now (because this command is not wrote
c...into PP file) but just continue the call function
c 
          if (fromt) then
              call nclu_cmd_run_from_to(kbeg,kend+1)
          else
              call nclu_cmd_run_from_to(kbeg,kend)
              ist = 11
          endif
          ictent = ictwas
          ifl4(10) = ifl410
          ifl(25) = ifl25
          ifl(37) = ifl37
          nextyp = isav
          inx    = ixsv
          cin = cinsv
          nccin = strlen1(cin)
          cimage = cimsv
          nccimg = strlen1(cimage)
          ityp = savityp
          token2 = svmaci
          tv = savtv
          nxteos = saveos
          pmode = smod
          fromt = sfromt
          ifl(123) = ifl123
          call resetcmd
          irctyp = trctyp
          call nclf_putw2 (1,tw2,tncw2,trctyp)
          if ((.not. err).and.(fromt))  goto 99999
      endif
c
c...Call macro
c
      if (ityp .eq. 2 .and. ist .eq. 11) then
c              go through all macros currently executing to check if this
c              call is calling a currently executing macro.
           call lrfind (token2, ifnd)
           if (ifnd.eq.1) then
               call error (90)
               goto 99999
           endif
c
          macnam=token2
          frstpg=ktv(1)
          pg=0
          el=ktv(2)
          numprm=ktv(3)
c
c...   Set ifl(38) to zero so termac will work correctly in case of error
c...   on a nested macro call.
c
          isv38 = ifl(38)
          ifl(38) = 0
          call mclcll(macnam)
          if (numprm.eq.0) go to 5000
c
c...
c... need check the call statement to see if macro call include a multi-entries for parameter
c... if Yes call assign data and change macro call statement
c....Yurong
c
          cinsv = cin
          nccin_sav = nccin
          cimsv = cimage
          nccimg_sav = nccimg
          
          call chkmacent(macnam, in_chg)
c
c **********************************************************************
c
c              check parameters against parms table and set up all
c              variable symbol table entries
c
c **********************************************************************
          call mcparm
c
c...reset after call if cin/cimage changed
c
          if (in_chg.eq.1) then
              cin = cinsv
              nccin = nccin_sav
              cimage = cimsv
              nccimg = nccimg_sav
c
c...delete temp data
c...if we delete here, it might used inside macro since
c...this temp data = data1+data2+...
c...when use 'num(inputpt)'
c
              do i = 1, data_n-1, 1
                  if (tmp_dkeys(i).gt.0) then
                       call addtmpdata(i, tmp_dkeys(i))
                  endif
              enddo
          endif
          if (err) go to 99999
5000      if (pg.eq.0) pg=frstpg
c
c...get header info
c
          call ncl_getmc_hd (maclin, next, termln, mode, head(8),
     1               head(9), callin)
c              set up looping region variables
              tmp = 0
              call lrpush (macnam, tmp, lname, tmp, maclin, next,
     1                     termln)
              ifl4(5)=maclin
              ifl4(6)=termln
              mode=ifl(152)
          call ncl_storehd (maclin, next, termln, mode, head(8),
     1                      head(9), callin)
c                 set ifl(38) here so that if there is an error, termac
c                 will back out looping region correctly.
              ifl(38)=2
c
8000      if (ifl(35).eq.0 .or. ifl(35) .eq. 2) call putw2 (nline)
c
c         add a 'C' at 80's column for the call statement.
c
c...Do not put back in source file
c...if this was a *CALL command
c...Bobby  -  8/23/93
c
          if (ifl4(10) .eq. 0) then
              call nclf_getw2 (ifl(123),w2,ncw2,irctyp)
              irctyp = irctyp + 30
              if (fromt) then
                 call putsrc
              else if (ifl(123).gt.0) then
                 call wrtpp(w2,ncw2,nline-1,irctyp,0)
                 nline=svll+ifl(123)
              endif
          endif
          svnam=macnam
          ifl(152)=2
          if (pgindx .lt. 30) pgindx = pgindx + 1
          pgmode(pgindx) = 2
      else
          call error (54)
          go to 99999
      endif
88888 if (err) pmode=svll
c
c...     Store called MACRO name in area for STATLN.F to use.
c
      mcname = macnam
c              load call statement info and put in vst
      token2=macnam
      ivxsub = 0
      call vstchk
      ifl(9)=ifl(11)
      ifl(10)=ifl(12)
      mpage=ifl(9)
      melem=ifl(10)
      buf  = macnam(1:6)
      isub = 0
      call nclf_src_line_to_rec (nline,callin)
      call ncl_storehd (maclin, next, termln, mode, head(8),
     1                      head(9), callin)
      if (pmode.lt.nline) pmode=nline
      call nclf_src_rec_to_line (next,nline)
c          error in call statement - reset parameters and 'parms' records
      if (err) then
          call termac
          pmode=0
          ifl(38)=isv38
c
c...Reset ifl(146) to allow for *CALL command
c...Bobby  -  8/23/93
c
      else
          ifl4(10) = 0
      endif
c
c.....Stop program if *RUN/MACRO is set
c      
      if (ifl(391).eq.1) then
          pmode = 0
          ifl(391) = 0
          cout = '***STOPPING AT MACRO CALL***'
          call putmsg(cout,80,nline+1,0)
c
c.....Enter command mode if *SET/RUNCMD is set
c     
          if (ifl(392).eq.1) call ncl_cmd_mode()
      endif      

99999 continue
      ldtext = .false.
      if (ifl(38) .eq. 2) then
          ictsav = ictent
          ictent = 0
      endif
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine mcparm
C*    purpose of subroutine: to set macro parameters for execution of a 
C*      macro. For call a macro parameters 
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
      subroutine mcparm

      include 'com.com'

      integer*4 msub,psav(2),ind,nrndex
      integer*2 ierr, ifnd, iflg
      character*40 mtok
      character*64 ttok

100   continue
      if (nxteos) goto 200
      ifl(44) = 1
c
c...  set flag to parse macro parameters with an _ if *set/lang,cadra is called
c
      if (ifl(374) .ne. 0) ifl(375) = 1
      call parsit
      if (ityp .ne. 2 .or. ist .ne. 12) goto 9071
c
c...   Set the current macro variable pointer
c
      call mclpst
c
c...     get parameter's assigned value
c
      ifl(44) = 9
c
c...Save this Macro's list
c...in case a Macro is used as a default parameter
c...Calls to 'mclspt' and 'mclrpt' throughout routine
c...Bobby  -  8/18/97
c
      call mclspt (psav)
c
c...when we pass the parameter, it may be a local variable from the calling macro
c...which is one lever higher then current macro, so set up the point as that,
c...then reset Yurong
c
      call mclsetprv
      idtype = -1
      ifl38 = ifl(38)
      ifl(38) = 3
      call parsit
      ifl(38) = ifl38
      call mclrpt (psav)
      if (.not.(ityp.eq.1 .or.
     x   (ityp.eq.2 .and. ((ist.gt.0 .and. ist.lt.13) .or.
     x    ist.eq.14 .or. ist.eq.20 .or. ist .eq. 21.or.ist.eq.TEXTVAR
     x    .or. ist.eq.VSOLID .or. ist .eq. 23)) .or. ityp .eq. 9
     x    .or. ityp.eq.3 .or. ityp.eq.4)) goto 9071
c
c...  If its another parameter, get its assigned value
c
      if (ityp.eq.2 .and. ist.eq.12) then
        call mclspt (psav)
        call mclprv (token2, ivxsub, ifnd, ityp, ist, tv, mtok, msub)
        call mclrpt (psav)
        if (ifnd.eq.0) goto 9071
        token2 = mtok
        ivxsub = msub
      endif
      if (ityp .eq. 9) then
          ttok = '"' // token2(1:63)
          ind = nrndex(ttok,' ')
          tv = ind - 1
          token2 = ttok(1:ind) // '"'
      endif
          
c
c...  Save this parameter
c 
      call mclsav (token2, ivxsub, tv, ityp, ist)

      goto 100
c
c...  Check to see if all params have been given values, assign defaults if not
c
200   call mclchk(iflg)
      if (iflg.eq.1) goto 9072

999   continue

      ifl(375) = 0
      return

9066  ierr = 66
      goto 9999
9069  ierr = 69
      goto 9999
9071  ierr = 71
      goto 9999
9072  ierr = 72
      goto 9999
9999  call error(ierr)
      call mclrst
      goto 999
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine chkmacent(macnam, chg)
C*    purpose of subroutine: to check and handle macro multi entries
C*          for parameters 
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
      subroutine chkmacent(macnam, chg)

      include 'com.com'
      character*64 macnam
      integer*2 chg

      common /dstcom/keydt
      common /tdatcom/ tmp_dkeys, data_n

      integer*4 keydt

      integer*4 msub,psav(2),ind,nrndex
      integer*2 ierr, ifnd, iflg, first, count,dist
      character*40 mtok
      character*64 svtok, dname, orig_parm, sav_savid2
      integer*4 strlen1, nc, nc1, nc2, nc3, len, savinx1, savinx2
      integer*2 buf_nc(20)
      character*(MAX_LEN) cinsv,buf, cinnew, temp
      logical*2 saveos, sfromt
      real*8 savtv     
      integer*4 smod, ifl410, dx
      integer*2 isav,ixsv, irest(4),isv38, ifl123
      integer*2 i,j,savist,savityp, trctyp, savidst
      integer*4 nlsav, in_nc, tncw2
      integer*2 n1, data_n, sityp, sist, snextyp, dinit
      integer*4 snclkey, sivxsub,ix, savivxsub
      real*8 stv
      real*8 srest
      integer*2 ksn(4)
      integer*4 nkey2, sav_ISVSUB, tmp_dkeys(50)
      equivalence (rest, ksn, nkey2)
      integer*4 tmp1, tmp2, i0,tlen,savinx4
      character*64 sTOKEN2, ttok
      character*64 labl
      data dname /' '/
      equivalence (rest,irest)
         
      saveos = nxteos
      smod = pmode
      savityp = ityp
      savist = ist
      savtv = tv
      sfromt = fromt          
      svtok = token2
      isav = nextyp
      ixsv = inx
      nlsav = nline
      ifl410 = ifl4(10)
      ifl123 = ifl(123)
      ssvll = svll
      isv38 = ifl(38)
      
      srest = rest    
      savivxsub = ivxsub
      savidst = idst
      sav_savid2 = savid2 
      sav_ISVSUB = ISVSUB
c   
      chg = 0
      dinit = 0
      first = 1;
      count = 0;
      n1 = 0
      data_n = 1;
      cinnew = cin
      in_nc = nccin
     
      dx = 0
      ifl(38) = 0
100   continue
      if (nxteos) then
c
c...remove the last data statement if count = 1
c
         if (count.eq.1) then
            data_n = data_n - 1
         endif
         if (count.gt.1) then
              call ptdesc (ix, DATAST, rest)
              irest(3) = count-1
              idst = DATAST
              tmp1 = ifl(9)
              tmp2 = ifl(10)
              savid2 = dname
              isvsub = 0
              call vxstor (savid2, isvsub, nkey2, tmp1, tmp2,
     x               ksn(3), ksn(4), ifl(299))   
c
c...save all the data key in order to deleted late after all the parse
c...since they are only temperate for parsing
c
              tmp_dkeys(data_n) = nkey2
              data_n = data_n + 1
         endif      
c
c....execute data statement
c....and continue 
         if ((first.eq.0).and.(data_n.ge.1)) then
             if (count.eq.1) then
                cinnew(savinx2:) = orig_parm(1:nc2)
                in_nc = savinx2 + nc2
             else if (count.gt.1) then
                cinnew(savinx2:) = dname(1:nc3)
                in_nc = savinx2+nc3
             endif
             chg = 1
         endif
         goto 8000
      endif
c
c...  set flag to parse macro parameters with an _ if *set/lang,cadra is called
c
      ifl(44) = 1
      if (ifl(374) .ne. 0) ifl(375) = 1
      savinx1 = inx
      call parsit
      if (ityp .eq. 5 .and. ist .eq. 9) then
          savinx1 = inx
          call parsit
      endif
      if (ityp.eq.3 .or.ityp.eq.4) then
          tlen = inx - savinx1
          token2(1:tlen) = cin(savinx1:inx-1)
          do i = tlen, 1, -1
             if (token2(i:i).eq.','.or.token2(i:i).eq.' ') then
                 token2(i:i) = ' '
                 tlen = tlen - 1
               else
                 goto 200
              endif
          enddo
      endif
      if (.not.(ityp.eq.1 .or.
     x   (ityp.eq.2 .and. ((ist.gt.0 .and. ist.lt.13) .or.
     x    ist.eq.14 .or. ist.eq.20 .or. ist .eq. 21.or.ist.eq.TEXTVAR
     x    .or. ist.eq.VSOLID .or. ist .eq. 23)) .or. ityp .eq. 9
     x    .or. ityp.eq.3 .or. ityp.eq.4)) goto 9071
c
c...if it is a un-identifier followed by "=", error
c
 200  if (ityp .eq. 2 .and. ist.eq.1) then
          if (nextyp.eq.1) goto 9071
      endif
      if (ityp .ne. 2 .or. ist .ne. 12) then
c
c...allow more then one entried for one paramters
c
          if (count.eq.0) goto 9071
c          
c...add this token in data statement
c
          if (dinit.eq.0) then
              savid2 = dname
              isvsub = 0
              dinit = 1
              call dtinit(ix)
c
c...if it is data statement, ignore and continue parser
c
              if (.not.(sityp.eq.2 .and. sist.eq.DATAST)) then
                   call dtstor(ix, count, sityp, sist, stv, 
     x                snextyp, stoken2, sivxsub)
                   count = count + 1
              endif
          endif
          if (.not.(ityp.eq.2 .and. ist.eq.DATAST)) then 
             call dtstor(ix, count, ityp, ist, tv, nextyp, 
     x           token2, ivxsub)
             count = count + 1;
          endif
          goto 100
      else if (first.ne.1) then   
          if (count.gt.1) then
              call ptdesc (ix, DATAST, rest)
              irest(3) = count-1
              idst = DATAST
              tmp1 = ifl(9)
              tmp2 = ifl(10)
              savid2 = dname
              isvsub = 0
              call vxstor (savid2, isvsub, nkey2, tmp1, tmp2,
     x               ksn(3), ksn(4), ifl(299))
c
c...save all the data key in order to deleted late after all the parse
c...since they are only temperate for parsing
c
              tmp_dkeys(data_n) = nkey2
              data_n = data_n + 1
          endif      
c...change the new input statement
c
          if (count.eq.1) then
              cinnew(savinx2:) = orig_parm(1:nc2)
              cinnew(savinx2+nc2:) = 
     x                ','//cin(savinx1:(MAX_LEN-savinx2-nc2-1))
              in_nc = strlen1(cinnew)
              dx = savinx1 - savinx2 - nc2 - 1; 
          else if (count.gt.1) then
              cinnew(savinx2:MAX_LEN) = dname(1:nc3)
              cinnew(savinx2+nc3:MAX_LEN) = 
     x                   ','//cin(savinx1:(MAX_LEN-savinx2-nc3-1))
              in_nc = strlen1(cinnew)
              dx = savinx1 - savinx2 - nc3 - 1; 
          endif
          if (nc2.gt.10) then
              nc2 = nc2 + 1 - 1
          endif
          count = 0
          chg = 1
      endif
      if (first.ne.0) then
          savinx2 = inx
      else
          savinx2 = inx - dx
      endif
      first = 0
 300  ifl(44) = 9
      idtype = -1
      savinx4 = inx
      call parsit
      
      if (.not.(ityp.eq.1 .or.
     x   (ityp.eq.2 .and. ((ist.gt.0 .and. ist.lt.13) .or.
     x    ist.eq.14 .or. ist.eq.20 .or. ist .eq. 21.or.ist.eq.TEXTVAR
     x    .or. ist.eq.VSOLID .or. ist .eq. 23)) .or. ityp .eq. 9
     x    .or. ityp.eq.3 .or. ityp.eq.4)) goto 9071
c
c...  If its another parameter, get its assigned value
c
      if (ityp.eq.2 .and. ist.eq.12) then
c...another parameter
        call mclspt (psav)
        call mclprv (token2, ivxsub, ifnd, ityp, ist, tv, mtok, msub)
        call mclrpt (psav)
        if (ifnd.eq.0) goto 9071
        token2 = mtok
        ivxsub = msub
      endif
c...
c...continue pass more entried for one paramter
c
      if (ityp.eq.3 .or.ityp.eq.4) then
          nc2 = inx - savinx4 
          token2(1:nc2) = cin(savinx4:inx-1)
          do i = nc2, 1, -1
             if (token2(i:i).eq.','.or.token2(i:i).eq.' ') then
                  token2(i:i) = ' '
                  nc2 = nc2 - 1
              else
                  goto 400
              endif
          enddo
      else
          nc2 = strlen1(token2)
      endif
 400  if (count.eq.0) then
          count = count + 1;
          nc1 = strlen1(macnam)
          call ncl_tmpdatn(macnam, nc1, dname, nc3)
          do 150 i=nc3+1,64,1
		    dname(i:i) = ' '
 150      continue
          sityp = ityp
          sist = ist
          stv = tv
          snextyp = nextyp
          stoken2 = token2
          sivxsub = ivxsub
          dinit = 0
          if (ivxsub.eq.0) then
              orig_parm(1:nc2) = token2(1:nc2)
          else
              labl = token2(1:64)
              i0 = 0
              call nclf_format_label (labl,ivxsub,orig_parm,i0)
              nc2 = strlen1(orig_parm)
          endif
      else
c
c...store data
c
          if (dinit.eq.0) then
              savid2 = dname
              isvsub = 0
              dinit = 1
              call dtinit(ix)
              if (.not.(sityp.eq.2 .and. sist.eq.DATAST)) then 
                   call dtstor(ix, count, sityp, sist, stv, 
     x                snextyp, stoken2, sivxsub)
                   count = count + 1;
              endif
          endif
          if (.not.(ityp.eq.2 .and. ist.eq.DATAST)) then 
             call dtstor(ix, count, ityp, ist, tv, nextyp, 
     x           token2, ivxsub)
             count = count + 1
          endif
      endif
      goto 100

999   continue
8000  cin = cinnew
      nccin = in_nc

      nextyp = isav
      inx    = ixsv
      ityp = savityp
      ist =  savist
      token2 = svtok
      tv = savtv
      nxteos = saveos
      pmode = smod
      fromt = sfromt
      nline = nlsav
      ifl(123) = ifl123
      ifl4(10) = ifl410
      ifl(38)=isv38
      svll=ssvll
      
      ivxsub = savivxsub
      rest = srest   
      idst = savidst
      savid2 = sav_savid2
      ISVSUB = sav_ISVSUB
      return      
c
c...if error, let main routine to handle it.
c
9071  ierr = 0
      err = .false.
      goto 999
      end
      
