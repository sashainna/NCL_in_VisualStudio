C*********************************************************************
C*    NAME         :  termac.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       termac.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       10/26/15 , 08:01:17
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine termac
c*       reset all parameters in a macro statement to macro parameter subtypes. 
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine termac

      include 'com8a.com'
      include 'cutter.com'
      include 'status.com'

      character*64 cparms(4)
      character*(MAX_LEN) buf
      character*64 macnam, lname,mcname
      equivalence (sc165, mcname)
      integer*2 head(12),mode,ktv(4)
      integer*4 termln,m4inf(4),callin,kline,curlin
      equivalence (ktv,tv)
      equivalence (mode,head(7))
      equivalence (nw,ktv(3))
      equivalence (melem,ifl(39))
      equivalence (termln,m4inf(3))
      integer*2 irslt,sinx
      integer*4 isub,nxtnum,n,nc,tmp,ilin
c
      character*64 errlab
      equivalence (sc182,errlab)
      logical last,first
c
      integer*2 MACRO,UNDO,wdisp
      parameter (MACRO=11)
      data UNDO /2/

      last=.false.
      first=.true.
      wdisp = 0

      call delm_tdata
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c   MACRO DEFINITION BEING TERMINATED 
c 
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
c          set termac line number if defining a macro 
      if (ifl(38).ne.1) go to 100 
          call ncl_getmc_hd (m4inf(1), m4inf(2), termln, mode, 
     1               head(8),  head(9), callin)
          call nclf_src_rec_to_line(m4inf(2),ilin)
          call ncl_ifunmatched(ilin,svll,kline)
          if (kline.gt.0) then
            write(errcom,1010) kline
1010        format(I6)
            call error(504)
            goto 9999
          endif
c
c...need check for match of do loop too
c
c          call dounmatched(m4inf(2),svll,kline)
c          if (kline.gt.0) then
c...MISMATCHED LOOPING STATEMENT FOUND
c            call error(411)
c            goto 9999
c          endif

          call nclf_src_line_to_rec (svll,termln)
          call ncl_storehd (m4inf(1), m4inf(2), termln, mode, head(8),
     1                      head(9), callin)
          mpage=0
          melem=0

c             Zero out ifl(67) that was set in DECMAC.F for DECLBL.F to
c             allow it to verify if a label was being redefined.
c          ifl(67)=0

c             Blank out area used in STATLN.F for current MACRO's name.
          mcname = '        '
      go to 2000

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c   MACRO EXECUTION BEING TERMINATED
c
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c...Get MACRO statement to get page and
c...element of 'parms' table
c
100   continue
c
c.....Stop running if *RUN/TERMAC is set
c
      if (ifl(391).eq.2) then
        pmode = 0
        ifl(391) = 0
        cout = '***STOPPING AT MACRO END***'
        call putmsg(cout,80,nline+1,0)
c
c.....Enter command mode if *SET/RUNCMD is set
c        
        if (ifl(392).eq.1) then
          svll = svll + 2
          nline = nline + 2
          call ncl_cmd_mode()
        endif
      endif
      call nclf_src_line_to_rec (nline,curlin)
      call lrdata (macnam,tmp,lname,tmp,ifl4(5),nxtnum,ifl4(6),
     1             ifl(254),ifl(255),errlab,tmp,1)
      if (macnam .eq. ' ') goto 9999
      token2 = macnam
      ivxsub = 0
      call vstchk
c
c...If the first item on the call stack is not a macro, it must be a loop
c...Call loopnd to deal with it and go back to look for a macro.
c
      if (ist.ne.MACRO) then
c
c...if the command start with "*"
c...or the loop is executing
c...no error message displayed
c
        sinx = inx
        inx = 1
        call parser
        if ((.not.(ityp.eq.5.and.(ist.eq.4.or.ist.eq.10)))
     1      .and. (wdisp.eq.0) .and. (ifl(45).eq.1)) then
            ist = 0
            call error(-529)
            wdisp = 1
        endif
        inx = sinx
        call loopnd (UNDO)
        goto 100
      endif
c
c...Reset if-then statements for *reset/call or *termac
c
      if (curlin .ne. ifl4(6)) then
        call ifrset(nxtnum,ifl4(6))
      else
        call nclf_src_rec_to_line (m4inf(2),ilin)
        call ncl_ifunmatched(ilin,svll,kline)
        if (kline.gt.0) then
          write(errcom,1010) kline
          call error(504)
          goto 9999
        endif
      endif
      call ncl_getmc_hd (m4inf(1), m4inf(2), termln, mode, 
     1               head(8),  head(9), callin)
      call nclf_src_rec_to_line (callin,nline)
      isub = 0
      irslt = 1
      call getsrc(buf,nc,nline-2,0)
      if (irctyp .ge. 30) irctyp = irctyp - 30
      call nclf_putw2 (1,buf,nc,irctyp)
      call wrtpp(buf,nc,nline-2,irctyp,0)
      if (ifl4(10).ne.0) ifl4(10)=nline
      call vstchk
c         Save record and element numbers that point to the macro
c         parameter table.
         numprm=nw
c          reset 'parms' table typrm parameters
500   max=numprm*2+12
      if (max.gt.35) max=35
c              if termac is called by callit, reset looping parameters
      if (ifl(38).ne.2) go to 800
      first=.false.
c
c...Pop this MACRO's looping region off of the stack
c...then update looping region limit values (IFL(67:68)
c...with the values from the looping region that was in
c...effect when this MACRO was called
c

      call lrpop
      call lrdata (macnam,tmp,lname,tmp,ifl4(5),nxtnum,ifl4(6),
     1             ifl(254),ifl(255),errlab,tmp,1)
      mcname = macnam
      call mclrst
  800 if (lcpmac) call delcmm
      if (ifl(310) .gt. 0) call delpod(cparms,0)
c
c...the lrdata could return LOOPST or Do label too
c...in that case, we need reset ifl value (mean this mac call is end)
c
1000  if (mcname.eq.'       ') then
          goto 2000
      else if ((mcname.eq.'LOOPST  ').or. (lname .ne. ' ') ) then
c
c...if the upper lever pgmode have any macro definintion or call,
c...then we can not reset ifl(38), so we need check all upper
c...lever, not just pgmode(pgindx-2), it could be nested loop or macro
c...calls between
c
         if ((pgindx-2).le.0) goto 2000 
         do 1099 n = pgindx-2, 1, -1
             if ((pgmode(n).eq.1)
     1            .or. (pgmode(n).eq.2) ) then
                 goto 3000
             endif
 1099    continue
         goto 2000
      else 
         goto 3000
      endif
2000  ifl(152)=mode
      if (ifl(38) .eq. 2) then
          ictent = ictsav
          ictsav = 0
      endif
      ifl(38)=0
3000  continue

c         Decrement index into program mode table (pgmode) so status line
c         gets changed to previous mode.
      if (pgindx .gt. 0) pgmode(pgindx) = 0
      if (pgindx .gt. 1) pgindx = pgindx - 1
9999  return
      end
