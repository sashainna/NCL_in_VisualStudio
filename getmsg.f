C*********************************************************************
C*    NAME         :  getmsg.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*        getmsg.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:07
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getmsg
C*     does all input from the user, depending     
C*     the value of pmode:                                           
C*       pmode = -1  batch - all input comes from the source file    
C*       pmode =  0  interactive - all input comes from the terminal 
C*       pmode gt 0  interactive - input comes from the source file  
C*                       until the line counter (nline) = pmode,     
C*                       then revert back to the terminal            
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine getmsg(jquit)

      include 'com8a.com'
      include 'cutter.com'
c      include '../incf/nclrd.com'

      integer*2 jquit
      integer*4 lineno,colmno
c      character*1 aimage(80),carcon
      character*1 aimage(MAX_LEN)
      equivalence (cimage,aimage)
      character*6 c6imag
      equivalence (cout,c6imag)
      integer*2 in(5),ifirst,nc1
      equivalence (cin,in)
      equivalence (ifl(124),ifirst)
      character*55 stpmsg
      character*(MAX_LEN) stpinp
      integer*4 ifmt,nc,strlen1
      logical  lflg,qflg

      data
     x stpmsg/'hit return to execute the part program statement below:'/
      assign 1020 to ifmt
      if (ksr) assign 1015 to ifmt
      ifirst=0
      ifl(281)=0
      inx=ifl(292)
      cin(inx+1:)=' '
      ifl(123)=0
      jquit=0
c
c...when we get the statement, we need reset those value in case when we get
c...errors and those value not reset
c
      wildnum = 0
      geomnum = 0
      ifl4(107)=0
c...to remember the begin line of this statement in case of more than one line
c...in one statement sometime we need know the begining line number sunch as 
c...'elseif' jump line should be at begin line, not the ending line
c
      bline = nline
c
c...Get next command automatically
c...from tool library's CUTTER/TOOL statement
c...Bobby  -  8/19/93
c
  100 ncsfl(6) = 0
      if (ictent .gt. 0) then
          call cutool (ictent,cimage,ictnc)
          if (ictent .ge. 0) then
              fromt=.true.
              strcmd = .true.
              ncsfl(6) = 1
              nccimg = ictnc
              go to 3300
          endif
          ictent = 0
      endif
      ictlin = 0
c
c...Batch mode
c...Get command from source file
c
      if (ifl(35).eq.1) then
c                          batch mode
          call getsrc (cimage,nccimg,nline-1,0)
          if (srceof) quit=.true.
          fromt=.false.
c
c        strcmd is used for "**" commands. kathy
c
          strcmd=.false.
          go to 3300
      endif
c
c...NCLVX mode
c...Get command from Varimetrix
c
c      if (ifl(35).eq.2 .and. ifl(322) .eq. 1) then
c          call getsrc(cimage,nccimg,nline-1,0)
c          if (cimage(1:nccimg) .eq. '*quit') srceof = .true.
c          if (srceof) jquit=1
c          fromt=.false.
c          strcmd=.false.
c          go to 3300
c      endif
      ifl(78)=0
200   if (init.or.ifl(268).eq.0) go to 300
c
c...Added for NCL501+ mode
c...Paul. 02/28/92
c
          if (ifl(350) .ne. 2) call crsplt (1,1)
c  
C          carcon=' '
C          if (ksr) carcon=' '
300    if (ifl(101).eq.1) then
          call putmsg (stpmsg,55,2,0)
          call getsrc (cimage,nccimg,nline-1,0)
          if (srceof) then
              call putmsg (' ',1,2,0)
              call putmsg (' ',1,3,0)
              call putmsg (' ',1,4,0)
              go to 3090
          endif
          call crsplt (3,9)
          do 400 i=1,nccimg,1
              if (aimage(nccimg-i+1).gt.' ') go to 500
400       continue
500       ie=nccimg-i+1
          write (cout,ifmt) nline, (aimage(i),i=1,ie)
1015          format (' ',i6,': ', 512a1)
c
c...Added check for NCL-VT mode
c...Paul  -  10/14/91
c...Old version was:
c   call putmsg (cout,80,2,0)
c
             nc = strlen1(cout)
             i = nc
             call putmsg (cout,i,3,1)
cuni          endif
          call erslin
c 
c...Added check for NCL-VT mode
c...Paul  -  10/14/91 
c...Old version was: 
c   call gettrm (cimage,1,9)
c
          call gettrm (cimage,nccimg)
c  
c...Added check for NCL-VT mode 
c...Paul  -  10/14/91  
c...Old version was:  
c   call crsplt (3,1)
c
          call crsplt (4,1)
          call erslin
          do 600 i=1,nccimg,1
C
C... ignor 'TAB' as well as 'SPACE' in part program
C... Sharon - 19JUN91
C
600           if (aimage(i).ne.' ' .and. aimage(i) .ne. '	') go to 700
          go to 800
700       call getsrc (stpinp,nc, nline - 1,0)
          if (stpinp(1:nc) .eq. cimage(1:nccimg) .and. nc .eq. nccimg)
     1            go to 810
          ifl(101)=0
          if (ifl(25).eq.2) then
              ifl(37)=2
          else
              ifl(37)=1
          endif
          pmode=0
          call crsplt (2,1)
          call erslin
          go to 3200
      endif

c          get next part program file record
      call ckintr(ifl(86),ifl(35))
      if (.not.(nline.lt.pmode.and..not.err)) go to 5199
800       call getsrc (cimage,nccimg,nline-1,0)
810       continue
          do 900 i=1,nccimg,1
              if (aimage(nccimg-i+1).gt.' ') go to 3000
900       continue
3000      ie=nccimg-i+1
          if (ie.gt.0.and.ifl(268).eq.1) then
            write (cout,ifmt) nline,(aimage(i),i=1,ie)
            nc = strlen1(cout)
            i = nc
            call putmsg (cout,i,1,0)
          else
            call crsplt (9,1)
          endif
1020      format (' ',i5,': ', 512a1)
          call erslin
c
c....Added for NCL501+ mode
c....Paul 02/28/92
c
          if (ifl(350) .ne. 2) call crsplt (1,1)
c
          fromt=.false.
          strcmd=.false.
          if (.not.(srceof)) go to 5099
3090          ifl(37)=1
              call domode
              go to 3100
5099      continue
          go to 5299

c              get next line from terminal
5199  continue
3100      if (ksr.or.init) then
              call gettrm (cimage,nccimg)
          else
              if (ifl(35) .eq. 2) then
                   write(cout,1010)nline
1010               format (' ',i6,':  ')
                   call putmsg(cout,8,1,0)
              endif
              call gettrm (cimage,nccimg)
          endif
3200      fromt=.true.
          strcmd = .true.
          pmode=0

c              set *run/step indicator off
          ifl(101)=0
          if (ifl(25).eq.2) then
              ifl(37)=2
          else
              ifl(37)=1
          endif
          cout=' '
5299  continue
c
c...Added check for NCL-VT mode
c...Paul  -  10/10/91
c...Old version was:
c   if (cimage(1:7).eq.'*return') then
c
c...And changed for NCL501+ mode
c...Paul - 03/17/92
c
      if (cimage(1:nccimg).eq.'*return'.or. 
     x    (cimage(1:nccimg).eq.'*quit'.and.ifl(35).eq.2.and.
     1    ifl(350).le.1)) then
        jquit=1
        goto 99998
      endif

c
c...Added check for NCL-VT mode
c...Paul  -  10/25/91
c
      if (cimage(1:nccimg).eq.'*start' .and. ifl(35) .eq. 2) then
        jquit=2 
        goto 99998
      endif


c          if statement is a * command and its in *input mode set up
c          to get out of *input mode
3300  if (aimage(1).eq.'*' .and. ifl(150).eq.1) ifl(150)=-1

c          if *input mode is set put statement in source file
c          but don't process it
      if (ifl(150).eq.1) then
          call nclf_putw2 (1,cimage,nccimg,irctyp)
          ifl(123)=1
          call putsrc
c
c...Added check for NCL-VT mode 
c...Paul  -  10/3/91 
c...Old version was: 
c   if (ifl(47).eq.0 .and. echo .and. ifl(35).eq.0)
c
          if (ifl(47).eq.0 .and. echo .and. 
     x    (ifl(35).eq.0 .or. ifl(35) .eq. 2))   call putw2(nline-1)
c
          svll=nline
          inx=0
          ifl(123)=0
c
c...Added check for NCL-VT mode
c...Paul  -  10/24/91
c...Old version was:
c   if (ifl(35) .eq. 0) call statln
c
          if ((ifl(35) .eq. 0 .or. ifl(35) .eq. 2) .and.
     1        ifl(322) .eq. 0) call statln
          go to 200
      endif

c          do not allow '$' to cause continuation in partno, pprint,
c          insert and letter statements
      nc1 = nccimg
      call convrt(aimage,cout,nc1)
      nccout = nccimg
c...added remark and titles
c...Yurong
      if (c6imag.eq.'PARTNO' .or.
     x    c6imag.eq.'PPRINT' .or.
     x    c6imag.eq.'INSERT' .or.
     x    c6imag.eq.'LETTER' .or.
     x    c6imag.eq.'REMARK' .or.
     x    c6imag.eq.'TITLES' ) then
          cin=cimage(1:nccimg)
          nccin = nccimg
          if (c6imag.eq.'PPRINT'.or.c6imag.eq.'INSERT' .or.
     1        c6imag.eq.'PARTNO'.or.
     2        (c6imag .eq. 'REMARK' .and. aptrem))then
              cin=cout
              nccin = nccout
          else
               cin(1:6)=c6imag
          endif
          inx=nccin
      else
          lflg = .false.
          qflg = .false.
          do 3400 i=1,nccimg
              if (aimage(i).eq.'"') then
                  qflg = .not.qflg
              endif
              if (aimage(i).eq.'''') then
                  lflg = .not.lflg
              endif
              if (aimage(i).eq.';') lmlt = .true.
              if (aimage(i).eq.'$' .and. .not. (lflg.or.qflg)) then
                  if (i .eq. nccimg .or. aimage(i+1).ne.'$') then
                      ifl(123)=ifl(123)+1
c
c...Increased number of continuation lines to 50
c...Bobby  -  3/1/94
c
c...use MAX_LINE
c...yurong
c
                      if (ifl(123).ge.MAX_LINE) go to 99999
                      call nclf_putw2 (ifl(123),cimage,nccimg,irctyp)
c 
c...Added check for NCL-VT mode  
c...Paul  -  10/3/91  
c...Old version was:  
c                  if (ifl(47).eq.0 .and. echo .and.
c    x                    ifl(35).eq.0) call putw2 (nline)
c
                      if (ifl(47).eq.0 .and. echo .and.
     x                (ifl(35).eq.0 .or. ifl(35) .eq. 2)) 
     x                call putw2 (nline)
                      nline=nline+1
                      if (nline.eq.pmode) ifl(292)=inx
                      go to 100
                  else
                     go to 3500
                  endif
              else if (aimage(i).eq.'%' .and. .not.qflg) then
                  go to 3500
C
C... ignor 'TAB' as well as 'SPACE' in part program
C... Sharon - 19JUN91
C
              else if (aimage(i).ne.' ' .and. aimage(i) .ne. '	'
     *                 .or. qflg) then
                  inx=inx+1
c
c...Increased number of characters in one line
c...to 1536
c...Bobby  -  3/1/94
c
                  if ((ifl(343) .eq. 1 .and. inx .gt. 1520) 
     1                 .or. inx .gt. 1535) go to 99999
                  if (qflg) then
                    ain(inx) = aimage(i)
                  else
                    call convrt(aimage(i),ain(inx),1)
                  endif
              endif
c                  if its the first '(' remember where it is
c
c..... do not pay attention to '(' inside double quotes - FSR 61034
c
c..... allow wildcard like ABC(*), in this case, don't save ifirst
c
              if (ifirst.eq.0 .and. .not.qflg .and. aimage(i).eq.'('
     x           .and. (i .ne. nccimg .or. aimage(i+1).ne.'*'))
     2               ifirst=inx
3400        continue
      endif
3500  ifl(140)=inx+1
      nccin = inx
      ain(ifl(140))=' '
      inx=1
      ifl(123)=ifl(123)+1
      call nclf_putw2 (ifl(123),cimage,nccimg,irctyp)
      if (ifl(150).ne.0) ifl(48)=0
      ifl(292)=0
99999 ifl(150)=0
99998 svmsg = 1

       return
       end
