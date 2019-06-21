C*********************************************************************
C*    NAME         :  nclfin.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       nclfin.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:34
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nclfin
c*       close files and call apt source generation routine.
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
      subroutine nclfin
 
      include 'com4a.com'
      include 'mocom.com'
 
c      common /authcom/ rtadr1,rtadr2,ntrm1,ntrm2
c      integer*4 ntrm1,ntrm2
c      integer*4 rtadr1(2),rtadr2(2)
 
      real*8 fini
      integer*2 ifini(4)
      equivalence (fini,ifini)
      character*8 ctime
      integer*4 i4,savepp,nci
      integer*4 klin, kcol, knc, kmxc, strlen1
      character*1 answer
 
c
c...close secondary unibase
c
      if (ifl(316) .eq. 2) then
         call ubact(2)
         call ur_cl04(ierr)
         if ( ierr .eq. 1) then
            call error(450)
         else if (ierr .gt. 1) then
            call error(451)
         endif
         ifl(316) = 0
      end if
      if (ifl(35).eq.1) then
        cout=' '
        call putmsg (cout,1,1,0)
        cin='***********************************************************
     1******************************'
        call putmsg (cin,80,2,0)
        call putmsg (cout,1,3,0)
        write (cout,1010) ifl(33),ifl(34)
1010    format('NUMBER OF ERRORS =',I4,'  NUMBER OF WARNINGS =',I4)
        call putmsg (cout,60,4,0)
        cout=' '
        call putmsg (cin,80,1,0)
        call nctime (ctime)
        call putmsg (cout,80,1,0)
200     format (1H ,A8,' ** ',A49,' **  ENDED    **')
      endif
 
c            write fini record to clfile
c      if (ifl(69).ne.0) then
c          ifini(1)=1
c          ifini(2)=14000
c          ifini(3)=0
c          ifini(4)=nline
c          clbuff(istat(1))=fini
c          call clput (i4stat(2),clbuff)
c      endif
c
c...Create clfile
c...Bobby  -  8/13/92
c
      call clfile
 
c          create apt source file
c
c...Added check for NCL-VT mode
c...Paul  -  10/3/91
c...Old version was:
c   if (ifl(88).eq.1.and.ifl(35).eq.0) then
c
c...VX
c
      if (ifl(88).eq.1.and.(ifl(35).eq.0 .or. ifl(35) .eq. 2 .and.
     1    ifl(322) .eq. 0)) then
          write (cout,1030) asfnam
1030      format (' creating apt source file: ',a49)
          call putmsg (cout,80,4,0)
      endif
c
c...Added parameters to 'aptsrc' call
c...Bobby  -  11/4/91
c
c      call aptsrc (1,cout)
      ifl(169)=0
      i4=0
c
c...Added check for NCL-VT mode
c...Paul  -  10/21/91
c
c...VX
c
      if (ifl(35) .eq. 2 .and. ifl(322) .ne. 1) then
 
         klin = 2
         kcol = 1
         cout ='DO YOU WANT THE PART PROGRAM UPDATED TO THIS SESSION [Y/
     xN]:'
c
c1235     call plot(klin,kcol)
c         call dmpbuf(cout,strlen1(cout))
c
1235     call putmsg(cout,strlen1(cout),2,1)
         knc = 0
         kmxc = 1
         klin = 1
         kcol = 1
         call gvtlin(answer,knc,kmxc,klin,kcol)
           if (answer .ne. 'y' .and. answer .ne. 'Y' .and.
     x     answer .ne. 'n' .and. answer .ne. 'N') goto 1235
           if (answer .eq. 'y' .or. answer .eq. 'Y' ) ifl(148) = 1
           if (answer .eq. 'n' .or. answer .eq. 'N' ) ifl(148) = 0
      endif
 
      nci = strlen1(ppfnam)
      if (ifl(148).eq.1) i4 = savepp(ppfnam,nci, 0)
c            close and delete part program work file if no save or save went ok
      if (i4.eq.0) call nclf_close_src
c
c.....Delete any auto save files created - ASF 1/13/14.
c
      call nclf_close_autosave
c            close and delete ranfile
      call rancls
c
c...Close all open scratch files
c...4/18/91   Bob Jr.
c
c     call scrfin
c
c...Delete name list
c
      call vxlfin
 
c            turn off nclcadd flag
c      if (ifl(35).eq.1) goto 99999
c=VMS
c      call trmext(%val(rtadr1(1)),ntrm1)
c      call trmext(%val(rtadr2(1)),ntrm2)
c=ALL
99999 return
      end
