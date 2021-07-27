C*********************************************************************
C*    NAME         : runvt.f
C*       CONTAINS:
C*
C*    MODULE NAME AND RELEASE LEVEL
c*       runvt.for , 25.1
c*    DATE AND TIME OF LAST MODIFICATION
c*       04/29/15 , 15:09:35
C*********************************************************************/
c** copyright (c) 1991 Numerical Control Computer Sciences
c**
c **********************************************************************
c **********************************************************************
c **  file name: runvt()                                              **
c **                                                                  **
c **  function name: runvt                                            **
c **  purpose of function: to load and run the NCL in the VT (NCL501) **
c **  mode                                                            **
c **                                                                  **
c **********************************************************************
c **********************************************************************
 
 
      subroutine runvt()
 
      include 'com4a.com'
 
      integer*2  errflg, jquit
      integer*4  CURSES, klin, kcol, count(8), kmxc, knc, count1
      integer*4 perpos, strlen1, nci
      character*(MAX_PATH) clnam
      character*2  CL_AS
      character*80 mess1, mess2, mess3, mess4, mess5, mess6, mess7
 
      mess1 = 'Enter Part Program File name: '
      mess2 = 'Enter "CL" to create a clfile, "AS" to create an APT sour
     *ce file,'
      mess3 = '"BO" to create both or press RETURN'
      mess4 = '******  NCL processor version *.*** ******'
      mess5 = 'Copyright (C) 1998 Numerical Control Computer Sciences'
      mess6 = 'Part Program Filename:'
      mess7 = 'Initializing NCL files'
 
      count(1) = 30
      count(2) = 69
      count(3) = 38
      count(4) = 45
      count(5) = 54
      count(6) = 22
      count(7) = 22
      count(8) = 80
 
c...  open the terminal for reading and writing
c
 
1     CURSES = 1
 
      call trmatt(CURSES)
      call clrscr
c
c... initialization of NCL variables
c
      klin = 6
      kcol = 1
      call plot(klin,kcol)
      call dmpbuf(mess7,count(7))
      call wflush
      call nclini
      call plot(klin,kcol)
      call clreol
      ifl(35)=2
      ifl(268) = 1
c
c... positions the cursor & Get PP name
c
      klin = 2
      kcol = 1
      call plot(klin,kcol)
      call dmpbuf(mess1,count(1))
      write (cout,10) sc(119)
   10 format ('******   NCL processor version ',f6.3,'   ******')
      klin = 10
      kcol = 20
      call plot(klin,kcol)
      call dmpbuf(cout,strlen1(cout))
      klin = 11
      kcol = 15
      call plot(klin,kcol)
      call dmpbuf(mess5,count(5))
 
      knc = 0
      kmxc = ifl(106)
      klin = 1
      kcol = 1
30    call gvtlin(ppfnam,knc,kmxc,klin,kcol)
      if (knc .le. 0 ) then
          knc    = 0
          go to 30
      endif
      if (ppfnam .eq. '*quit') go to 9000
      if (ppfnam(1:1) .eq. '*') then
          knc    = 0
          go to 30
      endif
      klin = 15
      kcol = 1
      call plot(klin,kcol)
      call dmpbuf(mess6,count(6))
      kcol = 25
      call plot(klin,kcol)
      count1 = 40
      call chkinp(ppfnam,1)
      call dmpbuf(ppfnam,count1)
c
c... Open PP file
c
      errflg = 0
      nci = strlen1(ppfnam)
      call loadpp(ppfnam,nci,errflg,0)
 
c
c... Get info about creating "CL" and "AS" files
c
      kcol = 1
      klin = 1
      call plot(klin,kcol)
      call clreol
      call clinit
 
      klin = 2
      call plot(klin,kcol)
      call dmpbuf(mess2,count(2))
      klin = 3
      call plot(klin,kcol)
      call dmpbuf(mess3,count(3))
      kmxc = 2
      klin = 1
      knc = 0
      call gvtlin(CL_AS,knc,kmxc,klin,kcol)
      if (CL_AS .eq. '*quit') go to 9000
      if ((CL_AS (1:1).eq. 'C' .or. CL_AS(1:1) .eq. 'c')
     *.and. (CL_AS(2:2).eq. 'L' .or. CL_AS(2:2) .eq. 'l')) then
         clnam = ppfnam
         call chkinp(clnam,2)
c         call getclf(clnam)
         nci = strlen1(clnam)
         call setclf(clnam,nci,1)
         ifl(69) = 1
      endif
      if ((CL_AS (1:1).eq. 'A' .or. CL_AS(1:1) .eq. 'a')
     *.and. (CL_AS(2:2).eq. 'S' .or. CL_AS(2:2) .eq. 's')) then
         clnam = ppfnam
         call chkinp(clnam,2)
c         call getclf(clnam)
         nci = strlen1(clnam)
         call setclf(clnam,nci,1)
         asfnam = clnam
         perpos = strlen1(asfnam)-2
         asfnam(perpos:)='.as'
         ifl(69) = 2
         ifl(88) = 1
      endif
 
      if ((CL_AS (1:1).eq. 'B' .or. CL_AS(1:1) .eq. 'b')
     *.and. (CL_AS(2:2).eq. 'O' .or. CL_AS(2:2) .eq. 'o'))  then
         clnam = ppfnam
         call chkinp(clnam,2)
c         call getclf(clnam)
         nci = strlen1(clnam)
         call setclf(clnam,nci,1)
         asfnam = clnam
         perpos = strlen1(asfnam)-2
         asfnam(perpos:)='.as'
         ifl(69) = 1
c
c ifl(88) changeed to 1 from 2 in the first version, becourse
c nclfin routine creates *.AS file only if ifl(88) = 1
c Paul. 10/11/91
c
         ifl(88) = 1
      endif
c
c...Creating the screen
c
      call clrscr
      call wflush
c
c...Added check for NCL-VT mode
c...Paul  -  10/11/91
c...Showing the first 8 lines from PP file in the Window 3
c
      nextyp = 11
      call ssrc(stat)
 
      call driver(jquit)
      call nclfin
 
      call clrscr
      call wflush
      call trmrst
      if (jquit .eq. 2) goto 1
c     write (*,222)
c222   format(//)
c
c     do 100 i = 1,10000
c         call getsrc(ppfnam,i)
c         write (*,200) ppfnam
c         if (srceof) go to 1000
c100   continue
c1000  continue
c200   format('source: ',a)
c
c     write (*,20) errflg
c20    format('error flag for "LOADPP": ',I4)
c     write (*,40) ifl(69),ifl(88), clnam
c40    format(//'IFL(69)=',i1,'   IFL(88)=',i1,'  clnam: ',a)
c
c...End of routine
c
 8000 return
c
c...Early exit
c
 9000 call trmrst
      go to 8000
      end
