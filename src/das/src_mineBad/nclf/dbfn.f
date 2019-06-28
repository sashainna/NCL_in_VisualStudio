c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbfn.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:49
c**
c**
c*****************************************************
c**
c** copyright (c) 1981,1982 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbfn                                           **
c **                                                                  **
c **  purpose of subroutine: to open the requested data base file and **
c **     load the pointers from record 0 to the ifl variables.        **
c **     or to close the requested data base file.                    **
c **                                                                  **
c **********************************************************************
c **********************************************************************
 
      subroutine dbfn
 
      include 'com4a.com'
      include 'comdb.com'
      include 'nclrd.com'

       integer*2 fnlgth,fnstrt,npg,gpg,el,svcdb,omode
      equivalence (omode, ifl(199))
       character*6 wpass,writep
       character*8 ncldb
       real*8 jb8(35)
       equivalence (jb8,jb)
       equivalence (npg,jb(5)),(gpg,jb(6)),(el,jb(7))
       equivalence (ncldb,jb(1))
      equivalence (wpass,jb(9)),(writep,token2)
      real*8 dbver
      equivalence (dbver,sc(166))
      character*1 ainl(MAX_LEN),lin(MAX_LEN)
      character*(MAX_LEN) cinl
      character*(MAX_PATH) fnam,pfile
      character*1 buf1(MAX_PATH)
      equivalence (fnam,buf1)
      equivalence (ainl,cinl,lin)
      logical close

      integer*4 knc, kmxc, kcol, klin, strlen1

       omode=1
       close=.false.
 
c              check if it is a close request
       call parsit
       ifl(2)=0
       if (ityp.eq.1.and.ist.eq.831) go to 250
           inx=isvinx
 
c          reset err and ifl(2) in case the parse of the file name caused
c          an error condition.  (parse will get an error on '[' or ']' or
c                                 '.' in the file name)
      err=.false.
      ifl(2)=0
c              get file name
       ist=2
       icnt=0
       fnstrt=inx
       inx=inx-1
       iiflg=0
       cinl = cin(1:nccin)
 
100    icnt=icnt+1
       inx=inx+1
 
       if (ainl(inx).eq.'[')iiflg=1
       if (ainl(inx).eq.']')iiflg=0
cc       if (ainl(inx).eq.' ') go to 200
       if (ainl(inx).eq.','.and.iiflg.eq.0) go to 300
       go to 100
200    cdbio=10
       fnlgth=inx-fnstrt
       go to 801
250    close=.true.
c              look for file number
300    fnlgth=inx-fnstrt
c
 
 
c              get file name
801    ist=2
       icnt=0
       iiflg=0
c
c      changed form cin to cimage by: kathy
c
       cinl=cimage(1:nccimg)
       ipos = index (cinl(1:nccimg), '/')
410    ipos = ipos+1
       if (ipos.lt.80.and.cinl(ipos:ipos).eq.' ') goto 410
         
       fnstrt = ipos
       ipos=ipos-1
c
101    icnt=icnt+1
       ipos=ipos+1
 
       if (ainl(ipos).eq.'[')iiflg=1
       if (ainl(ipos).eq.']')iiflg=0
       if (ainl(ipos).eq.' ') go to 201
       if (ainl(ipos).eq.','.and.iiflg.eq.0) go to 301
c          if ((ain(ipos).ge.'a'.and.ain(ipos).le.'z').or.
c    1         (ain(ipos).ge.'0'.and.ain(ipos).le.'9').or.
c    2         (ain(ipos).eq.'.')) go to 100
c              call error (100)
c              go to 99999
       go to 101
201    cdbio=10
       fnlgth=ipos-fnstrt
       go to 800
251    close=.true.
c              look for file number
301    fnlgth=ipos-fnstrt
c=rsx
c          move window to mapping area and set up to load data base file
c          name

c      if (ncsfl(7).ne.0) then
c          iiflg=ipos-1
c          ls=(ncsfl(7)-1)*82+53
c          vwdb(5)=0
c          call craw (vwdb,idsw)
c          if (idsw.ne.1) call crawer ('dbfn  ',idsw)
c          load data base file name to mapping area
c          do 310 i=fnstrt,iiflg
c            ls=ls+1
c310         lncrb(ls)=lin(i)
c          call craw (icwdb,idsw)
c          if (idsw.ne.1) call crawer ('dbfn  ',idsw)
c      endif
c=all
       inx=inx+1
       ipos=ipos+1
       ifl(44)=9
       call parsit
       ist=2
       if (ityp.eq.3.and.itv.ge.1.and.itv.le.2) go to 500
c      if (ityp.eq.3.and.itv.ge.1.and.itv.le.3) go to 500
           call error(103)
           go to 88888
c500    cdbio=9+itv
500    cdbio=10
       ldb501 = .false.
       if (itv.eq.2) ldb501 = .true.
       if (.not.close) go to 550
c           if (ifl(itv+63).ne.0) go to 88888
           if (ifl(64).ne.0) go to 88888
               call error(102)
               go to 88888
550    if (nextyp.eq.11) go to 800
c              get write password
       call parsit
       if (ityp.eq.2.and.ist.eq.1) go to 600
           ist=2
           call error (106)
           go to 88888
600    omode=3
       if (wdbio.eq.0) go to 800
           svcdb=cdbio
           cdbio=wdbio
           ist=2
           call dbclos
           cdbio=svcdb
800    ist=2
c              check if a data base file is already open for this
c               access number
c       if (ifl(cdbio+54).ne.0) then
       if (ifl(64).ne.0) then
         call dbclos
       else
         close(dblun)
       endif
c              open the data base file
cuni       lsav=lin(fnlgth+fnstrt)
cuni       lin(fnlgth+fnstrt)='00'x
cuni       open (unit=dblun,name=lin(fnstrt),access='direct',status='old',
cuni     1      shared,err=1003)
cuni       lin(fnlgth+fnstrt)=lsav
c
c     implement logical file names. kathy
c

      fnam=cinl(fnstrt:fnstrt+fnlgth-1)
      do 111 i=1, 79
         if (buf1(i) .eq. ' ') goto 11
111   continue
11    buf1(i)='{'
      call flname (5,fnam,pfile)
c      call flopen(dblun, fnam, 'OLD', 'DIRECT', 'UNFORMATTED',
      call flopen(dblun, pfile, 'OLD', 'DIRECT', 'UNFORMATTED',
     x            288, 'NULL', ierr)

      if (ierr .eq. 29 .and. omode .ne. 1) then

c              if batch, issue warning to user on listing
          if (ifl(35) .eq. 1) then
              call putmsg('WARNING: DATA BASE FILE DID NOT EXIST. DATA B
     XASE FILE BY GIVEN FILENAME CREATED.', 80, 2, 0)
      
c                   bump number of warnings count
              ifl(34) = ifl(34)+1
          else

c                  write warning message to user
              cout = 'WARNING: DATA BASE FILE DOES NOT EXIST.  DO YOU WA
     XNT IT AUTO CREATED?'

c                 read response to warning message
c.....
c.....Added for NCL501+. Paul 06/22/93
c.....
              if (ifl(35) .eq. 2) then
                 call putmsg(cout,78,2,2)
                 nccin = 0
                 kmxc =6
                 kcol = 9
                 klin = 1
                 call gvtlin(cin,nccin,kmxc,klin,kcol)
              else
                 knc = strlen1(cout)
                 call nclpmt (cout,knc,cin,nccin)
              endif
              if  (ain(1) .ne. 'y' .and. ain(1) .ne. 'Y') goto 850
          endif

c              user responded YES to automatically create data base ok. 
c          call flopen (dblun, fnam, 'NEW', 'DIRECT', 'UNFORMATTED',
          call flopen (dblun, pfile, 'NEW', 'DIRECT', 'UNFORMATTED',
     1                 288, 'NULL', ierr)
          if (ierr .eq. 0) then
              ncldb = 'NCLDATAB'
              wpass = writep
              wdbio = cdbio
              jb(5) = 12
              jb(6) = 11
              jb(7) = 1
              jb8(4) = sc(119)
              call putdb (jb8, 0)
              do 1027 i = 1, 11, 1
                  jb(i) = 0
1027          continue
              jb(141) = 0
              jb(143) = 0
              do 1028 i = 1, 11, 1
                  call putdb (jb8, i)
1028          continue
          endif
      endif

c          if file opened is not a data base file, report error
850   if (ierr .eq. -107) goto 910
      if (ierr .ne. 0) then
          write (errcom,1030) ierr
1030      format (' i/o error:' i8)
          call error (105)
          go to 88888
      endif
c              get the first record and verify data base file
900   call getdb (jb8,0)
      if (ncldb.eq.'NCLDATAB') go to 1100
910       call error (107)
          go to 88888
1100  if (omode.eq.1) go to 1400
c              verify write password and load pointers
          if (wpass.eq.writep) go to 1300
              call error (108)
              go to 88888
1300  wdbio=cdbio
      wdbnpg=npg
      wdbgpg=gpg
      wdbel=el
c1400  ifl(cdbio+54)=cdbio
1400  ifl(64)=cdbio
      dbver = jb8(4)
      if (dbver.gt.sc(119)+1.0) dbver = 0.
      go to 99999
 
c            set data base file number variable so close routine will
c            close the file in error
c                                                            msg 4-14-83
c88888 ifl(cdbio+54)=cdbio
88888 ifl(64)=cdbio
      call dbclos
c      ifl(cdbio+54)=0
      ifl(64)=0
 
99999 return
      end
