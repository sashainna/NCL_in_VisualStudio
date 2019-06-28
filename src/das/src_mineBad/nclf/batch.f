C*********************************************************************
C*    NAME         : batch.for
C*       CONTAINS:
C*
C*    MODULE NAME AND RELEASE LEVEL
C*       batch.for , 26.2
C*    DATE AND TIME OF LAST MODIFICATION
C*       09/26/18 , 12:52:04
C*********************************************************************/
c** copyright (c) 1986 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  file name: batch.f                                              **
c **                                                                  **
c **  function name: getpp                                            **
c **  purpose of function: to load and run the next part program file **
c **    from the batch queue. Returns 0 if there are no more          **
c **                                                                  **
c **********************************************************************
c **********************************************************************
 
 
      function getpp()
 
      include 'com4a.com'
 
      integer*4 getpp,ilun,ir,ncf,strlen1, nc, klen, knc,
     1             num1, num2, ret
      integer*2 perpos, i, j, k, epos, ioerr, whr
      character*1 cpri
      integer*2 lpri,npri,i1,i2,i3,i4,i5,i6
      logical svinc,sv
      character*(MAX_PATH) cfnam,prfnam,qfname,fname
      character*(MAX_PATH+20) cdat
      character*(MAX_LEN) cbuf
      character*8 ctime
      character*11 cdate
      character*6 c6
      equivalence (cin,iin,aain)
c
      equivalence (ncsfl(1),ili)
      integer*2 ipos,ipsv
      character*36 ulpost
      character*(MAX_FILE) tempf
 
c     getpp = 0 indicates a part program file name was not found
      ifl(35)=1
      getpp = 0
c
c....check if the common memory space enough for running NCL, if not, just retur
c
      call ud_chk_comm(ret)
      if (ret.ne.0) return
      call nclini
      call ifinit
      incerr = ifl(2)
      ulpost=' '
      call ncdate (cdate)
      call nctime (ctime)
 
c     set processing mode flag to batch again (reset by ifl(x)=0 loop)
      ifl(35)=1
 
c     read ncl.que to get next program to run
 
205   continue
 
      call flname (3, 'ncl.que{', fname)
 
      call flopen (quelun, fname, 'OLD', 'SEQUENTIAL', 'FORMATTED',
     x             MAX_PATH+20, 'null', ioerr)
 
      if (ioerr .ne. 0) go to 250
      call flname (10, 'sy:tmp{', fname)
      call flopen (scrlun, fname,'SCRATCH','SEQUENTIAL','FORMATTED',
     x             MAX_PATH+20, 'null', ioerr)
 
      icnt=0
 
c     90 = ascii 'Z' (to initialize previous priority variable)
      npri=91
 
210   read (quelun,2110,end=220) cdat
c          qfname,i1,i2,i3,i4,i5,i6,cpri,numlin
2110  format (a)
2100  format (a120,i1,i1,i1,i1,i1,i1,a1,i3)
2111  format (a120,i1,i1,i1,i1,i1,i1,a1,i3.3)
c
c...Parse the runtime options
c
      nc     = strlen1(cdat)
      read(cdat(nc-9:),2120)i1,i2,i3,i4,i5,i6,cpri,numlin
 2120 format(i1,i1,i1,i1,i1,i1,a1,i3)
c
c...Parse the filename
c
      i = nc - 10
      do while (i.gt.1 .and. cdat(i:i).ne.' ')
        i = i-1
      enddo
      qfname = cdat(1:i-1)
      ncf = i - 1
 
c     getpp = 1 indicates a part program file name was found
      getpp = 1
      icnt=icnt+1
      write (scrlun,2110) cdat(1:nc)
      lpri = ichar(cpri)
      if (lpri.ge.npri) go to 210
c      if (lpri.le.npri) go to 210
      ncnt=icnt
      cfnam=qfname
      npri=lpri
      icl=i1
      ias=i2
      ili=i3
      ifl(148)=i4
      ifl(149)=i5
      ifl(151)=numlin
      ipstp = i6
      go to 210
 
220   continue
      if (icnt.eq.0) go to 245
      rewind scrlun
c
c...using rewind instead of delete then open a new file to rewrite
c...the left pp files.
c...make this change is that 'close' with 'delete' will have problem when
c...run from NCQ
c...Yurong
      rewind quelun
      endfile quelun
c...      close (unit=quelun,status='delete')
c...      call flname (3, 'ncl.que{', fname)
c...      call flopnw (quelun, fname, 'SEQUENTIAL', 'FORMATTED',
c...     x             MAX_PATH+20, 'null', ioerr)
      icnt=0
230   continue
      read (scrlun,2110,end=240) cdat
      icnt=icnt+1
      if (icnt.eq.ncnt) go to 230
      nc = strlen1(cdat)
      write (quelun,2110) cdat(1:nc)
      go to 230
240   continue
      close (unit=quelun)
      close (unit=scrlun)
      go to 260
 
c     end of que
245   continue
      close (unit=quelun)
      close (unit=scrlun)
      ifl(35)=0
      getpp = 0
      call nclf_close_src
      call rancls
      go to 99999
 
c     pause 5 seconds before trying again
250   continue
      if (nerr.gt.10) then
        getpp = 0
        call nclf_close_src
        call rancls
        go to 99999
      endif
 
      nerr=nerr+1
      call nwait (5)
      go to 205
 
c     set up file names
260   continue
      ppfnam=cfnam
      knc = 40
      call shortname(ppfnam, tempf, knc, klen)
      ijob = ijob + 1
      num1 = -1
      num2 = -1
      call ncdate (cdate)
      call nctime (ctime)
      call ud_store_batchmsg(ijob, num2, num1, cdate, ctime, ret)
      call ul_write_logfile(tempf, klen, num1, num2, cdate, ctime)
 
      ncf = strlen1(ppfnam)
c
c...Set the default directory to the same
c...as the input part program name
c
      call setdef (ppfnam,ncf)
      perpos = ncf + 1
      epos = 0
      do 280 i=ncf,1,-1
          if (ppfnam(i:i) .eq. '/' .or. ppfnam(i:i) .eq. '\\') go to 282
          if (ppfnam(i:i) .eq. '.') then
              epos = i
              go to 282
          endif
  280 continue
ccc      epos = index(ppfnam(1:ncf),'.',BACK=.TRUE.)
  282 if (epos.ne.0) perpos = epos
      if (icl.eq.1 .or. ias.eq.1) then
          clfnam=ppfnam
          clfnam(perpos:)='.cl'
          call clinit()
          ifl(69)=1
          if (icl.eq.0) ifl(69)=2
      else
c
c...we still need initial internal clfile because we used it
c...whether we create cl file or not
c
          call clinit()
      endif
 
      if (ias.eq.1) then
          asfnam=ppfnam
          asfnam(perpos:)='.as'
          ifl(88)=1
      else
          asfnam=' '
      endif
 
c           open print file
      i149sv=ifl(149)
      ifl(149)=1
      prfnam=ppfnam
      prfnam(perpos:)='.pr'
      close (unit=prtlun)
      call flname (8, prfnam, qfname)
      call flopnw (prtlun, qfname, 'SEQUENTIAL', 'FORMATTED',
     x             MAX_LEN, 'null', ioerr)
      prline=100
 
      cin='*************************************************************
     1****************** '
      call putprt(cin,80,1)
      cout=' '
      call putprt(cout,1,1)
      write (cout,1010) cdate, sc(119), ctime
1010  format('date: ',a11,'     N C L   P R O C E S S O R  -',f8.2,
     1        7x,'time: ',a8,1x)
      call putprt (cout,80,1)
c     call lnkinf (1)
      cout=' '
      call putprt (cout,1,1)
      call putprt (cin,80,1)
      ifl(149)=i149sv
 
      whr = 0
      if (ili .eq. 1) whr = 2
      call loadpp (cfnam,ncf,ioerr,whr)
      IRCTYP = 0
      if (ioerr.ne.0) then
        cout  = ' '
        call putprt(cout,1,1)
        cout = '*********   ERROR OPENING PART PROGRAM FILE   *********'
        call putprt(cout,60,1)
        call nclf_close_src
        call rancls
        goto 99999
      endif
 
      if (ili.eq.0) goto 500
 
      if (ifl(149).eq.1) prline = 100
      cin='*************************************************************
     1****************** '
      call putprt (cin,80,1)
      cout=' '
      call putprt (cout,1,1)
      call nctime (ctime)
      write (cout,1010) cdate,sc(119),ctime
      call putprt (cout,80,1)
c     call lnkinf (1)
      cout=' '
      call putprt (cout,1,1)
      call putprt (cin,80,1)
 
500   continue
 
c
c...Added check for NCL-VT mode (for *START command)
c...Paul  -  10/25/91
c...Old version was:
c   call driver
c
      call driver(dummy)
 
      call nclfin
      close (unit=prtlun)
      do 570 i=perpos,1,-1
        if (cfnam(i:i) .eq. '\\') go to 600
570   continue
      i = 0
600   continue
      cfnam(perpos:)=' '
      ncf = strlen1(cfnam(i+1:))
      call lsavpp(cfnam(i+1:),ncf)
      if (ipstp.eq.1) then
          call postit
      endif
 
99999 if (getpp.eq.1) then
          num1 = ifl(33)
          num2 = ifl(34)
          call ncdate (cdate)
          call nctime (ctime)
          call ud_store_batchmsg(ijob, num1, num2,cdate,ctime, ret)
          call ul_write_logfile(tempf, klen, num1, num2, cdate, ctime)
      endif
 
      return
      end
 
      subroutine shortname(kfnam, ksnam, kmax, klen)
c
      include 'com.com'
c
      character*(MAX_FILE) kfnam
      character*(MAX_FILE) ksnam
      integer*4 kmax, klen
c
      integer*4 strlen1, len
c
      len = strlen1(kfnam)
      call ul_fshort_filename(kfnam, len, kmax, ksnam, klen)
 
      return
      end
 
      subroutine reset_job()
c
      include 'com.com'
c
      ijob = 0
      return
      end
 
