C*********************************************************************
C*    NAME         :  loadpp.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        loadpp.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:14
C********************************************************************/
c*
C*********************************************************************
C*    E_SUBROUTINE     : loadpp (inname, knc, errflg, whr)
c*       Read a sequential part program file into the partpgm.ncl
c*       file.
C*    PARAMETERS
C*       INPUT  :
C*          inname  - name of file to load. If blank, load last file specified.
C*          knc     - Number of chars in 'inname'.
C*          errflg  - 0 = ignore errors, 1 = don't ignore errors
C*          whr     - 0 = called by startup, 1 = called by *edt, 2 = batch
C*       OUTPUT :
C*          errflg  - 0 = no errors were encountered, 1 = error
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine loadpp (inname, knc, errflg, whr)
 
      include 'com4a.com'
 
      character*(MAX_PATH) inname
      character*(MAX_PATH) fbuf
      character*(MAX_LEN) buf
      character*1  buf1(MAX_LEN),fbuf1(MAX_PATH)
      equivalence(buf,buf1), (fbuf,fbuf1)
 
      integer*2 errflg,itxt
      integer*4 lunno,irec,ncf,knc,nc,strlen1,i4err,ip,nindex,nci,
     1          lstrec,nct
c
      integer*2 ioerr,ipos,ipsv,whr
 
      logical incld,lread,lrdfl(10)
      character*2 lsep
      character*6 c6
      character*(MAX_PATH) pfile
      character*(MAX_PATH) temp
      integer*4 flag
c
      character*64 errlab

      integer*4 luntmp(10),lunix
      data luntmp /20,21,22,23,24,25,26,27,28,29/
c
      data edt /1/, batch /2/
c
c...Initialize routine
c
      lunix  = 0
      incld=.false.
      lread=.false.
      lunno=scrlun
cc      call ifinit
c
c...Get name of file to load
c
      if (inname(1:knc) .eq. ' ') then
        fbuf = ppfnam
        knc = strlen1(ppfnam)
      else
        fbuf = inname(1:knc)
      endif
c
c...Remove trailing spaces
c
      do 10 i = knc, 1, -1
        if (fbuf1(i) .ne. ' ') go to 11
10    continue
c
c...Temporary work file is specified
c...Do a direct load
c
11    ilod = 0
      ncf = i
      call ulf_get_base_fname(fbuf,ncf,temp,nct)
      temp(nct+1:) = ' '
      if (temp(1:1) .eq. '~') then
          call nclf_load_src (fbuf,ncf,0,i4err)
          if (i4err .ne. -1) then
              ilod = 1
              ifl4(1) = i4err
          endif
      endif
c
c...Open the input file
c
      ipos = index(temp,'_')
      if (ilod .eq. 0) then
          if (whr .ne. edt) call nclf_close_src
          if (i.ne.1) i = i+1
          fbuf1(i)='{'
          irec = 0
          call flname(5, fbuf, pfile)
          call flopen(scrlun, pfile, 'OLD', 'SEQUENTIAL', 'FORMATTED',
     x        MAX_LEN, 'NULL', ioerr)
          if (temp(1:1).eq.'%'.and.ipos.gt.1) then
              call flname(5, temp(ipos+1:ncf), pfile)
          endif
          if (ioerr .ne. 0) go to 88888
      else
          call flname(5, temp(ipos+1:ncf), pfile)
          errflg = 0
      endif
c
c...Save the part program filename and
c...close existing part program file if
c...not *EDT
c
      if (whr .ne. edt) then
          ppfnam = pfile
          ncf = strlen1(pfile)
          call lsavsf(pfile,ncf)
c
          call ptdfnm(pfile,ncf)
          call lsavpp(pfile,ncf)
          if (ilod .eq. 1) go to 99999
          call nclf_set_loadpp
c
c...Set *EDT flag
c
      else
          EDTEXE = .true.
      endif
c
c...Position at End-of-file
c...If there prior to a *EDT
c...Bobby  -  6/27/91
c
      if (whr .eq. edt .and. svll .gt. ifl4(1)) svll = -1
c
18    inx=0
      cin=' '
c
c...Get the next record
c 
20    read (lunno, 30, end = 150, err = 8889) buf
30    format (a)
      nc = strlen1(buf)
      if (incld .and. .not. lread) then
          irctyp = 1
      else
          irctyp = 0
      endif
c
c...If this is a *EDT type load
c...Check for special records
c
      if (whr .eq. edt) then
c
c......If this was the next line before *EDT
c......then update nline
c
          if (buf(1:5) .eq. '<_N_>') then
              buf = buf(6:nc)
              nc = nc - 5
              svll=irec+1
              nline = svll
c
c......Look for LOOPND or DO LOOP executing
c
          else if (buf(1:5) .eq. '<_X_>') then
              buf = buf(6:nc)
              nc = nc - 5
              irctyp = irctyp + 20
c
c......Look for CALL statement currently executing
c
          else if (buf(1:5) .eq. '<_C_>') then
              buf = buf(6:nc)
              nc = nc - 5
              irctyp = irctyp + 30
          endif
      endif
c
c...Store record
c
      ifl(345) = 0
      if (whr .eq. edt) then
          call nclf_putmerge (buf,nc,irctyp,0)
      else
          call wrtpp (buf,nc,irec,irctyp,0)
      endif
      lstrec = irec
      irec=irec+1
c
c...Write record to .pr file when running batch
c
      if (whr .eq. batch .and. (.not. incld .or. incflg)) then
          nci = strlen1(buf)
          lsep = ': '
          if (incld) lsep = '# '
          write (cout,1070) irec,lsep,buf(1:nci)
 1070     format (i6,a2,a)
          i = nci + 8
          call putprt (cout,i,0)
      endif
c
c...Get rid of blanks to prepare for parsing 
c
      nci = strlen1(buf)
      do 40 i=1,nci,1
          if (buf1(i).eq.' ' .or. buf1(i).eq.'	') goto 40
          if (buf1(i).eq.'%') goto 45
          if (buf1(i).ne.'$') goto 35
          if (buf1(i+1).eq.'$') goto 45
          goto 20
 
35        inx=inx+1
          if (inx.lt.1536) call convrt(buf1(i), ain(inx), 1)
 
c
c...Check for fixed field words that may be legally followed
c...by a single dollar sign that does NOT mean continuation.
c
          if (inx .eq. 6) then
              if (cin(1:6) .eq. 'PARTNO' .or.
     x            cin(1:6) .eq. 'PPRINT' .or.
     x            cin(1:6) .eq. 'INSERT' .or.
     x            cin(1:6) .eq. 'LETTER') goto 45
          endif
40    continue
45    nccin = inx
      inx=1
c
c...Parse 1st token
c
      call parser
c
c...If this is NOT an INCLUD statement, go read the next record
c...otherwise, prepare to open the included file
c
c...Read all nested INLUDE files here now
c...This fixes a problem where a *EDT done
c...after a nested include file, would not load
c...the nested include file.
c...Bobby  -  4/8/96
c
50    if ((token2(1:6).ne.'INCLUD' .and. token2(1:4) .ne. 'READ') .or.
     1    lunix .ge. 10) goto 18
      ifl(2)=0
      errcom=' '
      incld=.true.
      if (token2(1:6) .eq. 'INCLUD') then
          i=index(buf,'/')
          lrdfl(lunix+1) = .false.
          lread = .false.
      else
          i=index(buf,',')
          lread = .true.
          if (lunix .gt. 0) then
              do 58 j=1,lunix,1
                  if (.not. lrdfl(j)) lread = .false.
   58         continue
          endif
          lrdfl(lunix+1) = lread
      endif
      nci = strlen1(buf)
60    i=i+1
      if (i.lt.nci.and.buf1(i).eq.' ') goto 60
 
c       check for include/filename,on. kathy
      ipos = 0
      if (token2(1:6) .eq. 'INCLUD') ipos = index (buf, ',')
      incflg = .false.
      if (ipos .gt. 0) then
        ipsv = ipos-1
        ip = nindex(buf(ipos+1:nci),' ')
        if (ip .ne. 0) then
            c6 = buf(ipos+ip:nci)
            call convrt (c6,c6,6)
            if (c6 .eq. 'ON') incflg = .true.
        endif
      else
        ipsv = nci
      endif
 
      lunix  = lunix  + 1
      itxt   = 0
   62 call flname(5, buf(i:ipsv), pfile)
      call flopen(luntmp(lunix), pfile, 'OLD', 'SEQUENTIAL',
     1            'FORMATTED', MAX_LEN, 'NULL', ioerr)
      if (ioerr.ne.0) then
          call flname(13, buf(i:ipsv), pfile)
          call flopen(luntmp(lunix), pfile, 'OLD', 'SEQUENTIAL',
     1                'FORMATTED', MAX_LEN, 'NULL', ioerr)
c
c...Could not open file
c...check for text variable used as filename
c
          if (ioerr .ne. 0) then
              if (whr .eq. edt .and. itxt .eq. 0) then
                    call parser
                    call parser
                  if (ityp .eq. 2 .and. ist .eq. TEXTVAR) then
                      ipsv = i - 1
                      call gttext (buf,ipsv)
                      itxt = 1
                      go to 62
                  endif
              endif
              go to 70
          endif
      endif
c 
c...point the logical unit number to the included file instead of
c...the temporary part program file
c
      lunno=luntmp(lunix)
      if (lunix .gt. 1 .or. lread) then
          if (lread) then
              temp = '$$' // buf
              nc = strlen1(temp)
              if (whr .eq. edt) then
                  call nclf_putmerge (temp,nc,0,1)
              else
                  call wrtpp (temp,nc,lstrec,0,0)
              endif
          else
              if (lrdfl(lunix-1)) then
                  irctyp = 4
              else
                  irctyp = 3
              endif
              nc = strlen1(buf)
              if (whr .eq. edt) then
                  call nclf_putmerge (buf,nc,irctyp,1)
              else
                  call wrtpp (buf,nc,lstrec,irctyp,0)
              endif
          endif
      endif
      goto 18
 
70    continue
      irctyp = 2
      nc = strlen1(buf)
      if (whr .eq. edt) then
          call nclf_putmerge (buf,nc,irctyp,1)
      else
          call wrtpp (buf,nc,lstrec,irctyp,0)
      endif
 
150   if (.not. incld) goto 152
      close(luntmp(lunix))
      lunix = lunix - 1
      if (lunix .eq. 0) then
          lunno=scrlun
          incld=.false.
          lread=.false.
      else
          lunno = luntmp(lunix)
          lread = lrdfl(lunix)
      endif
      goto 18
 
152   ifl4(1) = irec
c
c...Position at End-of-file
c...If there prior to a *EDT
c...Bobby  -  6/27/91
c
      if (svll .eq. -1 .or. svll .gt. ifl4(1)) svll = ifl4(1) + 1
160   close (unit = scrlun)
      ifl(2) = 0
      errflg = 0
c
c...we want to save the current pp file with path and extension
c...so we just pass in a o length file, the nclf_save_recent_file
c...will handle it
c
      ncf = strlen1(ppfnam)
      if (ncf.gt.0) then
         ncf = 0
         flag = 0
         call nclf_save_recent_file(ppfnam, ncf, flag)
      endif
c
c...Merge *EDT file with source file
c...and adjust loops and macros
c
      if (whr .eq. edt) then
          call nclf_merge_source (ichg)
          if (ichg .eq. 1) call fixpp (ifl4(1),errflg)
      endif
      goto 99999
 
88888 if (inname(1:knc) .eq. ' ') goto 152
 8889 if (errflg .eq. 1 .and. (ifl(35).eq.0 .or. ifl(35) .eq. 2)) then
 
        ityp = 6
      endif
      errflg = 1
 
99999 ifl(345) = 0
      call nclf_reset_loadpp
      EDTEXE = .false.
      return
      end
