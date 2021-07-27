C*********************************************************************
C*    NAME         :  includ.f
C*       CONTAINS:
C*          includ  lodmac
C*
C*    COPYRIGHT 2000 (c) N.C.C.S. Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       includ.f , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:10:12
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine includ
C*       handle include statement.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          kbeg   = Beginning line number of included file.
C*          kend   = Ending line number of included file.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine includ (kbeg,kend)

      include 'com8a.com'

      integer*4 icnt, irec, i, isav, isvll, kbeg, kend, nc, strlen1
      integer*2 ipos, ipos2, ipsv, k, READV
      integer*2 ioerr
      character*(MAX_LEN) buf,temp,buf1,mytemp
      character*(MAX_PATH) nfile, temp2
      character*8 cinc
      logical lread
c
      data READV /876/

c
c     Inittialized the text variable
c
      temp2=' ' 

      if (nextyp.ne.5) then
        isvinx=inx
        call error(22)
        goto 99999
      endif
      kbeg = 0
      kend = 0
c
c       added so the file name would work ok. (kathy)
c 
c     check for on flag. kathy
c
      incflg = .false.
      k    = index (cin(1:nccin),'$$')
      if (k .eq. 0) k = nccin
      if (ist .eq. READV) then
          lread = .true.
          ipos = 0
      else
          lread = .false.
          ipos = index (cin(1:k), ',')
      endif
      if (ipos .gt. 0) then
410      ipos = ipos+1
         if (ipos.lt.nccin .and. cin(ipos:ipos).eq.' ') goto 410
         ipos2 = index (cin(1:nccin), ' ')
         buf1  = cin(ipos:ipos2-1)
         call touppr (buf1,buf1)
         if (ipos.lt.nccin.and.buf1(1:2).eq.'ON') then
              incflg = .true.
         endif
      endif
c
c...vp 13-mar-95 in batch all includes are processed in getpp
c...Output error message with open failure in batch
c
c...READ and automatic *INCLUD from CALL are
c...not processed in getpp
c...Bobby  -  7/16/97
c
c...INCLUD/text-var may have been issued
c...try to open file again
c...Bobby - 9/19/05
c
      if (ifl(35) .eq. 1 .and. ist .ne. READV .and. cin(1:1) .ne. '*')
     1       then
          if (IRCTYP .eq. 2) go to 640
          if (IRCTYP .eq. 5) go to 99999
      endif

  640 ldtext = .true.
      call parsit
      ldtext = .false.
      err = .false.
      if (lstrng) then
        j = 0
        call gttext(temp2,j)
        ipos = 1
        do while (ipos.lt.j .and. temp2(ipos:ipos).eq.' ')
          ipos = ipos+1
        enddo
        temp = temp2(ipos:)
      else
        if (lread) then
            ipos = index (cimage(1:nccimg), ',') + 1
        else
            ipos = index (cimage(1:nccimg), '/') + 1
        endif
        do while (ipos.lt.nccimg .and. cimage(ipos:ipos).eq.' ')
          ipos = ipos+1
        enddo
        k    = index (cimage(1:nccimg),'$$')
        if (k .eq. 0) k = nccimg + 1
        ipsv = -1
        if (.not. lread) ipsv = index(cimage(1:k),',') - 1 
        if (ipsv .lt. 0) ipsv = k  - 1
        temp = cimage(ipos:ipsv)
      endif
c
c
c                          Read the line out of the source file to find
c                          out if this include has already been handled.
c                          Includes in original source are handled in
c                          start or loadpp and have blanks in col 73-79.
c                          Nested includes have 'include#' in col 73-79
c                          after they have been handled.
c                          If we got an error trying to include this one
c                          it will have 'incerr*' in col 73-79.
c
c
c...VX
c
      if (ifl(35) .eq. 2 .and. ifl(322) .eq. 1) then
          mytemp = temp(inx:ipsv)
          nc = ipsv - inx + 1
          call vx_includ(mytemp,nc,incflg)
          go to 99999
      endif

      call getsrc (buf, nc,nline-1,0)
      if (srceof) goto 10
      if (IRCTYP .eq. 1 .or. IRCTYP .eq. 2 .or. IRCTYP .eq. 5) go to 10
      call nclf_getw2 (1,w2,ncw2,i)
      if (buf(:nc) .ne .w2(:nc)) goto 10
      if (ncsfl(9).eq.1) goto 10
      if (lread .and. ifl(35) .eq. 1) goto 10
      goto 99999

10    continue
      if (lread) then
         mytemp = cimage(1:nccimg)
         cimage = '$$' // mytemp
         nccimg = nccimg + 2
         call nclf_putw2 (1,cimage,nccimg,IRCTYP)
      endif

c      open (unit=scrlun,file=temp(inx:72),status='old',
c     x      iostat=ioerr)
c
c     changed to check for file name. kathy
c
      call flname (5, temp(1:), nfile)
      call flopen (scrlun, nfile, 'OLD', 'SEQUENTIAL', 'FORMATTED',
     x             MAX_LEN, 'NULL', ioerr)
      if (ioerr.ne.0) then
          call flname(13, temp(1:), nfile)
          call flopen(scrlun, nfile, 'OLD', 'SEQUENTIAL',
     1                'FORMATTED', MAX_LEN, 'NULL', ioerr)
          if (ioerr.ne.0) go to 70
      endif

      irec=nline
      kbeg = nline
      if (ain(1) .eq. '*' .or. ifl(25) .eq. 2) irec=irec-1
c
      icnt=0
      if (srceof) goto 20
      itp = IRCTYP
      if (IRCTYP .eq. 2) itp = 0
c
c                    If this includ line is inserted do not write to pplun
c                    because it will get written to it later in putsrc.
c
      ifl(345) = 0
      if (ifl(25) .ne. 2 .and.  ain(1) .ne. '*') then
          call wrtpp(cimage, nccimg,nline-1,itp,0)
      endif
c                                  count lines in include file
cc20    read(scrlun,1020,end=30)buf
cc1020  format(a)
cc      icnt=icnt+1
cc      goto 20

cc30    continue
cc      if (icnt.eq.0) goto 99999
cc      ifl4(1)=ifl4(1)+icnt
cc      if (srceof) goto 45
c                                     move records down to make room.
cc      do 40 i=ifl4(1)-1,irec+icnt-1,-1
cc        call getsrc(buf,nc,i-icnt)
cc40      call wrtpp(buf,nc, i,0)
cc        ifl(345) = 0

cc45    rewind(scrlun)
c                                     write include records into partpgm
20    read(scrlun,1020,end=30)buf
1020  format(a)
      if (lread) then
          call touppr(buf,mytemp)
          nc = strlen1(mytemp)
          do 48 j=1,nc,1
              if (mytemp(j:j) .ne. ' ' .and. mytemp(j:j) .ne. '	')
     1                then
                  if (mytemp(j:j+3) .eq. 'READ') then
                      IRCTYP = 2
                      go to 50
                  else if (mytemp(j:j+5) .eq. 'INCLUD') then
                      IRCTYP = 5
                      go to 50
                  endif
              endif
   48     continue
      else
          IRCTYP = 1
      endif
      nc = strlen1(buf)
   50 call wrtpp(buf,nc,irec+icnt,IRCTYP,1)
      icnt=icnt+1
      goto 20
   30 ifl(345) = 0
cc      ifl4(1)=ifl4(1)+icnt

      close(scrlun)
c                                     fix up loops and macros after include
cc      call fixpp (icnt,irec+1)
c
c...Automatic INCLUD called from
c...CALL/macro
c...So write out CALL statement
c...Bobby  -  1/19/00
c
      kend = irec + icnt
      if (ncsfl(9) .eq. 1 .and. fromt) then
          isav = nline
          isvll = svll
          nline = irec + icnt + 1
          svll = nline
          call putsrc
          nline = isav
          svll = isvll
      endif
      goto 99999

70    continue
      call error(145)

99999 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine lodmac (fname,kend,ierr)
C*       Loads an external (macro) file.
C*    PARAMETERS   
C*       INPUT  : 
C*          fname  = filename to load.
C*       OUTPUT :  
C*          kend   = Last line of included file.
C*          ierr   = Non-zero on error.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine lodmac (fname,kend,ierr)
c
      include 'com4a.com'
c
      integer*2 ierr
      integer*4 kend
c
      character*(MAX_PATH) fname
c
      integer*4 kbeg,strlen1
c
c...Store INCLUD command
c
      cin = 'INCLUD/' // fname(1:MAX_PATH)
      nccin = strlen1(cin)
      cimage = cin(1:nccin)
      nccimg = nccin
      call nclf_putw2 (1,cin,nccin,0)
c
c...Issue INCLUD command
c
      ifl(2) = 0
      ist = 0
      nextyp = 5
      inx    = 8
      call includ (kbeg,kend)
      ierr = ifl(2)
      if (ifl(2) .ne. 0) go to 8000
c
c...Place in part program file
c
      call wrtpp(cimage,nccimg,nline-1,0,0)
c
 8000 return
      end
