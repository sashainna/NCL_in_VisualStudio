C*********************************************************************
C*    NAME         :  aptsrc.f
C*       CONTAINS:
C*					aptsrc  aptisn  asmult  ascutr  ascudi  aspwrd
C*             asclnm  asgoto
C*
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        aptsrc.for , 26.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*        09/26/18 , 12:48:55
C*********************************************************************
C
c
c***********************************************************************
c
c   SUBROUTINE:  aptsrc (kfl,casnm,kclf,kst,ken)
c
c   FUNCTION:  This is the controlling routine for creating an APT
c              source file.
c
c   INPUT:  kfl     I*2  D1  -  Flag to show whether we are creating a
c                               normal APT source (1) or an NCLIPV APT
c                               source file (2).  3 = Create standard
c                               APT source using provided name.
c
c           casnm   C*80 D1  -  Name of APT source file when creating an
c                               NCLIPV APT source file.
c
c           kclf    I*2  D1  -  Clfile to create APT source file from;
c                               1 = Internal, 2 = External.
c
c           kst     I*4  D1  -  Beginning clfile record for APT source
c                               file creation.
c
c           ken     I*4  D1  -  Ending clfile record.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine aptsrc (kfl,casnm,kclf,kst,ken)
 
      include 'com4a.com'
c
      integer*2 kfl,kclf
      logical*2 ignppr
      integer*4 kst(2),ken(2)
c
      character*(MAX_PATH) casnm
c
      integer*2 iclbuf(960),i,npts,pnc(10),iapt,is,icnt2,
     1          idc,jerr,iflg,iflg1,itp
      integer*4 irec(2),ir,iclw(6),isn,inum,icnt,nw,nc,ioerr,
     1          nclf_getsrc
c
      real*8 dclbuf(640),stpt(6)
c
      character*1 cbuf(80),ibuf(80),abuf(77)
      character*6 lbuf
      character*8 lclt,lclnm,lclrv
      character*10 lclsym
      character*11 lcld
      character*(MAX_PATH) temp,clname
      character*80 cbuf80,pbuf(10),lclf,lclsy1,lclsy2,lsym
c
      equivalence (dclbuf,iclbuf)
      equivalence (cbuf80,cbuf,lbuf)
      equivalence (ibuf(4),abuf)
      equivalence (dclbuf(1),lclsym), (dclbuf(4),lclsy1)
      equivalence (dclbuf(6),lclsy2)
      equivalence (dclbuf(1),lclf), (dclbuf(11),lcld)
      equivalence (dclbuf(13),lclt), (dclbuf(14),lclnm)
      equivalence (dclbuf(15),lclrv)
c
      data ibuf(1) /'$'/, ibuf(2) /'$'/, ibuf(3) /' '/
c
c...If no apt source file is to be created go close files
c
      close (unit=aslun)
      if (ifl(88) .eq. 0 .and. kfl .ne. 3) go to 99999
c
c...Open NCL-IPV file
c
      if (kfl .eq. 2) then
          temp   = casnm
c
c...Open provided filename
c
      else if (kfl .eq. 3) then
          call flname(7, casnm, temp)
c
c...Open standard APT source file
c
      else
          call aptcl (clname)
          call flname(7, asfnam, temp)
      endif
c
c...Open the file
c
      call flopnw (aslun, temp, 'SEQUENTIAL', 'FORMATTED',
     x   80, 'NULL', iostat)
 
c
c...Store APT Source name and Date record
c
      lclf   = temp(1:80)
      call ncdate (lcld)
      call nctime (lclt)
      lclnm  = 'NCLCAM'
      write (lclrv,10) sc(119)
   10 format (f8.2)
      if (aptmark) then
         call asclnm (dclbuf(1),pbuf,pnc)
         call putapt (pbuf(1),pnc(1))
         if (pnc(2) .ne. 0) call putapt (pbuf(2),pnc(2))
      endif
c
c...Initialize routine
c
      npts   = 3
      stpt(1) = 0.
      stpt(2) = 0.
      stpt(3) = 0.
      stpt(4) = 0.
      stpt(5) = 0.
      stpt(6) = 1.
      call ncl_setptr(kst, irec)
      iapt   = 0
      if (kfl .eq. 2 .or. ifl(308) .eq. 2) iapt = 1
      idc    = 0
      ignppr = .false.
c
c...NCLIPV file
c...Store previous MULTAX,CUTTER,FEDRAT & GOTO
c
      if (kfl .eq. 2) then
          call clread (kclf,irec,iclw,dclbuf,jerr)
          if (jerr.eq.1) goto 120
          if (iclw(3) .ne. 7200) then
              call gclinf (dclbuf(4),dclbuf(10),dclbuf(32),npts)
              iclbuf(122) = 0
              if (npts .eq. 6) iclbuf(122) = 1
              call ncl_setptr(kst, irec)
              iclbuf(109) = 0
          endif
          inum   = 1
          if (iclbuf(122) .eq. 1) inum = 0
          call asmult (inum,npts,cbuf80,nc)
          call putapt (cbuf,nc)
c
          if (iclbuf(109) .eq. 0 .or. iclbuf(109) .eq. 2 .or.
     1        iclbuf(109) .eq. 3) then
              ipt    = 10
          else
              ipt    = 16
          endif
          do 20 i=ipt+5,ipt,-1
              if (dclbuf(i) .ne. 0) go to 25
   20     continue
          i      = ipt
   25     icnt   = i - ipt + 1
          call ascutr (dclbuf(ipt),icnt,idc,pbuf,pnc)
          call putapt (pbuf(1),pnc(1))
c          if (pnc(2) .ne. 0) call putapt (pbuf(2),pnc(2))
          do 30 j=2,10
            if (pnc(j).ne.0) then
              call putapt (pbuf(j),pnc(j))
            else
              goto 35
            endif
   30     continue
c
   35     if (dclbuf(32) .gt. 0) then
              inum   = 1009
              icnt   = 1
              call aspwrd (inum,dclbuf(32),icnt,pbuf,pnc)
              do 40 i=1,10,1
                  if (pnc(i) .ne. 0) call putapt (pbuf(i),pnc(i))
   40         continue
          endif
c
          inum   = 3
          icnt2  = 0
          isn    = 0
          stpt(1) = dclbuf(4)
          stpt(2) = dclbuf(5)
          stpt(3) = dclbuf(6)
          stpt(4) = dclbuf(7)
          stpt(5) = dclbuf(8)
          stpt(6) = dclbuf(9)
          call asgoto (inum,dclbuf(4),npts,icnt2,isn,cbuf80,nc)
          call putapt (cbuf,nc)
      endif
      go to 120
c
c...get cl record
c
c  100 if (irec .eq. ken .or. irec .eq. 0) go to 2400
  100 call ncl_eqlptr(irec,ken,iflg)
      call ncl_tstptr(irec,iflg1)
      if (iflg .eq. 1 .or. iflg1 .eq. 0) go to 2400
  120 call clread (kclf,irec,iclw,dclbuf,jerr)
      if (jerr.eq.1) goto 2400
      if (iclw(3) .ne. 2000 .or. iclw(4) .ne. 1044) ignppr = .false.
c
c...get type of record
c
  170 nw     = iclw(5)
c
c...go to appropiate section
c
  190 if (iclw(3).eq.2000) go to 300
      if (iclw(3).eq.3000) go to 1650
      if (iclw(3).eq.5000) go to 1700
      if (iclw(3).eq.5200 .or. iclw(3) .eq. 5250) go to 1700
      if (iclw(3).eq.6000) go to 2150
      if (iclw(3).eq.7100 .and. iclw(4) .eq. 1) go to 2140
      if (iclw(3).eq.7100) go to 2180
      if (iclw(3).eq.9000) go to 2200
      if (iclw(3).eq.7400) go to 2300
      if (iclw(3).eq.14000) go to 2400
c
c...ignore record
c
      go to 100
c
c...post word
c
  300 call aspwrd (iclw(4),dclbuf(1),nw,pbuf,pnc)
c
c...If *SET/APTSRC,CUTTER,PPRINT
c...and PPRINTCUTTER then remove PPRINT
c...Sharon  -  05Aug91
c
      if (ifl(307) .eq. 2 .and. pbuf(1)(1:6) .eq. 'PPRINT' .and.
     2    (pbuf(1)(7:12) .eq. 'CUTTER' .or.
     3    pbuf(1)(7:12) .eq. 'cutter')) then
          pbuf(1)(1:66) = pbuf(1)(7:72)
          pnc(1) = 66
      endif
c
c...Don't output PPRINT IPV statements
c
      if (.not. ipvcom .and. pbuf(1)(1:6) .eq. 'PPRINT') then
          do 305 i=7,72,1
              if (pbuf(1)(i:i) .ne. ' ') then
                  if ((pbuf(1)(i:i+2) .eq. 'IPV' .or.
     1                 pbuf(1)(i:i+2) .eq. 'ipv') .and.
     2                 pbuf(1)(i+3:i+3) .eq. ' ') then
                      do 303 j=72,i+2,-1
                          if (pbuf(1)(j:j) .ne. ' ') then
                              if (pbuf(1)(j:j) .eq. '~') ignppr = .true.
                              go to 100
                          endif
  303                 continue
                  endif
                  go to 310
              endif
  305     continue
      endif
c
c...Don't output PPRINT IPV continuation statements
c
  310 if (ignppr) then
          ignppr = .false.
          do 313 j=72,i+2,-1
              if (pbuf(1)(j:j) .ne. ' ') then
                  if (pbuf(1)(j:j) .eq. '~') ignppr = .true.
                  go to 100
              endif
  313     continue
          go to 100
      endif
c
c...Output post-processor command(s)
c
      do 340 i=1,10,1
          if (pnc(i) .ne. 0) call putapt (pbuf(i),pnc(i))
  340 continue
      go to 100
c
c...Circular interpolation
c...Save circle's x and y components
c
 1650 call cirapt (kclf,irec,dclbuf(6),stpt,npts,ierr)
      go to 100
c
c...Motion
c
 1700 icnt    = 1
c
c...Write input comment line(s)
c
      if (aptcom .and. iclw(4) .ne. 6) then
          ir     = iclw(1) - 1
  173     ioerr = nclf_getsrc(ir,abuf,nc,itp,0)
          if (ioerr .ne. 0) go to 1800
          ie = nc + 3
          iee = ie
          call putapt (ibuf,ie)
          if (ie .eq. 2) go to 1800
          do 180 i=4,iee,1
              if (ibuf(i) .ne. '$') go to 180
              if (ibuf(i+1) .eq. '$') go to 1800
              ir     = ir     +1
              go to 173
  180     continue
      endif
c
c...Format GOTO record(s)
c
 1800 call asgoto (iclw(4),dclbuf(icnt),npts,iapt,iclw(1),pbuf,pnc)
      call putapt (pbuf(1),pnc(1))
      if (pnc(2) .ne. 0) call putapt (pbuf(2),pnc(2))
      is     = icnt   - 1
      isiz = npts
      if (iclw(3) .eq. 5200 .or. iclw(3) .eq. 5250) isiz = 21
      icnt   = icnt   + isiz
      if (icnt .lt. iclw(5)) go to 1800
      do 1850 i=1,npts,1
          stpt(i) = dclbuf(is+i)
 1850 continue
      go to 100
c
c...Cutter
c
 2140 idc    = iapt
 2150 call ascutr (dclbuf(1),nw,idc,pbuf,pnc)
      call putapt (pbuf(1),pnc(1))
      do 2160 j=2,10
        if (pnc(j).ne.0) then
          call putapt (pbuf(j),pnc(j))
        else
          goto 2170
        endif
 2160 continue
 2170 idc    = 0
      go to 100
c
c...Cutter/Disply
c
 2180 if (iclw(4) .eq. 7) then
          lsym = lclsy1(1:iclbuf(1))
      else if (iclw(4) .eq. 8) then
          lsym = lclsy2(1:iclbuf(1))
      else
          lsym = lclsym
      endif
      call ascudi (iclw(4),dclbuf(1),nw,lsym,pbuf,pnc)
      call putapt (pbuf(1),pnc(1))
      if (pnc(2) .ne. 0) call putapt (pbuf(2),pnc(2))
      idc    = 0
      go to 100
c
c...Multax
c
2200  call asmult (iclw(4),npts,cbuf80,inc)
      if (isiz.ne.21) isiz = npts
      call putapt (cbuf80,inc)
      go to 100
c
c...Clfile name and Date
c
 2300 call asclnm (dclbuf(1),pbuf,pnc)
      call putapt (pbuf(1),pnc(1))
      if (pnc(2) .ne. 0) call putapt (pbuf(2),pnc(2))
      go to 100
c
c...Close cl file & aptsrc file
c
 2400 inc=4
      call putapt ('FINI',inc)
      close (unit=aslun)
99999 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  aptisn (knum,cbuf,knc)
c
c   FUNCTION:  Formats the ISN from the clfile for output in an APT
c              source GOTO record.
c
c   INPUT:  knum    I*4  D1  -  ISN to format for output.
c
c   OUTPUT: cbuf    C*n  D1  -  Buffer to receive text formatted ISN.
c
c           knc     I*2  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine aptisn (knum,cbuf,knc)
c
      integer*2 knc
      integer*4 knum
c
      character*(*) cbuf
c
      integer*2 i
c
      character*10 lbuf
c
c...Format ISN for output
c
      knc    = 0
      write (lbuf,10) knum
   10 format (i10)
      do 100 i=1,10,1
          if (lbuf(i:i) .ne. ' ') then
              knc    = knc    + 1
              cbuf(knc:knc) = lbuf(i:i)
          endif
  100 continue
      knc    = knc    + 1
      cbuf(knc:knc) = ' '
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  asmult (kmult,knpts,cbuf,knc)
c
c   FUNCTION:  Formats a binary MULTAX record for output to an APT
c              source file.
c
c   INPUT:  kmult   I*4  D1  -  0 = MULTAX/ON, 1 = MULTAX/OFF.
c
c   OUTPUT: knpts   I*2  D1  -  Number of values for each clpt, 3 for
c                               MULTAX/OFF, 6 for MULTAX/ON.
c
c           cbuf    C*80 D1  -  APT source record.
c
c           knc     I*2  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine asmult (kmult,knpts,cbuf,knc)
c
      integer*2 knc,knpts
      integer*4 kmult
c
      character*80 cbuf
c
c...Format MULTAX statement
c
      if (kmult .eq. 1) then
          knc    = 10
          cbuf   = 'MULTAX/OFF'
          knpts  = 3
      else
          knc    = 9
          cbuf   = 'MULTAX/ON'
          knpts  = 6
      endif
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ascutr (gbuf,kcnt,kdc,cbuf,knc)
c
c   FUNCTION:  Formats a binary CUTTER record for output to an APT
c              source file.
c
c   INPUT:  gbuf    R*8  D7  -  Cutter parameters.
c
c           kcnt    I*4  D1  -  Number of parameters in 'gbuf'.
c
c           kdc     I*2  D1  -  1 = Construct CUTTER/DISPLY command.
c
c   OUTPUT: cbuf    C*80 D10 -  APT source record.
c
c           knc     I*2  D10 -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine ascutr (gbuf,kcnt,kdc,cbuf,knc)
c
      include 'com8a.com'
c
      integer*2 knc(10),kdc
      integer*4 kcnt
c
      real*8 gbuf(7)
c
      integer*2 ncc,BLADE,LATHE
      integer*4 jnc,strlen1
c
      logical icmnt
c
      character*14 lcutr
      character*80 cbuf(10)
c
      real*8 cutr(7)
c
      integer*2    strt,i,j,lens(7),lin,flg,wflg
      integer*2    posn,tn,curlen,typp,ids(7),curpos
      character*9  fst(7)
      character*30 cutstr
      character*72 tstr
c
      parameter (BLADE=191)
      parameter (LATHE=700)
c
c...Determine type of cutter command to output
c
      if (kdc .eq. 0) then
          lcutr = 'CUTTER/'
          ncc   = 7
      else
          lcutr = 'CUTTER/DISPLY,'
          ncc   = 14
      endif
c
c...Zero out cutter array
c
      do 100 i=1,7,1
          cutr(i) = 0.
 100  continue
      knc(1) = 0
      knc(2) = 0
      icmnt = ifl(307) .eq. 2
      if (kcnt .eq. 0) go to 8000
c
c...Store cutter parameters
c
      ncut   = kcnt
      do 200 i=1,kcnt,1
          cutr(i) = gbuf(i)
  200 continue
c
c...APT cutter statement
c
      if (ifl(307) .eq. 0) then
c
c...Don't output CUTTER/DISPLY, BLADE, or LATHE
c...style cutters
c
          if (kdc .eq. 1 .or. cutr(1) .lt. 0) then
              icmnt = .true.
              go to 500
          endif
          if (ncut .le. 4) then
C
C...If cutr(2) is not equal to 0 then there is
C...a corner radius.
C
              if (cutr(2) .ne. 0.) then
                  cutr(5) = cutr(1)/2. - cutr(2)
                  cutr(6) = cutr(2)
C
C...If cutr(4) is greater than 0 then we have a cone cutter.
C...If cutr(4) is less than 0 then we have a bell cutter.
C...Originally no changes were made to the diameter, so added
C...the necessary calculations to adjust the diameter for bell
C...and cone cutters.
C
                  if (cutr(4).gt.0.and.cutr(4).lt.90) then
                     cutr(1)=cutr(1)-(2*cutr(2))+(2*(cutr(2)*
     x                       tan(.01745329252d0*((90-cutr(4))/2))))
                  elseif (cutr(4).lt.0.and.cutr(4).gt.-90) then
                     cutr(1)=cutr(1)+2*(cutr(2)*
     x                       (tan(.01745329252d0*(-cutr(4)/2))+1)/
     x                       (tan(.01745329252d0*(90+cutr(4)))))
                  endif
 
              endif
c.....Write APTSRC,CUTTER with user defined precision for real values
              if (ifl(390) .ne. 0) then
                  ncut = 7
                  typp = 0
                  strt = 1
                  goto 550
              else if (ifl(319) .eq. 0) then
                  write (cbuf(1),1001) lcutr(1:ncc),cutr(1),cutr(2),
     1                                 cutr(5),cutr(6),cutr(7),cutr(4),
     2                                 cutr(3)
              else
                  write (cbuf(1),1002) lcutr(1:ncc),cutr(1),cutr(2),
     1                                 cutr(5),cutr(6),cutr(7)
                  write (cbuf(2),1003) cutr(4),cutr(3)
                  jnc = strlen1(cbuf(2))
                  knc(2) = jnc
              endif
          else
              if (cutr(4) .ne. 0) then
                  cutr(7) = cutr(2)
                  cutr(2) = cutr(4)
                  cutr(4) = cutr(1)/2. - cutr(4)
              endif
              if (ifl(390) .ne. 0) then
                  ncut = 7
                  typp = 1
                  strt = 1
                  goto 550
              else if (ifl(319) .eq. 0) then
                  write (cbuf(1),1001) lcutr(1:ncc),cutr(1),cutr(2),
     1                                 cutr(4),cutr(5),cutr(7),cutr(6),
     2                                 cutr(3)
              else
                  write (cbuf(1),1002) lcutr(1:ncc),cutr(1),cutr(2),
     1                                 cutr(4),cutr(5),cutr(7)
                  write (cbuf(2),1003) cutr(6),cutr(3)
                  jnc = strlen1(cbuf(2))
                  knc(2) = jnc
              endif
          endif
          ncut   = 7
c
c...NCL cutter statement
c
      else if (ifl(307) .eq. 1) then
          rbuf = gbuf(1)
c
c...Blade cutter
c
          if (cutr(1) .eq. BLADE-10000) then
              if (ifl(390) .ne. 0) then
                  typp = 2
                  strt = 2
                  goto 550
              else if (ifl(319) .eq. 0) then
                  write (cbuf(1),1005) lcutr(1:ncc),(cutr(i),i=2,ncut)
              else
                  write (cbuf(1),1006) lcutr(1:ncc),(cutr(i),i=2,ncut)
              endif
c
c...Lathe cutter
c
          else if (cutr(1) .eq. LATHE-10000) then
              if (ifl(390) .ne. 0) then
                  typp = 3
                  strt = 2
                  goto 550
              else if (ifl(319) .eq. 0) then
                  write (cbuf(1),1007) lcutr(1:ncc),(cutr(i),i=2,ncut)
              else
                  write (cbuf(1),1008) lcutr(1:ncc),(cutr(i),i=2,ncut)
              endif
c
c...Standard cutter
c
          else
              if (ifl(390) .ne. 0) then
                  typp = 4
                  strt = 1
                  goto 550
              else if (ifl(319) .eq. 0) then
                  write (cbuf(1),1001) lcutr(1:ncc),(cutr(i),i=1,ncut)
              else if (ncut .gt. 5) then
                  write (cbuf(1),1002) lcutr(1:ncc),(cutr(i),i=1,5)
                  write (cbuf(2),1003) (cutr(i),i=6,ncut)
                  jnc = strlen1(cbuf(2))
                  knc(2) = jnc
              else
                  if (ifl(319) .eq. 0) then
                    write (cbuf(1),1001) lcutr(1:ncc),(cutr(i),i=1,ncut)
                  else
                    write (cbuf(1),1011) lcutr(1:ncc),(cutr(i),i=1,ncut)
                  endif
              endif
          endif
      endif
c
c...PPRINT cutter statement
c...Comment out actual Cutter statements
c
  500  if (icmnt) then
          if (cutr(1) .eq. BLADE-10000) then
              write (cbuf(1),1009) lcutr(1:ncc),(cutr(i),i=2,ncut)
          else if (cutr(1) .eq. LATHE-10000) then
              write (cbuf(1),1010) lcutr(1:ncc),(cutr(i),i=2,ncut)
          else
              write (cbuf(1),1004) lcutr(1:ncc),(cutr(i),i=1,ncut)
          endif
      endif
 1001 format (A,6(F8.3,','),F8.3)
 1002 format (A,5(F10.5,','),'$')
 1003 format (7X,F10.5,',',F10.5)
 1004 format ('$$ ',A,6(F8.3,','),F8.3)
 1005 format (A,'BLADE,',3(F8.3,','),F8.3)
 1006 format (A,'BLADE,',3(F10.5,','),F10.5)
 1007 format (A,'LATHE,',4(F8.3,','),F8.3)
 1008 format (A,'LATHE,',4(F10.5,','),F10.5)
 1009 format ('$$ ',A,'BLADE,',3(F8.3,','),F8.3)
 1010 format ('$$ ',A,'LATHE,',4(F8.3,','),F8.3)
 1011 format (A,4(F10.5,','),F10.5)
c
c...Print with user defined precision
c
 550  if (ifl(390).gt.0.and. .not.icmnt) then
c
c......Set up data
c
        posn = ncc + 1
        curlen = ncc
        lin = 1
        flg = 0
c
c......Set the format strings, ordering of cutr statements and lengths
c......The ordering is based on the orders used above when
c......ifl(307) = 0 or 1
c
        do 600 i=strt,ncut
          if (typp.eq.0) then
            if (i.lt.3) then
              ids(i) = i
            else if (i.lt.6) then
              ids(i) = i + 2
            else
              ids(i) = 10 - i
            endif
          else if (typp.eq.1) then
            if (i.lt.3) then
              ids(i) = i
            else if (i.lt.5) then
              ids(i) = i + 1
            else if (i.lt.7) then
              ids(i) = 12 - i
            else
              ids(i) = 3
            endif
          else
            ids(i) = i
          endif
          call varfmt(cutr(ids(i)),ifl(390),fst(ids(i)),lens(ids(i)))
600     continue
c
c......Write to cbuf
c
        write (tstr,'(A)') lcutr(1:ncc)
        if (typp.eq.2.or.typp.eq.3) then
          if (typp.eq.2) then
            tstr(posn:71) = 'BLADE,'
          else
            tstr(posn:71) = 'LATHE,'
          endif
          posn = posn + 6
          curlen = curlen + 6
        endif
        do 700 i=strt,ncut
          tn = curlen + lens(ids(i)) + 1
          write(cutstr,fst(ids(i))) cutr(ids(i))
          if (tn.lt.72-ncc) then
            wflg = 0
            curlen = tn
            if (flg.ne.0) then
              tstr(posn:72) = ','//cutstr(1:lens(ids(i)))
            else
              tstr(posn:72) = cutstr(1:lens(ids(i)))
              posn = posn - 1
              flg = 1
c
c......No comma was used
c
              curlen = curlen - 1
            endif
c
c......Write current string to cbuf and move to new line
c
          else
            tstr(posn:72) = ' $'
            write(cbuf(lin),'(A)') tstr
            if (i.lt.ncut) wflg = 1
            if (lin.gt.1) then
              knc(lin) = strlen1(cbuf(lin))
            endif
            lin = lin + 1
            if (ncc.eq.7) then
              write(tstr,'(7X,A)') cutstr(1:lens(ids(i)))
            else
              write(tstr,'(14X,A)') cutstr(1:lens(ids(i)))
            endif
            curlen = ncc + lens(ids(i))
            posn = ncc
          endif
          posn = posn + lens(ids(i)) + 1
700     continue
c
c......Store the last string if it hasn't already been stored
c
        if (wflg.eq.0) then
          write(cbuf(lin),'(A)') tstr
          knc(lin) = strlen1(cbuf(lin))
        endif
      endif
c
c...Set number of characters in buffer(s)
c
      jnc = strlen1(cbuf(1))
      knc(1) = jnc
      if (cbuf(1)(jnc:jnc) .eq. ',') knc(1) = knc(1) - 1
      if (knc(2) .ne. 0 .and. cbuf(2)(knc(2):knc(2)) .eq. ',')
     1    knc(2) = knc(2) - 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ascudi (ktyp,gbuf,kcnt,csym,cbuf,knc)
c
c   FUNCTION:  Formats a binary CUTTER/DISPLY record for output to an APT
c              source file.
c
c   INPUT:  ktyp    I*4  D1  -  Subtype of CUTTER/DISPLY record.
c                               2 = Symbol, 3 = PART/ALL, 4 = SHANK.
c
c           gbuf    R*8  D7  -  Cutter parameters.
c
c           kcnt    I*4  D1  -  Number of parameters in 'gbuf'.
c
c           csym    C*20 D1  -  Cutter symbol text when 'ktyp' = 2.
c
c   OUTPUT: cbuf    C*80 D2  -  APT source record.
c
c           knc     I*2  D2  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine ascudi (ktyp,gbuf,kcnt,csym,cbuf,knc)
c
      include 'com8a.com'
c
      integer*2 knc(2)
      integer*4 ktyp,kcnt
c
      real*8 gbuf(20)
c
      character*80 cbuf(2),csym
c
      integer*2 ncc,inum(4),ipos,is1,is2,ic1
      integer*4 jnc,strlen1
c
      logical icmnt
c
      character*17 lcutr
      character*80 lbuf,lsym
c
      real*8 rnum
c
      equivalence (rnum,inum)
c
c...Initialize routine
c
      if (ifl(307) .ne. 1) then
          lcutr = '$$ CUTTER/DISPLY,'
          ncc   = 17
      else
          lcutr = 'CUTTER/DISPLY,'
          ncc   = 14
      endif
      knc(1) = 0
      knc(2) = 0
      icmnt = .false.
c
c...CUTTER/DISPLY,symbol (Pre V9.6)
c
      if (ktyp .eq. 2) then
          rnum = gbuf(4)
          if (inum(4) .le. 2) then
              write (cbuf(1),1001) lcutr(1:ncc),csym
          else
              ncut = 8
              if (gbuf(7) .eq. 0. .and. gbuf(8) .eq. 0.) ncut = 6
              if (ncut .eq. 6 .and. gbuf(6) .eq. 0.) ncut = 5
              write (cbuf(1),1001) lcutr(1:ncc),csym,(gbuf(i),i=5,ncut)
          endif
c
c...CUTTER/DISPLY,PART
c
      else if (ktyp .eq. 3) then
          rnum = gbuf(1)
          if (inum(4) .eq. 0) then
              write (cbuf(1),1002) lcutr(1:ncc)
          else
              write (cbuf(1),1003) lcutr(1:ncc)
          endif
c
c...CUTTER/DISPLY,SHANK (Pre V9.6)
c
      else if (ktyp .eq. 4) then
          cbuf(1) = lcutr(1:ncc) // 'SHANK'
          jnc = strlen1(cbuf(1))
          do 500 i=1,kcnt,1
              if (gbuf(i) .eq. 716-10000) then
                  lbuf = 'CUTTER'
                  nc = 6
              else if (gbuf(i) .eq. 157-10000) then
                  lbuf = 'HOLDER'
                  nc = 6
              else
                  write (lbuf,1005) gbuf(i)
                  nc = 9
              endif
              cbuf(1) = cbuf(1)(1:jnc) // ',' // lbuf(1:nc)
              jnc = jnc + nc
  500     continue
c
c...CUTTER/DISPLY,symbol (9.6+)
c
      else if (ktyp .eq. 5 .or. ktyp .eq. 7) then
          lsym   = csym
          ipos   = index(lsym,'&')
          if (ipos .ne. 0) lsym(ipos:ipos) = ','
          nc     = strlen1(lsym)
          is1 = 2
          is2 = 3
          if (ktyp .eq. 5) then
              is1 = 4
              is2 = 5
          endif
          if (gbuf(is1) .eq. 0. .and. gbuf(is2) .eq. 0.) then
              write (cbuf(1),1007) lcutr(1:ncc),lsym(1:nc)
          else
              write (cbuf(1),1006) lcutr(1:ncc),lsym(1:nc),gbuf(is1),
     1                             gbuf(is2)
          endif
c
c...CUTTER/DISPLY,SHANK-HOLDER (9.6+)
c
      else if (ktyp .eq. 6 .or. ktyp .eq. 8) then
          if (ktyp .eq. 6) then
              rnum = gbuf(3)
              is1 = inum(4)
              is2 = inum(3)
          else
              rnum = gbuf(1)
              is1 = inum(3)
              is2 = inum(2)
          endif
          if (is1 .eq. 2) then
              cbuf(1) = lcutr(1:ncc) // 'HOLDER'
          else
              cbuf(1) = lcutr(1:ncc) // 'SHANK'
          endif
          jnc = strlen1(cbuf(1))
c
          if (is2 .ne. 1) then
              lsym   = csym
              ipos   = index(lsym,'&')
              if (ipos .ne. 0) lsym(ipos:ipos) = ','
              cbuf(1)(jnc+1:) = ',' // lsym
              jnc = strlen1(cbuf(1))
          endif
c
          ncut   = 7
          if (ktyp .eq. 6) then
              ic1 = 4
              ncut = 7
          else
              ic1 = 2
              ncut = 5
          endif
          do while (gbuf(ncut) .eq. 0. .and. ncut .ge. is1)
              ncut   = ncut   - 1
          enddo
c
          if (ncut .ge. ic1) then
              do 600 i=ic1,ncut,1
                  if (i .eq. ic1+2 .and. is2 .ne. 1) then
                      cbuf(1)(jnc+1:) = ',OFFSET'
                      jnc    = jnc    + 7
                  endif
                  write (lbuf,1005) gbuf(i)
                  nc = 9
                  cbuf(1)(jnc+1:) = ',' // lbuf(1:nc)
                  jnc = jnc + nc
  600         continue
          endif
c
          if (is1 .ne. 2) then
              if (is1 .eq. 0) then
                  lbuf = 'CUTTER'
                  nc = 6
              else if (is1 .eq. 1) then
                  lbuf = 'HOLDER'
                  nc = 6
              endif
              cbuf(1)(jnc+1:) = ',' // lbuf(1:nc)
              jnc = jnc + nc + 1
          endif
      endif
c
c...Format statements
c
 1001 format (A,A,',',F8.3,',',F8.3,',OFFSET,',F8.3,',',F8.3)
 1002 format (A,'PART')
 1003 format (A,'ALL')
 1004 format (A,'SHANK,',3(F8.3,','),F8.3)
 1005 format (F8.3)
 1006 format (A,A,',OFFSET,',F8.3,',',F8.3)
 1007 format (A,A)
c
c...Set number of characters in buffer(s)
c
      jnc = strlen1(cbuf(1))
      if (cbuf(1)(jnc:jnc) .eq. ',') knc(1) = knc(1) - 1
      if (jnc .gt. 72) then
          do 800 i=jnc,1,-1
              if (cbuf(1)(i:i) .eq. ',') goto 810
  800     continue
  810     ipos = i + 1
          cbuf(2) = '       ' // cbuf(1)(ipos:)
          jnc = strlen1(cbuf(2))
          knc(2) = jnc
          cbuf(1)(ipos:) = '$'
          jnc = strlen1(cbuf(1))
      endif
      knc(1) = jnc
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  aspwrd (ktyp,gbuf,kcnt,cbuf,knc)
c
c   FUNCTION:  Formats a binary post-processor record for output to an
c              APT source file.
c
c   INPUT:  ktyp    I*4  D1  -  Major post-processor word.
c
c           gbuf    R*8  Dn  -  Post-processor command parameters.
c
c           kcnt    I*4  D1  -  Number of parameters in command.
c
c   OUTPUT: cbuf    C*80 D10 -  APT source record.  A post-processor
c                               command can create up to four APT
c                               source records.
c
c           knc     I*2  D10 -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine aspwrd (ktyp,gbuf,kcnt,cbuf,knc)
c
      include 'com8a.com'
c
      integer*2 knc(10)
      integer*4 ktyp,kcnt
c
      real*8 gbuf(10)
c
      character*80 cbuf(10)
c
      integer*2 ibuf(24),ifrst,is1,is4,iv,is
      integer*4 ival,jbuf(12),strlen1,ncc
c
      real*8 rbuf(6)
c
      character*24 ctok
      character*40 lbuf
c
      equivalence (rbuf,ibuf,jbuf, lbuf)
      data is1 /4/, is4 /1/
c
c...Format post word
c
      inc    = 1
      iv     = ktyp
      call asvoc (iv,1,ctok)
      ncc = strlen1(ctok)
      if (ncc .lt. 6) ncc = 6
      cbuf(1)(1:ncc) = ctok
      nc = ncc
      do 50 i=1,10,1
          knc(i) = 0
   50 continue
c
c...Store minor parameters
c
      if ((ktyp .lt. 1043 .or. ktyp .gt. 1046) .and. ktyp .ne. 937 .and.
     1    ktyp .ne. 1091) then
          cbuf(inc)(nc+1:nc+1) = '/'
          ifrst  = 1
          is     = 1
          do while (is .le. kcnt)
              rbuf(1) = gbuf(is)
c
c...MACHIN/name
c
              if (ktyp .eq. 1015 .and. ifrst .eq. 1) then
                  if (jbuf(1) .eq. -1 .and. jbuf(2) .gt. 0 .and.
     1                jbuf(2) .le. 40) then
                      nc1    = jbuf(2)
                      do 75 i=1,(nc1+7)/8,1
                          is     = is     + 1
                          rbuf(i) = gbuf(is)
   75                 continue
                      cbuf(inc)(nc+2:nc+nc1+1) = lbuf(1:nc1)
                      nc     = nc     + nc1    + 1
                  else
                      nc1    = strlen1(lbuf)
                      cbuf(inc)(nc+2:nc+nc1+1) = lbuf(1:nc1)
                      nc     = nc     + nc1    + 1
                  endif
c
c...Real value
c
              else if (ibuf(is1) .ne. 0 .or. ibuf(is4) .eq. 0) then
                  if (ifrst .eq. 0) cbuf(inc)(nc+1:nc+1) = ','
                  nc     = nc     + 2
                  if (nc .gt. 61) then
                      cbuf(inc)(nc:nc)='$'
                      knc(inc) = nc
                      if (inc .eq. 10) go to 8000
                      inc    = inc    + 1
                      cbuf(inc)(1:7)=' '
                      nc     = 8
                  endif
c
c...Output real value using proper format
c...depending on magnitude of number
c...Sharon  -  3-JUL-91
c
  700             if (ifl(386) .eq. 0 .and.
     1                (gbuf(is)-dint(gbuf(is)) .eq. 0.)) then
                      ival = gbuf(is)
                      write (cbuf(inc)(nc:nc+9),706) ival
  706                 format (i10)
                  else if (gbuf(is) .le. 99999.) then
                      write (cbuf(inc)(nc:nc+9),701) gbuf(is)
  701                 format (f10.4)
                  else if (gbuf(is) .le. 999999.) then
                      write (cbuf(inc)(nc:nc+9),702) gbuf(is)
  702                 format (f10.3)
                  else if (gbuf(is) .le. 9999999.) then
                      write (cbuf(inc)(nc:nc+9),703) gbuf(is)
  703                 format (f10.2)
                  else if (gbuf(is) .le. 99999999.) then
                      write (cbuf(inc)(nc:nc+9),704) gbuf(is)
  704                 format (f10.1)
                  else
                      write (cbuf(inc)(nc:nc+9),705) gbuf(is)
  705                 format (f10.0)
                  endif
                  nc     = nc     + 9
c
c...Minor word
c
              else
                  call asvoc (ibuf(is4),0,ctok)
                  ncc    = strlen1(ctok)
                  if (ncc .lt. 6) ncc = 6
                  nc     = nc     + 1
                  if (ifrst .eq. 0) cbuf(inc)(nc:nc) = ','
                  if (nc .gt. 64) then
                      nc     = nc     + 1
                      cbuf(inc)(nc:nc) = '$'
                      knc(inc) = nc
                      if (inc .eq. 10) go to 8000
                      inc    = inc    + 1
                      cbuf(inc)(1:7) = ' '
                      nc     = 7
                  endif
                  cbuf(inc)(nc+1:nc+ncc) = ctok
                  nc     = nc     + ncc
              endif
              ifrst  = 0
              is     = is     + 1
          enddo
          knc(inc) = nc
c
c...Store text post-processor command
c
      else
          do 1500 i=1,8,1
              rbuf(1) = gbuf(i)
              cbuf(1)(nc+1:nc+8) = lbuf
              nc     = nc     + 8
 1500     continue
          rbuf(1) = gbuf(9)
          cbuf(1)(71:72) = lbuf(1:2)
          knc(1) = 72
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  asclnm (gbuf,inc,cbuf,knc)
c
c   FUNCTION:  Formats the clfile name and date record (7300) for output
c              to an APT source file.
c
c   INPUT:  gbuf    R*8  Dn  -  Post-processor command parameters.
c
c   OUTPUT: cbuf    C*80 D4  -  APT source record.  A post-processor
c                               command can create up to two APT
c                               source records.
c
c           knc     I*2  D4  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine asclnm (gbuf,cbuf,knc)
c
      include 'com8a.com'
c
      integer*2 knc(4)
c
      real*8 gbuf(*)
c
      character*80 cbuf(4)
c
      integer*4 strlen1,nc,i,inc
c
      real*8 rbuf
c
      character*8 lbuf
      character*88 tbuf
c
      equivalence (rbuf,lbuf)
c
c...Initialize routine
c
      nc     = 7
      tbuf   = 'REMARK '
      inc    = 1
      knc(2) = 0
c
c...Store clfile name
c
      do 100 i=1,10,1
          rbuf   = gbuf(i)
          tbuf(nc+1:nc+8) = lbuf
          nc     = nc     + 8
 100  continue
      nc     = strlen1(tbuf)
      if (nc .gt. 72) then
          cbuf(1) = tbuf(1:72)
          knc(1) = 72
          inc    = 2
          cbuf(2) = 'REMARK ' // tbuf(73:nc)
          nc     = 7 + (nc-72)
      else
          cbuf(1) = tbuf(1:nc)
      endif
c
c...Store date
c
      if (nc+14 .gt. 72) then
          knc(1) = nc
          inc    = 2
          cbuf(2) = 'REMARK '
          nc     = 7
      endif
      rbuf   = gbuf(11)
      cbuf(inc)(nc+1:) = '     ' // lbuf
      rbuf   = gbuf(12)
      cbuf(inc)(nc+14:) = lbuf
      nc     = nc     + 16
c
c...Store time
c
      if (nc+10 .gt. 72) then
          knc(1) = nc
          inc    = 2
          cbuf(2) = 'REMARK '
          nc     = 7
      endif
      rbuf    = gbuf(13)
      cbuf(inc)(nc+1:) = '  ' // lbuf
      nc     = nc     + 10
c
c...Store program name
c
      if (nc+10 .gt. 72) then
          knc(1) = nc
          inc    = 2
          cbuf(2) = 'REMARK '
          nc     = 7
      endif
      rbuf    = gbuf(14)
      cbuf(inc)(nc+1:) = '  ' // lbuf
      nc     = nc     + 10
c
c...Store program version number
c
      if (nc+10 .gt. 72) then
          knc(1) = nc
          inc    = 2
          cbuf(2) = 'REMARK '
          nc     = 7
      endif
      rbuf    = gbuf(15)
      cbuf(inc)(nc+1:) = '  ' // lbuf
      nc     = nc     + 10
c
c...End of routine
c
 8000 knc(inc) = nc
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  asgoto (ktyp,gbuf,knpt,kfl,kisn,cbuf,knc)
c
c   FUNCTION:  Formats a binary Motion record for output to an APT
c              source file.
c
c   INPUT:  ktyp    I*4  D1  -  3 = FROM, 5 = GOTO, 6 = Continuation
c                               record.
c
c           gbuf    R*8  Dn  -  GOTO point values.
c
c           knpt    I*2  D1  -  Number of values in point; 3 = MULTAX/OFF
c                               6 = MULTAX/ON.
c
c           kfl     I*2  D1  -  1 = Output ISN as first part of APT
c                               source record.
c
c           kisn    I*4  D1  -  ISN of this motion record.
c
c   OUTPUT: cbuf    C*80 D4  -  APT source record.
c
c           knc     I*2  D4  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine asgoto (ktyp,gbuf,knpt,kfl,kisn,cbuf,knc)
c
      include 'com8a.com'
c
      integer*2 knpt,knc(4),kfl
      integer*4 kisn,ktyp
c
      real*8 gbuf(10)
c
      character*80 cbuf(4)
c
      real*8 rbuf(3),sec
c
c...WinNT
c
      character*6 goto1,from
c
      data goto1 /'GOTO  '/, from /'FROM  '/
c
c...NCLIPV Apt Source file
c...Store ISN
c
      knc(1) = 0
      knc(2) = 0
      if (kfl .eq. 1) call aptisn (kisn,cbuf(1),knc(1))
c
c...Store GOTO or FROM
c
      if (ktyp.eq.3) then
          cbuf(1)(knc(1)+1:knc(1)+6) = from
      else
          cbuf(1)(knc(1)+1:knc(1)+6) = goto1
      endif
      knc(1) = knc(1) + 7
      cbuf(1)(knc(1):knc(1)) = '/'
c
c...Store coordinates of motion point
c
      icnt    = 0
c
c......Apply APTSRC matrix
c
      rbuf(1) = gbuf(1)
      rbuf(2) = gbuf(2)
      rbuf(3) = gbuf(3)
      if (ifl(353) .eq. 1) call conent (rbuf,sc(183),3)
 
      if (ifl(319) .eq. 0) then
          write (cbuf(1)(knc(1)+1:knc(1)+32),10) rbuf(1),rbuf(2),rbuf(3)
          knc(1) = knc(1) + 32
      else
          write (cbuf(1)(knc(1)+1:knc(1)+44),11) rbuf(1),rbuf(2),rbuf(3)
          knc(1) = knc(1) + 44
      endif
   10 format (2(f10.4,','),f10.4)
   11 format (2(f14.6,','),f14.6)
c
c...Store tool axis vector
c
      if (knpt .eq. 6) then
          knc(1) = knc(1) + 1
          cbuf(1)(knc(1):knc(1)) = ','
          rbuf(1) = gbuf(4)
          rbuf(2) = gbuf(5)
          rbuf(3) = gbuf(6)
c
c......Apply APTSRC matrix
c
          if (ifl(353) .eq. 1) then
              call conent (rbuf,sc(183),4)
              sec = dsqrt(rbuf(1)**2 + rbuf(2)**2 + rbuf(3)**2)
              rbuf(1) = rbuf(1) / sec
              rbuf(2) = rbuf(2) / sec
              rbuf(3) = rbuf(3) / sec
          endif
 
          if (kfl .eq. 1 .and. ifl(319) .eq. 0) then
              write (cbuf(1)(knc(1)+1:knc(1)+29),20) rbuf(1),rbuf(2),
     1                  rbuf(3)
   20         format (2(f9.6,','),f9.6)
              knc(1) = knc(1) + 29
          else if (ifl(319) .eq. 0) then
              write (cbuf(1)(knc(1)+1:knc(1)+32),30) rbuf(1),rbuf(2),
     1                  rbuf(3)
   30         format (2(f10.7,','),f10.7)
              knc(1) = knc(1) + 32
          else
              cbuf(1)(knc(1)+1:knc(1)+3) = ' $'
              knc(1) = knc(1) + 2
              write (cbuf(2),40) rbuf(1),rbuf(2),rbuf(3)
   40         format (11x,2(f10.7,',    '),f10.7)
              knc(2) = 51
          endif
      endif
      return
      end
