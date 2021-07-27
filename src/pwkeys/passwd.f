c
c***********************************************************************
c
c   FILE NAME: passwd.for
c   CONTAINS:
c               pwdall  pwdaut  pwdbtc  pwdchk  pwdclr  pwdcmp  pwdctb
c               pwdcti  pwddnm  pwddat  pwddcv  pwddvc  pwddea  pwdext
c               pwdfil  pwdlen  pwdlic  pwdopt  pwdpas  pwdrd   pwdsid
c               pwdspc  pwdtrm
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c      MODULE NAME AND RELEASE LEVEL
c        passwd.f , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:10
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  pwdall (clprog,clmsg,kerr)
c
c   FUNCTION:  This routine allocates a user for either NCLCAM or
c              NCLCADD.  It assumes that the calling program has already
c              verified that it is authorized to run on this computer.
c
c   INPUT:  clprog  B*1  D20   -  Null terminated text string that con-
c                                 tains the name of the calling program.
c
c   OUTPUT: clmsg   B*1  D80   -  Null terminated error text when an
c                                 error occurred.
c
c           kerr    I*4  D1    -  0 = Success.  1 = Number of authorized
c                                 users exceeded.  2 = License manager
c                                 has not been run.
c
c***********************************************************************
       subroutine pwdall (clprog,clmsg,kerr)
c
C VAX-START
C     include 'passwd.inc'
C     include '($dvidef)'
C     include '($dcdef)'
C     include '($secdef)'
C     include '($jpidef)'
C VAX-END
c
       byte clprog(20),clmsg(80)
c
       integer*4 kerr
c
       character*80 msg
c
       integer*4 nc
c
       character*20 cprog
c
C VAX-START
C     integer*4 inadr(2),jflag,jstat,sys$mgblsc,jid,inc
c
C     byte lbuf(64),ltemp(6),lwin1(512),lwin2(512),lterm(64),lwina(513),
C    1     lwinb(513),lwinc(513),lwin3(512)
c
C     character*7 lsect
C     character*8 pnum
C     character*80 nnum
c
C     byte pbuf(8),nbuf(80)
C     equivalence (pnum,pbuf), (nnum,nbuf)
C     equivalence (lwin1,lwina(513)), (lwin2,lwinb(513))
C     equivalence (lwin3,lwinc(513))
c
c...Initialize routine
c
C VAX-END
       kerr   = 0
       msg   = ' '
       call pwdbtc (clprog,cprog,nc)
C SGI-SUN-IBM-OSF-HPX-WNT-START
       call pwcall (clprog,kerr)
       if (kerr .eq. 1) go to 9000
       if (kerr .eq. 2) go to 9100
C SGI-SUN-IBM-OSF-HPX-WNT-END
C VAX-START
C     ntrm1  = 0
C     ntrm2  = 0
C     ierr1  = 0
C     ierr2  = 0
c
c...Determine type of user
c...CAM or CADD
c
C     if (cprog .eq. 'NCLCAM') then
C         lsect  = '_CAM700'
C         inc    = 1
C         inadr(1) = %loc(lwin1)
C         inadr(2) = %loc(lwin1)
C     else if (cprog .eq. 'NCLCADD') then
C         lsect  = '_CAD700'
C         inc    = 2
C         inadr(1) = %loc(lwin2)
C         inadr(2) = %loc(lwin2)
C     else if (cprog .eq. 'NCLIPV') then
C         lsect  = '_NCLIPV'
C         inc    = 3
C         inadr(1) = %loc(lwin3)
C         inadr(2) = %loc(lwin3)
C     else
C         go to 9100
C     endif
c
c...Get the process id & name
c
C     jid    = jpi$_pid
C     istat  = lib$getjpi (jid,,,,pnum,)
C     if (.not. istat) go to 9000
C     jid    = jpi$_prcnam
C     istat  = lib$getjpi (jid,,,,nnum,)
C     if (.not. istat) go to 9100
c
c...Set up type of section to be created
c
C     jflag = sec$m_sysgbl .or. sec$m_wrt .or. sec$m_expreg
c
c...Map to global section
c
C     jstat = sys$mgblsc (inadr,rtadr(1,inc),,%val(jflag),lsect,,)
C     if (.not. jstat) goto 9000
c
c...Clear out all terminated users
c
C     call pwdclr (%val(rtadr(1,inc)))
c
c...Authorize this user
c
C     call pwdtrm (%val(rtadr(1,inc)),pbuf,nbuf,ntrm(inc),kerr)
C     if (kerr .ne. 0) go to 9000
C VAX-END
c
c...End of routine
c
 8000 call pwdctb (msg,clmsg)
      return
c
c...Exceeded number of users
c
 9000 kerr  = 1
       msg   = 'Number of ' // cprog(1:nc) // ' users exceeded.'
       go to 8000
c
c...License not loaded
c
 9100 kerr  = 2
      msg   = cprog(1:nc) // ' license has not been loaded.'
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdaut (clprog,clopt,clver,kopt,clmsg,kerr)
c
c   FUNCTION:  This routine verifies that the calling program is author-
c              ized to run on this computer.
c
c   INPUT:  clprog  B*1  D20   -  Null terminated text string contain-
c                                 ing the name of the calling program.
c
c           clopt   B*1  D60   -  Null terminated text string contain-
c                                 ing a list of acceptable options, se-
c                                 parated by commas.
c
c           clver   B*1  D11   -  Null terminated text string contain-
c                                 ing the version date of the calling
c                                 program, in the format '31-MAY-1999'.
c
c   OUTPUT: kopt    I*4  D8    -  Returns 1 in the array position for
c                                 each option that is licensed.
c
c           clmsg   B*1  D80   -  Null terminated text string of message
c                                 when an error occurred.  Returns left
c                                 over options when there is no error.
c
c           kerr    I*4  D1    -  Returns 1 when this program is not
c                                 authorized to run on this computer.
c                                 2 = Could not access license data
c                                 base.  3 = Could not obtain system id.
c                                 Returns 0 when program is licensed.
c
c***********************************************************************
c
      subroutine pwdaut (clprog,clopt,clver,kopt,clmsg,kerr)
c
c...remove use these libs for MFC
c...Yurong 3/24/98
c
C WXX-START
c...      USE DFPORT
C     INTERFACE
C        INTEGER*4 FUNCTION getenvC(keypt, valuept, len)
C           !DEC$ ATTRIBUTES C, ALIAS:'_getenvC'::getenvC
C        integer keypt, valuept, len
C        END FUNCTION
C     END INTERFACE

C WXX-END
      include 'passwd.inc'
c
      integer*4 kerr,kopt(8)
c
      byte clprog(20),clopt(60),clver(12),clmsg(80)
c
      integer*4 i,inum1,inum2,idat(51),irec,irs(9),ire(9),ides(51),ncid,
     1          nc1,nc2,pwdlen,nopt1,nopt2,ifl,iop1(8),iop2(8),j,inum3,
     2          inum4,irecl,istat, nc, kflag, getenvc
c
c      integer*4 getenvc
      character*8 lopt1(8),lopt2(8)
      character*20 cprog,lpass,lid, string
      character*60 lbuf1,lbuf2,copt
      character*80 msg
      character*1024 dev,fnam
      character*204 ldat
      character*13 lic_key
c      character*1024 lic_key
      data lic_key /'NCCS_LICENSE '/
c
      equivalence (idat,ldat)
c
      data irs /1,41,61,81,141,143,154,165,177/
      data ire /40,60,80,140,142,153,164,176,195/
      
      integer, parameter :: out_unit=20
c
c...Open licensing data base
c
      kerr   = 0
c
C VAX-START
C      open (unit=1,file='NCCS$LICENSE:NCCS_LICENSE.DBA',status='old',
C    1      access='direct',recl=51,readonly,err=9000)
C VAX-END
c
C WNT-SUN-IBM-DOS-HPX-START
      irecl  = 204
C WNT-SUN-IBM-DOS-HPX-END
C SGI-OSF-START
C     irecl  = 51
C SGI-OSF-END
C SUN-SGI-IBM-DOS-OSF-HPX-START
C     call getenv ('NCCS_LICENSE',dev)
C     nc1    = pwdlen(dev)
C DOS-END
C     fnam   = dev(1:nc1) // '/' // 'NCCS_LICENSE.DBA'
C SGI-SUN-IBM-OSF-HPX-END
c
c...changed from directly call getenv for WNT
c...because DFPORT lib is conflicks with MFC lib
c...when we use this routine for mpost
c...Yurong 3/13/98
c
C WNT-START
      nc1 = getenvc(lic_key, dev, 12)
      fnam   = dev(1:nc1) // '/' // 'NCCS_LICENSE.DBA'
C WNT-END

C DOS-START
C     if (dev(nc1:nc1) .eq. '\') nc1 = nc1 - 1
C     fnam   = dev(1:nc1) // '\' // 'NCCS_LIC.DBA'
C SUN-IBM-SGI-OSF-HPX-START
C      open (unit=1,file=fnam,status='old',access='direct',recl=irecl,
C    1       iostat=istat,err=9000)
C SUN-IBM-SGI-DOS-OSF-HPX-END
C WNT-START
      open (unit=1,file=fnam,status='old',access='direct',recl=51,
     1       iostat=istat,err=9000)
C WNT-END
c
c...Get hardware id
c
c
c...Get hardware id after read license option
c...Yurong changed 1/18/99
c
c...      call pwdsid (lid,ncid,kerr)
c...      if (kerr .ne. 0) go to 9200
c
c...Read descriptor record
c
      call pwdrd (1,ides,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Find requested software
c
      irec   = ides(2)
      call pwdbtc (clprog,cprog,nc1)
  100 if (irec .eq. 0 .or. irec .ge. ides(1)) go to 9100
      call pwdrd (irec,idat,kerr)
      if (kerr .ne. 0) go to 9000
c
c....ldat(irs(3):ire(3)) is always != '*'
c....in WINNT because the string length is defferent
c....changed by Yurong
c
cc      if (idat(50) .ne. 0 .and. (ldat(irs(3):ire(3)) .eq. cprog .or.
cc     1    ldat(irs(3):ire(3)) .eq. '*')) go to 500
      nc = pwdlen(ldat(irs(3):ire(3)))
C WNT-START ...consider end mark because use API	
      if(nc .eq. 2) nc = 1
C WIN_END
      if (idat(50) .ne. 0 .and. (ldat(irs(3):ire(3)) .eq. cprog .or.
     1    (ldat(irs(3):irs(3)) .eq. '*') .and. nc .eq. 1)) go to 500
  200 irec   = idat(51)
      go to 100
c
c...Found match
c...Check and make sure password is valid
c
  500 do 600 i=1,9,1
          LICDAT(i) = ldat(irs(i):ire(i))
          LICNC(i) = pwdlen(LICDAT(i))
C WNT-START ...consider end mark because use API	
      if(LICDAT(i)(LICNC(i):LICNC(i)) .eq. ' ') LICNC(i) = LICNC(i) -1
C WIN_END

  600 continue
c
c...read option first, then decide how to read system id
c...Yurong changed 1/18/99
c
      call pwdbtc (clopt,copt,nc1)
      call pwdopt (copt,nc1,lopt1,iop1,nopt1)
      call pwdopt (LICDAT(4),LICNC(4),lopt2,iop2,nopt2)
      kflag = 0
      do 630 i=1,nopt2,1
          if (lopt2(i)(1:iop2(i)) .eq. 'DONGLE') then
               kflag = 1
               goto 640
          else if (lopt2(i)(1:iop2(i)) .eq. 'FLOAT') then
               kflag = 2
               goto 640
          endif
  630 continue
  640 call pwdsid (clprog,lid,ncid,kflag,kerr)
      if (kerr .ne. 0) go to 9200

      if (LICDAT(8) .ne. '*') then
          LICDAT(8) = lid
          LICNC(8) = ncid
      endif
      call pwdpas (LICDAT,LICNC,lpass,kerr)
      if (kerr .ne. 0 .or. lpass .ne. LICDAT(9)) go to 200
c
c......Check expiration & version dates
c
      call pwddat (lbuf1,nc1)
      call pwddcv (lbuf1,nc1,lbuf2,nc2,inum1,kerr)
      if (kerr .ne. 0) go to 9100
      call pwddcv (LICDAT(6),LICNC(6),lbuf2,nc2,inum2,kerr)
      if (kerr .ne. 0 .or. inum1 .gt. inum2) go to 9100
c
      call pwdbtc (clver,lbuf1,nc1)
      call pwddcv (lbuf1,nc1,lbuf2,nc2,inum3,kerr)
      if (kerr .ne. 0) go to 9100
      call pwddcv (LICDAT(7),LICNC(7),lbuf2,nc2,inum4,kerr)
      if (kerr .ne. 0 .or. inum3 .gt. inum4) go to 9100
c
c...Verify version date is not greater
c...than current date
c
      if (inum3 .gt. inum1) go to 9100
      if ((LICDAT(1) .eq. '*' .or. LICDAT(8) .eq. '*') .and.
     1    abs(inum4-inum1) .gt. 31) go to 9100
c
c...Return valid options
c
      call pwdbtc (clopt,copt,nc1)
      do 700 i=1,8,1
          kopt(i) = 0
  700 continue
      call pwdopt (copt,nc1,lopt1,iop1,nopt1)
      call pwdopt (LICDAT(4),LICNC(4),lopt2,iop2,nopt2)
c
c...added for Dongle
c...Yurong
c
      j = 0
      do 710 i=1,nopt2,1
          if (j .ne. 0) then
                  lopt2(i) = lopt2(i+1)
                  iop2(i) = iop2(i+1)
          else
              if (lopt2(i)(1:iop2(i)) .eq. 'DONGLE') then
                  nopt2 = nopt2 - 1
                  lopt2(i) = lopt2(i+1)
                  iop2(i) = iop2(i+1)
            j = i
              endif
          endif
  710 continue

      if (lopt2(1) .eq. '*') then
          nopt2  = nopt1
          do 720 i=1,nopt1,1
              if (lopt1(i) .eq. '3AXIS' .or. lopt1(i) .eq. 'VMX' .or.
     1            lopt1(i) .eq. 'RUNTIME') then
                  nopt2  = nopt2  - 1
              else
                  lopt2(i) = lopt1(i)
                  iop2(i) = iop1(i)
              endif
  720     continue
      endif
      msg    = ' '
      nc1    = 0
      do 900 i=1,nopt2,1
          do 800 j=1,nopt1,1
              call pwdcmp (lopt1(j),iop1(j),lopt2(i),iop2(i),ifl)
              if (ifl .eq. 0) then
                  kopt(j) = 1
                  go to 900
              endif
  800     continue
          if (nc1 .eq. 0) then
              msg    = lopt2(i)(1:iop2(i)) // ','
          else
              lbuf1 = msg(1:nc1) // lopt2(i)(1:iop2(i)) // ','
              msg    = lbuf1
          endif
          nc1    = nc1    + iop2(i) + 1
  900 continue
      msg = ' '
c
c...End of routine
c
 8000 call pwdctb (msg,clmsg)
      close (unit=1)
      return
c
c...Error processing license file
c
 9000 kerr   = 2
C WNT-SUN-IBM-SGI-VAX-OSF-HPX-START
      msg    = 'Error processing NCCS_LICENSE.DBA.'
C WNT-SUN-IBM-SGI-VAX-OSF-HPX-END
C DOS-START
C     msg    = 'Error processing NCCS_LIC.DBA.'
C     call iostat_msg (istat,msg)
C DOS-END
      go to 8000
c
c...Invalid password
c
 9100 kerr   = 1
      msg    = 'You are not authorized to run this software product.' //
     1         '  SID = ' // lid(1:ncid)
      go to 8000
c
c...Unable to get system id
c
 9200 kerr   = 3
      msg   = 'Unable to obtain system id.'
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdbtc (cldat,cdat,knc)
c
c   FUNCTION:  This routine converts a null terminated text string into
c              character data.
c
c   INPUT:  cldat   B*1  Dn    -  Null terminated text string.
c
c   OUTPUT: cdat    C*n  D1    -  Character data to receive text string.
c
c           knc     I*4  D1    -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine pwdbtc (cldat,cdat,knc)
c
      integer*4 knc
c
      byte cldat(*)
      character*(*) cdat
c
      integer*4 i
c
      byte snum(1024)
      character*1024 sbuf
c
      equivalence (sbuf,snum)
c
c...Convert null terminated string
c...to character data
c
      do 100 i=1,1024,1
          if (cldat(i) .eq. 0) go to 200
          snum(i) = cldat(i)
  100 continue
  200 knc    = i      - 1
      cdat   = sbuf(1:knc)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdchk (cbuf,knc,cout,knco,kst,ken,kfl,kerr)
c
c   FUNCTION:  This routine verifies that all fields in the program li-
c              cence record are valid.
c
c   INPUT:  cbuf    C*n  D9    -  1 = Comany name, 2 = Hardware platform,
c                                 3 = Software product, 4 = Options, 5 =
c                                 Number of users, 6 = Termination date,
c                                 7 = Program version date, 8 = System
c                                 ID, 9 = Password.
c
c           knc     I*4  D9    -  Number of characters in 'cbuf'.
c
c           kst     I*4  D1    -  Starting field to check.
c
c           ken     I*4  D1    -  Ending field to check.
c
c   OUTPUT: cout    C*n  D9    -  Input fields converted to format re-
c                                 quired to calculate password.
c
c           knco    I*4  D9    -  Number of characters in 'cout'.
c
c           knc     I*4  D1    -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine pwdchk (cbuf,knc,cout,knco,kst,ken,kfl,kerr)
c
      integer*4 knc(9),knco(9),kst,ken,kfl,kerr
c
      character*(*) cbuf(9),cout(9)
c
      integer*4 nc,ipt,inc,ie,iop(8),isw,inum,i,index,pwdlen,ierr,ifl
c
      character*8 lopt(8),one(10),ten(6)
      character*80 ldat,sbuf
c
      data one /'ZERO','ONE','TWO','THREE','FOUR','FIVE','SIX','SEVEN',
     1          'EIGHT','NINE'/
      data ten /'TEN','TWENTY','THIRTY','FOURTY','FIFTY','SIXTY'/
c
c...Go to appropriate field logic
c
      kerr   = 0
      go to (100,200,300,400,500,600,700,800,900), kst
c
c...Company
c
  100 call pwdspc (cbuf(1),ldat,nc)
      if (nc .eq. 0) then
          kerr   = 1
          go to 8000
      endif
      if (kfl .eq. 1) then
          call pwdfil (ldat,nc)
          cout(1) = ldat
          knco(1) = nc
      endif
      if (ken .eq. 1) go to 8000
c
c...Hardware
c
  200 call pwdspc (cbuf(2),ldat,nc)
      if (nc .eq. 0) then
          kerr   = 2
          go to 8000
      endif
      if (kfl .eq. 1) then
          call pwdfil (ldat,nc)
          cout(2) = ldat
          knco(2) = nc
      endif
      if (ken .eq. 2) go to 8000
c
c...Software
c
  300 call pwdspc (cbuf(3),ldat,nc)
      if (nc .eq. 0) then
          kerr   = 3
          go to 8000
      endif
      if (kfl .eq. 1) then
          call pwdfil (ldat,nc)
          cout(3) = ldat
          knco(3) = nc
      endif
      if (ken .eq. 3) go to 8000
c
c...Options
c
  400 call pwdspc (cbuf(4),ldat,nc)
      ipt    = 0
c
c......Break out individual options
c
      if (nc .ne. 0) then
          inc    = 1
  450     ie     = index(ldat(inc:nc),',')
          if (ie .eq. 0) then
              ie     = nc
          else
              ie     = inc    + ie     - 2
          endif
          if (ie-inc+1 .gt. 8 .or. ie .lt. inc .or. ipt .eq. 8) then
              kerr   = 4
              go to 8000
          endif
c
          ipt    = ipt    + 1
          lopt(ipt) = ldat(inc:ie)
          iop(ipt) = ie    - inc    + 1
          inc    = ie     + 2
          if (inc .le. nc) go to 450
c
c......Sort options
c
  460     isw    = 0
          do 470 i=1,ipt-1,1
              call pwdcmp (lopt(i),iop(i),lopt(i+1),iop(i+1),ifl)
              if (ifl .eq. 0) then
                  kerr   = 4
                  go to 8000
              else if (ifl .eq. 1) then
                  isw    = 1
                  sbuf   = lopt(i)
                  lopt(i) = lopt(i+1)
                  lopt(i+1) = sbuf
                  inum   = iop(i)
                  iop(i) = iop(i+1)
                  iop(i+1) = inum
              endif
  470     continue
          if (isw .eq. 1) go to 460
      endif
c
c......Store options
c
      if (kfl .eq. 1) then
          if (ipt .eq. 0) then
              knco(4) = 0
          else
              cout(4) = lopt(1)(1:iop(1))
              knco(4) = iop(1)
              do 480 i=2,ipt,1
                  sbuf   = cout(4)(1:knco(4)) // ',' //
     1                     lopt(i)(1:iop(i))
                  cout(4) = sbuf
                  knco(4) = knco(4) + iop(i) + 1
  480         continue
              call pwdfil (cout(4),knco(4))
          endif
      endif
      if (ken .eq. 4) go to 8000
c
c...Number of users
c
  500 call pwdspc (cbuf(5),ldat,nc)
      if (nc .eq. 0) then
          kerr   = 5
          go to 8000
      endif
c
      call pwdcti (ldat(1:nc),inum,ierr)
      if (inum .le. 0 .or. inum .ge. 64) then
          kerr   = 5
          go to 8000
      endif
c
      if (kfl .eq. 1) then
          if (inum .gt. 9) then
              ldat   = ten(inum/10)
              nc     = pwdlen(ldat) + 1
              ldat(nc:nc) = '-'
              inum   = inum   - (inum/10*10)
          else
              ldat   = '-'
              nc     = 1
          endif
c
          cout(5) = ldat(1:nc) // one(inum+1)
          knco(5) = pwdlen(cout)
      endif
      if (ken .eq. 5) go to 8000
c
c...Termination date
c
  600 call pwdspc (cbuf(6),ldat,nc)
      call pwddcv (ldat,nc,ldat,nc,inum,ierr)
      if (ierr .eq. 1) then
          kerr   = 6
          go to 8000
      endif
      if (kfl .eq. 1) then
          call pwdfil (ldat,nc)
          cout(6) = ldat
          knco(6) = nc
      endif
      if (ken .eq. 6) go to 8000
c
c...Version date
c
  700 call pwdspc (cbuf(7),ldat,nc)
      call pwddcv (ldat,nc,ldat,nc,inum,ierr)
      if (ierr .eq. 1) then
          kerr   = 7
          go to 8000
      endif
      if (kfl .eq. 1) then
          call pwdfil (ldat,nc)
          cout(7) = ldat
          knco(7) = nc
      endif
      if (ken .eq. 7) go to 8000
c
c...Hardware ID
c
  800 if (cbuf(8) .eq. '*') then
          call pwdspc (cbuf(7),ldat,nc)
          call pwddcv (ldat,nc,sbuf,nc,inum,ierr)
          ldat   = cbuf(7)(2:6) // sbuf(1:nc)
          nc     = nc     + 5
      else
          call pwdspc (cbuf(8),ldat,nc)
          call pwdcti (ldat(1:nc),inum,ierr)
      endif
      if (ierr .eq. 1) then
          kerr   = 8
          go to 8000
      endif
      if (kfl .eq. 1) then
          call pwdfil (ldat,nc)
          cout(8) = ldat
          knco(8) = nc
      endif
      if (ken .eq. 8) go to 8000
c
c...Password
c
  900 kerr   = 9
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdclr (clwin)
c
c   FUNCTION:  This routine clears out all Licensing manager slots that
c              are no longer being used.
c
c   INPUT:  clwin   B*1  D512  -  Licensing manager's buffer.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C VAX-START
C     subroutine pwdclr (clwin)
c
C     include '($jpidef)'
C     include '($ssdef)'
c
C     byte clwin(512)
c
C     integer*4 j,ipid,istat,jid,ierr,k,ipt,ilpt
c
C     byte lbuf(100),lj(4),mbuf(8)
c
C     character*8 lpid
c
c...Get process id's
c
C     ipt    = 1
C     ilpt   = 0
c
C     do 300 j=1,31,1
C         if (clwin(ipt) .eq. -1) go to 400
c
C         if (clwin(ipt) .eq. 0) go to 200
c
c...Allocated user
c...Check to see if it is still active
c
C         k = ipt + clwin(ipt) + 1
C         decode (8,188,clwin(ipt),err=200) jid
C 188     format (z8)
C         ipid = jpi$_pid
C         istat = lib$getjpi (ipid,jid,,,lpid,)
C         if (istat .ne. ss$_nonexpr) go to 200
c
c......User is not active
c......Clear out slot
c
C         clwin(ipt) = 0
C 200     ipt    = ipt    + 16
C 300 continue
C 400 return
C     end
C VAX-END
c
c***********************************************************************
c
c   SUBROUTINE:  pwdcmp (cdat1,knc1,cdat2,knc2,kfl)
c
c   FUNCTION:  This routine checks 2 character strings and determines
c              which should go 1st for sorting purposes.  The strings
c              'NCLCAM' and 'NCLCADD' will be 1st and 2nd and then the
c              order will be determined alphabetically.
c
c   INPUT:  cdat1   C*n  D1    -  1st string to compare.
c
c           knc1    I*4  D1    -  Number of characters in 'cdat1'.
c
c           cdat2   C*n  D1    -  2nd string to compare.
c
c           knc2    I*4  D1    -  Number of characters in 'cdat2'.
c
c   OUTPUT: kfl     I*4  D1    -  Returns -1 when 'cdat1' should be 1st,
c                                 0 when the strings are equal, and 1
c                                 when 'cdat2' should be 1st.
c
c***********************************************************************
c
      subroutine pwdcmp (cdat1,knc1,cdat2,knc2,kfl)
c
      integer*4 knc1,knc2,kfl
c
      character*(*) cdat1,cdat2
c
      integer*4 i,nc,ix1,ix2,index
c
      character*8 lcam,lcad
      character*36 lalph
c
      data lcam /'NCLCAM'/, lcad /'NCLCADD'/
      data lalph /'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'/
c
c...Check for CAM or CAD
c
      if (cdat1 .eq. lcam .and. cdat2 .ne. lcam) go to 500
      if (cdat2 .eq. lcam .and. cdat1 .ne. lcam) go to 700
      if (cdat1 .eq. lcad .and. cdat2 .ne. lcad) go to 500
      if (cdat2 .eq. lcad .and. cdat1 .ne. lcad) go to 700
c
c...Compare the two strings
c
      nc     = knc1
      if (knc2 .lt. nc) nc = knc2
      do 100 i=1,nc,1
          ix1    = index(lalph,cdat1(i:i))
          ix2    = index(lalph,cdat2(i:i))
          if (ix1-ix2) 500,100,700
  100 continue
      if (knc1-knc2) 500,600,700
c
c...1st string is less
c
  500 kfl    = -1
      go to 8000
c
c...Strings are equal
c
  600 kfl    = 0
      go to 8000
c
c...1st string is greater
c
  700 kfl    = 1
      go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdctb (cdat,cldat)
c
c   FUNCTION:  This routine converts a character string into a null ter-
c              minated text string.
c
c   INPUT:  cdat    C*n  D1    -  Character string.
c
c   OUTPUT: cldat   B*1  Dn    -  Null terminated text string.
c
c***********************************************************************
c
      subroutine pwdctb (cdat,cldat)
c
      byte cldat(1024)
      character*(*) cdat
c
      integer*4 i,nc,pwdlen
c
      byte snum(1024)
      character*1024 sbuf
c
      equivalence (sbuf,snum)
c
c...Convert character data to text string
c
      sbuf   = cdat
      nc     = pwdlen(sbuf)
      if (nc .ge. 1024) nc = 1023
      snum(nc+1) = 0
      do 100 i=1,nc+1,1
          cldat(i) = snum(i)
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdcti (cdat,knum,kerr)
c
c   FUNCTION:  This routine converts a character string to an integer
c              number.
c
c   INPUT:  cdat    C*n  D1  -  Character string to be converted to
c                               integer number.
c
c   OUTPUT: knum    I*4  D1  -  Integer value from 'cdat'.
c
c           kerr    I*4  D1  -  Returns 1 on error.
c
c***********************************************************************
c
      subroutine pwdcti (cdat,knum,kerr)
c
      integer*4 knum,kerr
c
      character*(*) cdat
c
      character*20 lnum
c
      integer*4 nc,pwdlen
c
c...Convert character data to integer data
c
      kerr   = 0
      lnum   = ' '
      nc     = pwdlen (cdat)
      lnum(21-nc:20) = cdat(1:nc)
      read (lnum,10,err=9000) knum
   10 format (i20)
      return
c
c...Invalid numeric data
c
 9000 kerr   = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwddnm (knum)
c
c   FUNCTION:  This routine returns the current date in a numeric format.
c
c   INPUT:  none.
c
c   OUTPUT: knum    I*4  D1    -  Numeric date.
c
c***********************************************************************
c
      subroutine pwddnm (knum)
c
      integer*4 knum
c
      integer*4 nc1,nc2,ierr
c
      character*20 lbuf1,lbuf2
c
c...Get the current date
c
      call pwddat (lbuf1,nc1)
c
c...Convert the date to a number
c
      call pwddcv (lbuf1,nc1,lbuf2,nc2,knum,ierr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwddat (cdat,knc)
c
c   FUNCTION:  This routine returns the current date in the format
c              '31-DEC-1999'.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1    -  Character string containing current
c                                 date.
c
c           knc     I*4  D1    -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine pwddat (cdat,knc)
c
      integer*4 knc
c
      character*(*) cdat
c
      integer*4 imon,iday,iyear
      integer*4 values(8)
C SUN-CIM-DOS-START
C     integer*4 idat(3)
C SUN-CIM-DOS-END
c
      character*2 ld
      character*3 mon(12)
      character*4 ly
c
      data mon /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     1          'OCT','NOV','DEC'/
c
c...Get binary date
c
C HPX-START
C      call idate (iday,imon,iyear)
C HPX-END
C VAX-SGI-IBM-OSF-START
C     call idate (imon,iday,iyear)
C VAX-SGI-IBM-OSF-END
c
c...Before, WNT use same format as SGI, use idate, but
c...Two digit year return value may cause problem after 2000,
c...so change it
C WNT-START
      CALL DATE_AND_TIME(VALUES=values);
      iday = values(3)
      imon = values(2)
      iyear = values(1)
C WNT-END
C SUN-CIM-START
C     call idate (idat)
C     imon   = idat(2)
C     iday   = idat(1)
C     iyear  = idat(3)
C SUN-CIM-END
C DOS-START
C     call idatep (idat)
C     imon   = idat(2)
C     iday   = idat(1)
C     iyear  = idat(3)
C DOS-END
c
c...Convert to text date
c
      if (iyear .lt. 1900) iyear  = iyear  + 1900
      if (iyear .lt. 1970) iyear  = iyear  + 100
      write (ld,10) iday
   10 format (i2.2)
      write (ly,20) iyear
   20 format (i4.4)
      cdat   = ld  // '-' // mon(imon) // '-' // ly
      knc    = 11
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwddcv (cdat,knc,cout,kno,knum,kerr)
c
c   FUNCTION:  This routine converts a date string, in the format
c              '31-DEC-1999' into a numeric value that approximates the
c              number of days since 31-DEC-1989.
c
c   INPUT:  cdat    C*n  D1    -  Date string.
c
c           knc     I*4  D1    -  Number of characters in 'cdat'.
c
c   OUTPUT: cout    C*n  D1    -  Character representation of 'knum'.
c
c           kno     I*4  D1    -  Number of characters in 'cout'.
c
c           knum    I*4  D1    -  Number of days from 31-DEC-1989 until
c                                 the date represented by 'cdat'.
c
c           kerr    I*4  D1    -  Returns 1 when 'cdat' contains an in-
c                                 valid date.
c***********************************************************************
c
      subroutine pwddcv (cdat,knc,cout,kno,knum,kerr)
c
      integer*4 knc,kno,kerr,knum
c
      character*(*) cdat,cout
c
      integer*4 i,inc,ie,inum
c
      character*3 mon(12)
      character*12 lnum
c
      data mon /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     1          'OCT','NOV','DEC'/
c
   10 format (i12)
   20 format (i12)
c
c...Get date
c
      inc    = 1
      do 100 i=inc,knc,1
          if (cdat(i:i) .eq. '-') go to 150
  100 continue
      go to 9000
  150 if (i .eq. inc) go to 9000
      lnum   = ' '
      lnum(13-i+inc:12) = cdat(inc:i-1)
      read (lnum,10,err=9000) knum
      inc    = i      + 1
c
c...Get month
c
      do 200 i=inc,knc,1
          if (cdat(i:i) .eq. '-') go to 250
  200 continue
      go to 9000
  250 if (i-inc .ne. 3) go to 9000
      ie     = i      - 1
      do 260 i=1,12,1
          if (cdat(inc:ie) .eq. mon(i)) go to 270
  260 continue
      go to 9000
  270 knum   = knum   + i      * 30
      inc    = ie     + 2
c
c...Get year
c
      if (knc-inc+1 .ne. 4) go to 9000
      lnum   = ' '
      lnum(9:12) = cdat(inc:knc)
      read (lnum,10,err=9000) inum
      if (inum .lt. 1990 .or. inum .gt. 2200) go to 9000
      knum   = knum   + (inum-1990) * 365
c
c...Convert to numeric format
c
      write (cout,20,err=9000) knum
      kno    = 12
c
c...End of routine
c
 8000 return
c
c...Error converting date
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwddvc (knum,cout,kno)
c
c   FUNCTION:  This routine converts a numeric data to a date string,
c              in the format '31-DEC-1999'.
c
c   INPUT:  knum    I*4  D1    -  Number of days from 31-DEC-1989 until
c                                 the date represented by 'cdat'.
c
c   OUTPUT: cdat    B*1  Dn    -  Character representation of date.
c
c           knc     I*4  D1    -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine pwddvc (knum,cdat,knc)
c
      integer*4 knc,knum
c
      byte cdat(20)
c
      integer*4 iday,imon,iyear,inc,jmod
c
      character*2 ld
      character*3 mon(12)
      character*4 ly
      character*12 lnum
c
      data mon /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     1          'OCT','NOV','DEC'/
c
c...Calculate Year, Month, Day
c
      inc   = jmod((knum-30),365)
      iyear = (knum-inc) / 365 + 1990
      iday  = jmod(inc,30)
      if (iday .eq. 0) iday = 30
      imon  = (inc-iday) / 30 + 1
      if (imon .gt. 12) then
          imon = 12
          iday = 31
      endif
c
c...Format date
c
      write (ld,10) iday
   10 format (i2.2)
      write (ly,20) iyear
   20 format (i4.4)
      lnum   = ld  // '-' // mon(imon) // '-' // ly
      call pwdctb (lnum,cdat)
      knc    = 11
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwddea (clprog)
c
c   FUNCTION:  This routine deallocates a user for either NCLCAM or
c              NCLCADD.
c
c   INPUT:  clprog  B*1  D20   -  Null terminated text string that con-
c                                 tains the name of the calling program.
c
c   OUTPUT: none.
c
c***********************************************************************
c
       subroutine pwddea (clprog)
C VAX-START
c
C     include 'passwd.inc'
c
C VAX-END
       byte clprog(80)
C SGI-SUN-IBM-OSF-HPX-WNT-START
       call pwcdea (clprog)
C SGI-SUN-IBM-OSF-HPX-WNT-END
C VAX-START
c
C     integer*4 nc,inc
c
C     character*80 cprog
c
c...Determine type of user
c...CAM or CADD
c
C     call pwdbtc (clprog,cprog,nc)
C     if (cprog .eq. 'NCLCAM') then
C         inc    = 1
C     else if (cprog .eq. 'NCLCADD') then
C         inc    = 2
C     else if (cprog .eq. 'NCLIPV') then
C         inc    = 3
C     else
C         go to 8000
C     endif
c
c...Release this user
c
C     if (ntrm(inc) .ne. 0) call pwdext (%val(rtadr(1,inc)),ntrm(inc))
C     ntrm(inc) = 0
c
c...End of routine
c
C VAX-END
 8000 return
       end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdext (clwin,kpt)
c
c   FUNCTION:  This routine clears the requested Licensing manager's
c              slot.
c
c   INPUT:  clwin   B*1  D512  -  Licensing manager's buffer.
c
c           kpt     I*4  D1    -  Slot number to clear.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C VAX-START
C     subroutine pwdext (clwin,kpt)
c
C     byte clwin(512)
C     integer*4 kpt
c
C     if (kpt .gt. 0) clwin(kpt) = 0
C     return
C     end
C VAX-END
c
c***********************************************************************
c
c   SUBROUTINE:  pwdfil (cdat,knc)
c
c   FUNCTION:  This routine fills a character string so that the number
c              of characters are a multiple of 4.  The fill character
c              used is the last character in the input string.
c
c   INPUT:  cdat    C*n  D1    -  Character string to fill.
c
c           knc     I*4  D1    -  Number of characters in 'cdat'.
c
c   OUTPUT: cdat    C*n  D1    -  Filled character string.
c
c           knc     I*4  D1    -  See INPUT.
c
c***********************************************************************
c
      subroutine pwdfil (cdat,knc)
c
      integer*4 knc
c
      character*(*) cdat
c
      integer*4 i,inum,inc
c
c...Make sure there is at
c...least 1 character in buffer
c
      if (knc .eq. 0) then
          knc    = 1
          cdat   = '~'
      endif
c
c...Fill buffer to multiple of 4 bytes
c
      inc    = knc    / 4      * 4
      inum   = knc    - inc
      if (inum .eq. 0) go to 8000
      inum   = 4      - inum
      do 100 i=1,inum,1
          cdat(knc+i:knc+i) = cdat(knc:knc)
  100 continue
      knc    = knc    + inum
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   E-FUNCTION:  pwdlen (cstr)
c
c   FUNCTION:  This routine returns the number of characters in 'cstr'.
c              The last non-space character is considered the last char-
c              acter in the string.
c
c   INPUT:  cstr    C*n  D1  -  Character string to determine the length
c                               of.
c
c   OUTPUT: pwdlen  I*4  D1  -  Length of 'cstr'.
c
c***********************************************************************
c
      integer*4 function pwdlen (cstr)
c
      character*(*) cstr
c
      integer*4 nc
c
C VAX-START
C     integer*4 istat,str$trim
C
C     character*256 str
C VAX-END
c
C SUN-SGI-CIM-OSF-START
C     integer*4 lnblnk,nc1,len
C SUN-SGI-CIM-OSF-END
C IBM-DOS-HPX-START
C     integer*4 i,nc1
C IBM-DOS-HPX-END
c
c...Find the last non-blank
c...character in string
c
      if (cstr .eq. ' ') then
          pwdlen = 0
      else
C VAX-START
C         istat  = str$trim (str,cstr,nc)
C VAX-END
C SUN-SGI-CIM-OSF-START
C         nc     = lnblnk (cstr)
C         nc1    = len (cstr)
C         if (nc1 .lt. nc) nc = nc1
C SUN-SGI-CIM-OSF-END
C DOS-START
C         nc1    = len (cstr)
C         nc     = nblank (cstr)
C         if (nc1 .lt. nc) nc = nc1
C DOS-END
C WNT-IBM-HPX-START
          nc1    = len (cstr)
          do 100 i=nc1,1,-1
              if (cstr(i:i) .ne. ' ' .and. cstr(i:i) .ne. ' ' .and.
     1            cstr(i:i) .ne. '\0') go to 200
  100     continue
          i      = 0
  200     nc     = i
C WNT-IBM-HPX-END
          pwdlen = nc
      endif
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdlic (cpgm,copt,cdat,kopt,cmsg,kerr)
c
c   FUNCTION:  This routine is the Fortran interface to the licensing
c              routines.
c
c   INPUT:  cpgm    C*n  D1    -  Text string containing the name of the
c                                 calling program.
c
c           copt    C*n  D1    -  Text string containing a list of
c                                 acceptable options, separated by
c                                 commas.
c
c           cdat    C*n  D1    -  Text string containing the version
c                                 date of the calling program, in the
c                                 format '31-MAY-1999'.
c
c   OUTPUT: kopt    I*4  D8    -  Returns 1 in the array position for
c                                 each option that is licensed.
c
c           cmsg    C*n  D1    -  Text string of message when an error
c                                 occurred.  Returns left over options
c                                 when there is no error.
c
c           kerr    I*4  D1    -  Returns 1 when this program is not
c                                 authorized to run on this computer.
c                                 2 = Could not access license data
c                                 base.  3 = Could not obtain system id.
c                                 Returns 0 when program is licensed.
c
c***********************************************************************
c
      subroutine pwdlic (cpgm,copt,cdat,kopt,cmsg,kerr)
c
      integer*4 kerr,kopt(10)
c
      character*(*) cpgm,copt,cdat,cmsg
c
      integer*4 nc,pwdlen
c
      character*80 spgm,sopt,sdat
c
      byte lpgm(80),lopt(80),ldat(80),lmsg(80)
c
      equivalence (spgm,lpgm), (sopt,lopt), (sdat,ldat)
c
c...Store character data in logical arrays
c
      open (unit=out_unit,file="results.txt",action="write",
     1      status="replace")
          write (out_unit,*) "The file to debug from pwdlic 1"
          close (out_unit)
      spgm   = cpgm
      nc     = pwdlen(spgm)
      lpgm(nc+1) = 0
c
      sopt   = copt
      nc     = pwdlen(sopt)
      lopt(nc+1) = 0
c
      sdat   = cdat
      nc     = pwdlen(sdat)
      ldat(nc+1) = 0
c
c...Verify runtime license
c
       open (unit=out_unit,file="results.txt",action="write",
     1      status="replace")
          write (out_unit,*) "The file to debug from pwdlic 2"
          close (out_unit)
      call pwdaut (lpgm,lopt,ldat,kopt,lmsg,kerr)
      call pwdbtc (lmsg,cmsg,nc)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdopt (cdat,knc,copt,knco,knop)
c
c   FUNCTION:  This routine breaks apart an option list into individual
c              options.
c
c   INPUT:  cdat    C*n  D1    -  Input option list, with options se-
c                                 parated with commas.
c
c           knc     I*4  D9    -  Number of characters in 'cdat'.
c
c   OUTPUT: copt    C*n  D8    -  Individual options.
c
c           knco    I*4  D8    -  Number of characters in 'copt'.
c
c           knop    I*4  D1    -  Number of options in 'copt'.
c
c***********************************************************************
c
      subroutine pwdopt (cdat,knc,copt,knco,knop)
c
      integer*4 knc,knco(8),knop
c
      character*(*) cdat,copt(8)
c
      integer*4 inc,ipt,index
c
c...Initialize routine
c
      knop   = 0
      inc    = 1
c
c...Break out option
c
  100 if (inc .gt. knc) go to 8000
      ipt    = index(cdat(inc:knc),',')
      if (ipt .eq. 0) then
          ipt    = knc
      else
          ipt    = inc    + ipt    - 2
      endif
c
      knop   = knop   + 1
      copt(knop) = cdat(inc:ipt)
      knco(knop) = ipt    - inc    + 1
      if (knop .eq. 8) go to 8000
      inc    = ipt    + 2
      go to 100
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdpas (cbuf,knc,cpwd,kerr)
c
c   FUNCTION:  This routine calculates a valid password for the input
c              fields.
c
c   INPUT:  cbuf    C*n  D9    -  1 = Comany name, 2 = Hardware platform,
c                                 3 = Software product, 4 = Options, 5 =
c                                 Number of users, 6 = Termination date,
c                                 7 = Program version date, 8 = System
c                                 ID, 9 = Password.
c
c           knc     I*4  D9    -  Number of characters in 'cbuf'.
c
c   OUTPUT: cpwd    C*n  D1    -  Text string containing password.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine pwdpas (cbuf,knc,cpwd,kerr)
c
      integer*4 knc(9),kerr
c
      character*(*) cbuf(9),cpwd
c
      integer*4 nca(9),i,j,ipass(4)
c
      character*4 lpass(4)
      character*80 ldat(9)
c
c...Check validity of fields
c
      call pwdchk (cbuf,knc,ldat,nca,1,8,1,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Get field checksum
c
      ipass(1) = 13
      ipass(2) = 6
      ipass(3) = 24
      ipass(4) = 17
      do 300 i=1,8,1
          do 200 j=1,nca(i),4
              ipass(1) = ipass(1) + ichar(ldat(i)(j:j))
              ipass(2) = ipass(2) + ichar(ldat(i)(j+1:j+1))
              ipass(3) = ipass(3) + ichar(ldat(i)(j+2:j+2))
              ipass(4) = ipass(4) + ichar(ldat(i)(j+3:j+3))
  200     continue
  300 continue
c
c
c...Calculate password values
c
C WNT-VAX-SUN-SGI-OSF-HPX-START
      ipass(1) = (ipass(1).xor.10) * 23
      ipass(2) = (ipass(2).xor.36) * 18
      ipass(3) = (ipass(3).xor.17) * 21
      ipass(4) = (ipass(4).xor.24) * 16
C WNT-VAX-SUN-SGI-OSF-HPX-END
C IBM-START
C     ipass(1) = xor(ipass(1),10) * 23
C     ipass(2) = xor(ipass(2),36) * 18
C     ipass(3) = xor(ipass(3),17) * 21
C     ipass(4) = xor(ipass(4),24) * 16
C IBM-END
C DOS-START
C     ipass(1) = jieor(ipass(1),10) * 23
C     ipass(2) = jieor(ipass(2),36) * 18
C     ipass(3) = jieor(ipass(3),17) * 21
C     ipass(4) = jieor(ipass(4),24) * 16
C DOS-END
c
c...Make sure numbers are not too large
c
      if (ipass(1) .gt. 9999)
     1        ipass(1) = ipass(1) - (ipass(1)/10000*10000)
      if (ipass(2) .gt. 9999)
     1        ipass(2) = ipass(2) - (ipass(2)/10000*10000)
      if (ipass(3) .gt. 9999)
     1        ipass(3) = ipass(3) - (ipass(3)/10000*10000)
      if (ipass(4) .gt. 9999)
     1        ipass(4) = ipass(4) - (ipass(4)/10000*10000)
c
c...Assemble password
c
      do 600 i=1,4,1
          write (lpass(i),610) ipass(i)
  600 continue
  610 format (i4.4)
c
      cpwd = lpass(1)(1:1) // lpass(2)(2:2) // lpass(3)(3:3) //
     1       lpass(4)(4:4) // '-' //
     2       lpass(2)(1:1) // lpass(3)(2:2) // lpass(4)(3:3) //
     3       lpass(1)(4:4) // '-' //
     4       lpass(3)(1:1) // lpass(4)(2:2) // lpass(1)(3:3) //
     5       lpass(2)(4:4) // '-' //
     6       lpass(4)(1:1) // lpass(1)(2:2) // lpass(2)(3:3) //
     7       lpass(3)(4:4)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdrd (krec,kdat,kerr)
c
c   FUNCTION:  This routine reads a record from the NCCS Licensing Data
c              base file.
c
c   INPUT:  krec    I*4  D1    -  Record number to read.
c
c   OUTPUT: kdat    I*4  D51   -  Data read from file.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine pwdrd (krec,kdat,kerr)
c
      include 'menu.inc'
c
      integer*4 krec,kdat(51),kerr, sta
      character*80 ldat
c
      integer*4 nc
c
c
c...Read from data base file
c
      kerr   = 0
      read (1,rec=krec,iostat=sta, err=9000) kdat
c
c...End of routine
c
 8000 return
c
c...Error reading from file
c
 9000 kerr   = -1
c     call getfnm (1, ldat, nc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdsid (cprog,cid,knc,kflag,kerr)
c
c   FUNCTION:  This routine returns the Hardware System ID.
c
c   INPUT:  cprog   B*1  Dn    -  Name of program requesting system ID.
c
c           kflag   I*4  D1    -  0 = Get ID from Network Card,
c                                 1 = Local Dongle, 2 = from Network Dongle
c
c   OUTPUT: cid     C*n  D1    -  Character string containing the system
c                                 ID of the host computer.
c
c           knc     I*4  D1    -  Number of characters in 'cid'.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
c
c...added kflag for DONGLE
c...Yurong
c
      subroutine pwdsid (cprog,cid,knc,kflag, kerr)
c
      integer*4 knc,kflag,kerr
c
      byte cprog(*)
c
      character*(*) cid
c
C VAX-START
C     include '($secdef)'
C     include '($iodef)'
C     include '($syidef)'
c
C     parameter nma$c_pcli_hwa = 1160
c
C     character*80 cout,ccli
c
C     logical*1 lcli(80),lout(80),lsid(4),lbuf(200),lnum
c
C     integer*2 ist,ibuf(100),ipas(3),inum
c
C     integer*4 istat,ichan,sys$assign,sys$qiow,sid,jtmlst(2),nc,pwdstr,
C    1          iost(2),pwdlen
c
C     equivalence (lout,cout), (lcli,ccli), (sid,lsid), (lbuf,ibuf(2))
C     equivalence (lnum,inum)
C VAX-END
C WNT-SUN-SGI-CIM-DOS-IBM-OSF-HPX-START
      integer*4 inum

      kerr   = 0
C WNT-SUN-SGI-CIM-DOS-IBM-OSF-HPX-END
C VAX-START
c
c...See if this computer contains a
c...DEQNA
c
C     ist   = 98
C     istat = sys$assign ('_xqa0:',ichan,,)
c
c...DELUA
c
C     if (.not. istat) then
C         ist   = 116
C         istat = sys$assign ('_xea0:',ichan,,)
C     endif
c
c...Workstation ethernet (DESVA)
c
C     if (.not. istat) then
C         ist   = 104
C         istat = sys$assign ('_esa0:',ichan,,)
C     endif
c
c....DEBNA
c
C     if (.not. istat) then
C         ist    = 98
C         istat = sys$assign ('_eta0:',ichan,,)
C     endif
c
c....PMAD
c
C     if (.not. istat) then
C         ist    = 98
C         istat = sys$assign ('_eca0:',ichan,,)
C     endif
c
c....PCI Based
c
C     if (.not. istat) then
C         ist    = 98
C         istat = sys$assign ('_ewa0:',ichan,,)
C     endif
c
c....DEMNA
c
C     if (.not. istat) then
C         ist    = 98
C         istat = sys$assign ('_exa0:',ichan,,)
C     endif
c
c....SGEC,TGEC
c
C     if (.not. istat) then
C         ist    = 98
C         istat = sys$assign ('_eza0:',ichan,,)
C     endif
c
c...No ethernet type board
c...get cpu type
c
C     if (.not. istat) then
C         ist   = 0
C         istat = lib$getsyi (syi$_sid,sid,,,,)
C         if (.not. istat) go to 9000
C         if (sid .lt. 0.) sid = -sid
C  40     if (sid .gt. 999999999) then
C             sid    = sid    - 100000000
C             go to 40
C         endif
C         write (cid,60) sid
C  60     format (i9.9)
C         knc    = pwdlen(cid)
C     endif
c
c...Get the characteristics
c...of the ethernet board
c
C     if (ist .ne. 0) then
C         ibuf(1) = nma$c_pcli_hwa
C         jtmlst(1) = 200
C         jtmlst(2) = %loc(ibuf)
C         istat = sys$qiow (,%val(ichan),
C    1                      %val(io$_sensemode.or.io$m_ctrl),
C    2                      iost,,,,jtmlst,,,,)
C         if (.not. istat) go to 9000
c
c...Find the hardware address
c...of the ethernet board
c
C         do 100 i=1,iost(2)-5,1
C             if (lbuf(i) .eq. '08'x .and. lbuf(i+1) .eq. '00'x .and.
C    1            lbuf(i+2) .eq. '2b'x) go to 200
C 100     continue
C         if (ist .gt. iost(2)) go to 1000
C         go to 300
C 200     ist    = i + 3
C 300     continue
c
c...Return the system id
c
C1000     lnum = lbuf(ist)
C         ipas(1) = inum
C         lnum = lbuf(ist+1)
C         ipas(2) = inum
C         lnum = lbuf(ist+2)
C         ipas(3) = inum
C         write (cid,1001) ipas
C1001     format (3i3.3)
C         knc    = pwdlen(cid)
C     endif
C VAX-END
C SUN-SGI-CIM-DOS-IBM-OSF-HPX-START
C     call hostid (inum)
C SUN-SGI-CIM-DOS-IBM-OSF-HPX-END
C WNT-START
      call hostid (cprog, inum, kflag)
C WNT-END
C WNT-SUN-SGI-CIM-DOS-IBM-OSF-HPX-START
      write (cid,1002) inum
 1002 format (i12.9)
      call pwdspc (cid,cid,knc)
C WNT-SUN-SGI-CIM-DOS-IBM-OSF-HPX-END
c
c...End of routine
c
 8000 return
c
c...Could not get the system id
c
 9000 kerr  = 1
      cid = ' '
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdspc (cstr1,cstr2,knc)
c
c   FUNCTION:  This routine removes all of the spaces in a character
c              string, except for characters within quotes.
c
c   INPUT:  cstr1   C*n  D1  -  Original character string.
c
c   OUTPUT: cstr2   C*n  D1  -  Character string minus spaces.
c
c           knc     I*4  D1  -  Number of characters in 'cstr2'.
c
c***********************************************************************
c
      subroutine pwdspc (cstr1,cstr2,knc)
c
      integer*4 knc
c
      character*(*) cstr1,cstr2
c
      integer*4 i,pwdlen
c
      character*1 lqot,lc
      character*512 str
c
c...Remove spaces from string
c
      str    = cstr1
      cstr2  = ' '
      lqot   = ' '
      knc    = 0
      do 100 i=1,pwdlen(str),1
          lc     = str(i:i)
          if (lqot .eq. ' ') then
              if (lc .eq. ' ') go to 100
              if (lc .eq. '''' .or. lc .eq. '"') lqot = lc
          else
              if (lc .eq. lqot) lqot = ' '
          endif
          knc    = knc    + 1
          cstr2(knc:knc) = lc
  100 continue
      cstr2  = cstr2(1:knc)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdtrm (clwin,clpid,clprog,kpt,kerr)
c
c   FUNCTION:  This routine allocates a slot with the licensing manager
c              for the calling program.  It assumes that the calling
c              program has already verified that it is authorized to run
c              on this computer.
c
c   INPUT:  clwin   B*1  D512  -  Licensing manager's buffer.
c
c           clpid   B*1  D8    -  Text process number of the calling
c                                 program.
c
c           clprog  B*1  D80   -  Process name of the calling program.
c
c   OUTPUT: kpt     I*4  D1    -  Returns the slot number that was al-
c                                 located for the calling program.
c
c           kerr    I*4  D1    -  Returns 1 when a slot could not be al-
c                                 located.
c
c***********************************************************************
c
C VAX-START
C     subroutine pwdtrm (clwin,clpid,clprog,kpt,kerr)
c
C     include '($jpidef)'
c
C     integer*4 kerr,kpt
c
C     byte clwin(512),clpid(8),clprog(80)
c
C     integer*4 i,ist
c
C     kerr   = 0
C     kpt    = 1
C     ist    = 0
c
c...Verify this is the correct version
c
C     if (clwin(506) .ne. 8) go to 1000
c
c...Check to see if this user
c...has already been authorized
c
C 100 if (clwin(kpt) .eq. -1) go to 500
C     if (clwin(kpt) .eq. 0 .and. ist .eq. 0) ist = kpt
C     do 200 i=1,8,1
C         if (clwin(kpt+i-1) .ne. clpid(i)) go to 300
C 200 continue
C     go to 800
c
c......Check next authorized user
c
C 300 kpt   = kpt   + 16
C     if (kpt .lt. 512) go to 100
c
c...User has not been previously authorized
c...Store job process number
c
C 500 if (ist .eq. 0) go to 1000
C     kpt    = ist
C     do 600 i=1,8,1
C         clwin(kpt+i-1) = clpid(i)
C 600 continue
c
c...Store job process name
c
C 800 do 900 i=1,8,1
C         clwin(kpt+7+i) = clprog(i)
C 900 continue
C     return
c
c...Could not authorize user
c
C1000 kerr   = 1
C     kpt    = 0
C     return
C     end
C VAX-END
c
c***********************************************************************
c
c   SUBROUTINE:  pwdusr (kusr)
c
c   FUNCTION:  This routine returns the Number of Users who can run the
c              last authorized (using pwdaut) routine.
c
c   INPUT:  none.
c
c   OUTPUT: kusr    I*4  D1    -  Number of users authorized to run
c                                 routine last processed by 'pwdaut'.
c
c***********************************************************************
c
      subroutine pwdusr (kusr)
c
      include 'passwd.inc'
c
      integer*4 kusr
c
      integer*4 ierr
c
c...Return maximum number of users
c
      call pwdcti (LICDAT(5)(1:LICNC(5)),kusr,ierr)
      if (ierr .ne. 0) kusr = 0
c
c...End of routine
c
      return
      end
