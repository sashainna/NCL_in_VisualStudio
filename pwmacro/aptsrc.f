c
c***********************************************************************
c
c   FILE NAME:  aptsrc
c   CONTAINS:
c               srclod  srcbwd  srcprs  srcpwd  srcrd   wrclrc
c               ignwrd  srcppl  chknum  doplan  pararr  findin
c               txtcmd
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        aptsrc.f , 25.6
c     DATE AND TIME OF LAST  MODIFICATION
c        08/11/16 , 10:26:41
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  srclod (cfnam,ktest,cmsg,kerr)
c
c   FUNCTION:  This routine reads an APT Source file, converts it to a
c              neutral format and stores it in a scratch file.
c
c   INPUT:  cfnam   C*n  D1  Input Apt Source file name.
c
c           ktest   I*4  D1  1 = Test to see if this is an NCL clfile.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine srclod (cfnam,ktest,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'post.inc'
      include 'compile.inc'
C WNT-START
      include 'postworks_nt.inc'
C WNT-END

c
      integer*4 kerr,ktest
c
      character*(*) cfnam,cmsg
c
      equivalence (VECSAV,POSMAP(1372))
      real*8 VECSAV(3)
c
      integer*2 inbuf(964),iobuf(960),mxc
      integer*4 i,ipt,nwds,inc,index,inum,nc,ifl,igo,icont,iseq,ierr,
     1          irecl,ist,ien,nci,ityp,ibuf(50),icpt,icr,ifini,irec,
     2          jbuf(480),is1,is4,j,ilt,np,mfl(4),iosec,mix,nprs,ncx(5),
     3          nwrd,ncip,chknum,imfl,nugci,ihelx,ix,ipts,ict,isv,ntim
C WNT-START
      integer*4 flag,strlen1
C WNT-END
c
      real*8 robuf(240),rbuf(50),delt(3),gts(21),vec(3),ddl,riv(3),
     1       arad,ccen(3),psis(4),plon(4),cir(7),ndot,pltan(4),tolc,
     2       rhelx,znum,rver
c
      character*1 lc
      character*24 lbuf
      character*20 att(4)

	
      character*132 msg,msg1,lwrd(5)

      character*256 ldat,udat,sorc(3),tobuf
c
      equivalence (inbuf(5),robuf,iobuf,tobuf), (inbuf(1),mxc,jbuf)
c
C VAX-SUN-SGI-IBM-HPX-START
C      data is1 /0/, is4 /3/
C VAX-SUN-SGI-IBM-HPX-END
C WNT-DOS-DEC-START
      data is1 /3/, is4 /0/
C WNT-DOS-DEC-END
c
c...Initialize statement parsing routines
c
      MAXPRN = 0
      MAXTOK = 100
      CIRTYP = 0
      ICYACT = 0
c
c...LETTER  PPRINT  PARTNO  INSERT  REMARK
c
      call getvwd (1043,LCOMTX(1),ncx(1),1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1044,LCOMTX(2),ncx(2),1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1045,LCOMTX(3),ncx(3),1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1046,LCOMTX(4),ncx(4),1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (4009,LCOMTX(5),ncx(5),1,PSTWRD,PSTWVL,NPSTWD)
c
c...Open Apt Source file
c
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'old'
      irecl  = MCNSRC
      call opnfil (LUNSC2,cfnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call getfnm (LUNSC2,LCMPFI,NCCMPF,MAX_PATH)
c
c...Initialize Macros
c
      call amini
c
c...Initialze routine
c
      inbuf(2) = 0
      irec   = 0
      ifl    = 0
      iseq   = 0
      ifini  = 0
      icpt   = 0
      icr    = 0
      ihelx  = 0
      gts(4) = 0.
      gts(5) = 0.
      gts(6) = 1.0
      MULT   = 0
      mfl(1) = 0
      mfl(2) = 0
      mfl(3) = 0
      mfl(4) = 0
      nprs   = 0
      nwrd   = 0
      ICOMCI = 0
      ncip   = 0
      nugci  = 0
      ntim   = 0
      JMAC   = 0
      imfl   = 0
      tolc   = .001
      do 10 i=1,5
   10 PTL(i) = 0.0d0
      PTL(6) = 1.0d0
      VECSAV(1) = 0.0d0
      VECSAV(2) = 0.0d0
      VECSAV(3) = 1.0d0
c
c...Output MULTAX record for Unigraphics clfile
c
      if (ktest .eq. 0) then
          if (ICLF .eq. 4) MULT = 1
c
c...Store default MULTAX mode
c...if this is an INCLUDed clfile
c
          if (INCLNO .gt. 0 .or. ICLF .eq. 4) then
              call wrmult (MULT,0,icr,icpt,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
      endif
c
c...Read source file record
c
  100 kerr   = 0
      nprs   = nprs   + 1
      if (ktest .eq. 1 .and. nprs .gt. 2) go to 8000
      call srcprs (irec,ldat,nc,ifl,cmsg,kerr)
      if (kerr .eq. 2) then
c
c......End of include file
c
          if (INCFLN .eq. 0) go to 7000
          call clsfil (LUNSC2)
          LUNSC2 = INCLUN(INCFLN)
          INCFLN = INCFLN - 1
c
c.........Included file was specified on CALL statement
c
          if (imfl .eq. 1) then
              ldat   = MCMD
              nc     = MCMDNC
              call srcprs (irec,ldat,nc,1,cmsg,kerr)
          else
              go to 100
          endif
      end if
      if (kerr .ne. 0 .and. NTOK .eq. 0) go to 9000
      if (iseq .ne. 2) inbuf(2) = irec
c
c...Initialize per record flags
c
      ifl    = 0
      ipt    = 1
      igo    = 0
      icont  = 5
      if (iseq .ne. 2) iseq   = 0
c
c...Nxx sequence number
c
      if (ICTYP(ipt) .eq. 4) then
         ist    = RCSUB(ipt)
         ien    = ICNC(ipt)
         if ((LCTXT(ist:ist) .eq. 'N' .or. LCTXT(ist:ist) .eq. 'C')
     1        .and. ist .lt. ien) then
             if (LICOPT(7) .eq. 1) go to 9600
             call ctoi (LCTXT(ist+1:ien),inum,ierr)
             if (ierr .eq. 0) then
                inbuf(2) = inum
                if (LCTXT(ist:ist) .eq. 'C') icont = 6
                ipt    = ipt    + 1
                if (ipt .ge. NTOK) go to 9000
                if (ICTYP(ipt) .ne. 2 .or. RCSUB(ipt) .ne. 2)
     1                    go to 9000
                ipt    = ipt    + 1
                iseq   = 1
             endif
         endif
      endif
c
c...READ/2,file_name statement
c
      if (NTOK .gt. ipt+2 .and. ICTYP(ipt) .eq. 1 .and.
     1    RCSUB(ipt) .eq. 4047) then
          call aptinc (ipt,ldat,nc,cmsg,kerr)
          if (kerr .eq. 27) go to 9700
          if (kerr .ne. 0) go to 9000
          go to 100
      end if
c
c...MACRO definition
c
      if (NTOK .ge. ipt+2 .and. ICTYP(ipt) .eq. 4 .and.
     1    (ICTYP(ipt+1) .eq. 2 .and. RCSUB(ipt+1) .eq. 1) .and.
     2    (ICTYP(ipt+2) .eq. 1 .and. RCSUB(ipt+2) .eq. 5006)) then
          call aptmac (ipt,cmsg,kerr)
          if (kerr .eq. 2) go to 7000
          if (kerr .ne. 0) go to 9000
          go to 100
      endif
c
c...CALL statement definition
c
      if (NTOK .ge. ipt+2 .and. ICTYP(ipt) .eq. 1 .and.
     1    RCSUB(ipt) .eq. 824) then
          call aptcal (ipt,ldat,nc,imfl,cmsg,kerr)
          if (kerr .eq. 27) go to 9700
          if (kerr .eq. 2) go to 7000
          if (kerr .ne. 0) go to 9000
          go to 100
      endif
c
c...Ignore PAINT
c...       TOOL PATH
c...       END-OF-PATH
c...       MACH_AXIS
c...       MO_INIT
c
      if (ICTYP(ipt) .eq. 1) then
          if (RCSUB(ipt) .eq. 4046.) go to 100
          if (RCSUB(ipt) .eq. 617. .and. ICTYP(ipt+1) .eq. 1 .and.
     1        RCSUB(ipt+1) .eq. 234.) go to 100
          if (RCSUB(ipt) .eq. 1 .and. ICTYP(ipt+1) .eq. 2 .and.
     1        RCSUB(ipt+1) .eq. 5) go to 100
          if (RCSUB(ipt) .eq. 4069.) go to 100
      endif
c
c...PARTNO, PPRINT, INSERT, LETTER
c...DISPLAY/text is converted to PPRINT
c...C_VARPUT is converted to LETTER
c
      if (NTOK .ge. ipt .and. ICTYP(ipt) .eq. 1 .and.
     1    ((RCSUB(ipt) .ge. 1043. .and. RCSUB(ipt) .le. 1047.) .or.
     2    RCSUB(ipt) .eq. 4009 .or. RCSUB(ipt) .eq. 1021. .or.
     3    RCSUB(ipt) .eq. 4066 .or. RCSUB(ipt) .eq. 4067)) then
c
c...Check for DISPLY/word,word,...
c...or DISPLY/text
c
          if (RCSUB(ipt) .eq. 1021) then
              ipts   = ipt    + 1
              if (ipts .le. NTOK) then
                  if (ICTYP(ipts) .ne. 2 .or. RCSUB(ipts) .ne. 7)
     1                go to 9000
                  if (ipts .lt. NTOK) then
                      ipts   = ipts   + 1
                      call srcpwd (ityp,irec,ipts,ibuf,rbuf,nwds,cmsg,
     1                             kerr)
                      if (kerr .ne. 0) go to 160
                  endif
              endif
              go to 200
          endif
c
c......Break out text
c
  160    kerr   = 0
         inum   = RCSUB(ipt)
         if (inum.eq. 617) inum = RCSUB(ipt+1)
         call touppr (ldat(1:nc),udat)
         call getvwd (inum,lbuf,nci,1,PSTWRD,PSTWVL,NPSTWD)
         inc    = index(udat,lbuf(1:nci))
         if (inc .eq. 0) go to 9000
         tobuf  = ldat(inc+nci:nc)
         nci    = nc     - (inc+nci) + 1
c
c......Text is continued onto next line
c
         if (ldat(nc:nc) .eq. '~' .or. ldat(nc:nc) .eq. '$') then
             lc     = ldat(nc:nc)
             call srcprs (irec,ldat,nc,ifl,cmsg,kerr)
             if (kerr .lt. 0) go to 9000
             ict    = 0
             if (ictyp(1) .eq. 1 .and. ntok .gt. 1) then
                 if (ictyp(2) .ne. 2 .or. rcsub(2) .ne. 7) ict = 1
             else if (ictyp(1) .ne. 1) then
                 ict    = 1
             endif
             ist    = RCSUB(1)
             if (ictyp(1) .eq. 4 .and. (LCTXT(1:1) .eq. 'N' .or.
     1           LCTXT(1:1) .eq. 'C') .and. ntok .gt. 1) ict = 0
             if (nci+nc .le. 256 .and. kerr .ne. 2 .and.
     1           (ict .eq. 1 .or. lc .eq. '~') .and.
     2           (ictyp(1) .ne. 1 .or. ist .ne. 1044)) then
                tobuf(nci:) = ldat(1:nc)
                nci    = nci    + nc
             else
                ifl    = 1
             endif
             if (nci .lt. 66) nci = 66
         endif
c
c...Check for PPRINT MACHIN
c
         if (RCSUB(ipt) .eq. 1044 .and. nci .gt. 6) then
             call srcprs (irec,tobuf,nci,1,cmsg,kerr)
             if (ICTYP(1) .eq. 1 .and. RCSUB(1) .eq. 1015) then
                 ipt    = 1
                 go to 200
             endif
         endif
c
c......REMARK Clfile Header Record
c
         if (inum .eq. 4009) then
             if (nprs .eq. 1 .or. (nprs .eq. 2 .and.
     1           nwrd .ne. 0 .and. nwrd .lt. 5)) then
                 call srcbwd (tobuf,66,lwrd(nwrd+1),inc,5,0,1)
                 nwrd   = nwrd   + inc
                 if (nwrd .ge. 1) CLNAME = lwrd(1)
                 if (nwrd .ge. 2) CLDATE = lwrd(2)
                 if (nwrd .ge. 3) CLTIME = lwrd(3)
                 if (nwrd .ge. 4) CAMNAM = lwrd(4)
                 if (nwrd .ge. 5) then
                     CAMREV = lwrd(5)
                     call ctor (CAMREV,rver,ierr)
                     if (ierr .eq. 0) NCLVER = rver
                 endif
             endif
             go to 100
c
c......Store text record
c
         else
             inbuf(3) = 2000
             inbuf(4) = inum
             if (inbuf(4) .eq. 1021) inbuf(4) = 1044
             if (inbuf(4) .eq. 1047) inbuf(4) = 1044
             if (inbuf(4) .eq. 4066) inbuf(4) = 1044
             if (inbuf(4) .eq. 4067) inbuf(4) = 1044
             mxc    = (nci+7) / 8
             kerr   = 0
             go to 2000
         endif
      endif
c
c...Next token should be a Post word
c
  200 if (kerr .ne. 0 .or. ICTYP(ipt) .ne. 1) go to 9000
      ityp   = RCSUB(ipt)
      isv    = ipt
      ipt    = ipt    + 1
      nwds   = 0
      if (ityp .lt. 4028 .or. ityp .gt. 4030) then
         if (ipt .le. NTOK) then
            if (ICTYP(ipt) .ne. 2 .or. RCSUB(ipt) .ne. 7) go to 9000
            if (ipt .lt. NTOK) then
                ipt    = ipt    + 1
                call srcpwd (ityp,irec,ipt,ibuf,rbuf,nwds,cmsg,kerr)
                if (kerr .ne. 0) go to 9000
            endif
         endif
      endif
c
c...ARCSLP is in effect
c...without corresponding ARCDAT command
c...Output held back ARCSLP command
c
      if (ihelx .eq. 1 .and. ityp .ne. 4036) then
          ihelx  = 0
          inbuf(3) = 2000
          inbuf(4) = 1029
          robuf(1) = rhelx
          mxc    = 1
          if (ktest .eq. 0) call wrclrc (jbuf,mxc,icr,icpt,cmsg,ierr)
          if (ierr .ne. 0) go to 9000
      endif
c
c...ISN/n sequence number
c
      if (ityp .eq. 4023) then
         if (nwds .ne. 1) go to 9000
         if (ibuf(1) .ne. 0) go to 9000
         inbuf(2) = rbuf(1)
         iseq   = 2
         go to 100
c
c...CUTTER
c
      else if (ityp .eq. 4025) then
         mxc    = nwds
         if (chknum(ibuf,nwds) .ne. 0) go to 9000
         call copyn (rbuf,robuf,nwds)
         inbuf(3) = 6000
         inbuf(4) = 6
c
c...ARCDAT
c...CIRCLE
c...CYLNDR
c...MOVARC
c
      else if (ityp .eq. 4026 .or. ityp .eq. 4036 .or.
     -         ityp .eq. 4042 .or. ityp .eq. 4052) then
         if (LICOPT(7) .eq. 1 .and.
     -       (ityp .eq. 4026 .or. ityp .eq. 4042)) go to 9600
         mxc    = 7
         if (ihelx .eq. 2) ihelx = 0
         if (ihelx .eq. 1) ihelx = 2
c
c...If -clf:4 command line option is in effect use UG circle
c...definitions. If nwds is greater than 7, assume 11 parameter
c...UG circle for which positive rotation is clockwise.
c
         if (nwds .gt. 7 .or. ICLF .eq. 4 .or. ityp .eq. 4052) then
             nugci = 1
             if (nwds .gt. 7) then
                 if (ityp .eq. 4052) then
                     if (rbuf(9) .ne. 0.) nugci = rbuf(9)/dabs(rbuf(9))
                 else
                     nugci = -1
                 endif
c
c......CIRCLE/...,TIMES,n
c
                 if (nwds .eq. 13 .and. ibuf(12) .eq. 28)
     1               ntim = rbuf(13) - 1
                 nwds  = 7
             endif
         endif
         if (nwds .ne. mxc) go to 9000
         if (chknum(ibuf,nwds) .ne. 0) go to 9000
         call copyn (rbuf,cir,nwds)
         call unitvc (cir(4),cir(4))
         mfl(4) = 1
c
c......Check if CI/CY commented out
c...vp 4/28/98 replaced ICOMCI by ncip as a flag of commented
c...circle record.
c
         if (ICOMCI .ne. 0) then
             call ptlnds (gts,cir,arad)
             call nptln (gts,cir,ccen)
             ncip   = -1
             ICOMCI = 0
             if (ICNT.gt.0) then
               call do_circul(JCIRBF,NREC,icr,icpt,cmsg,ierr)
               if (ierr.gt.0) goto 9000
             endif
             go to 100
         end if
         if (ihelx .eq. 2) go to 100
         inbuf(3) = 3000
         inbuf(4) = 5
         call copyn (cir,robuf,nwds)
         if (nugci .ne. 0) go to 100
c
c...GOTO
c
      else if (ityp .eq. 4013 .or. ityp .eq. 4024 .or.
     -         ityp .eq. 4019 .or. ityp .eq. 4027) then
         if (ityp .eq. 4024) icont = 6
         if (ityp .eq. 4019) icont = 3
         mxc    = 0
         inbuf(3) = 5000
         inbuf(4) = icont
         if (ityp .eq. 4013 .or. ityp .eq. 4019) then
             if (nwds .eq. 3 .and. MULT .eq. 1) then
                 nwds = 6
                 do 555 i=4,nwds
                     ibuf(i) = 0
                     rbuf(i) = gts(i)
  555            continue
             else if (nwds .gt. 3 .and. MULT .eq. 0) then
                 nwds   = 3
c
c......Expanded clfile record
c......rbuf(7:9) = DS normal
c
             else if (nwds .eq. 9) then
                 inbuf(3) = 5200
                 rbuf(19) = rbuf(7)
                 rbuf(20) = rbuf(8)
                 rbuf(21) = rbuf(9)
                 do 556 i=7,18,1
                     rbuf(i) = 0.
  556            continue
                 nwds   = 21
             endif
c
c......UGII circle record
c
             if (nugci.ne.0) then
               j = inbuf(2)
cc               call outcir (cir,j,icr,icpt,cmsg,ierr)
cc               if (ierr .ne. 0) go to 9000
               ddl = ndot(cir(4),gts(4)) * nugci
               call genci2 (cir,gts,rbuf,ddl,MULT,j,icr,icpt,ntim,kerr)
               sorc(3) = ldat
               if (kerr .ne. 0) go to 9500
               nugci = 0
               ntim  = 0
               go to 100
             endif
c
c...GODLTA
c
         else if (ityp .eq. 4027) then
             if (LICOPT(7) .eq. 1) go to 9600
             if (nwds .ne. 6 .and.
     -          (nwds .gt. 3 .or. nwds .lt. 1)) go to 9000
             delt(3) = 0.0
             do 550 i=1,3
                 if (i .gt. nwds) then
                     ibuf(i) = 0
                 else
                     delt(i) = rbuf(i)
                 endif
  550        continue
             if (nwds .eq. 1) then
                 ddl = dsqrt (gts(4)**2+gts(5)**2+gts(6)**2)
                 if (ddl .gt. 1.d-7) then
                     vec(1) = gts(4) / ddl
                     vec(2) = gts(5) / ddl
                     vec(3) = gts(6) / ddl
                 else
                     vec(1) = 0.0
                     vec(2) = 0.0
                     vec(3) = 1.0
                 endif
                 rbuf(1) = gts(1) + vec(1) * delt(1)
                 rbuf(2) = gts(2) + vec(2) * delt(1)
                 rbuf(3) = gts(3) + vec(3) * delt(1)
             else
                 do 560 i=1,3
                     rbuf(i) = gts(i) + delt(i)
  560            continue
             endif
             nwds   = 3
             if (MULT .eq. 1) nwds = 6
         endif
c
  700    if (nwds .ne. 3 .and. nwds .ne. 6 .and. nwds .ne. 21)
     1           go to 9000
c
c...Output points if buffer is full
c
         if (mxc .eq. 240 .and. ncip .ne. 0 .and. mfl(4) .eq. 1) then
             if (ktest .eq. 0) call wrclrc(jbuf,mxc,icr,icpt,cmsg,ierr)
             if (ierr .ne. 0) go to 9000
             mxc = 0
             icont = 6
             inbuf(4) = icont
         end if
cc         if (mxc+nwds .gt. 240) go to 9000
         ifl    = 0
         if (chknum(ibuf,nwds) .ne. 0) go to 9000
         call copyn (rbuf,gts,nwds)
         if (nwds .eq. 6) call unitvc (gts(4),gts(4))
c
c......active commented out CI/CY: check if point is on circle
c......vp 4/28/98 use ncip instead of ICOMCI
c
         if (ncip .ne. 0 .and. mfl(4) .eq. 1) then
            call chkcir (gts,cir,ccen,arad,ierr)
            if (ierr .eq. 0) then
               j = inbuf(2)
               ncip = ncip + 1
               if (ncip .eq. 0) then
                  call outcir(cir,j,icr,icpt,cmsg,ierr)
                  ncip = 1
               end if
            else
               ncip   = 0
               ierr   = 0
               if (ktest .eq. 0 .and. mxc .gt. 0)
     1             call wrclrc(jbuf,mxc,icr,icpt,cmsg,ierr)
               mxc    = 0
               mfl(4) = 0
               icont  = 5
               inbuf(4) = 5
            end if
            if (ierr .ne. 0) go to 9000
         end if
c
         call copyn (gts,robuf(mxc+1),nwds)
         mxc = mxc + nwds
c
c......Check for another point
c......in this GOTO record
c
         call srcprs (irec,ldat,nc,ifl,cmsg,kerr)
         ifl    = 1
         if (kerr .eq. 2) ifl = 0
         if (kerr .eq. 0 .and. icont .ne. 3 .and.
     1        ((ICTYP(1) .eq. 1 .and. RCSUB(1) .eq. 4013. .and.
     2         (iseq .eq. 1 .or. ncip .ne. 0)) .or.
     3         (ICTYP(1) .eq. 3 .and. NTOK .ge. 5))) then
             ipt    = 1
             if (ICTYP(1) .eq. 1 .and. NTOK .ge. 3 .and.
     1            ICTYP(2) .eq. 2 .and. RCSUB(2) .eq. 7.) ipt = 3
             if (ICTYP(1) .eq. 3 .or. ipt .eq. 3) then
                call srcpwd (ityp,irec,ipt,ibuf,rbuf,nwds,cmsg,ierr)
                if (ierr .ne. 0) then
                    if (ktest .eq. 0 .and. mxc .gt. 0)
     1                  call wrclrc (jbuf,mxc,icr,icpt,cmsg,ierr)
                    go to 9000
                endif
                go to 700
             endif
             igo    = 1
         endif
         if (mfl(4) .eq. 1) mfl(4) = 2
c
c
c...MULTAX
c
      else if (ityp .eq. 1105) then
         if (nwds .gt. 1) go to 9000
         inbuf(3) = 9000
         mxc   = 0
         if (nwds .eq. 1) then
            if (ibuf(1) .eq. 71) then
                inbuf(4) = 1
            else if (ibuf(1) .eq. 72) then
                inbuf(4) = 0
            else
                go to 9000
            endif
         else
            inbuf(4) = 1
         endif
         MULT   = inbuf(4)
c
c...FINI
c
      else if (ityp .eq. 4012) then
         ifini  = 1
         inbuf(3) = 14000
         inbuf(4) = 1
         mxc    = 0
c
c...TOLER/tol
c
      else if (ityp .eq. 731) then
          if (nwds .lt. 1) go to 9000
          tolc = rbuf(1)
          go to 100
c
c...UNITS
c
      else if (ityp .eq. 841) then
         if (nwds .ne. 1) go to 9000
         inbuf(3) = 7300
         mxc   = 0
         if (ibuf(1) .eq. 301) then
             inbuf(4) = 2
         else if (ibuf(1) .eq. 303) then
             inbuf(4) = 1
         else
             go to 9000
         endif
c
c...INDIRV/i,j,k
c...INDIRP/x,y,z
c
      else if (ityp .eq. 4033 .or. ityp .eq. 4034) then
          if (nwds .ne. 3) go to 9000
          riv(1) = rbuf(1)
          riv(2) = rbuf(2)
          riv(3) = rbuf(3)
          if (ityp .eq. 4034) then
              riv(1) = riv(1) - gts(1)
              riv(2) = riv(2) - gts(2)
              riv(3) = riv(3) - gts(3)
          end if
          mfl(2) = 1
          sorc(1) = ldat
          go to 100
c
c...PSIS/(PLANE/a,b,c,d)
c
      else if (ityp .eq. 4029) then
c
c...Parse statement in parentheses
c...First, hope that it is in canonical form PLANE/a,b,c,d.
c
          sorc(2) = ldat
          call srcpar (ityp,irec,ipt,ilt,ibuf,rbuf,nwds,cmsg,kerr)
          if (kerr .eq. 2) then
c
c...Check for PSIS/(PLANE/(POINT/#,#,#),PERTO,(VECTOR/#,#,#))
c...Start the check at the "(" in (POINT/...... for srcppl().
c
             if ((ICTYP(ipt  ).ne.2) .and. (RCSUB(ipt  ).ne.7))
     -          go to 9000
             if ((ICTYP(ipt+1).ne.2) .and. (RCSUB(ipt+1).ne.17))
     -          go to 9000
             if ((ICTYP(ipt+2).ne.1) .and. (RCSUB(ipt+2).ne.4032))
     -          go to 9000
             if ((ICTYP(ipt+3).ne.2) .and. (RCSUB(ipt+3).ne.7))
     -          go to 9000
             if ((ICTYP(ipt+4).ne.2) .and. (RCSUB(ipt+4).ne.17))
     -          go to 9000
             ipt = ipt + 4
             call srcppl (ityp,irec,ipt,ilt,ibuf,rbuf,nwds,cmsg,kerr)
             if (kerr .ne. 0) go to 9000
c
c...If ordinary error, fail.
c
          else if (kerr .ne. 0) then
             go to 9000
          endif
          if (nwds .ne. 4 .or. ityp .ne. 4032) go to 9000
          if (ilt .ne. NTOK) go to 9000
          call copyn (rbuf,psis,4)
          if (dsqrt(ndot(psis,psis)) .lt. 1.d-6) go to 9100
          mfl(1) = 1
          go to 100
c
c...TLON (ignore it)
c
      else if (ityp .eq. 4028) then
         if (LICOPT(7) .eq. 1) go to 9600
         if (ipt .gt. NTOK) go to 100
         if (ICTYP(ipt) .eq. 2) then
             if (RCSUB(ipt) .ne. 2) go to 9000
             ipt = ipt + 1
             go to 200
         else if (ICTYP(ipt) .eq. 4) then
             ist    = RCSUB(ipt)
             if (LCTXT(ist:ICNC(ipt)) .eq. '$') go to 100
         end if
         go to 9000
c
c...GOFWD
c
      else if (ityp .eq. 4030) then
          if (LICOPT(7) .eq. 1) go to 9600
          sorc(3) = ldat
          ien    = 0
          call srcpar (ityp,irec,ipt,ilt,ibuf,rbuf,nwds,cmsg,kerr)
          if (kerr .ne. 0) go to 9000
c
c......Save circul definition
c
          if (ityp .eq. 4026 .or. ityp .eq. 4042) then
              j      = 0
              if (ibuf(1) .ne. 0) then
                  if (ibuf(1) .ne. 5031) go to 9000
                  j = 1
              end if
              if (chknum(ibuf(j+1),nwds-j) .ne. 0) go to 9000
              call copyn(rbuf(j+1),cir,nwds-j)
              call unitvc(gts(4),gts(4))
              if (nwds .lt. 7) then
                  cir(7) = cir(4)
                  call copyn (gts(4),cir(4),3)
              else
                  if (dsqrt(ndot(cir(4),cir(4))) .lt. 1.d-6) ien = 1
              end if
              mfl(3) = 1
c
c......Bypass ",ON,(" and get check plane definition
c
              ipt   = ilt + 1
              np    = 0
              iosec = 1
              mix   = 0
  910         do 950 j=ipt,NTOK
                  if (ICTYP(j) .eq. 4) then
                     ist    = RCSUB(j)
                     if (LCTXT(ist:ICNC(j)) .eq. '$') go to 960
                     go to 9000
                  end if
                  np = np + 1
                  if (np .eq. mix+1 .or. np .eq. mix+3) then
                     if (ICTYP(j) .ne. 2 .or. RCSUB(j) .ne. 2)
     -                     go to 9000
                  else if (np .eq. mix+2) then
                     if (ICTYP(j) .ne. 1 .or. (np .eq. 2 .and.
     -                   (RCSUB(j) .ne. 71 .and. RCSUB(j) .ne. 27) .or.
     -                   np .eq. 6 .and.  RCSUB(j) .ne. 727)) go to 9000
                     if (RCSUB(j) .eq. 27) iosec = -1
                  else if (np .eq. 4+mix) then
                     if (np .eq. 4) then
                        if (ICTYP(j) .eq. 3) then
                           inum = RCSUB(j)
                           if (inum .ne. 1 .and. inum .ne. 2) go to 9000
                           mix   = 4
                           iosec = inum
                        end if
                     else
                        if (ICTYP(j) .ne. 2 .or. RCSUB(j) .ne. 17)
     -                     go to 9000
                     end if
                  else if (np .eq. 5+mix) then
c
c...Plane definition
c
                     ipt = j
                     call doplan(ityp,irec,ipt,ilt,ibuf,rbuf,nwds,
     -                           cmsg,kerr)
                     if (kerr .ne. 0) go to 9000
                     go to 980
                  end if
  950         continue
c
c......Get continuation line
c
  960         call srcprs (irec,ldat,nc,0,cmsg,kerr)
              if (kerr .ne. 0) go to 9000
              ipt    = 1
              go to 910
c
c......Convert line to check plane
c
  980         if (ityp .eq. 4031) then
                 if (nwds .ne. 6) go to 9000
                 if (chknum(ibuf,nwds) .ne. 0) go to 9000
                 vec(1) = rbuf(1) - rbuf(4)
                 vec(2) = rbuf(2) - rbuf(5)
                 vec(3) = rbuf(3) - rbuf(6)
                 if (dsqrt(ndot(vec,vec)) .lt. 1.d-6) go to 9200
                 call unitvc (vec,vec)
                 if (iosec .eq. -1) then
                     call plnlnp (gts(4),vec,rbuf(1),pltan,kerr)
                     pltan(4) = -pltan(4)
                     call plnvpt (vec,cir(1),plon,kerr)
                     plon(4) = -plon(4)
                 else
                     call plnlnp (gts(4),vec,rbuf(1),plon,kerr)
                 endif
                 if (kerr .ne. 0) go to 9200
c
c......Save check plane
c
              else if (ityp .eq. 4032) then
                 if (nwds .ne. 4) go to 9000
                 if (chknum(ibuf,nwds) .ne. 0) go to 9000
                 if (dsqrt(ndot(rbuf,rbuf)) .lt. 1.d-6) go to 9200
                 call copyn (rbuf,plon,3)
                 plon(4) = 0.0 - rbuf(4)
              else
                 go to 9000
              end if
              if (mfl(2)+mfl(3) .ne. 2) go to 9300
              j      = inbuf(2)
              mfl(2) = 0
              mfl(3) = 0
              call gencip (cir,psis,plon,pltan,riv,gts,iosec,
     -                     MULT,j,icr,icpt,tolc,kerr)
              if (kerr .eq. 0) go to 100
              go to 9500
          else
              go to 9000
          end if
c
c...ARCSLP/helix
c
      else if (ityp .eq. 1029 .and. nwds .eq. 1 .and. ibuf(1) .eq. 0)
     1        then
          ihelx  = 1
          rhelx = rbuf(1)
          go to 100
c
c...ARCMOV - circul motion using ARCDAT
c
      else if (ityp .eq. 4037) then
          if (mfl(4) .ne. 1) go to 9000
          if (nwds .ne. 5) go to 9000
          if (ibuf(1) .ne. 59 .and. ibuf(1) .ne. 60) go to 9200
          ddl    = 119.0 - ibuf(1)*2
          if (chknum(ibuf(2),nwds-1) .ne. 0) go to 9000
          call copyn (rbuf(2),plon,3)
          ddl    = ddl * rbuf(5)
          j      = inbuf(2)
c
c......Helical interpolation is in effect (ARCSLP)
c......Output COUPLE command and
c......adjust circular record
c
          if (ihelx .eq. 2) then
              inbuf(3) = 2000
              inbuf(4) = 1049
              robuf(1) = cir(3) - plon(3)
              iobuf(2*4-is4) = 0
              iobuf(2*4-is1) = 1
              robuf(3) = rbuf(5)
              mxc    = 3
              if (ktest .eq. 0)
     1            call wrclrc (jbuf,mxc,icr,icpt,cmsg,ierr)
              if (ierr .ne. 0) go to 9000
              znum   = 0.
              ix     = 3
              do 990 i=1,3,1
                  if (dabs(cir(i+3)) .gt. znum) then
                      ix     = i
                      znum   = dabs(cir(i+3))
                  endif
  990         continue
              znum   = plon(ix)
              plon(ix) = cir(ix)
c
c.........Output held back circular record
c
              inbuf(3) = 3000
              inbuf(4) = 5
              mxc    = 7
              call copyn (cir,robuf,7)
              if (ktest .eq. 0)
     1            call wrclrc (jbuf,mxc,icr,icpt,cmsg,ierr)
          endif
c
c......Calculate and output circular record
c
          call genci1 (cir,gts,plon,ddl,MULT,j,icr,icpt,kerr)
c
c......Set final position for helical and
c......Issue COUPLE/OFF command
c
          if (ihelx .eq. 2) then
              gts(ix) = znum
              inbuf(3) = 2000
              inbuf(4) = 1049
              iobuf(1*4-is4) = 0
              iobuf(1*4-is1) = 72
              mxc    = 1
              if (ktest .eq. 0)
     1            call wrclrc (jbuf,mxc,icr,icpt,cmsg,ierr)
              ihelx  = 0
          endif
          mfl(4) = 0
          if (kerr .ne. 0) go to 9500
          go to 100
c
c...CW (CCW) circular record developed for Tucker Technology.
c
      else if (ityp .eq. 4044 .or. ityp .eq. 4045) then
          if (nwds .ne. 13) go to 9000
          if (chknum(ibuf,nwds) .ne. 0) go to 9000
          call copyn (rbuf(7),cir(1),3)
          call copyn (rbuf(1),cir(4),3)
          cir(7) = rbuf(13)
          call unitvc (cir(4),cir(4))
          call copyn (cir,robuf,7)
          call copyn (rbuf(10),plon,3)
c          mfl(4) = 1
          j      = inbuf(2)
c
c......output circle record before motion points
c
cc          call outcir (cir,j,icr,icpt,cmsg,ierr)
cc          if (ierr .ne. 0) go to 9000
          ddl    = 8089.0 - ityp*2
          call genci2 (cir,gts,plon,ddl,MULT,j,icr,icpt,ntim,kerr)
          if (kerr .ne. 0) go to 9500
          go to 100
c
c...GOCLW (GOCCLW)
c...GERBER Circular (Arden Eng.)
c
      else if (ityp .eq. 4050 .or. ityp .eq. 4051) then
          if (nwds .ne. 10) go to 9000
          if (chknum(ibuf,nwds) .ne. 0) go to 9000
          call copyn (rbuf(1),cir(1),3)
          call copyn (rbuf(4),cir(4),3)
          cir(7) = rbuf(7)
          call unitvc (cir(4),cir(4))
          call copyn (cir,robuf,7)
          call copyn (rbuf(8),plon,3)
c          mfl(4) = 1
          j      = inbuf(2)
c
c......output circle record before motion points
c
cc          call outcir (cir,j,icr,icpt,cmsg,ierr)
cc          if (ierr .ne. 0) go to 9000
          ddl    = 8101.0 - ityp*2
          call genci2 (cir,gts,plon,ddl,MULT,j,icr,icpt,ntim,kerr)
          if (kerr .ne. 0) go to 9500
          go to 100
c
c...HELICAL circular record (Catia V5)
c
      else if (ityp .eq. 4061) then
          if (nwds .ne. 14 .and. nwds .ne. 17) go to 9000
          if (chknum(ibuf,nwds) .ne. 0) go to 9000
          call copyn (rbuf(1),cir(1),3)
          call copyn (rbuf(7),cir(4),3)
          cir(7) = rbuf(11)
          call unitvc (cir(4),cir(4))
          call copyn (cir,robuf,7)
          call copyn (rbuf(nwds-2),plon,3)
          j      = inbuf(2)
          call gencdr (cir,gts,rbuf(4),ddl)
          if (nwds .eq. 17) then
              ntim = rbuf(14)
          else
              ntim   = -1
              plon(4) = rbuf(10)
          endif
          call genci2 (cir,gts,plon,ddl,MULT,j,icr,icpt,ntim,kerr)
          if (kerr .ne. 0) go to 9500
          go to 100
      else
c
c...Standard Post word
c...Ignore some words (now INTOL,OUTTOL,AUTOPS)
c
         call ignwrd (ityp,j)
         if (j .ne. 0) go to 100
c
c......MCHTOL/CIRCUL,tol
c
         if (ityp .eq. 1016 .and. ibuf(1) .eq. 75 .and. nwds .ge. 2)
     1           then
             tolc = rbuf(2)
             go to 100
         endif
c
c......Setup Post word clfile record
c
         inbuf(3) = 2000
         inbuf(4) = ityp
         mxc    = nwds
         if (nwds .ne. 0) then
             do 1015 i=1,nwds,1
                if (ibuf(i) .eq. 0) then
                    robuf(i) = rbuf(i)
                else
                    iobuf(i*4-is4) = 0
                    iobuf(i*4-is1) = ibuf(i)
                endif
 1015        continue
         endif
      endif
c
c...Write out clfile record
c
 2000 if (ktest .eq. 0) then
          if (mfl(4) .ne. 0 .and. (mfl(4) .ne. 2 .or. mxc .gt. 0)) then
              call wrclrc (jbuf,mxc,icr,icpt,cmsg,ierr)
          else
              call wrclrc2 (jbuf,mxc,icr,icpt,cmsg,ierr)
          endif
          if (ierr .ne. 0) go to 9000
      endif
c
c...vp 4/28/98 reset commented CI when output last record of points
c...the first and other record must be output at 700 label (GOTO)
c
      if (ncip .gt. 0 .or. mfl(4) .eq. 2) then
         mfl(4) = 0
         ncip   = 0
      end if
      if (ifini .eq. 0) go to 100
      if (ktest .eq. 0 .and. icpt .ne. 0) then
         icr    = icr    + 1
         call wrprm (LUNSC1,icr,JCLBUF,cmsg,kerr)
      endif
      go to 8000
c
c...End-of-file
c...Make sure FINI card is output
c
 7000 if (ifini .eq. 0) then
         ifini  = 1
         inbuf(3) = 14000
         inbuf(4) = 1
         mxc    = 0
         go to 2000
      endif
c
c...End of routine
c
 8000 call clsfil (LUNSC2)
      return
c
c...Invalid Apt Source record
c
 9000 if (kerr .lt. 0) go to 8000
      i      = 0
      kerr   = 0
      if (IOPFL(11) .eq. 0) go to 100
c
      call errtxt ('INVAPTSC',msg)
 9010 ierr   = kerr
      nugci  = 0
      if (ktest .eq. 1) then
          kerr   = 2
          go to 8000
      endif
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     call trmmsg (' ')
C      if (i .eq. 1) call trmmsg (msg1)
C      if (kerr .gt. 1 .or. kerr .eq. 0) call trmmsg (msg)
C      call trmmsg (ldat)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      flag = 1
      if (i .eq. 1) then
          nc = strlen1(msg1)
          if (nc .gt. 80) nc = 80
C
C...Add logic not to display message in PWorks interface status
C...Window
C
          if (IOPFL(11) .eq. 1) call add1dispmsg (msg1, nc, flag)
      endif
      if (kerr .gt. 1 .or. kerr .eq. 0) then
          nc = strlen1(msg)
          if (nc .gt. 80) nc = 80
C
C...Add logic not to display message in PWorks interface status
C...Window
C
          if (IOPFL(11) .eq. 1) call add1dispmsg (msg, nc, flag)
      endif
      nc = strlen1(ldat)
      if (nc .gt. 80) nc = 80
C
C...Add logic not to display message in PWorks interface status
C...Window
C
      if (IOPFL(11) .eq. 1) call add1dispmsg (ldat, nc, flag)
C WNT-END
c
c......Write error message to clfile
c
      inbuf(3) = 8000
      inbuf(4) = kerr
      tobuf  = ldat(1:nc)
      mxc    = (nc+7) / 8
      call wrclrc (jbuf,mxc,icr,icpt,cmsg,ierr)
      kerr   = 0
      go to 100
c
c...Errors in APT circular sequence
c
 9100 kerr   = 1
      go to 9500
 9200 kerr   = 2
      go to 9500
 9300 kerr   = 3
      go to 9500
 9400 kerr   = 4
      sorc(1) = sorc(3)
c
 9500 if (kerr .eq. 1) then
          call errtxt ('INVPSIS',msg1)
          ldat = sorc(2)
      else if (kerr .eq. 2) then
          call errtxt ('INVCKPL',msg1)
          ldat = sorc(3)
      else if (kerr .eq. 3) then
          call errtxt ('INVGOFWD',msg1)
          ldat = sorc(3)
      else if (kerr .eq. 4) then
          call errtxt ('VECZERO',msg1)
          ldat = sorc(1)
      else if (kerr .eq. 5) then
          call errtxt ('NOCRAD',msg1)
          ldat = sorc(3)
      else if (kerr .eq. 6) then
          call errtxt ('CIRVEC',msg1)
          ldat = sorc(3)
      end if
      call errtxt ('INVCIRC',msg)
      i       = 1
      go to 9010
c
 9600 kerr   = 10
      go to 9000
c
 9700 i     = 0
      msg   = cmsg
      go to 9010
c
      end
c
c***********************************************************************
c
c   SUBROUTINE:  srcbwd (cbuf,knc,cwrd,knwd,kmxwd,klast,krem)
c
c   FUNCTION:  This routine breaks out 'words' from a text string.  A
c              word is separated by spaces in the string.
c
c   INPUT:  cbuf    C*n  D1  Text string which contains words.
c
c           knc     I*4  D1  Number of chars in 'cbuf'.
c
c           kmxwd   I*4  D1  Maximum number of words to break out of
c                            'cbuf'.
c
c           klast   I*4  D1  Specifies which entry in the input string
c                            will be defined as a text string containing
c                            the remaining parameters.  This entry can
c                            contain spaces and no further parsing past
c                            this entry will be done.
c
c           krem    I*4  D1  1 = Parsing a REMARK statement (at least
c                            2 spaces between words due to spaces in
c                            filename). 0 = Parsing any other string.
c
c   OUTPUT: cwrd    C*n  Dn  Array of text strings to receive words.
c
c           knwd    I*4  D1  Number of words found.
c
c***********************************************************************
c
      subroutine srcbwd (cbuf,knc,cwrd,knwd,kmxwd,klast,krem)
c
      integer*4 knc,knwd,kmxwd,klast,krem
c
      character*(*) cbuf,cwrd(5)
c
      integer*4 ist,inc,inc1,ien,nindex
c
c...Find next word in text string
c
      knwd   = 0
      ist    = 1
  100 inc    = nindex(cbuf(ist:knc),' ')
      if (inc .eq. 0) go to 8000
      inc    = ist    + inc     - 1
      if (knwd+1 .eq. klast) then
          ien    = knc
      else
          if (krem .eq. 1) then
              inc1   = index(cbuf(inc:knc),'  ')
          else
              inc1   = index(cbuf(inc:knc),' ')
          endif
          ien    = inc    + inc1   - 2
          if (ien .lt. inc) ien = knc
      endif
      knwd   = knwd   + 1
      cwrd(knwd) = cbuf(inc:ien)
      ist    = ien    + 1
      if (ist .le. knc .and. knwd .lt. kmxwd) go to 100
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  srcprs (krec,cdat,knc,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from an APT Source file, and
c              performs the initial parsing.
c
c   INPUT:  kfl     I*4  D1  1 = Do not read record, only parse it.
c                            'cdat' and 'knc' will be furnished by the
c                            calling routine.
c
c   OUTPUT: krec    I*4  D1  Record number which was read.
c
c           cdat    C*n  D1  Data from record.
c
c           knc     I*4  D1  Number of chars in 'cdat'.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine srcprs (krec,cdat,knc,kfl,cmsg,kerr)
c
      include 'clnrd.inc'
      include 'menu.inc'
      include 'compile.inc'
c
      integer*4 krec,kerr,kfl,knc
c
      character*(*) cdat,cmsg
      character*256 dbug,ldat
c
      integer*4 strlen1,ityp,is1,ie1,l1,it,skn,ier1,lcom,jindex
      integer*4 answer,nc
      character*6 lrem
c
c...Initialize routine
c
      kerr    = 0
c
c...Record does not need to be read
c...parser data only
c
  100 ITXTPT = 1
c
c...Read source file record
c......MACRO in effect
c
      if (JMAC .eq. 1) then
          if (MRECS .eq. MRECE .and. kfl .ne. 1) then
              JMAC = 0
          else
              call amread (MRECS,ldat,nc,lcom,kfl,cmsg,kerr)
              skn    = knc
          endif
      endif
c
c......Read from source file
c
      if (JMAC .eq. 0) then
          if (kfl .eq. 1) go to 200
          call rdtxt (LUNSC2,ldat,cmsg,kerr)
          if (kerr .eq. 1) then
              kerr   = 2
              go to 8000
          endif
          if (kerr .ne. 0) go to 8000
          nc     = strlen1(ldat)
          krec   = krec   + 1
          if (nc .eq. 0) go to 100
          if (nc .gt. MCNSRC) knc = MCNSRC
c
c.........Store record
c
          if (kfl .eq. 2) then
              cdat(knc+1:) = ldat
              knc    = knc    + nc
          else
              cdat   = ldat
              knc    = nc
          endif
c
c...Parse record
c
  200     skn    = knc
          knc    = strlen1(cdat)
          if(knc .gt. MCNSRC) knc = MCNSRC
          call txtcmd (cdat,knc,answer)
          if (answer .ne. 0) goto 8000
          call stpars (cdat,knc,1,cmsg,kerr,lcom)
      endif
c
c...See if $$ CIRCLE but make sure it is not comment
c...just with circle word
c
      if (lcom .eq. 1 .and. NTOK .eq. 0) then
         if (cdat(knc+1:knc+1) .eq. '%') then
           cdat(knc+1:knc+1) = ' '
         else
           cdat(knc+1:knc+2) = ' '
         endif
         call txtcmd (cdat,skn,answer)
         if (answer .ne. 0) goto 8000
         call stpars (cdat,skn,1,dbug,ier1,lcom)
         if (ier1 .ne. 0) go to 100
         if (NTOK .eq. 0) go to 100
         ier1 = RCSUB(1)
         if (ICOMCI .eq. 2 .or. ICTYP(1) .eq. 1 .and.
     -      (ier1 .eq. 4042 .or. ier1 .eq. 4026)) then
            if (jindex(ICTYP,4,NTOK-1) .ne. 0) go to 100
            if (ICOMCI .eq. 1 .and.
     -         (ICTYP(2) .ne. 2 .or. RCSUB(2) .ne. 7)) go to 100
            ICOMCI = 1
            if (ICTYP(NTOK) .eq. 4) then
               is1    = RCSUB(NTOK)
               if (LCTXT(is1:ICNC(NTOK)) .eq. '$') ICOMCI = 2
            end if
            knc    = skn
         else
            go to 100
         end if
      end if
c
      if (NTOK .eq. 0 .and. kfl .eq. 1) go to 8000
      if (NTOK .eq. 0) go to 100
      if (ICTYP(1) .eq. 4) then
         if ((LCTXT(1:1) .eq. 'N' .or. LCTXT(1:1) .eq. 'C')
     1                            .and. ICNC(1) .gt. 0) then
            it     = 2
            if (ICTYP(it) .eq. 2 .and. NTOK .gt. 2) it = 3
         else
            it     = 1
         end if
c
c... if LETTER  PPRINT  PARTNO  INSERT REMARK
c
         if (ICTYP(it) .eq. 4) then
            is1    = RCSUB(it)
            l1     = is1 + 6
            ie1    = ITXTPT
            if (ie1-is1 .ge. 5) then
              call touppr (LCTXT(is1:is1+5),lrem)
              if (LCTXT(l1:l1) .ne. '/' .and.
     1            (lrem .eq. LCOMTX(1) .or. lrem .eq. LCOMTX(2) .or.
     2             lrem .eq. LCOMTX(3) .or. lrem .eq. LCOMTX(4) .or.
     3             lrem .eq. LCOMTX(5))) then
                  call getvnm (lrem,ityp,PSTWRD,PSTWVL,NPSTWD)
                  RCSUB(it) = ityp
                  ICTYP(it) = 1
              endif
            endif
         endif
      endif
c
c...Check if parsed a comment line incorrectly.
c
      if ((ICTYP(1) .eq. 1) .and. ((RCSUB(1).eq.1043).or.
     -     (RCSUB(1).eq.1044).or.(RCSUB(1).eq.1045).or.
     -     (RCSUB(1).eq.1046).or.(RCSUB(1).eq.4009))) then
         kerr = 0
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  srcpwd (ktyp,krec,kpt,kbuf,gbuf,knw,cmsg,kerr)
c
c   FUNCTION:  This routine validates and parses an APT Source file
c              record.
c
c   INPUT:  ktyp    I*4  D1  Major word value of post-processor command
c                            being parsed.
c
c           kpt     I*4  D1  Token within global arrays at which to
c                            start parsing.
c
c   OUTPUT: krec    I*4  D1  Record number of input record.
c
c           kbuf    I*4  D50 Array to receive minor word values of
c                            parameters in the commmand.
c
c           gbuf    R*8  D50 Array to receive real values of parameters
c                            in the command.
c
c           knw     I*4  D1  Number of parameters in command.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred in the
c                            input command syntax.
c
c***********************************************************************
c
      subroutine srcpwd (ktyp,krec,kpt,kbuf,gbuf,knw,cmsg,kerr)
c
      include 'menu.inc'
      include 'compile.inc'
c
      integer*4 ktyp,kpt,kbuf(100),knw,kerr,krec,ktyp1(2)
      character*(*) cmsg
c
      real*8 gbuf(100),ksub(2)
c
      integer*4 i,j,ist,ien,nc,ipt,ifl,ival(2),kyco,kcnc(2)
c
      real*8 rtxt(5),rval
c
      character*40 ltxt
      character*256 ldat
c
      equivalence (rtxt,ltxt), (rval,ival)
c
c...Initialize routine
c
      knw    = 0
      kerr   = 0
      ipt    = kpt
      kyco = 0
c
c....Handle consecutive commas
c
  100 if (IOPFL(12) .ne. 0) then
          ist    = 0
   50     ist    = ist    + 1
          if (ICTYP(ist) .eq. 2 .and. RCSUB(ist) .eq. 2 .and.
     1        (ist .eq. NTOK .or. ist .eq. kpt .or.
     1        (ICTYP(ist+1) .eq. 2 .and. RCSUB(ist+1) .eq. 2))) then
              if (ist .eq. kpt) ist = ist - 1
c
c......Remove extra comma
c
              if (IOPFL(12) .eq. 843) then
                  do 60 i=ist+1,NTOK-1,1
                      ICTYP(i) = ICTYP(i+1)
                      RCSUB(i) = RCSUB(i+1)
                      ICNC(i) = ICNC(i+1)
   60             continue
                  NTOK   = NTOK   - 1
c
c......Add word/value
c
              else
                  do 70 i=NTOK,ist+1,-1
                      ICTYP(i+1) = ICTYP(i)
                      RCSUB(i+1) = RCSUB(i)
                      ICNC(i+1) = ICNC(i)
   70             continue
c
                  NTOK   = NTOK   + 1
                  if (NTOK .ge. MAXTOK) go to 9000
                  if (IOPFL(12) .eq. -1) then
                      ICTYP(ist+1) = 3
                      RCSUB(ist+1) = FILVAL
                  else
                      ICTYP(ist+1) = 1
                      RCSUB(ist+1) = IOPFL(12)
                  endif
              endif
          endif
          if (ist .lt. NTOK) go to 50
c
c...Check if the statment end with ",$"
c   If so, modify the input, put the input parameter before the
c   last comma into temporary storage and replace it with the "$"
c   character
c
          if (kyco .eq. 0) then
              if (ICTYP(NTOK-1) .eq. 2 .and. RCSUB(NTOK-1) .eq. 2
     1           .and. ICTYP(NTOK) .eq. 4 .and. ICNC(NTOK) .eq. 1) then
                  NTOK = NTOK - 2
                  ktyp1(1) = ICTYP(NTOK)
                  ksub(1) = RCSUB(NTOK)
                  kcnc(1) = ICNC(NTOK)
                  ICTYP(NTOK) = ICTYP(NTOK+2)
                  RCSUB(NTOK) = RCSUB(NTOK+2)
                  ICNC(NTOK) = ICNC(NTOK+2)
                  kyco = 1
              endif
           endif
      endif
c
c...Check for correct syntax
c
      do 500 i=ipt,NTOK,2
         knw    = knw    + 1
c
c......Minor word as MACHIN/name
c
         if (ktyp .eq. 1015 .and. i .eq. ipt .and. ICTYP(i) .eq. 1) then
             ival(1) = RCSUB(i)
             call getvwd (ival(1),ltxt,ien,1,PSTWRD,PSTWVL,NPSTWD)
             gbuf(knw) = rtxt(1)

c
c......Minor word
c
         else if (ICTYP(i) .eq. 1) then
             kbuf(knw) = RCSUB(i)
c
c......Real value
c
         else if (ICTYP(i) .eq. 3) then
             kbuf(knw) = 0
             gbuf(knw) = RCSUB(i)
c
c......MACHIN/name
c......MACH_AXIS/name
c
         else if (i .eq. ipt .and. ICTYP(i) .eq. 4 .and.
     1            (ICNC(i)-RCSUB(i)) .lt. 40 .and.
     2            (ktyp .eq. 1015 .or. ktyp .eq. 4068)) then
             if (ktyp .eq. 4068) then
                 kbuf(knw) = 0
                 gbuf(knw) = 0.
                 ICTYP(i) = 3
             else
                 ist    = RCSUB(i)
                 ien    = ICNC(i)
                 call touppr (LCTXT(ist:ien),ltxt)
                 kbuf(knw) = 0
                 if (ien-ist+1 .le. 8) then
                     gbuf(knw) = rtxt(1)
                 else
                     ival(1) = -1
                     ival(2) = ien - ist + 1
                     gbuf(knw) = rval
                     do 200 j=1,(ival(2)+7)/8,1
                         knw    = knw    + 1
                         kbuf(knw) = 0
                         gbuf(knw) = rtxt(j)
  200                continue
                 endif
             endif
c
c......$ - Continuation character
c
         else if (ICTYP(i) .eq. 4) then
             ist    = RCSUB(i)
             ien    = ICNC(i)
             do 300 j=ist,ien,1
                 if (LCTXT(j:j) .eq. '$' .and. (j .eq. ien .or.
     1               LCTXT(j+1:j+1) .ne. '$')) then
c
c.........Get continuation record
c
                     ifl    = 0
                     if (j .ne. ist) then
                         ldat   = LCTXT(ist:j-1)
                         nc     = j - ist
                         ifl    = 2
                     endif
                     call srcprs (krec,ldat,nc,ifl,cmsg,kerr)
                     if (kerr .ne. 0) go to 8000
c
c...If the previous statement end with ",$", modify the input
c   by moving them 2 places up in the memory and put the
c   previous statement argument back as the first input arguement
c   and add the "," character as the 2nd input argument.
c
                     if (kyco .eq. 1) then
                         NTOK = NTOK + 2
                         if (NTOK .ge. MAXTOK) go to 9000
                         do 250 kyco=NTOK,3,-1
                             RCSUB(kyco) = RCSUB(kyco-2)
                             ICTYP(kyco) = ICTYP(kyco-2)
                             ICNC(kyco) = ICNC(kyco-2)
  250                    continue
                         RCSUB(1) = ksub(1)
                         RCSUB(2) = 2
                         ICTYP(1) = ktyp1(1)
                         ICTYP(2) = 2
                         ICNC(1) = kcnc(1)
                         ICNC(2) = 0
                         kyco = 0
                     endif
                     ipt    = 1
                     knw    = knw    - 1
                     go to 100
                 endif
  300        continue
             go to 9000
c
c......Unrecognized parameter
c
         else
             go to 9000
         endif
c
c...Next delimeter should be comma
c
         if (i .lt. NTOK .and. (ICTYP(i+1) .ne. 2 .or.
     1        RCSUB(i+1) .ne. 2)) go to 9000
  500 continue
      go to 8000
c
c...End of routine
c
 8000 return
c
c...Invalid syntax
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  srcrd (krec,kpt,kbuf,gbuf,kmxcl,cmsg,kerr)
c
c   FUNCTION:  This routine reads an APT Source record and converts it
c              to a neutral format.
c
c   INPUT:  krec    I*4  D1  Next clfile physical record to read.
c
c           kpt     I*4  D1  Pointer of to next logical clfile record
c                            within physical record.
c
c   OUTPUT: kbuf    I*4  D4  Array to receive integer clfile data.  1 =
c                            ISN, 2 = CL record #, 3 = Record type, 4 =
c                            Record sub-type.
c
c           gbuf    R*8  Dn  Array to receive real clfile data.
c
c           kmxcl   I*4  D1  Number of reals in 'gbuf'.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine srcrd (krec,kpt,kbuf,gbuf,kmxcl,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'post.inc'
c
      equivalence (ICLREC,KPOSMP(0002))
c
      integer*4 ICLREC
c
      integer*4 krec, kpt, kbuf(4),kmxcl,kerr
C WNT-START
      integer*4 krec1,kpt1
C WNT-END
c
      real*8 gbuf(10)
c
      character*(*) cmsg
c
      integer*2 ibuf(4)
      integer*4 i,ipt,nwds
c
      real*8 rbuf
c
      equivalence (rbuf,ibuf)
c
c...Initialze routine
c
      kerr   = 0
      ICLREC = ICLREC + 1
c
c...In winNT, parameter is not pointer, they are not allowwed changed
c...it will have errors without error messsge (the result will wrong)
c...changed by Yurong
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C... SYSTEM OTHER THAN
c
c...Read cl record if necessary
c

C      kpt   = kpt    + 1
C      if ((kpt .gt. 64) .or. (krec .ne. LSTIRC)) then
C         if (kpt .gt. 64) then
C             krec   = krec   + 1
C             kpt    = 1
C         endif
C        call rdprm (LUNSC1,krec,JCLBUF,cmsg,kerr)
C         if (kerr .ne. 0) go to 8000
C      endif
C      ipt    = (kpt*4) - 3
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END

C WNT-START
      kpt1 = kpt + 1
      krec1 = krec
      if ((kpt1 .gt. 64) .or. (krec .ne. LSTIRC)) then
         if (kpt1 .gt. 64) then
             krec1   = krec1   + 1
             kpt1    = 1
         endif
         call rdprm (LUNSC1,krec1,JCLBUF,cmsg,kerr)
         if (kerr .ne. 0) go to 8000
      endif
      ipt    = (kpt1*4) - 3
C WNT-END
c
c...Store integer values
c

      kbuf(1) = ICLBUF(ipt+1)
      kbuf(2) = ICLREC
      kbuf(3) = ICLBUF(ipt+2)
      kbuf(4) = ICLBUF(ipt+3)
      nwds    = ICLBUF(ipt)
c
c...Store real values
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C... SYSTEM OTHER THAN WINNT

C      do 100 i=1,nwds,1
c
C         kpt    = kpt    + 1
C         if (kpt .gt. 64) then
C             krec   = krec   + 1
C             kpt    = 1
C             call rdprm (LUNSC1,krec,JCLBUF,cmsg,kerr)
C             if (kerr .ne. 0) go to 8000
C         endif
c
C         gbuf(i) = RCLBUF(kpt)
C 100 continue
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START

      do 100 i=1,nwds,1
         kpt1    = kpt1    + 1
         if (kpt1 .gt. 64) then
             krec1   = krec1   + 1
             kpt1    = 1
             call rdprm (LUNSC1,krec1,JCLBUF,cmsg,kerr)
             if (kerr .ne. 0) go to 8000
         endif

         gbuf(i) = RCLBUF(kpt1)
  100 continue
C WNT-END
c
      kmxcl  = nwds
c
c...End of routine
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C8000 LSTIRC = krec
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
 8000 LSTIRC = krec1
      krec = krec1
      kpt = kpt1
C WNT-END
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  srcppr (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c
c   FUNCTION:  This routine validates an APT Source file statement
c              enclosed in printesis.  One level of printesis only is
c              valid.
c
c   INPUT:  ktyp    I*4  D1  Major word value of post-processor command
c                            being parsed.
c
c           kpt     I*4  D1  Token within global arrays at which to
c                            start parsing.
c
c   OUTPUT: krec    I*4  D1  Record number of input record.
c
c           kbuf    I*4  D50 Array to receive minor word values of
c                            parameters in the commmand.
c
c           gbuf    R*8  D50 Array to receive real values of parameters
c                            in the command.
c
c           klt     I*4  D1  Pointer of the closing parentesis in command.
c
c           knw     I*4  D1  Number of parameters in command.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred in the
c                            input command syntax.
c
c***********************************************************************
c
      subroutine srcppr (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c
      character*(*) cmsg
      integer*4 ktyp,kpt,klt,kbuf(*),kerr,krec,knw
      real*8 gbuf(*)
c
      include 'menu.inc'
      include 'compile.inc'
c
      integer*4 i,ist,ien,nc,ipt,icom,ntk
c
      character*256 ldat
c
c...Check for correct syntax
c
      kerr   = 0
      knw    = 0
      ntk    = 0
      ipt    = kpt
c
  100 do 500 i=ipt,NTOK,1
         ntk    = ntk + 1
         if (ntk .eq. 1) then
            if (ICTYP(i) .ne. 1) go to 9000
            ktyp   = RCSUB(i)
         else if (ntk .eq. 2) then
            if (ICTYP(i) .ne. 2 .and. RCSUB(i) .ne. 7) go to 9000
         else if (ntk .eq. 3 .and. ICTYP(i) .eq. 2 .and.
     -            RCSUB(i) .eq. 17) then
            klt = i
            go to 9100
         else
c
c......Minor word
c
           if (ICTYP(i) .eq. 1) then
              knw    = knw    + 1
              icom   = 0
              kbuf(knw) = RCSUB(i)
c
c......Delimiting coma
c
           else if (ICTYP(i) .eq. 2) then
              if (RCSUB(i) .eq. 2 .and. icom .eq. 0) then
                 icom   = 1
                 go to 500
              else if (RCSUB(i) .eq. 18) then
                 if (icom .eq. 1) go to 9000
                 klt    = i
                 go to 8000
              else
                 go to 9000
              end if
c
c......Real value
c
           else if (ICTYP(i) .eq. 3) then
              knw    = knw    + 1
              icom   = 0
              kbuf(knw) = 0
              gbuf(knw) = RCSUB(i)
c
c......$ - Continuation character
c
           else if (ICTYP(i) .eq. 4) then
              ist    = RCSUB(i)
              ien    = ICNC(i)
              if (LCTXT(ist:ien) .eq. '$') go to 600
              go to 9000
c
c......Unrecognized parameter
c
           else
              go to 9000
           endif
         endif
  500 continue
      go to 8000
c
c...Get continuation record
c
  600 call srcprs (krec,ldat,nc,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      ipt    = 1
      go to 100
c
c...End of routine
c
 8000 return
c
c...Invalid syntax
c
 9000 kerr   = 1
      go to 8000
c
 9100 kerr   = 2
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  srcpar (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c
c   FUNCTION:  This routine parses an APT Source file statement
c              enclosed in printesis.  One level of printesis only is
c              valid.
c
c   INPUT:  ktyp    I*4  D1  Major word value of post-processor command
c                            being parsed.
c
c           kpt     I*4  D1  Token within global arrays at which to
c                            start parsing.
c
c   OUTPUT: krec    I*4  D1  Record number of input record.
c
c           kbuf    I*4  D50 Array to receive minor word values of
c                            parameters in the commmand.
c
c           gbuf    R*8  D50 Array to receive real values of parameters
c                            in the command.
c
c           klt     I*4  D1  Pointer of the closing parentesis in command.
c
c           knw     I*4  D1  Number of parameters in command.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred in the
c                            input command syntax.
c
c***********************************************************************
c
      subroutine srcpar (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c
      character*(*) cmsg
      integer*4 ktyp,kpt,klt,kbuf(*),kerr,krec,knw
      real*8 gbuf(*)
c
      include 'menu.inc'
      include 'compile.inc'
c
      character*256 ldat
      integer*4 j,ist,nc,np,ipt,ilt
c
      kerr   = 0
      ipt    = kpt
      np     = 0
c
  100 do 250 j=ipt,NTOK
c
c...Check if not continuation mark
c
         if (ICTYP(j) .eq. 4) then
             ist    = RCSUB(j)
             if (LCTXT(ist:ICNC(j)) .eq. '$') go to 300
             go to 9000
         end if
         np = np + 1
c
c...Baypass "/("
c
         if (np .eq. 1) then
             if (ICTYP(j) .ne. 2 .or. RCSUB(j) .ne. 7) go to 9000
         else if (np .eq. 2) then
             if (ICTYP(j) .ne. 2 .or. RCSUB(j) .ne. 17) go to 9000
c
c...Parse statement in parentesis
c
         else if (np .eq. 3) then
             call srcppr (ktyp,krec,j,ilt,kbuf,gbuf,knw,cmsg,kerr)
             if (kerr .ne. 0) go to 9100
             klt    = ilt
             go to 8000
         end if
  250 continue
c
c...Parse continuation line
c
  300 call srcprs (krec,ldat,nc,0,cmsg,kerr)
      if (kerr .ne. 0) go to 9000
      ipt    = 1
      go to 100
c
c...End of routine
c
 8000 return
c
c...Error
c
 9000 kerr   = 1
 9100 go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrclrc (kbuf,kmxc,krec,kcpt,cmsg,kerr)
c
c   FUNCTION:  This routine write cl work file logical record.
c
c   INPUT:  kbuf    I*4  Dn  Input data buffer.
c
c           kmxc    I*2  D1  Number of R*8 words in logical cl record.
c
c           krec    I*4  D1  Record number to write.
c
c           kcpt    I*4  D1  Pointer of last used JCLBUF array location.
c
c   OUTPUT: krec    I*4  D1  Last used Record number.
c
c           kcpt    I*4  D1  Pointer of last used JCLBUF array location.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred during
c                            write procedure.
c
c*****************************e*****************************************
c
      subroutine wrclrc (kbuf,kmxc,krec,kcpt,cmsg,kerr)
c
      integer*4 kbuf(*),krec,kcpt,kerr
      integer*2 kmxc
      character*(*) cmsg
c
      include 'clnrd.inc'
      include 'menu.inc'
c
      integer*2 ibuf(4)
      integer*4 i,icr,icpt,ierr,nindex,nrndex,strlen1,nc,inc,jbuf(66)
c
      character*7 token
      character*132 lbuf
c
      equivalence (jbuf,ibuf,lbuf)
c
c...Initialize routine
c
      kerr   = 0
      icpt   = kcpt
      icr    = krec
c
c...Check for PPRINT INCLUD/
c
      jbuf(1) = kbuf(1)
      jbuf(2) = kbuf(2)
      if (ibuf(3) .eq. 2000 .and. ibuf(4) .eq. 1044 .and.
     1    (INCLNO .lt. 20 .or. INCLCO .eq. 1)) then
          do 100 i=1,kmxc*2,1
              jbuf(i) = kbuf(i+2)
  100     continue
          nc     = kmxc   * 8
c
          inc    = nindex(lbuf(1:nc),' ')
          nc     = nrndex(lbuf(1:nc),' ')
          if (inc .ne. 0) then
c
c......INCLUD continuation line
c
              if (INCLCO .eq. 1) then
                  INCLNM(INCLNO)(INCLNC(INCLNO)+1:) = lbuf(inc:nc)
                  INCLNC(INCLNO) = strlen1(INCLNM(INCLNO))
                  INCLCO = 0
                  go to 8000
c
c......INCLUD/filename
c
              else
                  call touppr(lbuf(inc:inc+5),lbuf(inc:inc+5))
                  if (lbuf(inc:inc+6) .eq. 'INCLUD/') then
                      INCLNO = INCLNO + 1
                      INCLNM(INCLNO) = lbuf(inc+7:nc)
                      INCLIX(1,INCLNO) = krec   + 1
                      INCLIX(2,INCLNO) = kcpt
                      INCLMU(INCLNO) = MULT
                      INCLCO = 0
                      inc    = nrndex(INCLNM(INCLNO),' ')
                      if (INCLNM(INCLNO)(inc:inc) .eq. '~') then
                          INCLCO = 1
                          INCLNM(INCLNO)(inc:inc) = ' '
                          inc    = inc    - 1
                      endif
                      INCLNC(INCLNO) = inc
                      go to 8000
                  endif
              endif
          endif
      endif
c
c...Store logical record in physical record
c
      do 215 i=1,(kmxc+1)*2,1
         if (icpt .eq. 128) then
             icr    = icr    + 1
             call wrprm (LUNSC1,icr,JCLBUF,cmsg,ierr)
             if (ierr .ne. 0) go to 9000
             icpt   = 0
         endif
         icpt   = icpt   + 1
         JCLBUF(icpt) = kbuf(i)
  215 continue
c
      kcpt   = icpt
      krec   = icr
c
c...End of routine
c
 8000 return
c
c...Error in writing
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ignwrd (ktyp,kflg)
c
c   FUNCTION:  This routine checks if it is valid postprocessor
c              command, so it can be ignored.
c
c   INPUT:  ktyp    I*4  D1 - Major word value.
c
c           kflg    I*4  D1 - Index of word in the 'ignor' array
c                             or 0 if not existing.
c***********************************************************************
c
      subroutine ignwrd (ktyp,kflg)
c
      integer*4 ktyp,kflg
c
      integer*4 ignor(20),nignor,jindex
c
      data nignor /2/
      data ignor /4038,4043,18*0/
c
      kflg  = jindex (ignor,ktyp,nignor)
c
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  srcppl (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c
c   FUNCTION:  This routine parses an APT Source PLANE statement using
c              nested POINT definitions and returns PL in cononical form.
c
c   INPUT:  ktyp    I*4  D1  Major word value of post-processor command
c                            being parsed.
c
c           kpt     I*4  D1  Token within global arrays at which to
c                            start parsing.
c
c   OUTPUT: krec    I*4  D1  Record number of input record.
c
c           kbuf    I*4  D50 Array to receive minor word values of
c                            parameters in the commmand: here zeroed.
c
c           gbuf    R*8  D50 Array to receive real values of parameters
c                            in the command.
c
c           klt     I*4  D1  Pointer of the closing parentesis in command.
c
c           knw     I*4  D1  Number of parameters in output buffer = 4.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred in the
c                            input command syntax.
c
c***********************************************************************
c
      subroutine srcppl (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c
      character*(*) cmsg
      integer*4 ktyp,kpt,klt,kbuf(*),kerr,krec,knw
      real*8 gbuf(*)
c
      include 'menu.inc'
      include 'compile.inc'
c
      character*256 ldat
c
      integer*4 ityp,irec,ipt,ilt,nwds,i,j,ier,ibuf(10),
     -          np,icom,nc,ist,vecpln
c
      real*8 d1,d2,rbuf(10),pt(9),ndist,vc1(3),vc2(3),norm(3),ndot
c
      kerr   = 0
      knw    = 0
      ipt    = kpt
      np     = 0
      icom   = 0
      vecpln = 0
      if ((ICTYP(ipt) .eq. 1) .and. (RCSUB(ipt) .eq. 4032)) then
         i = 0
         if ((ICTYP(ipt+1) .ne. 2) .or. (RCSUB(ipt+1) .ne. 7)) i = 1
         if ((ICTYP(ipt+2) .ne. 1) .or. (RCSUB(ipt+2) .ne. 18)) i = 1
         if (i .eq. 0) then
            go to 2000
         else
            go to 9000
         endif
      endif
c
c...Expecting nested POINT definiton
c...get pt coordinates
c
  100 i    = ipt + 1
         if (ICTYP(i) .eq. 1) then
            ityp   = RCSUB(i)
            if ((ityp .ne. 4035) .and. (ityp .ne. 604)) goto 9000
c
            call srcppr (ityp,irec,i,ilt,ibuf,rbuf,nwds,
     -                   cmsg,ier)
            if (ier .ne. 0 .or. nwds .ne. 3) go to 9100
c
            do 155 j=1,nwds
               if (ibuf(j) .ne. 0) go to 9000
               if (vecpln .eq. 0) then
c...point
                  pt(np+j) = rbuf(j)
               else if (vecpln .eq. 1) then
c...normal vector
                  norm(j) = rbuf(j)
               else
c...unknown error
                  go to 9000
               endif
  155       continue
            np     = np + 3
            icom   = 0
         else
            go to 9000
         end if
c
c...Process delimiters after POINT definition ",( and )"
c
  200    do 250 i=ilt+1,NTOK,1
            if (ICTYP(i) .eq. 2) then
               if (RCSUB(i) .eq. 2 .and. icom .eq. 0) then
                   icom = 1
                   go to 250
               else if (RCSUB(i) .eq. 17) then
                   if (icom .eq. 0) go to 9000
                   ipt = i
                   go to 100
               else if (RCSUB(i) .eq. 18) then
                   if (i .eq. NTOK .and. np .eq. 9) go to 800
                   if (i .eq. NTOK .and. vecpln .eq. 1)
     -                 go to 1000
c
                   go to 9000
               else
                   go to 9000
               end if
c
c...Check for continuation line
c
            else if (ICTYP(i) .eq. 4) then
               ist    = RCSUB(i)
               if (LCTXT(ist:ICNC(i)) .eq. '$') go to 600
               go to 9000
c
c...Check for PLANE/(POINT/#,#,#), PERPTO, (VECTOR/#,#,#)
c
            else if (ICTYP(i) .eq. 1) then
               if (RCSUB(i) .eq. 18) then
                  vecpln = 1
                  icom = 0
               else
                  go to 9000
               endif
c
c...Unrecognized parameter
c
            else
               go to 9000
            end if
  250    continue
      continue
c
c...Get next record from file
c
  600 call srcprs (krec,ldat,nc,0,cmsg,kerr)
      if (kerr .ne. 0) go to 9000
      ilt    = 0
      go to 200
c
c...Process PLANE/ statement,
c...create cononical definition
c
  800 d1     = ndist (pt(1),pt(4))
      d2     = ndist (pt(1),pt(7))
      do 805 i = 1,3,1
         vc1(i) = (pt(i+3) - pt(i)) / d1
         vc2(i) = (pt(i+6) - pt(i)) / d2
         kbuf(i) = 0
  805 continue
      call plnlnp (vc1,vc2,pt,gbuf,kerr)
      gbuf(4) = 0.d0 - gbuf(4)
      go to 6000
c
c...Create canonical plane definition from point and
c...perpendicular vector.  PLANE/(POINT/...),PERTO,(VECTOR/...).
c
 1000 call unitvc(norm, norm)
      call copyn(norm, gbuf, 3)
      gbuf(4) = ndot(norm, pt)
      kbuf(1) = 0
      kbuf(2) = 0
      kbuf(3) = 0
      go to 6000
c
c...Check the syntax of a plane (given by 4 numbers a,b,c,d) and
c...two points.  The points define the normal to the plane with
c...the plane containing the second point.
c...PLANE/PERPTO,#,#,#,#,(POINT/#,#,#),(POINT/#,#,#)
c
 2000 ipt = 5
      do 2100 i = 1, 7, 2
         if (ICTYP(ipt+i-1) .ne. 3) go to 9000
         if ((ICTYP(ipt+i) .ne. 2).and.(RCSUB(ipt+i) .ne. 2)) goto 9000
 2100 continue
c
c...Done with the plane info.  Now, check the two points.
c
      if ((ICTYP(ipt+8) .ne. 2) .and.(RCSUB(ipt+8) .ne.17))  goto 9000
      if ((ICTYP(ipt+18) .ne. 2).and.(RCSUB(ipt+18) .ne.17)) goto 9000
      if ((ICTYP(ipt+9).ne.1) .and.(RCSUB(ipt+9).ne.4035))   goto 9000
      if ((ICTYP(ipt+19).ne.1).and.(RCSUB(ipt+19).ne.4035))  goto 9000
      if ((ICTYP(ipt+10) .ne. 2).and.(RCSUB(ipt+10) .ne. 7)) goto 9000
      if ((ICTYP(ipt+20) .ne. 2).and.(RCSUB(ipt+20) .ne. 7)) goto 9000
      if ((ICTYP(ipt+16) .ne. 2).and.(RCSUB(ipt+16) .ne.18)) goto 9000
      if ((ICTYP(ipt+26) .ne. 2).and.(RCSUB(ipt+26) .ne.18)) goto 9000
 2200 do 2300 i = 1, 5, 1
         if (mod(i,2) .eq. 1) then
            if (ICTYP(ipt+i+10) .ne. 3) go to 9000
            if (ICTYP(ipt+i+20) .ne. 3) go to 9000
         else
            if ((ICTYP(ipt+i+10) .ne. 2) .and.
     -          (RCSUB(ipt+i+10) .ne. 2)) go to 9000
            if ((ICTYP(ipt+i+20) .ne. 2) .and.
     -          (RCSUB(ipt+i+20) .ne. 2)) go to 9000
         endif
 2300 continue
c
c......Calculate plane
c......RCSUB(5,7,9,11) = Normal Plane
c......RCSUB(16,18,20) = 1st Point
c......RCSUB(26,28,30) = 2nd Point
c
      do 2400 i = 1, 3, 1
         norm(i) = RCSUB(ipt+(2*(i-1)))
         pt(i) = RCSUB(ipt + (2*i) + 19) - RCSUB(ipt + (2*i) + 9)
         pt(i+3) = RCSUB(ipt + (2*i) + 19)
 2400 continue
      if (dsqrt(ndot(pt,pt)) .lt. 1.d-6) go to 9000
      call unitvc(pt,pt)
      call plnlnp(norm,pt,pt(4),gbuf,kerr)
      gbuf(4) = 0. - gbuf(4)
      if (kerr .ne. 0) go to 9000
c
c...Set output buffer like for simple input
c
 6000 kbuf(4) = 0
      knw     = 4
      klt     = NTOK
      go to 8000
c
c...End of routine
c
 8000 return
c
c...Errors
c
 9000 kerr   = 1
      go to 8000
 9100 kerr   = 2
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  chknum (kbuf,knum)
c
c   FUNCTION:  This routine checks if APT source buffer contains
c              numerical data only.
c
c   INPUT:  kbuf    I*4  D1 - Integer buffer (minor word value).
c
c           knum    I*4  D1 - Number of words to check.
c
c***********************************************************************
c
      function chknum (kbuf,knum)
c
      integer*4 chknum, kbuf(*), knum
c
      integer i
c
      chknum = 1
      do 100 i=1,knum,1
         if (kbuf(i) .ne. 0) go to 200
  100 continue
      chknum = 0
  200 return
      end


c
c***********************************************************************
c
c   SUBROUTINE:  doplan (ktyp,krec,kpos,klt,kbuf,gbuf,knw,cmsg,kerr)
c
c   FUNCTION:  Parse and reduce plane definitions (possibly embedded
c              and/or complex) into a canonical plane info
c              gout (1:4) = a,b,c,d
c              where (a,b,c) is a unit vector perpendicular to the
c              plane and d is the distance from the origin to the
c              nearest point on the plane.
c
c              Currently accepting (PLANE/PERPTO,plane,point,point)
c              and (PLANE/point,PERPTO,vector).
c
c   INPUT:  ktyp    I*4  D1  - Major word value of post-processor
c                              command being parsed.
c
c           kpos    I*4  D1  - Position in the input arrays ITYP and
c                              RCSUB to start the processing
c
c   OUTPUT: krec    I*4  D1  Record number of input record.
c
c           klt     I*4  D1  Pointer of the closing parentesis in command.
c
c           kbuf    I*4  D50 - Array to receive minor word values of
c                              parameters in the commmand.
c
c           gbuf    R*8  D4  - Canonical plane info (a,b,c,d)
c                              set to (0.0, 0.0, 0.0, 0.0) if error
c
c           knw     I*4  D1  - Number of parameters in command.
c
c           cmsg    C*n  D1  - Text of error message.
c
c           kerr    I*4  D1  - Error flag    = 0 if processed fully
c                                            = 1 if error
c
c***********************************************************************
c
      subroutine doplan (ktyp,krec,kpos,klt,kbuf,gbuf,knw,cmsg,kerr)
c
      integer*4 ktyp,krec,kpos,klt,kbuf(*),knw,kerr
      real*8 gbuf(*)
      character*(*) cmsg
c
      include 'menu.inc'
      include 'compile.inc'
c
      integer*4 toktyp(80),tokend(80),i,strpos,ist,maxptr,starta
      integer*4 recnum,numchr,statpt,prarry(80),prpntr,endarr
      integer*4 pntend,numprm,j,lasttk,iterat
      real*8 toksub(80),plnbuf(50)
      character*256 ldat
c
      gbuf(1) = 0.0
      gbuf(2) = 0.0
      gbuf(3) = 0.0
      gbuf(4) = 0.0
      knw = 0
      kerr = 0
      statpt = 1
      strpos = kpos
c
c...Store entire statement in the form of tokens and associated
c...info into some local arrays.  The reason to do this is to be
c...able to parse statements spanning multiple lines of apt source.
c
  200 do 500 i=strpos,NTOK,1
         if (statpt .gt. 80) go to 9000
         toktyp(statpt) = ICTYP(i)
         toksub(statpt) = RCSUB(i)
         tokend(statpt) = ICNC(i)
         statpt = statpt + 1
c
c...Check for continuation line
c
         if (ICTYP(i) .eq. 4) then
            ist    = RCSUB(i)
            if (LCTXT(ist:ICNC(i)) .eq. '$') go to 600
            go to 9000
         endif
  500 continue
c
c...If fully processed statement (ie. read in entire line and
c...didn't find any more continuation characters), go on to the
c...parsing of the statement.  If not finished/ready, set error
c...flag and bail out.
c
      if (i .eq. (NTOK+1)) then
         lasttk = statpt
         go to 700
      else
         go to 9000
      endif
c
c...Get next record from file.  Set statpt back by 1; I want
c...to overwrite the continuation character.
c
  600 call srcprs (recnum,ldat,numchr,0,cmsg,kerr)
      if (kerr .ne. 0) go to 9000
      strpos = 1
      statpt = statpt - 1
      go to 200
c
  700 do 710 i = 1, statpt, 1
         ICTYP(i) = toktyp(i)
         RCSUB(i) = toksub(i)
         ICNC(i)  = tokend(i)
  710 continue
      NTOK = statpt
      call srcppr(ktyp,recnum,1,klt,kbuf,gbuf,knw,cmsg,kerr)
      if (kerr .eq. 2) then
         call srcppl (ktyp,recnum,1,klt,kbuf,gbuf,knw,cmsg,kerr)

      end if
      if (kerr .eq. 0) go to 8000
c
c...Start parsing the statement.  First built an array of
c...positions of parentheses in the statement.  This array is
c...prarry[...].  Next, find the nested plane definitions, and
c...get them into canonical form: a,b,c,d.
c
 1000 call pararr(toktyp, toksub, statpt, prarry, prpntr)
      if (prpntr .eq. -1) go to 9000
      maxptr = prpntr - 1
c
c...Find leftmost, innermost, nested plane definition.
c...If found a nested definition, but it is not a plane,
c...set the positions in the parenthesis array to 0.  Go back
c...to find a (PLANE/...).
c...If there are no nested plane definitions, starta = endarr = 0
c
 2000 call findin(prarry, maxptr, starta, endarr)
      if ((starta .eq. -1) .or. (endarr .eq. -1)) go to 9000
      if ((starta .eq. 1) .and. (endarr .eq. maxptr)) then
c...Do entire remaining statement.
         starta = -2
         NTOK = lasttk
         goto 3100
      endif
c...Check for plane.
      if (toksub(prarry(starta)+1) .ne. 4032) then
         prarry(starta) = 0
         prarry(endarr) = 0
         go to 2000
      endif
c
c...Ready to process innermost, nested plane definition.
c...First, copy pertenent data into token arrays.
c...Second, let srcppl() routinue handle the rest.
c...Result is in plnbuf.
c
 3000 NTOK = (-prarry(endarr)) - prarry(starta) - 2
      if (NTOK .eq. -2) then
         NTOK = lasttk
         starta = -2
      endif
 3100 do 4000 i = 1, NTOK, 1
c
c...If (starta == -2), then the entire statement is used.
c...All nested definitions have already been processed.
c
         if (starta .ne. -2) then
            ICTYP(i) = toktyp(prarry(starta)+2+i)
            RCSUB(i) = toksub(prarry(starta)+2+i)
            ICNC(i)  = tokend(prarry(starta)+2+i)
         else
            ICTYP(i) = toktyp(i)
            RCSUB(i) = toksub(i)
            ICNC(i)  = tokend(i)
         endif
 4000 continue
      call srcppl (4032,recnum,1,pntend,kbuf,plnbuf,numprm,cmsg,kerr)
      if ((kerr .ne. 0) .or. (numprm .ne. 4)) go to 9000
c
c...Finished processing complete statement.
c
      if (starta .eq. -2) go to 7000
c
c...Rearrange statement to include proper plane definition.
c...Get rid of parentheses and "plane/".
c
      j = 1
      iterat = 1
 5000 do 6000 i = prarry(starta), lasttk, 1
         if (j .eq. lasttk) go to 6100
c
c...Break out of loop when entire statement has been processed.
c...Check j and not i because the resulting statement should be
c...shorter than original.
c
         if (j .le. 8) then
            if (mod(j,2) .eq. 1) then
               toktyp(i) = 3
               toksub(i) = plnbuf(iterat)
               iterat = iterat + 1
            else
               toktyp(i) = 2
               toksub(i) = 2.0
            endif
            tokend(i) = 0
         else
               toktyp(i) = toktyp(j)
               toksub(i) = toksub(j)
               tokend(i) = tokend(j)
         endif
c
c...I am replacing the nested plane definition with 8 tokens:
c...  4 numbers (a, b, c, d) and 4 commas.
c...This shortens the entire statement.  Enter the new plane
c...and then skip to the remainder of the statement (what came
c...after the nested plane definition).  Copy that into the reset
c...of the token arrays.
c
         if (j .ne. 8) then
            j = j + 1
         else
            j = -prarry(endarr) + 2
         endif
 6000 continue
 6100 lasttk = i - 1
      statpt = lasttk
      prarry(starta) = 0
      prarry(endarr) = 0
      go to 1000
c
c...Everything worked.  Load resulting plane data into output
c
 7000 gbuf(1) = plnbuf(1)
      gbuf(2) = plnbuf(2)
      gbuf(3) = plnbuf(3)
      gbuf(4) = plnbuf(4)
      ktyp = 4032
 7900 krec = recnum
      knw = 4
c
c...End of routine
c
 8000 return
c
c...Errors
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pararr(toktyp,toksub,insize,prarry,prpntr)
c
c   FUNCTION:
c
c   INPUT:  toktyp  I*4  D80 - Token types
c
c           toksub  R*8  D80 - Token subtypes
c
c           insize  I*4  D1  - size of token arrays
c
c   OUTPUT: prarry  I*4  D80 - array of {+1, -1} for parentheses
c                              in token arrays
c
c           prpntr  I*4  D1  - size of prarry
c                              = -1 if error
c
c***********************************************************************
c
      subroutine pararr (toktyp,toksub,insize,prarry,prpntr)
c
      integer*4 toktyp(80),insize,prarry(80),prpntr
      real*8 toksub(80)
c
      integer*4 i, prbaln
c
c...The statement started after the openning parenthesis;
c...however, the ending parenthesis will be included.
c...Therefore, to check for balanced parentheses, start off the
c...count at prbaln = 1.
c
      prarry(1) = 1
      prpntr = 2
      prbaln = 1
c
c...Check for parenthesis pairs.  Put the position into the
c...parenthesis array:
c...                    +position if '('
c...                    -position if ')'
c...By the end of the statement, the parenthesis array will
c...have a list of all parenthetical substatements.
c...Also, ensure balanced parentheses with prbaln.
c
  100 do 200 i = 1, insize, 1
      if (toktyp(i) .eq. 2) then
         if (toksub(i) .eq. 17) then
            prarry(prpntr) = i
            prpntr = prpntr + 1
            prbaln = prbaln + 1
         endif
         if (toksub(i) .eq. 18) then
            prarry(prpntr) = -i
            prpntr = prpntr + 1
            prbaln = prbaln - 1
         endif
      endif
  200 continue
c
c...If the parentheses don't balance out (prbaln != 0), fail.
c
      if (prbaln .ne. 0) prpntr = -1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  findin (prarry, maxptr, starta, endarr)
c
c   FUNCTION:  Find the starting and ending positions of parenthesis
c              pairs in an array of parentheses. Choose the leftmost
c              innermost pair.
c              By leftmost, I mean:
c                ->(,)<- ,(), ()
c              By innermost, I mean:
c                (,(, ->(,)<- ,),)
c              So, get the leftmost, innermost pair is:
c                (,(, ->(,)<- ,(,),(,), ),)
c              Return the positions in the array for
c                starta -> '('
c                endarr -> ')'
c
c   INPUT:  prarry  I*4  D80 - Array of positive and negetive
c                              integers  representing
c                              >0 -> opening parenthesis '('
c                              <0 -> closing parenthesis ')'
c
c           maxptr  I*4  D1  - Size of prarry array.
c
c   OUTPUT: starta  I*4  D1  - Start of leftmost, innermost
c                              parenthesis pair
c                              = 0 if no pair found
c                              = -1 if unbalanced
c
c           endarr  I*4  D1  - End of leftmost, innermost
c                              parenthesis pair
c                              = 0 if no pair found
c                              = -1 if unbalanced
c
c***********************************************************************
c
      subroutine findin (prarry, maxptr, starta, endarr)
c
      integer*4 prarry(80),maxptr,starta,endarr
c
      integer*4 i
c
      starta = 0
      endarr = 0
c
c...Find leftmost, innermost pair of parentheses.
c...Scan down the array.  Every time a '(' is found,
c...set starta (This prepares starta for output.).
c...When the first ')' is found, set endarr and exit.
c
  100 do 200 i=1,maxptr,1
         if (prarry(i) .gt. 0) then
            starta = i
         endif
         if ((prarry(i) .lt. 0) .and. (starta .ne. 0)) then
            endarr = i
            go to 8000
         endif
c
c...Unbalanced parenthesis.  ')' before any '('.  Fail.
c
         if ((prarry(i) .lt. 0) .and. (starta .eq. 0)) then
            go to 300
         endif
  200 continue
c
c...Ended search thru parenthetical statements but can't find a
c...balanced pair.  Fail.
c
  300 starta = -1
      endarr = -1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  txtcmd(cdat,size,answer)
c
c   FUNCTION:  Check if an APT Source line is a text command like
c              LETTER, PPRINT, PARTNO, INSERT, TPRINT, or REMARK.
c              If it is, set the global token arrays ICTYP, RCSUB,
c              ICNC, and NTOK.
c
c   INPUT:  cdat    C*4  D256 - APT line to check
c
c           size    I*4  D1  - size of string
c
c   OUTPUT: answer  I*4  D1  - token code number for text command or
c                              = 0 if not a text command
c
c***********************************************************************
c
      subroutine txtcmd (cdat,size,answer)
c
      include 'menu.inc'
      include 'compile.inc'
c
      character*(*) cdat
      integer*4 size, answer
c
      character*6 cmdstr
      integer*4 nc
c
      call getvwd (1043,LCOMTX(1),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1044,LCOMTX(2),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1045,LCOMTX(3),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1046,LCOMTX(4),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1047,LCOMTX(5),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (4009,LCOMTX(6),nc,1,PSTWRD,PSTWVL,NPSTWD)
c
cc    EXAMPLE CODE OF LINE NUMBERS LEADING A TEXT COMMAND LINE
cc
cc      if (ICTYP(1) .eq. 4) then
cc         if ((LCTXT(1:1) .eq. 'N' .or. LCTXT(1:1) .eq. 'C')
cc     1                            .and. ICNC(1) .gt. 0) then
cc            it     = 2
cc            if (ICTYP(it) .eq. 2 .and. NTOK .gt. 2) it = 3
cc         else
cc            it     = 1
cc         end if
cc
cc
      call touppr(cdat(1:6), cmdstr)
c
c... if LETTER  PPRINT  PARTNO  INSERT TPRINT REMARK
c
      answer = 0
      if ((cmdstr .eq. LCOMTX(1)) .or. (cmdstr .eq. LCOMTX(2)) .or.
     2    (cmdstr .eq. LCOMTX(3)) .or. (cmdstr .eq. LCOMTX(4)) .or.
     3    (cmdstr .eq. LCOMTX(5)) .or. (cmdstr .eq. LCOMTX(6))) then
         call getvnm (cmdstr,answer,PSTWRD,PSTWVL,NPSTWD)
c
         ICTYP(1) = 1
         RCSUB(1) = answer
c
         ICTYP(2) = 4
         RCSUB(2) = 7
         ICNC(2) = size - 6
c
         NTOK = 2
      endif
c
c...End of routine
c
 8000 return
      end


