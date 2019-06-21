c
c***********************************************************************
c
c   FILE NAME:  badstat.f
c   CONTAINS:
c			ptd_badcla(clafile, nc)
c			badcla(clafile,fnc)
c			ptd_badapt (aptfile,nc)
c			badapt (aptfile,fnc)
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        badstat.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c     	09/11/13 , 12:59:15
c
c***********************************************************************
c***********************************************************************
c
c   SUBROUTINE:  ptd_badcla(bufpt, sln, eln)
c
c   FUNCTION:  This routine read a text buffer
c              and put bad statement in to global array
c
c   INPUT:  bufpt   text buffer pointer
c			sln: start line
c			eln: endln
c		
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine ptd_badcla (bufpt, sln, eln)
c
      include 'menu.inc'
c
      integer*4 bufpt, sln, eln
      call badcla (bufpt, sln, eln)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  badcla (bufpt, sln, eln)
c
c   FUNCTION:  This routine reads an CLA buffer, check the syntax
c
c   INPUT:  bufpt   text buffer pointer
c			sln: start line
c			eln: endln
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine badcla (bufpt, sln, eln)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'post.inc'
      include 'compile.inc'
C WNT-START
      include 'postworks_nt.inc'
C WNT-END

c
      integer*4 bufpt, sln, eln
      integer*4 kerr, fnc, perc
c
      integer*4 mxc
      integer*4 ncpt
c
      integer*4 i,ipt,nwds,inc,index,inum,nc,ifl,icont,iseq,ierr,
     1          irecl,ist,ien,nci,ityp,ibuf(500),icpt,icr,ifini,irec,
     2          nprs,nwrd,ncip,chknum,nugci,hasseq
c
      real*8 rbuf(500),gts(6)
c
      character*20 att(4)
      character*24 lbuf
      character*80 ldat,msg,udat
c
c...Initialize statement parsing routines
c
      MAXPRN = 0
      MAXTOK = 100
c
c...LETTER  PPRINT  PARTNO  INSERT  REMARK
c
      call getvwd (1043,LCOMTX(1),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1044,LCOMTX(2),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1045,LCOMTX(3),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1046,LCOMTX(4),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (4009,LCOMTX(5),nc,1,PSTWRD,PSTWVL,NPSTWD)
c
c...Initialze routine
c
      irec   = sln
      ifl    = 0
      iseq   = 0
      ifini  = 0
      icpt   = 0
      icr    = 0
      gts(4) = 0.
      gts(5) = 0.
      gts(6) = 1.0
      MULT   = 0
      nprs   = 0
      nwrd   = 0
      ICOMCI = 0
      ncip   = 0
      nugci  = 0
      JMAC   = 0
      PTD_RDBUF = 1
      BUFEND = eln
c
c...Read source file record
c
  100 kerr   = 0
      nprs   = nprs   + 1
      if (irec.gt.eln) return
      perc = irec*100/(eln-sln)
      call pted_display_as_percent3(perc, bufpt)
c
      call srcprs (irec,ldat,nc,bufpt,msg,kerr)
      if (kerr.eq.2) goto 8000

      if (kerr .ne. 0 .and. NTOK .eq. 0) go to 9000
c
c...Initialize per record flags
c
  150 ifl    = 0
      ipt    = 1
      icont  = 5
c
c...Nxx sequence number
c
c...see if this record has sequence number
c
      hasseq = 0
      if (ICTYP(ipt) .eq. 4) then
         ist    = RCSUB(ipt)
         ien    = ICNC(ipt)
         if ((LCTXT(ist:ist) .eq. 'N' .or. LCTXT(ist:ist) .eq. 'C')
     1        .and. ist .lt. ien) then
             if (LICOPT(7) .eq. 1) go to 9000
             call ctoi (LCTXT(ist+1:ien),inum,ierr)
             if (ierr .eq. 0) then
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
c...PARTNO, PPRINT, INSERT, LETTER
c
      if (NTOK .ge. ipt .and. ICTYP(ipt) .eq. 1 .and.
     1    ((RCSUB(ipt) .ge. 1043. .and. RCSUB(ipt) .le. 1046.) .or.
     2    RCSUB(ipt) .eq. 4009)) then
c
c......Break out text
c
  160    kerr   = 0
         inum   = RCSUB(ipt)
         call touppr (ldat(1:nc),udat)
         call getvwd (inum,lbuf,nci,1,PSTWRD,PSTWVL,NPSTWD)
         inc    = index(udat,lbuf(1:nci))
         if (inc .eq. 0) go to 9000
         nci    = nc     - (inc+nci) + 1
c
c......Text is continued onto next line
c
         if (ldat(nc:nc) .eq. '~') then
             call srcprs (irec,ldat,nc,bufpt,msg,kerr)
             if (kerr .lt. 0) go to 9000
         endif
         go to 100
      endif
c
c...Next token should be a Post word
c
  200 if (kerr .ne. 0 .or. ICTYP(ipt) .ne. 1) go to 9000
      ityp   = RCSUB(ipt)
      ipt    = ipt    + 1
      nwds   = 0
		if (ipt .le. NTOK) then
          if (ICTYP(ipt) .ne. 2 .or. RCSUB(ipt) .ne. 7) go to 9000
          if (ipt .lt. NTOK) then
              ipt    = ipt    + 1
              call srcpwd (ityp,irec,ipt,ibuf,rbuf,nwds,msg,kerr)
              if (kerr .ne. 0) go to 9000
          endif
      endif
c
c...CUTTER
c
      if (ityp .eq. 4025) then
         if (chknum(ibuf,nwds) .ne. 0) go to 9000
c
c...CIRCLE
c
      else if (ityp .eq. 4026) then
         if (LICOPT(7) .eq. 1 .and.
     -       (ityp .eq. 4026)) go to 9000
         mxc    = 7
         if (nwds .ne. mxc) go to 9000
         if (chknum(ibuf,nwds) .ne. 0) go to 9000
c
c...GOTO
c
      else if (ityp .eq. 4013 .or. ityp .eq. 4019) then
         if (ibuf(1).eq.0) then
c
c...should be 5000 type
c
c
c......Check for another point
c......in this GOTO record
c
  350        mxc = mxc + nwds
             call srcprs (irec,ldat,nc,bufpt,msg,kerr)
             if (kerr .ne. 0 .and. NTOK .eq. 0) go to 9000
             if ((ICTYP(1) .eq. 1) .and. (RCSUB(1) .eq. 4013)) then
                 nwds   = 0
                 call srcpwd (ityp,irec,3,ibuf,rbuf,nwds,msg,kerr)
                 if (kerr .ne. 0) go to 9000
                 goto 350
             else
c
c...parse again
c
                 goto 150
             endif
         endif
c
c...MULTAX
c
      else if (ityp .eq. 1105) then
         if (nwds .gt. 1) go to 9000
         if (nwds .eq. 1) then
            if ((ibuf(1) .ne. 71).and.(ibuf(1) .ne. 72)) then
                go to 9000
            endif
         endif
c
c...FINI
c
      else if (ityp .eq. 4012) then
         ifini  = 1
      endif
      if (ierr .ne. 0) go to 9000
c
      if (ifini .eq. 0) go to 100
c
c...End of routine
c
c 8000 call clsfil(LUNSC3)
 8000  return
c
c...Invalid Apt Source record
c
 9000 if (kerr .lt. 0) go to 8000
      i      = 0
      kerr   = 2
c
c...somehow, it will be getting error if pass in nc
c
      ncpt = nc
      call add_badstr(ldat, ncpt)
      kerr   = 0
      go to 100
c
      end

c
c***********************************************************************
c
c   SUBROUTINE:  ptd_badapt (bufpt, sln, eln)
c
c   FUNCTION:  This routine read a text clfile
c              and put bad statement in to global array
c
c   INPUT:  bufpt   text buffer pointer
c			sln: start line
c			eln: endln
c		
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine ptd_badapt (bufpt, sln, eln)
c
      include 'menu.inc'
c
      integer*4 bufpt, sln, eln
      call badapt (bufpt, sln, eln)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  badapt (bufpt, sln, eln)
c
c   FUNCTION:  This routine reads an APT Source file, check the syntax.
c
c   INPUT:  bufpt   text buffer pointer
c			sln: start line
c			eln: endln
c
c   OUTPUT: None.
c
c***********************************************************************
c
      subroutine badapt (bufpt, sln, eln)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'post.inc'
      include 'compile.inc'
C WNT-START
      include 'postworks_nt.inc'
C WNT-END

c
      integer*4 bufpt, sln, eln
      integer*4 fnc, kerr,perc
      integer*4 ncpt
      character*80 cmsg
      integer*4 mxc
      integer*4 i,ipt,nwds,inc,index,inum,nc,ifl,icont,iseq,ierr,
     1          irecl,ist,ien,nci,ityp,ibuf(500),icpt,icr,ifini,irec,
     2          j,ilt,np,iosec, mix,nprs,nwrd,ncip,chknum,nugci
c
      real*8 rbuf(50),ddl
c
      character*20 att(4)
      character*24 lbuf
      character*80 ldat,udat,sorc(3)
c
c...Initialize statement parsing routines
c
      MAXPRN = 0
      MAXTOK = 100
c
c...LETTER  PPRINT  PARTNO  INSERT  REMARK
c
      call getvwd (1043,LCOMTX(1),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1044,LCOMTX(2),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1045,LCOMTX(3),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1046,LCOMTX(4),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (4009,LCOMTX(5),nc,1,PSTWRD,PSTWVL,NPSTWD)
c
c...Initialze routine
c
      irec   = sln
      ifl    = 0
      iseq   = 0
      ifini  = 0
      icpt   = 0
      icr    = 0
      MULT   = 0
      nprs   = 0
      nwrd   = 0
      ICOMCI = 0
      ncip   = 0
      nugci  = 0
      JMAC   = 0
      PTD_RDBUF = 1
      BUFEND = eln
c
c...Read source file record
c
  100 kerr   = 0
      nprs   = nprs   + 1
      if (irec.gt.eln) return
      perc = irec*100/(eln-sln)
      call pted_display_as_percent3(perc, bufpt)
c
c......read a line from buffer
c
      call srcprs (irec,ldat,nc,bufpt,cmsg,kerr)
      if (kerr .ne. 0 .and. NTOK .eq. 0) go to 9000
c
c...Initialize per record flags
c
      ipt    = 1
      icont  = 5
c
c...Nxx sequence number
c
      if (ICTYP(ipt) .eq. 4) then
         ist    = RCSUB(ipt)
         ien    = ICNC(ipt)
         if ((LCTXT(ist:ist) .eq. 'N' .or. LCTXT(ist:ist) .eq. 'C')
     1        .and. ist .lt. ien) then
             if (LICOPT(7) .eq. 1) go to 9000
             call ctoi (LCTXT(ist+1:ien),inum,ierr)
             if (ierr .eq. 0) then
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
          go to 100
      end if
c
c...MACRO definition
c
      if (NTOK .ge. ipt+2 .and. ICTYP(ipt) .eq. 4 .and.
     1    (ICTYP(ipt+1) .eq. 2 .and. RCSUB(ipt+1) .eq. 1) .and.
     2    (ICTYP(ipt+2) .eq. 1 .and. RCSUB(ipt+2) .eq. 5006)) then
          go to 100
      endif
c
c...CALL statement definition
c
      if (NTOK .ge. ipt+2 .and. ICTYP(ipt) .eq. 1 .and.
     1    RCSUB(ipt) .eq. 824) then
          go to 100
      endif
c
c...Ignore PAINT
c...       TOOL PATH
c...       END-OF-PATH
c
      if (ICTYP(ipt) .eq. 1) then
          if (RCSUB(ipt) .eq. 4046.) go to 100
          if (RCSUB(ipt) .eq. 617. .and. ICTYP(ipt+1) .eq. 1 .and.
     1        RCSUB(ipt+1) .eq. 234.) go to 100
          if (RCSUB(ipt) .eq. 1 .and. ICTYP(ipt+1) .eq. 2 .and.
     1        RCSUB(ipt+1) .eq. 5) go to 100
      endif
c
c...PARTNO, PPRINT, INSERT, LETTER
c...TOOL PATH is converted to PARTNO
c...DISPLAY/text is converted to PPRINT
c
      if (NTOK .ge. ipt .and. ICTYP(ipt) .eq. 1 .and.
     1    ((RCSUB(ipt) .ge. 1043. .and. RCSUB(ipt) .le. 1046.) .or.
     2    RCSUB(ipt) .eq. 4009 .or. RCSUB(ipt) .eq. 1021.)) then
c
c...Check for DISPLY/word,word,...
c...or DISPLY/text
c
         if (RCSUB(ipt) .eq. 1021) then
C WNT-START
            do 150 i=2,NTOK,1
C WNT-END
C VAX-SUN-SGI-IBM-HPX-DEC-DOS-START
C             do 150 i=ipt,NTOK,1
C VAX-SUN-SGI-IBM-HPX-DEC-DOS-END
                 if (ICTYP(i) .eq. 4) go to 160
  150        continue
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
         nci    = nc     - (inc+nci) + 1
c
c......Text is continued onto next line
c
         if (ldat(nc:nc) .eq. '~') then
             call srcprs (irec,ldat,nc,bufpt,cmsg,kerr)
             if (kerr .lt. 0) go to 9000
         endif
         go to 100
      endif
c
c...Next token should be a Post word
c
  200 if (kerr .ne. 0 .or. ICTYP(ipt) .ne. 1) go to 9000
      ityp   = RCSUB(ipt)
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
c...ISN/n sequence number
c
      if (ityp .eq. 4023) then
         if (nwds .ne. 1) go to 9000
         if (ibuf(1) .ne. 0) go to 9000
         go to 100
c
c...CUTTER
c
      else if (ityp .eq. 4025) then
         mxc    = nwds
         if (chknum(ibuf,nwds) .ne. 0) go to 9000
c
c...CIRCLE
c...CYLNDR
c
      else if (ityp .eq. 4026 .or. ityp .eq. 4036 .or.
     -         ityp .eq. 4042) then
         if (LICOPT(7) .eq. 1 .and.
     -       (ityp .eq. 4026 .or. ityp .eq. 4042)) go to 9000
         mxc    = 7
         if (nwds .gt. 7) then
           nugci = 1
           nwds  = 7
         endif
         if (nwds .ne. mxc) go to 9000
         if (chknum(ibuf,nwds) .ne. 0) go to 9000
c
c...GOTO
c
      else if (ityp .eq. 4013 .or. ityp .eq. 4024 .or.
     -         ityp .eq. 4019 .or. ityp .eq. 4027) then
         if (ityp .eq. 4024) icont = 6
         if (ityp .eq. 4019) icont = 3
         mxc    = 0
         if (ityp .eq. 4013 .or. ityp .eq. 4019) then
             if (nwds .eq. 3 .and. MULT .eq. 1) then
                 nwds = 6
             end if
c
c...GODLTA
c
         else if (ityp .eq. 4027) then
             if (LICOPT(7) .eq. 1) go to 9000
             if (nwds .ne. 6 .and.
     -          (nwds .gt. 3 .or. nwds .lt. 1)) go to 9000
             nwds   = 3
             if (MULT .eq. 1) nwds = 6
         endif
c
  700    if (nwds .ne. 3 .and. nwds .ne. 6) go to 9000
         if (mxc+nwds .gt. 240) go to 9000
         ifl    = 0
         if (chknum(ibuf,nwds) .ne. 0) go to 9000
         mxc = 0
         icont = 6
c
c......Check for another point
c......in this GOTO record
c
         call srcprs (irec,ldat,nc,bufpt,cmsg,kerr)
         ifl    = 1
         if (kerr .eq. 2) ifl = 0
         if (kerr .eq. 0 .and. icont .ne. 3 .and.
     1        ((ICTYP(1) .eq. 1 .and. RCSUB(1) .eq. 4013. .and.
     2         (iseq .eq. 1 .or. ncip .ne. 0)) .or.
     3          ICTYP(1) .eq. 3)) then
             ipt    = 1
             if (ICTYP(1) .eq. 1 .and. NTOK .ge. 3 .and.
     1            ICTYP(2) .eq. 2 .and. RCSUB(2) .eq. 7.) ipt = 3
             if (ICTYP(1) .eq. 3 .or. ipt .eq. 3) then
                call srcpwd (ityp,irec,ipt,ibuf,rbuf,nwds,cmsg,ierr)
			  if (ierr .ne. 0) go to 9000
                go to 700
             endif
         endif
c
c...MULTAX
c
      else if (ityp .eq. 1105) then
         if (nwds .gt. 1) go to 9000
         if (nwds .eq. 1) then
            if ((ibuf(1) .ne. 71).and.(ibuf(1) .ne. 72)) then
                go to 9000
            endif
         endif
c
c...FINI
c
      else if (ityp .eq. 4012) then
         ifini  = 1
c
c...INDIRV/i,j,k
c...INDIRP/x,y,z
c
      else if (ityp .eq. 4033 .or. ityp .eq. 4034) then
          if (nwds .ne. 3) go to 9000
          go to 100
c
c...PSIS/(PLANE/a,b,c,d)
c
      else if (ityp .eq. 4029) then
c
c......Parse statement in parentesis
c
          sorc(2) = ldat
          call srcpar (ityp,irec,ipt,ilt,ibuf,rbuf,nwds,cmsg,kerr)
          if (kerr .ne. 0) go to 9000
          if (nwds .ne. 4 .or. ityp .ne. 4032) go to 9000
          if (ilt .ne. NTOK) go to 9000
          go to 100
c
c...TLON (ignore it)
c
      else if (ityp .eq. 4028) then
         if (LICOPT(7) .eq. 1) go to 9000
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
          if (LICOPT(7) .eq. 1) go to 9000
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
     -                   RCSUB(j) .ne. 71 .or. np .eq. 6 .and.
     -                   RCSUB(j) .ne. 5)) go to 9000
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
                     ipt    = j
                     call srcppr (ityp,irec,ipt,ilt,ibuf,rbuf,nwds,
     -                               cmsg,kerr)
c
c...If nested "(" is open it must be PLANE/(POINT/ definition
c
                     if (kerr .eq. 2) then
                        ipt = ilt
                        call srcppl (ityp,irec,ipt,ilt,ibuf,rbuf,nwds,
     -                               cmsg,kerr)
                     end if
                     if (kerr .ne. 0 .or. ilt .ne. NTOK) go to 9000
                     if (ien .ne. 0) go to 9000
                     go to 980
                  end if
  950         continue
c
c......Get continuation line
c
  960         call srcprs (irec,ldat,nc,bufpt,cmsg,kerr)
              if (kerr .ne. 0) go to 9000
              ipt    = 1
              go to 910
c
c......Convert line to check plane
c
  980         if (ityp .eq. 4031) then
                 if (nwds .ne. 6) go to 9000
                 if (chknum(ibuf,nwds) .ne. 0) go to 9000
c
c......Save check plane
c
              else if (ityp .eq. 4032) then
                 if (nwds .ne. 4) go to 9000
                 if (chknum(ibuf,nwds) .ne. 0) go to 9000
              else
                 go to 9000
              end if
              go to 100
          else
              go to 9000
          end if
c
c...ARCMOV - circul motion using ARCDAT
c
      else if (ityp .eq. 4037) then
          if (nwds .ne. 5) go to 9000
          if (ibuf(1) .ne. 59 .and. ibuf(1) .ne. 60) go to 9000
          ddl    = 119.0 - ibuf(1)*2
          if (chknum(ibuf(2),nwds-1) .ne. 0) go to 9000
          go to 100
c
c...CW (CCW) circular record developed for Tucker Technology.
c
      else if (ityp .eq. 4044 .or. ityp .eq. 4045) then
          if (nwds .ne. 13) go to 9000
          if (chknum(ibuf,nwds) .ne. 0) go to 9000
          go to 100
      else
c
c...Standard Post word
c...Ignore some words (now INTOL,OUTTOL,AUTOPS)
c
         call ignwrd (ityp,j)
         if (j .ne. 0) go to 100
         mxc    = nwds
      endif
c
c...vp 4/28/98 reset commented CI when output last record of points
c...the first and other record must be output at 700 label (GOTO)
c
      if (ifini .eq. 0) go to 100
      go to 8000
c
c...End of routine
c
ccccccccccccccccccccccccc
c 8000 call clsfil (LUNSC2)
 8000    return
c
c...Invalid Apt Source record
c
 9000 if (kerr .lt. 0) go to 8000
      i      = 0
      kerr   = 2
c
c...somehow, it will be getting error if pass in nc
c
      ncpt = nc
      call add_badstr(ldat, ncpt)
      kerr   = 0
      go to 100
c
      end
