c
c***********************************************************************
c
c   FILE NAME:  cmpprt
c   CONTAINS:
c               cmpprt  pdfbrk  pdfgvl  pdfrdf  pdfvar  pdfvdf  pdfwrd
c               pdfwvl
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cmpprt.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 08:34:38
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cmpprt (cmsg,kerr)
c
c   FUNCTION:  This routine loads and compiles the Print Descriptor File
c              The descriptors are stored in the following arrays.
c
c              FRMBUF(1:60) = Text of print file lines.  Maximum of 6
c                             per record and maximum 10 records.
c              FRMFNC(1,n)  = Number of characters in FRMBUF.
c              FRMFNC(2,n)  = Number of lines for each record.
c              FRMFNC(3,n)  = Number of variables in record.  Maximum 15
c                             per record.
c              FRMFNC(4,n)  = 1 = New page prior to record.
c              FRMFNC(5,n)  = 1 = Blank line prior to record.
c              FRMFNC(6,n)  = 1 = Blank line after record.
c              IPRTXT       = Variable descriptors (size=10), 15 per
c                             record.
c              PRTLDS       = Line descriptors (size=6), 6 per record.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1     Text of error message.
c
c           kerr    I*4  D1     Returns nonzero when an error occurred.
c
c***********************************************************************
c
      subroutine cmpprt (cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
C WNT-START
      include "postworks_nt.inc"
C WNT-END
c
      equivalence (IPRDES,KPOSMP(1154)), (NTRAIL,KPOSMP(1175))
c
      integer*4 IPRDES(2,10),NTRAIL
c
      equivalence (PDFFIL,CPOSMP(3249))
c
      character*40 PDFFIL
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 i,irecl,nc,nca,irec,nrec,itxt,ifl,ivar,nvar,irecf(10),
     1          inum,strlen1,ityp,irary(6),ilin,ilinf(10),nlin,inc,ierr,
     2          ifpt,j
C WNT-START
      integer*4 flag
C WNT-END
c
      character*20 att(4)
      character*512 ldat,ltmp,lbuf
      character*(MAX_PATH) fnam
c
c...Don't process PDF file if no print file is generated
c
      if (IOPFL(4) .eq. 0) go to 8000
c
c...Open up print descriptor file
c
      nc     = strlen1(PDFFIL)
      fnam   = 'pworks_' // PDFFIL(1:nc) // '.pdf'
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'old'
      irecl  = 80
      call opnfil (LUNHLP,fnam,att,irecl,cmsg,kerr)
c
c......Local file does not exist
c......try global file
c
      if (kerr .eq. -2) then
          call fparse (fnam,fnam,DVDATA,'.pdf')
          call opnfil (LUNHLP,fnam,att,irecl,cmsg,kerr)
      endif
      if (kerr .ne. 0) go to 8000
c
c...Initialize routine
c
      irec   = 0
      nrec   = 0
      do 50 i=1,10,1
          irecf(i) = 0
          FRMFNC(2,i) = 0
          FRMFNC(3,i) = 0
   50 continue
c
c...Read descriptor record
c
  100 call rdtxt (LUNHLP,lbuf,cmsg,kerr)
c        write (6,*) lbuf
      if (kerr .eq. 1) go to 7000
      if (kerr .ne. 0) go to 8000
c
c...Parse record
c
      nc     = strlen1(lbuf)
      call pdftyp (lbuf,nc,ldat,nca,ityp,inum,1)
      if (ityp .eq. -1) then
          go to 9000
      else if (ityp .eq. 0) then
          go to 100
c
c......#RECORD n#
c
      else if (ityp .eq. 1) then
          if (inum .lt. 1 .or. inum .gt. 10) go to 9000
          if (irecf(inum) .ne. 0) go to 9100
c
c.........Store previous record
c
          if (irec .ne. 0) then
              FRMFNC(2,irec) = nlin
              FRMFNC(3,irec) = nvar
              FRMFNC(4,irec) = irary(1)
              FRMFNC(5,irec) = irary(2)
              FRMFNC(6,irec) = irary(3)
          endif
c
c.........Parse this record
c
          call pdfrdf (ldat,nca,irary,ierr)
          if (ierr .ne. 0) go to 9000
          if (irary(4) .ne. 0 .or. irary(5) .ne. 0 .or. irary(6) .ne. 0)
     1            go to 9000
c
c.........Initialize this record
c
          irec   = inum
          irecf(irec) = 1
          ilin   = 0
          nlin   = 0
          nvar   = 0
          ifl    = 0
          ifpt   = (irec-1) * 6
          do 300 i=1,6,1
              ilinf(i) = 0
              FRMFNC(1,ifpt+i) = 0
  300     continue
c
c....../LINE n/
c
      else if (ityp .eq. 2) then
          if (irec .eq. 0) go to 9000
          if (ilinf(inum) .ne. 0) go to 9100
          inc    = (irec-1) * 6 * 6 + (inum-1) * 6 + 1
          call pdfrdf (ldat,nca,PRTLDS(inc),ierr)
          if (ierr .ne. 0) go to 9100
          ilin   = inum
          ilinf(ilin) = 1
          if (ilin .gt. nlin) nlin = ilin
          itxt   = 0
          ivar   = 0
c
c....../TEXT/
c
      else if (ityp .eq. 3) then
          if (ilin .eq. 0) go to 9000
          if (itxt .eq. 1) go to 9100
          itxt   = 1
          ifl    = 1
          FRMBUF(ifpt+ilin) = ldat
          FRMFNC(1,ifpt+ilin) = nca
c
c....../VARS/
c
      else if (ityp .eq. 4) then
          if (ilin .eq. 0) go to 9000
          if (ivar .eq. 1) go to 9100
          call pdfvdf (ldat,nca,irec,ilin,nvar,ierr)
          if (ierr .eq. 1) go to 9000
          ivar   = 1
          ifl    = 2
c
c......TEXT continuation
c
      else if (ityp .eq. 5) then
          if (ifl .ne. 1) go to 9000
          ltmp   = FRMBUF(ifpt+ilin)(1:FRMFNC(1,ifpt+ilin)) //
     1             ldat(1:nca)
          FRMBUF(ifpt+ilin) = ltmp
          FRMFNC(1,ifpt+ilin) = FRMFNC(1,ifpt+ilin) + nca
          if (FRMFNC(1,ifpt+ilin) .gt. 132) FRMFNC(1,ifpt+ilin) = 132
c
c......VARS continuation
c
      else if (ityp .eq. 6) then
          if (ifl .ne. 2) go to 9000
          call pdfvdf (ldat,nca,irec,ilin,nvar,ierr)
          if (ierr .eq. 1) go to 9000
c
c......Unrecognized record
c
      else
          go to 9000
      endif
c
c...Go get next record
c
      go to 100
c
c...Store last record
c
 7000 if (irec .ne. 0) then
          FRMFNC(2,irec) = nlin
          FRMFNC(3,irec) = nvar
          FRMFNC(4,irec) = irary(1)
          FRMFNC(5,irec) = irary(2)
          FRMFNC(6,irec) = irary(3)
      endif
c
c...Define number of lines
c...in trailer record
c
      NTRAIL = 0
      do 7100 i=1,2,1
          irec   = IPRDES(i,2)
          if (irec .ne. 0) then
              NTRAIL = NTRAIL + FRMFNC(2,irec)
              if (FRMFNC(5,irec) .eq. 1) NTRAIL = NTRAIL + 1
              if (FRMFNC(6,irec) .eq. 1) NTRAIL = NTRAIL + 1
              inc    = (irec-1) * 36 + 1
              do 7050 j=1,FRMFNC(2,irec)
                  if (PRTLDS(inc+1) .eq. 1) NTRAIL = NTRAIL + 1
                  if (PRTLDS(inc+2) .eq. 1) NTRAIL = NTRAIL + 1
                  inc    = inc    + 6
 7050         continue
          endif
 7100 continue
      kerr   = 0
c
c...End of routine
c
 8000 call clsfil (LUNHLP)
      return
c
c...Invalid record
c
 9000 call errtxt ('PRCSYN',ldat)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C9050 call trmmsg (' ')
C      call trmmsg (ldat)
C      call trmmsg (lbuf)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
 9050 flag = 1
      nc = strlen1(ldat)
      call add1dispmsg (ldat, nc, flag)
      nc = strlen1(lbuf)
      call add1dispmsg (lbuf, nc, flag)
C WNT-END
      go to 100
c
c...Multiply defined record
c
 9100 call errtxt ('PRCMULT',ldat)
      go to 9050
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pdfbrk (cdat,kst,knc,cout,knco)
c
c   FUNCTION:  This routine breaks out the next parameter in a parameter
c              list that has the following syntax.
c
c              [parm1,parm2,...]
c
c   INPUT:  cdat    C*n  D1  -  Input line.
c
c           kst     I*4  D1  -  Current position within 'cdat'.  Should
c                               point to the '[' character on the first
c                               call and not changed by the calling
c                               routine thereafter.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c   OUTPUT: kst     I*4  D1  -  See INPUT.
c
c           cout    C*n  D1  -  Text of next parameter.
c
c           knco    I*4  D1  -  Number of characters in 'cout'.
c
c***********************************************************************
c
      subroutine pdfbrk (cdat,kst,knc,cout,knco)
c
      integer*4 kst,knc,knco
c
      character*(*) cdat,cout
c
      integer*4 inc1,inc2,inc3,inc4,index
c
c...Get end of parameter
c
      if (kst .ge. knc) go to 9000
      inc1   = index(cdat(kst+1:knc),',')
      inc2   = index(cdat(kst+1:knc),']')
      inc3   = index(cdat(kst+1:knc),'(')
      inc4   = index(cdat(kst+1:knc),')')
      if (inc3 .lt. inc1 .and. inc4 .gt. inc1 .and. inc4 .lt. inc2)
     1        then
          inc1    = index(cdat(inc4+1:knc),',')
          if (inc1 .ne. 0) inc1 = inc1 + inc4
      endif
      if (inc1 .eq. 0 .or. (inc2 .ne. 0 .and. inc2 .lt. inc1))
     1        inc1 = inc2
      inc2   = kst    + inc1
c
c...DOS
c
      if (inc2 .le. kst) go to 9000
c
c...Break out parameter
c
      call remspc (cdat(kst+1:inc2-1),cout,knco)
      kst    = inc2
c
c...End of routine
c
 8000 return
c
c...No more parameters
c
 9000 cout   = ' '
      knco   = 0
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pdfgvl (kary,kval)
c
c   FUNCTION:  This routine returns the value for a post variable parsed
c              using the pdfrdf routine.
c
c              [%post-var]
c
c   INPUT:  kary    I*4  D3  -  Post variable portion of array returned
c                               from 'pdfrdf'.
c
c   OUTPUT: kval    I*4  D1  -  Value held in the post variable described
c                               in 'kary'.  Returns 1 if there is not a
c                               valid variable specified.
c
c***********************************************************************
c
      subroutine pdfgvl (kary,gval)
c
      include 'post.inc'
c
      integer*4 kary(3)
c
      real*8 gval
c
      integer*4 isub
c
c...Get post variable's value
c
      if (kary(1) .eq. 3) then
          if (kary(2) .lt. 0) then
              gval   = KPOSMP(-kary(2))
          else
              gval   = POSMAP(kary(2))
          endif
c
c......Subscripted Post variable
c
      else if (kary(1) .eq. 4) then
          if (kary(3) .lt. 0) then
              isub   = KPOSMP(-kary(3))
          else
              isub   = POSMAP(kary(3))
          endif
c
          if (kary(2) .lt. 0) then
              gval   = KPOSMP(-kary(2)+isub-1)
          else
              gval   = POSMAP(kary(2)+isub-1)
          endif
c
c......Single time
c
      else
          gval   = 1.
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pdfrdf (cdat,knc,kary,kerr)
c
c   FUNCTION:  This routine parses the options for RECORD and LINE PDF
c              records.  The input format is as follows (all parameters
c              are optional).
c
c              [PAGE,BEFORE,AFTER,%post-var]
c
c   INPUT:  cdat    C*n  D1  -  Input line.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c   OUTPUT: kary    I*4  D10 -  RECORD/LINE descriptor array.  (1) =
c                               PAGE, (2) = 1 - BEFORE, 2 = AFTER, 3-6 =
c                               Variable descriptor.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine pdfrdf (cdat,knc,kary,kerr)
c
      include 'menu.inc'
c
      integer*4 knc,kary(6),kerr
c
      character*(*) cdat
c
      integer*4 i,ist,inc,nc,inum,ierr,index
c
      character*512 lbuf
c
c...Initialize routine
c
      kerr   = 0
      do 80 i=1,6,1
          kary(i) = 0
   80 continue
      if (knc .eq. 0) go to 8000
      ist    = 1
c
c...Get next parameter
c
  100 call pdfbrk (cdat,ist,knc,lbuf,nc)
      if (nc .eq. 0) go to 9000
      inc    = index(lbuf,'(') - 1
      if (inc .lt. 0) inc = nc
      call getvnm (lbuf(1:inc),inum,PSTWRD,PSTWVL,NPSTWD)
c
c......PAGE
c
      if (inum .eq. 5021) then
          kary(1) = 1
c
c......BEFORE
c
      else if (inum .eq. 5022) then
          kary(2) = 1
c
c......AFTER
c
      else if (inum .eq. 5023) then
          kary(3) = 1
c
c......Post Variable
c
      else if (inum .gt. 6000) then
          call pdfvar (lbuf,nc,inum,kary(4),ierr)
          if (ierr .eq. 1) go to 9000
c
c......Unrecognized
c
      else
          go to 9000
      endif
c
c...Check for next variable descriptor
c
      if (ist .gt. knc .or. (ist .eq. knc .and. cdat(ist:ist) .eq. ']'))
     1        go to 8000
      go to 100
c
c...End of routine
c
 8000 return
c
c...Invalid record
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pdfvar (cdat,knc,kvar,kary,kerr)
c
c   FUNCTION:  This routine parses a PDF Post Variable descriptor.  The
c              input format is as follows (the subscripts are optional,
c              but if a second subscript is specified, then it must be
c              a simple numeric subscript.
c
c              %var(sub1(sub2))
c
c   INPUT:  cdat    C*n  D1  -  Input line.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c           kvar    I*4  D1  -  Post variable number (6001+).
c
c   OUTPUT: kary    I*4  D3  -  (1) = 3 - Non-subscripter variable.  4 -
c                               Subscripted variable.  (2) = Post vari-
c                               able position in POSMAP when positive or
c                               KPOSMP when negative. (3) = Subscript
c                               variable position in POSMAP/KPOSMP.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine pdfvar (cdat,knc,kvar,kary,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
c
      integer*4 kvar,knc,kary(10),kerr
c
      character*(*) cdat
c
      integer*4 ist,inc,inc1,inc2,inc3,ipt,ierr,index,jindex,inum,
     1          isz(2)
c
c...All multidimensional arrays
c...are (6,3) at this time
c...(SGM variables)
c
      data isz /6,3/
c
c...Store variable position
c
      kerr   = 0
      if (kvar .eq. 6000 .or. kvar .eq. 6002) go to 9000
      ipt    = jindex(KPSTVR,kvar-6000,MAXKVR)
      if (ipt .ne. 0) then
          kary(2) = 0 - KPSTVL(ipt)
      else
          ipt    = jindex(RPSTVR,kvar-6000,MAXRVR)
          if (ipt .eq. 0) go to 9000
          kary(2) = RPSTVL(ipt)
      endif
c
c...Check for subscript
c
      ist    = index(cdat,'(')
c
c......No subscript
c
      if (ist .eq. 0) then
          kary(1) = 3
c
c......At least one subscript
c
      else
          inc1   = index(cdat(ist+1:knc),'(')
          inc2   = index(cdat(ist+1:knc),')')
          inc3   = index(cdat(ist+1:knc),',')
          if (inc2 .eq. 0) go to 9000
          if (inc3 .ne. 0 .and. inc3 .lt. inc2) then
              inum    = inc2
              inc2    = inc3
              inc3    = inum
          else
              inc3    = 0
          endif
          inc    = inc1
          if (inc2 .lt. inc1 .or. inc1 .eq. 0) inc = inc2
c
c.........Check for numeric value
c
          inc    = ist    + inc    - 1
          call ctoi (cdat(ist+1:inc),inum,ierr)
          if (ierr .eq. 0) then
              if (inum .le. 0) go to 9000
              if (inc1 .ne. 0) go to 9000
              if (inc3 .ne. 0) then
                  call ctoi (cdat(ist+inc2+1:ist+inc3-1),inc1,ierr)
                  if (ierr .ne. 0) go to 9000
                  inum   = (inc1-1)*isz(1) + inum
              endif
              kary(1) = 3
              if (kary(2) .lt. 0) then
                  if (inum .gt. KPSTSB(ipt)) go to 9000
                  kary(2) = kary(2) - inum + 1
              else
                  if (inum .gt. RPSTSB(ipt)) go to 9000
                  kary(2) = kary(2) + inum - 1
              endif
c
c.........Check for *
c
         else if (cdat(ist+1:inc) .eq. '*') then
               kary(1) = 4
               kary(3) = 0
c
c.........Check for variable name
c
          else
              call getvnm (cdat(ist+1:inc),inum,PSTWRD,PSTWVL,NPSTWD)
              if (inum .le. 6000 .or. inum .eq. 6002) go to 9000
              kary(1) = 4
              ipt    = jindex(KPSTVR,inum-6000,MAXKVR)
              if (ipt .ne. 0) then
                  kary(3) = 0 - KPSTVL(ipt)
              else
                  ipt    = jindex(RPSTVR,inum-6000,MAXRVR)
                  if (ipt .eq. 0) go to 9000
                  kary(3) = RPSTVL(ipt)
              endif
c
c............Double subscript
c
              if (inc1 .ne. 0) then
                  inc    = ist    + inc2   - 1
                  call ctoi (cdat(ist+inc1+1:inc),inum,ierr)
                  if (inum .le. 0 .or. ierr .ne. 0) go to 9000
                  inc    = index(cdat(inc+2:knc),')')
                  if (inc .eq. 0) go to 9000
                  if (kary(3) .lt. 0) then
                      if (inum .gt. KPSTSB(ipt)) go to 9000
                      kary(3) = kary(3) - inum + 1
                  else
                      if (inum .gt. RPSTSB(ipt)) go to 9000
                      kary(3) = kary(3) + inum - 1
                  endif
              endif
          endif
      endif
c
c...End of routine
c
 8000 return
c
c...Error processing variable name
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pdfvdf (cdat,knc,krec,klin,knvar,kerr)
c
c   FUNCTION:  This routine parses the Variable descriptor record.  The
c              input format is as follows (all parameters are optional).
c
c              [var,start,end,reg,RIGHT ,>]
c                                 LEFT   <
c                                 CENTER *
c                                        ~
c
c              The descriptor array will be stored in IPRTXT, with the
c              elements having the following values.
c
c                   (1-3) = Variable description as returned from
c                           'pdfwrd'.
c                   (4) = Starting column of variable in output line.
c                   (5) = Ending column.
c                   (6) = Register to use for format descriptor.
c                   (7) = Text position justification, 1 = RIGHT, 2 =
c                         LEFT, 3 = Center.
c                   (8) = Overflow control.  1 = > (drop right side),
c                         2 = < (drop left side), 3 = * (fill with *'s)
c                         4 = ~ (continue on next line).
c                   (9) = Not used.
c                  (10) = Line number for this variable.
c
c   INPUT:  cdat    C*n  D1  -  Input line.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c           krec    I*4  D1  -  Current record number.
c
c           klin    I*4  D1  -  Current line number.
c
c           knvar   I*4  D1  -  Number variables defined for this re-
c                               cord.
c
c   OUTPUT: knvar   I*4  D1  -  See INPUT.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine pdfvdf (cdat,knc,krec,klin,knvar,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      integer*4 knc,krec,klin,knvar,kerr
c
      character*(*) cdat
c
      integer*4 i,j,k,ist,inc,ipt,nc,inum,iary(10),ierr,index
c
      real*8 rnum
c
      character*80 lbuf
c
c...Initialize routine
c
      kerr   = 0
      if (knc .eq. 0) go to 8000
      ist    = 1
c
c...Variable name
c
  100 call pdfbrk (cdat,ist,knc,lbuf,nc)
      if (nc .eq. 0) go to 9000
      call pdfwrd (lbuf,nc,iary,ierr)
      if (ierr .eq. 1) go to 9000
c
c...Starting column
c
      call pdfbrk (cdat,ist,knc,lbuf,nc)
      call ctoi (lbuf(1:nc),inum,ierr)
      if (ierr .eq. 1) go to 9000
      iary(4) = inum
c
c...Ending column
c
      call pdfbrk (cdat,ist,knc,lbuf,nc)
      call ctoi (lbuf(1:nc),inum,ierr)
      if (ierr .eq. 1) go to 9000
      iary(5) = inum
c
c...Format
c
      call pdfbrk (cdat,ist,knc,lbuf,nc)
      call ctocd (lbuf,nc,inum,rnum,ierr)
      if (ierr .eq. 1 .or. inum .le. 0 .or. rnum .ne. DUMMY) go to 9000
      iary(6) = inum
c
c...Justification
c
      call pdfbrk (cdat,ist,knc,lbuf,nc)
      call getvnm (lbuf(1:nc),inum,PSTWRD,PSTWVL,NPSTWD)
c
c......RIGHT
c
      if (inum .eq. 24) then
          iary(7) = 1
c
c......LEFT
c
      else if (inum .eq. 8) then
          iary(7) = 2
c
c......CENTER
c
      else if (inum .eq. 2) then
          iary(7) = 3
c
c......Unrecognized
c
      else
          go to 9000
      endif
c
c...Overflow
c
      call pdfbrk (cdat,ist,knc,lbuf,nc)
      if (nc .ne. 1) go to 8000
      if (lbuf .eq. '>') then
          iary(8) = 1
      else if (lbuf .eq. '<') then
          iary(8) = 2
      else if (lbuf .eq. '*') then
          iary(8) = 3
      else if (lbuf .eq. '~') then
          iary(8) = 4
      else
          go to 9000
      endif
c
c...Store descriptor
c
      if (knvar .ge. 15) go to 9000
      if (cdat(ist:ist) .ne. ']') go to 9000
      iary(10) = klin
      ipt    = (krec-1) * 15 * 10
      do 800 i=1,knvar,1
          if (iary(10) .lt. IPRTXT(ipt+10)) go to 900
          ipt    = ipt    + 10
  800 continue
      go to 1200
c
  900 inc    = (krec-1) * 15 * 10 + (knvar-1) * 10
      do 1100 j=knvar,i,-1
          do 1000 k=1,10,1
              IPRTXT(inc+j+10) = IPRTXT(inc+j)
 1000     continue
          inc    = inc    - 10
 1100 continue
c
 1200 do 1300 i=1,10,1
          IPRTXT(ipt+i) = iary(i)
 1300 continue
      knvar  = knvar  + 1
c
c...Check for next variable descriptor
c
      if (ist .gt. knc) go to 8000
      inc    = index(cdat(ist:knc),'[')
      if (inc .eq. 0) go to 8000
      ist    = ist    + inc    - 1
      go to 100
c
c...End of routine
c
 8000 return
c
c...Invalid record
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pdfwrd (cbuf,knc,kary,kerr)
c
c   FUNCTION:  This routine parses a PDF/PHF Variable name.
c
c              The descriptor array will be stored in 'kary', with the
c              elements having the following values.
c
c                   (1) = 1     (2) = 1 - TAPEBLK, 2 - DATE, 3 - TIME,
c                                     4 - COMMAND, 5 - PCHFILE,
c                                     6 - PARTNO, 7 - RECAP, 8 - PPRINT,
c                                     9 - CLNAME, 10 - CLDATE,
c                                     11 - CLTIME, 12 - REVDATE,
c                                     13 - LASTBLK, 14 - TLNAME,
c                                     15 - CAMNAME, 16 - CAMVERSN,
c                                     17 - PCHBRIEF, 18 - PCHNAME,
c                                     19 - VERSION, 20 - COMPUTER,
c                                     21 - USERNAME
c                         2 - Register.
c                         3 - Post variable (no variable subscript).
c                             (2) = Position within POSMAP when position,
c                                   within KPOSMP when negative.
c                         4 - Post variable w/subscript.
c                             (2) = Position within POSMAP/KPOSMP.
c                             (3) = Subscript variable stored the same as
c                                   (2).
c
c   INPUT:  cbuf    C*n  D1  -  Text of variable name.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: kary    I*4  D3  -  See description above.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine pdfwrd (cbuf,knc,kary,kerr)
c
      include 'post.inc'
      include 'menu.inc'
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      integer*4 knc,kary(3),kerr
c
      character*(*) cbuf
c
      integer*4 NWRD
c
      parameter (NWRD = 23)
c
      integer*4 inc,index,inum,iwrd(NWRD),isub(NWRD),i
c
      real*8 rnum
c
      data iwrd /4021,5016,5017,5018,5024,1045,5025,1044,5032,5033,
     1           5034,5035,5036,5037,5038,5039,5040,5041,5043,5044,
     2           5045,5046, 5047/
      data isub /   0,   0,   0,   0,   0,   0,  32,   0,   0,   0,
     1              0,   0,   0, 120,   0,   0,   0,   0,   0,   0,
     2              0,   0, 100/
c
c...Initialize routine
c
      kerr   = 0
c
c...Get Variable's name
c
      inc    = index(cbuf,'(') - 1
      if (inc .lt. 0) inc = knc
      call getvnm (cbuf(1:inc),inum,PSTWRD,PSTWVL,NPSTWD)
c
c......Check for Print File variable
c
      do 100 i=1,NWRD,1
          if (inum .eq. iwrd(i)) then
              kary(1) = 1
              kary(2) = i
              if (isub(i) .ne. 0) then
c
c.........Get subscript
c
                  if (inc .eq. knc) go to 9000
                  if (cbuf(knc:knc) .ne. ')' .or. inc+2 .gt. knc)
     1                go to 9000
                  if (cbuf(inc+2:knc-1) .eq. '*') then
                      kary(3) = 0
                  else
                      call ctoi (cbuf(inc+2:knc-1),inum,kerr)
                      if (inum .lt. 1 .or. inum .gt. isub(i)) kerr = 1
                      if (kerr .eq. 1) go to 8000
                      kary(3) = inum
                  endif
              endif
          go to 8000
          endif
  100 continue
c
c......Post Variable
c
      if (inum .gt. 6000) then
          call pdfvar (cbuf,knc,inum,kary,kerr)
          if (kerr .eq. 1) go to 8000
c
c......Register
c
      else
          if (cbuf(1:knc) .eq. 'G' .or. cbuf(1:knc) .eq. 'g') then
              inum   = -1
          else if (cbuf(1:knc) .eq. 'M' .or. cbuf(1:knc) .eq. 'm') then
              inum   = -2
          else
              call ctocd (cbuf,knc,inum,rnum,kerr)
              if (rnum .ne. DUMMY) kerr = 1
              if (kerr .eq. 1) go to 8000
          endif
          kary(1) = 2
          kary(2) = inum
      endif
c
c...End of routine
c
 8000 return
c
c...Invalid variable
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pdfwvl (kary,kfrm,cout,knc)
c
c   FUNCTION:  This routine formats a PDF/PHF Variable for output.
c
c   INPUT:  kary    I*4  D3  -  Variable descriptor array as returned
c                               from 'pwdwrd'.
c
c           kfrm    I*4  D1  -  Register format to use when 'kary'
c                               describes a Post Variable.  If 'kfrm' is
c                               negative, then it describes the 'nth'
c                               word of a text variable.  For example,
c                               'kfrm = -2' will return 'the' from the
c                               following text.
c
c                               "Choose the second word."
c
c   OUTPUT: cout    C*n  D1  -  Formatted Post Variable text.
c
c           knc     I*4  D1  -  Number of characters in 'cout'.
c
c***********************************************************************
c
      subroutine pdfwvl (kary,kfrm,cout,knc)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NPARTN,KPOSMP(0075))
      equivalence (IRGOUT,KPOSMP(0695)), (NEOB  ,KPOSMP(0841))
      equivalence (PRTBNC,KPOSMP(1153)), (REGBNC,KPOSMP(2001))
      equivalence (FMTDES,KPOSMP(2133)), (REGENC,KPOSMP(3522))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 IRGOUT(MAXFMT),PRTBNC,REGBNC(MAXFMT),REGENC(MAXFMT),
     1          NEOB,NPARTN
c
      equivalence (MCHDLT,POSMAP(1474)), (ROTDLT,POSMAP(1486))
      equivalence (LINDLT,POSMAP(1490)), (AXSDLT,POSMAP(1496))
      equivalence (MCHMIN,POSMAP(1506)), (MCHMAX,POSMAP(1518))
      equivalence (ROTMIN,POSMAP(1530)), (ROTMAX,POSMAP(1534))
      equivalence (LINMIN,POSMAP(1538)), (LINMAX,POSMAP(1544))
      equivalence (AXSMIN,POSMAP(1550)), (AXSMAX,POSMAP(1560))
      equivalence (REGVAL,POSMAP(5000))
c
      real*8 REGVAL(MAXFMT),MCHDLT(3,4),ROTDLT(4),LINDLT(6),AXSDLT(10),
     1       MCHMIN(3,4),MCHMAX(3,4),ROTMIN(4),ROTMAX(4),LINMIN(6),
     2       LINMAX(6),AXSMIN(10),AXSMAX(10)
c
      equivalence (LPARTN,CPOSMP(0067))
      equivalence (LEOB  ,CPOSMP(0971)), (LSTBLK,CPOSMP(0988))
      equivalence (PRTBLK,CPOSMP(1653)), (MDESC ,CPOSMP(3853))
      equivalence (LPPRIN,CPOSMP(6499))
      equivalence (REGST ,CPOSMP(7011)), (REGEN ,CPOSMP(9219))
c
      character*5 LEOB
      character*24 REGST(MAXFMT),REGEN(MAXFMT)
      character*80 MDESC
      character*66 LPARTN
      character*512 LSTBLK,PRTBLK,LPPRIN
c
      integer*4 kary(10),kfrm,knc
c
      character*(*) cout
c
      integer*4 strlen1,inc,isub,ist,ien,nc,j,nindex,nc1,nc2
c
      real*8 rary(4),rnum
c
      character*20 sb1,sb2
      character*60 rbuf
      character*512 sbuf
      character*(MAX_PATH) ldev
      character*(MAX_FILE) lfil,lext
c
c...Go to appropriate section
c
      go to (200,1000,1100,1100), kary(1)
c
c...Function
c
  200 go to (210,220,230,240,250,260,270,280,290,300,310,320,330,340,
     1       350,360,370,380,390,400,410,420,430), kary(2)
c
c.........TAPEBLK
c
  210 cout   = PRTBLK
      knc    = PRTBNC
      go to 7000
c
c.........DATE
c
  220 cout   = LDATE
      knc    = 11
      go to 7000
c
c.........TIME
c
  230 cout   = LTIME
      knc    = 8
      go to 7000
c
c.........COMMAND
c
  240 cout   = ' '
      knc    = 1
      go to 7000
c
c.........PCHFILE
c
  250 call shfile (PCHFIL,cout,80)
      knc    = strlen1(cout)
      go to 7000
c
c.........PARTNO
c
  260 cout   = LPARTN
      knc    = strlen1(cout)
      go to 7000
c
c.........Recap
c............XYZ Recap
c
  270 inc    = kary(3)
      if (inc .le. 12) then
          isub   = (inc+2) / 3
          ist    = inc    - (isub-1) * 3
          rary(1) = MCHMIN(ist,isub)
          rary(2) = MCHMAX(ist,isub)
          rary(4) = MCHDLT(ist,isub)

c............Linear axis Recap
c
      else if (inc .le. 18) then
          isub   = inc    - 12
          rary(1) = LINMIN(isub)
          rary(2) = LINMAX(isub)
          rary(4) = LINDLT(isub)
c
c............Rotary axis Recap
c
      else if (inc .le. 22) then
          isub    = inc    - 18
          rary(1) = ROTMIN(isub)
          rary(2) = ROTMAX(isub)
          rary(4) = ROTDLT(isub)
c
c............Output axis Recap
c
      else
          isub    = inc    - 22
          rary(1) = AXSMIN(isub)
          rary(2) = AXSMAX(isub)
          rary(4) = AXSDLT(isub)
      endif
c
c............Format Recap buffer
c
      if (rary(2) .lt. rary(1)) then
          rary(1) = 0.
          rary(2) = 0.
      endif
      rary(3) = rary(2) - rary(1)
      ien    = 12
      cout   = ' '
      do 275 j=1,4,1
          if (kfrm .gt. 0) then
              call ftoc (rary(j),sbuf,nc,FMTDES(1,kfrm))
          else
              call rtoc (rary(j),sbuf,nc)
          endif
          if (nc .gt. 12) nc = 12
          ist    = ien    - nc     + 1
          cout(ist:ien) = sbuf(1:nc)
          ien    = ien    + 13
  275 continue
      knc    = 51
      go to 7000
c
c.........PPRINT
c
  280 cout   = LPPRIN
      knc    = strlen1(cout)
      go to 7000
c
c.........CLNAME
c
  290 cout   = CLNAME
      knc    = strlen1(cout)
      go to 7000
c
c.........CLDATE
c
  300 cout   = CLDATE
      knc    = strlen1(cout)
      go to 7000
c
c.........CLTIME
c
  310 cout   = CLTIME
      knc    = strlen1(cout)
      go to 7000
c
c.........REVDATE
c
  320 cout   = REVDAT
      knc    = strlen1(cout)
      go to 7000
c
c.........LASTBLK
c
  330 cout   = LSTBLK
      knc    = strlen1(cout)
      go to 7000
c
c.........TLNAME
c
  340 cout   = TLNAME(kary(3))
      knc    = strlen1(cout)
      go to 7000
c
c.........CAMNAME
c
  350 cout   = CAMNAM
      knc    = strlen1(cout)
      go to 7000
c
c.........CAMVERSN
c
  360 cout   = CAMREV
      knc    = strlen1(cout)
      go to 7000
c
c.........PCHBRIEF
c
  370 call fbreak (PCHFIL,ldev,lfil,lext)
      nc1    = strlen1(lfil)
      nc2    = strlen1(lext)
      ldev   = lfil(1:nc1) // lext(1:nc2)
      call shfile (ldev,cout,80)
      knc    = strlen1(cout)
      go to 7000
c
c.........PCHNAME
c
  380 call fbreak (PCHFIL,ldev,lfil,lext)
      nc1    = strlen1(lfil)
      call shfile (lfil,cout,80)
      knc    = strlen1(cout)
      go to 7000
c
c.........VERSION
c
  390 call itoc (PWVER1,sb1,nc1,1)
      call itoc (PWVER2,sb2,nc2,1)
      cout = sb1(1:nc1) // '.' // sb2(1:nc2)
      knc    = nc1 + nc2 + 1
      go to 7000
c
c......MACHDESC
c
  400 cout   = MDESC
      knc    = strlen1(cout)
      go to 7000
c
c......COMPUTER
c
  410 continue
      call gethst (cout,knc)
      go to 7000
c
c......USERNAME
c
  420 continue
      call getusr (cout,knc)
      go to 7000
c
c.........MPARTNO
c
  430 if (kary(3) .gt. NPARTN) then
          cout   = ' '
      else
          cout   = SFRMBUF(kary(3))
      endif
      knc    = strlen1(cout)
      go to 7000
c
c......Register
c
 1000 inc    = kary(2)
c
c.........End-of-Block
c
      if (inc .eq. -3) then
          cout   = LEOB(1:NEOB)
          knc    = NEOB
          go to 7000
c
c.........All G-codes
c
      else if (inc .eq. -1) then
          ist    = 18
          ien    = 28
c
c.........All M-codes
c
      else if (inc .eq. -2) then
          ist    = 37
          ien    = 47
c
c.........Standard Register
c
      else
          call regtyp (inc,REGVAL(inc))
          ist    = inc
          ien    = inc
      endif
c
c..........Format register(s)
c
      knc    = 0
      do 1020 j=ist,ien,1
          if (IRGOUT(j) .ne. 1) go to 1020
          call ftoc (REGVAL(j),rbuf,inc,FMTDES(1,j))
c
          if (REGBNC(j) .ne. 0) then
              sbuf   = REGST(j)(1:REGBNC(j)) // rbuf(1:inc)
              rbuf   = sbuf
              inc    = inc    + REGBNC(j)
          endif
c
          if (REGENC(j) .ne. 0) then
              sbuf   = rbuf(1:inc) // REGEN(j)(1:REGENC(j))
              rbuf   = sbuf
              inc    = inc    + REGENC(j)
          endif
c
          if (knc .eq. 0) then
              cout   = rbuf
              knc    = inc
          else
              sbuf   = cout(1:knc) // ' ' // rbuf(1:inc)
              cout   = sbuf
              knc    = knc    + inc    + 1
          endif
 1020 continue
      go to 7000
c
c......Post variable
c
 1100 call pdfgvl (kary,rnum)
      if (kfrm .gt. 0) then
          call ftoc (rnum,cout,knc,FMTDES(1,kfrm))
      else
          call rtoc (rnum,cout,knc)
      endif
c
c...Break out specific word if required
c
 7000 if (kfrm .lt. 0) then
          if (cout .eq. ' ') go to 8000
          isub   = 1
          ist    = nindex(cout,' ')
 7100     inc    = index(cout(ist:),' ')
          if (inc .eq. 0 .or. ist+inc .ge. knc) then
              sbuf   = cout(ist:)
              cout   = sbuf
              knc    = knc    - ist    + 1
              go to 8000
          else if (inc .eq. 1) then
              ist    = ist    + 1
              go to 7100
          else if (isub .eq. -kfrm) then
              ien    = ist    + inc    - 2
              sbuf   = cout(ist:ien)
              cout   = sbuf
              knc    = ien    - ist    + 1
              go to 8000
          else
              isub   = isub   + 1
              ist    = ist    + inc
              go to 7100
          endif
      endif
c
c...End of routine
c
 8000 return
      end
