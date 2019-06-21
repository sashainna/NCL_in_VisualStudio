c
c***********************************************************************
c
c   FILE NAME:  pdfgbl
c   CONTAINS:
c               pdftyp  pdfdsc pwg_mdfdsc  mdfdsc
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pdfgbl.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:18
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  pdftyp (cbuf,knc,cout,knco,ktyp,knum,kcal)
c
c   FUNCTION:  This routine determines the PDF record type.  Valid types
c              are as follows.
c
c              #RECORD n#  = New record definition.
c              #LINE n#    = New line definition for current record.
c              /TEXT/      = Text of current line.
c              /VARS/      = Variable definitions for current line.
c              ~text~      = Text continuation.
c              [...]       = Variable definitions continuation.
c
c   INPUT:  cbuf    C*n  D1  -  Input line.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c           kcal    I*4  D1  -  1 = Parse '.pdf' record.  2 = Parse
c                               '.phf' record.
c
c   OUTPUT: cout    C*n  D1  -  Remainder of text after record defini-
c                               tion.  The delimiting ~'s will be re-
c                               moved on TEXT definition lines.
c
c           knco    I*4  D1  -  Number of characters in 'cout'.
c
c                                         PDF Record Types
c                               ----------------------------------------
c           ktyp    I*4  D1  -  -1 = Error, 0 = Blank line, 1 = RECORD,
c                               2 = LINE, 3 = TEXT, 4 = VARS, 5 = Text
c                               continuation, 6 = Variable definition
c                               continuation.
c
c                                         PHF Record Types
c                               ----------------------------------------
c                               100 = Text record, 101 = HEADER, 102 =
c                               TRAILER, 103 = LOOPST, 104 = LOOPND.
c
c           knum    I*4  D1  -  Number of RECORD or LINE definition.
c
c***********************************************************************
c
      subroutine pdftyp (cbuf,knc,cout,knco,ktyp,knum,kcal)
c
      include 'menu.inc'
c
      integer*4 ktyp,knum,knc,knco,kcal
c
      character*(*) cbuf,cout
c
      integer*4 ist,inc,ien,index,nindex,nrndex,rindex,inum,ierr,ipt1,
     1          ipt2,nc
c
      character*20 ldat
c
c...Initialize routine
c
      ktyp   = 0
c
c...Get 1st delimiter type
c
      ist    = nindex(cbuf,' ')
      if (ist .eq. 0) go to 8000
c
c...New definition record
c
      if (cbuf(ist:ist) .eq. '#' .or. cbuf(ist:ist) .eq. '/') then
c
c......Break out record type
c
          inc    = ist    + nindex(cbuf(ist+1:knc),' ')
          ipt1   = index(cbuf(inc:knc),' ')
          ipt2   = index(cbuf(inc:knc),cbuf(ist:ist))
          if (ipt1 .eq. 0 .or. (ipt2 .ne. 0 .and. ipt2 .lt. ipt1))
     1            ipt1 = ipt2
          ien    = inc    + ipt1   - 2
          call getvnm (cbuf(inc:ien),inum,MENWRD,MENWVL,NMENWD)
c
c......PDF Style record
c
         if (kcal .eq. 1) then
c
c.........Break out record number
c
              inc    = ien    + index(cbuf(ien+1:knc),cbuf(ist:ist)) - 1
              if (inc .lt. ien) go to 9000
              call remspc (cbuf(ien+1:inc),ldat,nc)
              if (nc .eq. 0) then
                  knum   = 0
              else
                  call ctoi (ldat(1:nc),knum,ierr)
                  if (ierr .eq. 1) go to 9000
              endif
c
c.........Store rest of record
c
              ist     = inc    + nindex(cbuf(inc+2:knc),' ') + 1
              if (ist .le. inc+1) then
                  cout   = ' '
                  knco   = 0
              else if (cbuf(ist:ist) .eq. '~') then
                  ien    = ist    + rindex(cbuf(ist+1:knc),'~')
                  if (ien .le. ist) go to 9000
                  cout   = cbuf(ist+1:ien-1)
                  knco   = ien    - ist    - 1
              else
                  cout   = cbuf(ist:knc)
                  knco   = knc    - ist    + 1
              endif
c
c.........Set type of record
c
              if (inum .eq. 1012) then
                  ktyp   = 1
              else if (inum .eq. 1015) then
                  ktyp   = 2
              else if (inum .eq. 1013) then
                  ktyp   = 3
              else if (inum .eq. 1014) then
                  ktyp   = 4
              else
                  go to 9000
              endif
c
c......PHF Style Record
c......Set type of record
c
          else if (kcal .eq. 2) then
              if (inum .eq. 1022) then
                  ktyp   = 101
              else if (inum .eq. 1023) then
                  ktyp   = 102
c
c.........LOOPND
c.........Store rest of record
c
              else if (inum .eq. 1024) then
                  ktyp   = 103
                  inc    = ien    + nindex(cbuf(ien+2:knc),' ') + 1
                  knco   = 0
                  if (inc .lt. knc) then
                      cout   = cbuf(inc:knc)
                      knco   = knc    - inc    + 1
                  endif
              else if (inum .eq. 1025) then
                  ktyp   = 104
              else
                  ktyp   = 100
              endif
c
c......Unknown parsing mode
c
          else
              go to 9000
          endif
c
c...PHF Text record
c
      else if (kcal .eq. 2) then
          ktyp   = 100
          go to 8000
c
c...TEXT continuation record
c
      else if (cbuf(ist:ist) .eq. '~') then
          ien    = ist    + rindex(cbuf(ist+1:knc),'~')
          if (ien .le. ist) go to 9000
          cout   = cbuf(ist+1:ien-1)
          knco   = ien    - ist    - 1
          ktyp   = 5
c
c...VARS continuation record
c
      else if (cbuf(ist:ist) .eq. '[') then
          ien    = ist    + nrndex(cbuf(ist:knc),' ') - 1
          if (ien .le. ist) go to 9000
          cout   = cbuf(ist:ien)
          knco   = ien    - ist    + 1
          ktyp   = 6
c
c...Unknown record
c
      else
          go to 9000
      endif
c
c...End of routine
c
 8000 return
c
c...Error parsing record
c
 9000 ktyp   = -1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pdfdsc (kpdf,khed,cout,knc)
c
c   FUNCTION:  Obtains the print file header record from the supported
c              .PDF file which.
c
c   INPUT:  kpdf    I*4  D1  -  PDF file number.
c
c           khed    I*4  D1  -  Header record number in PDF file.
c
c   OUTPUT: cout    C*80 D1  -  Machine description text string.
c
c           knc     I*4  D1  -  Number of chars in 'cout'.
c
c***********************************************************************
c
      subroutine pdfdsc (kpdf,khed,cout,knc)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      integer*4 kpdf,khed,knc
c
      character*(*) cout
c
      integer*4 strlen1,nc,irecl,ierr,ifnd,inum,ityp,nct,nct1,inc
c
      character*20 lnum,att(4)
      character*80 msg,ltxt1,lbuf
      character*132 ltxt
      character*(MAX_PATH) fnam
c
c...Open PDF file
c
      call itoc (kpdf,lnum,nc,0)
      fnam   = 'pworks_' // lnum(1:nc) // '.pdf'
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'old'
      irecl  = 80
      call opnfil (LUNSC2,fnam,att,irecl,msg,ierr)
c
c......Local file does not exist
c......try global file
c
      if (ierr .eq. -2) then
          call fparse (fnam,fnam,DVDATA,'.pdf')
          call opnfil (LUNSC2,fnam,att,irecl,msg,ierr)
      endif
      if (ierr .ne. 0) go to 9000
c
c...File exists
c...Search for first text record
c
      ifnd   = 0
  100 call rdtxt (LUNSC2,lbuf,msg,ierr)
      if (ierr .ne. 0) go to 9000
c
c......Parse input record
c
      nc     = strlen1(lbuf)
      call pdftyp (lbuf,nc,ltxt,nct,ityp,inum,1)
c
c.........Record number
c.........Make sure it's the header record
c
      if (ityp .eq. 1 .and. inum .eq. khed) then
          ifnd   = 1
c
c.........TEXT statement within correct record number
c.........Check for text continuation record
c
      else if (ityp .eq. 3 .and. ifnd .eq. 1) then
          call rdtxt (LUNSC2,lbuf,msg,ierr)
          if (ierr .eq. 0) then
              nc     = strlen1(lbuf)
              call pdftyp (lbuf,nc,ltxt1,nct1,ityp,inum,1)
              if (ityp .eq. 5) then
                  ltxt   = ltxt(1:nct)  // ltxt1
                  nct    = nct    + nct1
              endif
          endif
          inc    = index(ltxt,'     ')
          if (inc .eq. 0) inc = nct1    + 1
          if (inc .eq. 1) go to 9000
          inc    = inc    - 1
          cout   = ltxt(1:inc)
          knc    = inc
          go to 8000
      endif
      go to 100
c
c...End of routine
c
 8000 call clsfil (LUNSC2)
      return
c
c...Could not find header text
c
 9000 cout   = ' '
      knc    = 0
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwg_mdfdsc (cfil,cout)
c
c   FUNCTION:  C-callable routine to return the MDF description.
c
c   INPUT:  cfil    B*1  Dn  -  MDF file name.
c
c   OUTPUT: cout    B*1  D41 -  Machine description text string.
c
c***********************************************************************
c
      subroutine pwg_mdfdsc (cfil,cout)
c
      include 'menu.inc'
c
      byte cfil(*),cout(*)
c
      integer*4 nc
c
      character*(MAX_PATH) lfil
      character*80 lout
c
c...Get the Machine Description
c
      call pwdbtc (cfil,lfil,nc)
      call mdfdsc (lfil,lout)
      call pwdctb (lout,cout)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mdfdsc (cfil,cout)
c
c   FUNCTION:  Returns the MDF description.
c
c   INPUT:  cfil    C*n  D1  -  MDF file name.
c
c   OUTPUT: cout    C*n  D1  -  Machine description text string.
c
c***********************************************************************
c
      subroutine mdfdsc (cfil,cout)
c
      include 'menu.inc'
c
      character*(*) cfil,cout
c
      integer*4 kposm(2000),NKPOSM,MCHOPT(20),IPRDES(2,10),ierr,ipdf,
     1          ihed,nmdf,nchk,irecl,nc
c
      character*1 cposm(8000)
      character*8 LUPDAT
      character*20 att(4)
      character*80 MDESC,msg
c
      equivalence (kposm,cposm)
c
      equivalence (NKPOSM,kposm(0171))
      equivalence (MCHOPT,kposm(0308)), (IPRDES,kposm(1154))
c
      equivalence (LUPDAT,cposm(3001)), (MDESC ,cposm(3853))
c
c...Initialize routine
c
      cout   = ' '
c
c...Open input file
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 8000
      call opnfil (LUNSC1,cfil,att,irecl,msg,ierr)
      if (ierr .ne. 0) go to 9000
c
c...Load the first record of Common KPOSMP
c
      call rdcom (LUNSC1,1,kposm,msg,ierr)
      if (ierr .ne. 0) go to 8000
      ipdf   = MCHOPT(3)
      ihed   = IPRDES(1,1)
c
c...Load the first record of Common CPOSMP
c
      call rdcom (LUNSC1,NKPOSM+1,kposm,msg,ierr)
      if (ierr .ne. 0) go to 8000
c
c....Get description from MDF file
c
      call cnvday (LUPDAT,nmdf)
      call cnvday ('04.23.09',nchk)
      if (nmdf .ge. nchk) then
          cout   = MDESC
c
c...Get description from PDF file
c...if older MDF file
c
      else
          call pdfdsc (ipdf,ihed,cout,nc)
      endif
c
c...Return the Machine Description
c
c
c...End of routine
c
 8000 call clsfil (LUNSC1)
 9000 return
      end
