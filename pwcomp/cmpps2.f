c
c***********************************************************************
c
c   FILE NAME:  cmpps2
c   CONTAINS:
c               cmpps2  inips2  stovp2  macps2  pttorp  rptopt
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cmpps2.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        06/09/14 , 16:42:11
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cmpps2 (cmsg,kerr)
c
c   FUNCTION:  This routine is the controlling routine for the 2nd pass
c              of compilation.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine cmpps2 (cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
c...No object file so
c...just exit
c
      if (IOPFL(4) .eq. 0) go to 8000
c
c...Initialize 2nd pass compiler
c
      call inips2
c
c...Store variables
c
      call stovp2 (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Store Macro's & code
c
      call macps2 (cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  inips2
c
c   FUNCTION:  This routine initializes the 2nd pass processor.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine inips2
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 nhdr,nchr,inum,irec,ipt
c
      character*8 PFUPD
      equivalence (PFUPD,PFDAT(9,1))
c
c...Initialize Pass 2 variables
c
      IPASS  = 2
      LISLIN = IOPFL(3) + 1
c
c...Initialize size of fixed fields
c
      nhdr   = 20
c
c...Changed NPMAC from 8 to 10 to allow for I*4 PC
c...Bobby  -  9/30/91
c
      NPMAC  = 10
      nchr   = 2
c
c...Get allocation space for
c...Macros, Reals & Chars
c
      PFDAT(1,1) = 1
      PFIDAT(14,1) = nhdr   + 1
      PFIDAT(13,1) = NMACRO
c
      inum   = nhdr   + NMACRO*NPMAC + 1
      call pttorp (inum,irec,ipt)
      if (ipt .ne. 1) then
          irec   = irec + 1
          ipt    = 0
          call rptopt (irec,ipt,inum)
      endif
      PFDAT(2,1) = irec
c
      inum   = inum   + ((ISCAST(1)-1)/4+1) * 4 + 1
      call pttorp (inum,irec,ipt)
      if (ipt .ne. 1) then
          irec   = irec + 1
          ipt    = 0
          call rptopt (irec,ipt,inum)
      endif
      PFDAT(3,1) = irec
c
      inum   = inum   + ISCAST(1)*4
      call pttorp (inum,irec,ipt)
      if (ipt .ne. 1) then
          irec   = irec + 1
          ipt    = 0
          call rptopt (irec,ipt,inum)
      endif
      PFDAT(4,1) = irec
c
      inum   = inum   + NSCAL(2)*nchr + ISCAST(2)/2 + 1
      call pttorp (inum,irec,ipt)
      if (ipt .ne. 1) then
          irec   = irec + 1
          ipt    = 0
          call rptopt (irec,ipt,inum)
      endif
      PFDAT(5,1) = irec
c
      inum   = inum   + IPC
      call pttorp (inum,irec,ipt)
      PFDAT(6,1) = irec
c
c...Save Revision date to object file
c
      PFUPD  = REVDAT
c
c...Initialize record pointers
c
      PFPT(1,1) = PFDAT(1,1)
      PFPT(2,1) = PFIDAT(14,1) - 1
c
      PFPT(1,2) = PFDAT(2,1)
      PFPT(2,2) = 0
c
      PFPT(1,3) = PFDAT(3,1)
      PFPT(2,3) = 0
c
      PFPT(1,4) = PFDAT(4,1)
      PFPT(2,4) = 0
c
      PFPT(1,5) = PFDAT(5,1)
      PFPT(2,5) = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  stovp2 (cmsg,kerr)
c
c   FUNCTION:  This routine stores the Real & Character variables in the
c              object file.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine stovp2 (cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 i,j,ipt,isub
c
c...Store REAL variables
c
      do 500 i=1,NSCAL(1),1
c
c.....Load next variable
c
          call lodscl (1,i,ipt,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c......Store array size
c
          isub   = MFDAT(ipt+6,3)
          do 100 j=1,MFDAT(ipt+6,3),1
              if (PFPT(2,2) .eq. 256) then
			        kerr = PFDAT(1,2)
                  call wrprm (LUNSC3,PFPT(1,2),PFDAT(1,2),cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  PFPT(1,2) = PFPT(1,2) + 1
                  PFPT(2,2) = 0
              endif
              PFPT(2,2) = PFPT(2,2) + 1
              PFIDAT(PFPT(2,2),2) = isub
              isub   = isub   - 1
  100     continue
c
c......Set variable to zero
c
          do 200 j=1,MFDAT(ipt+6,3),1
              if (PFPT(2,3) .eq. 64) then
                  call wrprm (LUNSC3,PFPT(1,3),PFDAT(1,3),cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  PFPT(1,3) = PFPT(1,3) + 1
                  PFPT(2,3) = 0
              endif
              PFPT(2,3) = PFPT(2,3) + 1
              PFRDAT(PFPT(2,3),3) = 0.
  200     continue
  500 continue
c
c...Write last REAL array record
c
      if (PFPT(2,2) .ne. 0) then
          call wrprm (LUNSC3,PFPT(1,2),PFDAT(1,2),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Write last REAL variable record
c
      if (PFPT(2,3) .ne. 0) then
          call wrprm (LUNSC3,PFPT(1,3),PFDAT(1,3),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Store CHAR variables
c
      do 1000 i=1,NSCAL(2),1
c
c.....Load next variable
c
          call lodscl (2,i,ipt,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c......Store array size
c
          if (PFPT(2,4) .eq. 256) then
              call wrprm (LUNSC3,PFPT(1,4),PFDAT(1,4),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              PFPT(1,4) = PFPT(1,4) + 1
              PFPT(2,4) = 0
          endif
          PFPT(2,4) = PFPT(2,4) + 1
          PFIDAT(PFPT(2,4),4) = MFDAT(ipt+6,4)
c
c......Set # of chars to zero
c
          if (PFPT(2,4) .eq. 256) then
              call wrprm (LUNSC3,PFPT(1,4),PFDAT(1,4),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              PFPT(1,4) = PFPT(1,4) + 1
              PFPT(2,4) = 0
          endif
          PFPT(2,4) = PFPT(2,4) + 1
          PFIDAT(PFPT(2,4),4) = 0
c
c......Point to next character array
c
          PFPT(2,4) = PFPT(2,4) + MFDAT(ipt+6,4)/2
          do while (PFPT(2,4) .gt. 256)
              call wrprm (LUNSC3,PFPT(1,4),PFDAT(1,4),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              PFPT(1,4) = PFPT(1,4) + 1
              PFPT(2,4) = PFPT(2,4) - 256
          end do
 1000 continue
c
c...Write last CHAR variable record
c
      if (PFPT(2,4) .ne. 0) then
          call wrprm (LUNSC3,PFPT(1,4),PFDAT(1,4),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  macps2 (cmsg,kerr)
c
c   FUNCTION:  This routine stores the Macro names and compiled code in
c              the object file.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine macps2 (cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*2 idat(10)
c
      integer*4 i,j,nc,jdat(5),ist,ipcs,ipt,iterm,ica,icb,icc,icd,
     -          ic5,ic6,ic7,ic8
c
      character*24 lab
c
      equivalence (idat,jdat)
c
c...Initialize routine
c
      MFDAT(2,1) = MFPT(1,1)
      IPC    = 0
      IMACPT = 128
c
c...Get next Macro record
c
      do 1000 i=1,NMACRO,1
c
c......Read Macro index record
c
          if (IMACPT .gt. 120) then
              call lodwrk (MFDAT(2,1),MFDAT(1,1),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              IMACPT = MNFIX(1) + 1
          endif
          NLABEL = MFDAT(IMACPT+2,1)
          call getvwd (MFDAT(IMACPT,1),LMACRO,nc,1,PSTWRD,PSTWVL,NPSTWD)
c
c......No object file
c......So skip storage logic
c
          if (IOPFL(4) .eq. 0) go to 150
c
c......Store Macro name
c
          jdat(1) = MFDAT(IMACPT,1)
c
c......Changed order of 'idat' array
c......to allow for I*4 pointers
c......Bobby  -  9/30/91
c
           jdat(2) = IPC
           jdat(3) = MFDAT(IMACPT+10,1)
           jdat(4) = MFDAT(IMACPT+12,1)
           idat(9) = 1
           idat(10) = MFDAT(IMACPT+11,1)
c
c          idat(3) = 1
c          idat(4) = IPC
c          idat(5) = MFDAT(IMACPT+11,1)
c          idat(6) = MFDAT(IMACPT+10,1)
c          idat(7) = MFDAT(IMACPT+12,1)
c
          do 100 j=1,NPMAC,1
              if (PFPT(2,1) .eq. 256) then
				  ica = pfdat(1,1)
				  icb = pfdat(2,1)
				  icc = pfdat(3,1)
				  icd = pfdat(4,1)
                  call wrprm (LUNSC3,PFPT(1,1),PFDAT(1,1),cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  PFPT(1,1) = PFPT(1,1) + 1
                  PFPT(2,1) = 0
              endif
c
              PFPT(2,1) = PFPT(2,1) + 1
              PFIDAT(PFPT(2,1),1) = idat(j)
  100     continue
c
c......Store compiled Macro statements
c......Load 1st record
c
  150     if (MFDAT(1,5) .ne. MFDAT(IMACPT+7,1)) then
              call lodwrk (MFDAT(IMACPT+7,1),MFDAT(1,5),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
          ist    = MFDAT(IMACPT+8,1)
          ipcs   = IPC
          iterm  = 0
c
c.........Load compiled record
c
  200     call lodcmp (ist,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c........JUMPTO/IF
c........Get label address
c
          if (ICMPL(2) .eq. 4 .or. ICMPL(2) .eq. 5) then
              if (ICMPL(2) .eq. 4) then
                  lab    = LCMPL(5:28)
              else
                  lab    = LCMPL(9:32)
              endif
              call getlab (lab,ipt,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              if (ipt .eq. 0) go to 600
c
              if (ICMPL(2) .eq. 4) then
                  JCMPL(2) = MFDAT(ipt+6,2)
              else
                  JCMPL(3) = MFDAT(ipt+6,2)
              endif
c
c.........TERMAC
c
          else if (ICMPL(2) .eq. 6) then
c
c............Write listing record
c
              call lstps2 (LMACRO,ipcs,IPC,MFDAT(IMACPT+11,1),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              iterm  = 1
          endif
c
c......No object file
c......So skip storage logic
c
  500     if (IOPFL(4) .eq. 0) go to 570
c
c...Store compiled record
c
          do 550 j=1,ICMPL(1),1
              if (PFPT(2,5) .eq. 256) then
				  ica = pfdat(1,1)
				  icb = pfdat(2,1)
				  icc = pfdat(3,1)
				  icd = pfdat(4,1)
                  call wrprm (LUNSC3,PFPT(1,5),PFDAT(1,5),cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  PFPT(1,5) = PFPT(1,5) + 1
                  PFPT(2,5) = 0
              endif
c
              PFPT(2,5) = PFPT(2,5) + 1
              PFIDAT(PFPT(2,5),5) = ICMPL(j)
  550     continue
c
c.........Go get next record
c
  570     IPC    = IPC    + ICMPL(1)
          if (iterm .eq. 1) go to 900
          go to 200
c
c.........Undefined label
c
  600     if (lab(1:3) .ne. 'QZ$') then
              call errtxt ('UNDEFLAB',cmsg)
              call lsterr (lab,cmsg,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
          go to 500
c
c........End of loop
c
  900     IMACPT = IMACPT + MNREC(1)
 1000 continue
c
c...Write last MACRO record
c
      if (PFPT(2,1) .ne. 0) then
				  ica = pfdat(1,1)
				  icb = pfdat(2,1)
				  icc = pfdat(3,1)
				  icd = pfdat(4,1)
				  ic5 = pfdat(5,1)
				  ic6 = pfdat(6,1)
				  ic7 = pfdat(7,1)
				  ic8 = pfdat(8,1)
          call wrprm (LUNSC3,PFPT(1,1),PFDAT(1,1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Write last CODE record
c
      if (PFPT(2,5) .ne. 0) then
          call wrprm (LUNSC3,PFPT(1,5),PFDAT(1,5),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pttorp (knum,krec,kpt)
c
c   FUNCTION:  This routine converts an absolute pointer to a record
c              number and pointer within the record, based on 256 I*2's
c              per record.
c
c   INPUT:  knum    I*4  D1  Absolute pointer inside array.
c
c   OUTPUT: krec    I*4  D1  Record number that contains the array ele-
c                            ment pointed to by 'knum'.
c
c           kpt     I*4  D1  Pointer within 'krec' record of array ele-
c                            ment.
c
c***********************************************************************
c
      subroutine pttorp (knum,krec,kpt)
c
      integer*4 knum,krec,kpt
c
c...Convert absolute number to
c...Record number & pointer within record
c
      krec   = (knum-1) / 256 + 1
      kpt    = knum   - (krec-1)*256
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rptopt (krec,kpt,knum)
c
c   FUNCTION:  This routine converts a record number and pointer within
c              the record to an absolute pointer, based on 256 I*2's
c              per record.
c
c   INPUT:  krec    I*4  D1  Record number that contains the array ele-
c                            ment pointed to by 'knum'.
c
c           kpt     I*4  D1  Pointer within 'krec' record of array ele-
c                            ment.
c
c   OUTPUT: knum    I*4  D1  Absolute pointer inside array.
c
c***********************************************************************
c
      subroutine rptopt (krec,kpt,knum)
c
      integer*4 knum,krec,kpt
c
c...Convert absolute number to
c...Record number & pointer within record
c
      knum   = (krec-1) * 256 + kpt
      return
      end
