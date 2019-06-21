c
c***********************************************************************
c
c   FILE NAME:  doctab.for
c   CONTAINS:
c               doctab  doctbo
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        doctab.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:55
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  doctab (cmsg,kerr)
c
c   FUNCTION:  This routine outputs the automatic documentation Spindle
c              and Feed Rate tables.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine doctab (cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (NSPRG ,KPOSMP(3101)), (SPNTYP,KPOSMP(3102))
      equivalence (NSPNTB,KPOSMP(3125))
      equivalence (IFDSUP,KPOSMP(3111)), (IFDTYP,KPOSMP(3148))
      equivalence (NFEDTB,KPOSMP(3183))
c
      integer*4 IFDSUP(4),IFDTYP(2),NSPRG,SPNTYP,NSPNTB(3),NFEDTB(2)
c
      equivalence (SPNTCD,POSMAP(3024)), (SPNTVL,POSMAP(3159))
      equivalence (FEDTCD,POSMAP(3330)), (FEDTVL,POSMAP(3420))
c
      real*8 FEDTCD(90),FEDTVL(90),SPNTCD(135),SPNTVL(135)
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 nlev,ilev(10),i,nct,ncs,inc
c
      character*8 lspn(3)
      character*80 sbuf,tbuf
c
      data lspn /'LOW','MEDIUM','HIGH'/
c
c...Get common header lines
c
      nlev   = 2
      ilev(1) = 7
      ilev(2) = 4
      DHED(7) = ' '
      NCHED(7) = 0
      call docprm (DHED(8),NCHED(8),ilev,nlev)
      ilev(2) = 5
      call docprm (DHED(9),NCHED(9),ilev,nlev)
      NDHED  = 9
c
c...Spindle tables
c
      if (SPNTYP .eq. 3) then
c
c......Get header lines
c
          do 500 i=1,3,1
              if ((NSPRG .eq. 1 .and. i .ne. 2) .or. (NSPRG .eq. 2 .and.
     1            i .eq. 2)) go to 500
              nlev   = 1
              if (NSPRG .eq. 1) then
                  call docprm (DHED(1),NCHED(1),ilev,nlev)
              else
                  nlev   = 2
                  ilev(2) = 1
                  call docprm (tbuf,nct,ilev,nlev)
                  call docsap (lspn(i),sbuf,ncs)
                  call touppr (sbuf,sbuf)
                  DHED(1) = sbuf(1:ncs) // ' ' // tbuf(1:nct)
                  NCHED(1) = ncs    + nct    + 1
              endif
              call dcenhd (DHED(1),NCHED(1),DHED(1),NCHED(1))
              DLIN   = 10000
c
c......Output spindle table
c
              inc    = (i-1) * 45 + 1
              call doctbo (SPNTCD(inc),SPNTVL(inc),NSPNTB(i),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
  500     continue
      endif
c
c...Feed rate tables
c
      do 800 i=1,2,1
          if (IFDSUP(i) .eq. 1 .and. IFDTYP(i) .eq. 2) then
c
c......Get header line
c
              nlev   = 2
              ilev(2) = i      + 1
              call docprm (DHED(1),NCHED(1),ilev,nlev)
              call dcenhd (DHED(1),NCHED(1),DHED(1),NCHED(1))
              DLIN   = 10000
c
c......Output feed rate table
c
              inc    = (i-1) * 45 + 1
              call doctbo (FEDTCD(inc),FEDTVL(inc),NFEDTB(i),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
  800 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  doctbo (gcod,gval,kncod,cmsg,kerr)
c
c   FUNCTION:  Machine Configuration menu handling routine.
c
c   INPUT:  gcod    R*8  D45 -  Output values for spindle/feed rate
c                               table.
c
c           gval    R*8  D45 -  Associated spindle/feed rate speed for
c                               output values 'gcod'.
c
c           kncod   I*4  D1  -  Number of entries in table.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine doctbo (gcod,gval,kncod,cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      integer*4 kncod,kerr
c
      real*8 gcod(45),gval(45)
c
      character*(*) cmsg
c
      integer*4 i,inc,ipt(4),nct1,nct2,nco
c
      character*20 t1buf,t2buf
      character*80 obuf
c
      data ipt /1,21,41,61/
c
c...Output predefined table
c
      inc    = 0
      obuf   = ' '
      nco    = 0
      do 100 i=1,kncod,1
          call rtoc (gcod(i),t1buf,nct1)
          call rtoc (gval(i),t2buf,nct2)
          call justr (t1buf,nct1,6)
          call justr (t2buf,nct2,7)
          inc    = inc    + 1
          obuf(ipt(inc):) = t1buf(1:6) // ' = ' // t2buf(1:7)
          nco    = ipt(inc) + 15
          if (inc .eq. 4) then
              call docout (obuf,nco,1,cmsg,kerr)
              obuf   = ' '
              nco    = 0
              inc    = 0
          endif
  100 continue
      if (nco .gt. 0) call docout (obuf,nco,1,cmsg,kerr)
      call docclr (cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
