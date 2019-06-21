c
c***********************************************************************
c
c   FILE NAME: prelst.for
c   CONTAINS:
c               lstdat  lsttxt  lstpwd  lstmsc  lstmot  lstmul  lstunt
c               lstcdi  lststk  lstunk  lsterr  lsthed  lstout
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        prelst.f , 26.2
c     DATE AND TIME OF LAST  MODIFICATION
c        07/23/18 , 14:02:55
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  lstdat (kbuf,gbuf,kwds,cmsg,kerr)
c
c   FUNCTION:  This routine formats and outputs a neutral clfile record
c              to the listing file.
c
c   INPUT:  kbuf    I*4  D4  -  Clfile record integer data.
c
c           gbuf    R*8  Dn  -  Clfile record real data.
c
c           kwds    I*4  D1  -  Number of reals in 'gbuf'.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lstdat (kbuf,gbuf,kwds,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
c
      integer*4 kbuf(4),kwds,kerr
c
      real*8 gbuf(10)
c
      character*(*) cmsg
c
      integer*4 nc,nc1,nc2
c
      character*20 lisn,lclr
      character*512 ldat,tbuf
c
c...Check Listing flag
c
      if (IOPFL(2) .eq. 0 .or. ILSTOP .eq. 0) go to 8000
c
c...Build post word record
c
      if (kbuf(3) .eq. 2000) then
c
c......Text command
c
          if (kbuf(4) .ge. 1043 .and. kbuf(4) .le. 1046 .or.
     -        kbuf(4) .eq. 1199) then
              call lsttxt (kbuf,gbuf,ldat,nc)
c
c......Major/minor post word
c
          else
              call lstpwd (kbuf,gbuf,kwds,ldat,nc)
          endif
c
c...Stock
c...Fixtur
c
      else if (kbuf(3) .eq. 2600 .or. kbuf(3) .eq. 2601) then
          call lststk (kbuf,gbuf,kwds,ldat,nc)
c
c...Circle
c...Cutter
c
      else if (kbuf(3) .eq. 3000 .or. kbuf(3) .eq. 6000) then
          call lstmsc (kbuf,gbuf,kwds,ldat,nc)
c
c...Motion
c
      else if (kbuf(3) .eq. 5000) then
          call lstmot (kbuf,gbuf,kwds,ldat,nc,cmsg,kerr)
          go to 8000
c
c...Positioning record 5210, temporary no listing
c
      else if (kbuf(3) .eq. 5210 .or. kbuf(3) .eq. 5220) then
          go to 8000
c
c...Cutter/Disply
c
      else if (kbuf(3) .eq. 7100) then
          call lstcdi (kbuf,gbuf,kwds,ldat,nc)
c
c...Multax
c
      else if (kbuf(3) .eq. 9000) then
          call lstmul (kbuf,ldat,nc)
c
c...Units
c
      else if (kbuf(3) .eq. 7300) then
          call lstunt (kbuf,ldat,nc)
c
c...Fini
c
      else if (kbuf(3) .eq. 14000) then
          ldat   = 'FINI'
          nc     = 4
c
c...Ignore type 7000-7999 records
c
      else if (kbuf(3) .ge. 7000 .and. kbuf(3) .le. 7999) then
         go to 8000
c
c...Unknown record type
c
      else
          call lstunk (kbuf,ldat,nc)
      endif
c
c...Set up listing record
c
      call itoc (kbuf(1),lisn,nc1,-5)
      call itoc (kbuf(2),lclr,nc2,-5)
      tbuf   = lisn(1:nc1) // '  ' // lclr(1:nc2) // '   ' // ldat(1:nc)
      ldat   = tbuf
      nc     = nc     + nc1    + nc2    + 5
c
c...Write listing record
c
      call lstout (ldat,nc,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lsttxt (kbuf,gbuf,cdat,knc)
c
c   FUNCTION:  This routine formats a Text post command (INSERT,PARTNO,
c              etc.) for printing.
c
c   INPUT:  kbuf    I*4  D4  -  Clfile record integer data.
c
c           gbuf    R*8  Dn  -  Clfile record real data.
c
c   OUTPUT: cdat    C*n  D1  -  Formatted post command.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine lsttxt (kbuf,gbuf,cdat,knc)
c
      include 'menu.inc'
c
      integer*4 kbuf(4),knc
c
      real*8 gbuf(10)
c
      character*(*) cdat
c
      integer*4 i,strlen1
c
      real*8 rbuf(9)
c
      character*66 lbuf
      character*132 tbuf
c
      equivalence (rbuf,lbuf)
c
c...Initialize routine
c
      knc    = 0
c
c...Store major word
c
      call getvwd (kbuf(4),cdat,knc,1,PSTWRD,PSTWVL,NPSTWD)
c
c...Store text data
c
      do 100 i=1,9,1
          rbuf(i) = gbuf(i)
  100 continue
      tbuf   = cdat(1:knc) // lbuf
      cdat   = tbuf
      knc    = knc    + strlen1(lbuf)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lstpwd (kbuf,gbuf,kwds,cdat,knc)
c
c   FUNCTION:  This routine formats a Major/minor post command for
c              printing.
c
c   INPUT:  kbuf    I*4  D4  -  Clfile record integer data.
c
c           gbuf    R*8  Dn  -  Clfile record real data.
c
c           kwds    I*4  D1  -  Number of reals in 'gbuf'.
c
c   OUTPUT: cdat    C*n  D1  -  Formatted post command.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine lstpwd (kbuf,gbuf,kwds,cdat,knc)
c
      include 'menu.inc'
c
      integer*4 kbuf(4),kwds,knc
c
      real*8 gbuf(10)
c
      character*(*) cdat
c
      integer*2 ibuf(4)
      integer*4 inum,nc,ist,is1,is4,k,jbuf(2),ien,inc,i
c
      real*8 rbuf(5)
c
      character*2 ldlm
      character*24 ldat
      character*40 lbuf
      character*512 tbuf
c
      equivalence (rbuf,ibuf,jbuf,lbuf)
C VAX-SUN-SGI-IBM-HPX-START
C      data is1 /1/, is4 /4/
C VAX-SUN-SGI-IBM-HPX-END
C WNT-DOS-DEC-START
      data is1 /4/, is4 /1/
C WNT-DOS-DEC-END
c
c...Initialize routine
c
      knc    = 0
      ldlm   = '/ '
c
c...Store major word
c
      call getvwd (kbuf(4),cdat,knc,1,PSTWRD,PSTWVL,NPSTWD)
      if (kwds .eq. 0) go to 8000
c
c...Machine name
c
      ist    = 1
      if (kbuf(4) .eq. 1015) then
          rbuf(1)   = gbuf(1)
          if (jbuf(1) .eq. -1 .and. jbuf(2) .gt. 8 .and.
     1            jbuf(2) .le. 40) then
              ist    = 9
              ien    = jbuf(2) + 8
              nc     = (ien-ist+1+7) / 8 + 1
              do 200 i=2,nc,1
                  rbuf(i) = gbuf(i)
  200         continue
              inc    = nc     + 1
          else
              ist    = 1
              ien    = 8
              inc    = 2
          endif
          tbuf   = cdat(1:knc) // ldlm // lbuf(ist:ien)
          cdat   = tbuf
          knc    = knc    + (ien-ist+1) + 2
          if (kwds .eq. 1) go to 8000
          ist    = inc
          ldlm   = ', '
      endif
c
c...Store minor word(s)/value(s)
c
      do 1000 k=ist,kwds,1
          rbuf(1) = gbuf(k)
c
c......Minor word
c
          if (ibuf(is1) .eq. 0 .and. ibuf(is4) .ne. 0) then
              inum   = ibuf(is4)
              call getvwd (inum,ldat,nc,2,PSTWRD,PSTWVL,NPSTWD)
c
c......Value
c
          else
              call rtoc (rbuf,ldat,nc)
          endif
c
c......Store minor word/value in string
c
          tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
          cdat   = tbuf
          knc    = knc    + nc     + 2
          ldlm   = ', '
 1000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lstmsc (kbuf,gbuf,kwds,cdat,knc)
c
c   FUNCTION:  This routine formats the CIRCLE and CUTTER cl records for
c              printing.
c
c   INPUT:  kbuf    I*4  D4  -  Clfile record integer data.
c
c           gbuf    R*8  Dn  -  Clfile record real data.
c
c           kwds    I*4  D1  -  Number of reals in 'gbuf'.
c
c   OUTPUT: cdat    C*n  D1  -  Formatted post command.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine lstmsc (kbuf,gbuf,kwds,cdat,knc)
c
      integer*4 kbuf(4),kwds,knc
c
      real*8 gbuf(10)
c
      character*(*) cdat
c
      integer*4 i,nc
c
      character*2 ldlm
      character*20 ldat
      character*132 tbuf
c
c...Initialize routine
c
      knc    = 0
      ldlm   = '/ '
c
c...Store major word
c
      if (kbuf(3) .eq. 3000) then
          cdat   = 'CIRCLE'
      else
          cdat   = 'CUTTER'
      endif
      knc    = 6
c
c...Store minor value(s)
c
      do 1000 i=1,kwds,1
          if (kbuf(3) .eq. 6000 .and. gbuf(i) .eq. 700-10000) then
              ldat   = 'LATHE'
              nc     = 5
          else
              call rtoc (gbuf(i),ldat,nc)
          endif
          tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
          cdat   = tbuf
          knc    = knc    + nc     + 2
          ldlm   = ', '
 1000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lstmot (kbuf,gbuf,kwds,cdat,knc,cmsg,kerr)
c
c   FUNCTION:  This routine formats Motion cl records for printing.
c
c   INPUT:  kbuf    I*4  D4  -  Clfile record integer data.
c
c           gbuf    R*8  Dn  -  Clfile record real data.
c
c           kwds    I*4  D1  -  Number of reals in 'gbuf'.
c
c   OUTPUT: cdat    C*n  D1  -  Formatted post command.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lstmot (kbuf,gbuf,kwds,cdat,knc,cmsg,kerr)
c
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (MULTAX,KPOSMP(0056))
c
      integer*4 MULTAX
c
      integer*4 kbuf(4),kwds,knc,kerr
c
      real*8 gbuf(10)
c
      character*(*) cdat,cmsg
c
      integer*2 motfmt(10),taxfmt(10)
      integer*4 i,nc,npt,j,nc1,nc2
c
      character*2 ldlm
      character*10 spac
      character*20 ldat,lisn,lclr
      character*132 tbuf
c
      data motfmt /3,0,1,5,5,4,4,0,4,2/, taxfmt /3,0,1,1,1,8,8,0,8,2/
      data spac /'          '/
c
c...Initialize routine
c
      npt    = 3
      if ((IMULTO .eq. -1 .and. MULTAX .eq. 1) .or. IMULTO .eq. 1)
     1        npt = 6
      ldlm   = '/ '
c
c...Store major word
c
      if (kbuf(4) .eq. 3) then
          cdat   = 'FROM'
      else if (kbuf(4) .eq. 6) then
          cdat   = 'CONT'
          ldlm   = '. '
      else
          cdat   = 'GOTO'
      endif
      knc    = 4
c
c...Store minor value(s)
c
      do 1000 i=1,kwds,npt
          do 500 j=0,2,1
              call ftoc (gbuf(j+i),ldat,nc,motfmt)
              if (nc .ne. 11) then
                  tbuf   = spac(1:11-nc) // ldat(1:nc)
                  ldat   = tbuf
              endif
              nc     = 11
              tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
              cdat   = tbuf
              knc    = knc    + nc     + 2
              ldlm   = ', '
  500     continue
          if (npt .eq. 6) then
              do 700 j=3,5,1
                  call ftoc (gbuf(j+i),ldat,nc,taxfmt)
                  if (nc .ne. 11) then
                      tbuf   = spac(1:11-nc) // ldat(1:nc)
                      ldat   = tbuf
                  endif
                  nc     = 11
                  tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
                  cdat   = tbuf
                  knc    = knc    + nc     + 2
  700         continue
          endif
c
c......Set up listing record
c
          if (i .eq. 1) then
              call itoc (kbuf(1),lisn,nc1,-5)
              call itoc (kbuf(2),lclr,nc2,-5)
              tbuf   = lisn(1:nc1) // '  ' // lclr(1:nc2) // '   ' //
     1                 cdat(1:knc)
              cdat   = tbuf
              knc    = knc    + nc1    + nc2    + 5
c
          else
              tbuf   = '               ' // cdat(1:knc)
              cdat   = tbuf
              knc    = knc    + 15
          endif
c
c......Write listing record
c
          call lstout (cdat,knc,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          cdat   = '    '
          knc    = 4
          ldlm   = '  '
 1000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lstmul (kbuf,cdat,knc)
c
c   FUNCTION:  This routine formats the MULTAX cl record for printing.
c
c   INPUT:  kbuf    I*4  D4  -  Clfile record integer data.
c
c   OUTPUT: cdat    C*n  D1  -  Formatted post command.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine lstmul (kbuf,cdat,knc)
c
      include 'menu.inc'
c
      integer*4 kbuf(4),knc
c
      character*(*) cdat
c
      integer*4 nc
c
      character*24 ldat
      character*132 tbuf
c
c...Initialize routine
c
      knc    = 0
c
c...Store major word
c
      cdat   = 'MULTAX/ '
      knc    = 8
c
c...Store ON/OFF word
c
      if (kbuf(4) .eq. 0) then
          call getvwd (72,ldat,nc,2,PSTWRD,PSTWVL,NPSTWD)
      else
          call getvwd (71,ldat,nc,2,PSTWRD,PSTWVL,NPSTWD)
      endif
      tbuf   = cdat(1:knc) // ldat(1:nc)
      cdat   = tbuf
      knc    = knc    + nc
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lstunt (kbuf,cdat,knc)
c
c   FUNCTION:  This routine formats the UNITS cl record for printing.
c
c   INPUT:  kbuf    I*4  D4  -  Clfile record integer data.
c
c   OUTPUT: cdat    C*n  D1  -  Formatted post command.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine lstunt (kbuf,cdat,knc)
c
      include 'menu.inc'
c
      integer*4 kbuf(4),knc
c
      character*(*) cdat
c
      integer*4 nc
c
      character*24 ldat
      character*132 tbuf
c
c...Initialize routine
c
      knc    = 0
c
c...Store major word
c
      cdat   = 'UNITS/ '
      knc    = 8
c
c...Store UNITS setting word
c
      if (kbuf(4) .eq. 2) then
          call getvwd (301,ldat,nc,2,PSTWRD,PSTWVL,NPSTWD)
      else
          call getvwd (303,ldat,nc,2,PSTWRD,PSTWVL,NPSTWD)
      endif
      tbuf   = cdat(1:knc) // ldat(1:nc)
      cdat   = tbuf
      knc    = knc    + nc
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lstcdi (kbuf,gbuf,kwds,cdat,knc)
c
c   FUNCTION:  This routine formats the CUTTER/DISPLY record for printing.
c
c   INPUT:  kbuf    I*4  D4  -  Clfile record integer data.
c
c           gbuf    R*8  Dn  -  Clfile record real data.
c
c           kwds    I*4  D1  -  Number of reals in 'gbuf'.
c
c   OUTPUT: cdat    C*n  D1  -  Formatted post command.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine lstcdi (kbuf,gbuf,kwds,cdat,knc)
c
      integer*4 kbuf(4),kwds,knc
c
      real*8 gbuf(10)
c
      character*(*) cdat
c
      integer*2 inum(8)
      integer*4 i,nc,strlen1,ist,nwds,ishk,mxc,inc
c
      real*8 rnum(10)
c
      character*2 ldlm
      character*10 sym
      character*20 ldat
      character*80 sym1
      character*132 tbuf
c
      equivalence (rnum,sym,sym1,inum)
c
c...Initialize routine
c
      knc    = 0
      ldlm   = ', '
c
c...CUTTER/DISPLY,params
c
      if (kbuf(4) .eq. 1 .or. kbuf(4) .eq. 2) then
          cdat   = 'CUTTER/DISPLY'
          knc    = 13
          nwds   = kwds
          ist    = 1
c
c...CUTTER/DISPLY,sym,params (Pre NCL V9.6)
c
          if (kbuf(4) .eq. 2) then
              rnum(1) = gbuf(1)
              rnum(2) = gbuf(2)
              cdat = cdat(1:knc) // ',' // sym
              knc    = knc    + strlen1(sym) + 1
              ist    = 5
              rnum(1) = gbuf(4)
              if (inum(4) .eq. 3 .or. inum(4) .eq. 4) then
                  nwds   = 8
                  if (gbuf(7) .eq. 0. and. gbuf(8) .eq. 0.) then
                      nwds = 6
                      if (gbuf(6) .eq. 0.) nwds = 5
                  endif
              else
                  nwds   = 4
              endif
          endif
          do 100 i=ist,nwds,1
              call rtoc (gbuf(i),ldat,nc)
              tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
              cdat   = tbuf
              knc    = knc    + nc     + 2
              ldlm   = ', '
 100      continue
c
c...CUTTER/DISPLY,PART-ALL
c
      else if (kbuf(4) .eq. 3) then
          rnum(1) = gbuf(1)
          if (inum(4) .eq. 0) then
              cdat   = 'CUTTER/DISPLY,PART'
          else
              cdat   = 'CUTTER/DISPLY,ALL'
          endif
          knc    = strlen1(cdat)
c
c...CUTTER/DISPLY,SHANK (Pre NCL V9.6)
c
      else if (kbuf(4) .eq. 4) then
          cdat   = 'CUTTER/DISPLY,SHANK'
          knc    = strlen1(cdat)
          do 500 i=1,kwds,1
              rnum(1) = 716 - 10000
              rnum(2) = 157 - 10000
              if (gbuf(i) .eq. 716-10000) then
                  ldat   = 'CUTTER'
                  nc     = 6
              else if (gbuf(i) .eq. 157-10000) then
                  ldat   = 'HOLDER'
                  nc     = 6
              else
                  call rtoc (gbuf(i),ldat,nc)
              endif
              tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
              cdat   = tbuf
              knc    = knc    + nc     + 2
              ldlm   = ', '
 500      continue
c
c...CUTTER/DISPLY,symbol (9.6+)
c
      else if (kbuf(4) .eq. 5 .or. kbuf(4) .eq. 7) then
          cdat   = 'CUTTER/DISPLY'
          knc    = 13
          nwds   = kwds
c
          if (kbuf(4) .eq. 5) then
              rnum(1) = gbuf(1)
              rnum(2) = gbuf(2)
              rnum(3) = gbuf(3)
              mxc = strlen1(sym1(1:20))
          else
              rnum(1) = gbuf(1)
              mxc = inum(1)
              do 550 i=1,(mxc+7)/8,1
                  rnum(i) = gbuf(i+3)
  550         continue
          endif
          cdat = cdat(1:knc) // ',' // sym1(1:mxc)
          knc    = knc    + mxc + 11
c
          ist = 2
          if (kbuf(4) .eq. 5) ist = 4
          if (gbuf(ist) .ne. 0. .or. gbuf(ist+1) .ne. 0.) then
              do 600 i=ist,ist+1,1
                  call rtoc (gbuf(i),ldat,nc)
                  tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
                  cdat   = tbuf
                  knc    = knc    + nc     + 2
                  ldlm   = ', '
 600          continue
          endif
c
c...CUTTER/DISPLY,SHANK-HOLDER
c
      else if (kbuf(4) .eq. 6 .or. kbuf(4) .eq. 8) then
          cdat   = 'CUTTER/DISPLY'
          knc    = 13
          nwds   = kwds
c
          if (kbuf(4) .eq. 6) then
              rnum(1) = gbuf(3)
              ist = 3
          else
              rnum(1) = gbuf(1)
              ist = 2
          endif
          ishk   = inum(ist+1)
          if (ishk .eq. 2) then
              cdat   = cdat(1:knc) // ldlm // 'HOLDER'
              knc    = knc    + 8
          else
              cdat   = cdat(1:knc) // ldlm // 'SHANK'
              knc    = knc    + 7
          endif
c
          if (inum(ist) .gt. 1) then
              if (kbuf(4) .eq. 6) then
                  rnum(1) = gbuf(1)
                  rnum(2) = gbuf(2)
                  rnum(3) = gbuf(3)
                  mxc = strlen1(sym1(1:20))
              else
                  rnum(1) = gbuf(1)
                  mxc = inum(1)
                  do 650 i=1,(mxc+7)/8,1
                      rnum(i) = gbuf(i+5)
  650             continue
              endif
              cdat = cdat(1:knc) // ',' // sym1(1:mxc)
              knc    = knc    + mxc + 2
          endif
c
          ist    = knc
          inc    = 2
          if (kbuf(4) .eq. 6) inc = 4
          do 700 i=inc,inc+3,1
              call rtoc (gbuf(i),ldat,nc)
              tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
              cdat   = tbuf
              knc    = knc    + nc     + 2
              if (gbuf(i) .ne. 0.) ist = knc
 700      continue
          knc    = ist
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lststk (kbuf,gbuf,kwds,cdat,knc)
c
c   FUNCTION:  This routine formats the STOCK and FIXTUR records for
c              printing.
c
c   INPUT:  kbuf    I*4  D4  -  Clfile record integer data.
c
c           gbuf    R*8  Dn  -  Clfile record real data.
c
c           kwds    I*4  D1  -  Number of reals in 'gbuf'.
c
c   OUTPUT: cdat    C*n  D1  -  Formatted post command.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine lststk (kbuf,gbuf,kwds,cdat,knc)
c
      integer*4 kbuf(4),kwds,knc
c
      real*8 gbuf(10)
c
      character*(*) cdat
c
      integer*2 inum(200)
      integer*4 i,nc,ist,nwds,jnum(100)
c
      real*8 rnum(50)
c
      character*2 ldlm
      character*20 ldat
      character*400 lnum
      character*132 tbuf
c
      equivalence (rnum,inum,jnum,lnum)
c
      integer*4 BOXV,CYLV,LOADV,STLV,CLONEV,MOVEV,REMOVV,MODV,CHIPV
c
      parameter (BOXV=340)
      parameter (CHIPV=331)
      parameter (CLONEV=576)
      parameter (CYLV=620)
      parameter (LOADV=1075)
      parameter (MODV=732)
      parameter (MOVEV=577)
      parameter (REMOVV=843)
      parameter (STLV=330)
c
c...Initialize routine
c
      do 100 i=1,kwds,1
          rnum(i) = gbuf(i)
  100 continue
      ldlm   = '/ '
      if (kbuf(3) .eq. 2600) then
          tbuf   = 'STOCK '
      else
          tbuf   = 'FIXTUR'
      endif
      if (kbuf(4) .eq. BOXV) then
          ldat   = 'BOX'
          nc     = 3
      else if (kbuf(4) .eq. CHIPV) then
          ldat   = 'REMOVE,CHIPS'
          nc     = 12
      else if (kbuf(4) .eq. CLONEV) then
          ldat   = 'CLONE'
          nc     = 5
      else if (kbuf(4) .eq. CYLV) then
          ldat   = 'CYLNDR'
          nc     = 6
      else if (kbuf(4) .eq. LOADV) then
          ldat   = 'LOAD'
          nc     = 4
      else if (kbuf(4) .eq. MODV) then
          ldat   = 'MODIFY'
          nc     = 6
      else if (kbuf(4) .eq. MOVEV) then
          ldat   = 'MOVE'
          nc     = 4
      else if (kbuf(4) .eq. REMOVV) then
          ldat   = 'REMOVE'
          nc     = 6
      else if (kbuf(4) .eq. STLV) then
          ldat   = 'STL'
          nc     = 3
      else
          knc = 0
          go to 8000
      endif
      cdat   = tbuf(1:6) // ldlm // ldat(1:nc)
      knc    = 6     + 2     + nc
      ldlm   = ', '
      if (kbuf(4) .ne. MOVEV .and. kbuf(4) .ne. REMOVV .and.
     1    kbuf(4) .ne. MODV) then
          call itoc (jnum(1),ldat,nc,0)
          tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
          cdat   = tbuf
          knc    = knc    + nc     + 2
      endif
c
c...STOCK/BOX-CYLNDR
c
      if (kbuf(4) .eq. BOXV .or. kbuf(4) .eq. CYLV) then
          call itoc (jnum(2),ldat,nc,0)
          tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
          cdat   = tbuf
          knc    = knc    + nc     + 2
          nwds   = 6
          if (kbuf(4) .eq. CYLV) nwds = 8
          do 200 i=1,nwds,1
              call rtoc (gbuf(i+1),ldat,nc)
              tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
              cdat   = tbuf
              knc    = knc    + nc     + 2
  200     continue
c
c...STOCK/LOAD
c
      else if (kbuf(4) .eq. LOADV) then
          nc     = jnum(2)
          tbuf   = cdat(1:knc) // ldlm // lnum(9:nc+8)
          cdat   = tbuf
          knc    = knc    + nc     + 2
c
c...STOCK/STL
c
      else if (kbuf(4) .eq. STLV) then
          if (jnum(3) .eq. 0) then
              ldat   = 'INCHES'
              nc     = 6
          else
              ldat   = 'MM'
              nc     = 2
          endif
          tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
          cdat   = tbuf
          knc    = knc    + nc     + 2
          nc     = jnum(2)
          tbuf   = cdat(1:knc) // ldlm // lnum(17:nc+16)
          cdat   = tbuf
          knc    = knc    + nc     + 2
c
c...STOCK/CLONE
c
      else if (kbuf(4) .eq. CLONEV) then
          call itoc (jnum(2),ldat,nc,0)
          tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
          cdat   = tbuf
          knc    = knc    + nc     + 2
          call itoc (jnum(3),ldat,nc,0)
          tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
          cdat   = tbuf
          knc    = knc    + nc     + 2
c
c...STOCK/MOVE-REMOVE-MODIFY
c
      else if (kbuf(4) .eq. MOVEV .or. kbuf(4) .eq. REMOVV .or.
     1         kbuf(4) .eq. MODV) then
          ist    = 2
          if (kbuf(4) .eq. MOVEV) then
              tbuf   = cdat(1:knc) // ldlm // lnum(105:110)
              cdat   = tbuf
              knc    = knc    + 6     + 2
              ist    = 28
              ldlm   = ', '
          else if (kbuf(4) .eq. MODV) then
              ist = 8
          endif
          do 600 i=1,jnum(1),1
              call itoc (jnum(i+ist),ldat,nc,0)
              if (knc+nc+2 .gt. 132) go to 8000
              tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
              cdat   = tbuf
              knc    = knc    + nc     + 2
              ldlm   = ', '
  600     continue
c
c...STOCK/REMOVE,CHIPS
c
      else if (kbuf(4) .eq. CHIPV) then
          do 700 i=1,kwds,1
              call rtoc (gbuf(i),ldat,nc)
              if (knc+nc+2 .gt. 132) go to 8000
              tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
              cdat   = tbuf
              knc    = knc    + nc     + 2
              ldlm   = ', '
  700     continue
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lstunk (kbuf,cdat,knc)
c
c   FUNCTION:  This routine formats unrecognized cl records for print-
c              ing.
c
c   INPUT:  kbuf    I*4  D4  -  Clfile record integer data.
c
c   OUTPUT: cdat    C*n  D1  -  Formatted post command.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine lstunk (kbuf,cdat,knc)
c
      integer*4 kbuf(4),knc
c
      character*(*) cdat
c
      integer*4 nc,strlen1
c
      character*20 lnum
c
c...Format listing record
c
      call errtxt ('UNRECCL',cdat)
      call itoc (kbuf(3),lnum,nc,0)
      call errstr (cdat,lnum,1)
      call itoc (kbuf(4),lnum,nc,0)
      call errstr (cdat,lnum,1)
      knc    = strlen1(cdat)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lsterr (cdat,ctxt,cmsg,kerr)
c
c   FUNCTION:  This routine formats and outputs a compilation error
c              message to the listing file and screen.
c
c   INPUT:  ctxt    C*n  D1  -  Text of error message to output.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lsterr (ctxt,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
C WNT-START
      include 'postworks_nt.inc'
C WNT-END
c
      equivalence (ISN   ,KPOSMP(0001)), (NUMERR,KPOSMP(0082))
      equivalence (LSTPC ,KPOSMP(0083)), (ICLOUT,KPOSMP(0084))
      equivalence (IERRPC,KPOSMP(0085)), (IPRTOP,KPOSMP(1176))
c
      integer*4 ISN,NUMERR,LSTPC,ICLOUT,IERRPC,IPRTOP
c
      equivalence (ERRMAC,CPOSMP(0193)), (LMACRO,CPOSMP(0861))
c
      character*8 ERRMAC
      character*24 LMACRO
c
      integer*4 kerr
C WNT-START
      integer*4 flag
C WNT-END
c
      character*(*) cmsg,ctxt
c
      integer*4 nc,strlen1,inum,ipop
c
      character*20 lnum
      character*80 sbuf
      character*132 ldat
c
c...Increment error count
c
      kerr   = 0
      ipop   = IPRTOP
      IPRTOP = 1
      if (MAXPER .eq. -1) go to 8000
      NERR   = NERR   + 1
      NUMERR = NUMERR + 1
c
c...MACRO call not in effect
c
      if (IPC .eq. 0 .or. (IMACPT .eq. 1 .and. IMACHD(1,1) .eq. 1103
     1    .and. IERRPC .ne. LSTPC)) then
          call errtxt ('NOMACERR',ldat)
          call itoc (NUMERR,lnum,nc,0)
          call errstr (ldat,lnum,0)
          call itoc (ISN,lnum,nc,0)
          call errstr (ldat,lnum,0)
          inum   = ICLOUT
          if (PGMNAM .eq. 'PostMacro') inum   = ICLOUT + 1
          call itoc (inum,lnum,nc,0)
          call errstr (ldat,lnum,0)
c
c...MACRO call is in effect
c
      else
          call errtxt ('MACERRLN',ldat)
          call itoc (NUMERR,lnum,nc,0)
          call errstr (ldat,lnum,0)
          call itoc (ISN,lnum,nc,0)
          call errstr (ldat,lnum,0)
          inum   = ICLOUT
          if (PGMNAM .eq. 'PostMacro') inum   = ICLOUT + 1
          call itoc (inum,lnum,nc,0)
          call errstr (ldat,lnum,0)
          call itoc (IERRPC,lnum,nc,0)
          call errstr (ldat,lnum,0)
          call errstr (ldat,ERRMAC,0)
      endif
c
c...Output error count & line number
c
      nc     = strlen1(ldat)
      if (PGMNAM .eq. 'PostMacro') then
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C          call trmmsg (ldat)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
          flag = 1
          nc = strlen1(ldat)
          call add1dispmsg (ldat, nc, flag)
C WNT-END
      else
          call prtout (' ',1)
          call prtout (ldat,nc)
      endif
      call lstout (ldat,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Output error message &
c...output clfile name
c
      nc     = strlen1(ctxt)
      if (PGMNAM .eq. 'PostMacro') then
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C          call trmmsg (ctxt)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
          flag = 1
          nc = strlen1(ctxt)
          call add1dispmsg (ctxt, nc, flag)
C WNT-END
      else
          call prtout (ctxt,nc)
      endif
      call lstout (ctxt,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
      if (PGMNAM .eq. 'PostMacro') then
          ldat   = 'Clfile: ' // LOBJFI(1:NOBJFI)
          nc     = NOBJFI + 8
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C          call trmmsg (ldat)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
          flag = 1
          nc = strlen1(ldat)
          call add1dispmsg (ldat, nc, flag)
C WNT-END
      else
          call shfile (PCHFIL,sbuf,80)
          nc     = strlen1(sbuf)
          ldat   = 'Punch file: ' // sbuf(1:nc)
          nc     = nc     + 12
          call prtout (ldat,nc)
      endif
      call lstout (ldat,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      if (PGMNAM .eq. 'PostMacro') then
C          call trmmsg (' ')
C      else
C          call prtout (' ',1)
C      endif
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      if (PGMNAM .ne. 'PostMacro') then
          call prtout (' ',1)
      endif
C WNT-END

      call lstout (' ',1,cmsg,kerr)
      if (PGMNAM .ne. 'PostMacro' .and. NUMERR .gt. MAXPER) go to 9000
c
c...End of routine
c
 8000 IPRTOP = ipop
      return
c
c...Maximum errors reached
c
 9000 call pstsum
      call errtxt ('MANYERRS',cmsg)
      call errkil (cmsg,1)
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lsthed (cmsg,kerr)
c
c   FUNCTION:  This routine outputs the listing file header.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lsthed (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 nc,nc1,nc2,ipt,strlen1
c
      character*20 lnum,sb1,sb2
      character*80 sbuf
      character*132 ldat,tbuf
c
c...If 1st time here then create header
c
      if (LISPAG .eq. 0) then
          call getsap ('PREHD1',ipt,IPRMDS,SALABL)
          call itoc (PWVER1,sb1,nc1,1)
          call itoc (PWVER2,sb2,nc2,1)
          LPREH1 = SAPRM(ipt)
          call errstr (LPREH1,PGMNAM,1)
          call errstr (LPREH1,sb1(1:nc1),1)
          call errstr (LPREH1,sb2(1:nc2),1)
          if (PGMNAM .eq. 'PostMacro') then
              call itoc (PRENUM(PREPT),lnum,nc,0)
              call errstr (LPREH1,lnum(1:nc),0)
          else if (PGMNAM .eq. 'Pwconv') then
              call errstr (LPREH1,PGMNAM,0)
          else
              nc     = strlen1(PSTNAM(PREPT))
              call errstr (LPREH1,PSTNAM(PREPT)(1:nc),0)
          endif
          call errstr (LPREH1,LDATE,0)
          call errstr (LPREH1,LTIME,0)
          NPREH1 = strlen1(LPREH1)
c
          call getsap ('PREHD2',ipt,IPRMDS,SALABL)
          LPREH2 = SAPRM(ipt)
          NPREH2 = SAPNC(ipt)
          call shfile (LCMPFI(1:NCCMPF),sbuf,60)
          nc     = strlen1(sbuf)
          tbuf   = LPREH2(1:NPREH2) // ' ' // sbuf
          LPREH2 = tbuf
          NPREH2 = NPREH2 + nc     + 1
c
          call getsap ('PREHD3',ipt,IPRMDS,SALABL)
          LPREH3 = SAPRM(ipt)
          NPREH3 = SAPNC(ipt)
          if (PGMNAM .eq. 'PostMacro' .or. PGMNAM .eq. 'Pwconv') then
              call shfile (LOBJFI(1:NOBJFI),sbuf,60)
              nc     = strlen1(sbuf)
              tbuf   = LPREH3(1:NPREH3) // ' ' // sbuf
              LPREH3 = tbuf
              NPREH3 = NPREH3 + nc     + 1
          else
              nc     = 132    - NPREH3
              call shfile (PCHFIL,sbuf,nc)
              nc     = strlen1(sbuf)
              tbuf   = LPREH3(1:NPREH3) // ' ' // sbuf(1:nc)
              LPREH3 = tbuf
              NPREH3 = NPREH3 + nc     + 1
          endif
c
          call getsap ('PREHD4',ipt,IPRMDS,SALABL)
          LPREH4 = SAPRM(ipt)
          NPREH4 = SAPNC(ipt)
      endif
c
c...Write header record
c
      call wrtxt (LUNSC2,LFORMF,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
      LISPAG = LISPAG + 1
      call itoc (LISPAG,lnum,nc,0)
      ldat   = LPREH1(1:NPREH1) // ' ' // lnum(1:nc)
      nc     = NPREH1 + 1 + nc
c
      call wrtxt (LUNSC2,ldat,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call wrtxt (LUNSC2,LPREH2,NPREH2,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call wrtxt (LUNSC2,LPREH3,NPREH3,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call wrtxt (LUNSC2,' ',1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call wrtxt (LUNSC2,LPREH4,NPREH4,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call wrtxt (LUNSC2,' ',1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      LISLIN = 6
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lstout (cdat,knc,cmsg,kerr)
c
c   FUNCTION:  This routine writes a line to the listing file.
c
c   INPUT:  cdat    C*n  D1  -  Text of line to write.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lstout (cdat,knc,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
c
      integer*4 knc,kerr
c
      character*(*) cdat,cmsg
c
      integer*4 nc,nc1,nc2,ist,ie,inc,rindex,mxc
c
      character*512 ldat
c
c...Write listing header
c
      if (IOPFL(2) .eq. 1) then
          if (LISLIN .ge. IOPFL(3) .and. ILSTOP .eq. 1)
     1        call lsthed (cmsg,kerr)
c
c...Write listing record
c
          ist    = 1
          nc     = knc
c
c......Break up lines longer than 132 characters
c
          if (nc .gt. 132) then
              mxc    = 132
              do while (nc .gt. 0)
c
c.........Break at comma
c
                  ie     = ist    + mxc    - 1
                  if (ie .lt. knc) then
                      inc    = rindex(cdat(ist:ie),",")
                      if (inc .ne. 0) ie     = ist    + inc    - 1
                  endif
                  nc1    = ie     - ist    + 1
c
c.........Indent continuation lines
c
                  if (mxc .ne. 132) then
                      ldat    = '                ' // cdat(ist:ie)
                      nc2    = ie     - ist    + 17
                      call wrtxt (LUNSC2,ldat,nc2,cmsg,kerr)
c
c.........Write out truncated line
c
                  else
                      call wrtxt (LUNSC2,cdat(ist:ie),nc1,cmsg,kerr)
                  endif
                  if (ILSTOP .eq. 1) LISLIN = LISLIN + 1
                  ist    = ist    + nc1
                  nc     = nc     - nc1
                  mxc    = 132    - 16
                  if (nc .lt. mxc) mxc = nc
              enddo
c
c...Write out entire line
c
          else
              call wrtxt (LUNSC2,cdat(ist:ist+nc-1),nc,cmsg,kerr)
          endif
      endif
      return
      end
