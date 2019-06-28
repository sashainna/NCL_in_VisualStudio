c
c***********************************************************************
c
c   FILE NAME: dbase.for
c   CONTAINS:
c               addrec  delrec  fprint  opndat  opnprt  purg    search
c               addrecc delrec searchc purgc pwdpasc sav_clr_spos
c               reset_spos getsearch create_batfile
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        dbase.f , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:06
c
c***********************************************************************
c
C WNT-START
      subroutine dbase
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE:  addrec (cmsg,kerr)
c
c   FUNCTION:  This routine adds a record to the Customer License data
c              base.  The records are added in alphabetic order.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Text of error message.
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
C VAX-WNT-START
      subroutine addrec (cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 nc,drec(4),ics(4),ice(4),i,ix,ians,imsg,irec,irs(10),
     1          ire(10),nc1,nc2,icpt,icx,ipt,inc,lstrec
c
      character*80 ldat,ldat1,ldat2
c
      data drec /11,22,33,44/, ics /1,45,89,133/, ice /40,84,128,172/
      data irs /213,253,273,293,353,355,366,377,389,408/
      data ire /252,272,292,352,354,365,376,388,407,415/
c
c...Find company
c
      call remspc (FRMBUF(1),ldat1,nc1)
      ix     = ichar(FRMBUF(1)(1:1)) - 65 + IERRDS(2)
      if (ix .lt. IERRDS(2)) ix = IERRDS(2)
      if (ix .gt. IERRDS(3)) ix = IERRDS(3)
c
c......Read company index record
c
  100 call rddbas (LUNSC1,ix,IPRTXT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Check for company name
c
      do 200 i=1,4,1
          call remspc (LPRTXT(ics(i):ice(i)),ldat2,nc2)
          if (nc2 .eq. 0) go to 200
          call pwdcmp (ldat1,nc1,ldat2,nc2,ifl)
          if (ifl .eq. 0) go to 300
          if (ifl .eq. -1) go to 205
  200 continue
c
c......Point to next company index
c
      if (IPRTXT(53) .ne. 0) then
          ix     = IPRTXT(53)
          go to 100
      endif
c
c......At end of indices
c......Allocate new company index
c
      ipt    = 0
      do 202 i=1,4,1
          if (LPRTXT(ics(i):ice(i)) .eq. ' ') then
              if (ipt .eq. 0) ipt = i
          else
              ipt    = 0
          endif
  202 continue
c
c.........Allocate index on this record
c
      if (ipt .ne. 0) then
          icx    = ix
          icpt   = ipt
          irec   = IERRDS(1)
          ix     = irec
          FRMBUF(31) = FRMBUF(1)
          IERRDS(1) = IERRDS(1) + 1
          go to 210
      else
c
c.........Allocate new record
c
          irec   = IERRDS(1)
          ix     = irec
          FRMBUF(31) = FRMBUF(1)
          IERRDS(1) = IERRDS(1) + 1
          icx    = IERRDS(1)
          icpt   = 1
          go to 214
      endif
c
c......Insert this company between
c......2 other companies
c
  205 icx    = ix
      icpt   = i
      ipt    = icpt
      irec   = IERRDS(1)
      ix     = irec
      FRMBUF(31) = FRMBUF(1)
      IERRDS(1) = IERRDS(1) + 1
c
c.........Search for a blank spot
c
  206 inc    = ipt    - 1
      if (inc .eq. 0) inc = 1
      do 207 i=inc,4,1
          if (LPRTXT(ics(i):ice(i)) .eq. ' ') go to 208
  207 continue
      go to 211
c
c............Found blank spot
c............Modify this record only
c
  208 do 209 i=i,ipt+1,-1
          LPRTXT(ics(i):ice(i)) = LPRTXT(ics(i-1):ice(i-1))
          IPRTXT(drec(i)) = IPRTXT(drec(i-1))
  209 continue
c
c...........Add this companies index
c
  210 LPRTXT(ics(ipt):ice(ipt)) = FRMBUF(31)
      IPRTXT(drec(ipt)) = ix
      call wrdbas (LUNSC1,IPRTXT(52),IPRTXT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      imsg   = 22
      go to 550
c
c............Did not find blank spot
c............Shift entire record
c
  211 FRMBUF(32) = LPRTXT(ics(4):ice(4))
      isav   = IPRTXT(drec(4))
      do 212 i=4,ipt+1,-1
          LPRTXT(ics(i):ice(i)) = LPRTXT(ics(i-1):ice(i-1))
          IPRTXT(drec(i)) = IPRTXT(drec(i-1))
  212 continue
      LPRTXT(ics(ipt):ice(ipt)) = FRMBUF(31)
      IPRTXT(drec(ipt)) = ix
c
      FRMBUF(31) = FRMBUF(32)
      ix    = isav
c
c...............Allocate new company index
c
  214 if (IPRTXT(53) .eq. 0) then
          IPRTXT(53) = IERRDS(1)
          IERRDS(1) = IERRDS(1) + 1
          call wrdbas (LUNSC1,IPRTXT(52),IPRTXT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          LPRTXT(1:212) = ' '
          IPRTXT(drec(1)) = 0
          IPRTXT(drec(2)) = 0
          IPRTXT(drec(3)) = 0
          IPRTXT(drec(4)) = 0
          IPRTXT(52) = IERRDS(1) - 1
          IPRTXT(53) = 0
c
c...............Read next company index
c
      else
          call wrdbas (LUNSC1,IPRTXT(52),IPRTXT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          call rddbas (LUNSC1,IPRTXT(53),IPRTXT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
      ipt    = 1
      go to 206
c
c...Company name matched
c...Search for actual data record
c
  300 icx    = ix
      icpt   = i
      irec   = IPRTXT(drec(i))
      lstrec = icx
      call remspc (FRMBUF(2),FRMBUF(32),SAPNC(32))
      call remspc (FRMBUF(3),FRMBUF(33),SAPNC(33))
      call remspc (FRMBUF(8),FRMBUF(38),SAPNC(38))
  350 call rddbas (LUNSC1,irec,IPRTXT(54),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Compare fields
c
      if (IPRTXT(105) .eq. 0) go to 400
      call remspc (LPRTXT(irs(3):ire(3)),ldat1,nc1)
      if (ldat1 .ne. FRMBUF(33)) go to 400
c
      call remspc (LPRTXT(irs(2):ire(2)),ldat,nc)
      if (ldat .ne. FRMBUF(32)) go to 400
c
      call remspc (LPRTXT(irs(8):ire(8)),ldat,nc)
      if (ldat .ne. FRMBUF(38)) go to 400
c
c......Fields compared
c......Replace record
c
      call prompt (2,ians)
      if (ians .eq. 1) then
          imsg   = 24
          go to 600
      else
          call messag (21)
          go to 8000
      endif
c
c......Field mismatch
c......Get next record
c
  400 call pwdcmp (FRMBUF(33),SAPNC(33),ldat1,nc1,ifl)
      if (ifl .eq. -1) go to 450
      if (IPRTXT(106) .eq. 0) then
          if (IPRTXT(105) .eq. 0) then
              IPRTXT(105) = irec
              go to 600
          else
              go to 500
          endif
      else
          lstrec = irec
          irec   = IPRTXT(106)
          go to 350
      endif
c
c......Add the new record prior
c......to the current record
c
  450 if (lstrec .eq. icx) then
          IPRTXT(drec(icpt)) = IERRDS(1)
          call wrdbas (LUNSC1,IPRTXT(52),IPRTXT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      else
          call rddbas (LUNSC1,lstrec,IPRTXT(54),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          IPRTXT(106) = IERRDS(1)
          call wrdbas (LUNSC1,lstrec,IPRTXT(54),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
      IPRTXT(105) = IERRDS(1)
      IPRTXT(106) = irec
      IERRDS(1) = IERRDS(1) + 1
      imsg   = 23
      go to 560
c
c...Add new record
c
  500 IPRTXT(106) = IERRDS(1)
      irec    = IERRDS(1)
      IERRDS(1) = IERRDS(1) + 1
      call wrdbas (LUNSC1,IPRTXT(105),IPRTXT(54),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      imsg   = 23
c
  550 IPRTXT(105) = irec
      IPRTXT(106) = 0
  560 call wrdbas (LUNSC1,1,IERRDS,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Store data record
c
  600 do 650 i=1,10,1
          LPRTXT(irs(i):ire(i)) = FRMBUF(i)
  650 continue
      call wrdbas (LUNSC1,IPRTXT(105),IPRTXT(54),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Print data record
c
      call fprint (0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (imsg.eq.24) repnum = repnum + 1
      if (imsg.eq.23) addnum = addnum + 1
      if (imsg.eq.22) addnum = addnum + 1
c
c...Display success message
c
      call messag (imsg)
c
c...End of routine
c
 8000 return
      end
C VAX-WNT-END
c
c***********************************************************************
c
c   SUBROUTINE:  delrec (cmsg,kerr)
c
c   FUNCTION:  This routine deletes a record from the Customer License
c              data base.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Text of error message.
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
C VAX-WNT-START
      subroutine delrec (cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,drec(4),ics(4),ice(4),i,ix,ians,irec,irs(10),
     1          ire(10),nc1,nc2,ifnd,ipt
c
      character*80 ldat,ldat1,ldat2
c
      data drec /11,22,33,44/, ics /1,45,89,133/, ice /40,84,128,172/
      data irs /213,253,273,293,353,355,366,377,389,408/
      data ire /252,272,292,352,354,365,376,388,407,415/
c
c...Find company
c
      ifnd   = 0
      call remspc (FRMBUF(1),ldat1,nc1)
      ix     = ichar(FRMBUF(1)(1:1)) - 65 + IERRDS(2)
      if (ix .lt. IERRDS(2)) ix = IERRDS(2)
      if (ix .gt. IERRDS(3)) ix = IERRDS(3)
c
c......Read company index record
c
  100 call rddbas (LUNSC1,ix,IPRTXT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Check for company name
c
      do 200 i=1,4,1
          call remspc (LPRTXT(ics(i):ice(i)),ldat2,nc2)
          if (ldat1 .eq. ldat2) go to 300
  200 continue
      ix     = IPRTXT(53)
      if (ix .eq. 0) go to 9000
      go to 100
c
c...Company name matched
c...Search for actual data record
c
  300 ipt    = i
      irec   = IPRTXT(drec(ipt))
      do 310 i=1,10,1
          call remspc (FRMBUF(i),FRMBUF(i+30),SAPNC(i+30))
  310 continue
  350 if (irec .eq. 0) go to 500
      call rddbas (LUNSC1,irec,IPRTXT(54),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Compare fields
c
      if (IPRTXT(105) .eq. 0) go to 400
      ifnd   = 1
      do 360 i=1,10,1
          call remspc (LPRTXT(irs(i):ire(i)),ldat,nc)
          if (ldat .ne. FRMBUF(i+30)) go to 400
  360 continue
c
c......Fields compared
c......Delete record
c
      call prompt (3,ians)
      if (ians .eq. 1) then
          IPRTXT(105) = 0
          call wrdbas (LUNSC1,irec,IPRTXT(54),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          call messag (26)
          go to 8000
      else
          go to 8000
      endif
c
c......Field mismatch
c......Get next record
c
  400 irec   = IPRTXT(106)
      go to 350
c
c...No match
c...Delete company record
c...if no data records were found
c
  500 if (ifnd .eq. 0) then
          LPRTXT(ics(ipt):ice(ipt)) = ' '
          call wrdbas (LUNSC1,ix,IPRTXT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          call messag (27)
      else
          go to 9000
      endif
c
c...End of routine
c
 8000 return
c
c...No match found
c
 9000 call messag (25)
      go to 8000
      end
C VAX-WNT-END
c
c***********************************************************************
c
c   SUBROUTINE:  fprint (kfl,cmsg,kerr)
c
c   FUNCTION:  This routine outputs a Customer License data base record
c              for printing in the Software Product License format.
c
c   INPUT:  kfl     I*4  D1    -  0 = Do not display success message.  1
c                                 = yes.
c
c   OUTPUT: cmsg    C*n  D1    -  Text of error message.
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
C VAX-WNT-START
      subroutine fprint (kfl,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
C VAX-WNT-END
c
C VAX-START
C     integer*4 ipt,inc,index,strlen1,nc,irecl
c
C     character*20 att(4)
C     character*80 dnam,fnam,lbuf,dnam1,fnam1
c
c...Create new DATA.LSP file
c
C     att(1) = 'sequential'
C     att(2) = 'list'
C     att(3) = 'formatted'
C     att(4) = 'old'
C     irecl  = 80
C     call opnfil (LUNSC2,'DATA.ORG',att,irecl,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
c
C     att(4) = 'new'
C     call opnfil (LUNSC4,'DATA.LSP',att,irecl,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
C     inquire (unit=LUNSC4,name=dnam)
c
c......Read record from data format file
c
C     ipt    = 0
C 200 call rdtxt (LUNSC2,lbuf,cmsg,kerr)
C     if (kerr .eq. 1) go to 500
C     if (kerr .ne. 0) go to 8000
c
c......Check for %s string
c......If so then add next field here
c
C     if (ipt .ne. 10) then
C         inc    = index(lbuf,'%s')
C         if (inc .ne. 0) then
C             ipt    = ipt    + 1
C             call errstr (lbuf,FRMBUF(ipt),0)
C         endif
C     endif
c
c......Write record
c
C     nc     = strlen1(lbuf)
C     call wrtxt (LUNSC4,lbuf,nc,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
C     go to 200
c
c......End of data file
c
C 500 call clsfil (LUNSC2)
C     call clsfil (LUNSC4)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c...Create new DATA.LPJ file
c
C     att(1) = 'sequential'
C     att(2) = 'list'
C     att(3) = 'formatted'
C     att(4) = 'old'
C     irecl  = 80
C     call opnfil (LUNSC2,'DATAJ.ORG',att,irecl,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
c
C     att(4) = 'new'
C     call opnfil (LUNSC4,'DATA.LPJ',att,irecl,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
C     inquire (unit=LUNSC4,name=dnam1)
c
c......Read record from data format file
c
C     ipt    = 0
C 201 call rdtxt (LUNSC2,lbuf,cmsg,kerr)
C     if (kerr .eq. 1) go to 501
C     if (kerr .ne. 0) go to 8000
c
c......Check for %s string
c......If so then add next field here
c
C     if (ipt .ne. 10) then
C         inc    = index(lbuf,'%s')
C         if (inc .ne. 0) then
C             ipt    = ipt    + 1
C             call errstr (lbuf,FRMBUF(ipt),0)
C         endif
C     endif
c
c......Write record
c
C     nc     = strlen1(lbuf)
C     call wrtxt (LUNSC4,lbuf,nc,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
C     go to 201
c
c......End of data file
c
C 501 call clsfil (LUNSC2)
C     call clsfil (LUNSC4)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c...Create new LICENSE.TYP file
c
C     att(1) = 'sequential'
C     att(2) = 'list'
C     att(3) = 'formatted'
C     att(4) = 'new'
C     irecl  = 80
c
C     call opnfil (LUNSC4,'LICENSE.TYP',att,irecl,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
C     inquire (unit=LUNSC4,name=fnam)
c...
c...Added for LPJ output. Paul 05/18/93
c...
C      call opnfil (LUNHLP,'LICENSEJ.TYP',att,irecl,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
C     inquire (unit=LUNHLP,name=fnam1)
c
c......First time here
c......Include font definition
c
C     if (IBANPT .eq. 1) then
C         call wrtxt (LUNSC4,'<INC>FONT.LSP',13,cmsg,kerr)
C         if (kerr .ne. 0) go to 8000
c...
c...Added for LPJ output. Paul 05/18/93
c...
C         call wrtxt (LUNHLP,'<INC>FONT.LPJ',13,cmsg,kerr)
C         if (kerr .ne. 0) go to 8000
C         IBANPT = 0
C     endif
c
c......Include form and data definitions
c
C     call wrtxt (LUNSC4,'<INC>FORM.LSP',13,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
C     nc     = strlen1(dnam)
C     lbuf   = '<INC>' // (dnam(1:nc))
C     nc     = nc     + 5
C     call wrtxt (LUNSC4,lbuf,nc,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
c...
c...Added for LPJ output. Paul 05/18/93
c...
C     call wrtxt (LUNHLP,'<INC>FORM.LPJ',13,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
C     nc     = strlen1(dnam1)
C     lbuf   = '<INC>' // (dnam1(1:nc))
C     nc     = nc     + 5
C     call wrtxt (LUNHLP,lbuf,nc,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
c
c
c......End of LICENSE.TYP file
c
C     call clsfil (LUNSC4)
c...
c...Added for LPJ output. Paul 05/18/93
c...
C     call clsfil (LUNHLP)
c
c...Create LSP.COM entry
c
C     nc     = strlen1(fnam)
C     lbuf   = '$ LSP ' // fnam
C     nc     = nc     + 6
C     call wrtxt (LUNSC3,lbuf,nc,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
c...
c...Added for LPJ output. Paul.
c...
C     nc     = strlen1(fnam1)
C     lbuf   = '$ LPJ ' // fnam1
C     nc     = nc     + 6
C     call wrtxt (LUNMAC,lbuf,nc,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
c...
C     if (kfl .eq. 1) call messag (29)
c
c...End of routine
c
C VAX-END
C VAX-WNT-START
 8000 return
      end
C VAX-WNT-END
c
c***********************************************************************
c
c   SUBROUTINE:  opndat (kfl,cmsg,kerr)
c
c   FUNCTION:  This routine opens the Customer License data base
c              'CUSTOMER_LICENSE.DBA'.
c
c   INPUT:  kfl     I*4  D1    -  1 = Open current data base for read.
c                                 2 = Open new data base for write.
c                                 3 = Open current data base for write.
c
c   OUTPUT: cmsg    C*n  D1    -  Text of error message.
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
C VAX-WNT-START
      subroutine opndat (kfl,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kerr,kfl
c
      character*(*) cmsg
c
      integer*4 i,irecl,ians,ilun
c
      character*20 att(4)
      character*80 fnam, fnam2
c
      data fnam /'CUSTOMER_LICENSE.DBA'/
      data fnam2 /'CUSTOMER_LICENSE.TMP'/
c
c...Initialize routine
c
      LPRTXT(1:40) = ' '
      LPRTXT(45:84) = ' '
      LPRTXT(89:128) = ' '
      LPRTXT(133:172) = ' '
c
      IPRTXT(11) = 0
      IPRTXT(22) = 0
      IPRTXT(33) = 0
      IPRTXT(44) = 0
c
c...Attempt to open data base
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      if (kfl .eq. 3) att(4) = 'write'
      irecl = 212
      if (kfl .eq. 2) then
          ilun   = LUNSC2
          kerr   = -2
      else
          ilun   = LUNSC1
          call opnfil (ilun,fnam,att,irecl,cmsg,kerr)
      endif
c
c......File exists
c......Read descriptor record
c
  100 if (kerr .eq. 0) then
          call rddbas (ilun,1,IERRDS,cmsg,kerr)
c
c......File does not exist
c......Ask to open new one
c
      else if (kerr .eq. -2) then
          if (kfl .eq. 1) then
              call prompt (1,ians)
              if (ians .eq. 2) go to 8000
          else if (kfl .eq. 3) then
              go to 8000
          endif
c
          att(4) = 'new'
C VAX-WNT-END
C VAX-START
C          call opnfil (ilun,fnam,att,irecl,cmsg,kerr)
C VAX-END
C WNT-START
          if (kfl .ne. 2) then
              call opnfil (ilun,fnam,att,irecl,cmsg,kerr)
          else
              call opnfil (ilun,fnam2,att,irecl,cmsg,kerr)
          endif
C WNT-END
C VAX-WNT-START
          if (kerr .ne. 0) go to 8000
c
c.........Initialize data base
c
          IERRDS(1) = 28
          IERRDS(2) = 2
          IERRDS(3) = 27
          call wrdbas (ilun,1,IERRDS,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
          LPRTXT(1:40) = ' '
          LPRTXT(45:84) = ' '
          LPRTXT(89:128) = ' '
          LPRTXT(133:172) = ' '
c
          IPRTXT(11) = 0
          IPRTXT(22) = 0
          IPRTXT(33) = 0
          IPRTXT(44) = 0
          IPRTXT(53) = 0
c
          do 500 i=2,27,1
              IPRTXT(52) = i
              call wrdbas (ilun,i,IPRTXT,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
  500     continue
      endif
c
c...End of routine
c
 8000 return
      end
C VAX-WNT-END
c
c***********************************************************************
c
c   SUBROUTINE:  opnprt (kfl,cmsg,kerr)
c
c   FUNCTION:  This routine opens the Customer License data base
c              'CUSTOMER_LICENSE.DBA'.
c
c   INPUT:  kfl     I*4  D1    -  0 = Do not display success message. 1
c                                 = yes.
c
c   OUTPUT: cmsg    C*n  D1    -  Text of error message.
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
C VAX-START
C     subroutine opnprt (cmsg,kerr)
c
C     include 'menu.inc'
c
C     integer*4 kerr
c
C     character*(*) cmsg
c
C     integer*4 irecl
c
C     character*20 att(4)
c
c...Open print control file
c...LSP.COM
c
C     att(1) = 'sequential'
C     att(2) = 'list'
C     att(3) = 'formatted'
C     att(4) = 'new'
C     irecl  = 80
c
C     call opnfil (LUNSC3,'LSP.COM',att,irecl,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
c...
c...Added for LPJ output. Paul
c...
C     call opnfil (LUNMAC,'LPJ.COM',att,irecl,cmsg,kerr)
C     if (kerr .ne. 0) go to 8000
C     IBANPT = 1
c
c...End of routine
c
C8000 return
C     end
C VAX-END
c
c***********************************************************************
c
c   SUBROUTINE:  purg (cmsg,kerr)
c
c   FUNCTION:  This routine creates a new License data base and removes
c              any deleted records and customers.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Text of error message.
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
C VAX-WNT-START
      subroutine purg (cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
      character*80 fnam1, fnam2
c
      integer*4 ix,icx,ipto,ics(4),ice(4),icso(4),iceo(4),drec(4),
     1          dreco(4),icur(4),inxt(4),inc(4),irs(10),ire(10),
     2          irso(10),ireo(10),icnt,ifl,i,irec,nc1,nc2
c
      data ics /1,45,89,133/, ice /40,84,128,172/, drec /11,22,33,44/
      data icso /425,469,513,557/, iceo /464,508,552,596/
      data dreco /117,128,139,150/
c
      data inc /1,54,107,160/, icur /52,105,158,211/
      data inxt /53,106,159,212/
c
      data irs /213,253,273,293,353,355,366,377,389,408/
      data ire /252,272,292,352,354,365,376,388,407,415/
      data irso /637,677,697,717,777,779,790,801,813,832/
      data ireo /676,696,716,776,778,789,800,812,831,839/
      data fnam1 /'CUSTOMER_LICENSE.DBA'/
      data fnam2 /'CUSTOMER_LICENSE.TMP'/
c
c...Initialize routine
c
      icx    = IERRDS(2)
      call prompt (4,ians)
      if (ians .eq. 2) go to 8000
c
c...Open new data base for temp purg
c
      call opndat (2,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Initialize company index record
c
   50 ix     = icx
      ipto   = 0
      LPRTXT(icso(1):iceo(4)) = ' '
      IPRTXT(dreco(1)) = 0
      IPRTXT(dreco(2)) = 0
      IPRTXT(dreco(3)) = 0
      IPRTXT(dreco(4)) = 0
      IPRTXT(icur(3)) = icx
      IPRTXT(inxt(3)) = 0
c
c...Read company index
c
  100 call rddbas (LUNSC1,ix,IPRTXT(inc(1)),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Store company records
c
  200 do 1000 i=1,4,1
          if (LPRTXT(ics(i):ice(i)) .eq. ' ') go to 1000
c
c......Store company index
c
          if (ipto .eq. 4) then
              IPRTXT(inxt(3)) = IERRDS(1)
              call wrdbas (LUNSC2,IPRTXT(icur(3)),IPRTXT(inc(3)),cmsg,
     1                     kerr)
              if (kerr .ne. 0) go to 8000
              ipto   = 0
              LPRTXT(icso(1):iceo(4)) = ' '
              IPRTXT(dreco(1)) = 0
              IPRTXT(dreco(2)) = 0
              IPRTXT(dreco(3)) = 0
              IPRTXT(dreco(4)) = 0
              IPRTXT(icur(3)) = IERRDS(1)
              IPRTXT(inxt(3)) = 0
              IERRDS(1) = IERRDS(1) + 1
          endif
c
          ipto   = ipto   + 1
          LPRTXT(icso(ipto):iceo(ipto)) = LPRTXT(ics(i):ice(i))
          IPRTXT(dreco(ipto)) = IERRDS(1)
c
c......Read next data record
c
          irec   = IPRTXT(drec(i))
          icnt   = 0
          ifl    = 0
  400     call rddbas (LUNSC1,irec,IPRTXT(inc(2)),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c......Store data record
c
          if (IPRTXT(icur(2)) .ne. 0) then
              if (ifl .eq. 1) then
                  IPRTXT(inxt(4)) = IERRDS(1)
                  call wrdbas (LUNSC2,IPRTXT(icur(4)),IPRTXT(inc(4)),
     1                         cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  ifl    = 0
              endif
              LPRTXT(irso(1):ireo(10)) = LPRTXT(irs(1):ire(10))
              IPRTXT(icur(4)) = IERRDS(1)
              IERRDS(1) = IERRDS(1) + 1
              icnt   = 1
              ifl    = 1
          endif
c
c.........Go get next record
c
          if (IPRTXT(inxt(2)) .ne. 0) then
              irec   = IPRTXT(inxt(2))
              go to 400
c
c.........End of data records
c
          else
              if (ifl .eq. 1) then
                  IPRTXT(inxt(4)) = 0
                  call wrdbas (LUNSC2,IPRTXT(icur(4)),IPRTXT(inc(4)),
     1                         cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              else
                  if (icnt .eq. 0) ipto = ipto - 1
              endif
          endif
 1000 continue
c
c...Get next company index record
c
      if (IPRTXT(inxt(1)) .ne. 0) then
          ix     = IPRTXT(inxt(1))
          go to 100
      else
c
c......Write company index record
c
          if (ipto .ne. 0) then
              IPRTXT(inxt(3)) = 0
              call wrdbas (LUNSC2,IPRTXT(icur(3)),IPRTXT(inc(3)),cmsg,
     1                     kerr)
              if (kerr .ne. 0) go to 8000
          endif
          if (icx .eq. IERRDS(3)) then
              call wrdbas (LUNSC2,1,IERRDS,cmsg,kerr)
              go to 7000
          else
              icx    = icx    + 1
              go to 50
          endif
      endif
c
c...Open new data base
c
 7000 call clsfil (LUNSC1)
      call clsfil (LUNSC2)
C VAX-WNT-END
C WNT-START
c
c...copy temp purg file to current auth file and delete temp file
c
      nc1 = 20
      nc2 = 20
      call cydelfile(fnam1, nc1, fnam2, nc2, kerr)
      if (kerr .ne. 0) go to 8000
C WNT-END
C VAX-WNT-START
      call opndat (3,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
C VAX-WNT-END
C VAX-START
C     call clrfrm
C VAX-END
C VAX-WNT-START
      call disfld (-1)
      call messag (28)
c
c...End of routine
c
 8000 return
      end
C VAX-WNT-END
c
c***********************************************************************
c
c   SUBROUTINE:  search (flag, cmsg,kerr)
c
c   FUNCTION:  This routine searches for the next match in the Customer
c              License data base.  If no fields are selected, then the
c              next company will be found.  If any of the fields other
c              than COMPANY are selected, then each software product
c              that matches all criteria in the selected fields will be
c              searched for.
c
c   INPUT:  dflag: 1: search and fill in result in form.
c
c
c   OUTPUT: cmsg    C*n  D1    -  Text of error message.
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
C VAX-WNT-START
      subroutine search (flag,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 flag,kerr
c
      character*(*) cmsg
c
      integer*4 nc,drec(4),ics(4),ice(4),i,ix,ians,irec,irs(10),
     1          ire(10),strlen1
c
      character*80 ldat
c
      data ics /1,45,89,133/, ice /40,84,128,172/, drec /11,22,33,44/
      data irs /213,253,273,293,353,355,366,377,389,408/
      data ire /252,272,292,352,354,365,376,388,407,415/

      kerr = 0
c
c...Set up search parameters
c
      do 40 i=1,10,1
          call remspc (FRMBUF(i+20),FRMBUF(i+30),SAPNC(i+30))
   40 continue
      icmp   = 0
      if (MLEVL(1) .eq. 1 .and. SAPNC(31) .ne. 0) icmp = 1
      ifl    = 0
c...why, this would disable company name search
c...Yurong changed 5/20/02
c...      do 20 i=2,10,1
      do 20 i=1,10,1
          if (MLEVL(i) .eq. 1) ifl = 1
   20 continue
c
c...Set up initial read
c
      ix     = SMREC(1)
      icx    = SMREC(2)
      irec   = SMREC(3)
      ipt    = SMREC(4)
c
c...Find company
c
      if (ifl .eq. 0 .or. irec .eq. 0) then
c
c......Company name has not been specified.
c......Point to next company
c
          if (icmp .eq. 0) then
              inc    = ix
c
c......Company name has been specified.
c......Point to first index record that
c......this company can be on
c
          else
              inc    = ichar(FRMBUF(31)(1:1)) - 65 + IERRDS(2)
              if (inc .lt. IERRDS(2)) inc = IERRDS(2)
              if (inc .gt. IERRDS(3)) inc = IERRDS(3)
          endif
c
c......This index is past the
c......1st possible company index
c
          if (ix .gt. inc) go to 9000
c
c......This index is prior to the
c......1st possible company index
c......Point to the 1st index
c
          if (ix .ne. inc) then
              ix     = inc
              icx    = inc
              ipt    = 1
c
c......Point to the next company
c......within the current index
c
          else
              ipt    = ipt    + 1
              if (ipt .gt. 4) then
                  call rddbas (LUNSC1,icx,IPRTXT,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  if (IPRTXT(53) .eq. 0) go to 9000
                  icx    = IPRTXT(53)
                  ipt    = 1
              endif
          endif
c
c......Read company index record
c
  100     call rddbas (LUNSC1,icx,IPRTXT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c......Check for company name
c
          do 200 i=ipt,4,1
              call remspc (LPRTXT(ics(i):ice(i)),ldat,nc)
              if (nc .ne. 0) then
                  if (icmp .eq. 0) go to 300
                  if (ldat(1:SAPNC(31)) .eq.
     1                FRMBUF(31)(1:SAPNC(31))) go to 300
              endif
  200     continue
          icx    = IPRTXT(53)
          ipt    = 1
          if (icx .eq. 0) then
              if (icmp .eq. 1 .or. ix .eq. IERRDS(3)) go to 9000
              ix     = ix     + 1
              icx    = ix
          endif
          go to 100
c
c......Company name matched
c......Search for actual data record
c
  300     ipt    = i
          irec   = IPRTXT(drec(ipt))
c
c...Did not search for company index
c...Point to next data record
c
      else
          call rddbas (LUNSC1,irec,IPRTXT(54),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          irec    = IPRTXT(106)
      endif
c
c......Search for matching data record
c
  400 if (irec .eq. 0) go to 600
      call rddbas (LUNSC1,irec,IPRTXT(54),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Compare fields
c
      if (IPRTXT(105) .eq. 0) go to 500
      if (ifl .eq. 0) go to 7000
      do 420 i=1,10,1
          if (MLEVL(i) .eq. 1 .and. SAPNC(i+30) .ne. 0) then
              call remspc (LPRTXT(irs(i):ire(i)),ldat,nc)
              if (ldat(1:SAPNC(i+30)) .ne. FRMBUF(i+30)(1:SAPNC(i+30)))
     1                go to 500
          endif
  420 continue
      go to 7000
c
c......Field mismatch
c......Get next record
c
  500 irec   = IPRTXT(106)
      go to 400
c
c......No match
c......Point to next company record
c
  600 ipt    = ipt    + 1
      if (ipt .gt. 4) then
          icx    = IPRTXT(53)
          if (icx .eq. 0) then
              if (icmp .eq. 1 .or. ix .eq. IERRDS(3)) go to 9000
              ix     = ix     + 1
              icx    = ix
          endif
c
          ipt    = 1
          call rddbas (LUNSC1,icx,IPRTXT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
      irec   = IPRTXT(drec(ipt))
      go to 400
c
c...Found match
c......Load new record
c
 7000 if (flag.eq.1) then
          do 7100 i=1,10,1
              FRMBUF(i) = LPRTXT(irs(i):ire(i))
              SAPNC(i) = strlen1(FRMBUF(i))
 7100     continue
      else
          do 7300 i=1,10,1
              SFRMBUF(i) = LPRTXT(irs(i):ire(i))
              SSAPNC(i) = strlen1(SFRMBUF(i))
 7300     continue
      endif
c
c......Update search starting location
c
      SMREC(1) = ix
      SMREC(2) = icx
      SMREC(3) = irec
      SMREC(4) = ipt
c
c......Display new record
c
      call disfld (-1)
c
c...End of routine
c
 8000 return
c
c...No match found
c
 9000 if (flag.eq.1) then
          call messag (25)
      else
          kerr = 1
      endif
      go to 8000
      end

C VAX-WNT-END

c***********************************************************************
c
c   SUBROUTINE:  searchc (cmsg,nc,kerr)
c
c   FUNCTION:  This routine searches for the next match in the Customer
c              License data base.  This function can be called from C++ routine
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Text of error message.
c		  nc:                -  number of error message characters
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
C WNT-START
      subroutine searchc (cmsg,nc, kerr)
c
      include 'menu.inc'
c
      integer*4 kerr, strlen1, nc,flag
c
      character*256 cmsg
      flag = 1
      call search (flag, cmsg,kerr)
      nc = strlen1(cmsg)
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE:  addrecc (cmsg,nc, kerr)
c
c   FUNCTION:  This routine This routine adds a record to the Customer
c              License data base.  This function can be called from C++ routine
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Text of error message.
c		  nc:                -  number of error message characters
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
C WNT-START
      subroutine addrecc (cmsg,nc, kerr)
c
      include 'menu.inc'
c
      integer*4 kerr,nc, strlen1
c
      character*256 cmsg

      call addrec (cmsg,kerr)
      nc = strlen1(cmsg)
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE:  delrecc (cmsg,nc,kerr)
c
c   FUNCTION:  This routine This routine deletes a record to the Customer
c              License data base.  This function can be called from C++ routine
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Text of error message.
c		  nc:                -  number of error message characters
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
C WNT-START
      subroutine delrecc (cmsg,nc, kerr)
c
      include 'menu.inc'
c
      integer*4 kerr,nc, strlen1
c
      character*256 cmsg

      call delrec (cmsg,kerr)
      nc = strlen1(cmsg)
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE:  purgc (cmsg,nc,kerr)
c
c   FUNCTION:  This routine creates a new License data base and removes
c              any deleted records and customers.
c				This function can be called from C++ routine
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Text of error message.
c		  nc:                -  number of error message characters
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
C WNT-START
      subroutine purgc (cmsg,nc, kerr)
c
      include 'menu.inc'
c
      integer*4 kerr,nc, strlen1
c
      character*256 cmsg

      kerr = 0
      call purg (cmsg,kerr)
      nc = strlen1(cmsg)
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE:  pwdpasc (kerr)
c
c   FUNCTION:  This routine calculates a valid password for the input
c              fields
c				This function can be called from C++ routine
c
c   INPUT:  none.
c
c   OUTPUT:
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
C WNT-START
      subroutine pwdpasc (ierr)
c
      include 'menu.inc'
c
      integer*4 ierr
c
      ierr = 0
      call pwdpas (FRMBUF,SAPNC,FRMBUF(9),ierr)
      if (ierr .eq. 0) then
          SAPNC(9) = 19
          call disfld (9)
      endif
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE:  sav_clr_spos()
c
c   FUNCTION:  This routine save the search position into temp global value
c
c   INPUT:  none.
c
c   OUTPUT: None
c
c***********************************************************************
c
C WNT-START
      subroutine sav_clr_spos()
c
      include 'menu.inc'
c
      do 100 i = 1,10,1
          SSMREC(i) = SMREC(i)
  100 continue
c
c...Initialize search pointers
c
      SMREC(1) = IERRDS(2)
      SMREC(2) = IERRDS(2)
      SMREC(3) = 0
      SMREC(4) = 0
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE:  reset_spos()
c
c   FUNCTION:  This routine reset the search position from temp global value
c
c   INPUT:  none.
c
c   OUTPUT: None
c
c***********************************************************************
c
C WNT-START
      subroutine reset_spos()
c
      include 'menu.inc'
c
      do 100 i = 1,10,1
          SMREC(i) = SSMREC(i)
  100 continue
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE:  getsearch (rst, kerr)
c
c   FUNCTION:  This routine searches for the next match in the Customer
c              License data base.  This function can be called from C++ routine
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Text of error message.
c		  nc:                -  number of error message characters
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
C WNT-START
      subroutine getsearch (rst,kerr)
c
      include 'menu.inc'
c
      integer*4 i, kerr, flag
c
      byte rst(132, 10)

      flag = 0
      call search (flag,cmsg,kerr)
      do 400 i=1,10,1
          call pwdctb (SFRMBUF(i),rst(1,i))
  400 continue
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE: create_batfile(fname)
c
c   FUNCTION:  Create a license file include all license records
c
c   INPUT:  fname: create a batch file
c						
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
c
C WNT-START
      subroutine create_batfile (fname)
c
      include 'menu.inc'
c
      character*256 fname
c
      integer*4 ix,icx,ipto,ics(4),ice(4),icso(4),iceo(4),drec(4),
     1          dreco(4),icur(4),inxt(4),inc(4),irs(10),ire(10),
     2          irso(10),ireo(10),icnt,ifl,i,irec,j
      character*80 msg
      integer*4 irecl,strlen1
      character*20 att(4),lnum
c
      data ics /1,45,89,133/, ice /40,84,128,172/, drec /11,22,33,44/
      data icso /425,469,513,557/, iceo /464,508,552,596/
      data dreco /117,128,139,150/
c
      data inc /1,54,107,160/, icur /52,105,158,211/
      data inxt /53,106,159,212/
c
      data irs /213,253,273,293,353,355,366,377,389,408/
      data ire /252,272,292,352,354,365,376,388,407,415/
      data irso /637,677,697,717,777,779,790,801,813,832/
      data ireo /676,696,716,776,778,789,800,812,831,839/
c
c...Initialize routine
c
      icx    = IERRDS(2)
c
c...Open batch file
c
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'new'
      irecl  = 80
      call opnfil (LUNBAT,fname,att,irecl,msg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Initialize company index record
c
   50 ix     = icx
      ipto   = 0
      LPRTXT(icso(1):iceo(4)) = ' '
      IPRTXT(dreco(1)) = 0
      IPRTXT(dreco(2)) = 0
      IPRTXT(dreco(3)) = 0
      IPRTXT(dreco(4)) = 0
      IPRTXT(icur(3)) = icx
      IPRTXT(inxt(3)) = 0
c
c...Read company index
c
  100 call rddbas (LUNSC1,ix,IPRTXT(inc(1)),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Store company records
c
  200 do 1000 i=1,4,1
          if (LPRTXT(ics(i):ice(i)) .eq. ' ') go to 1000
          ipto   = ipto   + 1
          LPRTXT(icso(ipto):iceo(ipto)) = LPRTXT(ics(i):ice(i))
          IPRTXT(dreco(ipto)) = IERRDS(1)
c
c......Read next data record
c
          irec   = IPRTXT(drec(i))
          icnt   = 0
          ifl    = 0
  400     call rddbas (LUNSC1,irec,IPRTXT(inc(2)),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c......Store data record
c
          if (IPRTXT(icur(2)) .ne. 0) then
              IPRTXT(inxt(4)) = IERRDS(1)
              do 410 j=1,10,1
                  call wrtxt (LUNBAT,LPRTXT(irs(j):ire(j)),
     1                   strlen1(LPRTXT(irs(j):ire(j))),cmsg,kerr)
  410         continue
              if (kerr .ne. 0) go to 8000
              ifl    = 0
              LPRTXT(irso(1):ireo(10)) = LPRTXT(irs(1):ire(10))
              IPRTXT(icur(4)) = IERRDS(1)
              IERRDS(1) = IERRDS(1) + 1
              if (ipto.eq.4) then
                  icnt   = 0
                  ifl    = 0
                  ipto = 0
              else
                 icnt   = 1
                 ifl    = 1
              endif
          endif
c
c.........Go get next record
c
          if (IPRTXT(inxt(2)) .ne. 0) then
              irec   = IPRTXT(inxt(2))
              go to 400
c
c.........End of data records
c
          else
              if (ifl .eq. 1) then
                  IPRTXT(inxt(4)) = 0
              endif
          endif
 1000 continue
c
c...Get next company index record
c
      if (IPRTXT(inxt(1)) .ne. 0) then
          ix     = IPRTXT(inxt(1))
          go to 100
      else
          if (icx .eq. IERRDS(3)) then
              go to 7000
          else
              icx    = icx    + 1
              go to 50
          endif
      endif
c
c...Open new data base
c
 7000 call clsfil (LUNBAT)
c
c...End of routine
c
 8000 return
      end
C WNT-END
