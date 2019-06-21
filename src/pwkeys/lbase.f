c
c***********************************************************************
c
c   FILE NAME: lbase.f
c   CONTAINS:
c               addrec  delrec  opndat  search
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        lbase.f , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:08
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  addrec (cmsg,kerr)
c
c   FUNCTION:  This routine adds a record to the Software License data
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
      subroutine addrec (cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 nc,i,ians,imsg,irec,irs(9),ire(9),nc1,lstrec,ifl,nc2
c
      character*80 ldat,ldat1, ldat2
c
      data irs /205,245,265,285,345,347,358,369,381/
      data ire /244,264,284,344,346,357,368,380,399/
c
c...Initialize routine
c
      irec   = IERRDS(2)
      if (IERRDS(1) .eq. IERRDS(2)) then
          IERRDS(1) = IERRDS(1) + 1
          imsg   = 23
          go to 550
      endif
c
c...Search for matching data record
c
      lstrec = 0
      call remspc (FRMBUF(2),FRMBUF(32),SAPNC(32))
      call remspc (FRMBUF(3),FRMBUF(33),SAPNC(33))
      call remspc (FRMBUF(8),FRMBUF(38),SAPNC(38))
  350 call rddbas (LUNSC1,irec,IPRTXT(52),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IPRTXT(101) .eq. 0) go to 410
c
c......Compare fields
c
c
c...can't allow ldat1(1:0)
c...it will give unexcept result and need compare all character
c...Yurong 4/11/00
c
c      call remspc (LPRTXT(irs(3):ire(3)),ldat1,nc1)
c      if (ldat1(1:nc1) .ne. FRMBUF(33)(1:nc1)) go to 400
c
c      call remspc (LPRTXT(irs(2):ire(2)),ldat,nc)
c      if (ldat(1:nc) .ne. FRMBUF(32)(1:nc)) go to 400
c
c      call remspc (LPRTXT(irs(8):ire(8)),ldat,nc)
c      if (ldat(1:nc) .ne. FRMBUF(38)(1:nc)) go to 400
      call remspc (LPRTXT(irs(3):ire(3)),ldat1,nc1)
      call remspc (FRMBUF(33),ldat2,nc2)
      if ((nc1.ne.0).and.(nc2.ne.0)) then
          if (ldat1(1:nc1) .ne. ldat2(1:nc2)) go to 400
      endif
      if (nc1.ne.nc2) go to 400
c
      call remspc (LPRTXT(irs(2):ire(2)),ldat,nc)
      call remspc (FRMBUF(32),ldat2,nc2)
      if ((nc.ne.0).and.(nc2.ne.0)) then
          if (ldat(1:nc) .ne. ldat2(1:nc2)) go to 400
      endif
      if (nc.ne.nc2) go to 400
c
      call remspc (LPRTXT(irs(8):ire(8)),ldat,nc)
      call remspc (FRMBUF(38),ldat2,nc2)
      if ((nc.ne.0).and.(nc2.ne.0)) then
          if (ldat(1:nc) .ne. ldat2(1:nc2)) go to 400
      endif
      if (nc.ne.nc2) go to 400
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
  410 if (IPRTXT(102) .eq. 0) then
          if (IPRTXT(101) .eq. 0) then
              IPRTXT(101) = irec
              imsg = 23
              go to 600
          else
              go to 500
          endif
      else
          lstrec = irec
          irec   = IPRTXT(102)
          go to 350
      endif
c
c......Add the new record prior
c......to the current record
c
  450 if (lstrec .eq. 0) then
          IERRDS(2) = IERRDS(1)
      else
          call rddbas (LUNSC1,lstrec,IPRTXT(52),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          IPRTXT(102) = IERRDS(1)
          call wrdbas (LUNSC1,lstrec,IPRTXT(52),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
      IPRTXT(101) = IERRDS(1)
      IPRTXT(102) = irec
      IERRDS(1) = IERRDS(1) + 1
      imsg   = 23
      go to 560
c
c...Add new record
c
  500 IPRTXT(102) = IERRDS(1)
      irec    = IERRDS(1)
      IERRDS(1) = IERRDS(1) + 1
      call wrdbas (LUNSC1,IPRTXT(101),IPRTXT(52),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      imsg   = 23
c
  550 IPRTXT(101) = irec
      IPRTXT(102) = 0
  560 call wrdbas (LUNSC1,1,IERRDS,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Store data record
c
  600 do 650 i=1,9,1
          LPRTXT(irs(i):ire(i)) = FRMBUF(i)
  650 continue
      call wrdbas (LUNSC1,IPRTXT(101),IPRTXT(52),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (imsg.eq.24) repnum = repnum + 1
      if (imsg.eq.23) addnum = addnum + 1
      call messag (imsg)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  delrec (cmsg,kerr)
c
c   FUNCTION:  This routine deletes a record from the Software License
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
      subroutine delrec (cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 nc,i,ians,irec,irs(9),ire(9)
c
      character*80 ldat
c
      data irs /205,245,265,285,345,347,358,369,381/
      data ire /244,264,284,344,346,357,368,380,399/
c
c...Initialize routine
c
      if (IERRDS(1) .eq. IERRDS(2)) go to 9000
      irec   = IERRDS(2)
c
c...Find data record
c
      do 310 i=1,9,1
          call remspc (FRMBUF(i),FRMBUF(i+30),SAPNC(i+30))
  310 continue
  350 if (irec .eq. 0) go to 9000
      call rddbas (LUNSC1,irec,IPRTXT(52),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Compare fields
c
      if (IPRTXT(101) .eq. 0) go to 400
      do 360 i=1,9,1
          call remspc (LPRTXT(irs(i):ire(i)),ldat,nc)
c
c...same length compared
c
c          if (ldat .ne. FRMBUF(i+30)(1:nc)) go to 400
          if (ldat(1:nc) .ne. FRMBUF(i+30)(1:nc)) go to 400
  360 continue
c
c......Fields compared
c......Delete record
c
      call prompt (3,ians)
      if (ians .eq. 1) then
          IPRTXT(101) = 0
          call wrdbas (LUNSC1,irec,IPRTXT(52),cmsg,kerr)
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
  400 irec   = IPRTXT(102)
      go to 350
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
c
c***********************************************************************
c
c   SUBROUTINE:  opndat (cmsg,kerr)
c
c   FUNCTION:  This routine opens the Software License data base
c              'NCCS_LICENSE.DBA'.
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
      subroutine opndat (cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 irecl,ians
c
      character*20 att(4)
      character*80 fnam
      character*(MAX_PATH) fout
c
C WNT-VAX-SUN-SGI-IBM-HPX-START
      data fnam /'NCCS_LICENSE.DBA'/
C WNT-VAX-SUN-SGI-IBM-HPX-END
C DOS-START
C     data fnam /'NCCS_LIC.DBA'/
C DOS-END
c
c...Attempt to open data base
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'write'
      irecl = 204
      call fparse (fnam,fout,LDEVIC,'.dba')
      call opnfil (LUNSC1,fout,att,irecl,cmsg,kerr)
c
c......File exists
c......Read descriptor record
c
  100 if (kerr .eq. 0) then
          call rddbas (LUNSC1,1,IERRDS,cmsg,kerr)
c
c......File does not exist
c......Ask to open new one
c
      else if (kerr .eq. -2) then
          call prompt (1,ians)
          if (ians .eq. 2) go to 8000
c
          att(4) = 'new'
          call opnfil (LUNSC1,fout,att,irecl,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c.........Initialize data base
c
          IERRDS(1) = 2
          IERRDS(2) = 2
          call wrdbas (LUNSC1,1,IERRDS,cmsg,kerr)
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
c   SUBROUTINE:  search (bmsg,kerr)
c
c   FUNCTION:  This routine searches for the next match in the Software
c              License data base.  If no fields are selected, then the
c              next software product will be found, otherwise the next
c              record that matches all criteria in the selected fields
c              will be searched for.
c
c   INPUT:  none.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine search
c
      include 'menu.inc'
c
      integer*4 nc,i,irec,irs(9),ire(9),strlen1,ifl,kerr
c
      character*80 cmsg, ldat
c
      data irs /205,245,265,285,345,347,358,369,381/
      data ire /244,264,284,344,346,357,368,380,399/
c
c...Set up search parameters
c
      if (IERRDS(1) .eq. IERRDS(2)) go to 9000
      ifl    = 0
      do 40 i=1,9,1
          if (MLEVL(i) .eq. 1) ifl = 1
          call remspc (FRMBUF(i+20),FRMBUF(i+30),SAPNC(i+30))
   40 continue
c
c...Set up initial read
c
      irec   = SMREC(3)
      if (irec .eq. 0) then
          irec = IERRDS(2)
      else
          call rddbas (LUNSC1,irec,IPRTXT(52),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          irec    = IPRTXT(102)
      endif
c
c......Search for matching data record
c
  400 if (irec .eq. 0) go to 9000
      call rddbas (LUNSC1,irec,IPRTXT(52),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Compare fields
c
      if (IPRTXT(101) .eq. 0) go to 500
      if (ifl .eq. 0) go to 7000
      do 420 i=1,9,1
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
  500 irec   = IPRTXT(102)
      go to 400
c
c...Found match
c......Load new record
c
 7000 do 7100 i=1,9,1
          FRMBUF(i) = LPRTXT(irs(i):ire(i))
          SAPNC(i) = strlen1(FRMBUF(i))
 7100 continue
c
c......Update search starting location
c
      SMREC(3) = irec
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
 9000 call messag (25)
      go to 8000
      end
