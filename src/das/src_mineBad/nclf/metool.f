C*********************************************************************
C*    NAME         :  metool.f
C*       CONTAINS:
C*					metool  ajtool
C*
C*    COPYRIGHT 1993 (c) Numerical Control Computer Sceinces Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       metool.f , 04/29/15
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:16
C********************************************************************/
C
c
c***********************************************************************
c
c   SUBROUTINE:  metool (gbuf,kin,kout)
c
c   FUNCTION:  This routine converts a tool library record's units from/
c              to inch/mm.
c
c   INPUT:  gnum    R*8  D128 -  Tool Library record to be converted.
c
c           kin     I*4  D1   -  Input units (1 = INCH, 2 = MM).
c
c           kout    I*4  D1   -  Output units.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine metool (gbuf,kin,kout)
c
      integer*4 kin,kout
c
      real*8 gbuf(128)
c
      integer*4 i
c
      real*8 cnv,rnum
c
      integer*2 inum(4),ityp
      integer*4 inc,nxt,jnum(2)
c
      equivalence (rnum,inum,jnum)
c
c...Only work with data records
c
      rnum = gbuf(1)
      if (jnum(2) .ne. 0) go to 8000
c
c...Get cutter type
c
      rnum = gbuf(8)
      ityp = inum(2)
c
c...Set conversion units
c
      if (kin .eq. kout) go to 8000
      cnv   = 1.0d0
      if (kin .eq. 1 .and. kout .eq. 2) cnv = 25.4d0
      if (kin .eq. 2 .and. kout .eq. 1) cnv = 1.0d0 / 25.4d0
c
c...Convert record
c......CUTTER
c
      gbuf(9) = gbuf(9) * cnv
      gbuf(10) = gbuf(10) * cnv
      gbuf(11) = gbuf(11) * cnv
      if (ityp .lt. 10) gbuf(13) = gbuf(13) * cnv
c
c......CUTTER/DISPLY
c
      gbuf(17) = gbuf(17) * cnv
      gbuf(18) = gbuf(18) * cnv
      gbuf(19) = gbuf(19) * cnv
      gbuf(21) = gbuf(21) * cnv
      gbuf(27) = gbuf(27) * cnv
      gbuf(65) = gbuf(65) * cnv
      gbuf(66) = gbuf(66) * cnv
      gbuf(67) = gbuf(67) * cnv
c
c......CUTTER/SHANK
c
      gbuf(69) = gbuf(69) * cnv
      gbuf(70) = gbuf(70) * cnv
      gbuf(71) = gbuf(71) * cnv
      gbuf(72) = gbuf(72) * cnv
c
c......Load Tool Parameters
c
      inc    = 4
      nxt    = 33
      do 200 i=1,20,1
          inc    = inc    + 1
          if (inc .gt. 4) then
              rnum   = gbuf(nxt)
              nxt    = nxt    + 1
              inc    = 1
          endif
          if (inum(inc) .eq. -2) gbuf(38+i) = gbuf(38+i) * cnv
  200 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ajtool (kbuf)
c
c   FUNCTION:  This routine updates a tool library record from an old
c              format to the current format.
c
c   INPUT:  kbuf    I*4  D128  -  Tool Library record to be converted.
c
c   OUTPUT: kbuf    I*4  D128  -  Converted Tool Library record.
c
c***********************************************************************
c
      subroutine ajtool (kbuf)
c
      integer*4 kbuf(128)
c
      integer*2 ibuf(256),i,pbuf(6)
      integer*4 jbuf(128)
c
      real*8 rbuf(64),tbuf(6)
c
      equivalence (jbuf,ibuf,rbuf)
c
c...Copy record to local buffer
c
      do 100 i=1,128,1
          jbuf(i) = kbuf(i)
  100 continue
c
c...Store cutter type
c
      ibuf(30) = 1
      if (rbuf(13) .ne. 0. .or. rbuf(14) .ne. 0.) then
          ibuf(30) = 2
      else if (rbuf(12) .gt. 0.) then
          ibuf(30) = 3
      else if (rbuf(12) .lt. 0.) then
          ibuf(30) = 4
      endif
c
c...Store new flags
c
      ibuf(32) = 1
      ibuf(104) = 0
      if (ibuf(127) .gt. 0) ibuf(104) = 1
c
c...Pseudo Cutter defined
c
      ibuf(31) = 0
      if (ibuf(63) .gt. 0) then
          ibuf(31) = 1
          do 200 i=1,6,1
              tbuf(i) = rbuf(i+16)
              rbuf(i+16) = rbuf(i+8)
              rbuf(i+8) = tbuf(i)
c
              pbuf(i) = ibuf(i+88)
              ibuf(i+88) = ibuf(i+56)
              ibuf(i+56) = pbuf(i)
  200     continue
      endif
c
c...Restore buffer
c
      do 300 i=1,128,1
          kbuf(i) = jbuf(i)
  300 continue
      return
      end
