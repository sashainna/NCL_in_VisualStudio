c*******************************************************
c**    NAME           : pokobt.f
c**      CONTAINS: Pocket support routines.
c**     subroutine pokobt
c**    MODULE NAME AND RELEASE LEVEL
c**       pokobt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:26
c*****************************************************
c**
c** Copyright (C) 1993 Numercical Control Computer Sciences
c**
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pokobt
C*      Obtain pocket entry points.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine pokobt

      include 'com.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      real*8 pokasw, buf(6)
      real*8 clpla, clplb, clplc, clpld, d
      character*64 sctok, pttok
      integer*4 keypok, i4er,isvtk4,ptinx,scidx
      integer*2 i, n, ier, nwds, ietype, ilast
      integer*2 irest(4), ktv(4)
      equivalence (rest,irest)
      equivalence (tv,ktv)
      logical lup
      integer*2 UP, DOWN
      parameter (UP = 112)
      parameter (DOWN = 113)

      real*8 bbuf(32)
      real*4 abuf(64)
      integer*2 ibuf(144)
      equivalence (bbuf,abuf,ibuf)
      integer*2 jramps, jentry, icltyp, itrans, ictcom, ilvdep
      equivalence (ibuf(1),jramps),(ibuf(2),jentry),(ibuf(3),icltyp)
      equivalence (ibuf(4),itrans),(ibuf(5),ictcom),(ibuf(6),ilvdep)
      real*4 armpds,anmlvl,agenfr,aposfr,aretfr,aentfr,atrnfr,afinfr
      real*4 amxang,afrsfr
      equivalence (abuf(5),armpds),(abuf(6),anmlvl),(abuf(7),agenfr)
      equivalence (abuf(8),aposfr),(abuf(9),aretfr),(abuf(10),aentfr)
      equivalence (abuf(11),atrnfr),(abuf(12),afinfr)
      equivalence (abuf(13),afrsfr),(abuf(14),amxang)
      real*8 bcllvl, btoppl(4),bbotpl(4),bclrpl(4),bmx(12)
      equivalence (bbuf(8),bcllvl),(bbuf(9),btoppl),(bbuf(13),bbotpl)
      equivalence (bbuf(17),bclrpl),(bbuf(21),bmx)

      pokasw = tv
      call parsit
      if (ityp.ne.2 .or. ist.ne.1.and.ist.ne.2) goto 9282
c
c...Scalar (or unknown - make it a scalar)
c
      idst = 2
      keyold = keyhld
      istold = ist
      savid2 = token2
      isvsub = ivxsub
      ifl(9)  = ifl(11)
      ifl(10) = ifl(12)
      rest = 0.
      call vstore
      sctok = token2
      scidx = ivxsub
c
c...  Get name of reserved point variable to store points in.
c
      call parsit
      if (ityp.ne.2) goto 9087
      if (ist.eq.1) then
c
c...  Unknown variable
c
        isvtk4 = 1
        if (ivxsub.ne.0) then
c
c...  Subscripted unknown variable - see if it has been reserved
c
          isvtk4 = ivxsub
          ivxsub = 0
          call vstchk
          if (ist.eq.14) goto 50
          if (ist.ne.1) goto 9089
        endif
c
c...  Variable has not been reserved - reserved it.
c
        savid2 = token2
        isvsub = 0
        keyold = keyhld
        ifl(9) = ifl(11)
        ifl(10) = ifl(12)
        idst = 14
        rest = 0.
        irest(3) = 32767
        irest(4) = 14
        call vstore
        ivxsub = isvtk4
        goto 80
      endif
      if (ivxsub.eq.0) go to 60
c
c...  Known and subscripted - check subscript
c
         isvtk4 = ivxsub
         ivxsub = 0
         call vstchk
         if (ityp.ne.2.and.ist.ne.14) goto 9088
50       if (isvtk4.gt.1000000) goto 9085
         ivxsub = isvtk4
         go to 80
60       if (ist.ne.14) goto 9088
c
c...  Reserved variable - start at subscript 1
c
         ivxsub = 1

80    continue
      pttok = token2
      ptinx = ivxsub
      call gtdesc (pokasw, keypok,nwds,ietype)
c
c...  Check for UP or DOWN
c
      lup = .true.
      if (nextyp.ne.11) then
        call parsit
        if (ityp.ne.1 .or. ist.ne.UP.and.ist.ne.DOWN) goto 9393
        lup = ist.eq.UP
      endif
      if (nextyp.ne.11) goto 9004
c
c...  If UP, get clearance to project entry points onto.
c
      if (lup) then
        call pklild (keypok, i4er)
        call pkllod (bbuf, ietype, n, ilast, i4er)
        clpla = btoppl(1)
        clplb = btoppl(2)
        clplc = btoppl(3)
        clpld = btoppl(4)
      endif
c
c...  Store points
c
      call pklnpt (keypok, n)
      if (sc(169) .ge. 9.549) then
        ietype = PNTVEC
      else
        ietype = 3
      endif
      do 100 i=1,n
        call pklept (buf)
        if (lup) then
c
c..... qar 96104 fix
c
          if (sc(169) .ge. 9.549) then
            clpla = buf(4)
            clplb = buf(5)
            clplc = buf(6)
          endif
          d = clpla*buf(1)+clplb*buf(2)+clplc*buf(3)-clpld
          buf(1) = buf(1)-clpla*d
          buf(2) = buf(2)-clplb*d
          buf(3) = buf(3)-clplc*d
        endif
        call storit(buf, pttok, ptinx, ietype, 1)
        if (ifl(2).gt.0) goto 9000
        ptinx = ptinx+1
100   continue
c
c...  Store number of points in scalar variable
c
      token2 = sctok
      ivxsub = scidx
      call vstchk
      rest = n
      idst = 2
      savid2 = token2
      isvsub = ivxsub
      ifl(9) = ifl(11)
      ifl(10) = ifl(12)
      call vstore

999   return

9000  ier = ifl(2)
      goto 9998
c
c... End of statement expected
c
9004  ier = 4
      goto 9998
c
c... Subscript out of range.
c
9085  ier = 85
      goto 9998
c
c... Identifier expected
c
9087  ier = 87
      goto 9998
c
c... Identifier has not been reserved
c
9088  ier = 88
      goto 9998
c
c... Identifier currently has different type
c
9089  ier = 89
      goto 9998
c
c... Scalar variable expected.
c
9282  ier = 282
      goto 9998
c
c... UP or DOWN expected.
c
9393  ier = 393
      goto 9998

9998  call error(ier)
      return
      end
