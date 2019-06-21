C********************************************************************/
C*    NAME         :  cutget.f
C*       CONTAINS:
C*         cutget  cutset  gdscut  getend  obcutr  obhold
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*      cutget.f , 25.3
C*    DATE AND TIME OF LAST  MODIFICATION
C*       10/27/16 , 13:48:04
C********************************************************************/
C
c***********************************************************************
c
c   SUBROUTINE:  cutget (cbuf,dbuf,icfl,sym,ssym,hsym)
c
c   FUNCTION:  Interface to C routines which returns the current cutter
c              definitions.
c
c   INPUT:  none.
c
c   OUTPUT: cbuf    R*8  D6  -  Actual cutter definition.
c
c           dbuf    R*8  D10 -  Display cutter definition.
c
c           icfl    I*2  D3  -  Cutter display flags.
c
c           sym     C*80 D1  -  Cutter display symbol.
c
c           ssym    C*80 D1  -  Shank display symbol.
c
c           hsym    C*80 D1  -  Holder display symbol.
c
c***********************************************************************
c
      subroutine cutget (cbuf,dbuf,icfl,sym,ssym,hsym)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 icfl(10)
      real*8 cbuf(6),dbuf(20)
      character*80 sym,ssym,hsym
c
      integer*4 ic
c
      real*8 rads
      real*4 asc(320)
c
      equivalence (sc,asc)
c
      integer*2 BLADE,LATHE
      PARAMETER (BLADE=191)
      PARAMETER (LATHE=700)
c
c...Return current cutter definition
c
      rads = 57.2957751d0
      call obcutr (cbuf,ic)
      if (ic .eq. 4) then
          do 50 i=6,2,-1
             cbuf(i) = cbuf(i-1)
   50     continue
			 cbuf(1) = LATHE - 10000
      else if (ic .eq. 3) then
          cbuf(6) = cbuf(4)
          cbuf(5) = cbuf(1)
          cbuf(4) = cbuf(2)
          cbuf(1) = BLADE - 10000
      endif
c
c......Display cutter
c
      dbuf(1) = dcutr(1)
      dbuf(2) = dcutr(2)
      dbuf(3) = dcutr(3)
      if (dcutr(9) .eq. 0.) then
cc          dbuf(4) = dasin(dcutr(4)) * rads
          dbuf(4) = dcutr(4)
          dbuf(5) = 0.
          dbuf(6) = 0.
      else
          dbuf(4) = dcutr(7)
          dbuf(5) = dcutr(6)
          dbuf(6) = dasin(dcutr(8)) * rads
      endif
		if (lthbit .ne. 0) then
          dbuf(9) = lthbit + 10
      else
          dbuf(8) = dcutr(9)
      endif
      do 100 i=10,20,1
          dbuf(i) = dcutr(i)
  100 continue
c
c......Cutter display flags
c
      do 200 i=1,10,1
          icfl(i) = icutfl(i)
  200 continue
c
c......Cutter symbol
c
      sym = cutsym(1)
      ssym = cutsym(2)
      hsym = cutsym(3)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cutset (cbuf,ierr)
c
c   FUNCTION:  Interface to C routines which sets the current cutter
c              definition.
c
c   INPUT:  cbuf    R*8  D6  -  Actual cutter definition.
c
c   OUTPUT: ierr    I*4  D1  -  Returns non-zero if an error occured
c                               setting the cutter parameters.
c
c***********************************************************************
c
      subroutine cutset (cbuf,ierr)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 ierr
      real*8 cbuf(6)
c
      integer*2 ksn(4)
      real*8 asn
      equivalence (asn,ksn)
c
c...Set up global parsing array with CUTTER parameters
c
      asn = 0.
      do 100 i=1,6,1
          sc(10+i) = cbuf(i)
          if (cbuf(i) .lt. -.0001 .or. cbuf(i) .gt. .0001) ksn(3) = i
  100 continue
      sc(10) = asn
c
c...Call CUTTER parsing routine
c
      ifl(2) = 0
      call cutseg
      ierr = ifl(2)
c
c...End of routine
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gdscut (cbuf,sym,ssym,hsym,symkey,icfl)
c
c   FUNCTION:  Interface to C routines which returns the current cutter
c              definitions formatted for motion display purposes.
c
c   INPUT:  none.
c
c   OUTPUT: cbuf    R*8  D10 -  Cutter definition for display.
c
c
c           sym     C*80 D1  -  Cutter display symbol.
c
c           ssym    C*80 D1  -  Shank display symbol.
c
c           hsym    C*80 D1  -  Holder display symbol.
c
c           symkey  I*4  D3  -  Unibase keys of symbols.
c
c           icfl    I*2  D10 -  Cutter display flags.
c
c***********************************************************************
c
      subroutine gdscut (cbuf,sym,ssym,hsym,symkey,icfl)
c
      include 'com8a.com'
      include 'cutter.com'
c
      real*8 cbuf(20)
c
      character*80 sym,ssym,hsym
c
      integer*4 icfl(10),symkey(3)
c
      real*8 cnv
c
      real*4 asc(320)
      equivalence (sc,asc)
c
c...Set conversion factor
c
      cnv = 1.0
      if (ifl(264).eq.1) cnv = 25.4
c
c......Store actual cutter parameters
c
      if (icutfl(1) .ne. 1) then
          cbuf(1) = sc(28) / cnv
          cbuf(2) = sc(29) / cnv
          cbuf(3) = sc(30) / cnv
          cbuf(4) = sc(31)
          cbuf(5) = asc(67) / cnv
          if (lthbit .eq. 0 .or. lthbit .eq. 4 .or. lthbit .eq. 5) then
              cbuf(6) = asc(68) / cnv
          else
              cbuf(6) = asc(68)
          endif
          cbuf(7) = asc(307) / cnv
          cbuf(8) = asc(305)
          if (lthbit .ne. 0) then
              cbuf(9) = lthbit + 10
          else
              cbuf(9) = ifl(282)
          endif
          do 400 i=10,17,1
             if ((lthbit .eq. 0).and.((i.eq.12).or.(i.eq.16))) then
                  cbuf(i) = dcutr(i)
             else
                  cbuf(i) = dcutr(i) / cnv
             endif
  400     continue
          cbuf(18) = dcutr(18)
          cbuf(19) = dcutr(19) / cnv
          cbuf(20) = dcutr(20) / cnv
c
c......Store display cutter parameters
c
      else
          do 500 i=1,17,1
             if ((lthbit .eq. 0).and.((i.eq.12).or.(i.eq.16))) then
                  cbuf(i) = dcutr(i)
             else
                  cbuf(i) = dcutr(i) / cnv
             endif
  500     continue
          cbuf(4) = dcutr(4)
          cbuf(8) = dcutr(8)
          cbuf(9) = dcutr(9)
          cbuf(18) = dcutr(18)
          cbuf(19) = dcutr(19) / cnv
          cbuf(20) = dcutr(20) / cnv
      endif
      if (lblade) cbuf(1) = -1-cbuf(1)
c
c...Store symbols
c
      sym = cutsym(1)
      ssym = cutsym(2)
      hsym = cutsym(3)
      symkey(1) = cutkey(1)
      symkey(2) = cutkey(2)
      symkey(3) = cutkey(3)
c
c...Store cutter flags
c
      do 600 i=1,10,1
          icfl(i) = icutfl(i)
  600 continue
      if (icfl(3) .eq. -1) icfl(3) = 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getend (tend)
c
c   FUNCTION:  Interface to C routines which returns the current cutter
c              location and tool axis.
c
c   INPUT:  none.
c
c   OUTPUT: tend    R*8  D6  -  Tool end point and tool axis.
c
c***********************************************************************
c
      subroutine getend (tend)
c
      include 'com8a.com'
c
      real*8 tend(6)
c
c...Return tool end point and tool axis
c
      tend(1) = sc(1)
      tend(2) = sc(2)
      tend(3) = sc(3)
      tend(4) = sc(4)
      tend(5) = sc(5)
      tend(6) = sc(6)
c
c...End of routine
c
 8000 return
      end
C
c***********************************************************************
c
c   SUBROUTINE:  obcutr (gbuf,ktyp)
c
c   FUNCTION:  Returns the current cutter parameters formatted the in
c              the same manner as they appear in the CUTTER statement.
c
c   INPUT:  none.
c
c   OUTPUT: gbuf    R*8  D6  -  Actual cutter definition.
c
c           ktyp    I*4  D1  -  1 = Standard cutter, 2 = Barrel cutter,
c                               3 = Blade cutter, 4 = Lathe cutter.
c
c***********************************************************************
c
      subroutine obcutr (gbuf,ktyp)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 ktyp
c
      real*8 gbuf(6)
c
      integer*4 iacy
c
      real*4 asc(400)
      real*8 rads
c
      equivalence (sc,asc)
c
      PARAMETER (rads=57.2957751d0)
c
c...Initialize routine
c
      iacy   = 5
c
c...Blade cutter
c
      if (lblade) then
          ktyp   = 3
          gbuf(1) = asc(68)
          gbuf(2) = asc(307)
          gbuf(3) = sc(30)
          gbuf(4) = asin(asc(305))*rads
          call dpoint(gbuf(4),gbuf(4),iacy)
          gbuf(5) = 0.
          gbuf(6) = 0.
c
c...Lathe cutter
c
      else if (lthbit .ne. 0) then
          ktyp   = 4
          gbuf(1) = sc(28) / 2.
          gbuf(2) = asc(307)
          gbuf(3) = sc(30)
          gbuf(4) = asc(305)
          gbuf(5) = asc(68)
          gbuf(6) = 0.
c
c...Barrel cutter
c
      else if (ifl(282) .eq. 1) then
          ktyp   = 2
          gbuf(1) = sc(28)
          gbuf(2) = sc(29)
          gbuf(3) = sc(30)
          gbuf(4) = asc(307)
          gbuf(5) = asc(68)
          gbuf(6) = asin(asc(305))*rads
          call dpoint(gbuf(6),gbuf(6),iacy)
c
c...Standard cutter
c
      else
          ktyp   = 1
          gbuf(1) = sc(28)
          gbuf(2) = sc(29)
          gbuf(3) = sc(30)
          gbuf(4) = sc(31)
          gbuf(5) = 0.
          gbuf(6) = 0.
      endif
c
c...End of routine
c
 8000 return
      end
C
c***********************************************************************
c
c   SUBROUTINE:  obcuds (gbuf,ktyp)
c
c   FUNCTION:  Returns the current display cutter parameters formatted the in
c              the same manner as they appear in the CUTTER/DISPLY statement.
c              The actual cutter definition will be returned if a pseudo
c              cutter is not in effect.
c
c   INPUT:  none.
c
c   OUTPUT: gbuf    R*8  D6  -  Display cutter definition.
c
c           ktyp    I*4  D1  -  1 = Standard cutter, 2 = Barrel cutter,
c                               3 = Blade cutter, 4 = Lathe cutter.
c
c***********************************************************************
c
      subroutine obcuds (gbuf,ktyp)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 ktyp
c
      real*8 gbuf(6)
c
      integer*4 iacy,istyp,ictyp
c
      real*4 asc(400)
      real*8 rads,rval(3),rlen
c
      equivalence (sc,asc)
c
      PARAMETER (rads=57.2957751d0)
c
c...Initialize routine
c
      iacy   = 5
c
c...Display cutter not defined
c
      if (icutfl(1) .eq. 0 .or. lthbit .ne. 0 .or. lblade) then
          call obcutr (gbuf,ktyp)
c
c...Command display parameters
c
      else if (icutfl(1) .eq. 1) then
          gbuf(1) = dcutr(1)
          gbuf(2) = dcutr(2)
          gbuf(3) = dcutr(3)
c
c...Standard cutter
c
          if (dcutr(9) .eq. 0) then
              ktyp   = 1
              gbuf(4) = dcutr(4)
              gbuf(5) = 0.
              gbuf(6) = 0.
c
c...Barrel cutter
c
          else
              ktyp   = 2
              gbuf(4) = dcutr(7)
              gbuf(5) = dcutr(6)
              gbuf(6) = asin(dcutr(8))*rads
              call dpoint(gbuf(6),gbuf(6),iacy)
          endif
c
c...Symbol/Geometry cutter
c
      else
          istyp  = icutfl(1)
          ictyp  = 1
          call nclf_cutter_get_bounds(cutsym(1),cutkey(1),istyp,ictyp,
     1        rval)
c
c.....replace item 3 height with profile defines if any exist
c
          call getcut_hgt(cutsym(1), rlen)
          if (rlen.ne.0) then
              rval(3) = rlen
          endif
          gbuf(1) = rval(1)
          gbuf(2) = 0.
          gbuf(3) = rval(3)
          gbuf(4) = 0.
          gbuf(5) = 0.
          gbuf(6) = 0.
      endif
c
c...End of routine
c
 8000 return
      end
C
c***********************************************************************
c
c   SUBROUTINE:  obhold (gbuf,ktyp)
c
c   FUNCTION:  Returns the current display holder parameters formatted the in
c              the same manner as they appear in the CUTTER/DISPLY statement.
c
c   INPUT:  ktyp    I*2  D1  -  1 = Obtain shank parameters, 2 = Holder.
c
c   OUTPUT: gbuf    R*8  D6  -  Display holder definition.
c
c***********************************************************************
c
      subroutine obhold (gbuf,ktyp)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*2 ktyp
c
      real*8 gbuf(6)
c
      integer*2 ics,ids
      integer*4 iacy,istyp,ictyp
c
      real*4 asc(400)
      real*8 rval(3),rlen
c
      equivalence (sc,asc)
c
c...Initialize routine
c
      iacy   = 5
      if (ktyp .eq. 1) then
          ics    = 5
          ids    = 14
      else
          ics    = 6
          ids    = 10
      endif
c
c...Shank/Holder not defined
c
      if (icutfl(ics) .eq. 0) then
          do 100 i=1,5,1
              gbuf(i) = 0.
  100     continue
c
c...Shank/Holder defined using parameters
c
      else if (icutfl(ics) .eq. 1) then
          gbuf(1) = dcutr(ids)
          gbuf(2) = dcutr(ids+1)
          gbuf(3) = dcutr(ids+2)
          if (lthbit .eq. 0) then
              gbuf(4) = dcutr(ids+3)
              gbuf(5) = 0.
          else
              gbuf(4) = 0.
              gbuf(5) = dcutr(ids+3)
          endif
c
c...Shank/Holder defined using symbol
c
      else
          istyp  = icutfl(ics)
          ictyp  = 1
          if (lthbit .ne. 0) ictyp = 2
          call nclf_cutter_get_bounds(cutsym(ktyp+1),cutkey(ktyp+1),
     1        istyp,ictyp,rval)
          if (lthbit .eq. 0) then
              gbuf(1) = rval(1)
              gbuf(2) = rval(3)
              gbuf(3) = 0.
              gbuf(4) = dcutr(ids)
          else
              gbuf(1) = rval(1)
              gbuf(2) = rval(2)
              gbuf(3) = dabs(dcutr(ids+3)-dcutr(ids+2))
              gbuf(4) = dcutr(ids)
              gbuf(5) = dcutr(ids+1)
          endif
c
c.....replace item 2 height with profile defines if any exist
c
          call getcut_hgt(cutsym(ktyp+1), rlen)
          if (rlen.ne.0) then
              gbuf(2) = rlen
          endif
      endif
c
c...End of routine
c
 8000 return
      end
