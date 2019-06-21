C**********************************************************************
C*    NAME         :  watrln.f
C*       CONTAINS:
c*
c*                 wpock   wpock1  wpock2  uerror2  wentry  wlevseq
c*
C*    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       watrln.f , 25.1
C*     DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:54
C**********************************************************************
C
c**********************************************************************
c*  subroutine name: wpock
c*
c*  purpose of subroutine: pass data to razpok for a plane-based waterline
c*    
c*    PARAMETERS
c*       INPUT  :
c*          botkey  - key of the bottom plane
c*          topdis  - top distance parameter
c*          itim    - level number, starts at 0
c*          nlops   - number of contours, including perimeter
c*          offpr   - OFF,PART flag
c*       OUTPUT :
c*          lmotion   - true iff some motion occured
c*          ier       - error or warning returned by razpok
c*********************************************************************
 
      subroutine wpock (botkey,topdis,itim,nlops,offpr,lmotion,ier)

      include 'com.com'
      include 'rrdm.com'

      integer*4 botkey
      real*8 topdis
      integer*2 nlops,lmotion,ier,itim,offpr
      real*4 snumlv

      integer*2 nwds,itsk,jx,offpr0
      real*8 sclev

      integer*2 isvasw(480)
      equivalence (rsvasw, isvasw)

      offpr0 = offprt
      if (offpr.eq.0) offprt = 0

      ifl(2) = 0
      isc10(1) = 738
      isc10(2) = 3
      isc10(3) = nlops
      isc10(4) = 0
      snumlv = numlvl
      numlvl = -1
      sclev = clrlvl
      if (cltyp.eq.0) clrlvl = clrlvl + itim*topdis 
c
c..... set IN/OUT for perimeter and islands to default(s): IN for perimeter,
c..... OUT for islands
c
      do 120 jx=1,nlops
        rsvasw(jx) = 0
        isvasw(jx*4-1) = 1
120   continue

      nwds = 4
      call ptdsc3(botkey,nwds,PLANE,sc(11))

      sc(12) = topdis

      itsk = 2
      call razpok(itsk)
      if (ifl(2) .eq. 13) then
        ifl(2) = 0
      else
        lmotion = 1
      endif
      ier = ifl(2)
      if (ifl(2).lt.0) then
c
c..... if batch put the warning into the pr file; in all cases
c..... reset and continue
c
        if (ifl(35).eq.1) then
          isvinx = 0
          call error(ifl(2))
        endif
        ifl(2) = 0
      endif
      numlvl = snumlv
      clrlvl = sclev
      offprt = offpr0

      return
      end

c**********************************************************************
c*  subroutine name: wpock1
c*
c*  purpose of subroutine: pass data to razpok for a surface-based waterline
c*    
c*    PARAMETERS
c*       INPUT  :
c*          asw     - key of the bottom plane
c*          topdis  - top distance parameter
c*          itim    - level number, starts at 0
c*          nlops   - number of contours, including perimeter
c*          offpr   - OFF,PART flag
c*          sc4     - initial tool axis vector
c*       OUTPUT :
c*          lmotion   - true iff some motion occured
c*          ier       - error or warning returned by razpok
c*********************************************************************
 
      subroutine wpock1 (asw,topdis,itim,nlops,offpr,sc4,lmotion,ier)

      include 'com.com'
      include 'rrdm.com'

      real*8 asw,topdis,sc4(3)
      integer*2 nlops,lmotion,ier,itim,offpr
      real*4 snumlv

      integer*2 itsk,jx,offpr0
      real*8 sclev

      integer*2 isvasw(480)
      equivalence (rsvasw, isvasw)

      offpr0 = offprt
      if (offpr.eq.0) offprt = 0

      ifl(2) = 0
      isc10(1) = 738
      isc10(2) = 3
      isc10(3) = nlops
      isc10(4) = 0
      snumlv = numlvl
      numlvl = -1
      sclev = clrlvl
      if (cltyp.eq.0) clrlvl = clrlvl + itim*topdis 
c
c..... set IN/OUT for perimeter and islands to default(s): IN for perimeter,
c..... OUT for islands
c
      do 120 jx=1,nlops
        rsvasw(jx) = 0
        isvasw(jx*4-1) = 1
        isvasw(jx*4) = 21
120   continue

      tamode = 1
      call ncl_nul_uv
      sc(11) = asw
      sc(12) = topdis

      sc(4) = sc4(1)
      sc(5) = sc4(2)
      sc(6) = sc4(3)

      itsk = 2
      call razpok(itsk,ilev)
      if (ifl(2) .eq. 13) then
        ifl(2) = 0
      else
        lmotion = 1
      endif
      ier = ifl(2)
      if (ifl(2).lt.0) then
c
c..... if batch put the warning into the pr file; in all cases
c..... reset and continue
c
        if (ifl(35).eq.1) then
          isvinx = 0
          call error(ifl(2))
        endif
        ifl(2) = 0
      endif
      numlvl = snumlv
      clrlvl = sclev
      offprt = offpr0

      return
      end
C
c**********************************************************************
c*  subroutine name: wpock2
c*
c*  purpose of subroutine: pass data to vmill2 for a plane-based waterline
c*    
c*    PARAMETERS
c*       INPUT  :
c*          botkey  - bottom plane key
c*          topdis  - top distance parameter
c*          itim    - level number, starts at 0
c*          nlops   - number of contours, including perimeter
c*          offpr   - OFF,PART flag
c*       OUTPUT :
c*          lmotion   - true iff some motion occured
c*          ier       - error or warning returned by razpok
c*********************************************************************
 
      subroutine wpock2 (botkey,topdis,itim,nlops,offpr,lmotion,ier)

      include 'com.com'
      include 'rrdm.com'
      include 'vmpcom.com'
c
      integer*4 botkey
c
      real*8 topdis,svpas1
      integer*2 nlops,lmotion,ier,itim,offpr
      real*4 snumlv

      integer*2 nwds,jx,offpr0
      real*8 sclev,asw

      integer*2 isvasw(480)
      equivalence (rsvasw, isvasw)

c
c...Initialize VMPOCK command
c
      ifl(2) = 0
      isc10(1) = 765
      isc10(2) = 3
      isc10(3) = nlops
      isc10(4) = 0
      svpas1 = VMPPAS(1)
      VMPPAS(1) = -1
      snumlv = numlvl
      numlvl = -1
      sclev = VMPCLF(1)
      if (IVMPFL(2) .eq. 1) VMPCLF(1) = VMPCLF(1) + itim*topdis 
      offpr0 = offprt
      if (offpr.eq.0) offprt = 0
c
c...Store VMPOCK parameters
c
      VMPTHK(1) = sc(23)
      VMPTHK(2) = sc(24)
      nwds = 4
      call ptdsc3 (botkey,nwds,PLANE,asw)
      call gtplt (asw,0,VMPBPL)
      IVMPFL(3) = 1
      VMPTPL(1) = topdis
      VMPTPL(4) = VMPBPL(4) + topdis
c
c.....Waterline geometry is input as curves
c
      do 120 jx=1,nlops
        rsvasw(jx) = 0
        isvasw(jx*4-1) = 1
120   continue

c
c.....Perform VoluMill pocket
c
      call vmill2
      if (ifl(2) .eq. 0) lmotion = 1
      ier = 0
      if (ifl(2).lt.0) then
c
c..... if batch put the warning into the pr file; in all cases
c..... reset and continue
c
        if (ifl(35).eq.1) then
          isvinx = 0
          call error(ifl(2))
        endif
      else
        ifl(2) = 0
        err = .false.
      endif
      VMPCLF(1) = sclev
      numlvl = snumlv
      VMPPAS(1) = svpas1
      offprt = offpr0

      return
      end
C
c**********************************************************************
c*  subroutine name: wpock3
c*
c*  purpose of subroutine: pass data to vmill2 for a plane-based waterline
c*    
c*    PARAMETERS
c*       INPUT  :
c*          botkey  - bottom plane key
c*          topdis  - top distance parameter
c*          itim    - level number, starts at 0
c*          nlops   - number of contours, including perimeter
c*          offpr   - OFF,PART flag
c*       OUTPUT :
c*          lmotion   - true iff some motion occured
c*          ier       - error or warning returned by razpok
c*********************************************************************
 
      subroutine wpock3(botkey,topdis,dis,itim,nlops,offpr,lmotion,ier)

      include 'com.com'
      include 'rrdm.com'
      include 'vmpcom.com'
c
      integer*4 botkey
c
      real*8 topdis,svpas1,dis
      integer*2 nlops,lmotion,ier,itim,offpr
      real*4 snumlv
      logical*2 twrk

      integer*2 nwds,jx,offpr0
      real*8 sclev,asw

      integer*2 isvasw(480)
      equivalence (rsvasw, isvasw)

c
c...Initialize VMPOCK command
c
      ifl(2) = 0
      isc10(1) = 765
      isc10(2) = 3
      isc10(3) = nlops
      isc10(4) = 0
      svpas1 = VMPPAS(1)
      snumlv = numlvl
      numlvl = -1
      sclev = VMPCLF(1)
      ivmfl2 = IVMPFL(2)
      if (IVMPFL(2) .eq. 1) VMPCLF(1) = VMPCLF(1) + itim*topdis 
      offpr0 = offprt
      if (offpr.eq.0) offprt = 0
c
c...Store VMPOCK parameters
c
      VMPTHK(1) = sc(23)
      VMPTHK(2) = sc(24)
      nwds = 4
      twrk = lwrk
      lwrk = .false.
      call ptdsc3 (botkey,nwds,PLANE,asw)
      call gtplt (asw,0,VMPBPL)
      lwrk = twrk
      IVMPFL(3) = 2
      do 120 jx=1,4
        VMPTPL(jx) = VMPBPL(jx)
        if (IVMPFL(2).lt.2) then
            VMPCLF(jx) = VMPBPL(jx)
        endif
120   continue
      VMPTPL(4) = topdis + VMPBPL(4)
      if (IVMPFL(2).lt.2) VMPCLF(4) = dis + VMPTPL(4)
      IVMPFL(2) = 2
c
c.....Perform VoluMill pocket
c
      call vmill2
      if (ifl(2) .eq. 0) lmotion = 1
      ier = ifl(2)
      err = .false.
      if (ifl(2).lt.0) then
c
c..... if batch put the warning into the pr file; in all cases
c..... reset and continue
c
        if (ifl(35).eq.1) then
          isvinx = 0
          call error(ifl(2))
        endif
      else if (ifl(2).gt.0) then
        call error(ifl(2))
      endif
      IVMPFL(2) = ivmfl2
      VMPCLF(1) = sclev
      numlvl = snumlv
      VMPPAS(1) = svpas1
      offprt = offpr0

      return
      end


c**********************************************************************
c*  subroutine name: wlevseq (itim)
c*
c*   FUNCTION:  Add SEQUNC/wlev1,2,3... or SEQUNC/END statements to the CL-file.
c*
c*    PARAMETERS
c*       INPUT  :
c*          itim         - level number, or -1 at the end
c*       OUTPUT : none
c*    RETURNS      : none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c***********************************************************************
 
      subroutine wlevseq (itim)

      include 'com.com'
      include 'rrdm.com'

      integer*2 itim

      character*20 cbuf
      character*10 lbuf
      integer*2 knc,i

      if (itim .lt. 0) then
        cbuf = 'END'
      else
        cbuf = 'WLEV'
        knc = 4
c
c..... we label the first level (itim = 0) as WLEV1, and so on
c
        write (lbuf,130) itim+1
  130 format (i10)
        do 140 i=1,10,1
          if (lbuf(i:i) .ne. ' ') then
            knc    = knc    + 1
            cbuf(knc:knc) = lbuf(i:i)
          endif
  140   continue
        knc    = knc    + 1
        cbuf(knc:) = ' '
      endif

      call sequnc(1,cbuf)

      return
      end

C **********************************************************************
C **********************************************************************
C **  subroutine name: uerror2(str)
C **
C **  purpose of subroutine: add a string to expnam.
C **********************************************************************
C **********************************************************************
 
      subroutine uerror2(str)
 
      include 'com.com'
 
      character*20 str
      integer*4 i,numc,strlen1

      numc = strlen1(str)
     
      do 222 i=1,numc
222   errcom(i:i) = str(i:i)

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine name:  wentry (loops,ier)
c **                                                                  
c **  purpose of subroutine: check if a small pocket's first offset loop
c **                         gouges the perimeter
c **
c **                         ier = 13 - perimeter is gouged
c **
c **********************************************************************
c **********************************************************************

      subroutine wentry (loops,ier)

      include 'com.com'
      include 'pokcom.com'

      integer*4 loops,ier

      integer*4 jj, kk
      real*8 xx,yy,x0,y0,x1,y1,x4,y4,crad
      real*4 ro,a0,b0,a1,b1,rr,xc,yc
      integer*2 ix,klok,i,jcut

      ix = locapo
      kk = loca(ix)

      call rrget(kk,xx,yy,ier)
      if (ier .ne. 0) goto 999
      a0 = xx
      b0 = yy

      jj  = kk+1
      call rrget(jj,x1,y1,ier)
      if (ier .ne. 0) goto 999
      if (x1 .ge. 0.9*flarc) then
        jj  = kk+4
        call rrget(jj,x4,y4,ier)
        if (ier .ne. 0) goto 999
        a1 = x4
        b1 = y4
      else
        a1 = x1
        b1 = y1
      endif

      crad = sc(28)/2.+sc(24)
      tol = sc(27)
c
c..... fsr 60979 fix
c
      if (loops.eq.1 .and. sc(169).ge.9.549d0) tol = tol*2
      if (ifl(264) .eq. 0) then
        crad = crad*25.4
        tol = tol*25.4
      endif
      ro = crad - tol

        jcut = 0
        i = 1
        jj = 1

2215    if (jj .eq. loca(i)) then
          call rrget (jj,x0,y0,ier)
          if (ier .ne. 0) goto 999
        else
          x0 = x1
          y0 = y1
        endif
        jj = jj + 1
        call rrget (jj,x1,y1,ier)
        if (ier .ne. 0) goto 999

        if (x1 .lt. 0.9*flarc) then
            call chkseg(a0,b0,a1,b1,ro,x0,y0,x1,y1,jcut)
        else
          jj = jj + 1
          call rrget (jj,x1,y1,ier)
          if (ier .ne. 0) goto 999
          rr = x1
          klok = y1
          jj = jj + 1
          call rrget (jj,x1,y1,ier)
          if (ier .ne. 0) goto 999
          xc = x1
          yc = y1
          jj = jj + 1
          call rrget (jj,x1,y1,ier)
          if (ier .ne. 0) goto 999
          call chkarc(a0,b0,a1,b1,ro,x0,y0,x1,y1,rr,xc,yc,klok,jcut)
        endif

        if (jcut .eq. 1) then
          ier = 13
          goto 999
        endif
          
        if (jj+1 .lt. loca(i+1)) goto 2215

999   return
      end
