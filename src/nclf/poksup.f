C*********************************************************************
c**    NAME           : poksup.f
c**      CONTAINS: Pocket support routines.
c**     subroutine tltosf (te, ta, tp, sn, partsf)
c**     subroutine psinit (asw, te, ta)
c**     subroutine dopock (asw)
c**     subroutine pokuvc (x,y,z,ox,oy,oz,dx,dy,dz,sec)
c**     subroutine pokhdr (trans, mx, ier)
c**     subroutine pokout (istep,npts,buf,ilast,icirc,cdat)
c**     subroutine pokfed (fout, fhld, mtype, ier)
c**     subroutine cyclrc
c**     subroutine pokdbg
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       poksup.f , 25.2
c**    DATE AND TIME OF LAST MODIFICATION
c**       10/27/16 , 13:58:24
C*********************************************************************
c**
c** Copyright (C) 1993 Numerical Control Computer Sciences
c**
C*********************************************************************
C*    E_SUBROUTINE     : subroutine tltosf (te, ta, tp, sn, partsf)
C*      Move the tool down the tool axis to the part surface.
C*    PARAMETERS
C*       INPUT  :
C*          te       - Tool end point.
C*          ta       - Tool axis.
C*          partsf   - Part Surface ID (sc(11) if called from razpok,
C*                                      sc(144) if from motimm).
C*       OUTPUT :
C*          te       - New tool end point.
C*          tp       - Tool contact point on surface.
C*          sn       - Surface normal at contact point.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine tltosf (te, ta, tp, sn, partsf)

      include 'com.com'
      include 'mocom.com'

      real*8 te(3), ta(3), tp(3), sn(3), partsf

c          motion common equivalences
      real*4 ad(300)
      integer*2 kd(600)
      equivalence (d,ad,kd)
      real*4 asc(100), sbe, dsk
      equivalence (sc,asc),(asc(63),sbe),(asc(66),dsk)

      real*8 pcon, pco, psi, svco, dst, ptol
      real*8 xte, yte, zte, xta, yta, zta, xfw, yfw, zfw
      integer*4 nclkey
      integer*2 i, iknt, isv23, nwds, ietype
      integer*2 kmax
      parameter (kmax=200)
      integer*2 isc144(4)
      equivalence (sc(144),isc144)
      equivalence (sc(167),ptol)
      integer*2 ia
      equivalence (ifl(51),ia)

      integer*2 jtry
      real*8 xtej,ytej,ztej

      jtry = 0
      call fwdini

      xfw = t(7,ia)
      yfw = t(8,ia)
      zfw = t(9,ia)

      isv23 = ifl(23)
      ifl(23) = 0
      call gtdesc (partsf,nclkey,nwds,ietype)
      xte = te(1)
      yte = te(2)
      zte = te(3)
      xta = ta(1)
      yta = ta(2)
      zta = ta(3)
      iknt = 0
100   t(1,ia) = xte
      t(2,ia) = yte
      t(3,ia) = zte

      if (ietype.eq.9) then
        call psrel
c
c..... qar 95073 - if a bad surface point try to project from a slightly
c..... different position
c
        if (ifl(2) .gt. 0) then
          if (ifl(2).eq.128 .and. jtry.lt.4) then
            if (jtry .eq. 0) then
              xtej = xte
              ytej = yte
              ztej = zte
              xte=xte-ptol*xta
              yte=yte-ptol*yta
              zte=zte-ptol*zta
            else if (jtry .eq. 1) then
              xte=xtej+ptol*xta
              yte=ytej+ptol*yta
              zte=ztej+ptol*zta
            else if (jtry .eq .2) then
              xte=xtej+ptol*xfw
              yte=ytej+ptol*yfw
              zte=ztej+ptol*zfw
            else if (jtry .eq .2) then
              xte=xtej-ptol*xfw
              yte=ytej-ptol*yfw
              zte=ztej-ptol*zfw
            endif
            jtry = jtry + 1
            ifl(2) = 0
            goto 100
          else
            goto 999
          endif
        else if (jtry .gt. 0) then
          xte = xtej
          yte = ytej
          zte = ztej
          jtry = 0
        endif

      endif
      iknt = iknt+1
      if (iknt.gt.kmax) goto 9138
      pco = xta*s(1,1) + yta*s(2,1) + zta*s(3,1)
c
c...  If sf normal opposes tool axis, reverse surface direction
c...  and try again
c
      if (pco.lt.0.0 .and. ietype.eq.9) then
        ad(2) = -ad(2)
        goto 100
      endif
c
c...  Calculate distance down tool axis to plane on surface returned from
c...  psrel in s(,1) array
c
      if (pco.gt.1.) pco = 1.
      psi = dsqrt(1.-pco**2)
      pcon = s(4,1)+sc(23)+tool(2)*(1.-pco)+tool(6)*psi
      if (dsk.eq.1. .and. pco.lt.sbe)
     x  pcon = s(4,1)+sc(23)+psi*tool(1)/2.-pco*tool(3)
      svco = pco
      if (pco.lt..2) pco = .2
      dst = (xte*s(1,1)+yte*s(2,1)+zte*s(3,1)-pcon)/pco
c
c...  If sf normal is perp to tool axis, try going down tool axis
c
      if (dabs(svco).lt..001) dst = dabs(dst)
      xte=xte-dst*xta
      yte=yte-dst*yta
      zte=zte-dst*zta
c
c...  If distance moved is greater than positional tol, go again.
c
      if (dabs(dst).gt.ptol) goto 100

      te(1) = xte
      te(2) = yte
      te(3) = zte
      do 220 i=1,3
      tp(i) = s(i+4,1)
220   sn(i) = s(i,1)
      goto 999

9138  ifl(2) = 138
      goto 999

999   ifl(23) = isv23
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine psinit (asw,te,ta)
C*      Initialize the part surface.
C*    PARAMETERS
C*       INPUT  :
C*          asw      - ASW of part surface
C*          te       - Tool end point.
C*          ta       - Tool axis.
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine psinit (asw,te,ta)

      include 'com.com'
      include 'mocom.com'
      include 'suvcom.com'

      real*8 asw, te(3), ta(3)
c
c..... motion common equivalences
c
      real*4 ad(300)
      integer*2 kd(600)
      equivalence (d,ad,kd)

      real*4 asc(100), dsk
      equivalence (sc,asc),(asc(66),dsk)

      integer*2 ia, isrf
      equivalence (ifl(51),ia),(ifl(54),isrf)

      integer*4 kcps
      equivalence (ifl(337),kcps)

      real*8 sfa, sfb, sfc,tol,crv_tol, f_dot
      real*4 u, v
      integer*4 nclkey,kps,kds
      integer*2 nwds, ietype, imult
      integer*4 ncl_check_key

      logical lv97

      call getsct (tol)

      ia = 3
      isrf = 1
      lv97 = sc(169) .lt. 9.749d0

      call conv8_4 (te,t(1,ia),3)
      call conv8_4 (ta,t(4,ia),3)
      call xyzvc4  (0.,0.,0.,t(7,ia))

      ifl(56) = 0
c
c...  If disk or flat bottom cutter, set ps calc level to 1 for psrela
c
      if (dsk.ne.0. .or. tool(6).ge.2.*tol) then
         ifl(56) = 1
         call xyzvc4 (0.,0.,0.,t(16,ia))
      endif
c
c..... get ps into d-tbl.   record this ps in sc(35)
c
      fautops = .false.
      sc(35) = asw
      call gtdesc (asw, nclkey, nwds, ietype)
c
c..... bra on pl or sf
c
      if(ietype.eq.PLANE) then
         call gtgeom (asw,d(3),nclkey,nwds,ietype)
         kd(1) = PLANE
         d(2)  = asw

         if( f_dot(ta(1),d(3)) .le. 0.) then
            call mnvc (d(3))
            d(6) = - d(6)
         endif
c
c..... add this pl to srf(isrf)
c
         call conv8_4 (d(3),s(1,1),4)
         return
      endif
c
c..... surf or CVonSF
c
      sc(144) = asw
      ad(2)   = 1.
c
c..... use ps 1st-look pt
c
      s(8,1)  = te(1)+ta(1)*tool(4)
      s(9,1)  = te(2)+ta(2)*tool(4)
      s(10,1) = te(3)+ta(3)*tool(4)

      u=psu
      v=psv
c
c...Make sure initial U & V are inside trimmed surface
c
		if (.not. lv97) call nclf_trimsrf_ckuv (nclkey,u,v)
c
c... aak 13-mar-1998: enable CVonSF as PS in go/sf1,sf2
c
      if (isrf.eq.1 .and. ietype.eq.CURVE) then
         kps = nclkey 
         crv_tol = 0.2*tol

         ifl(2) = ncl_psmult_init (kps,crv_tol,imult,1)
         if (ifl(2).ne.0 .and. ifl(2).ne.466) ifl(2) = 122
         psmult = imult.eq.1.and.cvoneps
         if (psmult) call ncl_cvonsf_free
         kcps = nclkey
         if (ifl(2).ne.0) return

         call gtdesc (sc(12),kds,nwds,ietype)
         ifl(2) = ncl_check_key (kds,kps)
         if (ifl(2).ne.0) return

         if(.not.psmult) then
            call ncl_cvonsf_init (kds,kps,1,crv_tol,ifl(2))
         else
            call ncl_cvonsf_init_mult (kds,kps,1,crv_tol,ifl(2))
         endif

         if (ifl(2) .ne. 0) return
      else
         call sfinit (asw, isrf, u, v)
      endif

      call surfpn (u,v,1)
      if (ifl(2).gt.0) return

      t(13,3) = u
      t(14,3) = v
      sfa = s(1,1)
      sfb = s(2,1)
      sfc = s(3,1)
c
c..... if psnorm not up, reverse direc(1) and ps tanpl.
c
      if(ta(1)*sfa+ta(2)*sfb+ta(3)*sfc.gt.0.) return

      ad(2)=-ad(2)
      call mnvc4 (s(1,1))
      s(4,1) = -s(4,1)

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dopock (keypok)
C*      Execute a pocket saved in the pocket lists.
C*    PARAMETERS
C*       INPUT  :
C*          keypok   - Key (memory address) of pocket.
C*       OUTPUT :
C*          none.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine dopock (keypok)

      include 'com.com'
      include 'const.com'
      include 'rrdm.com'
      include 'comgt.com'
c
      integer*4 keypok

      real*8 buf(120), cdat(44), feda(7), fhld, ftmp, sc123, feedc3
      real*8 x,y,z, ox,oy,oz, dx,dy,dz, odx,ody,odz, sec, co, hco
      real*8 fx, fy, fz, ptl(3), tmp8
      integer*4 ier, mupt
      integer*2 i, istep, itype, ilast, lsthld, npts, ihld, icirc
      integer*2 ixcir, ipt, ix, ixb, n, ixfr, ifl314, ifl315, itmp(4)
      real*8 bcyc2(120)
      integer*2 ncyc2
      equivalence (itmp,tmp8)

C   ------   FIX   ------
      real*8 FEEDR
      equivalence (FEEDR, MOTMAP(24))

      real*8 bbuf(32)
      real*4 abuf(64)
      integer*2 ibuf(144)
      equivalence (bbuf,abuf,ibuf)
      integer*2 jramps, jentry, icltyp, itrans, ictcom, ictcm, ilvdep
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

      logical lv92,lv94,l1lst

      integer*2 izro, i2v1, i2v2
      data izro /0/, i2v1 /1/, i2v2 /2/

      hco = slwang/RADIAN
      hco = cos(hco)
      if (ifl(314).eq.0 .and. ifl(315).eq.0) hco = 2.0d0

      ihld  = 0
      icirc = 0
      ixcir = 0
      ncyc2 = 0
      ifl314 = ifl(314)
      ifl315 = ifl(315)
      sc123 = sc(123)
      feedc3 = FEEDC(3)
      lv92 = sc(169) .lt. 9.249d0
      lv94 = sc(169) .lt. 9.449d0

      if (lv92) then
          mupt = 3*(ifl(82)+1)
      else
          mupt = 21
      endif
      call ncl_zroptr(nrcn)
      ifl(130) = 0
cc      ifl(270) = -1
      istep = 0
      ptl(1) = sc(1)
      ptl(2) = sc(2)
      ptl(3) = sc(3)
c
c...  Initialize loading of this pocket list.
c
      call pklild (keypok, ier)
      if (ier.gt.0) goto 888
c
c...  Load pocket header
c
      call pkllod (bbuf, itype, npts, ilast, ier)
      if (ier.gt.0) goto 888
      ictcm = 0
      l1lst = .true.
c
c...  Set up feed rates.
c
      fhld = agenfr
      if (fhld.eq.0.) fhld = sc123
      feda(1) = fhld
      feda(2) = aposfr
      if (feda(2).lt.0.) feda(2) = dabs(fhld*feda(2))
      feda(3) = aretfr
      if (feda(3).lt.0.) feda(3) = dabs(fhld*feda(3))
      feda(4) = aentfr
      if (feda(4).eq.0.) feda(4) = fhld
      if (feda(4).lt.0.) feda(4) = dabs(fhld*feda(4))
      feda(5) = atrnfr
      if (feda(5).eq.0.) feda(5) = fhld
      if (feda(5).lt.0.) feda(5) = dabs(fhld*feda(5))
      feda(6) = afinfr
      if (feda(6).eq.0.) feda(6) = fhld
      if (feda(6).lt.0.) feda(6) = dabs(fhld*feda(6))
      feda(7) = afrsfr
      if (feda(7).lt.0.) feda(7) = dabs(fhld*feda(7))
c      FEEDC(3) = feda(6)
      fhld = sc(123)

      call ptopen (nrcn, mupt)
      do 10 i=1,6
10    FROMSV(i) = sc(i)
      ox = sc(1)
      oy = sc(2)
      oz = sc(3)

      ilast = 0
100   if (ilast.eq.2) goto 900
c
c...  Load next element.
c
      lsthld = ilast
      call pkllod (buf, itype, npts, ilast, ier)
      if (l1lst .and. ilast.ne.2) l1lst = .false.
      if (ictcom.gt.1 .and. (ier.eq.-6 .or. l1lst)) then
        ictcm = 1
        call putcl(2000,1007,jctcom,rct)
      endif
      if (ier.gt.0) goto 888
      if (itype.gt.1) goto 140
c
c... Motion with or without tool axis
c
        if (ixfr.ne.6.or.hco.gt.1.0) then
          if (ihld.eq.1) then
            x = buf(1)
            y = buf(2)
            z = buf(3)
            call pokuvc (x,y,z,ox,oy,oz,dx,dy,dz,sec)
            co = dx*odx+dy*ody+dz*odz
            ifl(314) = 0
            if (co.lt.hco) ifl(314) = ifl314
            call pokout (istep,izro,buf,izro,icirc,cdat(ixcir+1),ptl)
          endif
          ix = npts*6-11
          if (npts.gt.1) then
            ox = buf(ix)
            oy = buf(ix+1)
            oz = buf(ix+2)
          endif
          ix = ix+6
          x = buf(ix)
          y = buf(ix+1)
          z = buf(ix+2)
          call pokuvc (x,y,z,ox,oy,oz,dx,dy,dz,sec)

          icirc = 0
          ifl(314) = 0
          ifl(315) = 0
          call pokout (istep,npts,buf,ilast,icirc,cdat(ixcir+1),ptl)
          ihld = 0
          if (ilast.eq.2 .and. ictcm.eq.1 .and. jctcom.gt.1) then
            call putcl(2000,1007,2,rct(15))
            ictcm = 0
          endif
          ox = x
          oy = y
          oz = z
          odx = dx
          ody = dy
          odz = dz
          if (ncyc2 .gt. 0) then
            call putcl(2000,1054,ncyc2,bcyc2)
            ncyc2 = 0
          endif
          goto 100
        endif
c
c...  Final pass. Check for secondary feed rate.
c
        x = buf(1)
        y = buf(2)
        z = buf(3)
        call pokuvc (x,y,z,ox,oy,oz,dx,dy,dz,sec)
        co = dx*odx+dy*ody+dz*odz
c
c...  If last motion was held, output now with slow down feed rate flag set
c...  appropriately.
c
        if (ihld.eq.1) then
          ifl(314) = 0
          if (co.lt.hco) ifl(314) = ifl314
          call pokout (istep,izro,buf,izro,icirc,cdat(ixcir+1),ptl)
        endif
        ifl(315) = 0
        if (co.lt.hco) ifl(315) = ifl315
        icirc = 0
        ox = x
        oy = y
        oz = z
        odx = dx
        ody = dy
        odz = dz
        ipt = 1
        n = 0
        ix = 1
        ixb = 1
c
c...  Loop through points and output to cl when angle exceeds angle specified 
c...  in pokmod statement.
c
110     ipt = ipt+1
        if (ipt.gt.npts) goto 120
        n = n+1
        ixb = ixb+6
        x = buf(ixb)
        y = buf(ixb+1)
        z = buf(ixb+2)
        call pokuvc (x,y,z,ox,oy,oz,dx,dy,dz,sec)
        co = dx*odx+dy*ody+dz*odz
        if (co.lt.hco) then
          ifl(314) = ifl314
          call pokout (istep, n, buf(ix), izro, izro, cdat, ptl)
          ifl(315) = ifl315
          ix = ixb
          n = 0
        endif
        odx = dx
        ody = dy
        odz = dz
        ox = x
        oy = y
        oz = z
        goto 110
120     continue
c
c...  Buffer up last point
c
        n=n+1
        call pokout (istep, n, buf(ix), i2v1, izro, cdat, ptl)
        ihld = 1
        goto 100

140   if (itype.gt.2) goto 160
c
c.. Circular record
c
        ixcir = 22-ixcir
        do 150 i=1,7
150     cdat(ixcir+i) = buf(i)
        call pkllod (buf, itype, npts, ilast, ier)
        if (ier.gt.0) goto 888
        if (itype.gt.1) goto 888
        cdat(ixcir+8) = ox
        cdat(ixcir+9) = oy
        cdat(ixcir+10) = oz
        cdat(ixcir+11) = cdat(ixcir+4)
        cdat(ixcir+12) = cdat(ixcir+5)
        cdat(ixcir+13) = cdat(ixcir+6)
        dx = ox-cdat(ixcir+1)
        dy = oy-cdat(ixcir+2)
        dz = oz-cdat(ixcir+3)
        fx = cdat(ixcir+5)*dz-cdat(ixcir+6)*dy
        fy = cdat(ixcir+6)*dx-cdat(ixcir+4)*dz
        fz = cdat(ixcir+4)*dy-cdat(ixcir+5)*dx
        sec = dsqrt(fx**2+fy**2+fz**2)
        x = buf(1)
        y = buf(2)
        z = buf(3)
        dx = x-ox
        dy = y-oy
        dz = z-oz
        co = fx*dx+fy*dy+fz*dz
        if (co.lt.0.) sec = -sec
        if (sec.ne.0.d0) then
          cdat(ixcir+14) = fx/sec
          cdat(ixcir+15) = fy/sec
          cdat(ixcir+16) = fz/sec
        endif
        if (ixfr.eq.6 .and. hco.le.1.) then
          sec = dsqrt(dx**2+dy**2+dz**2)
          if (sec.gt.0.d0) then
            dx = dx/sec
            dy = dy/sec
            dz = dz/sec
          endif
          co = dx*odx+dy*ody+dz*odz
          if (ihld.eq.1) then
            ifl(314) = 0
            if (co.lt.hco) ifl(314) = ifl314
            ix = 23-ixcir
            call pokout (istep, izro, buf, izro, icirc, cdat(ix), ptl)
          endif
          ifl(315) = 0
          if (co.lt.hco) ifl(315) = ifl315
        endif
        call pokout (istep, npts, buf, ilast, i2v2, cdat, ptl)
155     if (ilast.eq.1) then
          call pkllod (buf, itype, npts, ilast, ier)
          if (ier.gt.0 .or. itype.gt.1) goto 888
          call pokout (istep, npts, buf, ilast, i2v2, cdat, ptl)
          if (ilast.eq.1) goto 155
        endif
        ix = npts*6-11
        if (npts.gt.1) then
          ox = buf(ix)
          oy = buf(ix+1)
          oz = buf(ix+2)
        endif
        ix = ix+6
        x = buf(ix)
        y = buf(ix+1)
        z = buf(ix+2)
        cdat(ixcir+17) = x
        cdat(ixcir+18) = y
        cdat(ixcir+19) = z
        cdat(ixcir+20) = cdat(ixcir+4)
        cdat(ixcir+21) = cdat(ixcir+5)
        cdat(ixcir+22) = cdat(ixcir+6)
        call pokuvc (x,y,z,ox,oy,oz,odx,ody,odz,sec)
        icirc = 1
        if (ixfr.eq.6 .and. hco.le.1.) then
          ihld = 1
        else
          ihld = 0
          ifl(314) = 0
          ifl(315) = 0
          call pokout (istep,npts,buf,ilast,icirc,cdat(ixcir+1),ptl)
        endif
        odx = dx
        ody = dy
        odz = dz
        ox = x
        oy = y
        oz = z
        goto 100

160   if (itype.gt.3) goto 170
c
c... Feed Rate.
c
        ixfr = buf(1)
        if (ihld.eq.1) then
          ihld = 0
          ifl(314) = 0
          if (ixfr.ne.9) ifl(314) = ifl314
          call pokout (istep, izro, buf, izro, icirc, cdat(ixcir+1),ptl)
        endif
        if (ixfr.eq.8) then
          ftmp = feda(5)
          ixfr = 6
          i = 6
        else
          i = ixfr
          if (i.lt.1) i = 1
          if (i.gt.7) i = 7
          ftmp = feda(i)
          if ((i.lt.6.or.i.eq.7) .and. ictcm.eq.1 .and. 
     x                                      ictcom.gt.1) then
            call putcl(2000,1007,2,rct(15))
            ictcm = 0
          endif
        endif
        call fedout (ftmp, fhld)
        FEEDR = fhld
        FEEDC(3) = fhld
        goto 100

170   if (itype.ne.5) goto 180
c
c... "Couple" record.
c
c         buf(1) is dz
c         buf(2) is number of revolutions
 
        do i=1,15
          cdat(i) = 0
        enddo
        cdat(1) = 5
        do i=6,12
          cdat(i) = buf(i-3)
        enddo
        
        itmp(1) = 1
        itmp(2) = 0
        itmp(3) = 0
        itmp(4) = 0
        buf(2) = tmp8
        buf(3) = 360.
        call putcl(2000,1049,4,buf)

        call putcl(3000,2,13,cdat)

        goto 100

180   if (itype.ne.6) goto 888
c
c... "CYCLE/CIRCUL" record.
c
        call cyclrc (buf,ncyc2,bcyc2,feda,ier)
        if (ier.gt.0) goto 888
        goto 100

888   continue
      ifl(2) = 177
      sc(123) = sc123
      FEEDC(3) = feedc3
      goto 999

900   continue
      if (ihld.eq.1) then
        ihld = 0
        ifl(314) = ifl314
        call pokout (istep, izro, buf, izro, icirc, cdat(ixcir+1), ptl)
      endif
      ifl(314) = ifl314
      ifl(315) = ifl315
      sc(1) = x
      sc(2) = y
      sc(3) = z
      if (tamode .ge. 1) then
        sc(4) = buf(ix+3)
        sc(5) = buf(ix+4)
        sc(6) = buf(ix+5)
      endif
      if (.not.lv94) then
        sc(7) = dx
        sc(8) = dy
        sc(9) = dz
      endif
      sc(123) = fhld
cc      call motend
      FEEDC(3) = feedc3

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pokuvc (x,y,z,ox,oy,oz,dx,dy,dz,sec)
C*      Calulate a unit vector between 2 points.
C*    PARAMETERS
C*       INPUT  :
C*          x,y,z      - End point.
C*          ox,oy,oz   - Start point.
C*       OUTPUT :
C*          dx,dy,dz   - Unit vector
C*          sec        - Distance between points.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine pokuvc (x,y,z,ox,oy,oz,dx,dy,dz,sec)

      real*8 x,y,z,ox,oy,oz,dx,dy,dz,sec

      dx = x-ox
      dy = y-oy
      dz = z-oz
      sec = dsqrt(dx**2+dy**2+dz**2)
      if (sec.gt.0.d0) then
        dx = dx/sec
        dy = dy/sec
        dz = dz/sec
      endif

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pokhdr (trans, mx, ier)
C*      Save global pocket information in the first record of the 
C*      current pocket list.
C*    PARAMETERS
C*       INPUT  :
C*          trans      - True iff pocket is rotated from the Z plane.
C*          mx         - Rotation matrix.
C*       OUTPUT :
C*          ier        - 1 iff error else 0
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine pokhdr (trans, mx, ier)

      include 'com.com'
      include 'rrdm.com'
      include 'pokpl.com'

      logical trans
      real*8 mx(12)
      integer*4 ier

      real*8 buf(32)
      real*4 abuf(64)
      integer*2 ibuf(144)
      equivalence (buf,abuf,ibuf)
      integer*2 jramps, jentry, icltyp, itrans, ictcom,ilvdep
      equivalence (ibuf(1),jramps),(ibuf(2),jentry),(ibuf(3),icltyp)
      equivalence (ibuf(4),itrans),(ibuf(5),ictcom),(ibuf(6),ilvdep)
      real*4 armpds,anmlvl,agenfr,aposfr,aretfr,aentfr,atrnfr,afinfr
      real*4 amxang,afrsfr
      equivalence (abuf(5),armpds),(abuf(6),anmlvl),(abuf(7),agenfr)
      equivalence (abuf(8),aposfr),(abuf(9),aretfr),(abuf(10),aentfr)
      equivalence (abuf(11),atrnfr),(abuf(12),afinfr)
      equivalence (abuf(13),afrsfr),(abuf(14),amxang)
      real*8 bcllvl, btoppl(4),bbotpl(4),bclrpl(4),bmx(12)
      equivalence (buf(8),bcllvl),(buf(9),btoppl),(buf(13),bbotpl)
      equivalence (buf(17),bclrpl),(buf(21),bmx)

      integer*2 i, itype, mtype
      integer*2 i2v4, i2v6
      data i2v4 /4/, i2v6 /6/

      jramps = nramps
      jentry = entry
      icltyp = cltyp
      armpds = rmpdis
      anmlvl = numlvl
      ilvdep = lvdep
      agenfr = genfr
      aposfr = posfr
      aretfr = retrfr
      aentfr = entrfr
      atrnfr = tranfr
      afinfr = finfr
      afrsfr = frsfr
      bcllvl = clrlvl
      amxang = maxang
      ictcom = jctcom

      do 100 i=1,4
        btoppl(i) = toppl(i)
        bbotpl(i) = botpl(i)
        bclrpl(i) = clpl(i)
100   continue

      itrans = 0
      if (trans) then
        itrans = 1
        do 200 i=1,12
200     bmx(i) = mx(i)
        call transf (btoppl, mx, i2v4, i2v6)
        call transf (bbotpl, mx, i2v4, i2v6)
        call transf (bclrpl, mx, i2v4, i2v6)
      endif

      itype = 4
      mtype = 0
      call pklsto (itype, mtype, buf, ier)

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pokout (istep,npts,buf,ilast,
C*                                                        icirc,cdat)
C*      Output CL points to list file & CL file.
C*    PARAMETERS
C*       INPUT  :
C*          istep      - List file step number.
C*          npts       - Number of points.
C*          buf        - Points.
C*          ilast      - = 2 iff this is the last array of points.
C*          icirc      - Output circular record iff = 1
C*                       print & plot (circ rec goto) points only iff = 2
C*          cdat       - Circle data
C*       OUTPUT :
C*          istep      - List file step number.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine pokout (istep, npts, buf, ilast, icirc, cdat, ptlast)

      include 'com.com'
      include 'comgt.com'
c
      integer*2 istep, npts, ilast, icirc
      real*8 buf (120), cdat(19), ptlast(3)

      real*8 bufl(640)
      real*8 fx,fy,fz, sec
      integer*4 kptr, ncp, mupt, maxpt
      integer*2 i, j, k, ii, iclass, isubcl
      logical last
      logical lfalse,lv92
      data lfalse /.false./

      lv92 = sc(169) .lt. 9.249d0

      if (icirc.eq.1) then
cc        if (ifl(42).eq.0) call cidist (cdat(8),cdat(17),cdat)
        call cidist (cdat(8),cdat(17),cdat)
        goto 999
      endif

cc      if (lexpcl) then
      if (.not. lv92) then
        iclass = 5200
        isubcl = 5
        maxpt = 640
        mupt = 21
      else
        iclass = 5000
        isubcl = 5
        maxpt = 120
        mupt = 3*(ifl(82)+1)
      endif
      j = -5
      last = .false.

      do 100 i=1,npts
        j = j+6
        if (ilast.eq.2.and.i.eq.npts) last = .true.
        call poklst (istep, buf(j), last)
        if (ifl(42).eq.0) then
cc            call plotm (buf(j), lfalse)
            call ckintr (k,ifl(35))
        endif
100   continue
      if (icirc.eq.2) goto 999
      j = 0
      k = 0
      do 140 i=1,npts
        do 110 ii=1,6
110     bufl(j+ii) = buf(k+ii)
cc        if (lexpcl) then
        if (.not. lv92) then
          fx = bufl(j+1) - ptlast(1)
          fy = bufl(j+2) - ptlast(2)
          fz = bufl(j+3) - ptlast(3)
          sec = dsqrt(fx**2+fy**2+fz**2)
          if (sec.eq.0.0d0) sec = 1.0d0
          bufl(j+7) = fx/sec
          bufl(j+8) = fy/sec
          bufl(j+9) = fz/sec
          do 120 ii=10,21
120       bufl(j+ii) = 0.0d0
        endif
        ptlast(1) = bufl(j+1)
        ptlast(2) = bufl(j+2)
        ptlast(3) = bufl(j+3)
        j = j+mupt
        k = k+6
140   continue
      if (npts.gt.0) then
        ncp = npts
        call ptput1 (kptr, ncp, bufl, mupt, maxpt)
      endif
      if (ilast.ne.1) then
cc        if (ifl(42).eq.0) call csdist (iclass, isubcl)
        call csdist (iclass, isubcl)
        call ptclos
        if (ilast.eq.0) then
          call ptopen (nrcn, mupt)
          FROMSV(1) = ptlast(1)
          FROMSV(2) = ptlast(2)
          FROMSV(3) = ptlast(3)
        endif
      endif

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pokfed (fout, fhld, mtype, ier)
C*      Save a feed rate in the pocket list if it is different from the last
C*      feed rate saved.
C*    PARAMETERS
C*       INPUT  :
C*          fout       - Feed rate to save.
C*          fhld       - Last feed rate saved.
C*       OUTPUT :
C*          fhld       - Feed rate saved.
C*          mtype      - set to 0 because no continuation record is ever
C*                       needed.
C*          ier        - 0 iff no err else 1
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine pokfed (fout, fhld, mtype, ier)

      include 'com.com'

      real*8 fout, fhld
      integer*4 ier
      integer*2 mtype

      integer*2 itype

      if (fout.ne.fhld) then
        itype = 3
        mtype = 0
        call pklsto (itype, mtype, fout, ier)
        mtype = 0
        fhld = fout
      endif

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cyclrc (buf,ncyc2,bcyc2,feda,ier)
C*      Save a feed rate in the pocket list if it is different from the last
C*      feed rate saved.
C*    PARAMETERS
C*       INPUT  :
C*          fout       - Feed rate to save.
C*          fhld       - Last feed rate saved.
C*       OUTPUT :
C*          fhld       - Feed rate saved.
C*          mtype      - set to 0 because no continuation record is ever
C*                       needed.
C*          ier        - 0 iff no err else 1
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine cyclrc (buf,ncyc2,bcyc2,feda,ier)

      include 'com.com'
      include 'rrdm.com'

      real*8 buf(120),bcyc2(120),feda(7)
      integer*2 ncyc2
      integer*4 ier

      real*8 tmp8,bufl(120)
      integer*2 itmp(4)
      equivalence (itmp,tmp8)

      character*256 str1
      integer*2 n1, ns1, fr
      integer*2 VCYCLE/1054/,CIRCUL/75/,IPM/73/,CCLW/59/,DOWN/113/
      integer*2 DEPTH/510/,RADIUS/23/,STEP/92/,ATANGL/1/,ON/71/
      integer*2 RAPTO/280/
      
      logical lpercnt
      

        ncyc2 = 0
        if (keytxt.eq.0) then
c
c..... output "long" NCL-style command, same as used in postworks
c
          itmp(1) = CIRCUL
          itmp(2) = 0
          itmp(3) = 0
          itmp(4) = 0
          bufl(1) = tmp8

          itmp(1) = DEPTH
          itmp(2) = 0
          itmp(3) = 0
          itmp(4) = 0
          bufl(2) = tmp8
          bufl(3) = buf(1)

          itmp(1) = RADIUS
          itmp(2) = 0
          itmp(3) = 0
          itmp(4) = 0
          bufl(4) = tmp8
          bufl(5) = buf(2)

          itmp(1) = ON
          itmp(2) = 0
          itmp(3) = 0
          itmp(4) = 0
          bufl(6) = tmp8

          itmp(1) = STEP
          itmp(2) = 0
          itmp(3) = 0
          itmp(4) = 0
          bufl(7) = tmp8
          bufl(8) = buf(3)

          itmp(1) = IPM
          itmp(2) = 0
          itmp(3) = 0
          itmp(4) = 0
          bufl(9) = tmp8
          fr = buf(4)
          bufl(10) = feda(fr)

          itmp(1) = RAPTO
          itmp(2) = 0
          itmp(3) = 0
          itmp(4) = 0
          bufl(11) = tmp8
          bufl(12) = buf(5)

          itmp(1) = ATANGL
          itmp(2) = 0
          itmp(3) = 0
          itmp(4) = 0
          bufl(13) = tmp8
          bufl(14) = buf(6)

          itmp(1) = DOWN
          itmp(2) = 0
          itmp(3) = 0
          itmp(4) = 0
          bufl(15) = tmp8

          itmp(1) = CCLW
          itmp(2) = 0
          itmp(3) = 0
          itmp(4) = 0
          bufl(16) = tmp8

          n1 = 17
          call putcl(2000,1054,n1,bufl)
          return
        endif

        ns1 = 256
        call ncl_gttext(keytxt,str1,ns1)

        call touppr(str1,cin)
        nccin = ns1
        inx = 1
        call parsit
        if (.not.(vocab .and. voc.eq.VCYCLE)) goto 888
        call parsit

        n1 = 1
        lpercnt = .false.

100     call parsit

        if (lpercnt) then
          lpercnt = .false.
          if (token2(1:1) .eq. 'Z') then
            bufl(n1) = buf(1)
          else if (token2(1:1) .eq. 'D') then
            bufl(n1) = buf(2)
          else if (token2(1:1) .eq. 'I') then
            bufl(n1) = buf(3)
          else if (token2(1:1) .eq. 'F') then
            fr = buf(4)
            bufl(n1) = feda(fr)
          else if (token2(1:1) .eq. 'R') then
            bufl(n1) = buf(5)
          else
            goto 888
          endif
          n1 = n1+1
        else if (vocab) then
          itmp(1) = voc
          itmp(2) = 0
          itmp(3) = 0
          itmp(4) = 0
          bufl(n1) = tmp8
          n1 = n1+1
        else if (ain(inx).eq.'%') then
          ityp = 5
          lpercnt = .true.
          inx = inx+1
          goto 100
        endif

        if (.not.nxteos .and. inx.le.ns1) goto 100

        call putcl(2000,1054,n1,bufl)

        if (ain(inx).ne.';') return

199     inx = inx+1
        if (inx .ge. ns1) goto 999
        if (ain(inx) .ne. 'C') goto 199 
       
        call parsit
        if (.not.(vocab .and. voc.eq.VCYCLE)) goto 999
        call parsit

        n1 = 1
        lpercnt = .false.

200     call parsit

        if (lpercnt) then
          lpercnt = .false.
          if (token2(1:1) .eq. 'Z') then
            bcyc2(n1) = buf(1)
          else if (token2(1:1) .eq. 'D') then
            bcyc2(n1) = buf(2)
          else if (token2(1:1) .eq. 'I') then
            bcyc2(n1) = buf(3)
          else if (token2(1:1) .eq. 'F') then
            fr = buf(4)
            bcyc2(n1) = feda(fr)
          else if (token2(1:1) .eq. 'R') then
            bcyc2(n1) = buf(5)
          else
            goto 888
          endif
          n1 = n1+1
        else if (vocab) then
          itmp(1) = voc
          itmp(2) = 0
          itmp(3) = 0
          itmp(4) = 0
          bcyc2(n1) = tmp8
          n1 = n1+1
        else if (ain(inx).eq.'%') then
          ityp = 5
          lpercnt = .true.
          inx = inx+1
          goto 200
        endif

        if (.not.nxteos .and. inx.le.ns1) goto 200
    
        ncyc2 = n1
        goto 999

888   ier = 5

999   if (ier.eq.0) ain(inx) = ' '
      return
      end
c
c **********************************************************************
c **********************************************************************
c **   subroutine name:  pokdbg (xrr,yrr,zpln,npts,istp,icfl,color)
c **
c **  purpose of subroutine: outputs a perimeter array to a debug file
c **  parameters
c **     input  :
c **        xrr     - X-array of pocket perimeter points.
c **        yrr     - Y-array of pocket perimeter points.
c **        zpln    - Z-level to output points at.
c **        npts    - Number of points in XY arrays.
c **        istp    - 1 = Output *STOP,*ERASE prior to points.
c **        iclf    - 1 = Output arcs in Blue.
c **        color   - Color string to output geometry in.
c **     output :
c **        none
c **********************************************************************
c **********************************************************************
c
      subroutine pokdbg (xrr,yrr,zpln,npts,istp,icfl,ilcl,color)
c
      include 'com.com'
      include 'pokcom.com'
c
      integer*2 npts,istp,icfl,ilcl
c
      real*4 xrr(*),yrr(*),zpln,dis
c
      character*(*) color
c
      character*80 dbuf
c
      byte dout(80)
c
c...Write out stop & color lines
c.....Modifed routine slightly for better functionality - ASF 9/23/13
c
      if (istp .eq. 1) then
        call ctob ("*STOP",dout)
        call nclxostr (dout)
        call ctob ("ERASE/ALL",dout)
        call nclxostr (dout)
      endif
      write (dbuf,7000) color
 7000 format ('DRAFT/MODIFY,COLOR=',a)
      call ctob (dbuf,dout)
      call nclxostr(dout)
c
c...Loop through points
c
      l = 1
      iclsv = 1
      do while (l .lt. npts)
        if (xrr(l+1) .eq. flarc) then
          if (iclsv .ne. 2 .and. icfl .eq. 1) then
            call ctob ("DRAFT/MODIFY,COLOR=BLUE",dout)
            call nclxostr(dout)
            iclsv = 2
          endif
          rad = xrr(l+2)
          dir = yrr(l+2)
          vsx = -(yrr(l)-yrr(l+3))/rad * dir
          vsy = (xrr(l)-xrr(l+3))/rad * dir
          write (dbuf,7001) xrr(l),yrr(l),zpln,vsx,vsy
 7001     format ('CI/(PT/',2(f11.5,','),f11.5,'),(VE/',f11.5,',',
     1            f11.5,'),$')
          call ctob(dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7002) xrr(l+4),yrr(l+4),zpln
 7002     format ('(PT/',2(f11.5,','),f11.5,')')
          l = l + 4
        else
          if (iclsv .ne. 1) then
            if (ilcl .eq. 0) then
              call ctob ("DRAFT/MODIFY,COLOR=GREEN",dout)
            else
              call ctob ("DRAFT/MODIFY,COLOR=MAGNTA",dout)
            endif
            call nclxostr(dout)
            iclsv = 1
          endif
          dis = (xrr(l)-xrr(l+1))*(xrr(l)-xrr(l+1)) + 
     1          (yrr(l)-yrr(l+1))*(yrr(l)-yrr(l+1))
          if (dis > 0.000001) then
            write (dbuf,7003) xrr(l),yrr(l),zpln,xrr(l+1),yrr(l+1),
     1                    zpln
          endif
          l = l + 1
        endif
 7003   format ("LINE/",5(f11.5,","),f11.5)
        call ctob(dbuf,dout)
        call nclxostr(dout)
      enddo
c
c...End of routine
c
 8000 return
      end
