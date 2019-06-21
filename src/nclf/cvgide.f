C*********************************************************************
C*    NAME         :  cvgide.f
C*       CONTAINS:
C*
C*     subroutine cvgide
C*     subroutine cvpvst (vr, vfw)
C*     subroutine cvpv (vr, pcv, h, pta, vt)
C*     subroutine sfgide (pte, vta, vr, h, pta, psf, vt)
C*
C*     and older versions of these routines for NCL V8.4 & below
C*               (cvgideo, cvpvo, sfgideo)
C*
C*    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       cvgide.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:47
C*********************************************************************
C
C*********************************************************************
C*    SUBROUTINE     : subroutine cvgide
C*      Tilt tool to follow a guide curve or to do DS gouge check.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine cvgide

      include 'com.com'
      include 'mocom.com'
      include 'gidcom.com'
      include 'gidcm1.com'

      common/gidcm2/ gidst,dst1,irejdp,gidmvd,gdatcr
      real*8 gidst,dst1
      logical gidmvd,gdatcr
      integer*2 irejdp

      real*8 pte(3),pta(3),vta(3),pcv(3),vs(3),vr(3),vt(3),vcr(3),
     *       aver(3),dhld(50),vdelta(3),proj,proj0,vfw(3),del,
     *       h,hds,hds0,d1,d2,dst,width,d_tods,dist,HUGE,hhld,uhld, 
     *       alf,alfmin, si,co, thk, tlrgt,tol,zero,one,asn,gfc,
     *       cor,hdia,sbe,cbe,tbe,size,f_dot,f_mag,hcr

      real*4 asc(100),uvh_save(3),s_save(7),tasv(3),ucur,hcur
      real*4 tasv0(3),u0,h0
      real*4 tata(3),uuu,hhh

      integer*4 nclkey

      integer*2 kd(600),ia,ic,itlrgt,isrf,ksn(4),
     *          i,nwds,ietype,iknt,maxknt,idx,kdx,iwf
      equivalence (asn,ksn)

      logical lcsuv,lautuv,lv84,lv91,lv92,lv93,lv94,lviol,lcvrg0
      integer*2 ivta,i0,i2
c
c...          common equivalences
c
      equivalence (sc,asc)
      equivalence (ifl(21),itlrgt),(ifl(51),ia),(ifl(53),ic)
      equivalence (ifl(54),isrf)
      equivalence (d,kd)

      data maxknt/10/, alfmin/1.d-5/,zero/0.0d00/,one/1.0d00/,
     *     HUGE/1.0d09/

      lv84 = sc(169).lt.8.499d0
      lsamta = .false.
      gdatcr = .false.
      if(lv84) then
         call cvgideo
         return
      endif
      lv91 = sc(169).lt.9.149d0
      lv92 = sc(169).lt.9.249d0
      lv93 = sc(169).lt.9.349d0
      lv94 = sc(169).lt.9.449d0
      ivta = 0
      i0 = 0
      proj0 = 1000000.
c
c...   Initialize some variables.
c
      thk = sc(24)
      if (gthkon.eq.1) thk = gidthk

      tlrgt = itlrgt
      if (gidsid.gt.0) tlrgt = gidsid-2

      tol = sc(27)
      sbe = asc(63)
      cbe = asc(64)
      if (cbe.eq.zero) cbe = one
      tbe = sbe/cbe
      cor = tool(2)
      hdia = 0.5*sc(28)
      size = hdia - cor + cor/cbe - cor*tbe + thk
      hcr = cor*(1.-sbe) 

      call conv4_8(t(1,ia),pte,3)
      call conv4_8(t(4,ia),vta,3)
      call conv4_8(t(7,ia),vfw,3)
      call conv4_4(t(4,ia),tasv,3)
      del = (t(1,ia)-t(1,ic))*t(7,ia)+(t(2,ia)-t(2,ic))*t(8,ia)+
     *      (t(3,ia)-t(3,ic))*t(9,ia)  

      uhld = t(25,ia)
      hhld = t(27,ia)
      lcsuv = csuv
      lautuv = autouv
      csuv = .false.
      autouv = .false.

      call conv8_8(d(101),dhld,50)
c
c..... was just "if (ldschk)". changed by Ian's suggestion, as both guide 
c..... curve with contact "on" and DS gouge check seem incompatible
c
      if (ldschk .and. .not.(lcvgid.and.ltltct)) then 
        call conv4_4(t(19,ia),uvh_save,3)
        call conv4_4(s(1,2),s_save,7)
      endif
c
c..... initialize for ldschk, since lcvgid and ldschk could both be true - 
c..... Dassault MFGNC179, for example
c
      hds0 = 0.75*(sc(30)-sc(29))+sc(29)
c
c...   Set up to evaluate guide curve
c
      if (lcvgid) then
        call gtdesc (gidasw, nclkey, nwds, ietype)

        t(25,ia) = gidu
        t(27,ia) = gidh
        idx = 100
        kdx = 400
        d(idx+1) = zero
        kd(kdx+1) = ietype
        if (ietype.eq.CIRCLE) then
          call gtgeom(gidasw, d(idx+3), nclkey, nwds, ietype)
        else 
          if (.not.lv92) then
            lcvpv = .true.
            if (ifl(49).eq.-2) then
              igdfwd = 0
              t(25,ia) = gidu0
c              gidu = gidu0
c
c..... fix for oml-022706:
c..... check if a better initial parameter can be found
c
              if (.not.lv94 .and. .not.ltltct .and. tlrgt.ne.0.) then
                vr(1)=t(8,ia)*t(6,ia)-t(9,ia)*t(5,ia)
                vr(2)=t(9,ia)*t(4,ia)-t(7,ia)*t(6,ia)
                vr(3)=t(7,ia)*t(5,ia)-t(8,ia)*t(4,ia)
                call unitizevc(vr)
                isrf = 3
                call isitwf (nclkey, iwf)
                if (iwf.eq.0) then
                  call gtncsg (nclkey, kd(kdx+2))
                else if (.not.lv92) then
                  kd(kdx+3) = 1
                endif
                d(idx+2) = gidasw
                call cvpvst (vr,vfw)
              endif
              gidu = t(25,ia)
            endif
          endif
          call isitwf (nclkey, iwf)
          if (iwf.eq.0) then
            call gtncsg (nclkey, kd(kdx+2))
          else if (.not.lv92) then
            kd(kdx+3) = 1
          endif
        endif
        d(idx+2) = gidasw
      else
        dst = HUGE
      endif
      call gtdesc (sc(145), nclkey, nwds, ietype)
c
c... iteration loop; vr gives general direction of the right vector; 
c... vt = actual right vector
c
      iknt = 0
      lcvrg0 = ifl(23).eq.TA_FAN
     *         .and. tlrgt.ne.zero .and. .not.ltltct .and. lcvgid
      lcvrg1 = .not.lv93 .and. lcvrg0 .and. ifl(80).eq.1
      lcvrgt = .not.lv92 .and. lcvrg0 .and. ifl(80).eq.0

100   continue

      vr(1)=t(8,ia)*t(6,ia)-t(9,ia)*t(5,ia)
      vr(2)=t(9,ia)*t(4,ia)-t(7,ia)*t(6,ia)
      vr(3)=t(7,ia)*t(5,ia)-t(8,ia)*t(4,ia)
      call unitizevc(vr)
c
c...  Project look point onto guide curve; Calculate directed distance 
c...  (d2) to guide curve & compare to tool geo
c
      if (lcvgid) then
        isrf = 3
        if (iknt.eq.0 .or. .not.lcvrgt) then
          call cvpv (vr, pcv, h, pta, vt)
          gdatcr = .false.
          if (.not.lv93 .and. lcvrgt .and.  hcr.gt.10.*tol)
     x      gdatcr = (dabs(h/hcr-1).lt.0.5)
        else
          call uvcplvc(pte,vta,pta,h)
          call vcmnvc(pcv,pta,vt)
          proj = f_dot(vt,vta)
          call uvcplvc(vt,vta,vt,-proj)
          call unitizevc(vt)
          if (igdfwd .eq. 0) then
            if (f_dot(vt,vr).lt.zero) call mnvc(vt)
          else
            vcr(1)=gdfw(2)*t(6,ia)-gdfw(3)*t(5,ia)
            vcr(2)=gdfw(3)*t(4,ia)-gdfw(1)*t(6,ia)
            vcr(3)=gdfw(1)*t(5,ia)-gdfw(2)*t(4,ia)
            if (igdfwd*f_dot(vcr,vt) .lt. 0.) call mnvc(vt)
          endif
        endif
        if (ifl(2).gt.0) return

        call vcmnvc(pta,pcv,vdelta)
        d2 = f_mag(vdelta)
        if (f_dot(vdelta,vt).lt.zero) d2 = -d2
        if (tlrgt.eq.zero) then
          d1 = zero
          dst = d2
        else
          if (h.le.(tol + hcr) 
     *        .and. kd(kdx+1).eq.CURVE .and. .not.ltltct) then
            if (h .le. tol) then
              d2 = 0.
              d1 = 0.
            else if (lv93) then
              d1 = tlrgt*(tool(6)+h*(2*cor-h) + thk)
            else
              d1 = tlrgt*(tool(6)+dsqrt(h*(2*cor-h)) + thk)
            endif
          else
            d1 = tlrgt*(size + h*tbe)
            proj = dabs(f_dot(vdelta,vfw))
c
c..... the 'if' below is to quit when the guide curve is far ahead or
c..... behind (we do not use guide curve extensions). to solve problems
c..... for Dassault's MFGNC287, MFGNC289
c
            if (.not.lv93 .and. iknt.eq.0 .and. .not.ltltct .and.
     *          proj.gt.(sc(28)+thk) .and.
     *          (t(25,ia).gt.0.999 .or. t(25,ia).lt.0.001)) then
              d2 = dabs(d2)*tlrgt
            endif
          endif
          dst = tlrgt*(d2 - d1)

          if (gdatcr) then
c
c..... remember the first gouge distance
c
            if (irejdp.eq.1 .and. iknt.eq.0 .and.
     x          dst1.eq.0 .and. dst.le.-tol) then
              dst1 = -tol
            else if (irejdp .eq. 3) then
c
c..... if third attempt to correct the tool axis - do just one
c..... iteration, record the gouge distance, and exit
c
              if (iknt.eq.9) goto 900
              if (iknt.eq.0 .and. dst.lt.0) iknt = 8
            else if (irejdp.gt.3) then
c
c..... if further than third attempt to correct the tool axis - 
c..... just record the gouge distance and exit
c
              goto 900
            endif
          endif

        endif
      endif
c
c... DS goucck: check tool vis. DS at point at height hds along tool axis
c
110   if (ldschk.and.ietype.eq.SURF .and. .not.(lcvgid.and.ltltct)) then

        hds = hds0
        call sfgide (pte, vta, vr, hds, pta, pcv, vs)
        if (lv91) then
          call vcmnvc(pta,pcv,vdelta)
          d_tods = f_mag(vdelta)
          if(f_dot(vdelta,vs).lt.zero) d_tods = -d_tods 
        else
          d_tods = f_dot(pta,vs) - f_dot(pcv,vs)
        endif

        width = size + hds*tbe
        dist = tlrgt*d_tods - width
        if (hds.le.cor .or. dist.ge.dst) then
c
c..... if we are not changing TA because of DS gougck, see if we 
c..... still need to do it because of a guide curve
c
           if (lcvgid .and. .not.lv91) goto 200 
           goto 900
        endif

        d1 = tlrgt*width
        d2 = d_tods
        dst = dist
        h = hds
        call vctovc(vs,vt)

      endif

200   if (dst.ge.zero.and.iknt.eq.0.and.tlrgt.ne.zero.and..not.ltltct)
     *                                                            then
        lsamta = (.not.lv92 .and. lcvgid)
        goto 900
      endif
c
c...   Calculate average right vector if not fan ta or for DS gouge check
c
      if (ifl(23).ne.TA_FAN .or. (ldschk.and.ietype.eq.SURF)) then
        call vcplvc(vr,vt,aver)
        call unitizevc(aver)
        co = f_dot(aver,vr)
        if (co.lt.0.5d0) co = 0.5d0
        d1 = d1/co
        d2 = d2/co
      else if (ivta .gt. 0) then
        i2 = ivta/2
        if (lv93) then
          i4 = i2/2
          if (i2 .eq. 0) then
            call vctovc(vt,aver)
          else if (i2*2 .eq. ivta) then
            if (i4*2 .eq. i2) then
              gfc = (i4+1)*0.5
            else
              gfc = -(i4+2)*0.5
            endif
            call uvcplvc(vt,vr,aver,gfc)
          else
            if (i4*2 .eq. i2) then
              gfc = (i4+1)*0.5
            else
              gfc = (i4+1)*0.5
            endif
            call uvcplvc(vt,vfw,aver,gfc)
          endif
        else
          gfc = 0.5*i2
          if (i2 .eq. 0) then
            call vctovc(vt,aver)
          else if (i2*2 .eq. ivta) then
            call uvcplvc(vt,vr,aver,gfc)
          else
            call uvcplvc(vt,vfw,aver,gfc)
          endif
        endif
        call unitizevc(aver)
      else
        call vctovc(vr,aver) 
      endif
c
c...   alf = rotation angle.
c...   Rotate tool axis: vta = sin(alf)*aver + cos(alf)*vta
c
      alf = zero
      if(h.ne.zero) alf = datan2(d1,h) - datan2(d2,h)
      call avcplbvc(dsin(alf),aver,dcos(alf),vta,vta)
      call unitizevc(vta)
      gidmvd = .true.

      call conv8_4(vta,t(4,ia),3)
c
c...   Maybe should use linear tolerance here: f.i.,
c...      if (dabs(sin(alf))*dmax1(one,h).lt.tol) ....
c
      iknt = iknt+1
      if (dabs(alf).gt.alfmin.and.iknt.lt.maxknt) goto 100

      if (lcvrgt .and. iknt.le.maxknt) then
        ucur = t(25,ia)
        hcur = h
        vr(1)=t(8,ia)*t(6,ia)-t(9,ia)*t(5,ia)
        vr(2)=t(9,ia)*t(4,ia)-t(7,ia)*t(6,ia)
        vr(3)=t(7,ia)*t(5,ia)-t(8,ia)*t(4,ia)
        call unitizevc(vr)
        isrf = 3
        call cvpv (vr, pcv, h, pta, vt)
        call vcmnvc(pta,pcv,vdelta)
        d2 = f_mag(vdelta)
        if (f_dot(vdelta,vt).lt.zero) d2 = -d2
        if (lv93) then
          d1 = tlrgt*(size + h*tbe)
        else
          if (h.le.(tol + hcr) .and. kd(kdx+1).eq.CURVE) then
            if (h .le. tol) then
              d2 = 0.
              d1 = 0.
            else
              d1 = tlrgt*(tool(6)+dsqrt(h*(2*cor-h)) + thk)
            endif
          else
            d1 = tlrgt*(size + h*tbe)
          endif
        endif
        dst = tlrgt*(d2 - d1)
        lviol = dst.lt.-tol
        if (ivta .eq. 0) then
          proj = 0.
          uuu = t(25,ia)
          hhh = t(27,ia)
          call conv4_4(t(4,ia),tata,3)
          if (ifl(50).gt.0) then
            vdelta(1) = pcv(1) - gidpt(1)
            vdelta(2) = pcv(2) - gidpt(2)
            vdelta(3) = pcv(3) - gidpt(3)
            proj = f_dot(vdelta,vfw)
            proj = dabs(proj-del)
          endif
          if (proj .gt. 20.*tol) then
            t(25,ia) = gidu
            t(27,ia) = gidh
            call conv4_4(tasv,t(4,ia),3)
            call conv4_8(tasv,vta,3)
            ivta = 1
            iknt = 0
            goto 100
          else if (lviol) then
            goto 110
          else
            if (.not.lv93 .and. dst.lt.-0.75*tol) goto 110
            t(25,ia) = ucur
            h = hcur
c
c..... fix for MFGNC306.cpp
c
        if (iknt.eq.1 .and. lcvrgt .and. .not.lv93) then
          if (dabs(alf).lt.0.75*alfmin) lsamta = .true.
        endif

            goto 900
          endif
        else if (ivta.lt.9) then
          vdelta(1) = pcv(1) - gidpt(1)
          vdelta(2) = pcv(2) - gidpt(2)
          vdelta(3) = pcv(3) - gidpt(3)
          proj = f_dot(vdelta,vfw)
          proj = dabs(proj-del)
          if (.not.lviol .and. proj.lt.proj0) then
            i0 = ivta
            proj0 = proj
            u0 = ucur
            h0 = hcur
            call conv4_4(t(4,ia),tasv0,3)
          endif
          t(25,ia) = gidu
          t(27,ia) = gidh
          call conv4_4(tasv,t(4,ia),3)
          call conv4_8(tasv,vta,3)
          ivta = ivta+1
          iknt = 0
          goto 100
        else
c..... ivta.ge.9
          if (i0 .eq. 0) then
c           ifl(2) = 139
c..... Change to make Dassault MFGNC266 work with dpmax=50. Instead of
c..... ending with error end with the original bad projection.
c..... Unpleasant but in the test case it mananges to correct itself later.
c
             t(25,ia) = uuu
             t(27,ia) = hhh
             call conv4_4(tata,t(4,ia),3)
          else
            t(25,ia) = u0
            h = h0
            call conv4_4(tasv0,t(4,ia),3)
          endif
          goto 900
        endif
      endif

900   continue
c
c...   Save off height of tool look point & curve u value.
c
      if (lv92) then
        gidu = t(25,ia)
        gidh = h
      else
c
c..... Now we use only gidu,gidh,gidpt saved in mover after iptk is accepted
c
        giduia = t(25,ia)
        gidhia = h
        gdatcr = .false.
        if (.not.lv93 .and. lcvrgt .and.  hcr.gt.10.*tol)
     x    gdatcr = (dabs(h/hcr-1).lt.0.5)
        gdptia(1) = pcv(1)
        gdptia(2) = pcv(2)
        gdptia(3) = pcv(3)
        gidst = dst
      endif
c
c...   Restore check surface.
c
      t(25,ia) = uhld
      t(27,ia) = hhld
      csuv = lcsuv
      autouv = lautuv
      lcvpv = .false.

      call conv8_8(dhld,d(101),50)

      call gtdesc (sc(146), nclkey, nwds, ietype)
      isrf = 3
      if (ietype .ne. plane .and. nclkey .gt. 0)
     *  call evstup (nclkey, isrf)    

      if (ldschk .and. .not.(lcvgid.and.ltltct)) then
        call conv4_4(uvh_save,t(19,ia),3)
        call conv4_4(s_save,s(1,2),7)
      endif
c
c...   If top contact, rotate ds plane parallel to tool side
c
      si = (t(4,ia)*s(1,2)+t(5,ia)*s(2,2)+t(6,ia)*s(3,2))*itlrgt
      if (si.lt.sbe) then
        h = t(21,ia)
        call uvcplvc(pte,vta,pta,h)

        d1 = s(1,2)*pta(1)+s(2,2)*pta(2)+s(3,2)*pta(3)-s(4,2)
        do i = 1,3
           pcv(i) = pta(i)-s(i,2)*d1
        enddo
        aver(1) = t(8,ia)*t(6,ia)-t(9,ia)*t(5,ia)
        aver(2) = t(9,ia)*t(4,ia)-t(7,ia)*t(6,ia)
        aver(3) = t(7,ia)*t(5,ia)-t(8,ia)*t(4,ia)
        call unitizevc(aver)

        si = sbe*itlrgt
        do i=1,3
           s(i,2) = aver(i)*cbe + vta(i)*si
        enddo
        s(4,2) = s(1,2)*pcv(1)+s(2,2)*pcv(2)+s(3,2)*pcv(3)

      endif

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine cvpvst(vr,vfw)
c*     Try to find a better initial parameter on the guide curve.
C*     If a much better initial parameter is found, it is returned
C*     as t(25,ia); otherwise nothing happens.
C*    PARAMETERS  
C*       INPUT  :
C*          vr      - initial direction of right vector.
C*          vfw     - initial direction of forward vector.
C*       OUTPUT :
C*          t(25,ia) - the curve initial parameter
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine cvpvst(vr,vfw)

      include 'com.com'
      include 'mocom.com'

      real*8 vr(3),vfw(3)

      integer*2 jd(600), ia, isrf
      equivalence (d,jd)
      equivalence (ifl(51),ia), (ifl(54),isrf)

      real*8 vta(3),pte(3),pcv(3),vt(3),pta(3)
      real*8 f_dot
      real*8 h,proj,df0,dr0,atol,atolsq

      real*8 CS10,CS20,CS80
      parameter (CS10 = 0.9848)
      parameter (CS20 = 0.9397)
      parameter (CS80 = 0.17365)

      real*4 px,py,pz,vx,vy,vz,hinit,uinit,uu0
      integer*2 i,click

      ifl(2) = 0
      uinit = t(25,ia)
      hinit = t(27,ia)
      call conv4_8(t(1,ia),pte,3)
      call conv4_8(t(4,ia),vta,3)

      call curvpv(px,py,pz, vx,vy,vz)
      if (ifl(2) .gt. 0) return

      atol = 5.*sc(27)
      atolsq = atol*atol
      click = 0

      pcv(1) = px
      pcv(2) = py
      pcv(3) = pz

      h = t(27,ia)

      call uvcplvc(pte,vta,pta,h)
c
c... right vector vt = pcv - pta; make it orthogonal to the tool axis
c
      call vcmnvc(pcv,pta,vt)

      dd0 = f_dot (vt,vt)

      proj = f_dot(vt,vta)
      call uvcplvc(vt,vta,vt,-proj)
      call unitizevc(vt)

      df0 = f_dot (vt,vfw)
      dr0 = f_dot (vt,vr)

      if (dr0 .lt. CS20 .and. df0 .lt. -CS80) then

       uu0 = t(25,ia)
       do 20 i = 1,9
         t(25,ia) = 0.1*i
         t(27,ia) = hinit

         call curvpv(px,py,pz, vx,vy,vz)

         if (abs(t(25,ia) - uu0) .lt. 0.001) goto 20

         pcv(1) = px
         pcv(2) = py
         pcv(3) = pz

         h = t(27,ia)

         call uvcplvc(pte,vta,pta,h)
c
c... right vector vt = pcv - pta; make it orthogonal to the tool axis
c
         call vcmnvc(pcv,pta,vt)
         ddi = f_dot (vt,vt)

         proj = f_dot(vt,vta)
         call uvcplvc(vt,vta,vt,-proj)
         call unitizevc(vt)

         dfi = f_dot (vt,vfw)
         dri = f_dot (vt,vr)

         if (dri .ge. CS10 .and. dfi .ge. -CS80) then
           if (click.eq.0.and. ddi .le. dd0+atolsq) then
             click = 1
             dd0 = ddi
             uu0 = t(25,ia)
           else if (click .eq. 1 .and. ddi .lt. dd0) then
             dd0 = ddi
             uu0 = t(25,ia)
           endif
         endif
 20    continue

      endif

      if (click .eq. 1) then
        t(25,ia) = uu0
      else
        t(25,ia) = uinit
      endif
      t(27,ia) = hinit

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine cvpv(vr, pcv, h, pta, vt)
c*     Project point onto tool & calculate right vector.
C*    PARAMETERS   
C*       INPUT  : 
C*          vr      - General direction of right vector.
C*       OUTPUT :  
C*          pcv     - Point on curve.
C*          h       - Height of project point up tool axis.
C*          pta     - Project point on tool axis.
C*          vt      - Right vector.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine cvpv(vr, pcv, h, pta, vt)

      include 'com.com'
      include 'mocom.com'
      include 'gidcm1.com'

      real*8 vr(3),pcv(3),h,pta(3),vt(3)
      real*4 px,py,pz,vx,vy,vz

      integer*2 jd(600), ia, isrf, i
      real*8 vta(3),proj,f_dot,vcr(3),vfw(3)

c...                motion common equivalences
      equivalence (d,jd)
      equivalence (ifl(51),ia), (ifl(54),isrf)

      ifl(2) = 0
      if (jd((isrf-1)*200+1).eq.CIRCLE) then
        call circpv(px,py,pz, vx,vy,vz)
      else
        call curvpv(px,py,pz, vx,vy,vz)
c       if (ifl(2) .eq. 466) return
        if (ifl(2) .gt. 0) return
        if (sc(169).ge.9.249d0 .and. igdfwd.eq.0) then
          proj = vx*t(7,ia)+vy*t(8,ia)+vz*t(9,ia)
          if (proj .gt. 0.) then
            igdfwd = 1
          else if (proj .lt. 0.) then
            igdfwd = -1
          endif
        endif
      endif
      pcv(1) = px
      pcv(2) = py
      pcv(3) = pz

      h = t(27,ia)
      call conv4_8(t(4,ia),vta,3)

      do 1 i=1,3
 1       pta(i) = t(i,ia)+h*vta(i)
c
c... right vector vt = pcv - pta; make it orthogonal to the tool axis
c
      call vcmnvc(pcv,pta,vt)
      if (lcvpv .and. ltltct) then
c
c..... this is to use a curve  extension if necessary - was not supported
c..... before. flag lcvpv is not true unless the version is > 9.25
c
        u = t(25,ia)
        vfw(1) = vx
        vfw(2) = vy
        vfw(3) = vz
        proj = f_dot (vt,vfw)
        if (dabs(proj).gt.10.*sc(27) .and.
     *      (u.lt.0.0001 .or. u.gt.0.9999)) then
          call uvcplvc(pcv,vfw,pcv,-proj)
          call vcmnvc(pcv,pta,vt)
          proj = f_dot(vt,vta)
          h = h + proj
          t(27,ia) = h
          call uvcplvc(pta,vta,pta,proj)
          call vcmnvc(pcv,pta,vt)
        endif
      endif
      proj = f_dot(vt,vta)
      call uvcplvc(vt,vta,vt,-proj)
      call unitizevc(vt)
      if (igdfwd .eq. 0) then
        if (f_dot(vt,vr) .lt. 0.) call mnvc(vt)
      else
        gdfw(1) = vx
        gdfw(2) = vy
        gdfw(3) = vz
        vcr(1)=gdfw(2)*t(6,ia)-gdfw(3)*t(5,ia)
        vcr(2)=gdfw(3)*t(4,ia)-gdfw(1)*t(6,ia)
        vcr(3)=gdfw(1)*t(5,ia)-gdfw(2)*t(4,ia)
        if (igdfwd*f_dot(vcr,vt) .lt. 0.) call mnvc(vt)
      endif

      return
      end

C*********************************************************************
C*    SUBROUTINE : subroutine sfgide(pte, vta, vr, h, pta, psf, vt)
c*                                                               
C*    PARAMETERS   
C*       INPUT  : 
C*          pte     - Tool end point.
C*          vta     - Tool axis vector.
C*          vr      - General direction of right vector.
C*          h       - Height of look point up tool axis.
C*       OUTPUT :  
C*          h       - Height of look point up tool axis.
C*          pta     - Point on tool axis.
C*          psf     - Point on surface.
C*          vt      - Right vector.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine sfgide(pte, vta, vr, h, pta, psf, vt)

      include 'com.com'
      include 'mocom.com'

      real*8 pte(3),vta(3),vr(3),h,pta(3),psf(3),vt(3),proj,
     *       f_dot,zero/0.0d00/
      real*4 ad(300)
      integer*2 jd(600), ia, isrf

c          motion common equivalences
      equivalence (d,ad,jd)
      equivalence (ifl(51),ia), (ifl(54),isrf)

      t(21,ia) = h
      isrf = 2
      call dsrel
      call conv4_8(s(5,2),psf,3)

      h = t(21,ia)
      call uvcplvc(pte,vta,pta,h)

      call vcmnvc(psf,pta,vt)
      proj = f_dot(vt,vta)
      call uvcplvc(vt,vta,vt,-proj)
      call unitizevc(vt)
      if(f_dot(vr,vt).lt.zero) call mnvc(vt)

      return
      end
C*********************************************************************
C*     For NCL V8.4 and below:
C*     subroutines cvgideo, 
C*                 cvpvo(vr, pcv, h, pta, vt) 
C*                 sfgideo(pte, vta, vr, h, pta, psf, vt)
C*    MODULE NAME AND RELEASE LEVEL 
C*       cvgide.f , 12.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       8/23/96 , 09:36:47
C********************************************************************/
C
C*********************************************************************
C*    SUBROUTINE  : subroutine cvgideo
C*      Tilt tool to follow a guide curve. For NCL V8.4 and below
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
      subroutine cvgideo

      include 'com.com'
      include 'mocom.com'
      include 'gidcom.com'

c          motion common equivalences
      integer*2 kd(600)
      equivalence (d,kd)

      real*4 asc(100)
      equivalence (sc,asc)
      integer*2 ia, itlrgt, isrf
      equivalence (ifl(21),itlrgt), (ifl(51),ia)
      equivalence (ifl(54),isrf)

      real*8 pte(3), pta(3), vta(3), pcv(3), vs(3), vr(3), vt(3)
      real*8 h1,h2, d1,d2, dst, d3,d4,d5, xr,yr,zr, sec
      real*8 alf, alfmin, si,co, thk, tlrgt
      real*8 cor, hdia, sbe, cbe, tbe
      real*8 dhld(50), hhld, uhld
      real*8 udhld, vdhld, hdhld, shld(7)
      integer*4 nclkey
      integer*2 i, nwds, ietype, iknt, maxknt
      integer*2 idx, kdx, iwf
      logical lcsuv, lautuv
c
c...   Initialize some variables.
c
      if (gthkon.eq.1) then
        thk = gidthk
      else
        thk = sc(24)
      endif
      if (gidsid.gt.0) then
        tlrgt = gidsid-2
      else
        tlrgt = itlrgt
      endif
      iknt = 0
      maxknt = 10
      alfmin = 1.d-5
      sbe = asc(63)
      cbe = asc(64)
      if (cbe.eq.0.0d0) cbe = 1.0d0
      tbe = sbe/cbe
      cor = tool(2)
      hdia = sc(28)/2.0d0
      do 5 i = 1,3
      pte(i) = t(i,ia)
5     vta(i) = t(i+3,ia)
      uhld = t(25,ia)
      hhld = t(27,ia)
      lcsuv = csuv
      lautuv = autouv
      csuv = .false.
      autouv = .false.
      do 10 i=1,50
      dhld(i) = d(100+i)
10    continue
      if (ldschk) then
        udhld = t(19,ia)
        vdhld = t(20,ia)
        hdhld = t(21,ia)
        do 20 i=1,7
        shld(i) = s(i,2)
20      continue
      endif
c
c...   Set up to evaluate guide curve
c
      if (lcvgid) then
        call gtdesc (gidasw, nclkey, nwds, ietype)

        t(25,ia) = gidu
        t(27,ia) = gidh
        idx = 100
        kdx = 400
        d(idx+1) = 0.0d0
        kd(kdx+1) = ietype
        if (ietype.eq.CIRCLE) then
          call gtgeom(gidasw, d(idx+3), nclkey, nwds, ietype)
        else
          call isitwf (nclkey, iwf)
          if (iwf.eq.0) call gtncsg (nclkey, kd(kdx+2))
        endif
        d(idx+2) = gidasw
      else
        dst = 1.0d9
      endif
      call gtdesc (sc(145), nclkey, nwds, ietype)
100   continue
c
c...   Calculate right vector
c
      vr(1)=t(8,ia)*t(6,ia)-t(9,ia)*t(5,ia)
      vr(2)=t(9,ia)*t(4,ia)-t(7,ia)*t(6,ia)
      vr(3)=t(7,ia)*t(5,ia)-t(8,ia)*t(4,ia)
      sec = dsqrt(vr(1)**2+vr(2)**2+vr(3)**2)
      if (sec.eq.0.0d0) sec = 1.0d0
      vr(1) = vr(1)/sec
      vr(2) = vr(2)/sec
      vr(3) = vr(3)/sec
c
c...   Project look point onto guide curve
c
      if (lcvgid) then
        isrf = 3
        call cvpvo (vr, pcv, h1, pta, vt)
        if (ifl(2).gt.0) goto 999
c
c...  Calculate directed distance to guide curve & compare to tool geo
c
        d2 = (pta(1)-pcv(1))*vt(1)+(pta(2)-pcv(2))*vt(2)+
     x       (pta(3)-pcv(3))*vt(3)
        if (tlrgt.eq.0.0d0) then
          d1 = 0.0d0
          dst = d2
        else
          d1 = (hdia-cor+cor/cbe+(h1-cor)*tbe+thk)*tlrgt
          dst = tlrgt*(d2-d1)
        endif
      endif
      if (ldschk.and.ietype.eq.SURF) then
        h2 = (sc(30)-sc(29))*.5d0+sc(29)
        call sfgideo (pte, vta, vr, h2, pta, pcv, vs)
        d4 = (pta(1)-pcv(1))*vs(1)+(pta(2)-pcv(2))*vs(2)+
     x       (pta(3)-pcv(3))*vs(3)
        d3 = (hdia-cor+cor/cbe+(h2-cor)*tbe+thk)*tlrgt
        d5 = tlrgt*(d4-d3)
        if (h2.gt.tool(2) .and. d5.lt.dst) then
          d1 = d3
          d2 = d4
          dst = d5
          h1 = h2
          vt(1) = vs(1)
          vt(2) = vs(2)
          vt(3) = vs(3)
        endif
      endif

      if (dst.ge.0.0d0.and.iknt.eq.0.and.tlrgt.ne.0.0d0.and..not.ltltct)
     1    goto 900
c
      xr = vr(1)
      yr = vr(2)
      zr = vr(3)
c
c...   Calculate average right vector if not fan ta or for DS gouge check
c
      if (ifl(23).ne.4 .or. ldschk.and.ietype.eq.SURF) then
        xr = vr(1)+vt(1)
        yr = vr(2)+vt(2)
        zr = vr(3)+vt(3)
        sec = dsqrt(xr**2+yr**2+zr**2)
        if (sec.eq.0.0d0) sec = 1.0d0
        xr = xr/sec
        yr = yr/sec
        zr = zr/sec
      endif
c
c...   Calculate rotation angle required
c
      co = xr*vt(1)+yr*vt(2)+zr*vt(3)
      if (co.lt.0.5d0) co = 0.5d0
      d1 = d1/co
      d2 = d2/co
      alf = datan2(d1,h1)-datan2(d2,h1)
      co = dcos(alf)
      si = dsin(alf)
c
c...   Rotate tool axis
c
      vta(1) = xr*si+vta(1)*co
      vta(2) = yr*si+vta(2)*co
      vta(3) = zr*si+vta(3)*co
c
c... Test code to see if unitizing the tool axis vector fixes NCL10
c
c      sec = dsqrt(vta(1)**2+vta(2)**2+vta(3)**2)
c      if (sec.eq.0.0d0) sec = 1.0d0
c      vta(1) = vta(1)/sec
c      vta(2) = vta(2)/sec
c      vta(3) = vta(3)/sec
      t(4,ia) = vta(1)
      t(5,ia) = vta(2)
      t(6,ia) = vta(3)
C   Maybe should use linear tolerance here
      if (dabs(alf).lt.alfmin) goto 900
      iknt = iknt+1
      if (iknt.lt.maxknt) goto 100

900   continue
c
c...   Save off height of tool look point & curve u value.
c
      gidu = t(25,ia)
      gidh = h1
c
c...   Restore check surface.
c
      t(25,ia) = uhld
      t(27,ia) = hhld
      csuv = lcsuv
      autouv = lautuv
      do 910 i=1,50
      d(100+i) = dhld(i)
910   continue
      isrf = 3
      call gtdesc (sc(146), nclkey, nwds, ietype)
      call evstup (nclkey, isrf)
      if (ldschk) then
        t(19,ia) = udhld
        t(20,ia) = vdhld
        t(21,ia) = hdhld
        do 920 i=1,7
        s(i,2) = shld(i)
920     continue
      endif

c
c...   If top contact, rotate ds plane parallel to tool side
c
      si = (t(4,ia)*s(1,2)+t(5,ia)*s(2,2)+t(6,ia)*s(3,2))*itlrgt
      if (si.lt.sbe) then
        h1 = t(21,ia)
        pta(1) = pte(1)+h1*vta(1)
        pta(2) = pte(2)+h1*vta(2)
        pta(3) = pte(3)+h1*vta(3)
        d1 = s(1,2)*pta(1)+s(2,2)*pta(2)+s(3,2)*pta(3)-s(4,2)
        pcv(1) = pta(1)-s(1,2)*d1
        pcv(2) = pta(2)-s(2,2)*d1
        pcv(3) = pta(3)-s(3,2)*d1
        xr = t(8,ia)*t(6,ia)-t(9,ia)*t(5,ia)
        yr = t(9,ia)*t(4,ia)-t(7,ia)*t(6,ia)
        zr = t(7,ia)*t(5,ia)-t(8,ia)*t(4,ia)
        si = sbe*itlrgt
        s(1,2) = xr*cbe+vta(1)*si
        s(2,2) = yr*cbe+vta(2)*si
        s(3,2) = zr*cbe+vta(3)*si
        s(4,2) = s(1,2)*pcv(1)+s(2,2)*pcv(2)+s(3,2)*pcv(3)
      endif

999   return
c                            ---  Invalid drive surface
9028  ifl(2) = 28
      return
      end

C*********************************************************************
C*    SUBROUTINE     : subroutine cvpvo(vr, pcv, h, pta, vt)
c*     Project point onto tool & calculate right vector.
C*    PARAMETERS   
C*       INPUT  : 
C*          vr      - General direction of right vector.
C*       OUTPUT :  
C*          pcv     - Point on curve.
C*          h       - Height of project point up tool axis.
C*          pta     - Project point on tool axis.
C*          vt      - Right vector.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine cvpvo(vr, pcv, h, pta, vt)

      include 'com.com'
      include 'mocom.com'

      real*8 vr(3), pcv(3), h, pta(3), vt(3)
c          motion common equivalences
      integer*2 jd(600)
      equivalence (d,jd)

      integer*2 ia, isrf
      equivalence (ifl(51),ia), (ifl(54),isrf)

      real*8 vcv(3), sec
      real*4 px,py,pz,vx,vy,vz

C RLS - 03/27
      ifl(2) = 0
C RLS - END
      if (jd((isrf-1)*200+1).eq.CIRCLE) then
        call circpv (px,py,pz, vx,vy,vz)
      else
        call curvpv (px,py,pz, vx,vy,vz)
C RLS - 03/27
        if (ifl(2) .eq. 466) go to 999
C RLS - END
      endif
      pcv(1) = px
      pcv(2) = py
      pcv(3) = pz
      vcv(1) = vx
      vcv(2) = vy
      vcv(3) = vz

      h = t(27,ia)
      pta(1) = t(1,ia)+h*t(4,ia)
      pta(2) = t(2,ia)+h*t(5,ia)
      pta(3) = t(3,ia)+h*t(6,ia)

      vt(1) = vcv(2)*t(6,ia)-vcv(3)*t(5,ia)
      vt(2) = vcv(3)*t(4,ia)-vcv(1)*t(6,ia)
      vt(3) = vcv(1)*t(5,ia)-vcv(2)*t(4,ia)
      sec = dsqrt(vt(1)**2+vt(2)**2+vt(3)**2)
      if (sec.eq.0.0d0) sec = 1.0d0
      if (vr(1)*vt(1)+vr(2)*vt(2)+vr(3)*vt(3).lt.0.0d0) sec = -sec
      vt(1) = vt(1)/sec
      vt(2) = vt(2)/sec
      vt(3) = vt(3)/sec

C RLS - 03/27
  999 continue
C RLS - END
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine sfgideo (pcv, vcv)
c*                                                               
C*    PARAMETERS   
C*       INPUT  : 
C*          pte     - Tool end point.
C*          vta     - Tool axis vector.
C*          vr      - General direction of right vector.
C*          h       - Height of look point up tool axis.
C*       OUTPUT :  
C*          h       - Height of look point up tool axis.
C*          pta     - Point on tool axis.
C*          psf     - Point on surface.
C*          vt      - Right vector.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine sfgideo(pte, vta, vr, h, pta, psf, vt)

      include 'com.com'
      include 'mocom.com'

      real*8 pte(3), vta(3), vr(3), h, pta(3), psf(3), vt(3)

c          motion common equivalences
      real*4 ad(300)
      integer*2 jd(600)
      equivalence (d,ad,jd)

      integer*2 ia, isrf
      equivalence (ifl(51),ia), (ifl(54),isrf)

      real*8 fx,fy,fz, sx,sy,sz, sec

      t(21,ia) = h
      isrf = 2
      call dsrel
      h = t(21,ia)
      sx = s(1,2)
      sy = s(2,2)
      sz = s(3,2)

      psf(1) = s(5,2)
      psf(2) = s(6,2)
      psf(3) = s(7,2)
      h = (psf(1)-pte(1))*vta(1)+(psf(2)-pte(2))*vta(2)+
     x    (psf(3)-pte(3))*vta(3)
      if (h.lt.tool(2)) h = tool(2)
      pta(1) = pte(1)+vta(1)*h
      pta(2) = pte(2)+vta(2)*h
      pta(3) = pte(3)+vta(3)*h
      fx = vta(2)*sz-vta(3)*sy
      fy = vta(3)*sx-vta(1)*sz
      fz = vta(1)*sy-vta(2)*sx
      vt(1) = fy*vta(3)-fz*vta(2)
      vt(2) = fz*vta(1)-fx*vta(3)
      vt(3) = fx*vta(2)-fy*vta(1)
      sec = dsqrt(vt(1)**2+vt(2)**2+vt(3)**2)
      if (sec.eq.0.0d0) sec = 1.0d0
      if (vr(1)*vt(1)+vr(2)*vt(2)+vr(3)*vt(3).lt.0.0d0) sec = -sec
      vt(1) = vt(1)/sec
      vt(2) = vt(2)/sec
      vt(3) = vt(3)/sec

999   return
      end
