c**********************************************************************
c**
c**      CONTAINS:
c**          ucrvpv  ucvini   fndcor
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       ucrvpv.f , 25.1
c**    DATE AND TIME OF LAST  MODIFICATION
c**       04/29/15 , 15:10:50
c**
c** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
c**
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: UCRVPV                                            **
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C          SOLVES A POINT/VECTOR FOR THIS CV PER CURRENT TOOL LOC.
C **********************************************************************
C **********************************************************************

      subroutine ucrvpv (px,py,pz,vx,vy,vz)
 
      include 'com4a.com'
      include 'mocom.com'
      include 'gidcm1.com'
      include 'suvcom.com'
      include 'shrpcv.com'

      real*8 asn,u,a(3),b(3),fct
      real*4 f_dot4
      real*4 du0,stol
      integer*4 nclkey
      integer*2 kd(600),ietype,nwds,ishrpd
      real*4 ad(300)
      equivalence(d,ad,kd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      logical linit,lv81,lv8207,lv84,lv93,lfixfw
      real*4 herr,vtmp(3)
      integer*2 izro
      data izro /0/
      integer*2 jsh
 
      fct = 1.0d0
      du0 = 0.01
      stol = 1.e-4
      if (lcvpv) du0 = 0.0005
      if (isrf.eq.2 .and. ishrp.eq.0 .and. itfl.eq.-1) then
          du0 = 0.0005
      endif
      if (isrf .ne. 2) then
        ishrpd = ishrp
        ishrp = 0
      endif
      wedged = .false.
      linit = .true.
      lv81 = sc(169).lt.8.19999
      lv8207 = sc(169).lt.8.20799
      lv84 = sc(169).lt.8.49999
      lv93 = sc(169).lt.9.349d0
c          index to d-tbl
      idx=50*(isrf-1)
      jdx=2*idx
      kdx=4*idx
c          pt to h-val in tcol(ia) and set misc params
      jtim=0
      lgouck=0
      iknt=0
      ihx=6*isrf+9
      iux=ihx-2
      h=t(ihx,ia)
      asn=d(idx+2)
      call gtdesc(asn,nclkey,nwds,ietype)
      call evstup(nclkey,isrf)
      if (.not.lv93) call smtset(nclkey)
      nsegs=kd(kdx+2)
      iseg=kd(kdx+3)
c          if a seg is active, go try it
      if(iseg.gt.0)goto 40
c+-          if this is ds in reg mill stepover process, last
c+-          segno is in kd(204)                              21-apr-87
      if(isrf.ne.2.or.kd(kdx+4).eq.0)goto 20
      iseg=kd(kdx+4)
      kd(kdx+3)=iseg
      t(iux,ia)=.5
      goto 40
c+-          
20    iseg=1
c
c..... at start, when driving a curve, look if it has sharp corners
c
      if (ifl(280).eq.0 .and. itfl.eq.-2 .and. ishrp.eq.-1) then
        ishrp = 0
      endif
c*****************************    activate segment(iseg)
30    continue
c          record iseg and start at u=.5
      t(iux,ia)=.5
      if (isrf.eq.2.and.(dsuv.or.autouv)) t(iux,ia)=dsu
      if (isrf.eq.3.and.(csuv.or.autouv)) t(iux,ia)=csu
      kd(kdx+3)=iseg
c******************************  /* apply hpt to seg in delta form */
40    xe=t(1,ia)+t(4,ia)*h
      ye=t(2,ia)+t(5,ia)*h
      ze=t(3,ia)+t(6,ia)*h
      u=t(iux,ia)
      if(u.lt.0.)u=0.
      if(u.gt.1.)u=1.
      itim=0
      ctan=1.

      jsh = 0

      if (.not. lv84) fct = 1.0d0
      if (.not. lv8207) linit = .true.
C RLS - 03/27
42    call uevcvt(u,isrf,izro,a,b,ifl(2))
      if (ifl(2) .eq. 466) go to 99
c42    call uevcvt(u,isrf,izro,a,b)
C RLS - END

      if (ishrp.eq.1 .and. ifl(21).eq.0) then
        lfixfw = .false.
        if (zercor.eq.0) then
          lfixfw = abs(u-ushrp).lt.0.2 .and. fdshrp*(u-ushrp).ge.-1.e-5
        else if (zercor.eq.1) then
          lfixfw = t(19,ic).gt.0.8 .and. u.lt.0.2
        else if (zercor.eq.-1) then
          lfixfw = t(19,ic).lt.0.2 .and. u.gt.0.8
        endif
        if (lfixfw) then
          b(1) = zshrp(1)
          b(2) = zshrp(2) 
          b(3) = zshrp(3)
        endif
      endif
c          if here 1000 times, get out     ( safety exit )
      iknt=iknt+1
      if(iknt.gt.1000)goto 58

      den = b(1)**2+b(2)**2+b(3)**2
c              guard against zero divide       7-27-82
      if(abs(den).lt.1.e-8) den=.001
      if (lv81) den = den/3.
      uerr= (b(1)*(xe-a(1))+b(2)*(ye-a(2))+b(3)*(ze-a(3)))/den
      if(abs(uerr).lt.stol) goto 60

       if (lgouck.eq.0 .and. itim.ge.6 .and. ishrp.gt.0 .and. jsh.ge.2)
     x                                                             then
         u = ushrp
         a(1) = ptshrp(1)
         a(2) = ptshrp(2)
         a(3) = ptshrp(3)
         if (ifl(21).eq.0) then
           b(1) = zshrp(1)
           b(2) = zshrp(2) 
           b(3) = zshrp(3)
         else
           vtmp(1) = xe-a(1)
           vtmp(2) = ye-a(2)
           vtmp(3) = ze-a(3)
           b(1) = zshrp(2)*vtmp(3) - zshrp(3)*vtmp(2)
           b(2) = zshrp(3)*vtmp(1) - zshrp(1)*vtmp(3)
           b(3) = zshrp(1)*vtmp(2) - zshrp(2)*vtmp(1)
         endif          
         goto 60
       endif

      if(.not.linit)goto 45
      if (lv81.and.itim.eq.0) goto 48
      linit = .false.
      du = du0
      if (uerr.lt.0.0) du = -du0
      goto 50
45    continue
c          calc du
      den=oerr-uerr
c          if den small, use old ctan
      if (.not.lv81) then
        if(abs(den).gt.1.e-5) ctan=fct*du/den
c          neg ctan is unreal
        if(ctan.le.0.) ctan=-ctan
        if(ctan.gt.1.) ctan = 1.0
      else
        if(abs(den).gt.1.e-3) ctan=du/den
        if(ctan.le.0.) ctan=1.
      endif
48    du=uerr*ctan
50    continue
c          make sure next u does not exceed 0,1
      if(u+du.gt.1.)du=1.-u
      if(u+du.lt.0.)du=-u
c          update for next iter
      itim=itim+1

        if (ishrp.eq.1) then
          uic = t(19,ic)
          if (zercor.ne.0 .and. uic.lt.0.2) uic = uic+1
          uia = t(19,ia)
          if (uia .lt. 0) uia = 0
          if (zercor.ne.0 .and. uia.lt.0.2) uia = uia+1
          unx = u+du
          if (zercor.ne.0 .and. unx.lt.0.2) unx = unx+1
          if (zercor.eq.0 .and. unx.lt.0.2 .and. uia.gt.1) unx = unx+1

          if (fdshrp*(uia-uic).gt.0 .and. fdshrp*(unx-uic).lt.0) then
            uuu = u
            if (zercor.ne.0 .and. uuu.lt.0.2) uuu = uuu+1

            du = (uic - uuu)/2
            if(u+du.gt.1.)du=1.-u
            if(u+du.lt.0.)du=-u

          endif
        endif

       if (ishrp.gt.0 .and. abs(du).gt.1.e-5 .and.
     x                 (u-ushrp)*(u+du-ushrp).lt.0) then
         jsh = jsh+1
       endif

      u=u+du
      oerr=uerr
      if(itim.lt.120)goto 52
c
c...Flat spots on curves that
c...do not constitute a sharp corner
c...can give this routine a hard time
c...Loosen the tolerance if we are at the
c...end of the looping range to allow for a solution
c...QAR 97249
c
      if (itim .gt. 230) stol = 5.e-4


      if (lgouck.eq.1) then
        u = t(19,ia)
        goto 70
      endif
c
c...   We are not converging. Try successively reducing iteration step size.
c
      if((itim-itim/40*40).eq.0) fct = fct*.75d0
      ctan = ctan*.9d0
      if(itim.lt.240)goto 52
c          iterk failure. funny seg?
      ifl(2)=139
      goto 99
c          if u-chg real, go again.
52    if(abs(du).gt.1.e-5)goto 42
      if (.not.lv81) then
        if (abs(uerr).lt..001) goto 60
        if (u.gt..001.and.u.lt..999) goto 60
      endif
      call gtclsd (nclkey, 0, iclsd)
      if (iclsd.eq.0) goto 60

      if (ishrp.gt.0 .and. (ushrp.lt.0.001 .or. ushrp.gt.0.999))
     x  jsh = jsh+1

      u=1.-u
      linit = .true.
      goto 42
c          curvpv mystery exit
58    ifl(2)=134
      goto 99
c          u found this ept.  check h-value.
60    px=a(1)
      py=a(2)
      pz=a(3)
      herr=t(4,ia)*(px-xe)+t(5,ia)*(py-ye)+t(6,ia)*(pz-ze)
      h=h+herr
      if (sc(169).ge.9.25) herr = abs(herr) 
      if (.not.lcvpv .and. herr.lt.sc(27)*50.d0) goto 70
      if (jtim.gt.9 .or. herr.lt.sc(27)) goto 70
c          allow 10 h-fix
      jtim=jtim+1
      goto 40
c************************  normal exit path. fixup all data
70    continue
      sec=sqrt(b(1)**2+b(2)**2+b(3)**2)
      if(sec.gt.0.)goto 75
c          error.  sec=0.
      ifl(2)=135
      goto 99
75    continue
      
      vx=b(1)/sec
      vy=b(2)/sec
      vz=b(3)/sec

      if (ishrp.eq.1 .and. ifl(21).ne.0 .and.
     x    (lgouck.eq.1 .or. dabs(u-ushrp).gt.5.d-6)) then

        if (lgouck.eq.0) then
          lgouck = 1
          spx = px
          spy = py
          spz = pz
          svx = vx
          svy = vy
          svz = vz
          su = u
          sh = h
          vtmp(1) = xe-px
          vtmp(2) = ye-py
          vtmp(3) = ze-pz
          sdis = f_dot4(vtmp,vtmp)

          if (zercor .eq. 0) then
            if (fdshrp*(u-ushrp).lt.0) then
              t(19,ia) = ushrp + fdshrp*0.0001
            else
              t(19,ia) = ushrp - fdshrp*0.0001
            endif
          else
            if (u .gt. 0.8) then
              t(19,ia) = ushrp + 0.0001
              if (t(19,ia) .gt. 1) t(19,ia) = 0.0001
            else if (u .lt. 0.2) then
              t(19,ia) = ushrp - 0.0001
              if (t(19,ia) .lt. 0) t(19,ia) = 0.9999
            endif
          endif

          h = t(21,ia)
          du0 = 0.0005
          iknt = 0
          jtim = 0
          goto 40
        else if (lgouck.eq.1) then
          lgouck = 0
          dis = 100000.
          if (abs(uerr).lt.1.e-4) then
            vtmp(1) = xe-px
            vtmp(2) = ye-py
            vtmp(3) = ze-pz
            dis = f_dot4(vtmp,vtmp)
          endif
          dis = sqrt(dis)
          sdis = sqrt(sdis)
          dd = abs (dis-sdis)
          if (dd .lt. sc(27)) then
            if (zercor .eq. 0) then
              wedged = (su-ushrp)*(u-ushrp).lt.0
            else
              wedged = abs (u-su).gt.0.75
            endif
            if (wedged) then
              if ((zercor.ne.0 .and. fdshrp*(u-0.5).gt.0) .or.
     x            (zercor.eq.0 .and. fdshrp*(u-ushrp).lt.0)) then
                vxshrp(1) = svx
                vxshrp(2) = svy
                vxshrp(3) = svz
              else
                vxshrp(1) = vx
                vxshrp(2) = vy
                vxshrp(3) = vz
              endif
            endif
          endif

          if (sdis .lt. dis) then
            px = spx
            py = spy
            pz = spz
            vx = svx
            vy = svy
            vz = svz
            u = su
            h = sh
          endif
        endif
      endif

      t(iux,ia)=u
      t(ihx,ia)=h
 
99    if (.not.lv93) call smtrst()

      if (isrf .ne. 2) then
        ishrp = ishrpd
      endif

      return
      end

C **********************************************************************
C **  PROGRAM NAME: ucvini                                            **
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C          Finds a curve point close to the initial tool position
C **********************************************************************
C **********************************************************************

      subroutine ucvini(jret1)
 
      include 'com4a.com'
      include 'mocom.com'

      integer*2 jret1

      integer*2 kd(600)
      real*4 ad(300)
      equivalence(d,ad,kd),(itfl,ifl(49)),(iptk,ifl(50))
      integer*4 nclkey
      real*8 u,u0,u1,umin,dst,ddd,dmin,ccc,cmin,eps,asn,dco,wi
      real*8 a(3),b(3)
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)

      integer*2 izro
      data izro /0/
      integer*2 ietype,nwds,iclsd,idx

      u = 0.
      umin = 0.

      dst = 100*sc(91)
      if (ifl(21) .ne. 0) then
        dst = dst + tool(1)/2 + sc(24)
      endif
      dst = dst*dst
      dmin = dst

      jret1 = 0

20    if (u .gt. 1) goto 30
      call uevcvt(u,isrf,izro,a,b,ifl(2))
      if (ifl(2) .ne. 0) goto 99
      dco = 0
      ddd = 0
      do i = 1,3
        wi = t(i,ic) - a(i)
        dco = dco + wi*t(i+3,ic)
        ddd = ddd + wi*wi
      enddo
      ddd = ddd - dco*dco
      if (ddd .lt. dmin) then
        dmin = ddd
        umin = u
        call unitvc (b,b)
        cmin = t(7,ic)*b(1)+t(8,ic)*b(2)+t(9,ic)*b(3)
      endif

      u = u + 0.01
      goto 20

30    if (dmin .lt. dst) then
        eps = sc(27)*sc(27)
        iclsd = 0
        if (umin .lt. 0.1 .or. umin .gt. 0.9) then
          idx=50*(isrf-1)
          asn=d(idx+2)
          call gtdesc(asn,nclkey,nwds,ietype)
          call gtclsd (nclkey, 0, iclsd)
        endif
        u0 = umin
        u = umin-0.006
        u1 = umin + 0.005
        if (u1 .gt. 1 .and. iclsd.eq.0) u1 = 1

40      u = u + 0.001
        if (u .lt. 0) then
          if (iclsd .eq. 1) then
            u = u+1
          else
            u = 0
          endif
        else if (u .ge. 1) then
          if (iclsd .eq. 1) then
            u = u-1
            if (u1 .gt. 1) u1 = u1-1
          else
            u = 1
          endif
        endif
        call uevcvt(u,isrf,izro,a,b,ifl(2))
        if (ifl(2) .ne. 0) goto 99
        dco = 0
        ddd = 0
        do i = 1,3
          wi = t(i,ic) - a(i)
          dco = dco + wi*t(i+3,ic)
          ddd = ddd + wi*wi
        enddo
        ddd = ddd - dco*dco
        call unitvc (b,b)
        ccc = t(7,ic)*b(1)+t(8,ic)*b(2)+t(9,ic)*b(3)

        if (ddd.lt.dmin .or. (ddd.lt.dmin+eps .and. ccc.gt.cmin+0.1))
     x                                                           then
          dmin = ddd
          umin = u
          cmin = ccc
        endif
        if (u + 0.001 .eq. u0) u = u+0.001
        if (u .lt. u1) goto 40

        t(19,ia) = umin
        jret1 = 1
      endif
      return

99    ifl(2) = 466
      return
      end

C **********************************************************************
C **  PROGRAM NAME: fndcor                                            **
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C          Finds the first sharp corner on a curve in the forward
C **       direction
C **********************************************************************
C **********************************************************************

      subroutine fndcor
 
      include 'com4a.com'
      include 'mocom.com'
      include 'shrpcv.com'

      real*8 asn
      integer*4 nclkey
      integer*2 ietype,nwds,iclsd,ier

      real*8 u,u0,u1,ut,ubeg,uu,ddd,c0,c1
      real*8 a(3),b(3),b0(3),b1(3),bt(3),at(3),p1(3)
      real*8 f_dot
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      logical lfwd,lright

      integer*2 izro
      data izro /0/
 
      real*8 CO11
      parameter (CO11 = 0.981627)

      ier = 0
      ishrp = -1
      zercor = 0
      u = t(19,ia)
      bt(1) = t(7,ia)
      bt(2) = t(8,ia)
      bt(3) = t(9,ia)

      idx=50*(isrf-1)
      asn=d(idx+2)
      call gtdesc(asn,nclkey,nwds,ietype)
      call gtclsd (nclkey, 0, iclsd)

      call uevcvt(u,isrf,izro,a,b,ier)
      if (ier .eq. 466) goto 99

      ddd = f_dot (bt,b)
      lfwd = (ddd.ge.0)
c
c.... lfwd is true iff the curve forward is along the motion forward
c
      call unitvc (b,b0)

        if (iclsd .eq. 0) then
          u0 = u
        else
          if (lfwd) then
            ubeg = u
            u0 = 0
          else
            ubeg = u - 1.
            u0 = 1
          endif
        endif
c
c.... initial search starting with the current curve parameter, with the
c.... step = 0.01
c
20    if (lfwd) then
        u1 = u0 + 0.01
        if (u1 .gt. 0.999) return
      else
        u1 = u0 - 0.01
        if (u1 .lt. 0.001) return
      endif
      
      if (iclsd .eq. 1) then
        uu = u1 + ubeg
        if (uu .gt. 1) then
          uu = uu-1.
        else if (uu .lt. 0) then
          uu = uu+1.
        endif
      else
        uu = u1
      endif

      call uevcvt(uu,isrf,izro,a,b,ier)
      if (ier .eq. 466) goto 99

      call unitvc (b,b1)
      ddd = f_dot (b1,b0)

30    if (ddd .gt. CO11) goto 50
c
c..... refining binary search in the u0,u1 curve segment. abandoned if the
c..... curve segment turns out smooth
c
      call vctovc (a,p1)

40    if (dabs(u1 - u0) .lt. 1.e-5) then
        ddd = f_dot (b0,b1)
        if (ddd .gt. CO11) goto 50
        goto 45
      endif
      ut = (u0+u1)/2.

      if (iclsd .eq .1) then
        uu = ut + ubeg
        if (uu .gt. 1) then
          uu = uu-1.
        else if (uu .lt. 0) then
          uu = uu+1.
        endif
      else
        uu = ut
      endif

      call uevcvt(uu,isrf,izro,at,bt,ier)
      if (ier .eq. 466) goto 99

      call unitvc (bt,bt)
      c0 = f_dot (bt,b0)
      c1 = f_dot (bt,b1)
c
c..... lright means when we choose the right half of u0,u1
c
      lright = (c0 .ge. c1)
      if (lright) then
        ddd = c1
      else
        ddd = c0
      endif

      if (ddd .gt. CO11) goto 50

      if (lright) then
        u0 = ut
        call vctovc (bt,b0)
      else
        u1 = ut
        call vctovc (bt,b1)
        call vctovc (at,p1)
      endif

      goto 40

45    ishrp = 1
c
c..... found next corner.
c
      if (iclsd .eq. 1) then
        ushrp = u1 + ubeg
        if (ushrp .gt. 1) then
          ushrp = ushrp-1.
        else if (ushrp .lt. 0) then
          ushrp = ushrp+1.
        endif
        if (ushrp.lt.0.001 .or. ushrp.gt.0.999) then
c
c..... set 'corner at zero' flag
c
          if (lfwd) then
            zercor = 1
          else
            zercor = -1
          endif
        endif
      else
        ushrp = u1
      endif
c
c..... remember corner point
c
      ptshrp(1) = p1(1)
      ptshrp(2) = p1(2)
      ptshrp(3) = p1(3)

      if (lfwd) then
        zshrp(1) = b0(2)*b1(3) - b0(3)*b1(2)
        zshrp(2) = b0(3)*b1(1) - b0(1)*b1(3)
        zshrp(3) = b0(1)*b1(2) - b0(2)*b1(1)
        fdshrp = 1.
      else
        zshrp(1) = b1(2)*b0(3) - b1(3)*b0(2)
        zshrp(2) = b1(3)*b0(1) - b1(1)*b0(3)
        zshrp(3) = b1(1)*b0(2) - b1(2)*b0(1)
        fdshrp = -1.
      endif

      if (ifl(21) .eq. 0) then
c
c..... remember tangent vector after corner point
c
        vxshrp(1) = b1(1)
        vxshrp(2) = b1(2)
        vxshrp(3) = b1(3)

        b1(1) = zshrp(2)*b0(3) - zshrp(3)*b0(2)
        b1(2) = zshrp(3)*b0(1) - zshrp(1)*b0(3)
        b1(3) = zshrp(1)*b0(2) - zshrp(2)*b0(1)

        call unitvc (b1,b1)

        if (lfwd) then
          zshrp(1) = b1(1) + 0.001*b0(1)
          zshrp(2) = b1(2) + 0.001*b0(2)
          zshrp(3) = b1(3) + 0.001*b0(3)
        else
          zshrp(1) = -b1(1) + 0.001*b0(1)
          zshrp(2) = -b1(2) + 0.001*b0(2)
          zshrp(3) = -b1(3) + 0.001*b0(3)
        endif

      endif

      return

50    call vctovc (b1,b0)
      u0 = u1
      goto 20

99    continue
      if (ier .eq. 466) ifl(2) = 466

      return
      end
