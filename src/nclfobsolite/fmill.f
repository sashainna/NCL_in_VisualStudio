C**********************************************************************
C*    NAME         :  fmill.f
C*       CONTAINS:
C*                 fmill
C*                 fmout
C*                 fmproj
C*                 fmprj
C*                 fmprj1
C*                 fmprj2
C*                 fmilcs
C*                 fmevsf
c*                 fmilpar
C*                 fmilsav
C*    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       fmill.f , 26.2
C*     DATE AND TIME OF LAST MODIFICATION
C*       04/12/18 , 09:55:28
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmill
C*       Execute FMILL motion.
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
      subroutine fmill

      include 'com8a.com'
      include 'mocom.com'
      include 'fmill.com'

      real*8 apsw,hscal,tol,ftol,ust,vst,d1,co,totco,crad,ufct,hmin
      real*8 pte(24),ptl(9),pl(4),vfw(3),uv1(3),s0(10),s1(10),s00(10),
     x       s10(10),uv(3),vn(3),buf(12),tdat(640),avpl1(4),avpl2(4)
      real*8 f_dot,avcsf1(40),avthk1(40)
      real*4 u4,v4,usec,vsec,du,dv,delu,dir,ss(10)
      integer*4 j,nclkey,kcv,ksf,ksf11,ksec,nppp,npas,npts,kerr,npts1
      integer*4 jparam(5)
      integer*2 nwds,ietype,lnk,tonp,iret,irapt,inewfr,isftyp,ibndr,
     1          iret1,ifwd,nsf,lgoug,iclass,isubcl,jptk,numitm,ier,
     x          inips,ini2,ker2,lboth,ityp11,ityp12,iavcsf1,navcsf1
      integer*2 iparam(8)
      integer*2 isf/4/
      integer*2 isfsec /2/
      logical trflg/.false./
      logical lnetsf/.false./
      logical lgood0/.false./
      logical lg1st/.true./
      logical lbull/.false./
      logical lv95
      real*8 r8
      real*4 r4(2)
      equivalence (r4,r8)
      integer*2 ntk
      equivalence(ntk,ifl(79))
      integer*2 izro /0/
      integer*2 rdis,rpln,rsrf
      parameter (rdis=1,rpln=2,rsrf=3)
      
      lv95 = sc(169).lt.9.549d0
c
c... Check for ta/normal,ps or ta/same and ball-end mill
c... and no refsys
c
      if (ifl(23).gt.1) goto 9226
      lbull = (abs(tool(6)) .gt. 2.*sc(27))
c      if (abs(tool(6)).gt.sc(27)*2.0 .and. ifl(23).ne.1) goto 9228
      if (ifl(72).ne.0) goto 9227

      lpssec = (ifl(23).eq.TA_NORMAL_PS .and. lsecps)
      ksec = 0
      if (lpssec) then
        call gtdesc (sc(195),nclkey,nwds,ietype)
        if (ietype .eq. PLANE) then
          call gtentt (sc(195),trflg,nclkey,ietype,pl)
        else if (ietype .eq. SURF) then
          call ncl_get_sf_primtyp (nclkey,ietype)
          if (ietype .eq. 3) then
            call primpln (nclkey,izro,pl)
            ietype = PLANE
          endif
        else
          lpssec = .false.
        endif
        if (ietype .eq. PLANAR) then
          lpssec = .false.
          co = f_dot (sc(4),pl)
          if (co .lt. 0) call mnvc (pl,pl)
          call vctovc (pl,sc(4))
          ifl(23) = TA_SAME
        endif
        if (lpssec) ksec = nclkey
      endif
c
c... Initialize
c
      do j=1,24
        pte(j) = 0.0d0
      enddo

      do j=1,3
        ifl(330+j) = 0
      enddo

      ufct = 1.0d0
      if (ifl(264).eq.1) ufct = 25.4d0
      ifwd = isc10(3)
      d1   = 0.0d0
      dir  = 0.0d0
      crad = sc(29)

      if (sc(12).lt.0.0d0) then
        hscal = -sc(12)
        npas = 0
      else
        npas  = sc(12)
      endif

      call gtdesc(sc(11),ksf11,nwds,isftyp)
      call sftype (ksf11,ityp11)

      lnetsf = .false.
      if (ityp11 .eq. NETSF) then
        i = 1
        ksf = 0
        call ntsf1 (ksf11,ksf,i,nsf)
        if (ksf .eq. 0) goto 9122
        lnetsf =  (nsf .gt. 1)
        call ptdesc (ksf,SURF,apsw)
      else
        apsw = sc(11)
        ksf = ksf11
      endif

      if (isc10(2).eq.0) then
        r8  = sc(13)
        ust = r4(1)
        vst = r4(2)
      else
        call gtgeom(sc(13),pte,nclkey,nwds,ietype)
        u4 = .5
        v4 = .5
        call sfpt(apsw,pte,isf,dir,u4,v4,ss)
        ust = u4
        vst = v4
      endif

      nppp = 0
      tol = sc(27) / ufct
      if (sc(14) .gt. 0) then
        nppp = sc(14)
        if (nppp .le. 1) nppp = 0
      else
        ftol = -sc(14)
        if (ftol .gt. sc(27)/1000.d0) tol = ftol / ufct
      endif
c
c... Calculate distance between passes based on scallop height.
c
      hmin = 0.01*tol*ufct
      if (npas .lt. 1) then
        if (hscal.lt.hmin) hscal = hmin
        if (hscal.ge.crad) then
          d1 = 2.*crad + tool(6)
        else
          d1 = crad**2 - (crad-hscal)**2
          if (d1.lt.0.0d0) d1 = 0.0d0
          if (lv95) then
            d1 = dsqrt(d1) * 2.0d0 + tool(6)
          else
            d1 = 2.*(dsqrt(d1) + tool(6))
          endif
        endif
        d1 = d1 / ufct
        if (d1.lt.2*tol) d1 = 2*tol
        npas = 0
      endif

      call gtdesc(sc(15),kcv,nwds,ietype)
      call evstup(ksf, isf)
      call vctovc(sc, ptl)
      call vctovc(sc(4), ptl(4))

      tonp = isc10(4)

      if (lnetsf) then
        ier = 0
        call ntsf2 (ksf11,nsf,ksf,kcv,tol,ust,vst,ifwd,ier)
        if (ier .ne. 0) then
          if (ier .gt. 0) then
            ifl(2) = ier
          else
            ifl(2) = 122
          endif
          goto 9990
        endif
        call ptdesc (ksf,SURF,apsw)
        u4 = .5
        v4 = .5

        call sfinit (apsw, isf, u4, v4)

        lnetsf =  (nsf .gt. 1)
      endif

      lboth = 0
      if (avflg .gt. 50) then
        lboth = 1
        avflg = avflg - 50
      endif

      iparam(1) = ifwd
      iparam(2) = tonp
      iparam(3) = avflg
      iparam(4) = avlst
      iparam(5) = avdir
      iparam(6) = lboth
      iparam(7) = ifl(23)
      iparam(8) = delflg

      jparam(1) = ksf
      jparam(2) = kcv
      jparam(3) = ksec
      jparam(4) = npas
      jparam(5) = nppp
c
c... Call ncl_fmcreate to create flowline motion.
c
      call ncl_fmcreate (tol,d1,ust,vst,jparam,iparam,npts, sc(28))
      if (npts .le. 0) then
        call fmlerr (127)
        goto 9990
      endif
c
c... Organize AVOID data
c
      if (avflg.gt.0 .and. avflg.le.rsrf) then
        if (avflg.eq.rsrf) then
          call gtdesc (avasn1,nclkey,i,ietype)
          call ncl_get_sf_primtyp (nclkey,ietype)
          if (ietype .eq. 3) then
            avflg = rpln
            call primpln (nclkey,izro,avpl1)
          endif
        else if (avflg.eq.rpln) then
          call gtentt (avasn1,trflg,nclkey,ietype,avpl1)
        endif
        if (avfr1.eq.0 .and. sc(21).ne.0) avfr1 = sc(21)
        if (avrpto.eq.rsrf) then
          call gtdesc (avasn2,nclkey,i,ietype)
          call ncl_get_sf_primtyp (nclkey,ietype)
          if (ietype .eq. 3) then
            avrpto = rpln
            call primpln (nclkey,izro,avpl2)
          endif
        else if (avrpto.eq.rpln) then
          call gtentt (avasn2,trflg,nclkey,ietype,avpl2)
        endif
        if (avfr2.eq.0 .and. sc(18).ne.0) avfr2 = sc(18)
      endif
c
c... Determine sf side
c
      totco = 0.d0
      tadir = 0.0d0
      iret = -1
      iret1 = -1
      do j = 1,npts
        call ncl_fmgetuv(j,uv,iret)
        call uevsft(uv(1), uv(2), isf, buf,ier)
        if (ier .gt. 0) goto 9990
        call f_cross(buf(4),buf(7),vn)
        call unitizevc(vn)
        co = f_dot(vn,sc(4))
        if (ifl(23).eq.TA_SAME) then
          if (co.gt.0.01d0) then
            if (tadir.eq.-1.0d0) then
              call fmlerr (148)
              goto 9990
            endif
            tadir = 1.0d0
          elseif (co.lt.-0.01d0) then
            if (tadir.eq. 1.0d0) then
              call fmlerr (148)
              goto 9990
            endif
            tadir = -1.0d0
          endif
        endif
        totco = totco + f_dot(vn,sc(4))
      enddo
      if (totco.ge.0.d0) then
        tadir = 1.0d0
      else
        tadir = -1.0d0
      endif
c
c... calculate tool positions
c
      crad = sc(29)
      call vctovc(sc(4),pte(4))
      iret = 0
      inips = 0
      ini2 = 0

      do j = 1,npts
        call ncl_fmgetuv(j,uv,iret)
        call uevsft(uv(1), uv(2), isf, buf,ier)
        if (ier .gt. 0) goto 9990
        call vctovc(buf,pte)
        call f_cross(buf(4),buf(7),vn)
        call unitizevc(vn)
        call vctmsc(vn,vn,tadir)

        if (lpssec) then
          if (j .eq. 1) then
            usec = 0.5
            vsec = 0.5
            call sfini1 (sc(195),isfsec,usec,vsec,pte,sc(4))
            dir = 1
          endif
          call sfpt (sc(195),pte,isfsec,dir,usec,vsec,ss)
          do k=1,3
            pte(k+3) = ss(k)
          enddo
        endif

        if (j .lt. npts) then
          call ncl_fmgetuv(j+1,uv1,iret1)
          call uevsft(uv1(1),uv1(2),isf,buf,ier)
          if (ier .gt. 0) goto 9990
          call vcmnvc(buf,pte,vfw)
        endif

        if (ifl(23).eq.TA_SAME .or. lpssec) then
          co = f_dot(vn,pte(4))
          if (dabs(co).lt.0.99999) then
            if (tonp .gt. 0 .or. lbull) then
              call fmproj(inips,apsw,uv,pte,vfw)
            else
              call uvcplvc(pte,vn,pte,crad)
              call uvcplvc(pte,pte(4),pte,-crad)
            endif
          endif
        else if (ifl(23).eq.TA_NORMAL_PS) then
          call vctovc(vn,pte(4))
        endif

          lg1st = .true.
          du = 0.1
55        lgoug = 0
          if (du .lt. 0.0005) goto 110

          if (ifl(23).eq.TA_NORMAL_PS .and. lnetsf .and. j.lt.npts)
     x                                                           then
c            call ncl_fmgetuv(j+1,uv1,iret1)
            if (uv1(2).eq.uv(2)) then
              delu = uv1(1) - uv(1)
              if (delu .gt. 0) then
                delu = du
              else
                delu = -du
              endif

              u4 = uv(1) + delu
              v4 = uv(2)
              call fmprj2 (pte,u4,v4,isf,ini2,s1,lgoug,ker2)
              dv = v4 - uv(2)
              if (abs(dv) .gt. 0.2) then
                du = du/2.
                goto 55
              endif

              u4 = uv(1) - delu
              v4 = uv(2)
              call fmprj2 (pte,u4,v4,isf,ini2,s0,lgoug,ker2)
              dv = v4 - uv(2)
              if (abs(dv) .gt. 0.2) then
                du = du/2.
                goto 55
              endif

            endif
          endif

          if (lgoug .ge. 1) then
            if (.not.lg1st) then
c
c..... if still gouging just skip this position
c
              goto 111
            endif
c
c..... if the two planes are same use the previous point planes
c
            co = f_dot(s0,s1)
            if (dabs(co) .gt. 0.99) then
              if (lgood0) then
                do i = 1,10
                  s0(i) = s00(i)
                  s1(i) = s10(i)
                enddo
              else
                goto 111
              endif
            else
              lgood0 = .true.
              do i = 1,10
                s00(i) = s0(i)
                s10(i) = s1(i)
              enddo
            endif
            call fmprj3 (s0,s1,pte,vfw,tol)
            call vctovc(pte(4),vn)
            lg1st = .false.
            lgoug = 0
            goto 55
          else if (lg1st) then
            lgood0 = .false.
          endif

110     continue
        call uvcplvc(pte,vn,pte,sc(23))
        call ncl_fm_ptstor (pte,uv(1),uv(2),iret)
111     continue
      enddo

        if (ifl(23).eq.TA_SAME .and. lnetsf) then
c
c..... deloop the tool path
c
          call fmdelp (npts,crad,tol)
          if (npts .le. 0) then
            call fmlerr (127)
            goto 9990
          endif
        endif
          
      if (navcsf .gt. 0) then
        ier = 0
        sc25 = sc(25)
        sc(25) = 0
        sc(143+isf) = apsw
                
        npts1 = npts
        call ncl_fmptlst_create()
        
        navcsf1 = navcsf
        do iavcsf = 1,navcsf
          avcsf1(iavcsf) = avcsf(iavcsf)
          avthk1(iavcsf) = avthk(iavcsf)
        enddo
        
        iavcsf = 1
        navcsf = 0
        do iavcsf1 = 1,navcsf1
            call gtdesc(avcsf1(iavcsf1),nclkey,nwds,ietype)
            call sftype (nclkey,ityp12)
            if (ityp12 .eq. NETSF) then
                ksf = 0
                iavcsf2 = 1
                call ntsf1 (nclkey,ksf,iavcsf2,nsf)
                call ptdesc (ksf,SURF,apsw)
                avcsf(iavcsf) = apsw
                avthk(iavcsf) = avthk1(iavcsf1)
                do isf = 2, nsf
                    ksf = 0
                    call ntsf1 (nclkey,ksf,isf,nsf)
                    call ptdesc (ksf,SURF,apsw)
                    avcsf(isf+iavcsf-1) = apsw  
                    avthk(isf+iavcsf-1) = avthk1(iavcsf1)
                enddo
                navcsf = navcsf + nsf
                iavcsf = iavcsf + nsf
            else
                avcsf(iavcsf) = avcsf1(iavcsf1)               
                avthk(iavcsf) = avthk1(iavcsf1)
                navcsf = navcsf + 1
                iavcsf = iavcsf + 1
            endif
        enddo
                                         
       do iavcsf = 1,navcsf
           call seticsf(iavcsf)
           call gtdesc(avcsf(iavcsf),nclkey,i,ietype)
           call ncl_fm_chkpts (nclkey,npts,avthk(iavcsf),tol,ier)
           
           if (ier .ne. 0 .or. npts .le. 0) then
             call ncl_fmptlst_copy()
             goto 120
           endif
        enddo
        if (ier .eq. 0 .and. npts .gt. 0) then
           call ncl_fmptlst_free()
          goto 130
        endif
120     continue
        ier = 0
      
        call ncl_fm_chkpts1 (avcsf,navcsf,npts1,avthk,tol,ier) 
        if (ier .ne. 0 .or. npts .le. 0) then
            if (ier .gt. 0) then
              call fmlerr (ier)
            else
              call fmlerr (127)
            endif
            goto 9990
        endif
        npts = npts1
   
130     continue  
        sc(25) = sc25
      endif
c
c... Output points to cl file
c
      ntk  = 0
      jptk = 0
      lnk  = 1
      iclass = 5000
      isubcl = 5
      iret = 0
      irapt = 0
      ibndr = 0
      inewfr = 0

      do j = 1,npts
        call ncl_fm_ptget (j,pte,iret)
        if (j.eq.npts) call vctovc(pte,ptl)

        if (j.eq.1 .and. sc(16).gt.0) then
          if (sc(18).lt.0.0d0) then
            call ppwrt(5)
          else if (sc(18).gt.0.0d0) then
            call fedmut(sc(18))
            inewfr = 1
          endif
          isubcl = 5
          numitm = 1
          kerr   = 0
          if (sc(16).eq.rsrf) then
            call gtdesc(sc(17),nclkey,i,ietype)
            call ncl_get_sf_primtyp(nclkey,ietype)
            if (ietype .eq. 3) then
              call primpln (nclkey,izro,pl)
              call plnint (pte,pte(4),pl,ptl,kerr)
            else
              call fmprj (pte,ptl,sc(17),kerr)
            endif
          else if (sc(16).eq.rpln) then
            call gtentt(sc(17),trflg,nclkey,ietype,pl)
            call plnint (pte,pte(4),pl,ptl,kerr)
          else
            call uvcplvc(pte,pte(4),ptl,sc(17))
          endif
          if (kerr.eq.0) then
            call vctovc(pte(4),ptl(4))
            call putcl (iclass,isubcl,numitm,ptl)
            if (ifl(154).eq.1) call tdsply(lnk,ptl,jptk)
          endif
          isubcl = 5
        endif

        if (inewfr .eq. 1 .and. ibndr.eq.0) then
          inewfr = 0
          call fedmut (sc(123))
        endif

        if (irapt .eq. 1) then
          irapt = 0
c
c..... rapid at the clearance (retract) plane to a point above the reentry
c
          call ppwrt(5)
          isubcl = 5
          numitm = 1
          kerr   = 0

          if (avrpto.eq.rsrf) then
            call fmprj (pte,ptl,avasn2,kerr)
          else if (avrpto.eq.rpln) then
            call plnint (pte,pte(4),avpl2,ptl,kerr)
          else
            call uvcplvc(pte,pte(4),ptl,avasn2)
          endif
          if (kerr.eq.0) then
            call vctovc(pte(4),ptl(4))
            call putcl (iclass,isubcl,numitm,ptl)
            if (ifl(154).eq.1) call tdsply(lnk,ptl,jptk)
          endif
          isubcl = 5
c
c..... at the rapto feedrate go down to the rapto distance above the reentry
c
          if (avfr2.lt.0) then
            call ppwrt(5)
          else if (avfr2.gt.0) then
            call fedmut(avfr2)
          endif

          call putcl (iclass,isubcl,numitm,pte)
          if (ifl(154).eq.1) call tdsply(lnk,pte,jptk)
          inewfr = 1
          isubcl = 5

        else
        call fmout (tdat, pte, lnk, jptk, iclass, isubcl)
        endif

        if (iret .eq. 1) then
c
c..... output all on-surface points before retract
c
          if (ntk.gt.0) then
            numitm = ntk/(3+ifl(82)*3)
            call putcl (iclass,isubcl,numitm,tdat(1))
            ntk = 0
          endif
          irapt = 1
          if (avfr1.lt.0) then
            call ppwrt(5)
          else if (avfr1.gt.0) then
            call fedmut(avfr1)
          endif
          isubcl = 5
          numitm = 1
          kerr   = 0
          if (avflg.eq.rsrf) then
            call fmprj (pte,ptl,avasn1,kerr)
          else if (avflg.eq.rpln) then
            call plnint (pte,pte(4),avpl1,ptl,kerr)
          else
            call uvcplvc(pte,pte(4),ptl,avasn1)
          endif
          if (kerr.eq.0) then
            call vctovc(pte(4),ptl(4))
            call putcl (iclass,isubcl,numitm,ptl)
            if (ifl(154).eq.1) call tdsply(lnk,ptl,jptk)
          endif
          isubcl = 5
        else if (iret.eq.2 .and. sc(22).ne.0) then
c
c..... output all on-surface points before retract
c
            if (ntk.gt.0) then
              numitm = ntk/(3+ifl(82)*3)
              call putcl (iclass,isubcl,numitm,tdat(1))
              ntk = 0
            endif

          if (ibndr .eq. 0) then
            ibndr = 1
            if (sc(22) .lt. 0) then
              call ppwrt(5)
            else
              call fedmut(sc(22))
            endif
          else
            ibndr = 0
            inewfr = 1
          endif
        endif
      enddo

      ifl(270) = 0

      if(ntk.gt.0) then
         numitm = ntk/(3+ifl(82)*3)
         call putcl (iclass,isubcl,numitm,tdat(1))
         ntk = 0
      endif

      call vcmnvc(pte,ptl,ptl(7))
      call vctovc(pte,ptl)
      call vctovc(pte(4),ptl(4))

      if (sc(19).gt.0) then
        if (sc(21).lt.0.0d0) then
          call ppwrt(5)
        else if (sc(21).gt.0.0d0) then
          call fedmut(sc(21))
        endif
        iclass = 5000
        isubcl = 5
        numitm = 1
        kerr   = 0

        if (sc(19).eq.rsrf) then
          call gtdesc(sc(20),nclkey,i,ietype)
          call ncl_get_sf_primtyp(nclkey,ietype)
          if (ietype .eq. 3) then
            call primpln (nclkey,izro,pl)
            call plnint (ptl,ptl(4),pl,ptl,kerr)
          else
            call fmprj (ptl,ptl,sc(20),kerr)
          endif
        else if (sc(19).eq.rpln) then
          call gtentt(sc(20),trflg,nclkey,ietype,pl)
          call plnint (ptl,ptl(4),pl,ptl,kerr)
        else
          call uvcplvc(ptl,ptl(4),ptl,sc(20))
        endif

        if (kerr.eq.0) then
          call conv8_8 (ptl,buf,9)
          call putcl (iclass,isubcl,numitm,buf)
          if (ifl(154).eq.1) call tdsply(lnk,buf,jptk)
        endif
      endif

      call vctovc(ptl,sc)
      call vctovc(ptl(4),sc(4))
      call unitvc(ptl(7),sc(7))
      if (ifl(154).eq.0) call tdsply(lnk,ptl,jptk)

      call ncl_fmfin

      goto 9999

9122  ifl(2) = 122
      goto 9990

9148  ifl(2) = 148
      goto 9990

9226  ifl(2) = 226
      goto 9990

9227  ifl(2) = 227
      goto 9990

9228  ifl(2) = 228
      goto 9990

9466  ifl(2) = 466
      goto 9990

9990  err = .true.

9999  return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmlerr (ierr)
C*       Error exit: call fmfin and set error number.
C*********************************************************************
C
      subroutine fmlerr (ierr)

      include 'com8a.com'
      integer*2 ierr

        call ncl_fmfin
        ifl(2) = ierr

      return
      end

C*********************************************************************
C*    E_SUBROUTINE : subroutine fmout (tdat,tb,lnk,jptk,iclass,isubcl)
C*       Output FMILL motion.
C*    PARAMETERS
C*       INPUT  :
C*          tdat    - Output buffer
C*          tb      - Point to output
C*          lnk     - line count.
C*          jptk    - Point count.
C*          iclass  - Class (5000)
C*          isubcl  - Sub class
C*       OUTPUT :
C*          isubcl  - Set to 6 for continuation rec.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine fmout (tdat, tb, lnk, jptk, iclass, isubcl)

      include 'com8a.com'

      real*8 tdat(120), tb(6)
      integer*2 lnk, jptk, iclass, isubcl

      integer*2 i, n, numitm
      integer*2 ntk
      equivalence (ntk,ifl(79))
c
      n = 3+ifl(82)*3
      max = 120

      do 20 i=1,n
20    tdat(ntk+i) = tb(i)

      if (ifl(154).eq.1.or.jptk.eq.1) call tdsply(lnk,tdat(ntk+1),jptk)
c
c...Output tdat array to the cl file if record is fullg316
c
      ntk = ntk+n
      if (ntk.lt.max) goto 999
      numitm=ntk/n
      call putcl (iclass,isubcl,numitm,tdat)
      ntk = 0
      isubcl = 6

999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmproj (ip,apsw,uv,pte,nvec)
C*       Move the tool along tlaxis to put it on the surface. Adjusted
C*       from tltosf (poksup.f).
C*    PARAMETERS
C*       INPUT  :
C*          pte    - tool position: te + ta
C*          ip     - flag: initialize if 1
C*          nvec   - forward direction (not used)
C*       OUTPUT :
C*          pte    - new tool end
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine fmproj(ip,apsw,uv,pte,nvec)

      include 'com8a.com'
      include 'mocom.com'

      real*8 apsw,pte(6),uv(2),nvec(3)
      real*4 u,v
      integer*2 ip

      integer*2 isrf
      equivalence (ifl(54),isrf)

      real*8 pcon, pco, psi, svco, dst, ptol
      real*8 xte, yte, zte, xta, yta, zta
c          motion common equivalences
      real*4 ad(400)
      integer*2 kd(800)
      equivalence (d,ad,kd)
      real*4 asc(100), sbe
      equivalence (sc,asc),(asc(63),sbe)
      integer*2 kmax
      parameter (kmax=200)
      equivalence (sc(167),ptol)

      isrf = 1
      u = uv(1)
      v = uv(2)
      if (ip .eq. 0) then
c
c..... use ps 1st-look pt
c
        s(8,1)  = pte(1)+pte(4)*tool(4)
        s(9,1)  = pte(2)+pte(5)*tool(4)
        s(10,1) = pte(3)+pte(6)*tool(4)

        call sfinit (apsw, isrf, u, v)
        ip = 1
        call surfpn (u,v,1)
        if (ifl(2).gt.0) return
c
c..... if psnorm not up, reverse direc(1) and ps tanpl.
c
        if (pte(4)*s(1,1)+pte(5)*s(2,1)+pte(6)*s(3,1) .le. 0.) then
          ad(2)=-ad(2)
          call mnvc4 (s(1,1))
          s(4,1) = -s(4,1)
        endif
      endif
      xte = pte(1)
      yte = pte(2)
      zte = pte(3)
      xta = pte(4)
      yta = pte(5)
      zta = pte(6)
      iknt = 0

100   s(8,isrf) = xte+xta*tool(2)
      s(9,isrf) = yte+yta*tool(2)
      s(10,isrf) = zte+zta*tool(2)
      call surfpn(u,v,1)

      if (ifl(2).gt.0) return
      iknt = iknt+1
      if (iknt.gt.kmax) goto 9138
      pco = xta*s(1,1) + yta*s(2,1) + zta*s(3,1)
c
c...  If sf normal opposes tool axis, reverse surface direction
c...  and try again
c
      if (pco.lt.0.0) then
        ad(2) = -ad(2)
        goto 100
      endif
c
c...  Calculate distance down tool axis to plane on surface returned from
c...  psrel in s(,1) array
c
      if (pco.gt.1.) pco = 1.
      psi = dsqrt(1.-pco**2)
      pcon = s(4,1)+tool(2)*(1.-pco)+tool(6)*psi
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
      pte(1) = xte
      pte(2) = yte
      pte(3) = zte
      goto 999

9138  ifl(2) = 138

999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmprj (pte,ptl,asw,kerr)
C*       Move the tool along tlaxis to put it on the surface. Adjusted
C*       from tltosf (poksup.f).
C*    PARAMETERS
C*       INPUT  :
C*          pte    - tool position: te + ta
C*          asw    - Surface
C*       OUTPUT :
C*          ptl    - new tool end
C*          kerr   - error
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine fmprj (pte,ptl,asw,kerr)

      include 'com8a.com'
      include 'mocom.com'

      real*8 pte(6),ptl(3),asw
      integer*2 kerr

      real*4 u,v
      integer*2 idx,jdx

      integer*2 isrf
      equivalence (ifl(54),isrf)

      real*8 pcon, pco, psi, svco, dst, ptol
      real*8 xte, yte, zte, xta, yta, zta
c          motion common equivalences
      real*4 ad(400)
      integer*2 kd(800)
      equivalence (d,ad,kd)
      real*4 asc(100), sbe
      equivalence (sc,asc),(asc(63),sbe)
      integer*2 kmax
      parameter (kmax=200)
      equivalence (sc(167),ptol)

        isrf = 3
        idx=(isrf-1)*50
        jdx=idx*2
c
c..... use 1st-look pt
c
        s(8,isrf)  = pte(1)+pte(4)*tool(2)
        s(9,isrf)  = pte(2)+pte(5)*tool(2)
        s(10,isrf) = pte(3)+pte(6)*tool(2)

        u = 0.5
        v = 0.5
        call sfinit (asw, isrf, u, v)

        call surfpn (u,v,1)
        if (ifl(2).gt.0) goto 9138
c
c..... if psnorm not up, reverse direc(1) and ps tanpl.
c
        if (pte(4)*s(1,isrf)+pte(5)*s(2,isrf)+pte(6)*s(3,isrf) .le. 0)
     x                                                            then
          ad(jdx+2)=-ad(jdx+2)
          call mnvc4 (s(1,isrf))
          s(4,isrf) = -s(4,isrf)
        endif

        xte = pte(1)
        yte = pte(2)
        zte = pte(3)
        xta = pte(4)
        yta = pte(5)
        zta = pte(6)
        iknt = 0

100   s(8,isrf) = xte+xta*tool(2)
      s(9,isrf) = yte+yta*tool(2)
      s(10,isrf) = zte+zta*tool(2)
      call surfpn(u,v,1)

      if (ifl(2).gt.0) goto 9138
      iknt = iknt+1
      if (iknt.gt.kmax) goto 9138
      pco = xta*s(1,isrf) + yta*s(2,isrf) + zta*s(3,isrf)
c
c...  If sf normal opposes tool axis, reverse surface direction
c...  and try again
c
      if (pco.lt.0.0) then
        ad(jdx+2) = -ad(jdx+2)
        goto 100
      endif
c
c...  Calculate distance down tool axis to plane on surface returned from
c...  psrel in s(,1) array
c
      if (pco.gt.1.) pco = 1.
      psi = dsqrt(1.-pco**2)
      pcon = s(4,isrf)+tool(2)*(1.-pco)+tool(6)*psi
      svco = pco
      if (pco.lt..2) pco = .2
      dst = (xte*s(1,isrf)+yte*s(2,isrf)+zte*s(3,isrf)-pcon)/pco
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

      ptl(1) = xte
      ptl(2) = yte
      ptl(3) = zte
      goto 999

9138  ifl(2) = 0
      kerr = 1

999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmilcs (te,ta,cpt,cve,cuv,tol,
c*                                          isf,kinit,side,ier)
C*       Project the tool to the Check Surface.
C*    PARAMETERS
C*       INPUT  :
C*          te   - tool end
C*          ta   - tool axis
C*          tol  - tolerance
C*          kinit - first call if 1
C*          isf  - CS number
C*          cuv  - CS parameters: u,v,height
C*          side - Calling routine should not modify this value.
C*       OUTPUT :
C*          cpt     CS plane normal
C*          cve     CS plane vector
C*          cuv     CS parameters
C*          side    Initialized to 1 on first call.  Set depending on
C*                  side of check surface cutter is on.
C*          ier     error
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine fmilcs (te,ta,cpt,cve,cuv,tol,isf,kinit,side,ier)

      include 'com8a.com'
      include 'mocom.com'
      include 'fmill.com'

      real*4 te(3),ta(3),cpt(3),cve(3),cuv(3),side
      real*8 tol
      integer*2 isf,kinit,ier

      integer*2 ia,isrf
      equivalence (ifl(51),ia),(ifl(54),isrf)

c          motion common equivalences
      real*4 ad(400)
      integer*2 kd(800)
      equivalence (d,ad,kd)
      real*4 asc(100), sbe
      equivalence (sc,asc),(asc(63),sbe)

      real*8 c,avcsj
      real*4 u,v,h,px,py,pz,vx,vy,vz,co,rx,ry,rz
      real*4 teh(3)

      integer*4 cskey
      integer*2 idx,jdx,itype,iclsd,idum1,idum2
      integer*2 kc(4),kcs(4)
      equivalence (c,kc),(avcsj,kcs)

        isrf = isf
        idx = (isrf-1)*50
        jdx = idx*2
        avcsj = avcsf(javcsf)
        itype = kcs(4)

        u = cuv(1)
        v = cuv(2)
        h = cuv(3)

        if (kinit .gt. 0) then

          sc(143+isrf) = avcsj
          call gtdesc(avcsj,cskey,idum1,idum2)
          h = tool(5)

          teh(1) = te(1) + ta(1)*h
          teh(2) = te(2) + ta(2)*h
          teh(3) = te(3) + ta(3)*h

          if (itype .eq. SURF) then
            ad(jdx+2) = 1
            ifl(330+isrf) = 0
            if (ifl(361).ne.1 .and. ifl(340).ne.2) then
              ifl(330+isrf) = ifl(340)
              call tbdini (cskey, isrf, tol, ifl(330+isrf))
              if (ifl(2).gt.0) goto 999
            endif
              call wlsf0(teh,ta,u,v,tol)
          else
            side = 1
            c = 0
            kc(1) = itype
            if (itype .eq. CURVE) then
              call gtclsd (cskey, 0, iclsd)

              call wlcv0(teh,ta,u,tol)
              kc(2) = 1
              kc(3) = 1
            else
              call gtgeom(avcsj,d(idx+3),cskey,idum1,idum2)
            endif
            d(idx+1) = c
            d(idx+2) = avcsj
          endif
        endif

        ihx=6*isrf+9
        ia = 3
        t(ihx-2,ia) = u
        t(ihx-1,ia) = v
        t(ihx,ia) = h

        do k = 1, 3
          t(k,ia) = te(k)
          t(k+3,ia) = ta(k)
        enddo

        if (itype .eq. SURF) then
          call wlproj (tol)
          if (ifl(2).gt.0) goto 999
          do k = 1, 3
            cve(k) = s(k,isrf)
            cpt(k) = s(k+4,isrf)
          enddo
        else
          if (itype .eq. LINE) then
            px=d(idx+3)
            py=d(idx+4)
            pz=d(idx+5)
            vx=d(idx+6)
            vy=d(idx+7)
            vz=d(idx+8)
          else if (itype.eq.CURVE) then
            call wlcvpv(px,py,pz,vx,vy,vz,iclsd)
          else if (itype.eq.CIRCLE) then
             call circpv(px,py,pz,vx,vy,vz)
          endif
          if (ifl(2).gt.0) goto 999

          cpt(1) = px
          cpt(2) = py
          cpt(3) = pz

          ti = ta(1)
          tj = ta(2)
          tk = ta(3)

          rx = vy*tk-vz*tj
          ry = vz*ti-vx*tk
          rz = vx*tj-vy*ti
          sec = sqrt(rx**2+ry**2+rz**2)
          if (sec .lt. 0.0001) then
            ier = 128
            goto 999
          endif
          if (kinit .eq. 0) sec = sec*side
          cve(1) = rx/sec
          cve(2) = ry/sec
          cve(3) = rz/sec
        endif

        u = t(ihx-2,ia)
        v = t(ihx-1,ia)
        h = t(ihx,ia)

        if (kinit .gt. 0) then
          teh(1) = te(1) + ta(1)*h
          teh(2) = te(2) + ta(2)*h
          teh(3) = te(3) + ta(3)*h
          rx = cpt(1) - teh(1)
          ry = cpt(2) - teh(2)
          rz = cpt(3) - teh(3)
          co = rx*cve(1) + ry*cve(2) + rz*cve(3)
          if (co .lt. 0) then
            if (itype .eq. SURF) then
              ad(jdx+2) = -1
            else
              side = -1
            endif
            cve(1) = -cve(1)
            cve(2) = -cve(2)
            cve(3) = -cve(3)
          endif
        endif

        cuv(1) = u
        cuv(2) = v
        cuv(3) = h

999   if (ifl(2) .gt. 0) ier = ifl(2)
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmevsf (u,v,sn,ier)
C*       FMILL Part Surface evaluator interface.
C*    PARAMETERS
C*       INPUT  :
C*          u,v    - surface parameters
C*       OUTPUT :
C*          sn     surface normal
C*          ier    error
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine fmevsf (u,v,sn,ier)

      include 'com.com'
      include 'fmill.com'

      real*8 u,v,sn(3)
      integer*2 ier

      real*8 buf(9)
      integer*2 isf/4/

      call uevsft(u,v,isf,buf,ier)
      call f_cross(buf(4),buf(7),sn)
      call unitizevc(sn)
      call vctmsc(sn,sn,tadir)

      if (ifl(2).gt.0 .and. ier.eq.0) ier = ifl(2)

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmprj1 (pte,vta,u0,v0,sn,pcon,ier)
C*       Project the tool to the Part Surface. Adjust tlaxis if needed.
C*    PARAMETERS
C*       INPUT  :
C*          pte   - tool end
C*          pte   - tool axis
C*          u0,v0 - surface parameters
C*       OUTPUT :
C*          sn     surface plane normal
C*          pcon   surface plane distance
C*          u0,v0  surface parameters
C*          ier    error
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine fmprj1 (pte,vta,u0,v0,sn,pcon,ier)

      include 'com.com'
      include 'mocom.com'
      include 'fmill.com'

      real*4 pte(3),vta(3),pcon
      real*8 u0,v0,sn(3)
      integer*2 ier

c          motion common equivalences
      real*4 ad(400)
      integer*2 kd(800)
      equivalence (d,ad,kd)
      real*4 asc(100),sbe
      equivalence (sc,asc),(asc(63),sbe)
      equivalence (sc(167),ptol)
      integer*2 isrf
      equivalence (ifl(54),isrf)

      integer*2 idx,jdx
      integer*2 isf/4/

      real*8 pt8(3),vt8(3)
      real*4 xte, yte, zte, xta, yta, zta, crad, h
      real*4 pco,psi,u,v

      real*4 usec,vsec,dir,ss(10)
      integer*2 isfsec /3/
      integer*2 k
      logical lpsn /.false./


      lpsn = (ifl(23).eq.TA_NORMAL_PS .and. .not.lpssec)

        if (lpssec) then
          usec = 0.5
          vsec = 0.5
          do k=1,3
            pt8(k) = pte(k)
            vt8(k) = vta(k)
          enddo

          call sfini1 (sc(195),isfsec,usec,vsec,pt8,vt8)
          dir = 1
          call sfpt(sc(195),pt8,isfsec,dir,usec,vsec,ss)
          do k=1,3
            vta(k) = ss(k)
          enddo
        endif

      crad = sc(29)
      isrf = isf
      idx=(isrf-1)*50
      jdx=idx*2

      u = u0
      v = v0

      ad(jdx+2) = tadir
      xte = pte(1)
      yte = pte(2)
      zte = pte(3)
      xta = vta(1)
      yta = vta(2)
      zta = vta(3)

      if (lpsn) then
        h = 0
      else
        h = crad
      endif

      s(8,isrf) = xte+xta*h
      s(9,isrf) = yte+yta*h
      s(10,isrf) = zte+zta*h

      call surfpn(u,v,1)
      if (ifl(2).gt.0) goto 800

      u0 = u
      v0 = v

      sn(1) = s(1,isrf)
      sn(2) = s(2,isrf)
      sn(3) = s(3,isrf)

      if (lpsn) then
        vta(1) = s(1,isrf)
        vta(2) = s(2,isrf)
        vta(3) = s(3,isrf)
      endif

      pco = xta*s(1,isrf) + yta*s(2,isrf) + zta*s(3,isrf)
      if (pco .lt. 0.) then
        ifl(2) = 5
        goto 800
      endif
      if (pco .ge. 1.) then
        pco = 1.
        psi = 0.
      else
        psi = sqrt(1.-pco**2)
      endif

      pcon = s(4,isrf) + sc(23)
      pcon = pcon+tool(2)*(1.-pco)+tool(6)*psi

800   continue

      if (ifl(2).gt.0 .and. ier.eq.0) ier = ifl(2)

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmprj2 (pte,u,v,isf,linit,snew,
C*                                          lgoug,kerr)
C*       Check if the tool gouges the surface.
C*    PARAMETERS
C*       INPUT  :
C*          pte    - tool position: te + ta
C*          u,v    - starting surface parameters
C*          isf    - surface number
C*          linit  - if 0, set the ad2 number
C*       OUTPUT :
C*          snew   - the gouged plane, or current plane if not gouged
C*          lgoug  - the gouging flag
C*          kerr   - error if surfpn failed
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine fmprj2 (pte,u,v,isf,linit,snew,lgoug,kerr)

      include 'com8a.com'
      include 'mocom.com'
      include 'fmill.com'

      real*8 pte(6),snew(10)
      integer*2 isf,linit,kerr,lgoug

      real*4 u,v
      integer*2 idx,jdx

      integer*2 isrf
      equivalence (ifl(54),isrf)

      real*8 pcon, pco, psi, dst, ptol
      real*8 xte, yte, zte, xta, yta, zta
      real*8 f_dot

c          motion common equivalences
      real*4 ad(400)
      integer*2 kd(800)
      equivalence (d,ad,kd)
      real*4 asc(100), sbe
      equivalence (sc,asc),(asc(63),sbe)
      integer*2 kmax
      parameter (kmax=200)
      equivalence (sc(167),ptol)

      isrf = isf
      idx=(isrf-1)*50
      jdx=idx*2
      kerr = 0

      if (linit .eq. 0) then
        ad(jdx+2) = tadir
        linit = 1
      endif

      xte = pte(1)
      yte = pte(2)
      zte = pte(3)
      xta = pte(4)
      yta = pte(5)
      zta = pte(6)

      s(8,isrf) = xte+xta*tool(2)
      s(9,isrf) = yte+yta*tool(2)
      s(10,isrf) = zte+zta*tool(2)
      call surfpn(u,v,1)

      if (ifl(2).gt.0) goto 9138

      pco = xta*s(1,isrf) + yta*s(2,isrf) + zta*s(3,isrf)
c
c...  Calculate distance down tool axis to plane on surface returned from
c...  psrel in s(,1) array
c
      if (pco.gt.1.) pco = 1.

      psi = 1.-pco**2
      if (psi .gt. 0) then
        psi = dsqrt(psi)
      else
        psi = 0.
      endif

      pcon = s(4,isrf)+tool(2)*(1.-pco)+tool(6)*psi
      if (pco.lt..2) pco = .2
      dst = (xte*s(1,isrf)+yte*s(2,isrf)+zte*s(3,isrf)-pcon)/pco

      if (dst .ge. -ptol) then
c
c..... if not gouging, return the current plane
c
        do i = 1,3
          snew(i) = pte(i+3)
          snew(i+4) = pte(i)
        enddo
        snew(4) = f_dot (pte,pte(4))

        do i = 7,10
          snew(i) = s(i,isrf)
        enddo
        goto 999
      endif

      lgoug = lgoug + 1
c
c..... return the gouged plane
c
      do i = 1,10
        snew(i) = s(i,isrf)
      enddo

      goto 999

9138  ifl(2) = 0
      kerr = 1

999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmprj3 (sprev,snew,pte,vfw,tol)
C*       Interpolate between two planes and move the tool to a
C*       non-gouging position on a new plane.
C*    PARAMETERS
C*       INPUT  :
C*          pte         - tool position: te + ta
C*          sprev,snew  - surface planes, one of them or both gouged
C*          vfw         - forward direction
C*          tol         - tolerance
C*       OUTPUT :
C*          pte    - new tool position
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine fmprj3 (sprev,snew,pte,vfw,tol)

      include 'com8a.com'
      include 'mocom.com'

      real*8 pte(6),vfw(3),snew(10),sprev(10)
      real*4 ta(3),te(3),fwd(3),tfwd(3),sint(10),p
      real*4 f_mag4,f_dot4

      integer*2 NTOOL
      parameter (NTOOL=50)
      real*8 tool_struct(NTOOL)
      real*4 EPS1/1.e-04/

      do i = 1,3
        te(i) = pte(i)
        ta(i) = pte(i+3)
        fwd(i) = vfw(i)
      enddo

      if (f_mag4(fwd).ge.EPS1) call unitizevc4 (fwd)
      call triple_cross4 (ta,fwd,ta,tfwd)
      if (f_mag4(tfwd).ge.EPS1) call unitizevc4 (tfwd)

      call ncl_create_tool (tool_struct,te,ta,fwd,tfwd)

      call ncl_inter_pln (tool_struct,sprev,snew,sint,tol)

      do i = 1,3
        pte(i+3) = sint(i)
      enddo
c
c..... put the tool on the new plane, mover-style
c
      p = f_dot4(te,sint) - sint(4)

      do i = 1,3
        pte(i) = te(i) - p*sint(i)
      enddo

999   return
      end


C*********************************************************************
C*    E_SUBROUTINE     : subroutine seticsf(icsf)
C*********************************************************************
C
      subroutine seticsf (icsf)
      include 'com8a.com'
      include 'mocom.com'
      include 'fmill.com'
      
      integer*2 icsf
      
      javcsf = icsf
      
      end

C*********************************************************************
C*    E_SUBROUTINE     : fmilpar (fnpas, fhight, ffpstart, fspstart, fstep, frapto,frtrct,fstopfdrt,fthick,favasn2,favfr2,favasn1,favfr1,sc10_2, sc10_3, sc10_4)
C*       Retrieves/saves current interface defined FMILL parameters from/to fmill.com
C*       Fortran common area.  It is used to load the FMILL
C*       statement building form.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          fnpas      number of passes
C*          fhight     scul height
C*          ffpstart     start u value
C*          fspstart     start v value
C*          fstep         step value
C*          frapto        rapto value
C*          frtrct        retract value
C*          fstopfdrt     fedrat after STOP  
C*          fthick        thickness in "avoid" checkbox  
C*          favasn2       distance in "rapto" of boundaries interface screen 
C*          favfr2        fedrat for "rapto" of boundaries interface screen 
C*          favasn1       distance in "retract" of boundaries interface screen 
C*          favfr2        fedrat for "retract" of boundaries interface screen 
C*          sc10_2, sc10_3, sc10_4        values for global parameters isc10(2),isc10(3),isc10(4)


C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine fmilpar (fnpas, fhight, ffpstart, fspstart, fstep, 
     x frapto,frapto_dist,frtrct,frtrct_dist,fstopfdrt,fthick,favasn2,
     X favfr2,favasn1,favfr1,sc10_2, sc10_3, sc10_4)


      include 'com8a.com'
      include 'fmill.com'

        
      integer*4 sc10_2,sc10_3,sc10_4
      integer*4 fnpas
      real*8 fhight, ffpstart, fspstart,frapto, fstep, frtrct, 
     x fstopfdrt, frapto_dist, frtrct_dist,fthick,favasn2,favfr2,
     x favasn1,favfr1
      
      fnpas = 0
      fhight = 0.0
      if (scsav(1) .lt. 0.) then
          fhight = -scsav(1)
      else 
          fnpas = scsav(1)
      endif
      ffpstart  = scsav(2)
      fspstart  = scsav(3)
      fstep = scsav(4)
      frapto  = scsav(5)
      frapto_dist = scsav(6)
      frtrct = scsav(7)
      frtrct_dist = scsav(8)
      fstopfdrt = scsav(9)
      fthick = scsav(10)
      favasn2 = scsav(11)
      favfr2 = scsav(12)
      favasn1 = scsav(13)
      favfr1 = scsav(14)
      sc10_3 = isc10(3)
      sc10_2 = isc10(2)
      sc10_4 = isc10(4)

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmilsav ()
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine fmillsav (fnpas, ffpstart, fspstart, fstep, 
     x frapto,frapto_dist,frtrct,frtrct_dist,fstopfdrt,fthick,favasn2,
     X favfr2,favasn1,favfr1,sc10_3,sc10_2,sc10_4)

      include 'com8a.com'
      include 'fmill.com'
      
      integer*4 sc10_3,sc10_2,sc10_4

      real*8 fnpas,fhight, ffpstart, fspstart,frapto, fstep, frtrct, 
     x fstopfdrt, frapto_dist, frtrct_dist,fthick,favasn2,favfr2,
     x favasn1,favfr1 
      
      
      if (fnpas .ge. 1.) then
          scsav(1) = fnpas
      else
          fhight = fnpas
          scsav(1) = fhight
      endif
      scsav(2) = ffpstart 
      scsav(3) = fspstart  
      scsav(4) = fstep 
      scsav(5) = frapto  
      scsav(6) = frapto_dist 
      scsav(7) = frtrct 
      scsav(8) = frtrct_dist 
      scsav(9) = fstopfdrt 
      scsav(10) = fthick 
      scsav(11) = favasn2 
      scsav(12) = favfr2 
      scsav(13) = favasn1 
      scsav(14) = favfr1 
      
      isc10(3) = sc10_3
      isc10(2) = sc10_2 
      isc10(4) = sc10_4 
      
      return
      end



