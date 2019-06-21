C**********************************************************************
C*    NAME         :  pmill.f
C*       CONTAINS:
C*                 pmill
C*    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*      pmill.f , 24.1
C*     DATE AND TIME OF LAST MODIFICATION
C*      09/11/13 , 13:04:55
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pmill
C*       Execute PMILL motion.
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
      subroutine pmill

      include 'com8a.com'
      include 'mocom.com'
      include 'smill.com'
      include 'pmill.com'

      integer*2 pavpsf1,ityp12,iappsf,iappsf1,nsf,nsf1
      real*8 appsf1(max_appsf)

      real*8 apsw,tol,ufct
c      real*8 pte(6),ptl(9),tdat(420),buf(9)
      real*8 pte(24),ptl(24),tdat(420),buf(9)
      integer*4 i,j,npts,nclkey,ksf,iflag,npas
      integer*2 lnk,iret,irapt,inewfr,ibndr,iclass,isubcl,jptk
      integer*2 ntk,nwds,numitm
      equivalence(ntk,ifl(79))
      integer*2 izro /0/
      integer*2 rdis,rpln,rsrf,laynum
      parameter (rdis=1,rpln=2,rsrf=3)
      
      real*8 sc23
      character*80 dbuf
      byte dout(80)
      
c
c... Initialize
c
      if (ifl(23).eq.13) then
        call nclf_pmill_thrupt(tool(18),tool(19),tool(20))
      else
        call nclf_pmill_reset_thrupt
      endif
      laynum = isc10(3)
      do j=1,24
        pte(j) = 0.0d0
      enddo

      do j=1,3
        ifl(330+j) = 0
      enddo

      ufct = 1.0d0
      if (ifl(264).eq.1) ufct = 25.4d0
c
c...Convert all parameters to inches
c
      tol = sc(27)/ufct
c
c...Pass number,scallop height, step distance.
c
c     sc(12) = pass number or scallop height
c     sc(13) = stepover distance

c
c....Surface numbers and Surface keys
c.......laynum gets set to -1 so layer 0 can be used if the PMILL command
c.......has a layer given instead of surfaces - Andrew 11/28/12
c
      nsf = 0
      if (isc10(3).eq. 0 .and. pavpsf .gt. 0) then
        laynum = -1
        pavpsf1 = pavpsf
        do iappsf = 1,pavpsf
          appsf1(iappsf) = appsf(iappsf)
        enddo
        
        iappsf = 1
        nsf = 0
        do iappsf1 = 1,pavpsf1
            call gtdesc(appsf1(iappsf1),nclkey,nwds,ietype)
            call sftype (nclkey,ityp12)
            if (ityp12 .eq. NETSF) then
                ksf = 0
                iappsf2 = 1
                call ntsf1 (nclkey,ksf,iappsf2,nsf1)
                call ptdesc (ksf,SURF,apsw)
                appsf(iappsf) = apsw
                do isf = 2, nsf1
                    ksf = 0
                    call ntsf1 (nclkey,ksf,isf,nsf1)
                    call ptdesc (ksf,SURF,apsw)
                    appsf(isf+iappsf-1) = apsw  
                enddo
                nsf = nsf + nsf1
                iappsf = iappsf + nsf1
            else
                appsf(iappsf) = appsf1(iappsf1)               
                nsf = nsf + 1
                iappsf = iappsf + 1
            endif
        enddo
      endif
      
c
c... Call ncl_pmill to create motion.
c
      sc23 = sc(23)
      call ncl_pmill(laynum,nsf,appsf,pavend,pavd,apdpl,pavbnd,apbnd,
     x pavstep,sc(12),sc(13),sc(14),pavtype,pavtran,atrad,pavdir,ifl(2))
      
      sc(23) = sc23
      if (ifl(2).eq. 561) goto 9990
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
      iflag = 1
c
c..... Get the numbers of passes
c      
      call ncl_sm_clpath_getnpath(iflag,npas)
      do i = 1,npas
c     
c.....Get the number of cl points in the ith-pass
c
        call ncl_sm_clpath_getnpts(iflag,i,npts)  
     
        do j = 1,npts
            call ncl_sm_clpath_getpt(iflag,i,j,pte)
            if (j.eq.npts) call vctovc(pte,ptl)
c
c.....Rapto move
c
            if(j.eq.1 .and. sc(16).gt.0) then
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
                    call gtdesc(sc(17),nclkey,nwds,ietype)
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
                    numitm = 1
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

              if (avrpto.eq.rpln) then
                call plnint (pte,pte(4),avpl2,ptl,kerr)
              else
                call uvcplvc(pte,pte(4),ptl,avasn2)
              endif
              if (kerr.eq.0) then
                numitm = 1
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
                call smout (tdat, pte, lnk, jptk, iclass, isubcl)
            endif
c
c..... do j = 1,npts  
c           
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
c
c.....Last Retract
c         
        if (sc(19).gt.0 .and. (pavtype .eq. 2 .and. i .eq. npas) .or.
     x      (pavtype .ne. 2)) then
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
                call gtdesc(sc(20),nclkey,nwds,ietype)
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
                numitm = 1
                call conv8_8 (ptl,buf,9)
                call putcl (iclass,isubcl,numitm,buf)
                if (ifl(154).eq.1) call tdsply(lnk,buf,jptk)
            endif
        endif
c
c..... end do i = 1,npas  
c
      enddo
   
      call vctovc(ptl,sc)
      call vctovc(ptl(4),sc(4))
      call unitvc(ptl(7),sc(7))
      if (ifl(154).eq.0) call tdsply(lnk,ptl,jptk)
c
c... Call ncl_smfin to finish PMILL
c
      call ncl_smfin

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

