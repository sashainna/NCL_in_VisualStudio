C**********************************************************************
C*    NAME         :  smill.f
C*       CONTAINS:
C*                 smill
C*                 smout
C*    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*      smill.f , 26.2
C*     DATE AND TIME OF LAST MODIFICATION
C*      04/12/18 , 10:19:11
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmill
C*       Execute SMILL motion.
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
      subroutine smill

      include 'com8a.com'
      include 'mocom.com'
      include 'smill.com'

      integer*2 navpsf1,ityp12,iavpsf,iavpsf1,nsf,nsf1
      real*8 avpsf1(max_avpsf)

      real*8 apsw,tol,ufct
      real*8 pte(6),ptl(9),tdat(640),buf(9)
      integer*4 i,j,npts,nclkey,ksf,iflag,npas
      integer*2 lnk,iret,irapt,inewfr,ibndr,iclass,isubcl,jptk
      integer*2 ntk,nwds,numitm
      equivalence(ntk,ifl(79))
      integer*2 izro /0/
      integer*2 rdis,rpln,rsrf,laynum
      parameter (rdis=1,rpln=2,rsrf=3)
      
      character*80 dbuf
      byte dout(80)
      
c
c... Initialize
c
      laynum = isc10(3)
      do j=1,6
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
c.......laynum gets set to -1 so layer 0 can be used if the SMILL command
c.......has a layer given instead of surfaces - Andrew 11/28/12
c
      nsf = 0
      if (isc10(3).eq. 0 .and. navpsf .gt. 0) then
        laynum = -1
        navpsf1 = navpsf
        do iavpsf = 1,navpsf
          avpsf1(iavpsf) = avpsf(iavpsf)
        enddo
        
        iavpsf = 1
        nsf = 0
        do iavpsf1 = 1,navpsf1
            call gtdesc(avpsf1(iavpsf1),nclkey,nwds,ietype)
            call sftype (nclkey,ityp12)
            if (ityp12 .eq. NETSF) then
                ksf = 0
                iavpsf2 = 1
                call ntsf1 (nclkey,ksf,iavpsf2,nsf1)
                call ptdesc (ksf,SURF,apsw)
                avpsf(iavpsf) = apsw
                do isf = 2, nsf1
                    ksf = 0
                    call ntsf1 (nclkey,ksf,isf,nsf1)
                    call ptdesc (ksf,SURF,apsw)
                    avpsf(isf+iavpsf-1) = apsw  
                enddo
                nsf = nsf + nsf1
                iavpsf = iavpsf + nsf1
            else
                avpsf(iavpsf) = avpsf1(iavpsf1)               
                nsf = nsf + 1
                iavpsf = iavpsf + 1
            endif
        enddo
      endif
      
c
c... Call ncl_smill to create motion.
c
      call ncl_smill(laynum,nsf,avpsf,navend,navd,avdpl,navbnd,
     x               avbnd,navstep,sc(12),sc(13),sc(14),navtype,ifl(2))
      if (ifl(2) .gt. 0) go to 9990
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
      if (navtype .eq. 2) then
        iflag = 0
      else
        iflag = 1
      endif
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
c.....Lace Retract
c 
        if (navtype .eq. 2 .and. navlrpto .gt. 0 .and. i .lt. npas) then
             if (avlfr.lt.0.0d0) then
                call ppwrt(5)
            else if (avlfr.gt.0.0d0) then
                call fedmut(avlfr)
            endif
            iclass = 5000
            isubcl = 5
            numitm = 1
            kerr   = 0

            if (navlrpto.eq.rsrf) then
                call gtdesc(avlasn,nclkey,nwds,ietype)
                call ncl_get_sf_primtyp(nclkey,ietype)
                if (ietype .eq. 3) then
                    call primpln (nclkey,izro,pl)
                    call plnint (ptl,ptl(4),pl,ptl,kerr)
                else
                    call fmprj (ptl,ptl,avlasn,kerr)
                endif
            else if (navlrpto.eq.rpln) then
                call gtentt(avlasn,trflg,nclkey,ietype,pl)
                call plnint (ptl,ptl(4),pl,ptl,kerr)
            else
                call uvcplvc(ptl,ptl(4),ptl,avlasn)
            endif

            if (kerr.eq.0) then
                numitm = 1
                call conv8_8 (ptl,buf,9)
                call putcl (iclass,isubcl,numitm,buf)
                if (ifl(154).eq.1) call tdsply(lnk,buf,jptk)
            endif     
        endif
c
c.....Last Retract
c         
        if (sc(19).gt.0 .and. (navtype .eq. 2 .and. i .eq. npas) .or.
     x      (navtype .ne. 2)) then
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
c... Call ncl_smfin to finish SMILL
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

C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmlerr
C*       Error exit: call fmfin and set error number.
C*********************************************************************
C
      subroutine smlerr (ierr)

      include 'com8a.com'
      integer*2 ierr

        call ncl_smfin
        ifl(2) = ierr

      return
      end

C*********************************************************************
C*    E_SUBROUTINE : subroutine smout(tdat,tb,lnk,jptk,iclass,isubcl)
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
      subroutine smout (tdat, tb, lnk, jptk, iclass, isubcl)

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
c...Output tdat array to the cl file if record is full
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
C*    E_SUBROUTINE     : smillpar (pahismill, stessmill,rptsmill,rtrsmill, sc10_3,dpesmill,fresmill,tolsmill, fdrrptsmill, fdrrtrsmill)
C*       Retrieves/saves current interface defined FMILL parameters from/to smill.com
C*       Fortran common area.  It is used to load the SMILL
C*       statement building form.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          pahismill         height/number of passes
C*          stessmill         stepsize
C*          rptsmill          rapto dist in Entry/Exit screen
C*          rtrsmill          retact dist in Entry/Exit screen
C*          sc10_3            number of layers
C*          dpesmill          distance for retract value after pass end
C*          fresmill          fedrat for retract value after pass end
C*          tolsmill          tolerance  
C*          fdrrptsmill       rapto fedrat  in Entry/Exit screen
C*          fdrrtrsmill       retract fedrat  in Entry/Exit screen


C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine smillpar (pahismill, stessmill,rptsmill,rtrsmill,
     x sc10_3,dpesmill,fresmill,tolsmill, fdrrptsmill, fdrrtrsmill )


      include 'com8a.com'
      include 'smill.com'

        
      integer*4 sc10_3
      real*8 pahismill, stessmill,rptsmill,rtrsmill,dpesmill,fresmill,
     x tolsmill, fdrrptsmill, fdrrtrsmill     
      
      pahismill = avpsf(212)
      stessmill = avpsf(213)
      rptsmill = avpsf(217)
      rtrsmill = avpsf(220)
      sc10_3 = avpsf(230)
      dpesmill = avpsf(231)
      fresmill = avpsf(232)
      tolsmill = avpsf(227)
      fdrrptsmill = avpsf(218)
      fdrrtrsmill = avpsf(221)

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine smillsav ()
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine smillsav (pahismill, stessmill,rptsmill,rtrsmill,
     x sc10_3,dpesmill,fresmill,tolsmill, fdrrptsmill, fdrrtrsmill )

      include 'com8a.com'
      include 'smill.com'
      
      integer*4 sc10_3
      real*8 pahismill, stessmill,rptsmill,rtrsmill,dpesmill,fresmill,
     x tolsmill, fdrrptsmill, fdrrtrsmill     
      
      avpsf(212)=pahismill
      avpsf(213)=stessmill
      avpsf(217)=rptsmill
      avpsf(220)=rtrsmill
      avpsf(230)=sc10_3
      avpsf(231)=dpesmill
      avpsf(232)=fresmill
      avpsf(227)=tolsmill
      avpsf(218)=fdrrptsmill
      avpsf(221)=fdrrtrsmill
      
      return
      end

