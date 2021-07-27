C*********************************************************************
C*    NAME         :  mosyn2.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*      mosyn2.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*      04/29/15 , 15:10:17
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine mosyn2
c*      this routine handles the parsing and syntax checking for
c*      the pocket and scrub statements.  it builds the syscom in
c*      preparation for the motion generation section in the following
c*      format:
c*                      738 = pocket  sc(11) thru sc(18) are 8 scalars
c*                                    3 thru 20 points are in next
c*                                    available ranfil record
c*                                    isc10(1) = ist of POCKET vocabulary
c*                                               word (738)
c*                                    isc10(2) = 1 : old point POCKET
c*                                             = 2 : Advanced POCKET
c*                                                   with a PLANE as 
c*                                                   defining the top
c*                                                   PLANE
c*                                             = 3 : Advanced POCKET
c*                                                   with a distance as
c*                                                   defining the top PLANE
c*                                             = 4 : Waterline Roughing
c*                                    isc10(3) = number of periphery and
c*                                               island entities for 
c*                                               Advanced POCKET
c*                                             = number of periphery 
c*                                               POINTs for old point
c*                                               POCKET
c*                                    isc10(4) = 0 : no END point segment
c*                                                   number or POINT ID
c*                                                   was given.
c*                                             = 1 : a segment number was
c*                                                   given for the END
c*                                                   parameter
c*                                             = 2 : a POINT ID was given 
c*                                                   for the END parameter
c*
c*                      739 = scrub   sc(11) = surf or plane
c*                                    sc(12) = number of passes
c*                                    sc(13) = number of points each pass
c*                                    if isc10(2)=2 then sc(14) thru sc(17)
c*                                    hold the points.
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
      subroutine mosyn2

      include 'com8a.com'
      include 'vocab.com'
      include 'rrdm.com'
      include 'fmill.com'
      include 'smill.com'
      include 'vmpcom.com'
c      include 'com8a.com'
      include 'prfcom.com'
      include 'comgt.com'
      include 'pmill.com'

      integer*2 ktv(4), iside, tgeotp, tityp, numscl, npts
      integer*4 jtv(2), ind1, ind2
      equivalence (tv,jtv,ktv)
      integer*2 pg,el,nwds,tinx
      equivalence (pg,ktv(1)),(nwds,ktv(3)),(el,ktv(2))
      real*8 jbuff(35)
      real*8 fct,tthk
      integer*4 nclkey,i4
      integer*2 i,j,iprv
      real*8 r8,asn,pt(3),hgt
      real*4 r4(2)
      equivalence (r4,r8)
      integer*2 ksn(4)
      integer*4 jsn(2)
      equivalence (asn,jsn,ksn)

      integer*2 POCKET
      parameter (POCKET = 22)
      integer*2 POCKV
      parameter (POCKV = 738)
      integer*2 VMPOCK
      parameter (VMPOCK = 765)
      integer*2 VMP3AX
      parameter (VMP3AX = 962)
      integer*2 VMP5AX
      parameter (VMP5AX = 1022)
      integer*2 VCS
      parameter (VCS = 753)
      integer*2 ADJUST
      parameter (ADJUST = 159)
      integer*2 STEPV
      parameter (STEPV = 92)
      integer*2 UPV
      parameter (UPV = 112)
      integer*2 PMILLV
      parameter (PMILLV = 1051)
      integer*2 LINEAR
      parameter (LINEAR = 76)
      integer*2 VPOSITN
      parameter (VPOSITN = 1072)
      integer*2 rdis,rpln,rsrf
      parameter (rdis=1,rpln=2,rsrf=3)
      logical lboth,watrev

      if (nextyp.ne.5) then
        call error(22)
        goto 99999
      endif

      isc10(1)=ist
      isc10(2)=1
      ifl(44)=9
      call parsit
      if (isc10(1).eq.VPOCK) then
c
c***************** pocket *****************
c
         if (ityp.eq.2 .and. (geotyp .eq. POCKET)) then
c
c***************** named pocket *****************
c
           if (.not.nxteos) then     
              call error(4)
              goto 99999
           endif
           call gtdesc (tv, nclkey, nwds, ietype)
           call dopock (nclkey)
           isc10(1) = 0
         else if (geom .and. (geotyp.eq.plane .or.
     x            geotyp.eq.surf)) then
c        
c***************** advanced pocket *****************
c
c..... If the first token is a PLANE or SURFACE, it must be
c..... the Advanced POCKET bottom plane/surface token.
c..... Load its key in sc(11).
c
           numscl = 0
           tamode = 0
           sc(19) = 0
           isc10(2) = 2
           sc(11)=tv
           pclik = 1
           psthk = sc(23)
           dsthk = sc(24)
c
c..... parse the top plane token and load its key in sc(12).
c
           call parsit
           if (vocab .and. (voc.eq.SAME .or. voc.eq.NORMAL)) then
             if (voc.eq.NORMAL) then
             tamode = 1
             call parsit
             if (vocab .and. voc.eq.VPS) then
               tamode = 2
               call parsit
               if (geom .and. (geotyp.eq.PLANE.or.geotyp.eq.SURF)) then
                 sc(19) = tv
                 call parsit
               else
                 call error(326)
                 goto 99999
               endif
             endif
             endif
           endif

           ttyp = 0
           tdis = 0
           if (.not.((geom .and. (geotyp.eq.PLANE.or.geotyp.eq.SURF))
     x           .or. scalar)) then 
             call error(19)
             goto 99999
           endif
           if (geom.and.geotyp.eq.SURF) then
             call gtdesc(tv,nclkey,i,ietype)
             call ncl_get_sf_primtyp(nclkey,ietype)
             if (ietype.ne.3) then
               call error(19)
               goto 99999
             endif
           endif
           if (geom) then
             ttyp = 1
           else
             tdis = tv
           endif
           sc(12)=tv
           if (scalar) isc10(2) = 3
           isc10(4)=0
c
c..... parse the periphery and island geometry names
c
           iside = 1
           nloops = 9999
           finthk = 0.
           tthk = 0.
           call nclf_genpocket_offthk(tthk)
           offprt = 0
           openfl = 0
           dirp = 0
           diri = 1
           etyp = 0
           eelm = 0
           ltyp = 0
           lops = 9999
           call frpkys
           i = 1
130        call parsit

           if (vocab) then 
               if (voc .eq. on) then
                 iside = 0
                 dirp = 2
               else if (voc .eq. out) then
                 iside = -1
                 nloops = 1
                 dirp = 1
               else if (voc .eq. offset) then
                 dirp = 3
                 openfl = 1
c                 tinx = inx
c                 call parsit
c                 if (scalar) then
c                   offthk = tv
c                   call nclf_genpocket_offthk(tv)
c                 else
c                   tv = 0.
c                   call nclf_genpocket_offthk(tv)
c                   inx = tinx
c                 endif
               else if (voc .eq. VOCEND) then
                 call error(177)
                 goto 99999
               endif
               goto 130
           endif

           if (.not. (ityp.eq.2 .and. (geotyp .eq. 14 .or.
     x                              geotyp .eq. curve .or.
     x                              geotyp .eq. circle .or.
     x                              geotyp .eq. surf .or.
     x                              geotyp .eq. patern))) then
               call error(177)
               goto 99999
           endif
c
c..... surface
c
           if (geom .and. geotyp .eq. surf) then
c
c.....Set a flag so stpky1 does nothing but set parameters
c
               call gtdesc (tv, nclkey, nwds, ietype)
               call stpky1 (nclkey,jtv(1),ktv(4),sc(27),dirp,ifl(2))
               if (ifl(2) .gt. 0) goto 99999
               if (jtv(1) .eq. 0) ktv(4) = 21
           endif
c
c..... Load the OUT/ON/IN value in the third I*2 of the ASW
c
           ktv(3) = iside
c
c..... Load the ASW & token into the data area
c
           rsvasw(1) = tv
           rsvtok(1) = token2
           rsvsub(1) = ivxsub
           if (nextyp.eq.11) then
               isc10(3) = 1
               if (lctyp .gt. 0) goto 215
               goto 99999
           endif
c
c.....Check for open sides if points or curve given
c.......Currently only support composites and points
c
           tityp = ityp
           tgeotp = geotyp
           npts = ktv(1)
c
c... ensure consistency of default direction mod. IN/OUT in loop
c
140        iside = 1
c
c..... Eduard 3/25/1999. Included the "thru" option (for entering
c..... multiple islands) in the advanced pocket statement.
c..... An example of use:
c.....                    pocket/pl1,0.5,cv1,ci1,thru,ci100
c
           do 200 i=2,120
  150        idtype = 0
             call parsit
             if (ityp.eq.7) goto 210
c               
c..... The periphery and island geometry must be:
c..... Subscripted point id = 14 
c..... Curve   
c..... Circle  
c..... Patern 
c
             if (vocab) then 
               if (voc .eq. in) then
                 iside = 1
                 if (i .eq. 2) diri = 0
               else if (voc .eq. on) then
                 iside = 0
                 if (i .eq. 2) diri = 2
               else if (voc .eq. out) then
                 iside = -1
               else if (voc .eq. offset) then
                 iside = -1
                 if (i .eq. 2) diri = 3
               else if (voc.eq.VOCEND .or. voc.eq.FINISH .or.
     x                  (lctyp.gt.0 .and. voc.eq.START) .or.
     x                  voc.eq.OFF .or. voc.eq.VPS .or. voc.eq.VDS .or.
     x                  voc.eq.VOPEN) then
                 goto 210
               endif
c
c... IN/OUT is viewed from the CUT side for islands - not centroid
c
               iside = -iside
               goto 150
             endif
c
c..... if this is not a geometry token, go test for 
c..... number of loops info.
c
             if (.not. (ityp.eq.2 .and. (geotyp .eq. 14 .or.
     x                              geotyp .eq. surf .or.
     x                              geotyp .eq. curve .or.
     x                              geotyp .eq. circle .or.
     x                              geotyp .eq. patern))) goto 210

c
c..... surface
c
           if (geom .and. geotyp .eq. surf) then
               call gtdesc (tv, nclkey, nwds, ietype)
               call stpkys (nclkey,nwds,sc(27),ifl(2))
               if (ifl(2) .gt. 0) goto 99999
               isc10(3) = nwds
               do j = 2, nwds
                 if (diri.ne.3) then
                   call gtpkys (j,jtv(1),ktv(4),ifl(2))
                   if (ifl(2) .gt. 0) goto 99999
                   if (jtv(1) .eq. 0) ktv(4) = 21
                 endif
                 ktv(3) = iside
                 rsvasw(j) = tv
                 rsvtok(j) = token2
                 rsvsub(j) = ivxsub
               enddo
               if (nextyp.eq.11) then
                 if (lctyp .gt. 0) goto 215
                 goto 99999
               endif
               call parsit
               goto 215
           endif
c
c..... Load the OUT/ON/IN value in the third I*2 of the ASW
c
             ktv(3) = iside
c
c..... Load the ASW & token into the data area
c
             rsvasw(i) = tv
             rsvtok(i) = token2
             rsvsub(i) = ivxsub
             if (nextyp.eq.11) then
               isc10(3) = i
               if (lctyp .gt. 0) goto 215
               goto 99999
             endif
c
c... ensure consistency of default direction mod. IN/OUT in loop
c
             iside = 1
200        continue

210        isc10(3) = i-1
c
c..... separate ending for Lace Pocketing
c
215        continue
           if (lctyp .gt. 0 .and. vocab .and. voc.eq.START) then
             sc(14) = 0
             sttyp = 0
             if (.not.nxteos) then
               call parsit
               if (geom .and.(geotyp.eq.POINT.or.geotyp.eq.PNTVEC)) then
                 sc(14) = tv
                 sttyp = 1
               else
                 call error(20)
                 goto 99999
               endif
             endif
c
c..... Test if token is optional END point indicator or 
c..... number of loops value.
c..... Check if token is the end point geometry name.
c
c215        continue
           else if (vocab .and. voc .eq. VOCEND) then
             call parsit
             if (scalar) then
               isc10(4) = 1
               etyp = 1
               eelm = tv
             else if (geom .and. geotyp .eq. point) then
               isc10(4) = 2
               etyp = 2
             else
               call error(33)
               goto 99999
             endif
c
c..... Load the end point geometry key in sc(13)
c
             sc(13)=tv
             if (nxteos) goto 99999
             call parsit
             goto 215
c
c..... Check for and load optional number of loops parameter
c
           else if (scalar) then
             nloops = tv
             ltyp = 1
             lops = tv
             if (nxteos) goto 99999
             call parsit
             goto 215
c
c..... PS thick
c
           else if (vocab. and. voc.eq.VPS) then
             call parsit
             if (vocab. and. voc.eq.THICK) then
               call parsit
               if (.not.scalar) then
                 call error(7)
                 goto 99999
               endif
               psthk = tv
             endif
             if (nxteos) goto 99999
             call parsit
             goto 215
c
c..... DS thick
c
           else if (vocab. and. voc.eq.VDS) then
             call parsit
             if (vocab. and. voc.eq.THICK) then
               call parsit
               if (.not.scalar) then
                 call error(7)
                 goto 99999
               endif
               dsthk = tv
             endif
             if (nxteos) goto 99999
             call parsit
c
c.....Check for open boundary thick
c
             if (scalar) then
               call nclf_genpocket_offthk(tv)
               call parsit
               if (nxteos) goto 99999
             endif
             goto 215
c
c..... Final pass thick
c
           else if (vocab. and. voc.eq.FINISH) then
             call parsit
             if (vocab. and. voc.eq.THICK) then
               call parsit
               if (.not.scalar) then
                 call error(7)
                 goto 99999
               endif
               finthk = tv
             endif
             if (nxteos) goto 99999
             call parsit
             goto 215
c
c..... enter off part when possible
c
           else if (vocab. and. voc.eq.OFF) then
             call parsit
             if (vocab. and. voc.eq.PART) offprt = 1
             if (nxteos) goto 99999
             call parsit
             if (scalar) then
               offdis = tv
               offthk = offdis
               if (nxteos) goto 99999
               call parsit
             endif
             goto 215
           else if (vocab.and.voc.eq.VOPEN) then
             if (tityp.eq.2.and.(tgeotp.eq.14.or.tgeotp.eq.curve)) then
               tv = rsvasw(1)
               if (tgeotp.eq.curve) then
                 call gtdesc (rsvasw(1), nclkey, nwds, ietype)
                 call cvtype1(nclkey,ietype)
                 if (ietype.ne.5) then
                   call error(12)
                   goto 99999
                 endif
               else
                 nclkey = 0
               endif
               call ncl_genpocket_storegeo(rsvtok(1),nclkey,tgeotp,npts)
c
c.....Set flag for pocket boudnary retrieval later
c
               ktv(4) = 10
               rsvasw(1) = tv
               openfl = 1
c
c.....Get and store open section indices
c
               call parsit
217            if (ityp.eq.1.and.ist.eq.ALL) then
c...                                                                    ALL
                  if (numscl.eq.0) then
                    call ncl_genpocket_setall
                    if (nxteos) goto 99999
                    call parsit
                    goto 215
                  else
                    call error(182)
                    goto 99999
                  endif
               else if (ityp.eq.3.or.ityp.eq.4) then
                 numscl = numscl + 1
                 ind1 = tv
                 ind2 = tv
                 tinx = inx
                 call parser
                 if (ityp.eq.1.and.ist.eq.THRU) then
c...                                                                   THRU
                   call parser
                   if (ityp.eq.3.or.ityp.eq.4) then
                     ind2 = tv
                     call ncl_genpocket_addindex(ind1,ind2)
                     if (nextyp.eq.11) goto 99999
                     tinx = inx
                     call parser
                     if (ityp.eq.1) then
                       inx = tinx
                       call parsit
                       goto 215
                     endif
                     goto 217
                   else
                     call error(7)
                     goto 99999
                   endif
                 else if (ityp.eq.3.or.ityp.eq.4.or.ityp.eq.7.or.
     x                    ityp.eq.1) then
c...                                             SINGLE COMPOSITE COMPONENT
                   if (tgeotp.eq.8) then
                     call ncl_genpocket_addindex(ind1,ind2)
                     if (ityp.eq.7) goto 99999
                     inx = tinx
                     call parser
                     if (ityp.eq.1) then
                       inx = tinx
                       call parsit
                       goto 215
                     endif
                     goto 217
                   else
                     call error(7)
                     goto 99999
                   endif
                 endif
c...                                                   INVALID INDEX SYNTAX
               else if (numscl.eq.0.or.(numscl.eq.1.and.
     x                   tgeotp.eq.14)) then
                 call error(53)
                 goto 99999
               endif
               call parsit
               goto 215
             else
               call error(12)
               goto 99999
             endif
           else if (lctyp .gt. 0) then
             goto 88888
           else
             call error(4)
             goto 99999
           endif
c        
c***************** waterline roughing *****************
c
         else if (vocab .and. voc.eq.LAYER) then
           isc10(2) = 4
           isc10(4) = 2
           nloops = 9999
           offprt = 0
           offdis = 0.
           sc(12) = 0.
           sc(14) = 0.
           sc(15) = 0
           sc(16) = 0.
           sc(17) = 4.*sc(27)
           sc(18) = 0
           tamode = 0
           sc(19) = 0
           call parsit
           if (.not.(ityp.eq.5 .and. ist.eq.1)) then
             call error(6)
             goto 99999
           endif
           call parsit
           if (.not.scalar) then
             call error(7)
             goto 99999
           endif
           isc10(3) = tv
           wlayn = tv
           wclik = 1
           wstkp = 0
           wstock = 0
           wgap = sc(17)
           wfrom = 0
           wztyp = 1
           wzlst = 0
           wdeep = 0
           wltyp = 0
           wlops = 0
           wfrom = 0
           wfinis = 0
           wmeth = 0
           watrev = .false.
           wstep = 0.
           call parsit
c
c......POCKET/VMPOCK
c
           if (vocab .and. voc .eq. POCKV) then
               wmeth = 0
               call parsit
           else if (vocab .and. voc .eq. VMPOCK) then
               wmeth = 1
               call parsit
           else if (vocab .and. voc .eq. VMP3AX) then
               wmeth = 2
               call parsit
           else if (vocab .and. voc .eq. VMP5AX) then
               wmeth = 3
               call parsit    
           endif
c
c..... bottom
c
           asn = 0
           if (vocab .and. (voc.eq.VOCREV.or.voc.eq.VOCPLN)) then
             if (voc .eq. VOCREV) then
c
c..... waterline on a surface of revolution
c
               call parsit
               if (geom .and. geotyp.eq.SURF) then
                 call gtdesc(tv,nclkey,i,ietype)
                 call ncl_get_sf_primtyp(nclkey,ietype)
                 watrev = (ietype.eq.CYLINDER .or. ietype.eq.CONE .or.
     x                     ietype.eq.SPHERE .or. ietype.eq.REVOLV)
               endif
               if (.not.watrev) then
                 call error(480)
                 goto 99999
               endif
               jsn(1) = nclkey
               ksn(3) = 100
             endif
             call parsit
           endif

           ksn(4) = -1
           if (vocab .and. voc.eq.STOCK) then
             ksn(4) = 0
           else if (vocab. and. voc.eq.DEPTH) then
             ksn(4) = 2
           else if (vocab. and. voc.eq.AT) then
             ksn(4) = 3
           else if (.not.watrev .and. geom) then
             ietype = 0
             if (geotyp .eq. SURF) then
               call gtdesc(tv,nclkey,i,ietype)
               call ncl_get_sf_primtyp(nclkey,ietype)
             endif
             if (geotyp .eq. PLANE .or. ietype .eq. PLANAR) then
               asn = tv
               ksn(4) = 1
             endif
           endif
           if (ksn(4) .lt. 0) then
             call error(19)
             goto 99999
           endif

           sc(11) = asn
           wbtyp = ksn(4)
           call parsit
c
c..... bottom parameter
c
           if ((wbtyp.eq.2 .or. wbtyp.eq.3) .and. .not.scalar) then
             call error(7)
             goto 99999
           endif
           if (scalar) then
             sc(12) = tv
             wbotp = tv
             call parsit
           endif
c
c..... top
c
           asn = 0
           ksn(4) = -1
           if (vocab .and. voc.eq.STOCK) then
             ksn(4) = 0
           else if (vocab. and. voc.eq.OFFSET) then
             if (wbtyp .eq. 2) then
               call error(61)
               goto 99999
             endif
             ksn(4) = 2
           else if (vocab. and. voc.eq.AT) then
             ksn(4) = 3
           else if (.not.watrev .and. geom) then
             ietype = 0
             if (geotyp .eq. SURF) then
               call gtdesc(tv,nclkey,i,ietype)
               call ncl_get_sf_primtyp(nclkey,ietype)
             endif
             if (geotyp .eq. PLANE .or. ietype .eq. PLANAR) then
               asn = tv
               ksn(4) = 1
             endif
           endif
           if (ksn(4) .lt. 0) then
             call error(19)
             goto 99999
           endif

           sc(13) = asn
           wttyp = ksn(4)
c
c..... top parameter
c
           if (.not.nxteos) call parsit
           if ((wttyp.eq.2 .or. wttyp.eq.3) .and. .not.scalar) then
             call error(7)
             goto 99999
           endif
           if (scalar) then
             sc(14) = tv
             wtopp = tv
             if (nxteos) goto 99999
             call parsit
           endif
c
c..... FINISH,LEVEL - optional
c
           if (vocab .and. voc.eq.FINISH) then
             call parsit
             if (vocab .and. voc.eq.LEVEL) then
               wfinis = 1
               adjdn = 0
               adjup = 0
               call parsit
c
c..... ADJUST,UP,dup,DOWN,ddn - optional parameters
c
               if (vocab .and. voc.eq.ADJUST) then
                 call parsit
                 if (vocab .and. voc.eq.UP) then
                   call parsit
                   if (.not.scalar) then
                     call error(7)
                     goto 99999
                   endif
                   if (tv .gt. sc(27)) adjup = tv
                   call parsit
                   if (vocab .and. voc.eq.DOWN) then
                     call parsit
                     if (.not.scalar) then
                       call error(7)
                       goto 99999
                     endif
                     if (tv .gt. sc(27)) adjdn = tv
                     call parsit
                   else
                     call error(232)
                     goto 99999
                   endif
                 else
                   call error(232)
                   goto 99999
                 endif
               endif
             endif
           endif
c
c..... stock - optional
c
           if (vocab .and. (voc.eq.IN .or. voc.eq.OMIT .or.
     1         voc.eq.PART .or. voc.eq.FIT .or. voc.eq.BOUND)) then
             if (voc.eq.IN) then
               sc(15) = 3
             else if (voc.eq.PART) then
               sc(15) = 1
             else if (voc.eq.FIT) then
               sc(15) = 2
             else if (voc.eq.BOUND) then
               sc(15) = 4
               wstock = 4
               i = 0
 230           idtype = 9
               if (nxteos) goto 99999
               call parsit
               if (geom .and. geotyp.eq.surf) then
                 call gtdesc(tv,nclkey,nwds,ietype)
                 call addskx(0,nclkey,1,i)
                 goto 230
               else if (geom .and. geotyp.eq.vsolid) then
                 call gtdesc(tv,nclkey,nwds,ietype)
                 call addskx(0,nclkey,2,i)
                 goto 230
               else if (vocab .and. voc.eq.layer) then
                 call parsit
                 if (.not.(ityp.eq.5 .and. ist.eq.1)) then
                   call error(6) ! '=' expected
                   call delskx
                   goto 99999
                 endif
                 call parsit
                 if (.not.scalar) then
                   call error(7) ! scalar expected
                   call delskx
                   goto 99999
                 endif
                 i4 = tv
                 call addskx(1,i4,1,i)
                 goto 230
               else if (i .le. 0) then
                 call error(186) ! surf expected
                 call delskx
                 goto 99999                
               else if (scalar) then
                 sc(16) = tv
                 wstkp = tv
                 if (nxteos) goto 99999
                 call parsit
                 goto 235
               else  
                 goto 235              
               endif
             endif
             
             wstock = sc(15)
             if (nxteos) goto 99999
             call parsit
             if (scalar) then
               sc(16) = tv
               wstkp = tv
               if (nxteos) goto 99999
               call parsit
             endif
           endif
           
  235      continue 
               
           if (vocab .and. voc.eq.AVOID) then
             i = 0
 240         idtype = 9
             if (nxteos) goto 99999
             call parsit
             if (geom .and. geotyp.eq.surf) then
               call gtdesc(tv,nclkey,nwds,ietype)
               call addskw(0,nclkey,1,i)
               goto 240
             else if (geom .and. geotyp.eq.vsolid) then
               call gtdesc(tv,nclkey,nwds,ietype)
               call addskw(0,nclkey,2,i)
               goto 240
             else if (vocab .and. voc.eq.layer) then
               call parsit
               if (.not.(ityp.eq.5 .and. ist.eq.1)) then
                 call error(6) ! '=' expected
                 call delskw
                 goto 99999
               endif
               call parsit
               if (.not.scalar) then
                 call error(7) ! scalar expected
                 call delskw
                 goto 99999
               endif
               i4 = tv
               call addskw(1,i4,1,i)
               goto 240
             else if (i .le. 0) then
               call error(358) ! value too small
               call delskw
               goto 99999
             endif
           endif  
c
c..... max gap - optional
c            
           if (vocab .and. voc.eq.WERROR) then
             call parsit
             if (vocab .and. voc.eq.VTOLER) then
               sc(17) = sc(27)
             else if (scalar) then
               sc(17) = tv
             else
               call error(7)
               goto 99999
             endif
             wgap = sc(17)
             if (nxteos) goto 99999           
           else if (vocab .and. voc.eq.OFF) then
             goto 255
           endif          

c
c..... continue parsing
c
250        call parsit

255        continue 

c
c..... START,... - optional
c
           if (vocab .and. voc.eq.START) then
             call parsit
             if (.not.vocab) then
               call error(232)
               goto 99999
             endif
             if (voc.eq.NEGX) then
               wfrom = wfrom+1
             else if (voc.eq.POSX) then
               wfrom = wfrom+2
             else if (voc.eq.NEGY) then
               wfrom = wfrom+3
             else if (voc.eq.POSY) then
               wfrom = wfrom+4
             else if (voc.eq.NEGZ) then
               wfrom = wfrom+5
             else if (voc.eq.POSZ) then
               wfrom = wfrom+6
             else if (voc.eq.NEARPT) then
               wfrom = wfrom+7
               call parsit
               if (geom .and. geotyp.eq.POINT) then
                 sc(18) = tv
               else
                 call error(20)
                 goto 99999
               endif
             else if (voc.eq.IN) then
               wfrom = wfrom+8
             else if (voc.eq.OUT) then
               wfrom = wfrom+9
             else if (voc.eq.SMALL) then
               wfrom = wfrom+10
             else if (voc.eq.LARGE) then
               wfrom = wfrom+11
             else
               call error(182)
               goto 99999
             endif
             tv = sc(18)
             ktv(4) = wfrom
             sc(18) = tv
             if (nxteos) goto 99999
             goto 250
           endif
c
c..... ZONE,cv1,cv2,cv3,ALL/PART/MAIN
c
           if (vocab .and. voc.eq.ZONE) then
             i = 0
c...yurong300          call pthru(8)
  300        idtype = 8
             call parsit
c...yurong
             if (geom. and. (geotyp.eq.CURVE .or. geotyp.eq.CIRCLE))
     x                                                            then
               call gtdesc (tv, nclkey, nwds, ietype)
               call addsky(0,nclkey,1,i)
             else if (vocab .and. 
     x            (voc.eq.ALL .or. voc.eq.PART .or. voc.eq.MAIN)) then
               if (i .lt. 1) then
                 call delsky
               else
                 wfrom = wfrom + 100
                 if (voc .eq. ALL) then
                   wztyp = 0
                   wfrom = wfrom + 100
                 else if (voc .eq. MAIN) then
                   wztyp = 2
                   wfrom = wfrom + 200
                 endif
               endif
               tv = sc(18)
               ktv(4) = wfrom
               sc(18) = tv
               if (nxteos) goto 99999
               goto 250
             else if (i.gt.0 .and. vocab .and. 
     x            (voc.eq.LAST .or. voc.eq.VSTOP)) then
               if (voc .eq. LAST) then
                 wzlst = 1
                 nclkey = 0
                 call addsky(2,nclkey,1,i)
               endif
             else
               call delsky
               call error(21)
               goto 99999
             endif
             goto 300
           endif
c
c..... qar 95059 - 'part,last' should work just like 'last,part'
c..... when zones are active
c
           if (vocab .and. (voc.eq.LAST .or. voc.eq.VSTOP)) then
             if (wfrom.ge.100 .and. voc.eq.LAST) then
               wzlst = 1
               nclkey = 0
               call addsky(2,nclkey,1,i)
             endif
             if (nxteos) goto 99999
             goto 250
           endif
c
c..... max loops - optional
c
           if (scalar) then
             nloops = tv
             wltyp = 1
             wlops = tv
             if (nxteos) goto 99999
             goto 250
           endif
c
c..... CURVE or WCOMP - debug options
c
           if (vocab .and. (voc.eq.WCURVE .or. voc.eq.WCOMP)) then
c            if (voc .eq. WCURVE) then
c              isc10(4) = 0
c            else
c              isc10(4) = 1
c            endif 
             if (nxteos) goto 99999
             goto 250
           endif
c
c..... enter off part when possible
c
           if (vocab. and. voc.eq.OFF) then
             call parsit
             if (vocab. and. voc.eq.PART) offprt = 1
             if (nxteos) goto 99999
             call parsit
             if (scalar) then
                 offdis = tv
                 if (nxteos) goto 99999
                 call parsit
             else
                 goto 255
             endif 
             goto 250
           endif
c
c..... deep cutting
c
           if (vocab. and. (voc.eq.DEEP .or. voc.eq.LEVEL)) then
             if (voc .eq. DEEP) then
               isc10(4) = isc10(4) + 10
               wdeep = 1
             endif
             if (nxteos) goto 99999
             goto 250
           endif
c
c..... Step up cutting height
c
           if (vocab. and. voc.eq.STEPV) then
             call parsit
             if (vocab .and. voc .eq. UPV) then
               call parsit
               if (scalar) then
                 wstep = tv
               else
                 call error(7)
               endif
             else
               call error(232)
             endif
             if (nxteos) goto 99999
             goto 250
           endif
c
c...POCKET/POSITN,pt1,...,ptn
c
           if (vocab .and. voc .eq. VPOSITN) then
             itrflg = 2
  600        call gtpt (itrflg,pt,ierr)
             if (ierr .ne. 0) then
                 ierr   = 0
                 if (nxteos) goto 99999
                 goto 255
             else
                 if (VMPDRL(2) .ne. 0.) then
                     if (VMPDRL(1) .eq. 0.) VMPDRL(1) = sc(28)
                     hgt = (VMPDRL(1)/2.) / tan(VMPDRL(2)/RADIAN)
                     call uvcplvc (pt,sc(4),pt,hgt)
                 endif
                 call nclf_vm_push_entry(pt)
                 go to 600
             endif
           endif

           if (.not.nxteos) call error(4)
           goto 99999
c        
c***************** old pocket *****************
c
         else
           do 1100 i=11,18
               if (ityp.eq.2.and.ist.eq.2.or.(ityp.eq.3.or.ityp.eq.4))
     1             then
                   sc(i)=tv
                   if (i.lt.18) then
                       call parsit
                   else
                       idtype = 3
                       call parsit
                   endif
               else
                   call error(7)
                   goto 99999
               endif
1100       continue
           l=ifl(4)+1
           j=1
           numpts=0
           do 1200 i=1,20
               if (ityp.eq.2.and.ist.eq.3) then
                   numpts=numpts+1
                   if (numpts.gt.20) then
                       call error(43)
                       goto 99999
                   endif
                   jbuff(j)=tv
                   j=j+1
c...yurong300                call pthru(3)
                   idtype = 3
                   call parsit
c...yurong
               else if (ityp.eq.7) then
                   if (numpts.lt.3) then
                       call error(34)
                       goto 99999
                   endif
                   isc10(3)=i-1
                   isc10(2)=1
c
c..... flush the buffer
c
                   call putran(jbuff,l)
                   goto 88888
               else
                   call error(2)
                   goto 99999
               endif
1200       continue
           call error(15)
           goto 99999
         endif
      else if (isc10(1).eq.SCRUB) then
c        
c***************** scrub *****************
c
           if (ityp.eq.2 .and.ist.eq.9) then
               sc(11)=tv
           else
               call error(186)
               goto 99999
           endif
           call parsit
           if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.ist.eq.2))
     1         then
               sc(12)=tv
               call parsit
           else
               call error(7)
               goto 99999
           endif
           if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.ist.eq.2))
     1         then
               sc(13)=tv
           else
               call error(7)
               goto 99999
           endif
           if (nextyp.eq.11) then
               isc10(2)=1
           else
               do 1300 i=14,17
c...yurong                   call pthru(3)
                  idtype = 3
                  call parsit
c...yurong
                   if (err) goto 99999
                   if (ityp.eq.2.and.ist.eq.3) then
                       sc(i)=tv
                   else
                       call error(20)
                   endif
1300           continue
               isc10(3)=4
               isc10(2)=2
           endif
      else if (isc10(1).eq.FMILL) then
c        
c***************** fmill *****************
c
           isc10(2) = 0
           isc10(3) = 0
           isc10(4) = 0
           sc(12)   = 0.0d0
           sc(13)   = 0.0d0
           sc(14)   = 0.0d0
           sc(15)   = 0.0d0
           sc(16)   = 0.0d0
           sc(18)   = 0.0d0
           sc(19)   = 0.0d0
           sc(21)   = 0.0d0
           sc(22)   = 0.0d0
           lboth = .false.
           avflg = 0
           avrpto = 0
           avlst = 0
           avdir = 0
           avfr1 = 0
           avfr2 = 0
           navcsf = 0
           delflg = 0
c
c..... mandatory part first
c
           if (.not. geom .or. ist.ne.SURF) then
             call error(186)
             goto 99999
           endif
           sc(11) = tv
           call parsit
           if (.not. vocab .or. (ist.ne.HEIGHT .and. ist.ne.PASS)) then
             call error(5)
             goto 99999
           endif
           fct = 1.0d0
           if (ist.eq.HEIGHT) fct = -1.0d0
           call parsit
           if (.not. scalar) then
             call error(7)
             goto 99999
           endif
           sc(12)=tv*fct
           scsav(1) = sc(12)
           if (nxteos) goto 99999
           call parsit
c
c..... optional part
c
1350       if (vocab .and. ist.eq.START) then
             call parsit
             if (scalar) then
               if (tv.lt.0.0 .or. tv.gt.1.0d0) then
                 call error(377)
                 goto 99999
               endif
               r4(1) = tv
               scsav(2) = r4(1)
               call parsit
               if (.not. scalar) then
                 call error(7)
                 goto 99999
               endif
               if (tv.lt.0.0 .or. tv.gt.1.0d0) then
                 call error(377)
                 goto 99999
               endif
               r4(2)  = tv
               scsav(3) = r4(2)
               sc(13) = r8
             else if (geom .and. (ist.eq.POINT.or.ist.eq.PNTVEC)) then
               isc10(2) = 1
               sc(13)   = tv
             else
               call error(33)
               goto 99999
             endif
             if (nxteos) then
                 call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1				           sc(18),sc(20),sc(21),sc(22),0.0,
     2                         avasn2,avfr2,avasn1, avfr1, isc10(3),
     3                         isc10(2),isc10(4)) 
               goto 99999
             endif
             call parsit
             goto 1350
           else if (vocab .and. ist.eq.FWD) then
             call parsit
             if (.not. scalar .or. itv.lt.0 .or. itv.gt.1) then
               call error(170)
               goto 99999
             endif
             isc10(3) = itv
             if (nxteos) then
               call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),sc(18),
     1                       sc(20),sc(21),sc(22),0.0,avasn2,avfr2,
     2                       avasn1,avfr1,isc10(3),isc10(2),isc10(4)) 
               goto 99999
             endif
             call parsit
             goto 1350
           else if (vocab .and. ist.eq.VCS) then
             call parsit
             if (.not.geom) then
               call error(1)
               goto 99999
             endif
1500         navcsf = navcsf+1
             avcsf(navcsf) = tv
             avthk(navcsf) = 0
             if (nxteos) goto 99999
             call parsit
             if (scalar) then
               if (tv .gt. sc(27)) avthk(navcsf) = tv
               if (nxteos) then
                 call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                         sc(18),sc(20),sc(21),sc(22),0.0,
     2                         avasn2,avfr2,avasn1,avfr1,isc10(3),
     3                         isc10(2),isc10(4))  
                 goto 99999
               endif
               call parsit
             endif
             if (geom .and. navcsf.lt.max_avcsf) goto 1500
             goto 1350
           else if (vocab .and. (ist.eq.VTOLER .or. ist.eq.VSTEP)) then
             fct = 1.0d0
             if (ist.eq.VTOLER) fct = -1.0d0
             call parsit
             if (.not. scalar) then
                 call error(7)
                 goto 99999
             endif
             sc(14) = tv*fct
             scsav(4) = sc(14)
             if (nxteos) then
                 call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                         sc(18),sc(20),sc(21),sc(22),0.0,
     2                         avasn2,avfr2,avasn1,avfr1,isc10(3),
     3                         isc10(2),isc10(4))  
                 goto 99999
             endif
             call parsit
             goto 1350
           else if (vocab .and. (ist.eq.VOCTO.or.ist.eq.PAST.or.
     x                                           ist.eq.CONTCT)) then
             if (ist.eq.VOCTO) then
               isc10(4) = 1
             else if (ist.eq.PAST) then
               isc10(4) = 3
             endif
             if (nxteos) then
               call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                       sc(18),sc(20),sc(21),sc(22),0.0,avasn2,
     2                       avfr2,avasn1,avfr1,isc10(3),isc10(2),
     3                       isc10(4))  
               goto 99999
             endif
             call parsit
             goto 1350
           else if (vocab .and. ist.eq.ON) then
             if (nxteos) then
               isc10(4) = 2
               call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1            sc(18),sc(20),sc(21),sc(22),0.0,avasn2,
     2            avfr2,avasn1,avfr1,isc10(3),isc10(2),isc10(4))  
               goto 99999
             endif
             call parsit
             if (geom .and. (ist.eq.CURVE .or.ist.eq.CIRCLE)) then
               sc(15) = tv
               if (nxteos) then
                 call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                         sc(18),sc(20),sc(21),sc(22),0.0,
     2                         avasn2,avfr2,avasn1,avfr1,isc10(3),
     3                         isc10(2),isc10(4))  
                 goto 99999
               endif
               call parsit
             else
               isc10(4) = 2
             endif
             goto 1350
           else if (vocab.and.ist.eq.AVOID) then
             call parsit
             if (vocab .and. ist.eq.THRU) then
               avflg = 4
             else if (geom) then
               if (ist .eq. PLANE) then
                 avflg = rpln
               else if (ist .eq. SURF) then
                 avflg = rsrf
               else
                 call error(326)
                 goto 99999
               endif             
               avasn1 = tv
             else if (scalar .and. tv.gt.0) then
               avflg = rdis
               avasn1 = tv
               scsav(13) = avasn1
             else if (vocab .and. ist.eq.DOWN) then
               avflg = 5
             else if (vocab .and. ist.eq.BOTH) then
               lboth = .true.
               avflg = 5
               call parsit

               if (vocab .and. (ist.eq.VSHORT .or. ist.eq.SAME .or.
     x                              ist.eq.CLW .or. ist.eq.CCLW)) then
                 if (ist .eq. SAME) then
                   avdir = bsame
                 else if (ist .eq. CLW) then
                   avdir = bcw
                 else if (ist .eq. CCLW) then
                   avdir = bccw
                 endif
                 call parsit
               endif

               if (scalar .and. tv.gt.0) then
                 avflg = rdis
               else if (geom) then
                 if (ist .eq. PLANE) then
                   avflg = rpln
                 else if (ist .eq. SURF) then
                   avflg = rsrf
                 else
                   call error(326)
                   goto 99999
                 endif
               endif             
               avasn1 = tv
               scsav(13) = avasn1
             endif
             if (nxteos) then
               if (avflg.eq.rdis .or. avflg.eq.rpln .or. 
     x                                avflg.eq.rsrf) call error(326)
               call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                       sc(18),sc(20),sc(21),sc(22),0.0,avasn2,
     2                       avfr2,avasn1,avfr1,isc10(3),isc10(2),
     3                       isc10(4))  
               goto 99999
             endif
             call parsit
             if (avflg.eq.rdis .or. avflg.eq.rpln .or. 
     x                                   avflg.eq.rsrf) then
               if (vocab .and. (ist.eq.FEDRAT .or. ist.eq.RAPID)) then
                 if (ist.eq.FEDRAT) then
                   call parsit
                   if (scalar .and. tv.ge.0) then
                     avfr1 = tv
                     scsav(14) = avfr1
                   else if (vocab .and. ist.eq.RAPID) then
                     avfr1 = -1
                   else
                     call error(327)
                   endif
                 else
                   avfr1 = -1
                 endif
                 if (nxteos) then
                   call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                           sc(18),sc(20),sc(21),sc(22),0.0,
     2                           avasn2,avfr2,avasn1,avfr1,isc10(3),
     3                           isc10(2),isc10(4))  
                   goto 99999
                 endif
                 call parsit
               endif
               avasn2 = avasn1
               if (lboth) avflg = avflg + 50
               if (geom .or. (scalar .and. tv.ge.0)) then
                 avasn2 = tv
                 scsav(11) = avasn2
                 if (geom) then
                   if (ist .eq. PLANE) then
                     avrpto = rpln
                   else if (ist .eq. SURF) then
                     avrpto = rsrf
                   else
                     call error(326)
                     goto 99999
                   endif             
                 else
                   avrpto = rdis
                 endif
                 if (nxteos) then
                   call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                           sc(18),sc(20),sc(21),sc(22),0.0,
     2                           avasn2,avfr2,avasn1,avfr1,isc10(3),
     3                           isc10(2),isc10(4))  
                   goto 99999
                 endif
                 call parsit
                 if (vocab .and. (ist.eq.FEDRAT .or. ist.eq.RAPID)) then
                   if (ist.eq.FEDRAT) then
                     call parsit
                     if (scalar .and. tv.ge.0) then
                       avfr2 = tv
                       scsav(12) = avfr2
                     else if (vocab .and. ist .eq. RAPID) then
                       avfr2 = -1
                     else
                       call error(327)
                     endif
                   else
                     avfr1 = -1
                   endif
                   if (nxteos) then
                     call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                             sc(18),sc(20),sc(21),sc(22),0.0,
     2                             avasn2,avfr2,avasn1,avfr1,
     3                             isc10(3),isc10(2),isc10(4))  
                     goto 99999
                   endif
                   call parsit
                 endif
               endif               
             endif
             goto 1350
           else if (vocab.and.ist.eq.OMIT) then
             call parsit
             if (vocab .and. ist.eq.START) then
                delflg = 1
             else if (vocab .and. ist.eq.VOCEND) then
                delflg = 2
             else if (vocab .and. ist.eq.BOTH) then
                delflg = 3
             endif
             if (nxteos) then
               call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                       sc(18),sc(20),sc(21),sc(22),0.0,avasn2,
     2                       avfr2,avasn1, avfr1, isc10(3),isc10(2),
     3                       isc10(4))  
               goto 99999
             endif
             call parsit
             goto 1350
           else if (vocab.and.ist.eq.RAPTO) then
             call parsit
             if (geom) then
               if (ist .eq. PLANE) then
                 sc(16) = rpln
               else if (ist .eq. SURF) then
                 sc(16) = rsrf
               else
                 call error(326)
                 goto 99999
               endif             
             else if (scalar) then
               sc(16) = rdis
             else 
               call error(281)
               goto 99999
             endif
             sc(17) = tv
             scsav(5) = sc(17)
             if (nxteos) then
               call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                       sc(18),sc(20),sc(21),sc(22),0.0,avasn2,
     2                       avfr2,avasn1,avfr1,isc10(3),isc10(2),
     3                       isc10(4)) 
               goto 99999
             endif
             call parsit
             if (scalar .or. (vocab .and. ist .eq. RAPID)) then
               if (scalar) then
                 sc(18) = tv
                 scsav(6) = sc(18)
               else 
                 sc(18) = -1
                 scsav(6) = sc(18)
               endif
               if (nxteos) then
                 call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                         sc(18),sc(20),sc(21),sc(22),0.0,
     2                         avasn2,avfr2,avasn1,avfr1,isc10(3),
     3                         isc10(2),isc10(4))  
                 goto 99999
               endif
               call parsit
             endif
             goto 1350
           else if (vocab .and. ist .eq. RETRCT) then
             call parsit
             if (geom) then
               if (ist .eq. PLANE) then
                 sc(19) = rpln
               else if (ist .eq. SURF) then
                 sc(19) = rsrf
               else
                 call error(326)
                 goto 99999
               endif
             else if (scalar) then
               sc(19) = rdis
             else 
               call error(389)
               goto 99999
             endif
             sc(20) = tv
             scsav(7) = sc(20)
             if (nxteos) then
               call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                       sc(18),sc(20),sc(21),sc(22),0.0,avasn2,
     2                       avfr2,avasn1,avfr1,isc10(3),isc10(2),
     3                       isc10(4))  
               goto 99999
             endif
             call parsit
             if (scalar .or. (vocab .and. ist .eq. RAPID)) then
               if (scalar) then
                 sc(21) = tv
                 scsav(8) = sc(21)
               else if (vocab .and. ist .eq. RAPID) then
                 sc(21) = -1
                 scsav(8) = sc(21)
               endif
               if (nxteos) then
                 call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                         sc(18),sc(20),sc(21),sc(22),0.0,
     2                         avasn2,avfr2,avasn1,avfr1,isc10(3),
     3                         isc10(2),isc10(4)) 
                 goto 99999
               endif
               call parsit
             endif
             goto 1350
           else if (vocab .and. (ist.eq.LAST .or. ist.eq.VSTOP)) then
             if (ist .eq. LAST) avlst = 1
             if (nxteos) then
               call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                       sc(18),sc(20),sc(21),sc(22),0.0,avasn2,
     2                       avfr2,avasn1,avfr1,isc10(3),isc10(2),
     3                       isc10(4)) 
               goto 99999
             endif
             call parsit
             if (vocab .and. ist .eq. FEDRAT) then
               call parsit
               if (scalar .and. tv.ge.0) then
                 sc(22) = tv
                 scsav(9) = sc(22)
               else if (vocab .and. ist .eq. RAPID) then
                 call error(402)
               else
                 call error(327)
               endif
               if (nxteos) then
                 call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                         sc(18),sc(20),sc(21),sc(22),0.0,
     2                         avasn2,avfr2,avasn1,avfr1,isc10(3),
     3                         isc10(2),isc10(4)) 
                 goto 99999
               endif
               call parsit
             endif
             goto 1350
           else if (vocab .and. (ist.eq.VSHORT .or. ist.eq.SAME .or.
     x                              ist.eq.CLW .or. ist.eq.CCLW)) then
             if (ist .eq. SAME) then
               avdir = bsame
             else if (ist .eq. CLW) then
               avdir = bcw
             else if (ist .eq. CCLW) then
               avdir = bccw
             endif
             if (nxteos) then
               call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                       sc(18),sc(20),sc(21),sc(22),0.0,avasn2,
     2                       avfr2,avasn1,avfr1,isc10(3),isc10(2),
     3                       isc10(4)) 
               goto 99999
             endif
             call parsit
             endif
             goto 1350

             call fmillsav(sc(12),r4(1),r4(2),sc(14),sc(17),
     1                     sc(18),sc(20),sc(21),sc(22),0.0,avasn2,
     2                     avfr2,avasn1,avfr1,isc10(3),isc10(2),
     3                     isc10(4)) 
         
      else if (isc10(1).eq.SMILL) then
c        
c***************** smill *****************
c      
        isc10(2) = 0
        isc10(3) = 0
        isc10(4) = 0
        sc(12)   = 0.0d0
        sc(13)   = 0.0d0
        sc(14)   = 0.0d0
        sc(15)   = 0.0d0
        sc(16)   = 0.0d0
        sc(17)   = 0.0d0
        sc(18)   = 0.0d0
        sc(19)   = 0.0d0
        sc(20)   = 0.0d0       
        sc(21)   = 0.0d0
        sc(22)   = 0.0d0
        avlasn   = 0.0d0
        avlfr    = 0.0d0
        navpsf = 0
        nend = 0
        navbnd = 0
        navstep = -1
        navtype = 0
        nvstep = 0
c
c..... mandatory part first
c               
       if (vocab .and. voc.eq.LAYER) then
           call parsit
           if (.not.(ityp.eq.5 .and. ist.eq.1)) then
             call error(6)
             goto 99999
           endif
           call parsit
           if (.not.scalar) then
             call error(7)
             goto 99999
           endif
           isc10(3) = tv        
           call parsit
       else
          if (.not. geom .or. ist.ne.SURF) then
             call error(186)
             goto 99999
           endif    
      
2500       navpsf = navpsf+1
           avpsf(navpsf) = tv
           if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
           call parsit
           if (geom.and.navpsf.lt.max_avpsf .and. ist.eq.SURF) goto 2500
       endif
c
c....edge condition(TO,PAST,ON,CONTCT)
c
       if (vocab .and. (ist .eq. VOCTO .or. ist.eq. PAST .or.
     x                  ist .eq. ON .or. ist .eq. CONTCT)) then
          if (ist .eq. PAST) then
            navend = 1
          else if (ist .eq. ON) then       
            navend = 2
          else if (ist .eq. CONTCT) then
            navend = 3
          else
            navend = 0
          endif
          if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
          call parsit
       else             
          navend = 0
       endif      
c
c....Drive plane or drive vector
c       
       if (geom .and. (ist .eq. PLANE .or. ist .eq. SURF .or.
     1     ist .eq. VECTOR .or. ist .eq. PNTVEC .or. ist .eq. LINE))
     2         then
           if (ist .eq. PLANE .or. ist .eq. SURF) then
                if (ist .eq. SURF) then
                    call gtdesc (tv,nclkey,nwds,ietype)
                    call ncl_get_sf_primtyp(nclkey,ietype)
                    if (ietype .ne. 3) then
                        call error (19)
                        goto 99999
                    endif
                endif
                navd = 2
                avdpl(1) = tv
                if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
                call parsit
                if (geom .and. (ist .eq. PLANE .or. ist .eq. SURF)) then
                    if (ist .eq. SURF) then
                        call gtdesc (tv,nclkey,nwds,ietype)
                        call ncl_get_sf_primtyp(nclkey,ietype)
                        if (ietype .ne. 3) then
                            call error (19)
                            goto 99999
                        endif
                    endif
                    avdpl(2) = tv
                    if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
                    call parsit          
                else
                    call error(7)
                    goto 99999
                endif
           else if (ist .eq. VECTOR .or. ist .eq. PNTVEC .or. 
     x              ist .eq. LINE) then
               navd = 1
               avdpl(1) = tv
               if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
               call parsit               
cc               if (iprv .eq. LINE .and. geom .and. ist .eq. LINE) then 
cc                    navd = 2       
cc                    avdpl(2) = tv
cc                    if (nxteos) goto 99999
cc                    call parsit          
cc               endif
           endif
       else  
           call error(459)
           goto 99999      
       endif
c
c....boundary confinement
c
       if (vocab .and. voc.eq. BOUND) then
           call parsit
           if (ist .eq. PART) then
             navbnd = 0
           else if (ist .eq. CURVE .or. ist .eq. CIRCLE) then
             navbnd = 1
             avbnd = tv
           endif       
           if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
           call parsit  
       else  
           navbnd = 0   
       endif
c
c....PASS number or Scallop height,step distance
c 
       if (.not. vocab .or. (ist.ne.HEIGHT .and. 
     x      ist.ne.PASS .and. ist.ne.VSTEP)) then
             call error(5)
             goto 99999
       endif     
3500   if (ist .eq. HEIGHT) then
          nvstep = 0
          navstep = 0
       else if (ist.eq.PASS) then
          navstep = 1
       else if (ist.eq.VSTEP)  then
          nvstep = 1
          if (navstep .eq. -1) then
             navstep = 2
          else    
             navstep = 0
          endif
       endif
       call parsit
       if (.not. scalar) then
           call error(7)
           goto 99999
       endif
       if (nvstep .eq. 1) then
         sc(13)=tv
         avpsf(213)=sc(13)
       else
         sc(12)=tv
         avpsf(212)=sc(12)
       endif
       if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
       call parsit
       if (vocab .and. (ist.eq.HEIGHT .or. ist.eq.VSTEP)) goto 3500
c
c.....START point
c       
      if (vocab .and. voc.eq.START) then       
          call parsit       
          if (geom .and. geotyp.eq.POINT) then
              sc(14) = tv
          else
              call error(20)
              goto 99999   
          endif
          if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
          call parsit
      endif
c
c....SCRUB,COMBIN or LACE
c 
       if (vocab .and. (ist.eq.SCRUB .or. 
     x      ist.eq.1071 .or. ist.eq.LACE)) then
          if (ist .eq. SCRUB) then
            navtype = 0
          else if (ist.eq.1071) then
             navtype = 1
          else if (ist.eq.LACE)  then
            navtype = 2
          endif
          if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
          call parsit
c
c.....LACE retract options
c          
          if (geom .or. scalar) then
             if (geom) then
               if (ist .eq. PLANE) then
                 navlrpto = rpln
               else if (ist .eq. SURF) then
                 navlrpto = rsrf
               else
                 call error(326)
                 goto 99999
               endif             
             else if (scalar) then
               navlrpto = rdis
             endif
             avlasn = tv
             if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
             call parsit           
             if (scalar .or. (vocab .and. ist.eq.RAPID)) then
                if (scalar) then
                    avlfr = tv
                else 
                    avlfr = -1
                endif
                if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
                call parsit
             endif
          endif 
       else
          navtype = 0
       endif
c
c.....Tolerance
c       
       if (vocab .and. voc .eq. VTOLER) then 
           call parsit              
           if (.not.scalar) then
               call error(7)
               goto 99999
           endif
           sc(27) = tv
           if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
           call parsit
       endif
c
c.....Rapid and Retract
c       
       if ((vocab .and. ist.eq.RAPTO)) then
             call parsit
             if (geom) then
               if (ist .eq. PLANE) then
                 sc(16) = rpln
               else if (ist .eq. SURF) then
                 sc(16) = rsrf
               else
                 call error(326)
                 goto 99999
               endif             
             else if (scalar) then
               sc(16) = rdis
             else 
               call error(281)
               goto 99999
             endif
             sc(17) = tv
             if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
             call parsit          
             if (scalar .or. (vocab .and. ist.eq.RAPID)) then
                if (scalar) then
                    sc(18) = tv
                else 
                    sc(18) = -1
                endif
                if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
                call parsit
             endif
       endif
            
       if (vocab.and.ist.eq.RETRCT) then
           call parsit
           if (geom) then
               if (ist .eq. PLANE) then
                 sc(19) = rpln
               else if (ist .eq. SURF) then
                 sc(19) = rsrf
               else
                 call error(326)
                 goto 99999
               endif
           else if (scalar) then
               sc(19) = rdis
           else 
               call error(389)
               goto 99999
           endif
           sc(20) = tv
           if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
           call parsit
           if (scalar .or. (vocab .and. ist.eq.RAPID)) then
               if (scalar) then
                 sc(21) = tv
               else if (vocab .and. ist.eq.RAPID) then
                 sc(21) = -1
               endif
               if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
               call parsit
           endif
       endif
c
c...Ignore inner boundary
c
       if (vocab.and.ist.eq.OMIT) then
           call parsit
           if (vocab) then
               if (ist.eq.IN) then
                   ifl(394) = 1;
               else if (ist.eq.NOMORE) then
                   ifl(394) = 0;
               else
                   call error(232)
               endif
           else
               call error(232)
           endif
           if (nxteos) then
               call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
               goto 99999
               endif
           call parsit
       endif
       
       call smillsav (sc(12), sc(13),sc(17),sc(20),
     x isc10(3),avlasn,avlfr,sc(27), sc(18), sc(21) )
c.....end of smill
c
      else if (isc10(1).eq.PMILLV) then
          
c        
c***************** pmill *****************
c.....Currently uses same syntax as SMILL
c
        isc10(2) = 0
        isc10(3) = 0
        isc10(4) = 0
        sc(12)   = 0.0d0
        sc(13)   = 0.0d0
        sc(14)   = 0.0d0
        sc(15)   = 0.0d0
        sc(16)   = 0.0d0
        sc(17)   = 0.0d0
        sc(18)   = 0.0d0
        sc(19)   = 0.0d0
        sc(20)   = 0.0d0       
        sc(21)   = 0.0d0
        sc(22)   = 0.0d0
        avlasn   = 0.0d0
        avlfr    = 0.0d0
        pavpsf = 0
        pend = 0
        pavbnd = 0
        pavstep = -1
        pavtype = 0
        pvstep = 0
        pavtran = 2
        pavdir = 1
c
c..... mandatory part first
c               
       if (vocab .and. voc.eq.LAYER) then
           call parsit
           if (.not.(ityp.eq.5 .and. ist.eq.1)) then
             call error(6)
             goto 99999
           endif
           call parsit
           if (.not.scalar) then
             call error(7)
             goto 99999
           endif
           isc10(3) = tv        
           call parsit
       else
          if (.not. geom .or. ist.ne.SURF) then
             call error(186)
             goto 99999
           endif    
      


4500       pavpsf = pavpsf+1
           appsf(pavpsf) = tv
           if (nxteos) goto 99999
           call parsit
           if (geom.and.pavpsf.lt.max_avpsf .and. ist.eq.SURF) goto 4500
       endif
c
c....edge condition(TO,PAST,ON,CONTCT)
c
       if (vocab .and. (ist .eq. VOCTO .or. ist.eq. PAST .or.
     x                  ist .eq. ON .or. ist .eq. CONTCT)) then
          if (ist .eq. PAST) then
            pavend = 1
          else if (ist .eq. ON) then       
            pavend = 2
          else if (ist .eq. CONTCT) then
            pavend = 3
          else
            pavend = 0
          endif
          if (nxteos) goto 99999
          call parsit
       else             
          pavend = 0
       endif      
c
c....Drive plane or drive vector
c       
       if (geom .and. (ist .eq. PLANE .or. ist .eq. SURF .or.
     1     ist .eq. VECTOR .or. ist .eq. PNTVEC .or. ist .eq. LINE))
     2         then
           if (ist .eq. PLANE .or. ist .eq. SURF) then
                if (ist .eq. SURF) then
                    call gtdesc (tv,nclkey,nwds,ietype)
                    call ncl_get_sf_primtyp(nclkey,ietype)
                    if (ietype .ne. 3) then
                        call error (19)
                        goto 99999
                    endif
                endif
                pavd = 2
                apdpl(1) = tv
                if (nxteos) goto 99999
                call parsit
                if (geom .and. (ist .eq. PLANE .or. ist .eq. SURF)) then
                    if (ist .eq. SURF) then
                        call gtdesc (tv,nclkey,nwds,ietype)
                        call ncl_get_sf_primtyp(nclkey,ietype)
                        if (ietype .ne. 3) then
                            call error (19)
                            goto 99999
                        endif
                    endif
                    apdpl(2) = tv
                    if (nxteos) goto 99999
                    call parsit          
                else
                    call error(7)
                    goto 99999
                endif
           else if (ist .eq. VECTOR .or. ist .eq. PNTVEC .or. 
     x              ist .eq. LINE) then
               pavd = 1
               apdpl(1) = tv
               if (nxteos) goto 99999
               call parsit
           endif
       else  
           call error(459)
           goto 99999      
       endif
c
c....boundary confinement
c
       if (vocab .and. voc.eq. BOUND) then
           call parsit
           if (ist .eq. PART) then
             pavbnd = 0
           else if (ist .eq. CURVE .or. ist .eq. CIRCLE) then
             pavbnd = 1
             apbnd = tv
           endif       
           if (nxteos) goto 99999
           call parsit  
       else  
           pavbnd = 0   
       endif
c
c....PASS number or Scallop height,step distance
c 
       if (.not. vocab .or. (ist.ne.HEIGHT .and. 
     x      ist.ne.PASS .and. ist.ne.VSTEP)) then
             call error(5)
             goto 99999
       endif     
5500   if (ist .eq. HEIGHT) then
          pvstep = 0
          pavstep = 0
       else if (ist.eq.PASS) then
          pavstep = 1
       else if (ist.eq.VSTEP)  then
          pvstep = 1
          if (pavstep .eq. -1) then
             pavstep = 2
          else    
             pavstep = 0
          endif
       endif
       call parsit
       if (.not. scalar) then
           call error(7)
           goto 99999
       endif
       if (pvstep .eq. 1) then
         sc(13)=tv
       else
         sc(12)=tv
       endif
       if (nxteos) goto 99999
       call parsit
       if (vocab .and. (ist.eq.HEIGHT .or. ist.eq.VSTEP)) goto 5500
c
c.....START point
c       
      if (vocab .and. voc.eq.START) then       
          call parsit       
          if (geom .and. geotyp.eq.POINT) then
              sc(14) = tv
          else
              call error(20)
              goto 99999   
          endif
          if (nxteos) goto 99999
          call parsit
      endif
c
c.....Transition type
c       
      if (vocab .and. (voc.eq.HELIX .or.
     x    voc.eq.ARC .or. voc.eq.LINEAR)) then
          if (voc.eq.HELIX) then
            pavtran = 0
          else if (voc.eq.ARC) then
            pavtran = 1
            call parsit
            if (.not. scalar) then
              call error(7)
              goto 99999
            endif
            atrad = tv
          else
            pavtran = 2
          endif
          if (nxteos) goto 99999
          call parsit
      endif
c
c.....Transition type
c       
      if (vocab .and. (voc.eq.CCLW .or. voc.eq.CLW)) then
          if (voc.eq.CCLW) then
            pavdir = 0
          else
            pavdir = 1
          endif
          if (nxteos) goto 99999
          call parsit
      endif
c
c....SCRUB,COMBIN or LACE
c 
       if (vocab .and. (ist.eq.SCRUB .or. 
     x      ist.eq.1071 .or. ist.eq.LACE)) then
          if (ist .eq. SCRUB) then
            pavtype = 0
          else if (ist.eq.1071) then
             pavtype = 1
          else if (ist.eq.LACE)  then
            pavtype = 2
          endif
          if (nxteos) goto 99999
          call parsit
c
c.....LACE retract options
c          
          if (geom .or. scalar) then
             if (geom) then
               if (ist .eq. PLANE) then
                 pavlrpto = rpln
               else if (ist .eq. SURF) then
                 pavlrpto = rsrf
               else
                 call error(326)
                 goto 99999
               endif             
             else if (scalar) then
               pavlrpto = rdis
             endif
             aplasn = tv
             if (nxteos) goto 99999
             call parsit           
             if (scalar .or. (vocab .and. ist.eq.RAPID)) then
                if (scalar) then
                    aplfr = tv
                else 
                    aplfr = -1
                endif
                if (nxteos) goto 99999
                call parsit
             endif
          endif 
       else
          pavtype = 0
       endif
c
c.....Tolerance
c       
       if (vocab .and. voc .eq. VTOLER) then 
           call parsit              
           if (.not.scalar) then
               call error(7)
               goto 99999
           endif
           sc(27) = tv
           if (nxteos) goto 99999
           call parsit
       endif
c
c.....Rapid and Retract
c       
       if ((vocab .and. ist.eq.RAPTO)) then
             call parsit
             if (geom) then
               if (ist .eq. PLANE) then
                 sc(16) = rpln
               else if (ist .eq. SURF) then
                 sc(16) = rsrf
               else
                 call error(326)
                 goto 99999
               endif             
             else if (scalar) then
               sc(16) = rdis
             else 
               call error(281)
               goto 99999
             endif
             sc(17) = tv
             if (nxteos) goto 99999
             call parsit          
             if (scalar .or. (vocab .and. ist.eq.RAPID)) then
                if (scalar) then
                    sc(18) = tv
                else 
                    sc(18) = -1
                endif
                if (nxteos) goto 99999
                call parsit
             endif
       endif
            
       if (vocab.and.ist.eq.RETRCT) then
           call parsit
           if (geom) then
               if (ist .eq. PLANE) then
                 sc(19) = rpln
               else if (ist .eq. SURF) then
                 sc(19) = rsrf
               else
                 call error(326)
                 goto 99999
               endif
           else if (scalar) then
               sc(19) = rdis
           else 
               call error(389)
               goto 99999
           endif
           sc(20) = tv
           if (nxteos) goto 99999
           call parsit
           if (scalar .or. (vocab .and. ist.eq.RAPID)) then
               if (scalar) then
                 sc(21) = tv
               else if (vocab .and. ist.eq.RAPID) then
                 sc(21) = -1
               endif
               if (nxteos) goto 99999
               call parsit
           endif
       endif
c

c...Ignore inner boundary
c
       if (vocab.and.ist.eq.OMIT) then
           call parsit
           if (vocab) then
               if (ist.eq.IN) then
                   ifl(394) = 1;
               else if (ist.eq.NOMORE) then
                   ifl(394) = 0;
               else
                   call error(232)
               endif
           else
               call error(232)
           endif
           if (nxteos) goto 99999
           call parsit
       endif
c        
c.....end of pmill
c              
      endif
           
c        
c.....end of smill
c              
c      endif

88888  if (ifl(111).eq.0) then
          if (nextyp.ne.11) call error(4)
          goto 99999
       endif

       if (nextyp.ne.7) call error(4)

99999  return
       end

	   