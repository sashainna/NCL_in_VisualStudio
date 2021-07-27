C*********************************************************************
C*    NAME         :  declcv.f
C*       CONTAINS:
C*             declcv  prjprs
C*
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       declcv.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       08/17/15 , 17:50:55
C*********************************************************************
c
c*********************************************************************
c    e_subroutine     : subroutine declcv
c      this routine parses curve declaration. the valid syntax
c      constructs are:
c          1.  (name =) curve/pt1,pt2,ve1...(any point may have vector;
c                                           max points = 20)
c          2.  (name =) curve/fit,pt2,ve1,...(same as above except no max
c                                           number of points.
c          3.  (name =) curve/cv1,<xval>,<yval>,<zval>
c
c          4.  (name =) curve/sf1,<edge>,<num-pts>,<u/v-offset>
c
c          5.  (name =) curve/intof,sf1,sf2,[pt1]
c              (name =) curve/intof,sf1,pl1,[pt1]
c
c          6.  (name =) curve/conic,pt1,ve1,pt2,pt3,ve2
c                             conic,pt1,ve1,pt2,pt3,pt4
c                             conic,pt1,pt2,pt3,pt4,ve1
c                             conic,pt1,pt2,pt3,pt4,pt5
c
c          7.  (name =) curve/cv1,thru,cv4,cv5.....(max=50) (not implemented)
c
c          8.  (name =) curve/compos,ln|ci|cv,...,ln|ci|cv
c
c          9.  (name =) curve/spline, pt1,ve1,pt2,...
c
c         10.  (name =) curve/spline, fit, pt1,ve1,pt2,...
c
c         11.   name =  curve/cv1  - here isc10(2) = 3
c
c         12.   name =  spline/out,sf,n
c
c         13.   name =  spline/sf1,<edge>,<num-pts>,<u/v-offset>
c
c         14.  (name =) spline/intof,sf1,sf2,[pt1]
c              (name =) spline/intof,sf1,pl1,[pt1]
c
c         15.   name =  cv/offset,cv1,[comp1[,comp2]]xlarge,.25[,LINEAR]
c                                                                SMOOTH
c                                                                CHAMFR
c
c         16.  (name =) spline/cv1,<xval>,<yval>,<zval>
c
c         17.   name =  spline/offset,cv1,xlarge,.25
c
c         18.  (name =) cv/PROJCT,cv(,ve),sf(,WRAP)(,ATTACHPT)(,NEARPT)
c
c         19.  (name =) spline/PROJCT,cv(,ve),sf(,WRAP)(,ATTACHPT)(,NEARPT)
c
c      the following represents the info passed to geogn2 in syscom:
c             sc(10)             !       sc(11) thru sc(nn)
c--------------------------------!----------------------------------------
c type  subtype  numwds  notused !   sc(11)      sc(12)      sc(13)     sc(14)
c ----  -------  ------  ------- !   ------      ------      ------     ------
c  606     1   num elmnts  *     ! sc(11)-sc(nn)=elements (max=20 points)
c  606     2   num elmnts  *     ! sc(11)-sc(nn)=elements
c  606     3   num vals    *     !  cv1 loc       xval        yval        zval
c  606     4   edge      npts    !  sf1 loc      u/v offset
c  606     5   nr pt flg   *     !  sf1 loc    sf2/pl1 loc  nr pt loc
c  606     6   which fmt   *     !  list of pts and ves
c  606     7   n/a         *     !  list of cvs   (not implemented)
c  606     8   num elmnts  *     !  list of lines, circles and composite curves
c  606     9   num elmnts  *     ! elements
c  606    10   num elmnts  *     ! elements
c  606    12   n/a         *     !  sf loc         num
c  606    15   n/a         *     !  cv name       modifier    dist     not used
c                                !
c*************************************************************************

      subroutine declcv

      include 'com8a.com'
      include 'sfsio.com'
      include 'vocab.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld, svinx, ssub
      integer*2 istold

      parameter (maxpt=50, jbuff_size = 35)

      character*64 svtkn,stok

      integer*2 irest(4)
      equivalence (rest,irest)
c      real*8 rtok
c      equivalence (token2,leq1),(rtok,leq1),(rest,irest)
      real*4 u1,v1,u2,v2,uv(4),u3,v3,uv3(2)
      equivalence(sc(19),uv),(u1,uv(1)),(v1,uv(2)),(u2,uv(3)),(v2,uv(4))
      equivalence(sc(17),uv3),(u3,uv3(1)),(v3,uv3(2))
      real*8 ttv
      integer*2 mtv(4)
      equivalence (ttv,mtv)
      integer*2 isc12(4)
      equivalence (sc(12),isc12)

      real*8 jbuff(jbuff_size)
      real*8 W0,W1,vec(16),vlen
      integer*4 ncl_put_asn
      integer*4 j,nclkey,srest,vkey
      logical lflg,lv95,have1,fittng,rbsp,subs
      integer*2 inpv,inve,nv,nwds,ietype,jcomp,errfl,itrflg,tinx
      integer*2 i,jj,j0,j1,kt,idst0,nsf,ipfl(3),ind
      integer*2 nve(5)
      data nve /0,1,0,1,1/

      integer*2 conic,cmpos,intof,contrl,contur,PERCNT,ALLV,isca,NUMV
c      integer*2 COMBIN
c
      real*8 ptv(6),pval(6)
c
      parameter (ALLV  = 816)
      parameter (cmpos = 612)
      parameter (conic = 611)
      parameter (contur = 759)
      parameter (intof = 727)
      parameter (projct = 888)
      parameter (PERCNT = 763)
      parameter (NUMV = 563)
c      parameter (COMBIN=1071)

      lv95 = sc(169).lt.9.549d0
      rbsp = isc10(1) .eq. 628
      idst0 = idst
      svtkn = savid2
      svinx = isvsub
      sc(199) = 0
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c                                            ****  CV/CV1,<XVAL>,<YVAL>,<ZVAL>
c                                            ****  SP/CV1,<XVAL>,<YVAL>,<ZVAL>
      if (geom .and. (geotyp.eq.curve .or. geotyp.eq.line .or.
     *                         geotyp.eq.circle)) then
         isc10(2)=3
         if (rbsp) isc10(2) = 16
         sc(11)=tv
         isc10(3)=0
c                                  *** CV/CV1
         if (nxteos) goto 88888
         call parsit
         if (scalar) then
c                                  *** CV/CV1,XVAL,<YVAL>,<ZVAL>
            do 50,i=1,3
                sc(11+i)=tv
                if (nxteos) then
c                             end of statement
                    isc10(3)=i
                    goto 88888
                endif
                call parsit
                if (.not. scalar) then
                   call error(7)
                   goto 99999
                endif
50          continue
            call error(4)
            goto 99999
        endif

c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c                                            ****  CV/COMBIN,sf,pv
      else if (vocab .and. voc .eq. 1071) then
        call parsit
        if (ityp.eq.2.and.ist.eq.SURF) then
          itype = 6
          call gtdesc (tv, nclkey, nwds, itype)
          call sftype (nclkey, isftyp)
          if (isftyp.eq.0.or.isftyp.eq.27.or.isftyp.eq.28) then
            call error (321)
            goto 99999
          endif
          if (NXTEOS) then
            call error (11)
            goto 99999
          endif
          call parsit
          if (ityp.ne.2.or.(ist.ne.4.and.ist.ne.21)) then
            call error (11)
            goto 99999
          endif
          call gtdesc (tv,vkey,nwds,itype)
          call nclf_genpocket_cvs(nclkey,vkey,errfl)
          if (errfl.ne.0) then
            call error(errfl)
            goto 99999
          endif
          defwf = .true.
          isc10(2) = 20
        else
          call error(186)
          goto 99999
        endif
        goto 88888

c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c                                            ****  CV/OFFSET,cv1,XLARGE,.25
c                                            ****  SP/OFFSET,cv1,XLARGE,.25
c
c.....Added option to offset a portion of a composite curve using component
c.....number range - Andrew 1/22/13
c
      else if (vocab .and. voc .eq. offset) then
        isc10(2)=15
        if (rbsp) isc10(2) = 17
        sc(14) = 0
        sc(15) = 0
        isc10(3)=0
        ifl(395) = 0
        ifl(396) = 1
        call parsit
        if (.not. (geom .and. (geotyp.eq.curve .or. geotyp.eq.line .or.
     *                         geotyp.eq.circle))) then
          call error(21)
          goto 99999
        endif
        sc(11) = tv
        tinx = inx
        call parsit
c
c.....Range of components to offset was given
c
        if (scalar) then
          sc(14) = tv
          ifl(395) = 1
          tinx = inx
          call parsit
          if (scalar) then
            sc(15) = tv
            tinx = inx
            call parsit
          endif
c
c.....ALL was given
c
        else if (vocab .and. voc .eq. 816) then
          ifl(395) = 2
          tinx = inx
          call parsit
        endif
c
c.....Use given direction modifier
c
        if (ityp.eq.1.and.ist.gt.637.and.ist.lt.644) then
          sc(12)=0
          isc12(4) = ist
c
c.....Find offset direction modifier based on vector.  The direction will be
c.....based on the largest parameter.  Preference will be given to x-parameter
c.....first, then y-parameter and finally z-parameter - ASF 11/13/13.
c
        else
          itrflg = 2
          inx = tinx
          call gtvect(itrflg,sc(16),errfl)
          if (errfl.ne.0) then
            call error(18)
            goto 99999
          endif
          vlen = sqrt(sc(16)**2+sc(17)**2+sc(18)**2)
          sc(16) = sc(16)/vlen
          sc(17) = sc(17)/vlen
          sc(18) = sc(18)/vlen
          isc12(4) = 604
        endif
        call parsit
        if (scalar) then
          sc(13) = tv
        else
          call error(7)
          goto 99999
        endif
        if (.not.nxteos) then
          call parsit
          if (ityp.eq.1) then
            if (ist.eq.961) then
              ifl(396) = 3
            else if (ist.eq.76) then
              ifl(396) = 1
            else if (ist.eq.1085) then
              ifl(396) = 2
            else
              call error(182)
              goto 99999
            endif
          else
              call error(232)
              goto 99999
          endif
        endif
        goto 88888

c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c                                            ****  CV/PROJCT,cv1,...
c                                            ****  SP/PROJCT,cv1,...
      else if (vocab .and. voc .eq. projct) then
        isc10(2)=18
        if (rbsp) isc10(2) = 19
c
        call prjprs (1,ipfl,ptv,pval,ierr)
        if (ierr .ne. 0) then
          call error (ierr)
          go to 99999
        endif
c
        sc(11) = ptv(1)
c
        sc(12) = 0.
        if (ipfl(1) .eq. 1) then
          sc(12) = ptv(3)
        else if (ipfl(1) .eq. 2) then
          isc12(1) = atangl
          u2 = pval(1)
          v2 = pval(2)
        endif
c
        sc(13) = ptv(2)
        u1 = pval(3)
        v1 = pval(4)
c
        if (ptv(4) .ne. 0.) then
            sc(16) = ptv(4)
            u3 = pval(5)
            v3 = pval(6)
        endif
c
        isc10(3) = ipfl(2)
        isc10(4) = ipfl(3)
        sc(15) = ptv(5)
c
        sc(14) = ptv(6)
        goto 88888

c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c...vp3.22.93 this part is entirely new to support PV as well as pt,ve pairs
c
c                                            ****  CV/CONIC
c
      else if (vocab .and. voc .eq. conic) then
         inpv   = 0
         inve   = 0
         nwm    = 5
         nv     = 0
         j      = 0
         call ncl_free_uv

         do 550 i=1,5
            if (i .gt. nwm) go to 600
            idtype = 0
            call parsit
            ttv  = tv
            if (geom) then
c
c...process PV if acceptable
c
               if (geotyp .eq. pntvec) then
                  if (i .eq. 1) then
                     inpv = i
                     nwm = nwm - 1
                     nv  = nv + 1
                  else if (i .eq. 2) then
                     if (inpv .eq. 1) then
                         mtv(3) = 0
                     else
                         mtv(3) = 3
                         inve = i
                         nv = nv + 1
                     end if
                  else if (i .eq. 3) then
                     if (inpv .eq. 1) then
                         nv = nv + 1
                         nwm = nwm - 1
                     else
                         mtv(3) = 0
                     end if
                  else if (i .eq. 4) then
                     nv = nv + 1
                     if (inpv .eq. 0) then
                         inpv = i
                         nwm = nwm - 1
                     else
                         mtv(3) = 3
                         inve = i
                     end if
                  else
                     mtv(3) = 3
                     inve = i
                     nv = nv + 1
                  end if
c
c...process VE if acceptable
c
               else if (geotyp .eq. VECTOR .and. nve(i) .eq. 1 .and.
     *                   (inve .eq. 0 .or. i .eq. 5)) then
                  inve = i
                  nv  = nv + 1
c
c...must be a point
c
               else if (geotyp .ne. POINT) then
                  call error (20)
                  go to 99999
               end if
c
c... aak 09-jan-98: write to list instead of storing in sc(..)
c... for compartibility with "geogn2.f"
c
ccc  sc(10+i) = ttv
               j = j+1
               jbuff(j) = ttv
               if (j.eq.jbuff_size) j = ncl_put_asn (jbuff,j)
            end if

  550    continue

  600    continue

         if (j.gt.0) j = ncl_put_asn (jbuff,j)

         isc10(4) = nwm
c
c...set type of conic
c
         isc10(3) = 4
         if(nv .eq. 2) then
            isc10(3) = 1
         else if (nv .eq. 1) then
            if (nwm .eq. inve .or. nwm .eq. inpv) then
               isc10(3) = 3
            else
               isc10(3) = 2
            endif
         endif
         isc10(2)=6
         goto 77777
c
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c
c                                    ****  CV/SF1,<EDGE>,<U/V-OFFSET>,<NUM-PTS>
c                                            EDGE OF SURFACE
      else if (geom .and. geotyp .eq. surf) then
        isc10(2)=4
        if (rbsp) isc10(2)=13
        sc(11)=tv
        isc10(3)=1
        isc10(4)=20
        if (rbsp) isc10(4) = 0
        sc(12)=0.
        isc10(13) = 0
c                    if no value given, assume v=0
        if (nxteos) goto 88888

c          get which edge of the surface to use
        call parsit
        if (.not. (ityp .eq. 3 .or.
     x            (ityp .eq. 2 .and. ist .eq. 2))) then
          call error(53)
          goto 99999
        endif
        if (itv.lt.1.or.itv.gt.4) then
          call error(224)
          goto 99999
        endif
        isc10(3)=itv
        if (nxteos) goto 88888

        W0 = 0.0
        W1 = 1.0
c               u or v offset value to hold const (def 0. or 1. dep on <edge>)
        call parsit

        if (vocab .and. voc.eq.PERCNT) then
          isc10(13) = 1
          W1 = 100.0
          call parsit
        endif

        if (ityp.ne.5.and.ist.ne.9) then
          if (.not. scalar) then
            call error(7)
            goto 99999
          endif
          if (tv .lt. W0) tv = W0
          if (tv .gt. W1) tv = W1
          sc(12)=tv
        endif
        if (nxteos) goto 88888
c                        number of points to calculate (def 20)
        call parsit
        if (.not. (ityp .eq. 3 .or.
     x            (ityp .eq. 2 .and. ist .eq. 2))) then
          call error(53)
          goto 99999
        endif
        if (itv .lt. 20 .or. itv .gt. 200) then
          call error (373)
          goto 99999
        endif
        isc10(4)=itv
        goto 77777
c
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      else if (vocab .and. voc .eq. intof) then
        call parsit
c                     ****  CV/INTOF,ALL(COMPOS),sf-list,AT,z(pl1,sf2)[,ZL,0.1]
        if (vocab .and. (voc.eq.cmpos .or. voc.eq.all)) then
          isc10(3) = 0
          isc10(4) = 0
          isc10(2) = 23
          if (voc .eq. cmpos) isc10(2) = 24
          sc(11) = 0
          sc(12) = 0
          sclvar_def = .false.

          i = 0
 1850     idtype = 9
          call parsit
          if (geom .and. geotyp.eq.surf) then
            call gtdesc(tv,nclkey,nwds,ietype)
            call addsky(0,nclkey,1,i)
          else if (geom .and. geotyp.eq.vsolid) then
            call gtdesc(tv,nclkey,nwds,ietype)
            call addsky(0,nclkey,2,i)
          else if (vocab .and. voc.eq.layer) then
            call parsit
            if (.not.(ityp.eq.5 .and. ist.eq.1)) then
              call error(6) ! '=' expected
              call delsky
              goto 99999
            endif
            call parsit
            if (.not.scalar) then
              call error(7) ! scalar expected
              call delsky
              goto 99999
            endif
            j = tv
            call addsky(1,j,1,i)
          endif
          if (vocab .and. voc.eq.at) then
            if (i .gt. 0) then
              call parsit
              if (geom .and. geotyp.eq.plane) then
                isc10(3) = 1
              else if (geom .and. geotyp.eq.surf) then
                isc10(3) = 2
              else if (.not.scalar) then
                call error(326) ! plane or surface expected
                goto 99999
              endif
              sc(11) = tv

              if (nxteos) goto 88888
              call parsit
c
c..... offset for the 'AT' plane or surface
c
              if (isc10(3).gt.0 .and.
     x              (vocab .and. voc.ge.638 .and. voc.le.643)) then
                isc10(4) = voc ! direction modifier
                call parsit
                if (.not.scalar) then
                  call error(326) ! plane or surface expected
                  goto 99999
                endif
                sc(12) = tv ! offset
                if (nxteos) goto 88888
                call parsit
              endif
c
c...Get name of var to store # of pts generated
c
              if (ityp.eq.2.and.(ist.eq.1.or.ist.eq.2)) then
c
c...Scalar (or unknown - make it a scalar)
c
                idst = 2
                savid2 = token2
c...0311
                isvsub = ivxsub
                ifl(9)  = ifl(11)
                ifl(10) = ifl(12)
                rest = 0.
                keyold = keyhld
                istold = ist
                call vstore
c                sc(199) = rtok
                sclvar_lab = token2
                sclvar_inx = ivxsub
                sclvar_def = .true.
                idst = idst0
                savid2 = svtkn
c...0311
                isvsub = svinx
c
c...Simple variable expected
c
              else
                call error (282)
                call delsky
                goto 99999
              endif

              goto 88888
            else
              call error(358) ! value too small
              call delsky
              goto 99999
            endif
          else if (.not.nxteos .and. .not.vocab) then
            goto 1850
          else
            call error(343) ! 'at' or scalar expected
            goto 99999
          endif

        endif
c
c                                            ****  CV/INTOF,SF|PL,SF|PL,PT
c
        isc10(2)=5
        if (rbsp) then
          if (sc(169) .lt. 9.149d0) then
            call error(5)
            goto 99999
          endif
          isc10(2)=14
        endif
        isc10(3)=0
c          surf or plane is only acceptable geometry
        if (.not. (geom .and. (geotyp .eq. plane .or.
     x                         geotyp .eq. surf))) then
          call error(326)
          goto 99999
        endif
        u1 = .5
        v1 = .5
        u2 = .5
        v2 = .5
        nsf = 1
        if (ist.eq.9) then
          sc(11)=tv
          iret = 9
          call parsuv(iret, lflg, u1, v1)
          if (iret.gt.0) then
            call error(iret)
            goto 99999
          endif
          call parsit
c                          1st is surf - 2nd can be surf or plane
          if (.not. (geom .and. (geotyp .eq. plane .or.
     x                           geotyp .eq. surf))) then
            call error(326)
            goto 99999
          endif
          sc(12)=tv
          if (ist.eq.9) then
            nsf = 2
            iret = 9
            call parsuv(iret, lflg, u2, v2)
            if (iret.gt.0) then
              call error(iret)
              goto 99999
            endif
          endif
        else
          sc(12)=tv
          call parsit
c                          1st is plane - 2nd must be surf
          if (.not. (geom .and. geotyp .eq. surf)) then
            call error(186)
            goto 99999
          endif
          sc(11)=tv
          iret = 9
          call parsuv(iret, lflg, u1, v1)
          if (iret.gt.0) then
            call error(iret)
            goto 99999
          endif
        endif

        if (rbsp .and. nsf.eq.1 .and. .not.lv95) then
          isc10(2) = 25
          isc10(3) = 1
          isc10(4) = 4
          sc(13) = 0
          if (lflg) isc10(4) = 2
c                                         ALL
          if (.not.nxteos) then
            call parsit
            if (vocab .and. ist .eq. ALLV) then
                isc10(4) = 3
c                                         get optional near point
            else if (.not.(geom .and.
     x          (geotyp.eq.point .or. geotyp.eq.pntvec))) then
              call error(20)
              goto 99999
            else
              isc10(4) = 1
              sc(13) = tv
            endif
          endif

          i = 0
          call gtdesc(sc(11),nclkey,nwds,ietype)
          call addsky(0,nclkey,1,i)

          sc(11) = sc(12)
          sc(12) = 0
          goto 88888
        endif

        if (nxteos) goto 88888
c                                         get optional near point
        call parsit
        if (.not.(geom .and.
     x      (geotyp.eq.point .or. geotyp.eq.pntvec))) then
          call error(20)
          goto 99999
        endif
        isc10(3)=1
        sc(13)=tv
        goto 77777

c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c                                  ****  CV/COMPOS,LN|CI|CV,...LN|CI|CV
c.....Added option to connect disconnected entities to make connected curve
c.....Andrew 3/13/13
c
      else if (vocab .and. voc .eq.  cmpos) then
        j=0
        ifl(396) = 0
        invoc = .false.
        ranrec=ifl(4) + 1
        isc10(2)=8
        call ncl_free_uv
        do 190 i=1,1000
            call parsit
c               line, circle or curve is only acceptable geometry
            if (geom .and. (geotyp .eq. line .or.
     x                      geotyp .eq. circle .or.
     x                      geotyp .eq. curve)) then
                    j=j+1
                    jbuff(j)=tv
                    if (j.eq.jbuff_size) j = ncl_put_asn (jbuff,j)
            else if (ityp .eq. 1 .and. invoc .eq. .false.) then
                if (i.gt.1) then
                    call error(4)
                    goto 99999
                endif
                if (ist.eq.961) then
                    ifl(396) = 3
                else if (ist.eq.76) then
                    ifl(396) = 1
                else if (ist.eq.1085) then
                    ifl(396) = 2
                else if (ist.eq.172) then
                    ifl(396) = 0
                else
                    call error(182)
                    goto 99999
                endif
                invoc = .true.
            else if (ityp.eq.1) then
                if (i.gt.2) then
                    call error(4)
                    goto 99999
                endif
                if (ist .eq. 831) then
                    ifl(396) = -1*ifl(396)
                else if (ist.ne.50) then
                    call error(182)
                    goto 99999
                endif
            else if (ityp.eq.7.or.(ifl(111).ne.0.and.ityp.eq.5.and.
     1                                               ist .eq. 7)) then
                if (i.lt.2.or.(i.lt.3.and.invoc.eq. .true.)) then
                    call error(34)
                    go to 99999
                endif
c                                                **** flush the buffer
                isc10(3)=i-1
                if (j.gt.0) j = ncl_put_asn (jbuff,j)
                go to 88888
            else
                call error(408)
                goto 99999
            endif
190     continue
        call error(15)
        goto 99999

c                                  ****  CV/PART(CONTUR),d,z,sf-list
      else if (vocab .and. (voc.eq.part .or. voc.eq.contur)) then
        call parsit
        if (.not.scalar) then
          call error(7) ! scalar expected
          goto 99999
        endif
        sc(11) = tv
        if (nxteos) then
          call error(186) ! surface expected
          goto 99999
        endif
        isc10(2) = 21
        i = 0
c
        call parsit
        call sflist (3,0,i,ierr)
        if (ierr .ne. 0) then
            call error (ierr)
            go to 99999
        endif
c
        isc10(3) = 0
        if (i .gt. 0) then
          if (vocab .and. voc.eq.at) then
            call parsit
            if (geom .and. geotyp.eq.plane) then
              isc10(3) = 1
            else if (geom .and. geotyp.eq.surf) then
              isc10(3) = 2
            else if (.not.scalar) then
              call error(326) ! plane or surface expected
              goto 99999
            endif
            sc(12) = tv
            goto 88888
          endif
        else
          call error(358) ! value too small
          call delsky
          goto 99999
        endif

      else if (vocab .and. voc.eq.out) then
        call parsit
        if (geom .and. geotyp.eq.curve) then
c
c..... cv/OUT,ccv,ALL (1, 3,THRU,5) (,NUM,value)
c
          sc(11) = tv
          asn = tv
          isc10(2) = 22
          call gtdesc (tv,nclkey,nwds,ietype)
          call prsout (nclkey,1)
          go to 88888
        else if (ityp .eq. 2 .and. ist .eq. 9) then
c
c...Jingrong 9/23/98
c...SP/out,sf,n
c
          if (.not.rbsp) then
            call error(12)
            goto 99999
          endif
          sc(11) = tv
c
c...SPLINE/out,sf
c...SPLINE/out,sf,n,edge1,CCLW/CLW,edge2
c
          if (nxteos) then
            sc(12) = -1.0
          else
            call parsit
            if (scalar) then
              sc(12) = tv
              if (nxteos) then
                sc(13) = 0.0
                sc(14) = 0.0
                sc(15) = 0.0
              else
                call parsit
                if (scalar) then
                  sc(13) = tv
                else
                  call error(7)
                  go to 99999
                end if
                if (.not.nxteos) then     
                   call parsit
                   if (vocab .and. voc .eq. CLW) then
                      sc(15) = 1
                   else if (vocab .and. voc .eq. CCLW) then   
                      sc(15) = -1
                   else
                     call error(7)
                     go to 99999
                   end if            
                   call parsit 
                   if (scalar) then
                     sc(14) = tv
                   else
                     call error(7)
                     go to 99999
                   end if
                else
                   sc(14) = 0.0
                   sc(15) = 0.0
                endif
              end if
            else
              call error(4)
              goto 99999
            end if
	    endif
          if (.not.nxteos) then
            call error(4)
            go to 99999
          else
            isc10(2) = 12
            go to 88888
          end if
        else
          call error(186)
          go to 99999
        endif

      endif

      subs=.false.
      fittng=.false.
      contrl = 0
      have1=.false.
      numpts=0
      ranrec=ifl(4)+1
      j=0
      i=0

c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c                                            ****  CV/FIT,CV,...,CV
       if (vocab) then
         if (voc .eq. fit) then
           fittng=.true.
c
c...control points in spline definition
c
         else if (voc .eq. 126) then
           contrl = 1
         else
           call error(5)
           goto 99999
         endif

         idtype = 3
         call parsit
         if (err) go to 99999

       endif

       call ncl_free_uv
       do 200 i=1,1000

c              Do not use test for logical named GEOM because it will not be
c              set true for a number indicating a POINT.  e.g. CV/1,thru,5
           if (ityp .eq. 2 .and.
     -         (geotyp .eq. point .or. geotyp .eq. pntvec)) then
               numpts=numpts+1
               if (numpts.gt.maxpt.and..not.(fittng.or.rbsp)) then
                   call error(43)
                   go to 99999
               endif
               have1=.true.
                   j=j+1
                   jbuff(j)=tv
                   if (j.eq.jbuff_size) j = ncl_put_asn (jbuff,j)
c******************************************************* if its a vector
           else if (geom .and. geotyp .eq. vector) then
               if (have1) then
                   have1=.false.
               else
                   call error(16)
                   go to 99999
               endif
               j=j+1
               jbuff(j)=tv
               if (j.eq.jbuff_size) j = ncl_put_asn (jbuff,j)
           else if (ityp.eq.7.or.(ifl(111).eq.1.and.ityp.eq.5.and.
     x            ist.eq.7)) then
               if (numpts.lt.2) then
                   call error(34)
                   go to 99999
               endif
               isc10(3)=i-1
               isc10(2)=1
               if (rbsp) isc10(2)=9
               if (fittng) isc10(2)=isc10(2)+1
               if (rbsp .and. contrl .eq. 1) isc10(2) = 20
c                                                **** flush the buffer
               if (j.gt.0) j = ncl_put_asn (jbuff,j)
               go to 88888
           else
               call error(2)
               go to 99999
           endif
c...                           vp3.16.93 added pv (3 -> 0)
           kt = 3
           if (ist .eq. pntvec) kt = 0
           idtype = kt
           call parsit
           if (err) go to 99999
200    continue
       call error(15)
       go to 99999
77777  if (.not.nxteos) then
         call error(4)
         goto 99999
       endif
88888  continue
       isc10(1)=606
99999  return
       end
c
c***********************************************************************
c
c   SUBROUTINE:  prjprs (ktyp,kpfl,gtv,gval,kerr)
c
c   FUNCTION:  This is the controlling routine for creating an APT
c              source file.
c
c   INPUT:  ktyp    I*2  D1  -  1 = Curve projection, 2 = Pattern projection,
c                               3 = Annotation projection.
c
c   OUTPUT: kpfl    I*2  D3  -  (1) - 0 = Standard, 1 = Vector, 2 = Atangle.
c                               (2) - 0 = Nowrap, 1 = Wrap, 2 = Revolve,
c                                     3 = Radial.
c                               (3) - 1 = Start, 2 = Middle, 3 = End,
c                                     4 = Center, 5 = At nearpt.
c           gtv     R*8  D6  -  (1) = Curve/Pattern to project.
c                               (2) = Surface to project to.
c                               (3) = Projection vector.
c                               (4) = Secondary (controlling) surface.
c                               (5) = Projection start point.
c                               (6) = Near point.
c
c           gval    R*8  D6  -  (1) = Starting angle.
c                               (2) = Ending angle.
c                               (3) = Surface U-value.
c                               (4) = Surface V-value.
c                               (5) = Secondary surface U-value.
c                               (6) = Secondary surface V-value.
c
c           kerr    I*2  D1  -  Returns Non-zero on error.
c
c***********************************************************************
c
      subroutine prjprs (ktyp,kpfl,gtv,gval,kerr)
c
      include 'com8a.com'
c
      integer*2 ktyp,kerr,kpfl(3)
c
      real*8 gtv(6),gval(6)
c
      integer*2 wrap,nowrap,start,middle,vocend,center,at,vocrev,
     2          radial,atangl,iret
c
      parameter (at = 189)
      parameter (atangl = 1)
      parameter (center = 634)
      parameter (middle = 891)
      parameter (nowrap = 890)
      parameter (vocrev = 861)
      parameter (radial = 893)
      parameter (start = 57)
      parameter (vocend = 499)
      parameter (wrap = 889)
c
c...Initialize routine
c
      kpfl(1) = 0
      kpfl(2) = 0
      kpfl(3) = 1
c
      gtv(1) = 0.
      gtv(2) = 0.
      gtv(3) = 0.
      gtv(4) = 0.
      gtv(5) = 0.
      gtv(6) = 0.
c
      gval(1) = 0.
      gval(2) = 0.
      gval(3) = .5
      gval(4) = .5
      gval(5) = .5
      gval(6) = .5
c
      kerr   = 0
      iret   = 9
c
c...PROJCT,geo
c
      if (ktyp .ne. 3) then
          call parsit
          if (geom) then
              if ((ktyp .eq. 1 .and. geotyp .ne. curve .and.
     1            geotyp .ne. line .and. geotyp .ne. circle) .or.
     2            (ktyp .eq. 2 .and. geotyp .ne. patern)) go to 9000
          else
              goto 9000
          endif
          gtv(1) = tv
      endif
c
c...PROJCT,vector
c
      call svpars
      call parsit
      if (ityp .eq. 2 .and. (ist .eq. vector .or. ist .eq. pntvec)) then
          kpfl(1) = 1
          gtv(3) = tv
          call svpars
          call parsit
c
c...PROJCT,ATANGL
c
      else if (ityp .eq. 1 .and. ist .eq. atangl .and. ktyp .ne. 3) then
          kpfl(1) = 2
          call svpars
          call parsit
          if (.not. scalar) go to 9010
          gval(1) = tv
          gval(2) = tv
          call svpars
          call parsit
          if (scalar) then
              gval(2) = tv
              call svpars
              call parsit
          endif
      endif
c
c...PROJCT,[ATANGL,]sf1
c
      if (ityp .ne. 2 .or. (ist .ne. surf .and. ist .ne. plane))
     1        go to 9020
      gtv(2) = tv
      gtv(4) = tv
      call parsuv (iret,lflg,gval(3),gval(4))
      gval(5) = gval(3)
      gval(6) = gval(4)
      if (nxteos) goto 8000
c
c...PROJCT,sf2
c
      call svpars
      call parsit
      if (ityp .eq. 2 .and. (ist .eq. surf .or. ist .eq. plane) .and.
     1    kpfl(1) .eq. 2) then
          gtv(2) = tv
          call parsuv (iret,lflg,gval(3),gval(4))
          if (nxteos) goto 8000
          call svpars
          call parsit
       endif
c
c...PROJCT,NOWRAP-WRAP-REVOLV-RADIAL
c
      if (ityp .eq. 1 .and. (ist .eq. wrap .or. ist .eq. nowrap .or.
     1    ist .eq. radial .or. ist .eq. vocrev)) then
          if (ist .eq. wrap) kpfl(2) = 1
          if (ist .eq. vocrev) kpfl(2) = 2
          if (ist .eq. radial) kpfl(2) = 3
          if (kpfl(1) .eq. 2 .and. kpfl(2) .ne. 0) go to 9050
          if (nxteos) goto 8000
          call svpars
          call parsit
        endif
c
c...PROJCT,START-MIDDLE-END-CENTER-AT
c
      if (ityp .eq. 1 .and. (ist .eq. start .or. ist .eq. middle .or.
     1    ist .eq. vocend .or. ist .eq. center .or. ist .eq. at)) then
          if (ist .eq. start) kpfl(3) = 1
          if (ist .eq. middle) kpfl(3) = 2
          if (ist .eq. vocend) kpfl(3) = 3
          if (ist .eq. center) kpfl(3) = 4
c
c...PROJCT,AT,pt
c
          if (ist .eq. at) then
              kpfl(3) = 5
              call svpars
              call parsit
              if ((ityp .eq. 2) .and. (ist .eq. point .or.
     1            ist .eq. pntvec)) then
                  gtv(5) = tv
              else
                  go to 9030
              endif
          endif
          if (kpfl(1) .eq. 2 .and. kpfl(3) .ne. 1) go to 9050
          if (nxteos) goto 8000
          call svpars
          call parsit
        endif
c
c...PROJCT,nearpt
c
      if ((ityp .eq. 2) .and. (ist .eq. point .or. ist .eq. pntvec))
     1    gtv(6) = tv
c
c...Expected end of command
c
      if (.not. nxteos .and. ktyp .ne. 3) go to 9040
      if (ktyp .eq. 3) call rtpars
c
c...End of routine
c
 8000 return
c
c...Invalid geometry type
c
 9000 kerr   = 21
      if (ktyp .eq. 2) kerr = 342
      go to 8000
c
c...Scalar expected
c
 9010 kerr   = 7
      go to 8000
c
c...Surface expected
c
 9020 kerr   = 186
      go to 8000
c
c...Point expected
c
 9030 kerr   = 20
      go to 8000
c
c...End of command expected
c
 9040 kerr   = 4
      go to 8000
c
c...Statement not yet supported
c
 9050 kerr   = 5
      go to 8000
      end
