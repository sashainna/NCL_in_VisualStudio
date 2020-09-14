C*********************************************************************
C*    NAME         :  declss.f
C*       CONTAINS:
C*    COPYRIGHT 1997 (c) INTERSOFT Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       declss.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 17:20:12
C*********************************************************************
c
c*********************************************************************
c    e_subroutine     : subroutine declss
c      This routine parses spline on surface declaration. 
c      The valid syntax constructs are:
c
c          1.  (name =) ssplin/sf1,<edge>,<num-pts>,<u/v-offset>
c
c          2.  (name =) ssplin/intof,sf1,sf2,[pt1]
c              (name =) ssplin/intof,sf1,pl1,[pt1]
c          
c          3.  (name =) ssplin/compos,ss1,ss2, ..., ssn
c
c			  4.  (name =) sspline/out,sf,n1,n2
c
c      NOTE: We do not restrict number of points to evolve curve.
c
c    the following represents the info passed to geogss (via geogn2) in sc:
c             sc(10)             !       sc(11) thru sc(nn)
c--------------------------------!----------------------------------------
c type  subtype  numwds  notused !   sc(11)      sc(12)      sc(13)   
c ----  -------  ------  ------- !   ------      ------      ------  
c  619     1   edge      npts    !  sf1 loc      u/v offset
c  619     2   nr pt flg   *     !  sf1 loc    sf2/pl1 loc  nr pt loc
c  619     12    n/a             !  sf loc     curve key     sub crvid
c*************************************************************************

      subroutine declss

      include 'com8a.com'

      parameter (maxpt=50)

      real*8 jbuff(35)
      real*4 u1,v1,u2,v2,uv(4)
      equivalence(sc(19),uv),(u1,uv(1)),(v1,uv(2)),(u2,uv(3)),(v2,uv(4))
      integer*2 isc12(4)
      equivalence (sc(12),isc12)

      logical lflg,  rbsp
      integer*2 nwds, itype,i,ipfl(3)
      integer*4 nclkey, j, ncl_put_asn, cvtype1, status, cvkey, nents,
     1          i4, inc, subkey, newkey
c
      real*8 ptv(6),pval(6)
c
      integer*2 intof, cmpos, ssplin, revol1, radial
      parameter (intof = 727, cmpos = 612, ssplin = 619)

      integer*2 wrap,nowrap,start,middle,end,center,at,projct,atangl

      parameter (projct = 888,wrap = 889,nowrap = 890,revol1=861)
      parameter (start = 57, middle = 891, end = 499)
      parameter (center = 634, at = 189, atangl = 1, radial = 893)
      parameter (CLW = 60, CCLW = 59)


c
c...CV/SF1,<EDGE>,<U/V-OFFSET>
c......EDGE OF SURFACE
c
      if (geom .and. geotyp .eq. surf) then
        call gtdesc(TV,nclkey,nwds,itype)
        call sftype (nclkey,itype)
        if (itype .eq. 27 .or. itype .eq. 99) go to 9100
        isc10(2) = 4
        sc(11)   = tv
        isc10(3) = 1
        isc10(4) = 20
        sc(12)   = 0.
c
c...if no value given, assume v=0
c
        if (nxteos) goto 88888
c
c...get which edge of the surface to use
c
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
        isc10(3) = itv
        if (NXTEOS) goto 88888
c
c...u or v offset value to hold const (def 0. or 1. dep on <edge>)
c
        call parsit
        if (ityp.ne.5.and.ist.ne.9) then
          if (.not. scalar) then
            call error(7)
            goto 99999
          endif
          if (tv .lt. 0.d0 .or. tv .gt. 1.d0) go to 9200 
          sc(12) = tv
        endif
        goto 77777
c
c...CV/INTOF,SF|PL,SF|PL,PT
c 
      else if (vocab .and. voc .eq. intof) then
        isc10(2) = 5
        isc10(3) = 0
        call parsit
c
c...surf or plane is only acceptable geometry
c
        if (.not. (geom .and. (geotyp .eq. plane .or.
     x                         geotyp .eq. surf))) then
          call error(326)
          goto 99999
        endif
        u1 = .5
        v1 = .5
        u2 = .5
        v2 = .5
        if (ist.eq.9) then
          call gtdesc(TV,nclkey,nwds,itype)
          call sftype (nclkey,itype)
c
c...net surface not supported yet
c
          if (itype .eq. 27) goto 9100
          sc(11) = TV
          iret = 9
          call parsuv(iret, lflg, u1, v1)
          if (iret.gt.0) then
            call error(iret)
            goto 99999
          endif
          call parsit
c
c...1st is surf - 2nd can be surf or plane
c
          if (.not. (geom .and. (geotyp .eq. plane .or.
     x                           geotyp .eq. surf))) then
            call error(326)
            goto 99999
          endif
          sc(12) = TV
          if (ist.eq.9) then
            iret = 9
            call parsuv(iret, lflg, u2, v2)
            if (iret.gt.0) then
              call error(iret)
              goto 99999
            endif
          endif
        else
          sc(12) = TV
          call parsit
c
c...1st is plane - 2nd must be surf
c
          if (.not. (geom .and. geotyp .eq. surf)) then
            call error(186)
            goto 99999
          endif
          call gtdesc(TV,nclkey,nwds,itype)
          call sftype (nclkey,itype)
c
c...net surface not supported yet
c
          if (itype .eq. 27) goto 9100
          sc(11) = TV
          iret = 9
          call parsuv(iret, lflg, u1, v1)
          if (iret.gt.0) then
            call error(iret)
            goto 99999
          endif
        endif
        if (nxteos) goto 88888
c
c...get optional near point
c
        call parsit
        if (.not.(geom .and. (geotyp.eq.point .or. geotyp.eq.21))) then
          call error(20)
          goto 99999
        endif
        isc10(3) = 1
        sc(13) = TV
        goto 77777
c
c... aak 02-dec-1997: 
c
c... ssx = SSPLIN/COMPOS,ss1,ss2, ..., ssn
c... only CVonSF are allowed in this command
c
      else if (vocab .and. voc .eq. cmpos) then
        j = 0
        isc10(2) = 8
        isffl = 0

        do 190 i = 1, 1000
          call parsit
c
c......SSPLIN/COMPOS,ssp1
c
          if(geom.and.geotyp.eq.curve) then
            call gtdesc(TV,nclkey,nwds,itype)
            status = cvtype1(nclkey,itype)
c
c.........if not SSPLIN, give ERR409: "Composite curve failed to be built"
c
            if(itype.ne.13) then
               call error(409)
               return
            endif

            j = j+1
            jbuff(j) = tv
            if (j.eq.35) j = ncl_put_asn (jbuff,j)
c
c......SSPLIN/COMPOS,sf1
c
          else if (geom .and. geotyp .eq. surf) then
            if (sffl .eq. 1) then
               call error (7)
               return
            endif
            call gtdesc(TV,nclkey,nwds,itype)
            status = sftype(nclkey,itype)
c
c.........Get boundary curve
c
            if (itype .eq. 99) then
                isffl = 1
                idir  = 0
                i4    = 0
                call nclf_get_bnkey (nclkey,i4,cvkey,nents)
                if (nents .eq. 0) then
                  call error (163)
                  return
                endif
            else
               call error (557)
               return
            endif
c
c......SSPLIN/COMPOS,sf1,m
c
          else if (((ityp.eq.2 .and. ist.eq.2) .or. ityp.eq.3) .and.
     1             isffl .ne. 0) then
            n = tv
            istrt = n
            if (idir .ne. 0) istrt = ipts
            inc = istrt
            do 150 k=1,nents,1
              inc = inc + idir
              if (inc .eq. 0) inc = nents
              if (inc .gt. nents) inc = 1
              ierr = 0
              call gtcccv (cvkey,inc,subkey,ierr)
              if (ierr .ne. 0) then
                call error (163)
                return
              endif
              subkey = iabs(subkey)
c
c.........Boundary curves are stored as standard B-splines
c.........These must be converted to a Surface-spline
c.........Prior to adding it to the composite curve
c
              call nclf_cp_struct_rbcv_uvcv (subkey,nclkey,newkey)

              j = j+1
              call ptdesc (newkey,curve,tv)
              jbuff(j) = tv
              if (j.eq.35) j = ncl_put_asn (jbuff,j)
              isffl = isffl + 1
              if (inc .eq. n) go to 160
  150       continue
  160       idir = 0
            ipts = n
c
c......SSPLIN/COMPOS,sf1,m,CLW
c......                    CCLW
c
          else if (ityp.eq.1) then
            if (isffl .ne. 2) then
              call error (53)
              return
            endif
c
            if (ist .eq. 60) then
              idir = 1
            else if (ist .eq. 59) then
              idir = -1
            else
              call error (316)
              return
            endif
c
c......End of command
c
          else if (ityp.eq.7.or.(ifl(111).ne.0.and.ityp.eq.5.and.
     *             ist .eq. 7)) then
            if (i.lt.2) then
                call error(34)
                return
            endif
c
c... flush the buffer
c
            isc10(3) = i-1
            if (j.gt.0) j = ncl_put_asn (jbuff,j)
            isc10(1) = 606
            return
          else
            call error(409)
            return
          endif

190     continue
	
c                                            ****  SSPLIN/PROJCT,cv1,...
      else if (vocab .and. voc .eq. projct) then
        isc10(2)=19
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
      endif

c...jingrong 9/18/98
c
c...SSPLIN/out,sf,n1,n2
c
		rbsp = isc10(1) .eq. ssplin
          if (vocab .and. voc .eq. 653) then
           if (.not.rbsp) then
             call error(12)
             go to 99999
           end if
           call parsit
           if (ityp .eq. 2 .and. ist .eq. 9) then
             sc(11) = tv
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
                     goto 99999
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
             endif
           else
             call error(186)
             go to 99999
           end if
c
c.....SSPLIN/OFFSET,cv1,XLARGE,.25  
c
          else if (vocab .and. voc .eq. 666) then
           isc10(2)=15
           if (rbsp) isc10(2) = 17
           isc10(3)=0
           call parsit
          if (.not.(geom .and. (geotyp.eq.curve .or. geotyp.eq.line .or.
     *                         geotyp.eq.circle))) then
            call error(21)
            goto 99999
           endif
           sc(11) = tv
           call parsit
           if (ityp.eq.1.and.ist.gt.637.and.ist.lt.644) then
            sc(12)=0
            isc12(4) = ist
           else
            call error(18)
            goto 99999
           endif
          call parsit
          if (scalar) then
            sc(13) = tv
            goto 88888
          else
            call error(7)
            goto 99999
          endif
         endif
        
77777 if (.not.NXTEOS) then
         call error(4)
         goto 99999
      endif
88888 continue
      isc10(1) = 619
      go to 99999
c
c...errors
c
 9100 call error(321)
      return
 9200 call error(445)
 
99999 return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine geogss
C*       prepare and store ssplin in UNibase.
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
      subroutine geogss

      include 'com8a.com'

      integer*2 isc(640), ietype,ierr
      integer*4 npt, nclkey,key,numpck,ibnum,ienum,idir,ncl_ssonsf1
      real*8 asf,bsf
      integer*2 ksn(4), isub
      equivalence (asn,ksn)
      equivalence (sc,isc), (isub,ksn(4))


      asn    = sc(10)
      ksn2   = ksn(2)

      if (ksn(1).ne.619) goto 99


c
c...EDGE curve
c
      if (ksn2.eq.4) then
        call ssedge (sc(11),ksn(3),sc(12))
        if (ifl(2).lt.1) goto 99
        goto 981
      endif

c...jingrong 9/18/98
c
c...SSPLINE out
c
      if (ksn2.eq.12) then
        call gtdesc (sc(11),nclkey,nwds,ietype)
        numpck = sc(12)
        ibnum = sc(13)
        ienum = sc(14)
        idir = sc(15)
        if(numpck .ne. -1) then
          ierr = ncl_ssonsf1(nclkey,numpck,key,ietype,ibnum,ienum,idir)
        else
          ierr = ncl_sson_revsf (nclkey,key,ietype)
	  endif
        if (ierr.eq.0) then
          call ptdesc(key, ietype, tv)
          rest = tv
          lfl(73) = .true.
          goto 99
        else
          ifl(2) = 163
          goto 981
        endif
      endif

c
c...INTOF surfaces curve
c...generate intersection points first
c
      if (ksn2.eq.5) then
        asf = sc(11)
        bsf = sc(12)
        if (sc(169) .lt. 9.149d0) then
          call cvintx(2)
        else
          call cvio(2)
        endif
        if (ifl(2).ge.1) then
          if (ifl(2).ne.410.and.ifl(2).ne.385) ifl(2) = 163
          goto 981
        endif
        npt = isc10(3)
c
c...Build curve using intersection points
c...This is rbspline curve on surface
c
        call ssintof (asf,npt)
        if (ifl(2) .eq. 0) goto 99 
        goto 981
      endif
c
c...PROJCT curve on surface
c
      if (ksn2.eq.19) then
        call projcv(2)
        if (ifl(2) .eq. 0) goto 99 
      endif
c
c...SSPLIN/Offset
c
      if (ksn2.eq.17) then
        call offcv(2)
        if (ifl(2) .eq. 0) goto 99 
      endif
      
981   err = .true.

99    return
      end
C***************************************************************************
C*    E_SUBROUTINE     : subroutine ssedge (gsf,kedg,goff)
C*       Build edge on the surface to create uv curve. Set data for
C*       rbspline corresponding to edge data
C*    PARAMETERS
C*       INPUT  :
C*          gsf    - base surface id word
C*          kedg   - edge selector (1,2,3,4)
C*          goff   - offset of the uv curve from edge 
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
c****************************************************************************
      subroutine ssedge (gsf,kedg,goff)
c
      include 'com8a.com'
c
      real*8 gsf,goff
      integer*2 kedg
c
      real*8 pts(3,2),x,one,zero,s(4) 
      integer*4 ix,nclkey,key,ncl_create_ssplin
      integer*2 nwds,itype,ierr
c
      data one  /1.d0/
      data zero /0.d0/
c
c...set u,v points
c
      pts(3,1) = zero 
      pts(3,2) = zero
      x = goff
      if (kedg .lt. 3) then
         ix = 2
         if (kedg .eq. 2) x = one - x
      else
         ix = 1
         if (kedg .eq. 4) x = one - x
      end if
c
c...start and end points of u/v line
c
      pts(ix,1) = x
      pts(3-ix,1) = zero 
      pts(ix,2) = x
      pts(3-ix,2) = one 
c
c...parametrization of line
c
      s(1) = zero
      s(2) = zero
      s(3) = one
      s(4) = one
c
c...get base surface key and
c...store uv curve in unibase
c
      call gtdesc(gsf,nclkey,nwds,itype)
      ix = 2
      ierr = ncl_create_ssplin (nclkey,s,pts,ix,key)
      if (ierr .eq. 0) then
         call ptdesc (key,8,TV)
         REST = TV
      else
         ifl(2) = ierr
      end if
c
      return
      end 

c****************************************************************************
C*    E_SUBROUTINE     : subroutine ssintof (gsf,knum)
C*       Build uv curve on surface using uv points generated on intersection
C*       curve.  
C*    PARAMETERS
C*       INPUT  :
C*          gsf      - base surface id word
C*          knum     - number of points generated for curve definition
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
c****************************************************************************
      subroutine ssintof (gsf,knum)
 
      include 'com8a.com'
 
      real*8 gsf
      integer*4 knum
 
      integer*4 bskey,ncl_intof_def,key
      integer*2 nwds,itype,ierr
c
c...get base surface key and interpolate bspline curve 
c
      bskey = 0

      call gtdesc(gsf,bskey,nwds,itype)

      ierr = ncl_intof_def (bskey,1,knum,key)
c
c...get id word of created curve
c
      if (ierr .eq. 0) then
         call ptdesc (key,8,TV)
         REST = TV
      else
         ifl(2) = ierr
      endif
 
      return
      end 

