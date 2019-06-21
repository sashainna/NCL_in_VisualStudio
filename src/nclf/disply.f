C*********************************************************************
C*    NAME         :  disply.f
C*       CONTAINS: disply and disply2
C*     Handles the DISPLY/... statement.
C*
C*    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       disply.f , 25.1
C*     DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:55
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine disply
C*     Handles the DISPLY/... statement.
C*
C*    A DISPLY/... statement will be parsed and two arrays loaded with
C*    data that defines what entities are to be displayed.  The two
C*    arrays are: GEONAM and TYPGEO.  Their contents are:
C*
C*      For a single geometry name:
C*        The GEONAM entry will contain the ASW of the geometry entity.
C*        The TYPGEO will contain a value of 1.
C*
C*      For a single PLANE ID followed by a token 'AT' with a POINT ID
C*      after that:
C*        The GEONAM entry will contain the ASW of the PLANE entity.
C*        The following GEONAM entry will contain the ASW of the POINT
C*        entity.
C*        The TYPGEO will contain a value of 2.
C*
C*      For a single CIRCLE or CURVE ID followed by an 'S' scalar value:
C*        The GEONAM entry will contain the ASW of the CURVE entity.
C*        The following GEONAM entry will contain the 'S' value.
C*        The TYPGEO will contain a value of 3.
C*
C*      For a single SURF ID followed by 'U' and 'V' scalar values
C*      or a single NET SURF ID followed or not by 'U' and 'V' values:
C*        The GEONAM entry will contain the ASW of the SURF entity.
C*        The following GEONAM entry will contain the 'U' value.
C*        The next following GEONAM entry will contain the 'V' value.
C*        The TYPGEO will contain a value of 4.
C*        If it is a single NET SURF ID without following 'U' and 'V'
C*        values, 'U' and 'V' values of zero will be used.
C*
C*      For a single MATRIX ID followed by 'axis' and 'box' scalar values
C*        The GEONAM[igeo:igeo+2] entry will contain the ASW, 'axis' &
C*        'box' (R*8 + 4*R*4) values.
C*        The TYPGEO will contain a value of 5.
C*
C*      For a class of geometry: (PT, LN, CI, etc.)
C*        The GEONAM entry will contain the vocabulary number of the
C*        word indicating the class of geometry.
C*        The TYPGEO will contain a value of 11.
C*
C*      For a CIRCLE or CURVE class of geometry followed by a 'S' scalar value:
C*        The GEONAM entry will contain the vocabulary number 607 (CIRCLE)
C*        or the vocabulary number 608 (CURVE).
C*        The following GEONAM entry will contain the 'S' value.
C*        The TYPGEO will contain a value of 13.
C*
C*      For a SURF class of geometry followed by 'U' and 'V' scalar values:
C*        The GEONAM entry will contain the vocabulary number 609. (SURF)
C*        The following GEONAM entry will contain the 'U' value.
C*        The next following GEONAM entry will contain the 'V' value.
C*        The TYPGEO will contain a value of 14.
C*
C*      For a MATRIX class followed by 'axis' and 'box' scalar values:
C*        The GEONAM[igeo:igeo+2] entry will contain the 610, 'axis' &
C*        'box' (R*8 + 4*R*4) values.
C*        The TYPGEO will contain a value of 15.
C*
C*      For a wildcard geometry name: (ABC*)
C*        The GEONAM entry will contain the character string portion of the
C*        geometry wildcard name.
C*        The TYPGEO will contain a value of 99 + the length of the string.
C*
C*      For an 'ALL' entry:
C*        The GEONAM entry will not contain anything significant.
C*        The TYPGEO will contain a value of 31.
C*
C*      The end of the TYPGEO entries is noted by an entry containing the
C*      value 99.
C*
C*
C*    After the DISPLY/... statement has passed all syntax checking, the
C*    requested entities will be displayed.  The GEONAM and TYPGEO arrays
C*    are processed to get the names, etc. of the entities to display.
C*
C*    All entities are 'VISIBLED' rather than actually redisplayed (drawn)
C*    except for PLANEs with 'AT POINT's, CURVEs with 'S' segment numbers
C*    and SURFs with 'U' & 'V' segment numbers.  These are redisplayed
C*    using the values given for POINT, S, U & V respectively.
C*
C*
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine disply

      include 'com4a.com'
      include 'comdb.com'

      integer*2 nwds, ietype, gclass, i
      integer*4 ipg, iel,ginx,inum,igeo
      integer*2 gtype, styp, ngeo, ins
      integer*4 nclkey,jpd(4),igeonm(2),isub,kwds,itmp(4),plsub
c
c...gname save the geo name (not label, but type, such as 'LN', 'PT' when typgeo=11
c...or geo ID when typgeo=1
c
      real*8 asw, gname,origin(3),tmpnam
      real*4 rgeonm(2),rpd(4)
      character*1 atname(64), awname(64)
      character*64 wname,ttname
      character*8 cvs(35), name
      character*64 pltok, sftok, mxtok
      equivalence (gname, name), (gname, igeonm, rgeonm)
      equivalence (ttname, atname), (wname, awname)
      equivalence (cvs, jb)

      logical planid,curvid,surfid,nietsf,lgeo,mtrxid,mgeo,lredraw
      integer*2  nname, idum

      integer*2  at, all, geo, redraw
      parameter (at  = 189)
      parameter (all  = 816)
      parameter (geo  = 835)
      parameter (redraw = 924)

c
C...Adding integers layer1 and layer2 for the command
C...DISPLAY/LAYER, these will be the upper and lower
C...boundries for the layers that will be displayed.
C...Integer temp is a temporary holding place incase
C...layer1 is greater than layer2.
C
      integer*2 layer1,layer2, temp, ilyrfl

      planid = .false.
      curvid = .false.
      surfid = .false.
      nietsf = .false.
      mtrxid = .false.
      igeo = 0
      ifl(44)=9
      ilyrfl = 0
      lredraw = .false.

c**********************************************************************
c
c    Syntax check the DISPLY statement first.
c    Build the GEONAM and TYPGEO tables.
c
c**********************************************************************

c
c..... look ahead for REDRAW, then reset the position
c
      call svpars
20    idtype = 0
      wdtype = -1
      call parsit
      if (vocab .and. voc.eq.redraw) then
        lredraw = .true.
        goto 25
      endif
      if (.not.nxteos) goto 20
25    call rtpars
C
C...Get the first entity to process
C
      idtype = 0
      call parsit
C
c...Set 'idst' to 'ist' (geometry type)
c...Just in case we store the display geometry
c...For example, pl,AT,pt or sf,u,v
c...'idst' is used in 'vstore'.
c
      idst   = ist

C
C...      Don't process the following:
C...         DISPLY/CUTTER
C...         DISPLY/AXIS
C...         DISPLY/ON
C...         DISPLY/OFF
C...         DISPLY/AUTO
C...         DISPLY/OPTION
C...         DISPLY/TOOL
C

      if (vocab .and.
     x    voc .eq. 716 .or.
     x    voc .eq. 132 .or.
     x    voc .eq.  71 .or.
     x    voc .eq.  72 .or.
     x    voc .eq.  88 .or.
     x    voc .eq. 144 .or.
     x    voc .eq. 617) goto 99999
C                                     ALL
      if (vocab .and. voc.eq.all) then
        if (nxteos) then
          call blankg (3, nclkey)
          goto 99999
        endif
        call error (4)
        goto 99999
      endif

C
C...If voc is equal to 902 then we wish to display a
C...certain set of layers.  Thus we will call display2
C...which will get all geometry and filter out those that
C...are not in the given layers.
C

      if (voc.ne.902) goto 55
45    continue
      call parsit
C
C...IF ityp is not equal to 3 then it was not an
C...integer and an error will be called.
C
      if (ityp.ne.3) then
        if ((ityp.eq.7).and.(ilyrfl.ne.0)) goto 99999
        call error(53)
        goto 99999
      else
C
C...ilyrlf is used to indicate that at least one layer
C...has been displayed.
C
        ilyrfl = 1
        layer1=tv
        isvinx = inx
        idtype = -1
50      call parsit
C
C...If this token is THRU, get the ending parameter of the THRU
C...If not, disply the the layer in layer1.  JLS 5/6/99
C
        if (ist.eq.152) then
           call parsit
           if (ityp.eq.3) then
             layer2 = tv
           else
             call error(431)
             goto 99999
           endif
        else
C
C...We are displaying only one layer this time thru, restore
C...inx so that when parsit is called again, the correct token
C...will be returned.
C
          inx = isvinx
          layer2 = layer1
        endif
      endif
C
C...If layer1 is greater than layer2, then
C...we need to make layer1 the lower range and
C...make layer2 the upper range.
C
      if (layer1.gt.layer2) then
         temp=layer2
         layer2=layer1
         layer1=temp
      endif
      call disply2(layer1,layer2,lredraw)
C
C...If there are more layers to be displayed, go back
C...and display them, if not exit disply routine
C
      if (ityp.ne.7) goto 45
      goto 99999

C
C...If voc is equal to 1096, then the user would like to display
C...the work axes. Hence we will call umf_drw_cpl_axis which is the
C...buffer function that calls um_drw_cpl_axis.
C...Forgot to put a check in to make sure that the program wasn't
C...running in batch.  Addid that for both work axis and model axis.
C...JLS 2/9/99
C
55    continue
      if (voc.eq.1096) then
        if(ifl(35).eq.0) call umf_drw_cpl_axis
        goto 99999
      endif
C
C...If voc is equal to 870, then the user would like to display
C...the model axes. Thus we will call umf_drw_mod_axis, which is the
C...buffer function that will call um_drw_mod_axis to turn on the display.
C
      if (voc.eq.870) then
        if(ifl(35).eq.0) call umf_drw_mod_axis
        goto 99999
      endif
      goto 101

C
C...Get the next entity to process
C
  100 idtype = 0
      call parsit
101   igeo = igeo + 1
C
C...If an error other than # 88, 85, 61 or 9 and a wild card token
C...was found, exit the routine.
C
      if (err .and.
     x       .not. (ifl(2) .eq. 88 .or.
     x              ifl(2) .eq. 85 .or.
     x              ifl(2) .eq. 61)) then
          goto 99999
      else
          err=.false.
          ifl(2)=0
      endif

C
C...Turn off the NET SURF type flag unless the next thing is a
C...'U' scalar value.
C
      if (.not. scalar) then
        nietsf = .false.
        curvid = .false.
        surfid = .false.
        mtrxid = .false.
        call vctoid (voc,ist1,idum)
        call ifidge (ist1,mgeo)
      endif

C                                                   PARSE GEO NAME

C
C...Check if it is a geometry name
C
      if (geom) then
C
C...Check if previous geometry was a PLANE ID with 'AT' following
C...and this is a POINT ID.
C
          gname = tv
          gtype = 1
          if (igeo .gt. 1 ) then
             inum = igeo - 1
             call gtskg(inum, tmpnam, styp)
          endif
          if (igeo .gt. 1 .and. styp .eq. 2) then
              if (geotyp .ne. point) then
                  call error(20)
                  goto 99999
              endif
              call addskg(gname, gtype, ginx)
          else if (geotyp .eq. plane) then
              planid = .true.
              pltok = token2
              plsub = ivxsub
              call addskg(gname, gtype, ginx)
              goto 400
          else if (geotyp .eq. circle .or. geotyp .eq. curve) then
              curvid = .true.
              call addskg(gname, gtype, ginx)
              goto 400
          else if (geotyp .eq. matrix) then
              mtrxid = .true.
              mxtok  = token2
              call addskg(gname, gtype, ginx)
              goto 400
          else if (geotyp .eq. surf) then
              surfid = .true.
              sftok = token2
              call gtdesc (tv, nclkey, nwds, ietype)
              call sftype (nclkey, isftyp)

C
C...Set 'U' and 'V' values to zero for NET SURF.  These may get
C...set to values provided later in the statement.
C
              if (isftyp .eq. NETSF) then
                  nietsf = .true.
                  gtype = 4
                  call addskg(gname, gtype, ginx)
                  gname = 0
                  call addskg(gname, gtype, ginx)
                  call addskg(gname, gtype, ginx)
                  igeo = igeo + 2
              else
                  call addskg(gname, gtype, ginx)
              endif
              goto 400
          else 
              call addskg(gname, gtype, ginx)          
          endif
c
c...SYMBOL
c
      else if (vocab .and. voc .eq. 877) then
          gname = voc
          gtype = 17
          call addskg(gname, gtype, ginx)
C
C...Check if generic geometry type (PT, VE, LN, PL, CI, CV, SF, PN, SH)
C
      else if (vocab .and. mgeo) then
          gname = voc
          gtype = 11
          call addskg(gname, gtype, ginx)
c
c...vp 3/24/98 for now CI or CV (any type) is a generic curve
c...for specific selection user can use geo filter in GUI
c
          if (ist1 .eq. 7 .or. ist1 .eq. 8) then
              curvid = .true.
              goto 400
          else if (voc .eq. 610) then
              mtrxid = .true.
              goto 400
          else if (voc .eq. 609) then
              surfid = .true.
              goto 400
          endif
c
c...Symbol & Instances
c
      else if (ityp .eq. 2 .and. ist .eq. 1) then
          call ub_symbol_name(token2,ivxsub,nclkey,origin,i)
          if (i .eq. 1) then
              gtype = 16
              call ptdesc (nclkey,31,gname)
              call addskg(gname, gtype, ginx)
          else if (i .eq. 2) then
              gtype = 1
              call ptdesc (nclkey,32,gname)
              call addskg(gname, gtype, ginx)
          else
              errcom = ' '
              call error (244)
              go to 99999
          endif
c                                                      PARSE PLANE 'AT' POINT

C
C...Check if vocabulary word 'AT' appears after a specific PLANE ID.
C
      else if (vocab .and. voc .eq. at .and. planid) then
          igeo = igeo - 1
          call gtskg(igeo, gname, gtype)
          gtype = 2
          call chgskg(igeo, gname, gtype)
          planid = .false.
      else if (scalar .and. (curvid .or. surfid .or. mtrxid)) then
          if (mtrxid) then
            igeo = igeo - 1
            call gtskg(igeo, gname, gtype)
            gtype = gtype + 4
            call chgskg(igeo, gname, gtype)
            igeo = igeo + 2
c
c...Parse MX axis & box size parameters if any
c
            rpd(1) = tv
            do 115 i=1,3
               ins  = INX
               call parsit
               if (scalar) then
                  rpd(i+1) = tv
               else if (.not.scalar .and. i .eq. 2) then
                  INX = ins
                  NXTEOS = .false.
                  go to 120
               else
                  call error(282)
                  goto 99999
               endif
               if (NXTEOS) go to 120
  115       continue
  120       rgeonm(1) = rpd(1)
            rgeonm(2) = rpd(2)
            call addskg(gname, gtype, ginx)
            if (i .gt. 2) then
               rgeonm(1) = rpd(3)
               rgeonm(2) = rpd(4)
            else
               rgeonm(1) = rpd(2)
               rgeonm(2) = rpd(2)
            end if
            call addskg(gname, gtype, ginx)
        else if (wildwd) then
c
c...should exclude wildcard
c...Yurong
c
           igeo = igeo - 1
           goto 400
        else
c                                                      PARSE CURVE S VALUE
c                                                      PARSE SURF U & V VALUES

C
C...Check if s or u and v scalar values given after a CIRCLE, CURVE or
C...SURF ID.
C
          itv = tv
          if (dabs(tv - itv) .gt. 0.0001) then
              call error (217)
              goto 99999
          endif
          if (curvid) then
              igeo = igeo - 1
              call gtskg(igeo, gname, gtype)
              gtype = gtype + 2
              call chgskg(igeo, gname, gtype)
              igeo = igeo + 1
              gname = itv
              call addskg(gname, gtype, ginx)
          else if (surfid) then
              jpd(1) = itv
              if (.not.nietsf) then
                  igeo = igeo - 1
                  call gtskg(igeo, gname, gtype)
                  gtype = gtype + 3
                  call chgskg(igeo, gname, gtype)
                  igeo = igeo + 1
              endif
c
c...vp 6-jul-93 added DISPLY/SF1,u-points,u-lines,v-points,v-lines
c
              do 125 i=1,3
                 ins  = INX
                 call parsit
                 if (scalar) then
                    itv = tv
                    if (dabs(tv - itv) .gt. 0.0001) then
                       call error (217)
                       goto 99999
                    endif
                    jpd(i+1) = itv
                 else if (.not.scalar .and. i .eq. 2) then
                    INX = ins
                    NXTEOS = .false.
                    go to 150
                 else
                    call error(282)
                    goto 99999
                 endif
                 if (NXTEOS) go to 150
  125         continue
  150         itmp(3) = jpd(1)
              itmp(4) = jpd(2)
              itmp(1) = jpd(3)
              itmp(2) = jpd(4)
              if (i .le. 2) then
                  itmp(2) = jpd(1)
                  if (sc(169) .lt. 8.35) then
                     itmp(1) = jpd(2)
                  else
                     itmp(1) = 0
cc                     itmp(2) = 0
                     itmp(3) = 0
cc                     itmp(4) = 0
                  end if
              end if
              if (nietsf) then                  
                  igeo = igeo - 2
                  call gtskg(igeo, gname, gtype)
                  igeonm(1) = itmp(1)
                  igeonm(2) = itmp(2)
                  call chgskg(igeo, gname, gtype)
                  igeo = igeo + 1
                  call gtskg(igeo, gname, gtype)
                  igeonm(1) = itmp(3)
                  igeonm(2) = itmp(4)
                  call chgskg(igeo, gname, gtype)
                  igeo = igeo + 1
              else
                  igeonm(1) = itmp(1)
                  igeonm(2) = itmp(2)
                  call addskg(gname, gtype, ginx)
                  igeonm(1) = itmp(3)
                  igeonm(2) = itmp(4)
                  call addskg(gname, gtype, ginx)
                  igeo = igeo + 1
              endif
          endif
        endif
c
c                                                      PARSE 'ALL' GEOMETRY
c
C...Check for vocabulary word 'GEO'.
C
      else if (vocab .and. voc .eq. geo) then
          gtype = 31
          call addskg(gname, gtype, ginx)
      else if (vocab .and. voc .eq. redraw) then
          goto 200
c
c...if it is wild card parser word, if could return any type
c...because we didn't checked when parser
c
      else if ((wildwd).or. (filtgeo)) then
          igeo = igeo - 1
          goto 400
      else
          errcom= ' '
          call error (244)
          go to 99999
      endif

C
C...Reset PLANID, CURVID and SURFID flags to false for all tokens
C...other than PLANEs, CURVEs and SURFs.
C
  200 planid = .false.
      curvid = .false.
      surfid = .false.
      mtrxid = .false.

C
C...Get next token unless we're at the end of the statement
C
400   if (.not.NXTEOS) goto 100

C
C...Load a 99 after the last entry to mark the end of the stack
C
      gtype = 99
      call addskg(gname, gtype, ginx)
c
c**********************************************************************
c
c    Statement is syntactically correct.  Now display the geometry
c    from the GEONAM and TYPGEO arrays.
c
c**********************************************************************
c
c...Added check for NCL501+ mode
c...Paul  -  01/31/92
c...Old variant was:
c   if (ifl(35) .eq. 1 .or. ifl(35) .eq. 2) goto 99999
c
      if (ifl(35) .eq. 1 .or. (ifl(35) .eq. 2 .and.
     x  (ifl(350) .eq. 0 ))) goto 99999

C
C...Restore the pointer to the first token
C
      igeo = 1
      call gtskg(igeo, gname, gtype)
c                                                    DISPLAY GEO NAME
C
C...Display specifically named geometric entity
C
  500 if (gtype .eq. 1) then
          call gtdesc (gname, nclkey, nwds, ietype)
          if (lredraw) call dsprdw (nclkey,ietype)
          call blankg (2, nclkey)

c                                                    DISPLAY GEO TYPE

C
C...Display all of a generic geometry type
C...(pt, ve, ln, pl, ci, cv, sf, sh or pn)
C
      else if (gtype .eq. 11 .or. gtype .eq. 13 .or.
     x         gtype .eq. 14 .or. gtype .eq. 15) then
          nname  = gname
          call vctoid (nname,gclass,idum)
          call vxlfst
210       continue
          call vxlnxt (token2, isub, nclkey, kwds, ietype, ipg, iel)
          nwds = kwds
          if (ietype.eq.1) goto 220
          if (ietype.eq.gclass) then
            if (gtype .eq. 11) then
              if (lredraw) call dsprdw (nclkey,ietype)
              call blankg (2, nclkey)
            else
              call ptdsc3 (nclkey, nwds, ietype, asw)
              if (gtype .eq. 15) then
                 ginx = igeo + 1
                 call gtskg(ginx, gname, gtype)
                 rpd(1) = rgeonm(1)
                 rpd(2) = rgeonm(2)
                 ginx = igeo + 2
                 call gtskg(ginx, gname, gtype)
                 rpd(3) = rgeonm(1)
                 rpd(4) = rgeonm(2)
                 call dispmx (asw, rpd, ttname)
              else if (gtype .eq. 13) then
                 ginx = igeo + 1
                 call gtskg(ginx, gname, gtype)
                 call dispcv (asw, gname)
              else if (gtype .eq. 14) then
                 ginx = igeo + 1
                 call gtskg(ginx, gname, gtype)
                 itmp(1) = igeonm(1)
                 itmp(2) = igeonm(2)
                 ginx = igeo + 1
                 call gtskg(ginx, gname, gtype)
                 itmp(3) = igeonm(1)
                 itmp(4) = igeonm(2)
                 call dispsf (asw, itmp,ttname)
              endif
            endif
          endif
          goto 210
220       continue
c
c...Erase single symbol instances
c
      else if (gtype .eq. 16) then
         call gtdesc (gname, nclkey,nwds, ietype)
         call ub_symbol_set_vis(2,nclkey)
c
c...Erase all symbol instances
c
      else if (gtype .eq. 17) then
         nclkey = 0
         call ub_symbol_set_vis(2,nclkey)
C
C...Display PLANE at a POINT
C
      else if (gtype .eq. 2) then
          tmpnam = gname
          ginx = igeo + 1
          call gtskg(ginx, gname, gtype)
          call disppl (tmpnam, gname, pltok, plsub)
          igeo = igeo + 1
c                                                    DISPLAY CIRCLE OR CURVE
c                                                     WITH S VALUE

C
C...Display CURVE with specific number of segments
C
      else if (gtype .eq. 3) then
          tmpnam = gname
          ginx = igeo + 1
          call gtskg(ginx, gname, gtype)
          call dispcv (tmpnam, gname)
          igeo = igeo + 1
c                                                    DISPLAY SURF
c                                                     WITH U & V VALUES

C
C...Display SURF with specific number of U & V segments
C
      else if (gtype .eq. 4) then
          tmpnam = gname
          ginx = igeo + 1
          call gtskg(ginx, gname, gtype)
          itmp(1) = igeonm(1)
          itmp(2) = igeonm(2)
          ginx = ginx + 1
          call gtskg(ginx, gname, gtype)
          itmp(3) = igeonm(1)
          itmp(4) = igeonm(2)
          call dispsf (tmpnam, itmp, sftok)
          igeo = igeo + 2
      else if (gtype .eq. 5) then
          tmpnam = gname
          ginx = igeo + 1
          call gtskg(ginx, gname, gtype)
          rpd(1) = rgeonm(1)
          rpd(2) = rgeonm(2)
          ginx = igeo + 2
          call gtskg(ginx, gname, gtype)
          rpd(3) = rgeonm(1)
          rpd(4) = rgeonm(2)
          call dispmx (tmpnam, rpd, mxtok)
          igeo = igeo + 2
c                                                    DISPLAY 'ALL' GEOMETRY
C
C...Display all geometry if token is vocabulary word 'GEO'
C
      else if (gtype .eq. 31) then
          call vxlfst
350       continue
          call vxlnxt (token2, isub, nclkey, kwds, ietype, ipg, iel)
          if (ietype.eq.1) goto 360
          call ifidge (ietype,lgeo)
          if (lgeo .and. ietype .ne. matrix) then
            if (lredraw) call dsprdw (nclkey,ietype)
            call blankg (2,nclkey)
          endif
          goto 350
360       continue
      endif
C
C...Go get next token unless we're at the end of the statement
C
      igeo = igeo + 1
      call gtskg(igeo, gname, gtype)
      if (gtype .ne. 99) goto 500

99999 if (ifl(2) .ne. 0) call error (ifl(2))
      call delskg
      return
      end


C*********************************************************************
C*    E_SUBROUTINE     : subroutine disply2
C*     Handles the DISPLY/LAYER... statement.
C*     THis is a modified version of the subroutine
C*     disply, it retrieves all the geometry and then
C*     filters out those geometries which are not in the
C*     specified layers.
C*
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine disply2(layer1,layer2,lredraw)

      include 'com4a.com'
      include 'comdb.com'

      integer*2 layer,color,pen,linwgt,lintyp
      integer*2 layer1, layer2
      logical lredraw

      integer*2 nwds, ietype, gclass
      integer*4 ipg, iel
      integer*2 gtype
      integer*4 nclkey,isub,kwds,ginx
      real*8 asw, gname
      character*1 atname(64), awname(64)
      character*64 wname, ttname
      character*8 cvs(35), name
      equivalence (gname, name)
      equivalence (ttname, atname), (wname, awname)
      equivalence (cvs, jb)

      logical planid,curvid,surfid,nietsf,mtrxid,mgeo

      integer*2  at, all, geo, nname, idum, ntyp
      parameter (at  = 189)
      parameter (all  = 816)
      parameter (geo  = 835)
      parameter (nvoc = 13)
c
      integer*2 vvoc(nvoc),vinc
c
      data vvoc /602,603,604,605,606,607,608,609,636,616,123,610,613/

      planid = .false.
      curvid = .false.
      surfid = .false.
      nietsf = .false.
      mtrxid = .false.
      ifl(44)=9
      err=.false.
      ifl(2)=0
C
C...Sets voc equal to 602 so that it will search through the shapes
C...after searching through the shapes it will continue the search
C...thru voc equal 609(surfaces) and the continue to voc equal
C...to 636(patern)
C
      vinc = 1
515   call vctoid (vvoc(vinc),ist1,idum)
      call ifidge (ist1,mgeo)
C                                                      PARSE GEO TYPE
      gname = vvoc(vinc)
      gtype = 11
C
C...vp 3/24/98 for now CI or CV (any type) is a generic curve
C...for specific selection user can use geo filter in GUI
C
      if (ist1 .eq. 7 .or. ist1 .eq. 8) then
          curvid = .true.
          goto 810
      else if (vvoc(vinc) .eq. 609) then
          surfid = .true.
          goto 810
      endif
C                                                      PARSE 'ALL' GEOMETRY

C
C...Reset PLANID, CURVID and SURFID flags to false for all tokens
C...other than PLANEs, CURVEs and SURFs.
C
610   planid = .false.
      curvid = .false.
      surfid = .false.
      mtrxid = .false.

810   continue
C
C...Display all of a generic geometry type
C...pt, ve, ln, pl, ci, cv, sf, sh or pn)
C
      nname  = gname
      call vctoid (nname,gclass,idum)
      call vxlfst
620   continue
      call vxlnxt (token2, isub, nclkey, kwds, ietype, ipg, iel)
      nwds = kwds
C
C...Call umf_get_attrib to get token2's attributes
C...in this particular case we will only be interested
C...in token2's layer number.
C
      if (ietype.eq.1) goto 630
      if (ietype.eq.gclass) then
          call umf_get_attrib(nclkey,color,pen,lintyp,
     x                      linwgt,layer)
C
C...Now we check to see if token2 is in the desired layer
C...if it is, it will be displayed, if not the program will
C...continue to search and check the geometies in unibase.
C
          if (.not.(layer.ge.layer1.and.layer.le.layer2)) then
              goto 620
          elseif (gtype .eq. 11) then
              if (lredraw) call dsprdw (nclkey,ietype)
              call blankg (2, nclkey)
          endif
      endif
      goto 620
630   continue
C
C...Here is where the voc is changed so that all geometries
C...are searched for and the appropriate ones displayed.
C
      if (vinc .lt. nvoc) then
          vinc = vinc + 1
          goto 515
      endif
99999 if (ifl(2) .ne. 0) call error (ifl(2))
      return
      end
