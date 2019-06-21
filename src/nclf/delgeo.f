c*******************************************************************
c*    NAME        :  delgeo.f
c*        CONTAINS: subroutines delgeo and delgeo2
c*     Handles the *INVIS/...statement
c*
c*     COPYRIGHT (c) 1987 MILLS DATA SYSTEMS CORPORATION
c*
C*     MODULE NAME AND RELEASE LEVEL 
C*       delgeo.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:55
c*
c **********************************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine delgeom
C*     Handles the INVIS/... statement.
C*
C*    A INVIS/... statement will be parsed and two arrays loaded with
C*    data that defines what entities are to be invisibled.  The two
C*    arrays are: GEONAM and TYPGEO.  Their contents are:
C*
C*      For a single geometry name:
C*        The GEONAM entry will contain the ASW of the geometry entity.
C*        The TYPGEO will contain a value of 1.
C*    
C*      For a class of geometry: (PT, LN, CI, etc.)
C*        The GEONAM entry will contain the vocabulary number of the 
C*        word indicating the class of geometry.
C*        The TYPGEO will contain a value of 11.
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
C*    After the INVIS/... statement has passed all syntax checking, the 
C*    requested entities will be invisibled.  The GEONAM and TYPGEO arrays
C*    are processed to get the names, etc. of the entities to invisibled.
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
      subroutine delgeo

      include 'com4a.com'
      include 'comdb.com'

      integer*2 nwds, ietype, gclass
      integer*2 gtype
      integer*4 nclkey, isub, kwds, ipg, iel, igeo
      real*8 gname, origin(3)
      character*1 atname(64),awname(64)
      character*64 ttname, wname
      character*8 cvs(35), name
      logical lgeo
      integer*2 layer1,layer2,ilyrfl

      equivalence (gname, name)
      equivalence (ttname, atname),(wname, awname)
      equivalence (cvs, jb)

      integer*2 all, geo, nname, idum, ncid
      parameter (all = 816)
      parameter (geo = 835)
c
c
c
      ilyrfl =0
      igeo = 0
      ifl(44)=9

c
c... Syntax check the INVIS statement first.
c... Build teh GEONAM and TYPGEO tables.
c
c...         Get the first entity to process
C
      idtype = 0
      call parsit
C
C...if voc is equal to 902 then we are working with
C...the command ERASE/LAYER.
C
      if (voc.ne.902) goto 55
45    continue
      call parsit
C
C...It ityp is not equal to 3 then it is not an integer
C...and an error will be called, if it is the first time
C...thru, otherwise, just exit.  JLS 5/6/99
C
      if (ityp.ne.3) then
			if ((ityp.eq.7).and.(ilyrfl.ne.0)) goto 99999
         call error(53)
         goto 99999
      else
         ilyrfl = 1
         layer1=tv
         isvinx = inx
         idtype = -1
         call parsit  
C
C...If it is a THRU get the rest of the clause.
C
         if (ist.eq.152) then
            call parsit
            if (ityp.eq.3) then
              layer2=tv
            else
              call error(431)
              goto 99999
            endif
         else
C
C...Erase an individual layer. Set inx to isvinx because we won't
C...use the current token yet.
C
           inx = isvinx
           layer2=layer1
         endif
C
C...A check to make sure that the layers were entered
C...in increasing order, if they weren't the larger numbered
C...layer will be put in layer2 and the lower numbered layer
C...will be put in layer 1.
C
      endif
      if (layer1.gt.layer2) then
         temp=layer2
         layer2=layer1
         layer1=temp
      endif
C  
C...Call delgeo2 so that the appropriate geometries will
C...be erased.
C
      call delgeo2(layer1,layer2)
C
C...If it isn't the end of the statement return go back up
C...for more layers to delete.
C
      if (ityp.ne.7) goto 45
      goto 99999
C
C...If voc is equal to 1096 then the user would like to erase
C...the work axes.
C
55    continue
      if (voc.eq.1096) then
        call umf_drw_cpl_axis_off
        goto 99999
      endif
C
C...If voc is equal to 870 then the user would like to erase
C...the model axes.
C
      if (voc.eq.870) then
        call umf_drw_mod_axis_off
        goto 99999
      endif
c      
c...Do not process the following at this point
c...ERASE/MOTION 
c...ERASE/AXIS
c
      if (vocab .and.
     x    voc .eq. 836 .or. 
     x    voc .eq. 132 ) goto 99999
c                                     ERASE/ALL
      if (vocab .and.voc.eq.all) then
        if (nxteos) then
          call blankg (1, nclkey)
          goto 99999
        endif
        call error(4)
        goto 99999
      endif

      goto 101
C
c...Get the next entity to process
C
  100 idtype = 0
      call parsit
101   igeo = igeo +1
c
c...If an error other than # 88, 85, 61, or 9 and a wild card token
c...was found, exit the routine.
      if (err .and.
     x    (.not.(ifl(2).eq.88 .or.
     x           ifl(2).eq.85 .or.
     x           ifl(2).eq.61))) then
          go to 99999
      else
          err=.false.
          ifl(2)=0
      endif
c                                                      PARSE GEO NAME
C
c...Check if it is a geometry name
C
      call vctoid (voc,ncid,idum)
      call ifidge (ncid,lgeo)
      if (geom .and. geo .ne. matrix) then
          call vstchk
          if (ist .eq. 1)  then
               write (errcom,1010) token2
1010           format (' name not found:',a64)
               call error(9)
               goto 99999
          endif
          gname = tv
          gtype = 1
          call addskg(gname, gtype, ginx)
c
c...SYMBOL
c
      else if (vocab .and. voc .eq. 877) then
          gname = voc
          gtype = 17
          call addskg(gname, gtype, ginx)
c                                                      PARSE GEO TYPE
c...Check if generic geometry type (PT, VE, LN, PL, CI, CV, SF, PN, SH)
C
      else if (vocab .and.
     x         ((lgeo .and. ncid .ne. 10).or.(voc.eq.610))) then 
          gname = voc
          gtype = 11
          call addskg(gname, gtype, ginx)
c
c...Symbols & Instances
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
             errcom= ' '
             call error (244)
             go to 99999
         endif
C
c...Check for vocabulary word 'GEO'.
C
      else if (vocab .and. voc .eq. geo) then
          gtype = 31
          call addskg(gname, gtype, ginx)
c...if it is wild card parser word or in filter function, if could return any type
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
c
c...Get next token unless we're at the end of the statement
C
400   if (nextyp .ne. 11) goto 100

C
c...Load a 99 after the last entry to mark the end of the stack
C
      gtype = 99
      call addskg(gname, gtype, ginx)
c**********************************************************************
c
c    Statement is syntactically correct.  Now invisible the geometry
c    from the GEONAM and TYPGEO arrays.
c
c**********************************************************************
c
c...Added check for NCL501+ mode
c...Paul  -  02/11/91
c...Old variant was:
c   if (ifl(35) .eq. 1 .or. ifl(35) .eq. 2) goto 99999
c
      if (ifl(35).eq.1.or.(ifl(35).eq.2.and.ifl(350).eq.0)) goto 99999

C
c...Restore the pointer to the first token
C
      igeo = 1
      call gtskg(igeo, gname, gtype)
c                                                INVISIBLE GEO NAME
C
c...Erase specifically named geometric entity
C
 500  if (gtype .eq. 1) then 
           call gtdesc (gname, nclkey,nwds, ietype)
           call blankg (0,nclkey)
c                                                INVISIBLE GEO TYPE

C
c...Erase all of a generic geometry type 
c...(pt, ve, ln, pl, ci, cv, sf, sh or pn)
C
       else if (gtype .eq. 11) then
          nname  = gname
          call vctoid (nname,gclass,idum)
          call vxlfst
210       continue
          call vxlnxt (token2, isub, nclkey, kwds, ietype, ipg, iel)
          if (ietype.eq.1) goto 220
          if (ietype .eq. gclass) call blankg (0,nclkey)
          goto 210
220       continue
c
c...Erase single symbol instances
c
      else if (gtype .eq. 16) then
         call gtdesc (gname, nclkey,nwds, ietype)
         call ub_symbol_set_vis(0,nclkey)
c
c...Erase all symbol instances
c
      else if (gtype .eq. 17) then
         nclkey = 0
         call ub_symbol_set_vis(0,nclkey)

c                                                    INVISIBLE ALL
C
c...Display all geometry if token is vocabulary word 'ALL'
C
      else if (gtype .eq. 31) then
          call vxlfst
10        continue
          call vxlnxt (token2, isub, nclkey, kwds, ietype, ipg, iel)
          if (ietype.eq.1) goto 111
          call ifidge (ietype,lgeo)
          if (lgeo .and. ietype .ne. matrix) call blankg (0,nclkey)
          goto 10
111       continue
      endif
C
c...Go get next token unless we're at the end of the statement
C
      igeo = igeo + 1
      call gtskg(igeo, gname, gtype)
      if (gtype .ne. 99) goto 500

99999 if (ifl(2) .ne. 0) call error (ifl(2))
      call delskg
      return
      end



C*********************************************************************
C*    E_SUBROUTINE     : subroutine delgeo2
C*     Handles the INVIS/LAYER... statement.
C*     A modified version of delgeo.
C*
C********************************************************************/
      subroutine delgeo2(layer1,layer2)

      include 'com4a.com'
      include 'comdb.com'

      integer*2 layer1,layer2

      integer*2 layer, color, pen, linwgt, lintyp

      integer*2 ietype, gclass
      integer*2 gtype
      integer*4 nclkey, isub, kwds, ipg, iel
      real*8 gname
      character*1 atname(64),awname(64)
      character*64 ttname, wname
      character*8 cvs(35), name

      equivalence (gname, name)
      equivalence (ttname, atname),(wname, awname)
      equivalence (cvs, jb)

      integer*2 all, geo, nname, idum
      parameter (all = 816)
      parameter (geo = 835)
c
      parameter (nvoc = 13)
c
      integer*2 vvoc(nvoc),vinc
c
      data vvoc /602,603,604,605,606,607,608,609,636,616,123,610,613/

c
c
      ifl(44)=9
      err=.false.
      ifl(2)=0

      vinc = 1
305   continue
      gname = vvoc(vinc)
      gtype = 11
C
c...Display all of a generic geometry type
c...(pt, ve, ln, pl, ci, cv, sf, sh or pn)
C
      nname  = gname
      call vctoid (nname,gclass,idum)
      call vxlfst
610   continue
      call vxlnxt (token2, isub, nclkey, kwds, ietype, ipg, iel)
C
C...Call umf_get_attrib to get the layer number of the current 
C...entity, if it is in the appropriate layer, it will be
C...erased.
C
      if (ietype.eq.1) goto 620
      if (ietype .eq. gclass) then
          call umf_get_attrib(nclkey,color,pen,lintyp,
     x                        linwgt,layer)
C
C...Checks to see if it is in the layer range.
C
          if (.not.(layer.ge.layer1.and.layer.le.layer2)) then
              goto 610
          else
              call blankg (0,nclkey)
          endif
      endif
      goto 610
620   continue
C
C...The following will send the program back thru so that all
C...the geometries will be checked to see if they need to be
C...erased.
C
c..........
c...      if (voc.lt.609) then
c...         voc=voc+1
c...         goto 305
c...      elseif (voc.eq.609) then
c...         voc=636
c...         goto 305
c...      else
c...         continue       
c...     endif
      if (vinc .lt. nvoc) then
          vinc = vinc + 1
          goto 305
      endif
99999 if (ifl(2) .ne. 0) call error (ifl(2))
      return
      end


