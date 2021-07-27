C*********************************************************************
C*    NAME         :  geomf.f
C*       CONTAINS:
C*    COPYRIGHT 2007 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       geomf.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:06
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine geomf(flag)
C*      This routine handles parser COLF/LAYF expressions.  It is called by
C*      the PARSIT and it returns the list of geoms in geolst
C*      with ITYP equal to 2.  
C*    PARAMETERS
C*       INPUT  :
C*          flag: 1: parser COLF
C*				2: parser LAYF
C*				3: parser CLIPF
C*				4: parser FILTER
C*                5: parser MARKF
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine geomf(flag)
c
      include 'com8a.com'
      integer*2 flag
      integer*2 i,j,ilayer,color,pen,linwgt,lintyp,icolr,ilayr2
      integer*2 nwds, ietype, nlay, nmkr, nclr
      integer*4 nclkey, isub, ipg, iel, indx,strlen1
      logical*2 found
      character*256 comstr
      real*8 asw
      integer*2 imark,marker
      common /subgeom/ geominx    
      integer*4  geominx
      integer*2 BLACKV,WHITEV,BLUEV,REDV,GREENV,MGNTAV,YELLOV,CYANV,
     1          BROWNV,TANV,LTBLUV,SEAGRV,ORANGV,PINKV,PURPLV,GREYV
      integer*2 DOTV,PLSV,ASTV,CIRV,CRSV,TRIV,DIMV,SQRV,DBLV,LRGV,CUBV
      parameter (BLACKV=904)
      parameter (WHITEV=905)
      parameter (BLUEV =906)
      parameter (REDV  =907)
      parameter (GREENV=908)
      parameter (MGNTAV=909)
      parameter (YELLOV=910)
      parameter (CYANV =911)
      parameter (BROWNV=912)
      parameter (TANV  =913)
      parameter (LTBLUV=914)
      parameter (SEAGRV=915)
      parameter (ORANGV=1101)
      parameter (PINKV=1102)
      parameter (PURPLV=1103)
      parameter (GREYV =1104)
      parameter (DOTV=561)
      parameter (PLSV=19)
      parameter (ASTV=953)
      parameter (CIRV=607)
      parameter (CRSV=819)
      parameter (TRIV=954)
      parameter (DIMV=956)
      parameter (SQRV=955)
      parameter (DBLV=957)
      parameter (LRGV=958)
      parameter (CUBV=959)
    
      if (flag.eq.3) then
          call geomclipf
          return
      endif
      if (flag.eq.4) then
          call preflt
          return
      endif
c
C...Initialization
c
      if (filtwd) then
c
c...shouldn't goes here if filter function is on
c
          return
      endif
      
      call delskm
      geomnum = 0
      geominx = 1

      call parser
c
c... the token have to be a '(' 
c
      if (ityp.ne.5.or.ist.ne.6) then
          ifl(2)=309
          goto 99999
      endif
      call parser
c
c.....COLF
c
      if (flag.eq.1) then
10        if (ityp.eq.1) then
             icolr = -1
             if (ist .eq. BLACKV) icolr = 0
             if (ist .eq. WHITEV) icolr = 1
             if (ist .eq. BLUEV ) icolr = 2
             if (ist .eq. REDV  ) icolr = 3
             if (ist .eq. GREENV) icolr = 4
             if (ist .eq. MGNTAV) icolr = 5
             if (ist .eq. YELLOV) icolr = 6
             if (ist .eq. CYANV ) icolr = 7
             if (ist .eq. BROWNV) icolr = 8
             if (ist .eq. TANV  ) icolr = 9
             if (ist .eq. LTBLUV) icolr = 10
             if (ist .eq. SEAGRV) icolr = 11
             if (ist .eq. ORANGV) icolr = 12
             if (ist .eq. PINKV ) icolr = 13
             if (ist .eq. PURPLV) icolr = 14
             if (ist .eq. GREYV ) icolr = 15
          else if ((ITYP .eq. 2 .and. ist.eq.1 ).or.
     x      (ITYP .eq. 2 .and. ist.eq.24 ).or.
     x       (ityp.eq.9)) then
              if ((ityp.eq.9).or.(ITYP.eq.2 .and. ist.eq.1)) then
                  comstr = token2
                  j = strlen1(token2)
                  isub = ivxsub
              endif
              if (ityp.eq.2.and.ist.eq.24) then
                  j = 0
                  call gttext(comstr,j)
                  isub = 0
              endif
              call ncl_getclr_inx(comstr, j, isub, indx)
              icolr = indx              
          else if (.not.( (ityp.eq.3).or.(ityp.eq.4).or.
     x        ((ityp.eq.2).and.(ist.eq.2)) )) then
             ifl(2)=53
             go to 99999
          else
              icolr = tv
          endif
          call ncl_add_clrlst(icolr,nclr)
          if (nextyp.eq.9) then
              call parser
              goto 10
          endif
c
c.....LAYF
c
      else if (flag.eq.2) then
          ilayer = tv
          call ncl_add_laylst(ilayer,nlay)
20        if (nextyp.eq.9) then
              call parser
          else 
              goto 100
          endif
          if (ityp.eq.1.and.ist.eq.152) then
              call parser
              if (.not.( (ityp.eq.3).or.(ityp.eq.4).or.
     x           ((ityp.eq.2).and.(ist.eq.2)) )) then
                  ifl(2)=53
                  go to 99999
              endif
              ilayr2 = tv
              i = ilayer + 1
              do while (i.le.ilayr2)
                  call ncl_add_laylst(i,nlay)
                  i = i + 1
              enddo
          else if (ityp.eq.3.or.ityp.eq.4.or.
     x            (ityp.eq.2.and.ist.eq.2)) then
              ilayer = tv
              call ncl_add_laylst(ilayer,nlay)
          endif
          goto 20
c
c.....MARKF
c
      else if (flag.eq.5) then
30        if (ityp.eq.1) then
              imark = -1
              if (ist .eq. DOTV) imark = 1
              if (ist .eq. PLSV) imark = 2
              if (ist .eq. ASTV) imark = 3
              if (ist .eq. CIRV) imark = 4
              if (ist .eq. CRSV) imark = 5
              if (ist .eq. TRIV) imark = 6
              if (ist .eq. DIMV) imark = 7
              if (ist .eq. SQRV) imark = 8
              if (ist .eq. DBLV) imark = 9
              if (ist .eq. LRGV) imark = 10
              if (ist .eq. CUBV) imark = 11
              if (imark .eq. -1) then
                  ifl(2) = 182
                  goto 99999
              endif             
          else if (.not.( (ityp.eq.3).or.(ityp.eq.4).or.
     x        ((ityp.eq.2).and.(ist.eq.2)) )) then
              ifl(2)=53
              go to 99999
          else
              imark=tv
          endif
          call ncl_add_mkrlst(imark,nmkr)
          if (nextyp.eq.9) then
              call parser
              goto 30
          endif
      endif
100   if (flag.eq.1 .and. icolr .lt. 0) then
          ifl(2)=428
          go to 99999
      else if (flag.eq.5.and.imark.lt.0) then
          ifl(2)=445
          goto 99999
      endif
c
c.....Get the list of geometry
c
      call vxlfst
620   continue
      call vxlnxt (token2, isub, nclkey, nwds, ietype, ipg, iel)
      if (ietype.eq.1) goto 630
      if (nclkey .eq. 0) go to 620
c
c...we don't return certain type of the label
c
      if (((ietype.ge.11).and.(ietype.le.17))
     x     .or. (ietype.eq.19) .or. (ietype.eq.2)) go to 620
C
C...Call umf_get_attrib to get token2's attributes
C...in this particular case we will only be interested
C...in token2's layer/color number.
C...Call umf_get_marker_type to get token2's marker type if 
C...token2 is a point or patern.
C
      if (flag.lt.3) then
          call umf_get_attrib(nclkey,color,pen,lintyp,
     x                      linwgt,layer)
      else if (flag.eq.5.and.(ietype.eq.3.or.ietype.eq.20)) then
          call umf_get_marker_type (nclkey,marker)
      else
          goto 620
      endif
c
c.....Check geometry against filter
c
      call ncl_filter_keep(nclkey,ietype,token2,found)
      if (found .eq. .false.) goto 620
      call ptdsc3 (nclkey, nwds, ietype, asw)
      call addskm(asw, geomnum)
      goto 620
  630 continue
c
c... the token have to be a ')'
c
      call parser
      if (ityp.ne.5.or.ist.ne.7) then
          ifl(2)=310
          goto 99999
      endif

99999 if (ifl(2) .ne. 0) then
          err=.true.
      endif
c
c.....Delete allocated memory for filter lists
c
      call ncl_delete_filtlsts
C 
C...save nextyp in ifl(122)
C 
      ifl(122)=nextyp
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : geomclipf
C*      This routine handles parser CLIPF expressions.  It is called by
C*      the PARSIT and it returns the list of geoms in geolst
C*      with ITYP equal to 2.  
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine geomclipf
c
      include 'com8a.com'
      integer*2 nwds, ietype, ifl44
      integer*4 nclkey, isub, ipg, iel,initflag, level,ret
      real*8 asw
      integer*4 pln_num, side, tcross, flag, filtnum
      real*8 rplane(4),cnv
      common /subgeom/ geominx    
      integer*4  geominx  
      logical*2  sv_lwrk, trflg
      integer*2 XSMALL, XLARGE, YSMALL, YLARGE, ZSMALL, ZLARGE, 
     1           SAME, CROSS, ON, OFF
c
      parameter (XSMALL =641)
      parameter (XLARGE =638)
      parameter (YSMALL =642)
      parameter (YLARGE =639)
      parameter (ZSMALL =643)
      parameter (ZLARGE =640)
      parameter (SAME =730)
      parameter (CROSS =819)
      parameter (ON =71)
      parameter (OFF =72)
c
C...Initialization
      if (filtwd) then
c
c...geomnum save the skfclst number and finally skmlst number
c...filtnum save the skmlst number temperately
c
          filtnum = 0
      else
          call delskm
          geomnum = 0
          geominx = 1  
      endif
      pln_num = 0
      side = 0
      tcross = 0
      cnv = 1.
      if (ifl(264) .eq. 1) cnv = 1.d0 / 25.4d0
      do 100, i=1,4
         rplane(i) = 0.0
  100 continue
      ifl44 = ifl(44)
      ifl(44)=9
      call parser
c
c... the token have to be a '(' 
c
      if (ityp.ne.5.or.ist.ne.6) then
          ifl(2)=309
          goto 99999
      endif
  200 call parsit 
c
c...must a plane
c  
  202 if (ityp .ne. 7 .and. pln_num .eq. 6) then
          ifl(2) = 4
          goto 99999
      endif
      if (.not. (geom .and. geotyp .eq. plane)) then
          if (pln_num.eq.0) then
              call error(19)
              goto 99999
          endif
      endif
c
c...don't apply MODSYS and REFSYS when save the plane for filter
c
      sv_lwrk = lwrk
      lwrk = .false.
      trflg = .false.
      call gtentt(tv, trflg, nclkey, ietype, sc(10))
      lwrk = sv_lwrk      
      rplane(1) = sc(10)
      rplane(2) = sc(11)
      rplane(3) = sc(12)
      rplane(4) = sc(13) * cnv
      call parsit 
      side = 0
      if (ityp.eq. 1) then
          if (ist .eq. SAME) then
              side = 6
          else if (ist .eq. XSMALL) then
              side = 0
          else if (ist .eq. XLARGE) then 
              side = 1
          else if (ist .eq. YSMALL) then 
              side = 2
          else if (ist .eq. YLARGE) then 
              side = 3
          else if (ist .eq. ZSMALL) then
              side = 4
          else if (ist .eq. ZLARGE) then 
              side = 5
          else
             ifl(2)=18
             go to 99999
          endif
      else
          ifl(2)=18
          go to 99999
      endif          
      call parsit
      tcross = 0
      if (ityp.eq. 1) then
          if (ist .eq. CROSS) then
             tcross = 2
          else if (ist .eq. ON) then
             tcross = 1
          else if (ist .eq. OFF) then
             tcross = 0
          else
             ifl(2)=61
             go to 99999
          endif
          if (nextyp.eq.7) then
              goto 400
          else if (nextyp.eq.9) then
              pln_num = pln_num + 1
              call saveclip (rplane(1), rplane(2), 
     1              rplane(3), rplane(4), 
     2              side, tcross, pln_num) 
              goto 200
          endif        
      else if (ityp.eq.5.and.ist.eq.7) then
C 
C...save nextyp in ifl(122)
C 
          ifl(122)=nextyp
          goto 400
      else if (ityp.eq.5.and.ist.eq.9) then
          pln_num = pln_num + 1
          call saveclip (rplane(1), rplane(2), rplane(3), rplane(4), 
     1              side, tcross, pln_num) 
          goto 200
      else if (ityp.eq.2.and.ist.eq.6) then
          pln_num = pln_num + 1
          call saveclip (rplane(1), rplane(2), rplane(3), rplane(4), 
     1              side, tcross, pln_num) 
          goto 202
      else
          ifl(2)=61
          go to 99999
      endif
 400  pln_num = pln_num + 1
      call saveclip (rplane(1), rplane(2), rplane(3), rplane(4), 
     1              side, tcross, pln_num)
c.
c...clipf
c
c...if in batch, create the internal segment first
c
      if (ifl(35).eq.1) then
          call ud_clip_seg
      endif
c
c...select entity follow Filter Bounding Region
c
      call ud_init_select_buffer
      flag = 1
      call 	ud_clip_region (flag)
c
c...select entity follow Filter Bounding Region
c
      initflag = 1
      level = 1
      ret = 0
620   continue
      call gtpknxt(initflag, nclkey, level, ret)
      initflag = 0
      if (ret.eq.0) goto 630
      if (nclkey .eq. 0) go to 620
      call ptdsc3 (nclkey, nwds, ietype, asw)
      if (filtwd) then
           call addskm(asw, filtnum)
      else
           call addskm(asw, geomnum)
      endif
      goto 620
  630 continue
c
c...force reset the selection
c
      flag = 1
      call ncl_reset_select(flag)
      if (ifl(35).eq.1) then
          call ud_close_clipseg
      endif
      if (filtwd) then
c...geomnum is always number of skfclst, filtnum is 
c...temp number of skmlst
         call andskm_skfc(filtnum, geomnum)
      endif
c
c... the token have to be a ')'
c... if already parsed ')', do't parser again
c
      if (ityp.eq.5.and.ist.eq.7) goto 99999
      call parser
      if (ityp.ne.5.or.ist.ne.7) then
          ifl(2)=310
          goto 99999
      endif

99999 if (ifl(2) .ne. 0) then
          err=.true.
      endif
C 
C...save nextyp in ifl(122)
C 
      ifl(122)=nextyp
      ifl(44)= ifl44
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine getgeomlst()
C*       return THRU/COLF/LAYF next label/index into current token2 and ivxsub.
C*		all THRU/COLF/LAYF/* will handled in parsit routine to save the list of 
C*		labels
C*    PARAMETERS
C*       INPUT  : none
C*       OUTPUT : none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine getgeomlst

      include 'com.com'

      common/subcom/ idelim
      common /subgeom/ geominx    
      integer*4  geominx  
      integer*2 idelim

      real*8 asw
      integer*2 nwds, ietype
      integer*4 nclkey
c
c...   Return the next entity
c
      if (ifl(44).eq.0) then
c
c...   If caller is not skipping delimeters, return comma every second call.
c
        idelim = 1-idelim
        if (idelim.eq.1) then
          ityp = 5
          ist = nextyp
          return
        endif
      endif
      if ((geomnum.eq.0).or.((geomnum-geominx).lt.0)) then
           token2 = ' '
           ivxsub = 0
           goto 100
      endif
      call gtskm(geominx, asw)
      geominx = geominx + 1
      call gtdesc (asw, nclkey, nwds, ietype)
      call nclf_getlabel(nclkey, token2, ivxsub)
      ityp = 2
      ist = ietype
c
c...call this function to assign some value (such tv)
c
      call vstchk
C
C...restore nextyp
C
  100 if ((geomnum.eq.0).or.((geomnum-geominx).lt.0)) then
         nextyp=ifl(122)
c...don't delete list here, delete it when we recreate it because we 
c...may use svpars, rtpars function, need reset back
c
c...         call delskm
         geomnum = 0
         geominx = 0
      else
C
C...set nextyp to ','
C
         nextyp=9
      endif
      filtgeo = .true.
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine reset_geomlst()
C*       reset THRU/COLF/LAYF/CLIPF/FILTER list
C*	
C*    PARAMETERS
C*       INPUT  : none
C*       OUTPUT : none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine reset_geomlst

      include 'com.com'
      common /subgeom/ geominx    
      integer*4  geominx  
c
      nextyp=ifl(122)
      call delskm
      geomnum = 0
      geominx = 0
      return
      end
      
C*********************************************************************
C*    E_SUBROUTINE     : subroutine preflt()
C*     This routine parses the filter statement before doing the
C*     filtering so multiple entity types or attributes can be
C*     given in the same filter call.  When multiple filters of the
C*     same type are given they will be treated as an OR. In other
C*     words, if an entity matches any of the filters of one type
C*     it will be kept.  Each type of filter will still be treated
C*     as an AND so an entity must match a filter of each type given
C*     to be kept.       
C*	
C*    PARAMETERS
C*       INPUT  : none
C*       OUTPUT : none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine preflt

      include 'com8a.com'
c
      integer*2 imark,icolr,igeom,ilayr,idum,sgnum
      integer*2 i,j,nwds,ngeo,nclr,nmkr,nlay,nclp,nlab,sinx,snxtyp
      integer*4 ipg,iel,isub,nclkey,indx,strlen1
      logical*2 keep
      real*8 asw
      character*256 comstr
      common /subgeom/ geominx    
      integer*4  geominx
      integer*2 BLACKV,WHITEV,BLUEV,REDV,GREENV,MGNTAV,YELLOV,CYANV,
     1          BROWNV,TANV,LTBLUV,SEAGRV,ORANGV,PINKV,PURPLV,GREYV
      integer*2 DOTV,PLSV,ASTV,CIRV,CRSV,TRIV,DIMV,SQRV,DBLV,LRGV,CUBV
      parameter (BLACKV =904)
      parameter (WHITEV =905)
      parameter (BLUEV  =906)
      parameter (REDV   =907)
      parameter (GREENV =908)
      parameter (MGNTAV =909)
      parameter (YELLOV =910)
      parameter (CYANV  =911)
      parameter (BROWNV =912)
      parameter (TANV   =913)
      parameter (LTBLUV =914)
      parameter (SEAGRV =915)
      parameter (ORANGV=1101)
      parameter (PINKV =1102)
      parameter (PURPLV=1103)
      parameter (GREYV =1104)
      parameter (DOTV=561)
      parameter (PLSV= 19)
      parameter (ASTV=953)
      parameter (CIRV=607)
      parameter (CRSV=819)
      parameter (TRIV=954)
      parameter (DIMV=956)
      parameter (SQRV=955)
      parameter (DBLV=957)
      parameter (LRGV=958)
      parameter (CUBV=959)  
c
C...Initialization
c...shouldn't go here if filter function is on
c
      if (filtwd) then
          return
      endif
      
      call delskm
      geomnum = 0
      geominx = 1
      filtwd = .true.
      ngeo = 0
      nmkr = 0
      nlab = 0
      nlay = 0
      nclp = 0
      nclr = 0

      call parser
c
c... the token has to be a '(' 
c
      if (ityp.ne.5.or.ist.ne.6) then
          ifl(2)=309
          goto 99999
      endif
      call parser
c
c.....Parse filter statement to get all filters
c
10    if (ityp.eq. 1) then
c
c.....COLF         
c
        if (ist.eq.942) then
          icolr = -1
          call parser
          if (ityp.ne.5.or.ist.ne.6) then
            ifl(2)=309
            goto 99999
          endif
          call parser
20        if (ityp.eq.1) then
            if (ist .eq. BLACKV) icolr = 0
            if (ist .eq. WHITEV) icolr = 1
            if (ist .eq. BLUEV ) icolr = 2
            if (ist .eq. REDV  ) icolr = 3
            if (ist .eq. GREENV) icolr = 4
            if (ist .eq. MGNTAV) icolr = 5
            if (ist .eq. YELLOV) icolr = 6
            if (ist .eq. CYANV ) icolr = 7
            if (ist .eq. BROWNV) icolr = 8
            if (ist .eq. TANV  ) icolr = 9
            if (ist .eq. LTBLUV) icolr = 10
            if (ist .eq. SEAGRV) icolr = 11
            if (ist .eq. ORANGV) icolr = 12
            if (ist .eq. PINKV ) icolr = 13
            if (ist .eq. PURPLV) icolr = 14
            if (ist .eq. GREYV ) icolr = 15
          else if ((ityp .eq. 2 .and. ist.eq.1 ).or.
     x          (ityp .eq. 2 .and. ist.eq.24).or.
     x          (ityp.eq.9)) then
              if ((ityp.eq.9).or.(ityp.eq.2 .and. ist.eq.1)) then
                comstr = token2
                j = strlen1(token2)
                isub = ivxsub
              endif
              if (ityp.eq.2.and.ist.eq.24) then
                j = 0
                call gttext(comstr,j)
                isub = 0
              endif
              call ncl_getclr_inx(comstr, j, isub, indx)
              icolr = indx   
c            endif
          else if (.not.((ityp.eq.3).or.(ityp.eq.4).or.
     x       ((ityp.eq.2).and.(ist.eq.2)))) then
            ifl(2)=53
            go to 99999
          else
            icolr = tv
          endif
          call ncl_add_clrlst(icolr,nclr)
          call parser
          if (ityp.lt.5) goto 20
          if (ityp.ne.5.and.ist.ne.7) then
            ifl(2)=310
           goto 99999
          endif
c          
c.....MARKF
c
        else if (ist.eq.960) then
          imark = -1
          call parser
          if (ityp.ne.5.or.ist.ne.6) then
            ifl(2)=309
            goto 99999
          endif
          call parser
30        if (ityp.eq.1) then
            if (ist .eq. DOTV) imark = 1
            if (ist .eq. PLSV) imark = 2
            if (ist .eq. ASTV) imark = 3
            if (ist .eq. CIRV) imark = 4
            if (ist .eq. CRSV) imark = 5
            if (ist .eq. TRIV) imark = 6
            if (ist .eq. DIMV) imark = 7
            if (ist .eq. SQRV) imark = 8
            if (ist .eq. DBLV) imark = 9
            if (ist .eq. LRGV) imark = 10
            if (ist .eq. CUBV) imark = 11
          else if (.not.((ityp.eq.3).or.(ityp.eq.4).or.
     x       ((ityp.eq.2).and.(ist.eq.2)))) then
            ifl(2)=53
            go to 99999
          else
            imark = tv
          endif
          call ncl_add_mkrlst(imark,nmkr)
          call parser
          if (ityp.lt.5) goto 30
          if (ityp.ne.5.and.ist.ne.7) then
            ifl(2)=310
           goto 99999
          endif
c
c.....LAYF
c
        else if (ist.eq.943) then
          ilayr = -1
          call parser
          if (ityp.ne.5.or.ist.ne.6) then
            ifl(2)=309
            goto 99999
          endif
          call parser
          if (.not.((ityp.eq.3).or.(ityp.eq.4).or.
     x       ((ityp.eq.2).and.(ist.eq.2)))) then
            ifl(2)=53
            go to 99999
          endif
          ilayr = tv
          call ncl_add_laylst(ilayr,nlay)
40        if (nextyp.eq.9.or.nextyp.eq.7) then
            call parser
            if (ityp.eq.5.and.ist.eq.7) then
              call parser
              goto 50
            endif
          else
            ifl(2)=53
            go to 99999
          endif
          if (ityp.eq.1.and.ist.eq.152) then
            call parser
            if (.not.((ityp.eq.3).or.(ityp.eq.4).or.
     x         ((ityp.eq.2).and.(ist.eq.2)))) then
              ifl(2)=53
              go to 99999
            endif
            ilay2 = tv
            i = ilayr + 1
            do while (i.le.ilay2)
              call ncl_add_laylst(i,nlay)
              i = i + 1
            enddo
          else if (ityp.eq.3.or.ityp.eq.4.or.
     x            (ityp.eq.2.and.ist.eq.2)) then
            ilayr = tv
            call ncl_add_laylst(ilayr,nlay)
          else
            ifl(2)=53
            go to 99999
          endif
          if (nextyp.eq.9) goto 40
          call parser
          if (ityp.ne.5.and.ist.ne.7) then
            ifl(2)=310
            goto 99999
          endif
c
c.....CLIPF
c.......The extra parser call is to move beyond the ')' of
c.......the CLIPF call so parsing doesn't think that it is the
c.......end of the FILTER statement
c
        else if (ist.eq.949) then
          call ncl_add_clplst(inx,nclp)
          do
            call parser
            if ((ityp.eq.5.and.ist.eq.7).or.ityp.eq.7) exit
          enddo
          if (ityp.eq.7) then
            ifl(2)=310
            goto 99999
          endif
          call parser
          goto 50
c
c.....Geometry type
c
        else if (ist.ge.602 .and. ist.le. 609 .or. ist.eq.636
     1     .or. ist.eq.616 .or. ist.eq.123 .or. ist.eq.610
     2     .or. ist.eq.613) then
          ilayr = -1
          call vctoid (ist,igeom,idum)
          if (igeom.eq.0) then
            ifl(2)=53
            go to 99999
          endif
          call ncl_add_geolst(igeom,ngeo)
c
c.....Invalid vocab word in filter function
c
        else
          ifl(2) = 182
          goto 99999
        endif
c
c.....Wildcard string
c
      else if (ityp.eq.10) then
        call ncl_add_lablst(token2,nlab)
      endif
      call parser
50    if (ityp.ne.5.and.ityp.ne.7) goto 10
      if (ityp.ne.5.and.ist.ne.7) then
        ifl(2)=310
        goto 99999
      endif
c
c.....Build geometry list based on filters
c
      call vxlfst
620   continue
      call vxlnxt (token2, isub, nclkey, nwds, ietype, ipg, iel)
      if (ietype.eq.1) goto 630
      if (nclkey .eq. 0) goto 620
c
c.....Ignore certain geometry types
c
      if (((ietype.ge.11).and.(ietype.le.17))
     x     .or. (ietype.eq.19) .or. (ietype.eq.2)) goto 620
c
c.....Do the actual filtering
c
      call ncl_filter_keep(nclkey,ietype,token2,keep)
      if (keep .eq. .false.) goto 620
      call ptdsc3 (nclkey, nwds, ietype, asw)
      call addskfc(asw, geomnum)
      goto 620
630   continue
c
c.....Handle CLIPF filters
c.......This approach uses the current CLIPF function. Extra
c.......lists are used so each resulting list from the CLIPF
c.......function can be used without losing the previous result.
c.......After each CLIPF call the returned geometry is added to
c.......the filter list of geometry (without duplication).  This
c.......follows the pattern above where each filter is treated 
c.......as an OR so as long as an entity fits one of the filters
c.......it is kept.
c
      snxtyp = nextyp
      if (nclp.gt.0) then
        call copyskf
        sinx = inx
        sgnum = geomnum
        i = 0
        do while (i.lt.nclp)
          if (i.gt.0) call copysflt
          geomnum = sgnum
          call ncl_get_clplst(i,inx)
          call geomclipf
          call orskf_sclp
          call delskm
          i = i + 1
        enddo
        inx = sinx
        call copyskfc(geomnum)
        call delsflt
        call delsclp
      endif
      if (geomnum.gt.0) then
        call copyskftm(geomnum)
        call delskf
        call delskfc
      endif
      filtwd = .false.
      nextyp = snxtyp
      ifl(122) = nextyp
99999 call ncl_delete_filtlsts
      return
      end      
