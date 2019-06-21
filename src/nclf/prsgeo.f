C*********************************************************************
C*    NAME         :  prsgeo.f
C*       CONTAINS:  gtpt    gtpv    gtpvln  gtvect  sflist  sflst1
C*                  prsout
C*
C*    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*        prsgeo.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*        08/17/15 , 17:50:32
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtpt (inflg, buff, ierr)
C*       Purpose: to return the point components of a point, point-vector,
C*                or 3 scalars.
C*    PARAMETERS
C*       INPUT  :
C*          inflg    - 0 = Don't apply refsys transformation.
C*                     1 = Refsys transformation should be applied,
C*                     2 = Apply refsys transformation to point data given
C*                         by canonical form.
C*       OUTPUT :
C*          buff     - vector components.
C*          ierr     - >0 iff error
C*    RETURNS      : none
C*    SIDE EFFECTS :
C*       The point coordinates will be adjusted for METRIC, MODSYS, and
C*       TRACUT.
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine gtpt (inflg, buff, ierr)
c
      include 'com.com'
      include 'wrksys.com'
c
      logical trflg
      real*8  buff(3)
      integer*2 ierr

      real*8 pvbuf(6)
      integer*4 nclkey
      integer*2 i, ietype
      logical lsca
c
c...Initialize routine
c
      trflg = .false.
      lsca  = .false.
      if (inflg .eq. 1) trflg = .true.
c
c...Get next parameter
c
      ierr = 0
      call parsit
      if (ityp.eq.2 .and. ist.eq.POINT) then
         call gtentt (tv,trflg,nclkey,ietype,buff)
      else if (ityp.eq.2 .and. ist.eq.PNTVEC) then
         call gtentt (tv,trflg,nclkey,ietype,pvbuf)
         buff(1) = pvbuf(1)
         buff(2) = pvbuf(2)
         buff(3) = pvbuf(3)
      else if (scalar) then
         do 10 i=1,2
           buff(i) = tv
           call parsit
           if (.not. scalar) then
             ierr = 7
             goto 999
           endif
10       continue
         buff(3) = tv
         if (inflg .eq. 2) lsca = .true.
      else
         ierr = 20
      endif
c
c...Metric conversion
c
      if (ierr .eq. 0) then
          if (ifl(264) .eq. 1) then
              buff(1) = buff(1) / 25.4
              buff(2) = buff(2) / 25.4
              buff(3) = buff(3) / 25.4
          endif
c
c...Modsys
c
cc          if (lwrk) call conent (buff,WRKMX,POINT)
c
c...Refsys
c
          if (ifl(72) .eq. 1 .and. lsca) call conent (buff,SC(56),POINT)
      endif
c
c...End of routine
c
999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtpv (inflg, buff, ierr)
C*       Purpose: to return the point-vector components of a point-vector
C*                or 6 scalars.
C*    PARAMETERS
C*       INPUT  :
C*          inflg    - 0 = Don't apply refsys transformation.
C*                     1 = Refsys transformation should be applied,
C*                     2 = Apply refsys transformation to point data given
C*                         by canonical form.
C*       OUTPUT :
C*          buff     - point-vector components.
C*          ierr     - >0 iff error
C*    RETURNS      : none
C*    SIDE EFFECTS :
C*       The point coordinates will be adjusted for METRIC, MODSYS, and
C*       TRACUT.
C*    WARNINGS     : none
C********************************************************************/
      subroutine gtpv (inflg, buff, ierr)
c
      include 'com.com'
      include 'wrksys.com'
c
      real*8  buff(6)
      integer*2 ierr,inflg

      integer*4 nclkey
      integer*2 i, ietype
      logical trflg,lsca
c
c...Initialize routine
c
      trflg = .false.
      lsca  = .false.
      if (inflg .eq. 1) trflg = .true.
c
c...Get next parameter
c
      ierr = 0
      call parsit
      if (ityp.eq.2 .and. ist.eq.POINT) then
         call gtentt (tv,trflg,nclkey,ietype,buff)
         call parsit
         if (ityp.eq.2 .and. ist.eq.VECTOR) then
             call gtentt (tv,trflg,nclkey,ietype,buff(4))
         else
             ierr   = 16
         endif
      else if (ityp.eq.2 .and. ist.eq.PNTVEC) then
         call gtentt (tv,trflg,nclkey,ietype,buff)
      else if (scalar) then
         do 10 i=1,5
           buff(i) = tv
           call parsit
           if (.not. scalar) then
             ierr = 7
             goto 999
           endif
10       continue
         buff(6) = tv
         if (inflg .eq. 2) lsca = .true.
      else
         ierr = 443
      endif
c
c...Metric conversion
c
      if (ierr .eq. 0) then
          if (ifl(264) .eq. 1) then
              buff(1) = buff(1) / 25.4
              buff(2) = buff(2) / 25.4
              buff(3) = buff(3) / 25.4
          endif
c
c...Modsys
c
cc          if (lwrk) call conent (buff,WRKMX,POINT)
c
c...Refsys
c
          if (ifl(72) .eq. 1 .and. lsca) then
              call conent (buff,SC(56),POINT)
              call conent (buff(4),SC(56),VECTOR)
          endif
      endif
c
c...End of routine
c
999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtpvln (inflg, buff, ierr)
C*       Purpose: to return the point-vector components of a point-vector
C*                or 6 scalars or a line.
C*    PARAMETERS
C*       INPUT  :
C*          inflg    - 0 = Don't apply refsys transformation.
C*                     1 = Refsys transformation should be applied,
C*                     2 = Apply refsys transformation to point data given
C*                         by canonical form.
C*       OUTPUT :
C*          buff     - point-vector components.
C*          ierr     - >0 iff error
C*    RETURNS      : none
C*    SIDE EFFECTS :
C*       The point coordinates will be adjusted for METRIC, MODSYS, and
C*       TRACUT.
C*    WARNINGS     : none
C********************************************************************/
      subroutine gtpvln (inflg, buff, ierr)
c
      include 'com.com'
      include 'wrksys.com'
c
      real*8  buff(6)
      integer*2 ierr,inflg

      integer*4 nclkey
      integer*2 i, ietype
      logical trflg,lsca
c
c...Initialize routine
c
      trflg = .false.
      lsca  = .false.
      if (inflg .eq. 1) trflg = .true.
c
c...Get next parameter
c
      ierr = 0
      call parsit
      if (ityp.eq.2 .and. ist.eq.POINT) then
         call gtentt (tv,trflg,nclkey,ietype,buff)
         call parsit
         if (ityp.eq.2 .and. ist.eq.VECTOR) then
             call gtentt (tv,trflg,nclkey,ietype,buff(4))
         else
             ierr   = 16
         endif
      else if (ityp.eq.2 .and. ist.eq.PNTVEC) then
         call gtentt (tv,trflg,nclkey,ietype,buff)
      else if (ityp .eq. 2 .and. ist .eq. LINE) then
         call gtentt (tv,trflg,nclkey,ietype,buff)
         call unitvc (buff(4),buff(4))
      else if (scalar) then
         do 10 i=1,5
           buff(i) = tv
           call parsit
           if (.not. scalar) then
             ierr = 7
             goto 999
           endif
10       continue
         buff(6) = tv
         if (inflg .eq. 2) lsca = .true.
      else
         ierr = 443
      endif
c
c...Metric conversion
c
      if (ierr .eq. 0) then
          if (ifl(264) .eq. 1) then
              buff(1) = buff(1) / 25.4
              buff(2) = buff(2) / 25.4
              buff(3) = buff(3) / 25.4
          endif
c
c...Modsys
c
cc          if (lwrk) call conent (buff,WRKMX,POINT)
c
c...Refsys
c
          if (ifl(72) .eq. 1 .and. lsca) then
              call conent (buff,SC(56),POINT)
              call conent (buff(4),SC(56),VECTOR)
          endif
      endif
c
c...End of routine
c
999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtvect (inflg, buff, ierr)
C*       Purpose: to return the vector components of a vector, point-vector,
C*                or 3 scalars.
C*    PARAMETERS
C*       INPUT  :
C*          inflg    - 0 = Don't apply refsys transformation.
C*                     1 = Refsys transformation should be applied,
C*                     2 = Apply refsys transformation to point data given
C*                         by canonical form.
C*       OUTPUT :
C*          buff     - vector components.
C*          ierr     - >0 iff error
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine gtvect (inflg, buff, ierr)
c
      include 'com.com'
c
      real*8  buff(3)
      integer*2 ierr,inflg

      real*8 pvbuf(6)
      integer*4 nclkey
      integer*2 i, ietype
      logical trflg,lsca
c
c...Initialize routine
c
      trflg = .false.
      lsca  = .false.
      if (inflg .eq. 1) trflg = .true.
c
      ierr = 0
      call parsit
      if (ityp.eq.2 .and. ist.eq.VECTOR) then
         call gtentt (tv,trflg,nclkey,ietype,buff)
      else if (ityp.eq.2 .and. ist.eq.PNTVEC) then
         call gtentt (tv,trflg,nclkey,ietype,pvbuf)
         buff(1) = pvbuf(4)
         buff(2) = pvbuf(5)
         buff(3) = pvbuf(6)
      else if (scalar) then
         do 10 i=1,2
           buff(i) = tv
           call parsit
           if (.not. scalar) then
             ierr = 7
             goto 999
           endif
10       continue
         buff(3) = tv
         if (inflg .eq. 2) lsca = .true.
      else
         ierr = 11
      endif
      if (ierr .eq. 0) then
          if (ifl(72) .eq. 1 .and. lsca)
     1        call conent (buff,SC(56),VECTOR)
      endif

999   return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : sflist(kfl,keep,kns,kerr)
C*      Parses standard surface list and places the resultant keys in
C*      the SKY list.  The following lists are supported.
C*
C*         1. List of surfaces.
C*         2. Net surface.
C*         3. Layer
C*         4. All
C*
C*      The first surface should already be parsed.
C*
C*    PARAMETERS   
C*       INPUT  : 
C*          kfl     - 1 = Do not allow solids, break up NET sfs.
C*                    2 = Allow solids, keep single NET sf.
C*                    3 = Allow solids, break up NET sf.
C*          keep    - 1 = Store Composite/Net key with subcompoent
C*                        keys as negative number in list.
C*       OUTPUT :  
C*          kns     - number of surfaces in list
C*          kerr    - syntax error number if not 0.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C***********************************************************************
c
      subroutine sflist (kfl,keep,kns,kerr)
c
      include 'com8a.com'
c
      integer*2 kerr,kns,kfl,keep
c
      integer*2 inc,nsf,inet
c
      integer*4 nclkey,layer,styp,nparms
c
      real*8 params(20)
c
      integer*2 LAYERV,ALLV
c
      parameter (ALLV=816)
      parameter (LAYERV=902)
c
c...Initialize routine
c
      kerr   = 0
      inet = 2 - kfl
      call netsky (inet)
c
c...LAYER
c
      if (ityp .eq. 1 .and. ist .eq. LAYERV) then
          call parsit
          if (ityp .eq. 5 .and. ist .eq. 1) call parsit
          if (.not. scalar) go to 9000
          layer = tv
          call addsky (1,layer,1,kns)
          if (kns .eq. 0) kerr = 1
          call parsit
c
c...ALL
c
      else if (ityp .eq. 1 .and. ist .eq. ALLV) then
          call addsky (3,layer,1,kns)
          if (kns .eq. 0) kerr = 1
          call parsit
c
c...Surfaces
c
      else if (ityp .eq. 2 .and. ist .eq. SURF .or. 
     1         (ist .eq. VSOLID .and. kfl .ge. 2)) then
  400     call gtdesc (tv,nclkey,nwds,ietype)
          ityp12 = 0
          if (ist .eq. SURF) then
              call sftype (nclkey,ityp12)
          else
              call nclf_get_solid (nclkey,styp,params,nparms)
          endif
c
c......NET surface
c
          if (ist .eq. SURF .and. ityp12 .eq. NETSF .and. kfl .ne. 2)
     1          then
              if (keep .eq. 1) call addskw (0,nclkey,1,kns)
              inc = 1
  500         call ntsf1 (nclkey,layer,inc,nsf)
              call addsky (0,layer,1,kns)
              inc = inc + 1
              if (inc .le. nsf) go to 500
c
c......Composite solid
c
          else if (ist .eq. VSOLID .and. styp .eq. 10) then
              if (keep .eq. 1) call addskw (0,nclkey,1,kns)
              inc = 1
  600         call nclf_get_solid_comp (nclkey,layer,inc,nsf)
              call addsky (0,layer,1,kns)
              inc = inc + 1
              if (inc .le. nsf) go to 600
c
c......Standard surface
c
          else
              call addsky (0,nclkey,1,kns)
          endif
          call parsit
          if (ityp .eq. 2 .and. (ist .eq. SURF .or. ist .eq. VSOLID))
     1        go to 400
c
c...Unrecognized entity for surface list
c
      else
          go to 9100
      endif
c
c...End of routine
c
 8000 call netsky (1)
      return
c
c...Scalar expected
c
 9000 kerr   = 7
      go to 8000
c
c...Surface expected
c
 9100 kerr   = 186
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : sflst1 (kns,sf,ierr) 
C*      parses surface list for PODDEF command.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          kns     - number of surfaces in list
C*          sf(kns) - array (R*8) of asws for surfaces 
C*          ierr    - sytax error number if not 0.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C***********************************************************************
c
      subroutine sflst1 (kns,sf,ierr)
c
      include 'com8a.com'
c
      integer*4 kns
      integer*2 ierr
      real*8 sf(*)
c
      integer*2 nsvi
c
      ierr   = 0
      kns    = 0
  400 nsvi   = INX
      call parsit
      if (ITYP .eq. 2 .and. (IST .eq. 9 .or. IST .eq. 6)) then
          kns    = kns + 1
          sf(kns)  = TV
      else if (kns .eq. 0) then
          go to 9026
      else
          INX   = nsvi
          go to 8000
      end if
c
c...Check for EOS
c
      if (NEXTYP .ne. 11) then
          if (kns .lt. 25) then
             go to 400
          else
             go to 9015
          end if
      end if
c
c...Errors
c
 9001 ierr   = 1
      go to 8000
 9015 ierr   = 15
      go to 8000
 9026 ierr   = 326
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : prsout (nclkey,kfl)
C*      Parses CV-SF/OUT command for extracting components from a
C*      composite entity.
C*
C*      Syntax:  geo/OUT,geo1,ALL         (,NUM,val)
C*                            (m,n,THRU,o)
C*
C*      'geo1' should already be parsed before calling this routine.
C*
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey  - Key of composite entity.
C*          kfl     - 1 = Composite entity is a curve.
C*                    2 = Composite entity is a net surface or
C*                        composite solid.
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C***********************************************************************
c
      subroutine prsout (nclkey,kfl)
c
      include 'com8a.com'
      include 'vocab.com'
c
      integer*2 kfl
      integer*4 nclkey
c
      integer*2 nwds,sidst,i,j,jj,j0,j1
      integer*4 srest,ssub
c
      character*64 stok
c
      integer*2 NUMV
      parameter (NUMV = 563)
c
c..... cv/OUT,ccv,ALL (1, 3,THRU,5) (,NUM,value)
c.......Modified parsing to accommodate NUM,value in parsing
c.......Andrew 11/12/12
c
c
c...Initialize routine
c
      i = -1
      nwds = 0
      srest = 0
      sidst = idst
      stok = savid2
      ssub = isvsub
      srest = rest
c
c...Get next token
c
      call parsit
c
c...OUT,ALL
c
      if (vocab .and. voc .eq. ALL) then
          i = 0
          call parsit
          call gtccnm (nclkey,nwds)
c
c...OUT,m [,THRU,n]
c
      else
          jj = 0
  100     if (ityp .eq. 1 .and. ist .eq. THRU) then
              j0 = jj + 1
              call parsit
              if (scalar) then
                  j1 = tv
                  do jj=j0,j1,1
                      call addskj(jj,i)
                  enddo
                  nwds = nwds + j1-j0+1
              endif
          else if (scalar) then
              jj = itv
              call addskj(jj,i)
              isc10(4) = jj
              nwds = nwds + 1
          else
              go to 200
          endif
c
c......Get next token
c
          idtype = -1
          call parsit
          go to 100
      endif
c
c...OUT,NUM,val
c......Variable for storing number of components extracted
c
  200 if (ityp .eq. 1 .and. ist .eq. NUMV) then
          call parsit
          if (ityp .eq. 2.and. ist .le. 2) then
              idst = 2
              savid2 = token2
              isvsub = ivxsub
              rest = nwds
              call vstore
              call parsit
              idst = sidst
              savid2 = stok
              isvsub = ssub
              rest = srest
          else
              call error(282)
          endif
      else if (ityp .ne. 7) then
          call error(7) ! scalar expected
      endif
c
c....Store number of values in component list
c
      isc10(3) = i
      if (i .lt. 0) then
          call error(358) ! value too small
          goto 9000
      endif
c
c...ENd of routine
c
 8000 return
c
c...An error occured
c
 9000 if (i .gt. 0) call delsky
      go to 8000
      end
