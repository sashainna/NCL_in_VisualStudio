C*********************************************************************
C*    NAME         :  profil.f
C*       CONTAINS:
C*           profil  prfps   prfcpl  prfpos  prfret  prfout  prffot
C*           prffdo  prfcir
C*    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       profil.f , 25.3
C*    DATE AND TIME OF LAST  MODIFICATION
C*       11/22/17 , 11:19:37
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine profil
c*      This is the controlling routine for creating motion around a
c*      profile.
c*
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
      subroutine profil
c
      include 'com8a.com'
      include 'prfcom.com'
      include 'comgt.com'
c
      integer*2 ktv(4)
      integer*4 jtv(2)
      equivalence (tv,jtv,ktv)
c
      real*8 FEEDR
      equivalence (FEEDR ,MOTMAP(24))
c
      integer*2 ietype,ipstyp,isrf,ier,lrev
      integer*4 nclkey,npts,npas,pskey,iw(5),jerr,ipt,js,je,ji,ks,ke,
     1          ki,ls,le,li,j,k,l,iclose,ient,iret,icvtyp,ictcom,itsk
c
      real*8 dis,dtol,fstep,foffs,rdep,fpas,dstep,pt3(3),scpt(3), 
     1      scptt(3)
c
      data iw /0,1,2,3,4/
c
      integer*2 PASS
      parameter (PASS   = 755)
c
c...Initialize the routine
c
	ier = 0
      PRFFSV = FEEDR
      if (IPRFFD(1) .eq. 0 .or. IPRFFD(1) .eq. 2) then
          PRFFED(1) = FEEDR
      else if (IPRFFD(1) .eq. 3) then
          PRFFED(1) = FEEDR * PRFFED(1)
      endif
      IPRFFD(1) = 1
      ictcom = 0
c
c...Get the part surface
c
      if (IPRFFL(4) .eq. 2) call gtdesc (PRFTV(2),pskey,nwds,ipstyp)
c
c...Initiate the curve evaluation parameters
c
      dtol   = sc(27)
      if (IPRFFL(3) .eq. 1) then
          foffs  = PRFTHK(1)
      else
          foffs    = sc(28) / 2. + PRFTHK(1)
      endif
      fpas   = sc(29)
c
c...Convert all parameters to inches
c
      call vctovc (sc(1),scpt)
      call vctovc (sc(1),scptt)
c
      if (ifl(264) .eq. 1) then
          dtol  = dtol/25.4d0
          fpas  = fpas/25.4d0
          foffs = foffs/25.4d0
          call vctmsc (scpt,scpt,1.0d0/25.4d0)
c
          if (IPRFFL(7) .eq. 2 .or. IPRFFL(7) .eq. 3) then
              PRFCLF(1) = PRFCLF(1)/25.4d0
          else
              PRFCLF(4) = PRFCLF(4)/25.4d0
          endif
c
          PRFDEP(1) = PRFDEP(1)/25.4d0
          PRFDEP(3) = PRFDEP(3)/25.4d0
c
          PRFENT(1) = PRFENT(1)/25.4d0
          PRFENT(2) = PRFENT(2)/25.4d0
          PRFENT(3) = PRFENT(3)/25.4d0
          PRFENT(5) = PRFENT(5)/25.4d0
          PRFENT(6) = PRFENT(6)/25.4d0
          PRFEXI(1) = PRFEXI(1)/25.4d0
          PRFEXI(2) = PRFEXI(2)/25.4d0
          PRFEXI(3) = PRFEXI(3)/25.4d0
          PRFEXI(5) = PRFEXI(5)/25.4d0
          PRFEXI(6) = PRFEXI(6)/25.4d0
c
          PRFLAP(1) = PRFLAP(1)/25.4d0
          PRFLAP(2) = PRFLAP(2)/25.4d0
c
          PRFPAS(1) = PRFPAS(1)/25.4d0
          PRFPAS(3) = PRFPAS(3)/25.4d0
c
          PRFPLN(4) = PRFPLN(4)/25.4d0
          if (IPRFFL(12) .eq. 3) then
              PRFRPL(1) = PRFRPL(1)/25.4d0
          else
              PRFRPL(4) = PRFRPL(4)/25.4d0
          endif
c
          if (IPRFFL(19) .eq. 2 .or. IPRFFL(19) .eq. 3) then
              PRFPPL(1) = PRFPPL(1)/25.4d0
          else
              PRFPPL(4) = PRFPPL(4)/25.4d0
          endif
c
          call vctmsc (PRFSPT,PRFSPT,1.d0/25.4d0)
          call vctmsc (PRFEPT,PRFEPT,1.d0/25.4d0)
c
          PRFTHK(1) = PRFTHK(1)/25.4d0
          PRFTHK(2) = PRFTHK(2)/25.4d0
c
          if (IPRFFL(6) .eq. 3) then
              PRFTPL(1) = PRFTPL(1)/25.4d0
          else
              PRFTPL(4) = PRFTPL(4)/25.4d0
          endif
c
          PRFMXD = PRFMXD/25.4d0
c
          PRFTLT(1) = PRFTLT(1)/25.4d0
          PRFTLT(2) = PRFTLT(2)/25.4d0
      endif
c
c...Evaluate the input curve
c
      call gtdesc (PRFTV(1),nclkey,nwds,ietype)
c
c ...Set annotation depth from PASS number and max-step if no PS depth is given
c     
      if (ietype .eq. VANOTE .and. IPRFFL(4) .eq. 0 .and. 
     1    IPRFFL(6) .eq. 1 .and. PRFTHK(2) .eq. 0.) then
          PRFTHK(2) = - PRFDEP(2)*PRFDEP(3)
      endif      
c
c...Store the part surface
c
      call ncl_profile_ps (pskey,IPRFFL(4),IPRFFL(6),PRFPLN,IPRFFL(8),
     1                     sc(4),PRFTHK(2),IPRFFL(21),PRFTLT)

c
c......Annotation
c
      if (ietype .eq. VANOTE) then
          icvtyp = 0
          call ncl_profile_anote (nclkey,npts,npas)
c
c......Curve
c
      else
          icvtyp = 1
          isrf   = 3
          call evstup (nclkey,isrf)
          lrev = 0
          call ncl_profile_curve (dtol,npts,lrev)
      endif
      if (npts .le. 1) go to 9000
c
c...Calculate the correct starting point
c
      call ncl_profile_start (PRFSPT,scpt,IPRFFL(15),IPRFFL(5),PRFIVC,
     1                        PRFNVC,IPRFFL(17),IPRFFL(14),PRFEPT,dtol,
     2                                                iclose,npts)
      if (npts .le. 1) go to 9000
c
c...Calculate the number of passes to take
c......Annotation
c
      if (icvtyp .eq. 0) then
          fstep = 0.
c
c......Tool condition is ON
c......Ignore number of passes
c
      else if (IPRFFL(3) .eq. 1) then
          npas   = 1
          fstep  = 0.
c
c......Based on passes & step distance
c
      else if (IPRFFL(1) .eq. 1) then
          npas   = PRFPAS(2)
          fstep  = PRFPAS(3)
c
c......Based on thickness & passes
c
      else if (IPRFFL(2) .eq. 1) then
          npas   = PRFPAS(2)
          if (npas .le. 1) then
              fstep  = 0.
          else
              fstep  = PRFPAS(1) / (npas-1)
          endif
c
c......Based on thickness & step distance
c
      else
          npas   =  PRFPAS(1) / PRFPAS(3) + 1
          if (npas .le. 1) then
              fstep  = 0.
          else
              fstep  = PRFPAS(1) / (npas-1)
          endif
      endif
c
c...Create and store all offset curves first
c...to save time
c...All offset curves must be created, even
c...if the first offset is the same as the original curve
c
      if (icvtyp .eq. 1) then
          do 200 k=0,npas-1,1
              dis    = foffs + fstep*k
              l = (k+1) * (-1)
              call ncl_profile_offset (iw(1),l,npas,dis,PRFNVC,dtol,
     1            npts)
              if (npts .lt. 0) ier = 518
              if (npts .le. 1) go to 9000
  200     continue
      endif
c
c...Calculate the Z-level planes
c...Bottom plane, Top-of-part, Clearance plane
c
      call prfcpl (npas,foffs,fstep,dtol,icvtyp,npts,jerr)
      if (jerr .ne. 0) go to 9000
c
c...Re-Store the part surface
c...After possibly calculating part surface plane
c
      call ncl_profile_ps (pskey,IPRFFL(4),IPRFFL(6),PRFPLN,IPRFFL(8),
     1                     sc(4),PRFTHK(2),IPRFFL(21),PRFTLT)
c
c...Calculate the number of depth levels
c...Based on number of passes
c...PRFPLN(4)is already the lowest level for annotation
c
      if (icvtyp .eq. 0 ) then
         rdep   = PRFTPL(4) - PRFPLN(4)
      else
         rdep   = PRFTPL(4) - PRFPLN(4) - PRFTHK(2)
      endif
      
      if (IPRFFL(6) .eq. 1 .and. icvtyp .eq. 1) then
          ndep   = PRFDEP(2)
          dstep  = PRFDEP(3)
c
c......Based on thickness & passes
c
      else if (IPRFFL(9) .eq. 1) then
          ndep   = PRFDEP(2)
          if (ndep .le. 1) then
              dstep  = 0.
          else
              dstep  = rdep   / ndep
          endif
c
c......Based on thickness & step distance
c
      else
          ndep   = rdep   / PRFDEP(3) + .99
          if (ndep .le. 1) then
              dstep  = 0.
          else
              dstep  = rdep   / ndep
          endif
      endif
c
c...Determine the looping regions
c
      if (icvtyp .eq. 0) then
          ks = 0
          ke = npas - 1
          ki = 1
      else
          ks = npas - 1
          ke = 0
          ki = -1
      endif
c
      if (IPRFFL(10) .eq. 1) then
          js = ndep - 1
          je = 0
          ji = -1
c
          li = 1
      else
          js = 1
          je = 1
          ji = 1
c
          ls = ndep - 1
          le = 0
          li = -1
      endif
c
c...Loop through the passes
c...Outer loop is for Depth curve when Offset is calculated first
c...Second loop is for Offset curve
c...Inner loop is for Depth curve
c
      do 1000 j=js,je,ji
          if (ji .eq. -1) then
              ls = j
              le = j
          endif
c
c......Calculate the correct offset curve
c
          do 800 k=ks,ke,ki
              dis    = foffs + fstep*k
              if (icvtyp .eq. 0 .or. dis .ne. 0.) then
                  inc    = 2
                  call ncl_profile_offset (iw(1),k,npas,dis,PRFNVC,dtol,
     1                npts)
                  if (npts .le. 1) go to 9000
              else
                  inc    = 1
              endif
c
c......Calculate the Z-level curve
c
              incsv  = inc
              
              do 600 l=ls,le,li
                  dis    = PRFTHK(2) + dstep*l
                  if (IPRFFL(4) .ne. 0 .or.
     1                    dis .ne. 0. .or.
     2                    (icvtyp .eq. 0 .and. dstep .ne. 0) .or.
     3                    IPRFFL(8) .eq. 2) then
                      ipt    = 1

                     
                      call ncl_profile_project (iw(incsv),dis,ipt,npts,
     1                    sc(201), ier)
                      
                      
                      if (npts .le. 1 .or. ier.gt.0) go to 9000
                      inc    = 3
                  else
                      inc    = incsv
                  endif
c
c......Prepare the curve for output
c
                  
 !                   if (icvtyp .eq. 1 .and. PRFTLT(3) .eq. 0  .and. 
 !    1                  PRFTLT(4) .eq. 0) then
                     if (icvtyp .eq. 1 ) then
                      call ncl_profile_weed (iw(inc),PRFLAP,dtol,
     1                IPRFFL(16),PRFMXD,npts)
                      if (npts .eq. 0) go to 9000
                  endif
                  inc    = 4
c
c......Determine the entry type
c
                  if ((j .eq. js .and. k .eq. ks .and. l .eq. ls) .or.
     1                    (iclose .eq. 0 .and. icvtyp .eq. 1)) then
cc     2                    (icvtyp .eq. 0 .and. PRFEXI(4) .eq. 0.)) then
                      ient   = 1
                  else if (IPRFFL(10) .eq. 2 .or. k .eq. ks .or.
     1                     IPRFFL(18) .ne. 0 .or. icvtyp .eq. 0) then
                      ient   = 3
                  else
                      ient   = 2
                  endif
                  lstdis = dis          
c
c......Check if CUTCOM will be added or not
c                       
                  if (iptcom .gt. 1 .and. (IPRFFL(20) .eq. 0 .or. 
     x                (IPRFFL(20) .eq. 1 .and. k .eq. ks))) then
                    ictcom = 1
                  endif
c
c.....Initialize whead0 for circle entry interplation
c
                  istk = 0
                  call ncl_free_circ(itsk)           
c
c......Position the tool
c                 
                  call prfpos (ient,dtol,icvtyp,ictcom)
c
c.....Initialize whead0 for circle pass interplation
c
                  call ncl_free_circ(itsk)                  
c
c......Output the pass around the curve
c
                  call prfout (1,iw(inc),ient,pt3,npts)
c
c......Determine the exit type
c
                      ient   = 1
                  if ((j .eq. je .and. k .eq. ke .and. l .eq. le) .or.
     1                   (iclose .eq. 0 .and. icvtyp .eq. 1)) then
c     2                    (icvtyp .eq. 0 .and. PRFEXI(4) .eq. 0.)) then
                      iret   = 1
                  else if (IPRFFL(10) .eq. 2 .or. k .eq. ke .or.
     1                     IPRFFL(18) .ne. 0 .or. icvtyp .eq. 0) then
                      iret   = 3
                  else
                      iret   = 2
                  endif                 
c
c.....Initialize whead0 for circle exit interplation
c                           
                  call ncl_free_circ(itsk)  
c
c......Retract the tool
c
                  call prfret (iret,icvtyp,npts,ictcom)
                  ictcom = 0
  600         continue
  800     continue
 1000 continue
c
c...Mark there being enough points for filleting
c
      if (ifl(347) .ne. 0) ifl(347) = 3
c
c...End of routine
c
 8000 call ncl_profile_free
c      call vctovc (scptt,sc(1))
      return
c
c...Could not generate curve
c
 9000 if (ier .gt. 0) then
        ifl(2) = ier
      else
        ifl(2) = 213
      endif
      err    = .true.
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine prfps (kwhich,khow,ginp,gvec,gdis,
C*                                         gout,gnorm,kinit,ier)
c*      Projects a profile curve point onto a canted plane or surface
c*      when used as the part surface.
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          kwhich   = 1 = Project to PS, 2 = Secondary PS.
C*          khow     = 1 = Project to surface, 2 = Project on surface.
C*          ginp     = Input location.
C*          gvec     = Projection plane vector (tool axis).
C*          gdis     = Offset distance (thick).
C*          kinit    = 0 = Initialize U,V parameters.
C*       OUTPUT :  
C*          gout     = Output location.
C*          gnorm    = Surface normal at contact point.  Usually only
C*                     used when 'khow = 2'.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine prfps (kwhich,khow,ginp,gvec,gdis,gout,gnorm,kinit,ier)
c
      include 'com8a.com'
      include 'mocom.com'
      include 'prfcom.com'
c
      integer*4 kwhich,khow,kinit
      integer*2 ier
c
      real*8 ginp(3),gout(3),gvec(3),gnorm(3),gdis
c
      integer*4 inc
c
      real*8 rsav,conpt(3)
c
      integer*4 nclkey
      integer*2 nwds,ietype
c
c...Initialize the routine
c
      rsav   = sc(23)
      sc(23) = gdis
      if (ifl(264) .eq. 1) sc(23) = sc(23) * 25.4d0
      inc    = kwhich + 1
c
c...Convert to metric
c
      call vctovc (ginp,gout)
      if (ifl(264) .eq. 1) call vctmsc (gout,gout,25.4d0)
c
c...Initialize the Part Surface
c
      if (kinit .eq. 0) then
        call gtdesc (PRFTV(inc), nclkey, nwds, ietype)
        if (ietype .ne. SURF) then
          call psinit (PRFTV(inc),gout,gvec)
        else
          call psini1 (khow,PRFTV(inc),gout,gvec)
        endif
        if (ifl(2) .gt. 0) then
          ier = ifl(2)
          return
        endif
      endif
c
c...Project te tool to the Part Surface
c
      ia = ifl(51)

      call conv8_4 (gvec,t(4,ia),3)

      call tltos1 (khow,gout,gvec,conpt,gnorm,PRFTV(inc))

      if (ifl(2) .gt. 0) then
        ier = ifl(2)
        return
      endif
c
c...Convert from metric
c
      if (ifl(264) .eq. 1) call vctmsc (gout,gout,1.d0/25.4d0)
c
c...Reset global flags
c
      sc(23) = rsav
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : prfcpl (knpass,gthk,gstep,gtol,ktyp,knpts,kerr)
c*      Calculates the lowest level, Top-of-part, clearance, rapto, and
c*      retract planes.  These planes only need to be re-calculated with
c*      the following conditions.
c*
c*          Lowest Level    - Part surface is a canted plane or surface or
c*                            is not defined.
c*          Top-of-part     - Defined as distance above Lowest Level.
c*          Clearance Plane - Defined as distance above Top-of-part.
c*          Rapto Plane     - Defined as distance above Top-of-part.
c*          Retract Plane   - Defined as distance above Top-of-part.
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          knpass   I*4   D1   - Number of passes to take around profile.
C*          gthk     R*8   D1   - Drive surface thick.
C*          gstep    R*8   D1   - Drive surface step size.
C*          gtol     R*8   D1   - Geometry tolerance.
C*          ktyp     I*4   D1   - 0 = Annotation, 1 = Curve.
C*          knpts    I*4   D1   - Number of points in evaluated curve.
C*       OUTPUT :  
C*          kerr     I*4   D1   - Returns 1 if an error occurred.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine prfcpl (knpass,gthk,gstep,gtol,ktyp,knpts,kerr)
c
      include 'com8a.com'
      include 'prfcom.com'
c
      integer*4 knpass,kerr,knpts
c
      real*8 gthk,gstep,gtol
c
      integer*4 i,j,iw(5),npts,inc,npt,imx,ipt,vcpara,ktyp
      integer*2 ier
c
      real*8 pt(6),tmppl(4),dis,pt1(3)
      real*8 vefs(3), vela(3)
c
      data iw /0,1,2,3,4/
c
c...Initialize routine
c
      kerr   = 0
      npts   = knpts
      imx    = 6
      inc    = 1
c
c...Recalculate the lowest level
c...By looping through each pass
c
      if (IPRFFL(4) .eq. 0 .or. IPRFFL(4) .eq. 2) then
          call vctovc (sc(4),PRFPLN)
          PRFPLN(4) = 10000.
          ipt    = 1
          do 500 i=knpass-1,0,-1
c
c......Calculate the correct offset curve
c
              dis    = gthk   + gstep*i
              if (dis .ne. 0. .or. ktyp .eq. 0) then
                  inc    = 2
                  call ncl_profile_offset (iw(1),i,knpass,dis,PRFNVC,
     1                gtol,npts)
                  if (npts .le. 1) go to 9000
              else
                  inc    = 1
              endif
c
c......Calculate the Z-level curve
c
              if (IPRFFL(4) .ne. 0 .or. PRFTHK(2) .ne. 0. .or.
     1                IPRFFL(8) .eq. 2) then
                  call ncl_profile_project (iw(inc),PRFTHK(2),ipt,npts, 
     1                                    sc(201), ier)
                  if (npts .le. 1 .or. ier.gt.0) go to 9000
                  inc    = 3
              endif
c
c......Determine lowest planar level
c
              npt    = 1
              j = 1
                  call ncl_profile_pt (iw(inc),j,imx,npt,pt)

      if (IPRFFL(21) .eq. 1 .or. IPRFFL(21) .eq. 2) then
           call nclf_getpv_value (vefs,vela)
           call vctovc (vefs,pt(4))
      endif
                      call plnvpt (pt(4),pt,PRFPLN,kerr)
c             It looks like only the offset plane through the first profile point can properly make the job
c             That why the above comms out. Sasha, Aug.16-17, 2017
  500     continue
      endif
c
c...Calculate the Top-of-part
c......Based on passes & step
c
      if (IPRFFL(6) .eq. 1) then
          if (ktyp .eq. 0) then
              call ncl_profile_pt (iw(2),npt,imx,npt,pt)
              call plnvpt (PRFPLN,pt,PRFTPL,kerr)
          else if (IPRFFL(8) .eq. 2) then
              call ncl_profile_pt (iw(inc),npt,imx,npt,pt)
              call plnvpt (PRFPLN,pt,PRFTPL,kerr)
              PRFTPL(4) = PRFTPL(4) + (PRFDEP(2)-1)*PRFDEP(3)
c              IPRFFL(6) = 3
          else
              call vctovc (PRFPLN,PRFTPL)
              PRFTPL(4) = PRFPLN(4) + (PRFDEP(2)-1)*PRFDEP(3)
          endif
c
c......Based on part thickness
c
      else if (IPRFFL(6) .eq. 3) then
          if (PRFDEP(1) .eq. 0.) then
              call ncl_profile_pt (iw(inc),npt,imx,npt,pt)
              call plnvpt (PRFPLN,pt,PRFTPL,kerr)
          else
              PRFTPL(4) = PRFPLN(4) + PRFDEP(1)
              call vctovc (PRFPLN,PRFTPL)
          endif
c
c......User provided
c......Make sure it is parallel to part surface plane
c......When profiling normal to PS
c
      else if (IPRFFL(8) .ne. 0) then
          if (vcpara(PRFPLN,PRFTPL) .eq. 0) then
              call ncl_profile_pt (iw(inc),npt,imx,npt,pt)
              call plnint (pt,pt(4),PRFTPL,pt1,kerr)
              call plnvpt (PRFPLN,pt1,PRFTPL,kerr)
          endif
      endif
c
c...Calculate the clearance plane
c
      if (IPRFFL(7) .eq. 2) then
          PRFCLF(4) = PRFTPL(4) + PRFCLF(1)
          call vctovc (PRFTPL,PRFCLF)
      endif
c
c...Calculate the rapto plane
c
      if (IPRFFL(19) .eq. 2) then
          PRFPPL(4) = PRFTPL(4) + PRFPPL(1)
          call vctovc (PRFTPL,PRFPPL)
      endif
c
c...Calculate the retract plane
c
      if (IPRFFL(12) .eq. 3) then
          PRFRPL(4) = PRFTPL(4) + PRFRPL(1)
          call vctovc (PRFTPL,PRFRPL)
      endif
c
c...Calculate the retract distance
c...for annotation based on the
c...Top-of-Part and 1st Point of curve
c
      if (ktyp .eq. 0 .and. IPRFFL(6) .ne. 2) then
          inc    = 1
          call ncl_profile_pt (iw(inc),npt,imx,npt,pt)
          call plndis (PRFTPL,pt,dis)
          PRFEXI(6) = PRFEXI(6) - dabs(dis)
      endif
c
c...End of routine
c
 8000 return
c
c...Error calculating plane(s)
c
 9000 kerr   = 1
      if (ier .gt. 0) kerr = ier
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : prfpos (kent,dtol,ktyp,kctcom)
c*      Positions the tool prior to taking a pass around the profile.
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          kent     I*4   D1   - Entry type.  1 = Initial entry,
C*                                2 = Transitional, 3 = Depth.
C*          dtol     R*8   D1   - Geometry tolerance.
C*          ktyp     I*4   D1   - 0 = Annotation, 1 = Curve.
C*          kctcom   I*4        - 1 = CUTCOM, 0 = No CUTCOM 
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine prfpos (kent,dtol,ktyp,kctcom)
c
      include 'com8a.com'
      include 'prfcom.com'
c
      integer*4 kent,ktyp,kctcom
c
      real*8 dtol
c
      integer*4 npt,jerr,inc,imx,nent,iw(5)
c
      logical lsame,idid,ifirst
c
      real*8 tmppl(4),pt(6),pt1(6),scpt(6),stpl(4),curpl(4),ept(6),
     1       cpl(4),ptent(6)

      real*8 vefst(3), velas(3)
c
      data iw /0,1,2,3,4/
c
c...Do not perform positioning moves
c...if not initial entry
c
      if (kent .eq. 2) go to 8000
c
c...Disable ARCSLP/FILLET on positioning moves
c
      call asfrst
c
c...Initialize routine
c
      lsame = .false.
      npt    = 1
      inc    = 3
      if (ktyp .eq. 0) inc = 1
      imx    = 6
      call vctovc (sc(4),pt1(4))
c
c...Calculate point coming from
c
      if (ifl(264) .eq. 1) then
          call vctmsc (sc(1),scpt,1.0d0/25.4d0)
      else
          call vctovc (sc(1),scpt)
      endif
      call vctovc (sc(4),scpt(4))   
c
c...Calculate starting and entry points
c
      call ncl_profile_entry (inc,1,IPRFFL(11),IPRFFL(3),kctcom,PRFENT,
     x                        pt,ptent,nent)
c
c...Recall the surface normal vector it tilted angle active
c
      if (IPRFFL(21) .eq. 1 .or. IPRFFL(21) .eq. 2) then
           call nclf_getpv_value (vefst,velas)
           call vctovc (vefst,pt(4))
      endif
      if (ktyp .eq. 0 .and. IPRFFL(6) .eq. 2) then
          call vctovc (PRFTPL,stpl)
          stpl(4) = PRFTPL(4)
      else
          call plnvpt (pt(4),pt,stpl,jerr)
      endif
      call ncl_profile_pt (inc,npt,imx,npt,ept)
c
c...Calculate current tool position plane
c
      if (ktyp .eq. 1) then
          call plnvpt (PRFTPL,scpt,curpl,jerr)
      else
          call plnvpt (scpt(4),scpt,curpl,jerr)
      endif
c
c...Determine if tool is already
c...positioned at entry point
c
      call plnint (pt,pt(4),curpl,pt1,jerr)
      if (f_dist(pt1,scpt) .le. dtol) lsame = .true.
c
c...Calculate Clearance plane
c
      if (kent .eq. 1) then
          if (IPRFFL(7) .eq. 3) then
              call uvcplvc (ept,stpl,pt1,PRFCLF(1))
          else
              call plnint (ept,stpl,PRFCLF,pt1,jerr)
          endif
      endif
c
c...Position above entry point
c
      if (.not. lsame) then
          idid   = .false.
          ifirst = .false.
          call plnvpt (stpl,pt1,tmppl,jerr)
          if (tmppl(4) .gt. (stpl(4)+dtol) .or. 
     1        (ktyp .eq. 0 .and. PRFEXI(6) .eq. 0. .and.
     2         tmppl(4) .gt. (stpl(4)-dtol)) .or.
     3        (ktyp .eq. 1 .and. IPRFFL(11) .eq. 3 .and.
     4         PRFENT(3) .gt. 0. .and. tmppl(4).gt.(stpl(4)-dtol))) then
              call plnint (pt,stpl,tmppl,pt1,jerr)
              idid   = .true.
              ifirst = .true.
          else if (tmppl(4) .lt. (stpl(4)-dtol)) then
c
c.... Should use the curve end pt instead of current point
c...              call plnint (scpt,stpl,stpl,pt1,jerr)
c
              call plnint (ept,stpl,stpl,pt1,jerr)
              idid   = .true.
          endif
          if (IPRFFL(21) .eq. 1 .or. IPRFFL(21) .eq. 2) then
                call vctovc (ptent(4),pt(4))
          endif
            if (idid) then
              if (IPRFFL(18) .ne. 2 .or. ifirst) then
                call vctovc (pt(4),pt1(4))
                call prffdo (2)
                call prfout (2,iw(1),kent,pt1,npt)
                call vctovc (pt1,scpt)
                call vctovc (pt1(4),scpt(4))
              endif
            endif
      endif
c
c...Position to Rapto plane
c
      if (kent .eq. 1 .or. ktyp .eq. 0) then
          idid   = .false.
          if (IPRFFL(19) .eq. 0 .or. IPRFFL(19) .eq. 3) then
              call uvcplvc (ept,stpl,pt1,PRFPPL(1))
          else
              call plnint (ept,stpl,PRFPPL,pt1,jerr)
          endif
          call plnvpt (stpl,pt1,tmppl,jerr)
          call plnvpt (stpl,scpt,cpl,jerr)
          if (tmppl(4) .lt. stpl(4)) tmppl(4) = stpl(4)
          if (cpl(4) .gt. tmppl(4)+dtol .and.
     1            (tmppl(4) .gt. stpl(4)+dtol .or.
     2            (IPRFFL(19) .ne. 0 .or. IPRFFL(11) .ne. 0))) then
             if (IPRFFL(21) .eq. 1 .or. IPRFFL(21) .eq. 2) then
                  call vctovc (vefst,ept(4))
                  call plnint (ept,ept(4),tmppl,pt1,jerr)
	      else
                  call plnint (scpt,ept(4),tmppl,pt1,jerr)
              endif
              call vctovc (pt(4),pt1(4))
              call prffdo (2)
              call prfout (2,iw(1),kent,pt1,npt)
              call vctovc (pt1,scpt)
              call vctovc (pt1(4),scpt(4))
          endif
      endif
c
c...Position to entry point for Arc entry without arc rise value
c
      if (f_dist(pt,scpt) .gt. dtol .and. IPRFFL(11) .eq. 1 .and.
     x    PRFENT(2) .eq. 0.0) then
c
c... modified by KC
c... wrong fedrat mode specified. original "call prffdo(2)
c... 2 means position, i.e. rapid, 4 means entry mode feedrate, 
c
         call prffdo (4)
         call prfout (2,iw(1),kent,pt,npt)
         call vctovc (pt,scpt)
         call vctovc (pt(4),scpt(4)) 
      endif  
c
c...Perform Cutter compensation
c      
      if (kctcom.eq.1) then
c
c...Position to positional distance start point if not yet
c        
         if (f_dist(pt,scpt) .gt. dtol) then
            call prffdo (2)
            call prfout (2,iw(1),kent,pt,npt)
            call vctovc (pt,scpt)
            call vctovc (pt(4),scpt(4))      
         endif     
       
         call putcl(2000,1007,iptcom,prrct)
      endif 
c
c...positional distance start point to first entry point
c
      if (kctcom.eq.1 .and. PRFENT(6).gt.dtol) then
         call prffdo (4)
         call prfout (2,iw(1),kent,ptent,npt)
         call vctovc (ptent,scpt)
         call vctovc (ptent(4),scpt(4)) 
      endif      
c
c...Perform entry move
c
 7000 if (nent .ne. 0) then
          call prffdo (4)
          call prfout (1,iw(5),kent,ptent,nent)
      endif
c
c...Save entry point
c
      if (ifl(264) .eq. 1) then
          call vctmsc (scpt,sc(1),25.4d0)
      else
          call vctovc (scpt,sc(1))
      endif
      call vctovc (scpt(4),sc(4))
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : prfret (kent,ktyp,knpts,kctcom)
c*      Retracts the tool after taking a pass around the profile.
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          kent     I*4   D1   - Exit type.  1 = Final pass,
C*                                2 = Transitional, 3 = Depth.
C*          ktyp     I*4   D1   - 0 = Annotation, 1 = Curve.
C*          knpts    I*4   D1   - Number of points in weeded curve.
C*          kctcom   I*4        - 1 = CUTCOM, 0 = No CUTCOM 
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine prfret (kent,ktyp,knpts,kctcom)
c
      include 'com8a.com'
      include 'prfcom.com'
c
      integer*4 kent,ktyp,knpts,kctcom
c
      integer*4 npt,jerr,inc,nent,iw(5),imx
c
      logical idid
c
      real*8 pt(6),pt1(6),scpt(6),curpl(4),retpl(4),ept(6),
     1       tmppl(4),ptent(6)
c
      real*8 vefst(3), velas(3)
c
      data iw /0,1,2,3,4/
c
c...Do not perform exit moves
c...on offset transitions
c
      if (kent .eq. 2) go to 8000
c
c...Disable ARCSLP/FILLET on positioning moves
c
      call asfrst
c
c...Initialize routine
c
      npt    = 1
      inc    = 3
      if (ktyp .eq. 0) inc = 1
      imx    = 6
      call vctovc (sc(4),pt1(4))
c
c...Calculate point coming from
c
      if (ifl(264) .eq. 1) then
          call vctmsc (sc(1),scpt,1.0d0/25.4d0)
      else
          call vctovc (sc(1),scpt)
      endif
      call vctovc (sc(4),scpt(4))
      call ncl_profile_pt (inc,knpts,imx,npt,ept)
c
c...Store the top of the part for Annotation
c  
      if (ktyp .eq. 0 .and. kent .ne. 1) then
          if (IPRFFL(6) .eq. 2) then
              call plnint (ept,scpt(4),PRFTPL,scpt,jerr)
          else
              call vctovc (ept,scpt)
          endif
      endif
c
c...Calculate and output exit move
c
      call ncl_profile_entry (inc,2,IPRFFL(13),IPRFFL(3),kctcom,PRFEXI,
     x                        pt,ptent,nent)
      if (nent .ne. 0) then
          call prffdo (6)
          call prfout (1,iw(5),kent,pt,nent)
          call vctovc (pt,scpt)
      endif
c
c...Perform Cutter compensation /off
c      
      if (kctcom.eq.1) then
         call putcl(2000,1007,2,prrct(15))
      endif 
c
c...Retract tool
c......Closed curve depth pass
c

c
c...Added so retract for OUT,dis,angle motion will be
c...normal to ps
c
      call nclf_getpv_value (vefst,velas)
      if (IPRFFL(22) .ne. 0) call vctovc (velas,scpt(4))

      idid   = .false.
      if (kent .eq. 3) then
          call uvcplvc (scpt,scpt(4),pt1,PRFEXI(6))
          idid   = .true.
          
c......Retract to clearance plane
c
      else if (IPRFFL(12) .eq. 1) then
          if (IPRFFL(7) .ne. 3) then
              call plnint (scpt,scpt(4),PRFCLF,pt1,jerr)
              idid   = .true.
          else
              call uvcplvc (ept,scpt(4),pt1,PRFCLF(1))
              call plnvpt (scpt(4),pt1,tmppl,jerr)
              call plnint (scpt,scpt(4),tmppl,pt1,jerr)
              idid   = .true.
          endif
c
c......Retract to retraction plane
c
      else if (IPRFFL(12) .eq. 2) then
          call plnint (scpt,scpt(4),PRFRPL,pt1,jerr)
          idid   = .true.
c
c......Retract to absolute distance
c
      else if (IPRFFL(12) .eq. 3) then
          if (IPRFFL(22) .ne. 0 .or. IPRFFL(21) .ne. 0) then
              call vctovc (velas,PRFRPL)
          else
              call vctovc (scpt(4),PRFRPL)
          endif
          call plnint (scpt,scpt(4),PRFRPL,pt1,jerr)
          idid   = .true.
c
c......Retract a distance
c
      else if (IPRFFL(12) .eq. 4) then
          call uvcplvc (ept,scpt(4),pt1,PRFRPL(1))
          call plnvpt (scpt(4),pt1,tmppl,jerr)
          call plnint (scpt,scpt(4),tmppl,pt1,jerr)
          idid   = .true.
      endif
c
c......Retract the tool
c
      if (idid) then
          call plnvpt (scpt(4),scpt,curpl,jerr)
          call plnvpt (scpt(4),pt1,retpl,jerr)
          if (curpl(4) .lt. retpl(4) .or.
     1        (ktyp .eq. 0 .and. curpl(4) .le. retpl(4))) then
              call prffdo (3)
              call prfout (2,iw(1),kent,pt1,npt)
              call vctovc (pt1,scpt)
          endif
      endif
c
c...Save final point
c
      if (ifl(264) .eq. 1) then
          call vctmsc (scpt,sc(1),25.4d0)
      else
          call vctovc (scpt,sc(1))
      endif
      call vctovc (scpt(4),sc(4))
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : prfout (ktype,kwhich,kent,gpt,knpts)
c*      Outputs the profile pass to the clfile.  Uses the weeded profile
c*      curve.
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          ktype    I*2   D1   - 1 = Output curve points,
C*                                2 = Output provided point.
C*          kwhich   I*4   D1   - Which array to get the points from.
C*          kent     I*4   D1   - 2 = First point on curve is transition
C*                                move.
C*          gpt      R*8   Dn   - Point(s) to output when 'kwhich' = 2.
C*          knpts    I*4   D1   - Number of points to output.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine prfout (ktype,kwhich,kent,gpt,knpts)
c
      include 'com8a.com'
      include 'prfcom.com'
c
      integer*2 ktype
      integer*4 kwhich,knpts,kent
c
      real*8 gpt(6)
c
      integer*2 lnk,jptk,iclass,isubcl
      integer*4 i,inc,npt,n,is,ncirc,icst,icen,is1
c
      real*8 pt(6),buf(640),spt(3),mpt(3),ang,vc1(3),vc2(3),f_dist,
     1       cir(7)
c
      integer*2 ntk
      equivalence (ntk   ,ifl(79))
c
c...Initialize routine
c
      lnk    = 0
      jptk   = 0
      ntk    = 0
      iclass = 5000
      isubcl = 5
      n      = 3 + ifl(82) * 3
c
c...Output curve points
c
      if (ktype .eq. 1) then
          npt    = 1
          is     = 1
c
c......First point is either
c......Entry or Transition point
c
          if (kwhich .eq. 3) then
c
c.........Disable ARCSLP/FILLET on entry moves
c
              if (kent .ne. 2) call asfrst
c
c.........Output entry point
c
              call ncl_profile_pt (kwhich,is,n,npt,pt)
              if (ifl(264) .eq. 1) call vctmsc (pt,pt,25.4d0)
              if (f_dist(pt,sc(1)) .gt. sc(27) .or.
     1                f_dist(pt(4),sc(4)) .eq. sc(27)) then
                  call vctovc (pt,spt)
                  if (kent .eq. 2) then
                      call prffdo (5)
                  else
                      call prffdo (4)
                  endif
                  call fmout (buf,pt,lnk,jptk,iclass,isubcl)
                  call prffot (iclass,isubcl,n,buf)
              endif
              call prffdo (1)
              is     = 2
          endif
c
c......Calculate any circular records
c
          ncirc  = 0
          icst   = 0
          icen   = 0
          if (kwhich .eq. 3 .or. kwhich .eq. 4) then
              call ncl_profile_circle (kwhich,is-1,ncirc,cir,icst,icen,
     1                                 sc(27) )
              icst   = icst   + 2
              icen   = icen   + 1
c
c......Enable ARCSLP/FILLET on profile moves
c
              if (kwhich .ne. 4) call asfset
          endif
c
c.....Do not output the first and last point for circle
c 
         is1 = is
         knpts1 = knpts        
         if (ncirc .ne. 0 .and. kwhich .eq. 4) then
            icst = icst + 1
            is1 = is + 1
         endif
c
c......Get the curve point
c
          do 1000 i=is1,knpts,1
              call ncl_profile_pt (kwhich,i,n,npt,pt)
              if (ifl(264) .eq. 1) call vctmsc (pt,pt,25.4d0)
c
c......End of circular record
c......Get starting position of next circular record
c
              if (ncirc .ne. 0 .and. i .eq. icen+1) then
                  call prffot (iclass,isubcl,n,buf)
                  call ncl_profile_circle (kwhich,is,ncirc,cir,icst,
     1                                     icen,sc(27))
                  icst   = icst   + 2
                  icen   = icen   + 1
              endif
c
c......Circular record needs to be output
c
              if (ncirc .ne. 0 .and. i .eq. icst) then
                  call prffot (iclass,isubcl,n,buf)
                  call prfcir (cir)
c
c......Break up output depending of Filleting angle
c
              else if (PRFANG .ne. 0. .and. 
     1                 (i .lt. icst .or. i .gt. icen)) then
                  if (i .eq. 1) then
                      call vctovc (pt,spt)
                  else if (i .eq. 2) then
                      call vctovc (pt,mpt)
                  else
                      call vcmnvc (mpt,spt,vc1)
                      call vcmnvc (pt,mpt,vc2)
                      call unitvc (vc1,vc1)
                      call unitvc (vc2,vc2)
                      call betvec (vc1,vc2,ang)
                      if (ang+sc(27) .ge. PRFANG)
     1                    call prffot (iclass,isubcl,n,buf)
                      call vctovc (mpt,spt)
                      call vctovc (pt,mpt)
                  endif
              endif
              if (i .eq. knpts) jptk = 1  
              call fmout (buf,pt,lnk,jptk,iclass,isubcl)
c
c......Store current tool position
c
              call vcmnvc (pt,sc(1),sc(7))
              call unitvc (sc(7),sc(7))
              call vctovc (pt,sc(1))
              if (n .eq. 6) call vctovc (pt(4),sc(4))
 1000     continue
c
c...Output provided points
c
      else
          inc    = 1
          do 1100 i=1,knpts,1
              if (ifl(264) .eq. 1) then
                  call vctmsc (gpt(inc),pt,25.4d0)
              else
                  call vctovc (gpt(inc),pt)
              endif
              if (i .eq. knpts) jptk = 1
              if (n .eq. 6) call vctovc (gpt(inc+3),pt(4))
              call fmout (buf,pt,lnk,jptk,iclass,isubcl)
              inc    = inc    + n
c
c......Store current tool position
c
              call vcmnvc (pt,sc(1),sc(7))
              call unitvc (sc(7),sc(7))
              call vctovc (pt,sc(1))
              if (n .eq. 6) call vctovc (pt(4),sc(4))
 1100     continue
      endif
c
c...Output final positions
c
      call prffot (iclass,isubcl,n,buf)
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : prffot (kclass,ksubcl,kmxc,gbuf)
c*      Forces out any motion points waiting to be output to the clfile.
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          kclass   I*2   D1   - Motion class type (typically 5000).
C*          ksubcl   I*2   D1   - Motion subtype (5 or 6).
C*          kmxc     I*4   D1   - 3 for MULTAX/OFF, 6 for MULTAX/ON.
C*          gbuf     R*8   D420 - Buffer that holds the motion clfile data.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine prffot (kclass,ksubcl,kmxc,gbuf)
c
      include 'com8a.com'
c
      integer*2 kclass,ksubcl
      integer*4 kmxc
c
      real*8 gbuf(640)
c
      integer*2 numitm
c
      integer*2 ntk
      equivalence (ntk   ,ifl(79))
c
c...Output final positions
c
      if (ntk .ge. 3) then
          numitm = ntk    / kmxc
          call putcl (kclass,ksubcl,numitm,gbuf)
          ntk    = 0
          ksubcl = 5
      endif
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : prffdo (kfed)
c*      Outputs a PROFIL feed rate record to the clfile.
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          kfed     I*2   D1   - Type of feed rate to output.
C*                                1 = Driving motion.
C*                                2 = Positioning motion.
C*                                3 = Retract motion.
C*                                4 = Entry motion.
C*                                5 = Transition motion.
C*                                6 = Exit motion.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine prffdo (kfed)
c
      include 'com8a.com'
      include 'prfcom.com'
c
      integer*2 kfed
c
      real*8 rfed
c
c...Output Rapid rate
c
      if (IPRFFD(kfed) .eq. 2) then
          call ppwrt (5)
c
c...Output programmed feed rate
c
      else
          if (IPRFFD(kfed) .eq. 0) then
              rfed = PRFFED(1)
c
c...Output specified feed rate
c
          else if (IPRFFD(kfed) .eq. 1) then
              rfed = PRFFED(kfed)
c
c...Output percentage of feed rate
c
          else if (IPRFFD(kfed) .eq. 3) then
              rfed = PRFFED(1) * PRFFED(kfed)
          endif
          call fedmut (rfed)
      endif
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : prfcir (gcir)
c*      Outputs a PROFIL circular record to the clfile.
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          gcir     R*8   D7   - XYZIJKR of circle to output.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine prfcir (gcir)
c
      include 'com8a.com'
      include 'prfcom.com'
c
      real*8 gcir(7)
c
      integer*4 i
c
      real*8 rdat(13)
c
c...Output circular record
c
      rdat(1) = 5.
      rdat(2) = 0.
      rdat(3) = 0.
      rdat(4) = 0.
      rdat(5) = 0.
      do 100 i=1,7,1
          rdat(i+5) = gcir(i)
  100 continue
      call putcl (3000,2,13,rdat)
c
c...End of routine
c
 8000 return
      end

