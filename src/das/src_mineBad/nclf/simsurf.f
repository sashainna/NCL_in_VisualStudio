C*********************************************************************
C*    NAME         :  simsurf.f
C*       CONTAINS: functions simulating surface (PS) created thru the
C*                 curve on DS. PS is a rulled surf normal to DS at
C*                 any point on the curve.
C*
C*    COPYRIGHT 1989 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       simsurf.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:43
C*********************************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine simsurf (sfu,iret)
c*        This routine is called when a srf p/n is reqd on a CV on 
c*        SF (surface is simulated as rulled surface normal to
c*        SF at CV).
C*    PARAMETERS   
C*       INPUT  : 
C*          sfu        - u value to start at
C*                Common array SRF(8-10,ISRF) contains external point
C*       OUTPUT :  
C*          sfu        - u value of calc'd point & normal
C*                Common array SRF(1:4,ISRF) - normal plane
C*                Common array SRF(5:7,ISRF) - point on surface
C*          iret       - return value
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine simsurf (sfu, iret)

      include 'com4a.com'
      include 'mocom.com'
      real*4 sfu
      integer*2 iret
 
c
c...jingrong 02/25/99 move timer to file "neeval.c" to count cv evaluation.
c
c      common /timx/ ncall,etim
c      integer*4 stims,stimm,ncall,etims,etimm,etim

      real*4 srf(10,3)
      equivalence (srf,s)

      real*4 ad(300)
      integer*2 kd(600),ia,ib,ic
      equivalence (d,ad,kd),(ifl(54),isrf)
      equivalence (ifl(51),ia),(ifl(52),ib),(ifl(53),ic)
      real*8 pex(3),vtn(3),vdsn(3),vpsn(3),pt(3),ln(6),
     *       tpar,r,f_dot

c
c...jingrong 02/25/99 move timer to file "neeval.c" to count cv evaluation.
c
c      ncall = ncall + 1
c      call gtimx(stims,stimm)

c
c...project external point on CVonSF 
c
      call conv4_8 (SRF(8,ISRF),pex,3)
      tpar = sfu
      call norm_cvonsf (pex,tpar,ISRF,pt,vtn)
c
c...find vector normal to the SF at point on CV,
c
c... aak 17-nov-1997: now norm_sfatcv treats cv on Sf as 
c... polyline and interpolates u,v values accordingly
c
      call norm_sfatcv (tpar,ISRF,vdsn)
c
c...jingrong 02/25/99 move timer to file "neeval.c" to count cv evaluation.
c
c      call gtimx(etims,etimm)
c      etim = etim + ((etims-stims)*1000 + (etimm - stimm))
c
c...create plane tangent to the CV and normal to SF
c
      call f_cross (vdsn,vtn,vpsn)
      call unitvc (vpsn,vpsn)
c
c... aak: 26-nov-1997: replaced this call by the next 3 operators;
      call point_on_plane (pex,pt,vpsn,pt)
c... projection on plane can put the point on extension of the surface;
c... to be consistent with PSREL, the point must always be on the surface
c
c      call vctovc(pt,ln)
c      call vctovc(vdsn,ln(4))
c      call point_on_line (pex,ln,pt)
c
c...normal exit.  Load data in SRF(1-3)
c
      r = AD(100*(ISRF-1)+2)
      call vctmsc (vpsn,vpsn,r)
      call conv8_4 (vpsn,SRF(1,ISRF),3)
      call conv8_4 (pt,SRF(5,ISRF),3)
      SRF(4,ISRF) = f_dot (vpsn,pt) 
      sfu = tpar
999   return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine norm_cvonsf (gpt,gpar,
c*                                  ixs,gppt,gvtn)
c*    FUNCTION:
c*        Interface function between ncl world space and unibase model.
c*        Finds projection of the external point on CV using evolved
c*        CVonSF data stored in the list.  Calculates curve parameter
c*        where projected point is located, point coordinates and 
c*        vector tangent to the CV at the point. 
C*    PARAMETERS   
C*       INPUT  : 
c*          gpt    - external point projected on curve
C*          gpar   - parameter value to start at
c*          ixs    - surface index in motion block storage (ISRF)
C*       OUTPUT :  
C*          gpar   - parameter value at calc'd point
C*          gppt   - calc'd point on CV 
C*          gvtn   - calc'd vector tangent to the CV at gppt 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine norm_cvonsf (gpt,gpar,ixs,gppt,gvtn)

      include 'com4a.com'
      include 'wrksys.com'

      real*8 gpt(3),gpar,gppt(3),gvtn(3)
      integer*2 ixs
      integer*4 jsf
      real*8 pnt(3),pt(3),vc(3),sclmm

      data sclmm /25.4d0/
c
c...convert ncl system to unibase system (this applies to ncl 
c...geometry stored in unibase, in open ncl there is one system) 
c
      sclmm = 1.0
      if (ifl(264) .eq. 1) sclmm = 1.0/25.4 
      call vctmsc (gpt,pnt,sclmm)
      if (lwrk) call ptmatb (pnt,pnt,wrkmx,1)
c
c...find normal to the curve on surface evolved previosly
c...using cvonsf_init call for surface specified by ixs
c
      jsf = ixs - 1
      call norm_to_cvonsf (pnt,gpar,jsf,pt,vc)
c
c...transform output to ncl system (see comment 1)
c
      if (ifl(264) .eq. 1) call vctmsc (pt,pt,sclmm)
      if (lwrk) then
          call ptmatb (pt,pt,invwrk,1)
          call ptmatb (vc,vc,invwrk,2)
      end if
      call conv8_8 (pt,gppt,3)
      call conv8_8 (vc,gvtn,3)
c
c......need also error flag when normal is not defined in curve range
c
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cvonsf_init (cvid,isf)
c*    FUNCTION:
c*        Initialize CV on SF common list data to use in NCL motion
C*    PARAMETERS   
C*       INPUT  : 
c*          cvid   - CV on SF ncl key number
c*          isf    - surface index in motion block storage (ISRF)
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine cvonsf_init (cvid,isf)
c
      include 'com4a.com'
      include 'mocom.com'
c
      integer*4 cvid
      integer*2 isf
c
      integer*2 KD(600)
      real*4 AD(300)
      equivalence (D,AD,KD)
      integer*2 nwds,ietype
      integer*4 cvkey,sfkey
      real*8 crv_tol
c
      cvkey = cvid
      call gtdesc (sc(144+isf),sfkey,nwds,ietype)
c
      if (cvkey .eq. sfkey) then
         ifl(2) = 28
      else
         ifl(2) = ncl_check_key(sfkey,cvkey)
         if (ifl(2) .ne. 0) return
c
c... aak 14-nov-1997: evaluate curve with a tolerance better than
c... machining tol.
c... if psmult: drive as a multiple PS; if (!psmult): as one curve
c
         crv_tol = 0.2*sc(27)

         if(.not.psmult) then
            call ncl_cvonsf_init(sfkey,cvkey,1,crv_tol,ifl(2))
         else
            call ncl_cvonsf_init_mult(sfkey,cvkey,1,crv_tol,ifl(2))
         endif

         if (ifl(2) .ne. 0) return
c
c.. jingrong 11/12/98 replace the following with the above for Psmult 
c         call ncl_cvonsf_init (sfkey,cvkey,isf,0.05*sc(27),ifl(2))
c
         kd((isf-1)*200+1) = 9
         if( ad((isf-1)*100+2) .eq. 0) ad((isf-1)*100+2) = 1.0
      end if
c
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine norm_sfatcv (gpar,ixs,gvtn)
c*    FUNCTION:
c*        Calculates normal vector to the SF at the point on 
c*        CVonSF defined by CV parameter value 
C*    PARAMETERS   
C*       INPUT  : 
C*          gpar   - parameter value locating point on CV
c*          ixs    - surface index in motion block storage (ISRF)
c*                   This is surface simulated by plane (not the
c*                   one where CVonSF is laying).
C*       OUTPUT :  
C*          gvtn   - calc'd vector normal to the SF CV and tangent to
c*                   the SF at gppt. 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine norm_sfatcv (gpar,ixs,gvtn)
c
      include 'com4a.com'
      include 'wrksys.com'
c
      integer*2 ixs
      real*8 gpar,gvtn(3) 
c
      real*8 vn(3)
c
      integer*4 jsf
c
c...comment 1.
c...convert ncl system to unibase system (this applies to ncl 
c...geometry stored in unibase, in open ncl there is one system) 
c
c
c...find normal to the surface at curve point defined by 
c...parameter using CVonSF evaluator 
c
      jsf = ixs - 1
c
c... aak 17-nov-1997: changes related to considering CVonSF as polyline
c...      call norm_to_sfatcv (gpar,jsf,pt,vt,vn)
      call ncl_norm_to_sfatcv (gpar,jsf,vn)
c
c...transform output to ncl system (see comment 1)
c
      if (lwrk) call ptmatb (vn,vn,invwrk,2)
      call vctovc(vn,gvtn)
c
c......need also error flag when normal is not defined in curve range
c
      return
      end
