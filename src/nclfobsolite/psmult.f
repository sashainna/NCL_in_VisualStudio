C*********************************************************************
C*    NAME         :  psmult.f
C*       CONTAINS:
C*
C*                psload
C*                pssave
C*                load_surface
C*                gettool
C*
C*    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        psmult.f , 25.1
C*     DATE AND TIME OF LAST MODIFICATION
C*        04/29/15 , 15:10:29
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine psload (isf, iuse, nb)
C*       If the next part surface is within the cutter envelope, 
C*       load it into the part surface drive tables.
C*     PARAMETERS
C*       INPUT  :
C*          isf       - Index of this surface
C*          tcyl      - Tool cylinder.
C*       OUTPUT :
C*          iuse      - 1 = use this surface, 0 do not use.
C*          nb        - number of trim boundaries if sf is used as
C*                      trimmed (for ncl90 compatibility only)
C*     RETURNS      : none
C*     SIDE EFFECTS : none
C*     WARNINGS     : none
C*********************************************************************
C
      subroutine psload (isf, iuse, nb)

      include 'com.com'
      include 'mocom.com'
      include 'suvcom.com'

      integer*2 isf, iuse, nb

      integer*2 jd(600), ia, ib, ic, isrf
      real*4 ad(300)
      equivalence(d,ad,jd)
      equivalence(ia,ifl(51)),(ib,ifl(52)),(ic,ifl(53)),(ifl(54),isrf)

      common/savcuv/cuv
      real*4 cuv(10)

      common/psldcom/irejog,ro
      integer*4 irejog
      real*4 ro
c
c... local variables
c
      real*8 te(3),ta(3),tsv(9),uv(4)
      real*4 ad2
      integer*4 nclkey
      integer*2 nwds,ietype,i,ild, iasv
      logical lv90,lv97

      lv90 = sc(169).lt.9.049d0
      lv97 = sc(169).lt.9.749d0

      call ncl_psmult_use (isf, nclkey, iuse, nb)
      if (iuse.eq.0) return
      nwds = 1
      ietype = SURF
      call ptdsc3 (nclkey, nwds, ietype, sc(144))
      call ncl_psmult_isinit (isf, ild)

      if (ild.eq.0) then
C   ------   FIX   ------ Need a way to set & save uv values for each sf
        t(13,ia) = psu
        t(14,ia) = psv

        do 10 i=1,9,2
           cuv(i)   = psu
           cuv(i+1) = psv
10      continue

        iasv = ia
        call conv4_8(t(1,3),tsv,9)
        call conv4_8(t(1,3),te,3)
        call conv4_8(t(4,3),ta,3)

        call psinit (sc(144), te, ta)

        ia = iasv
        call conv8_4(tsv,t(1,3),9)

        ad2 = ad(2)
        call ncl_psmult_sfinit (isf, t(13,1), t(13,2), t(13,3), ad2,
     *       cuv)
      else
        call ncl_psmult_load (isf, t(13,1), t(13,2), t(13,3), ad2, cuv)
c
c...jingrong 06/15/99 extrap u,v and look vec for each isf unless rejog.
c
        if(irejog.eq.0 .and. .not.lv90) then
           t(13,ia)=t(13,ic)+ro*(t(13,ic)-t(13,ib))
           t(14,ia)=t(14,ic)+ro*(t(14,ic)-t(14,ib))
           if(ifl(56).ne.0)
     *        call avcplbvc4 (1.+ro,t(16,ic),-ro,t(16,ib),t(16,ia))
        endif

        call sfinit (sc(144), isrf, t(13,ia), t(14,ia))
        ad(2) = ad2
      endif
c
c... load uv box for usrfpn and set trimmed flag for trimmed surfaces
c
      if (ifl(340) .eq. 1) ifl(330+isrf) = 1
      call tbdini (nclkey,isrf,sc(27),ifl(330+isrf))
c
c.....Do not start exactly at the edge of a surface
c.....this has caused problems with multi-ps and ball cutter
c.....when the tool can find a location on the extension
c.....that makes it happy, but a better location can be
c.....found on the surface (psmult/net_surf2.pp)
c.....Bobby  -  04/30/10
c
      if (.not. lv97) then
          call gtmmuv (isrf,uv(1),uv(2),uv(3),uv(4))
          if (uv(1) .eq. 0. .and. uv(3) .eq. 0.) uv(3) = 1.
          if (uv(2) .eq. 0. .and. uv(4) .eq. 0.) uv(4) = 1.
          do 100 i=1,2,1
              if (t(12+i,ia) .le. uv(i)+.001) 
     1            t(12+i,ia) = uv(i) + (uv(i+2)-uv(i))*.15 
              if (t(12+i,ia) .ge. uv(i+2)-.001) 
     1            t(12+i,ia) = uv(i+2) - (uv(i+2)-uv(i))*.15 
  100     continue
      endif
      return
      end

C*********************************************************************
C*     E_SUBROUTINE     : subroutine pssave (isf, dis, iext)
C*       Save ending data for this part surface.
C*     PARAMETERS
C*       INPUT  :
C*          isf       - Index of this surface.
C*          dis       - Distance of tool from surface.
C*          iext      - 0 = on surface, 1 = on surface extension.
C*       OUTPUT :
C*          none
C*     RETURNS      : none
C*     SIDE EFFECTS : none
C*     WARNINGS     : none
C********************************************************************/
C
      subroutine pssave (isf, dis, iext)

      include 'com.com'
      include 'mocom.com'

      real*8 dis
      integer*2 isf, iext

c          motion common equivalences
      real*4 ad(300)
      equivalence (d,ad)
      integer*2 ia
      equivalence (ifl(51),ia)

      common/savcuv/cuv
      real*4 cuv(10)

      call ncl_psmult_save (isf, t(13,1), t(13,2), t(13,3), s(1,1), cuv,
     *     dis, iext, ifl(2),ad(2))
c
c... jingrong do not reset the err flag if it has got a warning. 12/06/99.
c... was: ifl(2) = 0
c
      if (ifl(2).gt.0) ifl(2) = 0
      return
      end

C*********************************************************************
C*     SUBROUTINE     : subroutine load_surface (srf)
C*     Loads surface info into the NCL surface structure
C*     PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          srf - surface structure containing current surface info
C*     RETURNS      : none
C*     SIDE EFFECTS : none
C*     WARNINGS     : none
C********************************************************************/
      subroutine load_surface (srf)

      include 'com.com'
      include 'mocom.com'

      real*8 srf(20)

      integer*2 ia
      equivalence (ia,ifl(51))

c      call conv4_8 (s(1,1),srf,7)
      call conv4_8 (s(1,1),srf(1),7)
      call conv4_8 (t(13,ia),srf(8),2)
      srf(10) = 0.
      srf(11) = 0.
		if (ifl(23).ne.TA_NORMAL_PS) srf(11) = sc(23)

      return
      end

C*********************************************************************
C*    SUBROUTINE     : subroutine gettool (i, val)
c*          C callable routine to get tool parameters
C*       INPUT  :
C*                i - index of tool table
C*       OUTPUT :
C*                val - value of tool(i)
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine gettool (i, val)

      include 'com8a.com'
      include 'mocom.com'

      real*4 asc(600),sbe,cbe,cutn
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)

      integer*2 i
      real*8 val

      if (i .lt. 21) then
      val = tool(i)
	else if (i .eq. 63) then
	val = sbe
	else if (i .eq. 64) then
	val = cbe
	else if (i .eq. 65) then
	val = cutn
	endif

      return
      end

