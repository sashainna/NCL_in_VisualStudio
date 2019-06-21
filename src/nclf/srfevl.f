c**
C*********************************************************************
C*    NAME         :  srfevl.f
C*       CONTAINS:
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       srfevl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:44
C********************************************************************/
C
c **********************************************************************
c **********************************************************************
c **  subroutine name: srfevl
c **
c **  last revision:
c **  purpose of subroutine: this routine evaluates a surface at u & v
c **      returns point, normal, slope in u direc, slope in v direc
c **
c **********************************************************************
c **********************************************************************
 
      subroutine srfevl (asw, u, v, pvs)

      include 'com8a.com'

      real*8 asw,pvs(12)
      real*4 u,v

      real*8 u8,v8,ddu(3),ddv(3)
      integer*4 nclkey
      integer*2 nwds, itype, isf, isftyp, iwf

      asn=asw
      call gtdesc(asw,nclkey,nwds,itype)
      call isitwf (nclkey, iwf)
      if (iwf.eq.1) then
        isf = 3
        u8 = u
        v8 = v
        call evstup (nclkey, isf)
        call uevsft (u8, v8, isf, pvs, ifl(2))
        if (ifl(2).eq.0 .and. sc(169).ge.9.549d0) then
          call vctovc(pvs(4),ddu)
          call vctovc(pvs(7),ddv)
          call f_cross(ddu,ddv,pvs(4))
          call vctovc(ddu,pvs(7))
          call vctovc(ddv,pvs(10))
        endif
      else
        call sftype (nclkey, isftyp)
        if (isftyp.eq.26) then
          call mshevl(asw,u,v,pvs)
        else if (isftyp.eq.91) then
          call nsfevl(asw,u,v,pvs)
        else
          ifl(2)=321
          err=.true.
        endif
      endif

99999 return
      end
