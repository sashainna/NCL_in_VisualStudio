C*********************************************************************
C*    NAME         :  psmod3.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       psmod3.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:29
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine psmod3
c*       this routine does psrel work when cutmode =3       7-22-82
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
      subroutine psmod3

      include 'com4a.com'
      include 'mocom.com'

      integer*2 jd(600)
      real*4 ad(300)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)

      iknt=0
      irst=0
      iexit=0
      dmov=0.
      u=t(13,ia)
      v=t(14,ia)
c
c.....cpt and rgt sense
c
      sec=sqrt(t(4,ia)**2+t(5,ia)**2+t(6,ia)**2)
      ti=t(4,ia)/sec
      tj=t(5,ia)/sec
      tk=t(6,ia)/sec
c
c..... aak 26-mar-1998: if tlonps, look point = tend
c
      if (ifl(342).eq.1) then
         xc = t(1,ia)
         yc = t(2,ia)
         zc = t(3,ia)
      else
         xc = t(1,ia)+ti*tool(2)
         yc = t(2,ia)+tj*tool(2)
         zc = t(3,ia)+tk*tool(2)
      endif

      xr=t(8,ia)*tk-t(9,ia)*tj
      yr=t(9,ia)*ti-t(7,ia)*tk
      zr=t(7,ia)*tj-t(8,ia)*ti
c           unitize rgt-sense
      sec=sqrt(xr**2+yr**2+zr**2)
      if(sec.gt..1) goto 20
c              no real rgt sense.  use cpt for 1-time look, exit.
      iexit=1
      goto 30
20    dmov=tool(6)*tool(11)/sec
30    s(8,1)=xc+xr*dmov
      s(9,1)=yc+yr*dmov
      s(10,1)=zc+zr*dmov
c              max surfpn calls is 10
      iknt=iknt+1
c
c.....too many tries. give up.
c
      if(iknt.ge.10) then
         ifl(2) = 128
         return
      endif

      call surfpn(u,v,0)
      if (ifl(2) .eq. 466) return

      t(13,ia)=u
      t(14,ia)=v
      if(iexit.eq.1) return
c
c.....alpha is rgt angle ta and psnorm
c
      sal=(xr*s(1,1)+yr*s(2,1)+zr*s(3,1))/sec
c              if this was re-start, go try l/r select
      if(tool(11).eq.0.) goto 60
c              not re-start.  if l/r still ok, exit
      if(tool(11)*sal.le.0.) goto 99
c              l/r switch indicated. if restrt already done, do an
c              endflat dmov and go once more.
      if(irst.eq.1) goto 40
      tool(11)=0.
      dmov=0.
      irst=1
      goto 30
c              endflat estimate
40    iexit=1
      den=abs(hsal)+abs(sal)
      dmov=dmov*abs(hsal)/den
      goto 30
c              after restrt, sto ps pnrm and try l/r select
60    if(abs(sal).lt..002) goto 99
      psa=s(1,1)
      psb=s(2,1)
      psc=s(3,1)
      psd=s(4,1)
      hsal=sal
      tool(11)=-sal/abs(sal)
      goto 20
99    return
      end
