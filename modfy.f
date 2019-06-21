C*********************************************************************
C*    NAME         :  modfy.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       modfy.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:17
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine modfy(tdat,td)
c*        this routine jogs the cutter rgt,fwd,etc. as data goes to
c*        clfile when ta/,,modify is in effect.      10-15-82
c*
c*        output loads directly in tdat-tbl
C*    PARAMETERS   
C*       INPUT  : 
C*          td         data to modify
C*       OUTPUT :
C*          tdat       modified data
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine modfy(tdat,td)

      include 'com4a.com'
      include 'mocom.com'

      real*8 tdat(9),td(9)

      real*8 xh,yh,zh
      equivalence(xh,sc(84)),(yh,sc(85)),(zh,sc(86))

c              xt (rgt) is crossf fwd,up
      xr=td(8)*td(6)-td(9)*td(5)
      yr=td(9)*td(4)-td(7)*td(6)
      zr=td(7)*td(5)-td(8)*td(4)
      sec=sqrt(xr**2+yr**2+zr**2)
      if (sec.eq.0.0) then
        xf = td(5)*zh-td(6)*yh
        yf = td(6)*xh-td(4)*zh
        zf = td(4)*yh-td(5)*xh
        sec=sqrt(xf**2+yf**2+zf**2)
        if (sec.gt.0.0) then
          xr = yf*td(6)-zf*td(5)
          yr = zf*td(4)-xf*td(6)
          zr = xf*td(5)-yf*td(4)
          sec=sqrt(xr**2+yr**2+zr**2)
        endif
      endif
      if (sec.gt.0.0) then
        xr=xr/sec
        yr=yr/sec
        zr=zr/sec
        xh = xr
        yh = yr
        zh = zr
      endif
c              yt (fwd) is crossf rgt,up
      xf=zr*td(5)-yr*td(6)
      yf=xr*td(6)-zr*td(4)
      zf=yr*td(4)-xr*td(5)
c              bump rgt,fwd,up
      tdat(1)=td(1)+xr*tool(12)+xf*tool(13)+td(4)*tool(14)
      tdat(2)=td(2)+yr*tool(12)+yf*tool(13)+td(5)*tool(14)
      tdat(3)=td(3)+zr*tool(12)+zf*tool(13)+td(6)*tool(14)
c              if multax off, exit
      if (ifl(82).eq.0)goto 99
      sec=sqrt(tool(15)**2+tool(16)**2+tool(17)**2)
      if (sec .eq. 0) then
          tdat(4) = td(4)
          tdat(5) = td(5)
          tdat(6) = td(6)
      else
          tdat(4)=xr*tool(15)+xf*tool(16)+td(4)*tool(17)
          tdat(5)=yr*tool(15)+yf*tool(16)+td(5)*tool(17)
          tdat(6)=zr*tool(15)+zf*tool(16)+td(6)*tool(17)
      endif
c
c     the following patch unitizes the
c     tlaxis vector.    epm  12-19-83
c
      sec=dsqrt(tdat(4)**2+tdat(5)**2+tdat(6)**2)
      tdat(4)=tdat(4)/sec
      tdat(5)=tdat(5)/sec
      tdat(6)=tdat(6)/sec

99    return
      end
