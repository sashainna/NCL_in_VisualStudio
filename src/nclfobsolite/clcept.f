C*********************************************************************
C*    NAME         :  clcept.f
C*       CONTAINS:
C*    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       clcept.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:42
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine clcept
c*          Calculate tool end point in drive contact mode
C*    PARAMETERS   
C*       INPUT  : 
C*          tb        - motion point, ta vector & fwd vector.
C*       OUTPUT :  
C*          tb        - modified motion point, ta vector & fwd vector.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine clcept (tb)

      include 'com.com'
      include 'mocom.com'

      real*8 tb(9)

      common/clccom/svtool(6),iflsv(3)
      real*4 svtool
      integer*2 iflsv

      real*4 htb(9), pte(3)
      real*4 co,si,xt,yt,zt,xr,yr,zr,sec,hdis
      real*4 xf,yf,zf
      integer*2 i
c      integer*2 itim
      integer*2 ic
      equivalence (ic,ifl(53))
c
c      itim = 1
      co = tool(9)
      si = tool(8)
      do 5 i=1,9
5     htb(i) = tb(i)
c --- debgug ---
c      pte(1)=tool(19)*htb(6)-tool(20)*htb(5) 
c      pte(2)=tool(20)*htb(4)-tool(18)*htb(6) 
c      pte(3)=tool(18)*htb(5)-tool(19)*htb(4) 
c --- debgug ---
c      if (iflsv(1).eq.11.or.iflsv(1).eq.12) then
c        t(16,ic) = htb(7)
c        t(17,ic) = htb(8)
c        t(18,ic) = htb(9)
c      endif
10    continue
      if (iflsv(1).eq.2.or.iflsv(1).eq.10.or.iflsv(1).eq.11
     x         .or.iflsv(1).eq.12) then
        if (iflsv(1).eq.11.or.iflsv(1).eq.12) then
          pte(1) = htb(1)-htb(7)*svtool(6)
          pte(2) = htb(2)-htb(8)*svtool(6)
          pte(3) = htb(3)-htb(9)*svtool(6)
          call tltang(si,co,pte,htb(7),t(13,ic),t(14,ic))
        endif
        xt = htb(4)*co+htb(7)*si
        yt = htb(5)*co+htb(8)*si
        zt = htb(6)*co+htb(9)*si
        if (iflsv(1).eq.10.or.iflsv(1).eq.12) then
c
c... PERPTO vector
c
          xr = tool(19)*zt-tool(20)*yt
          yr = tool(20)*xt-tool(18)*zt
          zr = tool(18)*yt-tool(19)*xt
          xt = yr*tool(20)-zr*tool(19)
          yt = zr*tool(18)-xr*tool(20)
          zt = xr*tool(19)-yr*tool(18)
        else if (iflsv(3).eq.1) then
c
c... Tilt forward or right
c
          xr = htb(8)*zt-htb(9)*yt
          yr = htb(9)*xt-htb(7)*zt
          zr = htb(7)*yt-htb(8)*xt
          sec = sqrt(xr**2+yr**2+zr**2)
          if (sec.eq.0.) sec = 1.0
          xr = xr/sec
          yr = yr/sec
          zr = zr/sec
          xf = yt*zr-zt*yr
          yf = zt*xr-xt*zr
          zf = xt*yr-yt*xr
          sec = sqrt(xf**2+yf**2+zf**2)
          if (sec.eq.0.) sec = 1.0
          xf = xf/sec
          yf = yf/sec
          zf = zf/sec
          xt = xr*sc(81)+xf*sc(82)+xt*sc(83)
          yt = yr*sc(81)+yf*sc(82)+yt*sc(83)
          zt = zr*sc(81)+zf*sc(82)+zt*sc(83)
        endif
        sec = sqrt(xt**2+yt**2+zt**2)
        if (sec.gt.1.e-6) then
          tb(4) = xt/sec
          tb(5) = yt/sec
          tb(6) = zt/sec
        endif
      endif
c      xt = -htb(7)
c      yt = -htb(8)
c      zt = -htb(9)
c --- debug ---
c      xr = tb(5)*htb(6)-tb(6)*htb(5)
c      yr = tb(6)*htb(4)-tb(4)*htb(6)
c      zr = tb(4)*htb(5)-tb(5)*htb(4)
c      pte(1) = yr*tb(6)-zr*tb(5)
c      pte(2) = zr*tb(4)-xr*tb(6)
c      pte(3) = xr*tb(5)-yr*tb(4)
c --- debug ---
      hdis = htb(4)*tb(4)+htb(5)*tb(5)+htb(6)*tb(6)
      if (abs(hdis).gt.1.e-6) then
        xt = htb(4)-hdis*tb(4)
        yt = htb(5)-hdis*tb(5)
        zt = htb(6)-hdis*tb(6)
        sec = sqrt(xt**2+yt**2+zt**2)
        if (sec.gt.1.e-6) then
          htb(7) = -xt/sec
          htb(8) = -yt/sec
          htb(9) = -zt/sec
        endif
      endif
      tb(1) = htb(1)+(htb(4)-tb(4))*svtool(2)-htb(7)*svtool(6)
      tb(2) = htb(2)+(htb(5)-tb(5))*svtool(2)-htb(8)*svtool(6)
      tb(3) = htb(3)+(htb(6)-tb(6))*svtool(2)-htb(9)*svtool(6)

c      if (iflsv(1).ne.11.and.iflsv(1).ne.12) goto 999
c      if (itim.eq.2) goto 999
c      itim = 2
c      xr = tb(5)*tb(9)-tb(6)*tb(8)
c      yr = tb(6)*tb(7)-tb(4)*tb(9)
c      zr = tb(4)*tb(8)-tb(5)*tb(7)
c      xt = yr*tb(6)-zr*tb(5)
c      yt = zr*tb(4)-xr*tb(6)
c      zt = xr*tb(5)-yr*tb(4)
c      xt = htb(1)-tb(1)
c      yt = htb(2)-tb(2)
c      zt = htb(3)-tb(3)
c      sec = sqrt(xt**2+yt**2+zt**2)
c      if (sec.lt.1.e-6) goto 999
c      htb(7) = xt/sec
c      htb(8) = yt/sec
c      htb(9) = zt/sec
c      htb(1) = tb(1)+htb(7)*svtool(6)
c      htb(2) = tb(2)+htb(8)*svtool(6)
c      htb(3) = tb(3)+htb(9)*svtool(6)
c      co = tool(9)
c      si = tool(8)
c      call tltang(si,co,htb,htb(7),t(13,ic),t(14,ic))
c      xt = htb(4)*co+htb(7)*si
c      yt = htb(5)*co+htb(8)*si
c      zt = htb(6)*co+htb(9)*si
c      if (iflsv(1).eq.12) then
c        xr = tool(19)*zt-tool(20)*yt
c        yr = tool(20)*xt-tool(18)*zt
c        zr = tool(18)*yt-tool(19)*xt
c        xt = yr*tool(20)-zr*tool(19)
c        yt = zr*tool(18)-xr*tool(20)
c        zt = xr*tool(19)-yr*tool(18)
c      endif
c      sec = sqrt(xt**2+yt**2+zt**2)
c      if (sec.gt.1.e-6) then
c        tb(4) = xt/sec
c        tb(5) = yt/sec
c        tb(6) = zt/sec
c      endif

999   return
      end
