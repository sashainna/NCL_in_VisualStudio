C*********************************************************************
C*    NAME         :  cveval.f
C*       CONTAINS:
C*    COPYRIGHT 1986 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cveval.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:47
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cveval(nclkey,max,npts,buf)
C*     Take the NCL curve pointed to by nclkey and return in buf a maximum
C*     of max points along it.
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey - Unibase key of curve to evaluate.
C*          max    - Maximum number of points to return in buf.
C*       OUTPUT :  
C*          npts   - number of points returned.
C*          buf    - X, Y and Z co-ordinates of points along curve.
C*    RETURNS      : none
C*    SIDE EFFECTS : Calls dcrvpt which uses common area dspcom.
C*    WARNINGS     : none
C********************************************************************/

      subroutine cveval (nclkey, max, npts, buf)

      include 'com4a.com'
c      include '../incf/dspcom.com'

      parameter (maxpt=50)
      parameter (maxwd=(maxpt*6+(maxpt+1)/2))

      common/dspcom/ws(20),w(maxwd)

      real*8 ws,w
      real*4 aws(40), aw(300)
      equivalence (ws,aws),(w,aw)

      integer*4 nclkey
      integer*2 max, npts
      real*8 buf(450) 

      integer*2 ix ,ninc,itype
      real*4 s,sinc,vt(3)

      s=0.
      ix=0
      itype=8
      ninc=ifl(136)
      if (ninc.eq.0) ninc=50
      if (ninc.lt.10) ninc=10
      if (ninc.gt.max) ninc=max
      npts=ninc
      sinc=ninc
      sinc=1./(sinc-1.)

c...
c      if (debug) then
c      write(cout,9010)ninc,sinc
c9010  format('cveval: ninc,sinc'i4,f10.4)
c      call putmsg(cout,80,1,0)
c      endif
c...

      call gtgeo(nclkey, w(1))

      ws(1)=0
      aws(39)=0.
      aws(40)=aw(1)
      aw(1)=0.

      do 100 i=1,ninc
        call dcrvpt (s, itype, buf(ix+1), vt)
c...
c      if (debug) then
c      write(cout,9020)buf(ix+1),buf(ix+2),buf(ix+3)
c9020  format('cveval: buf('3f12.4')')
c      call putmsg(cout,80,1,0)
c      endif
c...
        ix=ix+3
        s=s+sinc
100   continue

      return
      end
