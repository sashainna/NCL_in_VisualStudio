C*********************************************************************
C*    NAME         :  tanto.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       tanto.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:47
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine tanto
c*       calc's a cs plane in tanto case and sets sc(36)=0 for 'on'  
c*       reqs:  1.   ta/same  or  ps is a plane                     
c*              2.   ln, ci, pl are valid types  for ds & cs        
c*              3.   circles are perpto ta                          
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
      subroutine tanto

      include 'com4a.com'
      include 'mocom.com'

      real*8 asn
      integer*2 jd(600),ksn(4)
      real*4 ad(300)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      equivalence(asn,ksn)
      logical lv91

      lv91 = sc(169).lt.9.14999d0
c          branch on ta mode
      if(ifl(23).gt.0)goto 20
c          ta/same. up=sc(4,5,6)
      ux=sc(4)
      uy=sc(5)
      uz=sc(6)
      goto 30
c          ps must be plane.  (normps only cutmode except same)
20    if(jd(1).eq.6)goto 22
c          error. variable ta ng with tanto
      ifl(2)=171
      goto 99
c              up is pl norm
22    ux=d(3)
      uy=d(4)
      uz=d(5)
c              if ds is circle, go 100
30    asn=sc(11)
      if(ksn(4).eq.7)goto 100
c              must be line or pl
      if(ksn(4).ne.5)goto 40
c              line. fwd is vector portion
      fx=d(56)
      fy=d(57)
      fz=d(58)
c                    make sure fwd perpto ta
      rx=uy*fz-uz*fy
      ry=uz*fx-ux*fz
      rz=ux*fy-uy*fx
      fx=ry*uz-rz*uy
      fy=rz*ux-rx*uz
      fz=rx*uy-ry*ux
      goto 50
c              must be pl
40    if(ksn(4).eq.6)goto 42
c          error. invalid ds type
      ifl(2)=172
      goto 99
c              fwd is crossf plnorm and up
42    fx=uy*d(55)-uz*d(54)
      fy=uz*d(53)-ux*d(55)
      fz=ux*d(54)-uy*d(53)
c              cs must be circle
50    asn=sc(13)
      if(ksn(4).eq.7)goto 52
c              error. ln/pl ds reqs circle cs
      ifl(2)=172
      goto 99
c              circle axis must be somewhat parlel up
52    co=ux*d(106)+uy*d(107)+uz*d(108)
      if(abs(co).ge..999)goto 54
c              error. circle is tipped
      ifl(2)=161
      goto 99
c              defpt is circ center
54    px=d(103)
      py=d(104)
      pz=d(105)
      goto 200
c
c***********   ds is circle. see if tipped.
c
c        add fut checks for gt 180 degree case
c
100   co=ux*d(56)+uy*d(57)+uz*d(58)
      if(abs(co).ge..999)goto 102
      ifl(2)=161
      goto 99
c              defpt is center
102   px=d(53)
      py=d(54)
      pz=d(55)
c              now cs
      asn=sc(13)
      if(ksn(4).ne.5)goto 110
c               ci-ln case.  fwd = ln dltas
      fx=d(106)
      fy=d(107)
      fz=d(108)
c                    make sure fwd perpto ta
      rx=uy*fz-uz*fy
      ry=uz*fx-ux*fz
      rz=ux*fy-uy*fx
      fx=ry*uz-rz*uy
      fy=rz*ux-rx*uz
      fz=rx*uy-ry*ux
      goto 115
110   if(ksn(4).ne.6)goto 120
c               ci-pl case.  fwd is crossf plnorm and up
      fx=uy*d(105)-uz*d(104)
      fy=uz*d(103)-ux*d(105)
      fz=ux*d(104)-uy*d(103)
c               if fx,y,z opposes te to circ ctrpt, flip it.
c                (for very small effective radius moves)   22-apr-86
115   fdis=fx*(px-sc(1))+fy*(py-sc(2))+fz*(pz-sc(3))
      if(fdis.ge.0.)goto 200
      fx=-fx
      fy=-fy
      fz=-fz
      goto 200
120   if(ksn(4).eq.7)goto 130
c          error. invalid cs type
      ifl(2)=172
      goto 99
c               ci-ci case.  do vec ctr to ctr
c                  if cs circ not tipped
130   co=ux*d(106)+uy*d(107)+uz*d(108)
      if(abs(co).ge..999)goto 132
      ifl(2)=161
      goto 99
132   rx=d(103)-d(53)
      ry=d(104)-d(54)
      rz=d(105)-d(55)
c              fwd perpto rgt and up
      fx=ry*uz-rz*uy
      fy=rz*ux-rx*uz
      fz=rx*uy-ry*ux
c          finup.  fwd must be real
200   sec=sqrt(fx**2+fy**2+fz**2)
      if(sec.ge..001)goto 210
c          degen case (concentric circles ?)
      ifl(2)=173
      goto 99
c          all ok.  put pseudo cs pl in s-tbl and set 'on' cs
210   sc(36)=0.
      jd(401)=6
      s(1,3)=fx/sec
      s(2,3)=fy/sec
      s(3,3)=fz/sec
      s(4,3)=s(1,3)*px+s(2,3)*py+s(3,3)*pz
cc             allow for .gt. 180 degrees in ds circle case   17-mar-83
      if(jd(201).ne.7)goto 99
cc             ifl(102)=1 informs endpas to backout at 1st entry.
      ifl(102)=0
      r1=d(59)
c              set vec from te to ds ctrpt
      vx=px-sc(1)
      vy=py-sc(2)
      vz=pz-sc(3)
c              init fwd is crossf vxyz and up.  (may not oppose t(,3)fwd)
      fx=vy*uz-vz*uy
      fy=vz*ux-vx*uz
      fz=vx*uy-vy*ux
      if(fx*t(7,3)+fy*t(8,3)+fz*t(9,3).gt.0.)goto 220
      fx=-fx
      fy=-fy
      fz=-fz
220   if(ksn(4).ne.5)goto 230
c              cs is line.  cross deltas and 'up' for nvec
      vnx=d(107)*uz-d(108)*uy
      vny=d(108)*ux-d(106)*uz
      vnz=d(106)*uy-d(107)*ux
      dis=vnx*(d(103)-px)+vny*(d(104)-py)+vnz*(d(105)-pz)
      if(dis.gt.0.)goto 300
      vnx=-vnx
      vny=-vny
      vnz=-vnz
      goto 300
230   if(ksn(4).ne.6)goto 240
c              cs is plane.
      adir=1.
      dis=d(106)-px*d(103)-py*d(104)-pz*d(105)
      if(dis.lt.0.)adir=-1.
      vnx=d(103)*adir
      vny=d(104)*adir
      vnz=d(105)*adir
      goto 300
c              cs is circle.  find nvec to nrpt bet 2 circles.
240   vnx=d(103)-d(53)
      vny=d(104)-d(54)
      vnz=d(105)-d(55)
c
c...  Get distance between circle centers in plane of circles.
c
      del=vnx**2+vny**2+vnz**2
      dis=(vnx*ux+vny*uy+vnz*uz)**2
      if (dis.gt.del) dis=del
      if (sc(169).lt.8.19999d0) dis=0.0
      del=sqrt(del-dis)
      if(del.gt..0005)goto 242
c              error. ds and cs circles are concentric
      ifl(2)=168
      goto 99
242   r1=d(59)
      r2=d(109)
      asm=abs(del+r2-r1)
      da=abs(del-r1-r2)
      if(da.lt.asm)asm=da
      bsm=abs(del-r2+r1)
      if(asm.le.bsm)goto 300
      vnx=-vnx
      vny=-vny
      vnz=-vnz
c              set ifl(102) if gt 180 (unless in 180 + .001 range)
300   sec1=sqrt(fx**2+fy**2+fz**2)
      sec2=sqrt(vnx**2+vny**2+vnz**2)
c              guard against zero divides.       19-dec-83
      if(sec1.gt.0..and.sec2.gt.0..and.r1.gt.0.) goto 310
c              error.  zero divide.
      ifl(2)=163
      goto 99
310   si=(fx*vnx+fy*vny+fz*vnz)/sec1/sec2
c
c... Set ifl(102) to 2 if moving towards check sf, 1 if moving away
c    Negate if on side of circle opposite to the check sf. Mover sets
c... it to 0 if tanto position is at first intersection of plane or
c... to 4 for second intersection, taking into account the fact that
c... the first move could move through the check plane. IJD 7-Dec-2001
c
      ifl(102) = 1
      if (si .ge. 0.) ifl(102) = 2
      if(vx*vnx+vy*vny+vz*vnz.gt.0.)ifl(102) = -ifl(102)
      if (lv91) then
        if(si.lt.-.001/r1)ifl(102)=4
c              extra check for 360 degree case   11-apr-85
        if(abs(si).gt..0001)goto 3105
        if(vx*vnx+vy*vny+vz*vnz.lt.0.)ifl(102)=4
      endif
3105  continue
99    continue
      return
      end
