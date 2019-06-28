C*********************************************************************
C*    NAME         :  nsrfpn.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       nsrfpn.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:20
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nsrfpn (u,v)
c*        this routine is called when a srf p/n is reqd.
c*        activate patch/panel as nec and call ipatch to calc
c*        the actual srf data.
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
      subroutine nsrfpn (u,v,iret)

      include 'com4a.com'
      include 'mocom.com'
      include 'drvcom.com'

c          motion common equivalences
      real*4 srf(10,3)
      equivalence (srf,s)

      real*8 e(14)
      real*4 ad(300),ae(28)
      integer*2 kd(600)
      equivalence (d,ad,kd),(e,ae),(ifl(54),isrf)
      equivalence (ifl(51),ia),(ifl(52),ib),(ifl(53),ic)
      integer*2 nwds,ietype
      integer*4 srfkey
c             get surface unibase key
      call gtdesc (sc(isrf+143), srfkey, nwds, ietype)
c          set ptrs to srf in d-tbl
      idx=50*(isrf-1)
      jdx=2*idx
      kdx=4*idx
      jpx=2*mxpnhd*(isrf-1)
      istyp=kd(kdx+1)
c+++
      npans=kd(kdx+2)
c          safety counter for ipatch calls
      ipatk=0
8     npats=kd(kdx+38)
c
c          if no active patch, go search.
      ipat=kd(kdx+41)
      if(ipat.eq.0) goto 900
c          allow some small o/b before activating a patch
      if(u+.001.lt.ad(jdx+15).or.u-.001.gt.ad(jdx+16))goto 900
      goto 50
c          scan s-list this panel for indicated patch
900   do 910 i=2,npats
      if(apnhed(jpx+i+4).ge.u)goto 920
910   continue
c          must be last pat
      i=npats+1
920   ipat=i-1
c          set u-min/max
925   ad(jdx+15)=apnhed(jpx+ipat+4)
      ad(jdx+16)=apnhed(jpx+ipat+5)
      if(ipat.eq.npats) ad(jdx+16)=1.
c          if sfu not in this range, put it at umid
      if(u.lt.ad(jdx+15).or.u.gt.ad(jdx+16)) u=
     1   (ad(jdx+15)+ad(jdx+16))/2.
c          point to activ panel in ranfil
10    ipan=kd(kdx+42)
      isfx=4*mxsfhd*(isrf-1)+2*ipan+7
c      ipg=kd(isfx)
c      iel=kd(isfx+1)
      if(istyp.eq.9)goto 19
c*********************************************  rldsrf
c          read 8 wds this patch into d-tbl
130   continue
cuni130   call getent(d(idx+25),8,ipg,iel,0)
      call gtsptt(srfkey,ipan,ipat,d(idx+25),8)
      rho=ad(jdx+64)
c          read 8 wds next patch into e
cuni140   call getent(e,8,ipg,iel,0)
      call gtsptt(srfkey,ipan,ipat+1,e,8)
      dx=e(1)-d(idx+25)
      dy=e(2)-d(idx+26)
      dz=e(3)-d(idx+27)
c          p7,p8 from next pat
      ad(jdx+70)=dx
      ad(jdx+71)=dy
      ad(jdx+72)=dz
      ad(jdx+73)=dx+ae(7)
      ad(jdx+74)=dy+ae(8)
      ad(jdx+75)=dz+ae(9)
c          zer0 e(2,3) and extrap back for pts 5,6
      e(2)=0.
      e(3)=0.
      do 145 i=64,69
      j=i+6
      k=i-54
      l=k-6
145   ad(jdx+i)=ad(jdx+j)-rho*(ae(k)-ae(l))
      goto 46
c*******************************************  full srf
c          move pointer to ipat in ranfil
19    continue
c          read 14 wds this patch into d-tbl
cuni30    call getent(d(idx+25),14,ipg,iel,0)
      call gtsptt(srfkey,ipan,ipat,d(idx+25),14)
      rho=ad(jdx+76)
c          read 14 wds next patch into e
cuni40    call getent(e,14,ipg,iel,0)
      call gtsptt(srfkey,ipan,ipat+1,e,14)
      dx=e(1)-d(idx+25)
      dy=e(2)-d(idx+26)
      dz=e(3)-d(idx+27)
c          note.  improve the following later. <<<<<<<<<<
      ad(jdx+88)=dx
      ad(jdx+89)=dy
      ad(jdx+90)=dz
      ad(jdx+91)=dx+ae(7)
      ad(jdx+92)=dy+ae(8)
      ad(jdx+93)=dz+ae(9)
      ad(jdx+94)=dx+ae(10)
      ad(jdx+95)=dy+ae(11)
      ad(jdx+96)=dz+ae(12)
      ad(jdx+97)=dx+ae(13)
      ad(jdx+98)=dy+ae(14)
      ad(jdx+99)=dz+ae(15)
c          zer0 e(2,3) and extrap back for pts 9-12
      e(2)=0.
      e(3)=0.
      do 45 i=76,87
      j=i+12
      k=i-60
      l=k-12
45    ad(jdx+i)=ad(jdx+j)-rho*(ae(k)-ae(l))
c          upshift origpt 2 locs and zero pt1  ....!@#$%^&*?@#$/^%!!
46    d(idx+23)=d(idx+25)
      d(idx+24)=d(idx+26)
      d(idx+25)=d(idx+27)
      ad(jdx+52)=0.
      ad(jdx+53)=0.
      ad(jdx+54)=0.
c          record active pat and begin at midpt.
      kd(kdx+41)=ipat
c          u,v=.5 init
c      u=.5
c      v=.5
c****************************************************
c
50    continue
c          u,v are input
c          cpt is te, for now.  later set by calling subr.
cc    srf(8,isrf)=      )
cc    srf(9,isrf)=      )  set in calling subr.
cc    srf(10,isrf)=     )
c***************************************
      ipatk=ipatk+1
55    call ipatch(u,v,iret)
c***************************************
      if(iret.eq.0.or.iret.eq.3)goto 99
      kret=iret
      if (ipatk.lt.30) goto 60
c                  30 ipatch calls - if not within tol giv error
      vx=srf(8,isrf)-srf(5,isrf)
      vy=srf(9,isrf)-srf(6,isrf)
      vz=srf(10,isrf)-srf(7,isrf)
      sec=sqrt(vx**2+vy**2+vz**2)
      if (sec.lt.sc(27)) goto 99
      vx=vx/sec
      vy=vy/sec
      vz=vz/sec
      co=abs(vx*srf(1,isrf)+vy*srf(2,isrf)+vz*srf(3,isrf))
      if (co.gt..9999) goto 99
      if (sec*sqrt(1-co**2).lt.sc(27)) goto 99
      ifl(2)=138
      goto 99

60    if(iabs(kret).eq.1)goto 80
c          must be v-bdy violation  ( iret= +/- 2 )
c          if another panel there, activate it
65    ipan=kd(kdx+42)
      ipan=ipan+kret/2
      if(ipan.gt.0.and.ipan.le.npans)goto 70
      if (ifl(306).eq.0) goto 89
      kret=ifl(306)
      ifl(306)=0
      goto 80
c          get panel into panhed
70    isfx=4*mxsfhd*(isrf-1)+2*ipan+7
c      kpx=kdx+2*ipan+7
cuni      call getent(d(idx+10),12,kd(kpx),kd(kpx+1),0)
      call gtspan(srfkey,ipan,panhed(mxpnhd*(isrf-1)+1))
      d(idx+10)=panhed(mxpnhd*(isrf-1)+1)
      kd(kdx+42)=ipan
      fnpans=npans
c          set v min/max
      fipan=ipan
      ad(jdx+18)=fipan/fnpans
      ad(jdx+17)=ad(jdx+18)-1./fnpans
      if (ipan.eq.1) ad(jdx+17)=0.0
c          if v o/b, set it halfway
      if(v.lt.ad(jdx+17).or.v.gt.ad(jdx+18))
     1  v=(ad(jdx+17)+ad(jdx+18))/2.
c          force a new pat sch
      kd(kdx+41)=0
      goto 8
c          u-bdy violation. try up or down patch (if any)
80    ipat=ipat+kret
      if(ipat.gt.0.and.ipat.le.npats)goto 85
      if (ifl(306).eq.0) goto 89
      kret=ifl(306)
      ifl(306)=0
      goto 65
85    kd(kdx+41)=ipat
c          go updat u min/max
      goto 925
c          unfixable bdy violation.  set warning flag.
c89    ifl(2)=-127
89    continue
99    return
      end
