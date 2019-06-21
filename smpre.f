c*****************************************************
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       smpre.f , 25.1
c**    DATE AND TIME OF LAST  MODIFICATION
c**       04/29/15 , 15:10:43
c**
c*****************************************************
c**
c** COPYRIGHT (c) 1986 MILLS DATA SYSTEMS CORPORATION
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: smpre
c **
c **  purpose of subroutine: prepare a super surface from a collection
c **                         of mesh or ncl surfaces. 
c **
c **********************************************************************
c **********************************************************************
 
      subroutine smpre
 
      include 'com4a.com'
 
      PARAMETER (MAXPT=50)
      PARAMETER (MAXPN=20)
      PARAMETER (MXSFHD=(2+(MAXPN+1)/2))
      PARAMETER (MXPNHD=(2+(MAXPT+1)/2))
 
      common/pblok/p(400)
      common/wblok/w(600)

      real*8 p,w,asn,panel(200),srfhed(MXSFHD),u8,v8,sv(9)
      real*4 aw(1200),p4(800),ap(800),apan(400)
      integer*2 kw(4),ksn(4),levn(4),ip(4),ipanel(800),isfhed(28)
      integer*4 jsn(2),isfkey,ipnkey,i4sc10(20),i4w(1200)
      equivalence (aw,kw,i4w,w),(asn,jsn,ksn),(p,p4,ip),(p4(4),ap)
      equivalence (w(400),panel,apan,ipanel),(srfhed,isfhed)
      integer*2 ktv(4),ipata(4),ix1(4),ix2(4),ix3(4),ix4(4),ietype
      equivalence (tv,ktv),(sc(10),i4sc10)
      logical nored

c      data ix1/1,13,25,37/,ix2/10,22,34,46/
c      data ix3/1,4,7,10/,ix4/37,40,43,46/

c      data ix1/1,10,1,37/,ix2/13,22,4,40/
c      data ix3/25,34,7,43/,ix4/37,46,10,46/

      data ix1/1,37,1,10/,ix2/4,40,13,22/
      data ix3/7,43,25,34/,ix4/10,46,37,46/

      nored=.false.
      nsf=isc10(3)
      kw(1)=27
      kw(2)=nsf
c          get surfaces from sc-table/ranfil
      n=nsf
      if (n.gt.12)n=12
      do 10 i=1,n
10    i4w(2+i)=i4sc10(i*2+1)
      if (nsf.lt.13) goto 12
      ipg=ifl(4)+1
      call getran (w(14),ipg)
      do 11 i=13,nsf
11    i4w(2+i)=i4w(i*2+1)
12    continue
c                              get center point of each side of each surface
      ipx=98
      do 200 isf=1,nsf
      isfkey=i4w(isf+2)
      call isitwf (isfkey, isftyp)
      if (isftyp.eq.1) goto 125
      call gtgeo(isfkey,srfhed)
      if (isfhed(1).eq.26) goto 120
c                                      ncl surface
      numpan=isfhed(2)
      levn(1)=0
      if((numpan/2)*2.eq.numpan) levn(1)=1
      levn(2)=levn(1)
      isvpat=0
      isvpan=0
      goto 125
c                                      mesh surface
120   m=isfhed(3)
      n=isfhed(4)
      ipata(1)=(n/2)*m+1
      ipata(2)=((n+2)/2)*m
      ipata(3)=(m+2)/2
      ipata(4)=ipata(3)+(n-1)*m
      levn(1)=0
      levn(3)=0
      if((n/2)*2.eq.n)levn(1)=1
      if((m/2)*2.eq.m)levn(3)=1
      levn(2)=levn(1)
      levn(4)=levn(3)

125   do 200 i=1,4
      ipx=ipx+3
      if (isftyp.eq.0) goto 127
      u8=.5
      v8=.5
      if (i.eq.1) u8=0.
      if (i.eq.2) u8=1.
      if (i.eq.3) v8=0.
      if (i.eq.4) v8=1.
      call evstup (isfkey, 2)
      call uevsrf (u8, v8, 2, sv)
      p4(ipx)   = sv(1)
      p4(ipx+1) = sv(2)
      p4(ipx+2) = sv(3)
      goto 190
c                             choose patch for center of this side.
127   if (isfhed(1).eq.26) goto 130
c                                     ncl surface
      ipan=1
      if (i.lt.3)ipan=(numpan+2)/2
      if (i.eq.4)ipan=numpan
c      ipg=isfhed(ipan*2+7)
c      iel=isfhed(ipan*2+8)
c      call getent(panel,1,ipg,iel,0)
      call gtspa1(isfkey,ipan,panel,ipnkey)
      npats=ipanel(2)
      ipat=1
      if (i.eq.2) ipat=npats
      if (i.gt.2) then
        ipat=(npats+2)/2
        levn(i)=0
        if((npats/2)*2.eq.npats)levn(i)=1
      endif
      if (ipat.eq.isvpat.and.ipan.eq.isvpan) goto 160
      isvpat=ipat
      isvpan=ipan
c      call patexp(ipg,iel,ipat,panel(1),p)
      call patexp(ipnkey,ipat,panel(1),p)
      goto 160
c                                         mesh surface
130   continue
c      itmp=(ipata(i)-1)*26
c      ipg=ksn(1)+itmp/35
c      iel=ksn(2)+itmp-(itmp/35*35)+1
c140   if (iel.lt.36) goto 150
c      ipg=ipg+1
c      iel=iel-35
c      goto 140
c150   call getent(p,26,ipg,iel,0)
      call gtmpat(isfkey,ipata(i),p)

c              calculate center point 
160   x0=p(1)
      y0=p(2)
      z0=p(3)
      ap(1)=0
      ap(2)=0
      ap(3)=0
      i1=ix1(i)
      i2=ix2(i)
      i3=ix3(i)
      i4=ix4(i)
      if (levn(i).eq.0) goto 180
c             use corner of patch if even number of patches on this side
      p4(ipx)=ap(i1)+x0
      p4(ipx+1)=ap(i1+1)+y0
      p4(ipx+2)=ap(i1+2)+z0
      goto 190
180   p4(ipx)=.125*ap(i1)+.375*ap(i2)+.375*ap(i3)+.125*ap(i4)+x0
      p4(ipx+1)=.125*ap(i1+1)+.375*ap(i2+1)+.375*ap(i3+1)+.125*ap(i4+1)
     x          +y0 
      p4(ipx+2)=.125*ap(i1+2)+.375*ap(i2+2)+.375*ap(i3+2)+.125*ap(i4+2)
     x          +z0
190   continue
c...
c      write(19,9010)p4(ipx),p4(ipx+1),p4(ipx+2)
c9010  format('pt/'f12.4','f12.4','f12.4)
c...
c RAH: added call to set intr flag - effective on SGI ONLY
      call ckintr(ifl(86),ifl(35))
      if (ifl(86).eq.1) goto 9320
200   continue      
c                         build a map
220   iwst=84
      do 300 i=1,nsf*4
300   kw(iwst+i)=0
      do 420 isf=1,nsf
         do 420 ipx=1,nsf
            if (ipx.eq.isf) goto 415
            do 410 i=1,4
               k=(ipx-1)*4+i
               if (kw(k+iwst).gt.0) goto 400
               j=(ipx-1)*12+i*3+98
               call chkedg(p4(j),i4w(isf+2),isd)
               if (isd.eq.0) goto 400
               kw(k+iwst)=isf
               k=(isf-1)*4+isd
               if (kw(k+iwst).gt.0) goto 400
               kw(k+iwst)=ipx
400            continue
c RAH: added call to set intr flag - effective on SGI ONLY
               call ckintr(ifl(86),ifl(35))
               if (ifl(86).eq.1) goto 9320
410         continue
            go to 420
c RAH: added call to set intr flag - effective on SGI ONLY
415         call ckintr(ifl(86),ifl(35))
            if (ifl(86).eq.1) go to 9320
420   continue
c...
c      write(19,9020)(kw(k+iwst),k=1,nsf*4)
c9020  format(4i4)
c...
      ifl(2)=0
c      n=2*nsf+1
c      call putent(w,n,ipg,iel,nored,0)
      call ptntsf(w,isfkey)
c
c         To inable the RESET/DISPLY,sf for net surface.
c
      if (ldspsf) then
      else
         call blkgeo(isfkey, 1)
      endif
      ietype=9
      call ptdesc(isfkey, ietype, tv)
      ktv(3)=nsf
      rest=tv
      goto 999
c                      operation aborted by operator
9320  ifl(2)=320
      err=.true.

999   return
      end

      subroutine chkedg(pt,isfkey,isd)

      include 'com4a.com'
      include 'mocom.com'
      include 'drvcom.com'

      real*4 pt(3)
      integer*4 isfkey
      integer*2 isd

c          motion common equivalences
      real*4 srf(10,3)
      equivalence (srf,s)
 
      real*4 ad(300)
      integer*2 kd(600)
      equivalence (d,ad,kd),(ifl(54),isrf)

      integer*4 ipnkey

      isd=0
      isrf=2
      isx=(isrf-1)*mxsfhd
      idx=50
      jdx=100
      kdx=200
      u=.5
      v=.5
      srf(8,isrf)=pt(1)
      srf(9,isrf)=pt(2)
      srf(10,isrf)=pt(3)
      call isitwf (isfkey, isftyp)
      if (isftyp.eq.0) goto 10
      call evstup (isfkey, isrf)
      ad(jdx+2)=1.
      call usrfpn (u, v, iret)
c      iret = 1
c      if ((pt(1)-srf(5,isrf))**2+(pt(2)-srf(6,isrf))**2
c     x  +(pt(3)-srf(7,isrf))**2 .lt. 2*sc(27)**2) iret = 0
      goto 30
10    call ptdesc(isfkey,9,sc(145))
      call gtgeo(isfkey,srfhed(isx+1))
      d(idx+1)=srfhed(isx+1)
      d(idx+2)=srfhed(isx+2)
      isftyp=kd(kdx+1)

      if (isftyp.ne.26) goto 20
      kd(kdx+37)=kd(kdx+4)
      kd(kdx+38)=kd(kdx+3)
      kd(kdx+39)=0
      kd(kdx+1)=26
      ad(jdx+2)=1.
      call meshpn (u,v,iret)
      goto 30

20    continue
      ipx=(isrf-1)*mxpnhd
      call gtspa1(isfkey,1,panhed(ipx+1),ipnkey)
      d(idx+10)=panhed(ipx+1)
c      call getent (d(idx+10),12,kd(kdx+9),kd(kdx+10),0)
c          set actpat=0  and  actpan=1
      kd(kdx+41)=0
      kd(kdx+42)=1
c          set vmin,vmax this panel
      fnpans=kd(kdx+2)
      ad(jdx+17)=0.
      ad(jdx+18)=1./fnpans
c          if rldsrf, set sftyp=21
      kd(kdx+1)=9
      if(kd(kdx+37).eq.1) kd(kdx+1)=21
      ad(jdx+2)=1.
      call nsrfpn (u,v,iret)

30    continue
      if (iret.ne.0) goto 999
      if (abs(u).le..002)isd=1
      if (abs(1.-u).le..002)isd=2
      if (abs(v).le..002)isd=3
      if (abs(1.-v).le..002)isd=4

999   return
      end
