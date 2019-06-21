c*****************************************************
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       offpre.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:23
c*****************************************************
c**
c** copyright (c)   1987   mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name:  offpre             written pem  20-jan-1987
c **                                                                  **
c **  purpose of subroutine: create a surface offset a given distance
c **                         from an existing surface per command --
c **
c **                           sf/offset,sf1,xyzls,dist
c **
c **  note:  new sf is identical to old but with a '7' and dist in wd2.
c **         this will cause a new sfpt calc when applied in subrs
c **         ipatch, pncalc, dsplsf.
c **  
c **         Store new hedr only 
c **           - NCL502 changed to store complete surface
c **
c **********************************************************************
c **********************************************************************

      subroutine offpre

      include 'com4a.com'
      include 'wrksys.com'

      common/pblok/p(400)
      common/wblok/w(600)

      integer*2 ksn(4),kw(200),ksc(100),ktv(4)
      real*4 aw(100),bsn(2),ae(80)
      real*8 p,w,asn,e(40)
      equivalence (asn,bsn,ksn),(w,aw,kw),(w(25),e,ae),(tv,ktv),(sc,ksc)
      integer*4 nclkey,ipnkey,nsfkey,keyvec
      integer*2 nwds,ietype, isftyp
      real*8 dis, vec(6), xm,ym,zm
      equivalence (vec(1),xm),(vec(2),ym),(vec(3),zm)
      logical trflg
c
c...get modifier or direction vector
c
      call gtdesc(sc(12),keyvec,nwds,mod)
      if (keyvec.gt.0) then
        trflg = .true.
        call gtentt(sc(12),trflg,keyvec,ietype,vec)
        if (ietype.eq.PNTVEC) then
          vec(1) = vec(4)
          vec(2) = vec(5)
          vec(3) = vec(6)
        endif
      else
        xm=0.
        ym=0.
        zm=0.

        if(mod.eq.638)xm=1.
        if(mod.eq.639)ym=1.
        if(mod.eq.640)zm=1.
        if(mod.eq.641)xm=-1.
        if(mod.eq.642)ym=-1.
        if(mod.eq.643)zm=-1.
      endif
c                          get sf1 hedr into w-tbl
      asn=sc(11)
cuni      call getent(w,7,ksn(1),ksn(2),ksn(4))
      ietype = ksn(4)
      call gtgeom(sc(11),w,nclkey,nwds,ietype)
      call isitwf (nclkey, iwf)
      if (iwf .eq. 1) then
        defwf = .true.
        dis = sc(13)
        if (ifl(264).eq.1) dis = dis/25.4d0
        j = 0
        if (lwrk) j=1
        call ofwfsf (nclkey,vec,dis,ifl(72),sc(68),j,invwrk,nsfkey)
        if (nsfkey.eq.0) goto 9321
        goto 90
      endif
      call sftype (nclkey, isftyp)
c                               ncl & mesh sfs only
      if (isftyp.eq.91) goto 15
      if (isftyp.eq.25) goto 5
      if (isftyp.eq.26) goto 10
      goto 9321
c                                   quilt sf - get center patch
5     npats=kw(2)
      ipat=(npats+1)/2
      call gtqpat(nclkey,ipat,e)
      if (ifl(72) .eq. 1) call trnqlt(e, sc(68))
      vx=ae(7)
      vy=ae(31)
      vz=ae(57)
      ux=ae(11)
      uy=ae(35)
      uz=ae(61)
      goto 30
c                                   mesh sf - get center patch
10    npats=kw(2)
      ipat=(npats+1)/2
      call gtmptt(nclkey,ipat,e)
      if (ifl(72) .eq. 1) call trnmsh(e, sc(68))
      vx=ae(7)
      vy=ae(8)
      vz=ae(9)
      ux=ae(16)
      uy=ae(17)
      uz=ae(18)
      goto 30

c                       ncl surface - get mid panl in w(10)   12 wds
15    npans=kw(2)
      ipan=(npans+1)/2
      bsn(1)=aw(ipan+4)
      ipg=ksn(1)
      iel=ksn(2)
cuni      call getent(w(10),12,ipg,iel,0)
      call gtspa1(nclkey,ipan,w(10),ipnkey)
c                   get midpatch this panl in p-tbl
      npats=kw(38)
      ipat=(npats+1)/2
      ipntyp=9
      nwds=14
      if(kw(37).eq.0) goto 20
      nwds = 8
      ipntyp=21
20    call gtpptt(ipnkey,ipat,e,nwds)
c      if(ipntyp.eq.9)goto 30
c     *********************************  rldsrf  
c                               move iel ptr to midpat and get 8 wds
c      iel=iel-6+8*ipat+(npats+1)/2
c20    if(iel.lt.36)goto 22
c      iel=iel-35
c      ipg=ipg+1
c      goto 20
c22    call getent(e,8,ipg,iel,0)
c      goto 40
c     *********************************** full srf  get 12 wds
c30    iel=iel-12+14*ipat+(npats+1)/2
c32    if(iel.lt.36)goto 34
c      iel=iel-35
c      ipg=ipg+1
c      goto 32
c34    call getent(e,12,ipg,iel,0)
c                 do u,v vecs at patch orig corner
      vx=ae(7)
      vy=ae(8)
      vz=ae(9)
      j=10
      if(ipntyp.eq.9)j=16
      ux=ae(j)
      uy=ae(j+1)
      uz=ae(j+2)
c                 normal 'w' direc
30    xn=uy*vz-uz*vy
      yn=uz*vx-ux*vz
      zn=ux*vy-uy*vx

c                 do direc indicated by xyzls modifier and compare to 'w'
      dis=sc(13)
c                if input direc opposite 'w', switch dis
      if(xm*xn+ym*yn+zm*zn.lt.0.)dis=-dis
      if (ifl(72).eq.1) dis = dis*dsqrt(sc(56)**2+sc(57)**2+sc(58)**2)
c                if sf1 also type 7, chg oldis per dis
      odis=0.
      if(kw(5).eq.7)odis=aw(4)
c                fix w(2) for new sf and sto 7 wds
      kw(5)=7
      aw(4)=dis+odis

      if (isftyp.eq.25) goto 70
      if (isftyp.eq.26) goto 80

      call ptshed(w,nsfkey)
      do 60 ipan=1,npans
      call gtspa1(nclkey,ipan,w(10),ipnkey)
      nsz=14
      if(kw(37).eq.1)nsz=8
      npats=kw(38)
      ix=12+(npats+1)/2
      do 50 ipat=1,npats+1
      call gtppat(ipnkey,ipat,w(ix))
50    ix=ix+nsz
60    call ptspan(nsfkey,ipan,w(10))
      goto 90
c      call putent (w,7,jpg,jel,nored,0)
c      ktv(1)=jpg
c      ktv(2)=jel

70    call ptqhed(w,nsfkey)
      do 74 ipat=1,npats
      call gtqpat(nclkey,ipat,e)
74    call ptqpat(nsfkey,ipat,e)
      goto 90

80    call ptmhdt(w,nsfkey)
      call ptmpat(nsfkey,npats,e)
      do 84 ipat=1,npats
      call gtmpat(nclkey,ipat,e)
84    call ptmpat(nsfkey,ipat,e)

90    ktv(3)=ksc(43)
      call ptdesc(nsfkey,ietype,tv)
      rest=tv
      if (.not. ldspsf) call blkgeo (nsfkey, 1)
      goto 99

9321  ifl(2)=321
      err=.true.
 
99    return
      end
