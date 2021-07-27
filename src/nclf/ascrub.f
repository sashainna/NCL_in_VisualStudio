C*********************************************************************
C*    NAME         :  ascrub.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ascrub.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:37
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ascrub(srfkey)
C*       description
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
      subroutine ascrub(srfkey,pankey)

c             created 8-apr-83       ( from scrub break-up )
c
c          scrub/sf1,m,n          ( m-passes, n-pts per pass )
c          scrub/sf1,m,n,pt1,pt2,pt3,pt4

      include 'com.com'
      include 'mocom.com'

      integer*4 srfkey,pankey

       common/scrblk/e,qx,qy,qz,dqx,dqy,dqz,sfu,sfv,xsf,ysf,zsf,
     1 vna,vnb,vnc,uold

      real*4 sfu,sfv,xsf,ysf,zsf,vna,vnb,vnc,uold
      real*8 dbuf(9),hbuf(6),e(14),tdat(420)
      real*4 ad(300),ae(28),qx(4),qy(4),qz(4),dqx(4),dqy(4),dqz(4)
      integer*2 ksc(500),kd(600),stepno
      equivalence (sc,ksc),(d,ad,kd),(e,ae)
      real*4 cdir,r1,delx,dely,delz,ti,tj,tk,fx,fy,fz,dtol,drob,droa
      real*4 rob,adirec,roa,cco,chd,dx,dy,dz,x,y,z,co,disn,diso,hx,hy,hz
      real*4 ha,hb,hc,dn,doo,rx,ry,rz,alx,aly,alz,seca,blx,bly,blz,secb
      real*4 ust,vst,und,vnd,oa,ob,vnx,vny,vnz,sec,cosb,sinb,cosab,sinab
      real*4 oc,adis,ro,ddx,ddy,ddz,ddis,sal,tdis,ra,rb,rc,secr,sinc
      real*4 cosc,wdis,sdis,holdx,holdy,holdz,holda,holdb,holdc,fsec
      real*4 dlx,dly,dlz,hldu,hldv
      integer*2 max,iclass,kcl,mid,np,ntd,iprt,inudir,i,kad2,isc12
      integer*2 isc13,jpst,jpnd,ipst,ihold
      logical first,lv92

      lv92 = sc(169) .lt. 9.249d0
cc      if (lexpcl) then
      if (.not. lv92) then
        max = 420
        iclass = 5200
      else
        max = 120
        iclass = 5000
      endif
      first=.true.
c  if clfile,aptsrc, or tape count, turn on clcall
c      kcl=0
c      if(ifl(69).ne.0.or.ifl(88).eq.1.or.ifl(99).ne.0) kcl=1
      kcl = 1
c              load ijk in dbuf (in case multax on).           11-july-83
      dbuf(4)=sc(4)
      dbuf(5)=sc(5)
      dbuf(6)=sc(6)
      dbuf(7)=sc(7)
      dbuf(8)=sc(8)
      dbuf(9)=sc(9)
c     set online plot indicator (unless dntcut) and re-issue
c                last given pu-pd command.
      mid = 0
      np=0
      ntd=0
      stepno=0
cc      kpl=0
      iprt=0
      if(ifl(26).ne.0)iprt=1
      cdir=-1.
      inudir=0.
      if (isc10(2).eq.1) then
        call gtclsd (srfkey, 0, inudir)
        call gtclsd (srfkey, 1, i)
        if (inudir.eq.1 .or. i.eq.1) cdir=1.
      endif
cc      if(ifl(100).eq.1.and.ifl(42).eq.0) kpl=1
cc      if(kpl.eq.0)goto 12
cc      dbuf(1)=sc(1)
cc      dbuf(2)=sc(2)
cc      dbuf(3)=sc(3)
cc      call plotm(dbuf,.true.)
cc12    continue
c     r1=dist from sfpt to ball-end ctr (sph r + pthk)
      r1=tool(2)+sc(23)
c     delxyz are from sph r. center to te
      delx=tool(2)*sc(4)
      dely=tool(2)*sc(5)
      delz=tool(2)*sc(6)
      ti=sc(4)
      tj=sc(5)
      tk=sc(6)
C               INIT FWD SENSE TO ZERO FOR SIDEWISE CHKS    12-JUN-89
      FX=0.
      FY=0.
      FZ=0.
c     init sfuv generator
c                sc(12),sc(13)=integer
      kad2=0
      isc12=sc(12)
      sc(12)=isc12
      isc13=sc(13)
      sc(13)=isc13
c                init holdback params
      ihold=0
      jpst=1
      jpnd=0
      dtol=sc(27)
c                m=1 min,  n=2 min
      if(sc(12).lt.1..or.sc(13).lt.2.) goto 998
      drob=2.
      if(sc(12).gt.1.) drob=1./(sc(12)-1.)
      droa=1./(sc(13)-1.)
      rob=-drob
      adirec=cdir
      roa=2.
      ipst=0
c24may89          set srf direction per ave u,v   (not per first motion)
      sfu=(ad(101)+ad(103)+ad(105)+ad(107))/4.
      sfv=(ad(102)+ad(104)+ad(106)+ad(108))/4.
      call pncalc(srfkey,pankey)
      cco=ti*vna+tj*vnb+tk*vnc
      if(cco.lt.0.) ad(2)=-1.
c24may89 end of chg
      goto 212

c  ************** sfuv generator ***********************
21    roa=roa+droa*adirec
      if(roa.gt.1.002.or.roa.lt.-.002) goto 212
      goto 218
c     init new pass (if any)
212   rob=rob+drob
      jpnd=0
c               if anything in tdat, o/p now
      if(ntd.lt.1) goto 2120
      call putcl (iclass,5,np,tdat)
      np=0
      ntd=0
2120  continue
c               add tape break at pass-end                  11-mar-83
      if(kad2.eq.1.and.ifl(144).eq.0)call putcl(2000,16,1,dbuf(1))
      if(kad2.eq.1)ipst=1
      if(rob.lt.1.002) goto 213
c++             add last loc to sc-tbl and exit              2-mar-83
      sc(1)=dbuf(1)
      sc(2)=dbuf(2)
      sc(3)=dbuf(3)
c++             save fwd sense                               27-mar-86
      if (chd.le.0)goto 2121
      sc(7)=dx/chd
      sc(8)=dy/chd
      sc(9)=dz/chd

2121  continue
      if (ifl(23).eq.0) goto 2122
c                                   if 'ta/normal,ps' save off last tool axis
      sc(4)=dbuf(4)
      sc(5)=dbuf(5)
      sc(6)=dbuf(6)

c++             if print/small, print the last point - epm 10-18-84
2122  if (ifl(154).eq.0.and.motdfl) then
        stepno=stepno+1
        do 2123 i=1,6
2123    hbuf(i)=dbuf(i)
        if (ifl(73).eq.1.and.ifl(267).eq.1) then
          call conent(hbuf,sc(41),3)
          if (ifl(82).eq.1) call conent(hbuf(4),sc(41),4)
        endif
        if (ifl(82).eq.0) then
          write (cout,1010) stepno,(hbuf(i),i=1,3)
          call putmsg (cout,43,ifl(139),0)
        else
          write (cout,1010) stepno,(hbuf(i),i=1,6)
          call putmsg (cout,79,ifl(139),0)
        endif
      endif
      goto 999
c++
213   adirec=adirec*cdir
      roa=0.
      if(adirec.gt.0.)goto 214
      roa=1.
c     uv start and end
214   ust=ad(101)+rob*(ad(105)-ad(101))
      vst=ad(102)+rob*(ad(106)-ad(102))
c     uv end deltas
      und=ad(103)+rob*(ad(107)-ad(103))-ust
      vnd=ad(104)+rob*(ad(108)-ad(104))-vst
c     sf uv calc
218   continue
      hldu = sfu
	hldv = sfv
      sfu=ust+roa*und
      sfv=vst+roa*vnd
      if (inudir.eq.1) then
        sfv=ust+roa*und
        sfu=vst+roa*vnd
      endif
c
      call pncalc(srfkey,pankey)
c                  (old xyz is in dbuf)
350   if(ipst.eq.0)goto 351
      oa=vnx
      ob=vny
      oc=vnz
      mid=0
351   vnx=vna
      vny=vnb
      vnz=vnc
      sec=sqrt(vnx**2+vny**2+vnz**2)*ad(2)
      if(abs(sec).gt.0.)goto 352
c                  error.  sec=0
      ifl(2)=128
      goto 998
352   vnx=vnx/sec
      vny=vny/sec
      vnz=vnz/sec
      co=vnx*ti+vny*tj+vnz*tk
cmc		ist tim here compare sfnrm to ijk
cm      if(kad2.eq.1)goto 360
      kad2=1
cmc               add test for tool parlel ps at start           2-feb-87
cm      if(abs(co).gt..0001)goto 354
cm      ifl(2)=319
cm      goto 999
cm354   if(co.gt.0.)goto 360
cm      ad(2)=-1.
cm      goto 350
c               cos may not be neg   (tool top edge contact)
360   if(co.ge.-.005)goto 362
c                 skip this test if ta/normal,ps              pem 29-jan-87
      if(ifl(23).eq.1) goto 362
      ifl(2)=148
      goto 998
362   continue
      if (ifl(23).eq.1) goto 364

c               te is r1 up along sfnm, sph. r dn along tlaxis
      x=xsf+vnx*r1-delx
      y=ysf+vny*r1-dely
      z=zsf+vnz*r1-delz
      goto 366
c                     ta/normal,ps - just apply thick to sf point
364   x=xsf+vnx*sc(23)
      y=ysf+vny*sc(23)
      z=zsf+vnz*sc(23)

366   if(ipst.eq.0)goto 380
      ipst=0
      jpst=1
c               check this loc vs old
      dx=x-dbuf(1)
      dy=y-dbuf(2)
      dz=z-dbuf(3)
      disn=dx*oa+dy*ob+dz*oc
      diso=vnx*dx+vny*dy+vnz*dz
      diso=-diso
c               if concave case, st-ln jog
      if(disn.ge.0..and.diso.ge.0.)goto 380
c               midpt reqd.
370   mid=1
      hx=x
      hy=y
      hz=z
      ha=vnx
      hb=vny
      hc=vnz
      if(disn.lt.0..and.diso.lt.0.)goto 375
cxxx             this is a very rough guess at midpt     28-mar-83
c                  when curve inflects
      dn=-disn/4.
      doo=-diso/4.
      if(dn.lt.0.)dn=0.
      if(doo.lt.0.)doo=0.
      x=dbuf(1)+dx/2.+oa*dn+vnx*doo
      y=dbuf(2)+dy/2.+ob*dn+vny*doo
      z=dbuf(3)+dz/2.+oc*dn+vnz*doo
      goto 377
cxxx
c                 midpt calc   (probable case)           28-mar-83
375   chd=sqrt(dx**2+dy**2+dz**2)
      if(chd.lt..002)goto 379
c                  ave rgt sense
      rx=dy*(oc+vnz)-dz*(ob+vny)
      ry=dz*(oa+vnx)-dx*(oc+vnz)
      rz=dx*(ob+vny)-dy*(oa+vnx)
c                  a-line
      alx=rz*ob-ry*oc
      aly=rx*oc-rz*oa
      alz=ry*oa-rx*ob
      seca=sqrt(alx**2+aly**2+alz**2)
      blx=rz*vny-ry*vnz
      bly=rx*vnz-rz*vnx
      blz=ry*vnx-rx*vny
      secb=sqrt(blx**2+bly**2+blz**2)
      if(seca.eq.0..or.secb.eq.0.)goto 379
      cosb=(dx*blx+dy*bly+dz*blz)/(chd*secb)
      if(abs(cosb).ge.1.)cosb=.999999
      sinb=sqrt(1.-cosb**2)
      cosab=(alx*blx+aly*bly+alz*blz)/(seca*secb)
      if(abs(cosab).ge.1.)cosab=.999999
      sinab=sqrt(1.-cosab**2)
      adis=chd*sinb/sinab/seca
c              if flat case, adis = chd/2
      if(cosab.ge..999998)adis=.5*chd/seca
      x=dbuf(1)+alx*adis
      y=dbuf(2)+aly*adis
      z=dbuf(3)+alz*adis
c              if solved point in-line with dbuf-hxyz or not between
c              same, backout from midpt process.
377   ro=dx*(x-dbuf(1))+dy*(y-dbuf(2))+dz*(z-dbuf(3))
      ro=ro/(dx**2+dy**2+dz**2)
      if(ro.lt..1.or.ro.gt..9)goto 379
      ddx=dbuf(1)+dx*ro-x
      ddy=dbuf(2)+dy*ro-y
      ddz=dbuf(3)+dz*ro-z
      ddis=sqrt(ddx**2+ddy**2+ddz**2)
      if(ddis.le.dtol)goto 379
      oa = vnx
      ob = vny
      oc = vnz
      if(ifl(23).eq.0)goto 380
c
c...Calculate the tool axis for calculated mid point.
c
	sfu = hldu + (sfu-hldu)/2.0
	sfv = hldv + (sfv-hldv)/2.0
      call pncalc(srfkey,pankey)
      sec=sqrt(vna**2+vnb**2+vnc**2)*ad(2)
      if(sec.eq.0.)then
        ifl(2)=128
        goto 998
      endif
      vnx=vna/sec
      vny=vnb/sec
      vnz=vnc/sec
      goto 380
c                midpt backout  (mid=off , restore xyz)
379   mid=0
      x=hx
      y=hy
      z=hz
380   if(jpst.eq.1)goto 400
c                if toler .lt. .0001, output all computed pts.
      if(dtol.lt..0001)goto 400
      if(roa.gt..995.or.roa.lt..005) jpnd=1
c              check for in-line with previous loc.  this check includes
c               both curvature and twist of surface.
385   dx=x-dbuf(1)
      dy=y-dbuf(2)
      dz=z-dbuf(3)
      chd=sqrt(dx**2+dy**2+dz**2)
      if(chd.lt..001)chd=.001
      sal=(dx*(vnx-oa)+dy*(vny-ob)+dz*(vnz-oc))/chd
      tdis=chd*abs(sal)/8.
c               now effective twist crwn
      ra=dy*vnz-dz*vny
      rb=dz*vnx-dx*vnz
      rc=dx*vny-dy*vnx
      secr=sqrt(ra**2+rb**2+rc**2)
      if(secr.le.0.)goto 386
      sinc=(ra*(vnx-oa)+rb*(vny-ob)+rc*(vnz-oc))/secr/2.
      if(abs(sinc).gt..99)goto 386
      cosc=sqrt(1.-sinc**2)
      wdis=abs(r1*(1.-cosc))
      if(wdis.gt.tdis)tdis=wdis
386   if(abs(tdis).gt.dtol)goto 390

CJUN89        ADD SIDEWISE CHECK                12-JUN-89
C              
      DLX=DBUF(1)+CHD*FX-X
      DLY=DBUF(2)+CHD*FY-Y
      DLZ=DBUF(3)+CHD*FZ-Z
      SDIS=SQRT(DLX**2+DLY**2+DLZ**2)
      IF(SDIS.GT.8.*DTOL)GOTO 390
CJUN89END

c              in-line.  hold this xyz and go nexpt
      if(jpnd.ne.0)goto 390
387   ihold=1
      holdx=x
      holdy=y
      holdz=z
      holda=vnx
      holdb=vny
      holdc=vnz
      goto 21
c              if any in hold, o/p now
390   if(ihold.eq.0)goto 400
      hx=x
      hy=y
      hz=z
      ha=vnx
      hb=vny
      hc=vnz
      x=holdx
      y=holdy
      z=holdz
      vnx=holda
      vny=holdb
      vnz=holdc
c              load dbuf for output
C                (FIRST DO A FWD SENSE)              12-JUN-89
400   FX=X-DBUF(1)
      FY=Y-DBUF(2)
      FZ=Z-DBUF(3)
      FSEC=SQRT(FX**2+FY**2+FZ**2)
      IF(FSEC.LT..001)FSEC=.001
      FX=FX/FSEC
      FY=FY/FSEC
      FZ=FZ/FSEC
C                CONTINUE 
      dbuf(1)=x
      dbuf(2)=y
      dbuf(3)=z
      dbuf(7)=fx
      dbuf(8)=fy
      dbuf(9)=fz
      if (ifl(23).eq.0) goto 4001
      dbuf(4)=vnx
      dbuf(5)=vny
      dbuf(6)=vnz

4001  if(kcl.eq.0)goto 401
c%%%          ifl(144)=1 means nccs output (set/mode,circul,1) 28-feb-84
      if(ifl(144).eq.0)goto 4009
c              add dbuf to tdat
      tdat(ntd+1)=dbuf(1)
      tdat(ntd+2)=dbuf(2)
      tdat(ntd+3)=dbuf(3)
      np=np+1
      ntd=ntd+3
c              if multax, add i,j,k
cc      if(ifl(82).eq.0 .and. .not. lexpcl)goto 4004
      if(ifl(82).eq.0 .and. lv92)goto 4004

      if(ifl(23).eq.1)goto 4002

      tdat(ntd+1)=sc(4)
      tdat(ntd+2)=sc(5)
      tdat(ntd+3)=sc(6)
      goto 4003

4002  tdat(ntd+1)=dbuf(4)
      tdat(ntd+2)=dbuf(5)
      tdat(ntd+3)=dbuf(6)

4003  ntd=ntd+3
cc      if (lexpcl) then
      if (.not. lv92) then
        tdat(ntd+1) = fx
        tdat(ntd+2) = fy
        tdat(ntd+3) = fz
        tdat(ntd+4) = 0.0d0
        tdat(ntd+5) = 0.0d0
        tdat(ntd+6) = 0.0d0
        tdat(ntd+7) = 0.0d0
        tdat(ntd+8) = 0.0d0
        tdat(ntd+9) = 0.0d0
        tdat(ntd+10) = 0.0d0
        tdat(ntd+11) = 0.0d0
        tdat(ntd+12) = 0.0d0
        tdat(ntd+13) = 0.0d0
        tdat(ntd+14) = 0.0d0
        tdat(ntd+15) = 0.0d0
        ntd = ntd+15
      endif
c               if tdat full, dump it now  (otherwise at pass end)
4004  if(ntd.lt.max)goto 401
      call putcl(iclass,5,np,tdat)
      np=0
      ntd=0
      goto 401
c%%%
c     output as indicated.
4009  call putcl(5000,5,1,dbuf)
c                re-set dbuf to x,y,z    (may have chgd in putcl)
      dbuf(1)=x
      dbuf(2)=y
      dbuf(3)=z
      if(ifl(82).eq.0)goto 401
      dbuf(4)=sc(4)
      dbuf(5)=sc(5)
      dbuf(6)=sc(6)
401   continue
cc      if(kpl.eq.1) call plotm(dbuf,.false.)
      if (motdfl) then
cuni
         if (first) then
c
c...Added check for NCL-VT mode
c...Paul  -  10/2/91
c
           if ((ifl(139).gt.24 .or. ifl(139).eq.0) .and.
     x        (ifl(35).eq.0 .or. ifl(35) .eq. 2 .or. (ifl(35).eq.1 .and.
     2         first))) then
c
c...Added check for NCL-VT mode
c...Paul  -  10/24/91
c
              if (ifl(35) .eq. 2) call ersw3(15,1)

              if (ifl(82).eq.0) then
                write (cout,403)
                call putmsg (cout,37,15,0)
              else
                write (cout,404)
                call putmsg (cout,73,15,0)
              endif
403           format ('  step',6x,'x',11x,'y',11x,'z')
404           format ('  step',6x,'x',11x,'y',11x,'z',11x,'i',11x,
     -                  'j',11x,'k')
              ifl(139)=16
c
c...Added check for NCL-VT mode
c...Paul  -  10/2/91
c
              if (ifl(35).eq.0 .or. ifl(35) .eq. 2) call ersw3 (16,1)
           endif
          endif
          if (ifl(154).eq.1 .or. first) then
c                print large
              if (ifl(139).gt.24) then
                ifl(139)=16
c
c...Added check for NCL-VT mode
c...Paul  -  10/2/91
c
                if (ifl(35).eq.0 .or. ifl(35) .eq. 2) call ersw3 (16,1)
              endif
              stepno=stepno+1
              do 405 i=1,6
405           hbuf(i)=dbuf(i)
              if (ifl(73).eq.1.and.ifl(267).eq.1) then
                call conent(hbuf,sc(41),3)
                if (ifl(82).eq.1) call conent(hbuf(4),sc(41),4)
              endif
              if (ifl(82).eq.0) then
                write (cout,1010) stepno,(hbuf(i),i=1,3)
                call putmsg (cout,43,ifl(139),0)
              else
                write (cout,1010) stepno,(hbuf(i),i=1,6)
                call putmsg (cout,79,ifl(139),0)
              endif
1010          format (i4,3f12.4,3f12.6)
              ifl(139)=ifl(139)+1
              first=.false.
          endif
      endif
c RAH: added call to set intr flag - effective on SGI ONLY
      call ckintr(ifl(86),ifl(35))
      if (ifl(86).ne.0) then
          ifl(2)=149
          go to 999
      endif
c
c  **********  go back for next pt do (unless this was midpt)
c
      if(mid.eq.0)goto 410
      mid=0
      x=hx
      y=hy
      z=hz
      vnx=oa
      vny=ob
      vnz=oc
      goto 400
410   if(ihold.eq.0)goto 420
      ihold=0
      x=hx
      y=hy
      z=hz
      oa=vnx
      ob=vny
      oc=vnz
      vnx=ha
      vny=hb
      vnz=hc
      if(jpnd.eq.0)goto 385
      goto 400
c              sto this sfnrm
420   oa=vnx
      ob=vny
      oc=vnz
      jpst=0
      goto 21
c
c     temp error exit
998   if(ifl(2).lt.1)ifl(2)=5
c               normal exit
999   continue

c          set window three line count to zero so next scrub will start
c          at top of window
      ifl(139)=0
      return
      end
