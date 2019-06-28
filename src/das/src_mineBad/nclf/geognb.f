C*********************************************************************
C*    NAME         :  geognb.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       geognb.f , 25.2
C*    DATE AND TIME OF LAST MODIFICATION
C*       08/31/15 , 08:27:14
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine geognb
C*       prepare and store matrix definitions
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
      subroutine geognb

      include 'com8a.com'

      integer*2 isc(100),ktv(4),nw,pg,el,peq(4),ksn(4),mtyp
      real*8 w(16),a(35),pgelnw,u(12),t(12),ru(12)
      real*8 gts1(6),gts2(6),gts3(6)
      equivalence (a,jb),(sc(10),isc),(tv,ktv),(peq(1),pgelnw)
      equivalence (pg,peq(1)),(el,peq(2)),(nw,peq(3))
      equivalence (ru(4),u(1)),(asn,ksn),(mtyp,peq(4))
      logical nored
      integer*4 nclkey,ierr
      integer*2 ietype
      logical trflg

      trflg = .true.
      radian=57.2957795d0
      nored=.false.
      ktv(4)=idst
      isub=isc(2)
c
c...Set default disply parameters for MX
c
      w(13) = 1.0
      if (ifl(264) .eq. 1) w(13) = 25.d0
      w(14) = .75 * w(13)
      w(15) = .75 * w(13)
      w(16) = .75 * w(13)

c          must be matrix              3-8-82
      if(idst.ne.10)goto 990
c************************************************** matrix *****
500   ktv(3)=12
      if(isub-2)501,520,529
c     ************************** mx/ (12 params)
501   do 502 i=1,12
502   w(i)=sc(10+i)
c          do a rough check on input matrix and issue
c          warning if not 1/1 orthogonal

c          skip doing check for nclcadd version                  M. Gump 3-13-87
          
c      if(dabs(w(1)**2+w(2)**2+w(3)**2-1.).gt..01)goto 509
c      if(dabs(w(5)**2+w(6)**2+w(7)**2-1.).gt..01)goto 509
c      if(dabs(w(9)**2+w(10)**2+w(11)**2-1.).gt..01)goto 509
c      if(dabs(w(1)*w(5)+w(2)*w(6)+w(3)*w(7)).gt..01)goto 509
c      if(dabs(w(1)*w(9)+w(2)*w(10)+w(3)*w(11)).gt..01)goto 509
c      goto 900
c509   err=.true.
c      ifl(2)=-52
      goto 900
c     ************************** mx/invers,mx1
520   pgelnw=sc(11)
c          get mx1 into u and build invers in w
cuni      call getent(u,nw,pg,el,10)
      call gtentt(pgelnw, trflg, nclkey, ietype, u(1))
      sfx=u(1)**2+u(5)**2+u(9)**2
      sfy=u(2)**2+u(6)**2+u(10)**2
      sfz=u(3)**2+u(7)**2+u(11)**2
      sm=1.d-4
      if(sfx.gt.sm.and.sfy.gt.sm.and.sfz.gt.sm)goto 522
c          error. mx does not invert
      ifl(2)=121
      goto 990
c          do rotation params
522   do 524 i=1,3
      j=4*i-3
      w(i)=u(j)/sfx
      w(i+4)=u(j+1)/sfy
524   w(i+8)=u(j+2)/sfz
c          origin tranlation in w4,8,12
      do 526 j=4,12,4
      i=j-3
526   w(j)=-w(i)*u(4)-w(i+1)*u(8)-w(i+2)*u(12)
      goto 900
529   if(isub-4)530,540,549
c     ************************** mx/mx1,mx2  (mx1 in u)*(mx2 in t)
530   pgelnw=sc(11)
cuni      call getent(u,nw,pg,el,10)
      call gtentt(pgelnw, trflg, nclkey, ietype, u(1))
      pgelnw=sc(12)
cuni      call getent(t,nw,pg,el,10)
      call gtentt(pgelnw, trflg, nclkey, ietype, t(1))
      i=0
      do 534 j=1,9,4
      do 532 k=1,4
      i=i+1
532   w(i)=u(j)*t(k)+u(j+1)*t(k+4)+u(j+2)*t(k+8)
534   continue
c          fix origin
      w(4)=w(4)+u(4)
      w(8)=w(8)+u(8)
      w(12)=w(12)+u(12)
      goto 900
c
c...vp 9-apr-93 pointvector added as ve1 and/or ve2
c     ************************** mx/pt1,ve1,ve2
c...get ve1 in u(1), ve2 in u(4)
c
540   pgelnw=sc(12)
      ix    = 4
      if (mtyp .eq. 21) ix = 1
      call gtentt(pgelnw, trflg, nclkey, ietype, t(ix))
c
      pgelnw=sc(13)
      ix    = 4
      if (mtyp .eq. 21) ix = 1
      call gtentt(pgelnw, trflg, nclkey, ietype, u(ix))
      u(1) = t(4)
      u(2) = t(5)
      u(3) = t(6)
  541 u(7)=u(2)*u(6)-u(3)*u(5)
      u(8)=u(3)*u(4)-u(1)*u(6)
      u(9)=u(1)*u(5)-u(2)*u(4)
      u(4)=u(3)*u(8)-u(2)*u(9)
      u(5)=u(1)*u(9)-u(3)*u(7)
      u(6)=u(2)*u(7)-u(1)*u(8)
c          unitize and sto in w
      k=0
      do 544 i=1,7,3
          k=k+1
          sec=dsqrt(u(i)**2+u(i+1)**2+u(i+2)**2)
          if(sec.gt.1.d-4)goto 543
          ifl(2)=121
          goto 990
543       w(k)=u(i)/sec
          w(k+4)=u(i+1)/sec
          w(k+8)=u(i+2)/sec
544   continue
      if (isub .gt. 9) go to 900
c
c...Get pt and put in w(4,8,12)
c
      pgelnw=sc(11)
cuni      call getent(u,nw,pg,el,3)
      call gtentt(pgelnw, trflg, nclkey, ietype, u(1))
      w(4)=u(1)
      w(8)=u(2)
      w(12)=u(3)
      goto 900
c          continue branching on subtype
549   if(isub-6)550,560,569
c**********************************  mx/mirror,xyplan,yzplan, etc.
c                      (not yet)
550   goto 990
c**********************************  mx/mirror,pl1
560   asn=sc(11)
c          get pl1 in u, build mx in w
      call gtplt(asn, ifl(72), u(1))
      w(1)=1.-2.*u(1)**2
      w(6)=1.-2.*u(2)**2
      w(11)=1.-2.*u(3)**2
      w(2)=-2.*u(1)*u(2)
      w(3)=-2.*u(1)*u(3)
      w(7)=-2.*u(2)*u(3)
      w(5)=w(2)
      w(9)=w(3)
      w(10)=w(7)
c          now origin
      w(4)=2.*u(1)*u(4)
      w(8)=2.*u(2)*u(4)
      w(12)=2.*u(3)*u(4)
      goto 900
c          continue mx isub branching
569   if(isub-8)570,580,590
c***************************  mx/scale,a
c          zero w(2-12), add scalar(a) to major diagonal
570   do 572 i=2,12
572   w(i)=0.
      w(1)=sc(11)
      w(6)=sc(12)
      w(11)=sc(13)
      if (sc(14) .ne. 0.) then
          pgelnw=sc(14)
          call gtentt(pgelnw, trflg, nclkey, ietype, t(1))
          w(4) = t(1) - w(1)*t(1)
          w(8) = t(2) - w(6)*t(2)
          w(12) = t(3) - w(11)*t(3)
      endif
          
      goto 900
c      ***********************   mx/--rot,alf
580   alf=sc(12)/radian
      co=dcos(alf)
      si=dsin(alf)
c          build unit mx in w(1-12)
      do 582 i=2,12
582   w(i)=0.
      w(1)=1.
      w(6)=1.
      w(11)=1.
c          bra on xyrot, yzrot, zxrot
      if(sc(11).ne.733.) goto 584
c             *********************   xyrot
      w(1)=co
      w(6)=co
      w(2)=-si
      w(5)=si
      goto 900
584   if(sc(11).ne.734.)goto 586
c             ************************ yzrot
      w(6)=co
      w(11)=co
      w(7)=-si
      w(10)=si
      goto 900
586   if(sc(11).ne.735.)goto 990
c             ************************ zxrot
      w(1)=co
      w(11)=co
      w(3)=si
      w(9)=-si
      go to 900
590   if (isub-10)591,600,600
c            ************************* transl
591   w(1)=1
      w(2)=0
      w(3)=0
      w(4)=sc(11)
      w(5)=0
      w(6)=1
      w(7)=0
      w(8)=sc(12)
      w(9)=0
      w(10)=0
      w(11)=1
      if (isc(3).eq.3) then
         w(12)=sc(13)
      else
         w(12)=0
      endif
      go to 900
c
c...pv,(ve,pv)  vp 9-apr-93 PV support added (isub = 10,11)
c
  600 if (isub .eq. 12) go to 700
c
c...mx/pt1,pt2,pt3,pt1a,pt2a,pt3a
c
      if (isub .eq. 13) go to 800
      call gtentt(sc(11), trflg, nclkey, ietype, ru(1))
      if (isub .eq. 10) then
          call gtentt(sc(12), trflg, nclkey, ietype, u(4))
      else
          call gtentt(sc(12), trflg, nclkey, ietype, t(1))
          u(4) = t(4)
          u(5) = t(5)
          u(6) = t(6)
      end if
      w(4) = ru(1)
      w(8) = ru(2)
      w(12) = ru(3)
      go to 541
c
c...mx/mx1.   Paul. 10/11/93.
c
  700 call gtentt(sc(11), trflg, nclkey, ietype, w(1)) 
      go to 900
  800 pgelnw=sc(11)
      ix = 1
      call gtentt(pgelnw, trflg, nclkey, ietype, gts1(ix))
      pgelnw=sc(12)
      call gtentt(pgelnw, trflg, nclkey, ietype, gts2(ix))
      pgelnw=sc(13)
      call gtentt(pgelnw, trflg, nclkey, ietype, gts3(ix))
      ix = 4
      pgelnw=sc(14)
      call gtentt(pgelnw, trflg, nclkey, ietype, gts1(ix))
      pgelnw=sc(15)
      call gtentt(pgelnw, trflg, nclkey, ietype, gts2(ix))
      pgelnw=sc(16)
      call gtentt(pgelnw, trflg, nclkey, ietype, gts3(ix))
      
      call makmat(gts1,gts2,gts3,w,ierr)
      
      go to 900
c
c...geo store this item
c
900   nw=ktv(3)
cuni      call putent (w,nw,pg,el,nored,ktv(4))
c
cuni     call ptgeom(ktv(4), w, nclkey)
cuni      call ptdesc(nclkey, ietype, tv)
c
c          update wdknt and put ipg/iel in tv for vstore
c
      ietype = ktv(4)
      call ptentt(ietype, w, nclkey, tv)
      rest = tv

c     call blkgeo (nclkey,2)

      goto 999
c          error exit. set err and zero tv & 'rest'
990   err=.true.
      if(ifl(2).eq.0)ifl(2)=5
      tv=0.
      rest=0.

999   return
      end
      
c
c***********************************************************************
c
c   SUBROUTINE:  makmat (gpt1,gpt2,gpt3,gmat,kerr)
c
c   FUNCTION:  This routine creates conversion matrix using 3 pairs of
c              points where the first triplets defines an input coord-
c              system and the second one defines the output coord-sys.
c
c   INPUT:  gpt1    R*8  D6  Reference point 1; 1-3 original coordintaes
c                            4-6 new coordinates
c
c           gpt2    R*8  D6  Reference point 2;         --"--
c
c           gpt3    R*8  D6  Reference point 3;         --"--
c
c   OUTPUT: gmat    R*8  D12 Conversion Matrix array.
c
c           kerr    I*4  D1  Returns 1 when points are too close or
c                            on a single line.
c
c***********************************************************************
c
      subroutine makmat (gpt1,gpt2,gpt3,gmat,kerr)
c
      integer*4 kerr
      real*8 gpt1(6),gpt2(6),gpt3(6),gmat(12)
c
      integer*4 i,i2,i3
c
      real*8 ve1(3),ve2(3),dl1,dl2,org(3),vc1(3),vc2(3),
     -       ve3(3),vz1(3),tran(3),scal,tmp(3),mat3(4,3),
     -       mat1(4,3),mat2(4,3),a,a1,a2,b,b1,b2,ml(12),
     -       ve4(3),vz2(3),vs1(3),vs2(3),dl3,dl4,c,c1,c2,f_dot,f_dist
c
      equivalence (ml,mat1)
c
c...Set initial values
c
      kerr   = 06
      i2     = 2
      i3     = 3
c
c...Get direction of old & new system
c
      do 115 i=1,3,1
          ve1(i) = gpt2(i) - gpt1(i)
          ve2(i) = gpt2(i+3) - gpt1(i+3)
          ve3(i) = gpt3(i) - gpt1(i)
          ve4(i) = gpt3(i+3) - gpt1(i+3)
          tran(i) = 0.d0
  115 continue
c 
      dl1 =  f_dist (gpt2,gpt1)
      dl2 =  f_dist (gpt2(4),gpt1(4))
      dl3 =  f_dist (gpt3,gpt1)
      dl4 =  f_dist (gpt3(4),gpt1(4))  
c
c...Check if points are different,
c...get scale factor
c
      if (dl1 .lt. .001 .or. dl2 .lt. .001) go to 9000
      if (dl3 .lt. .001 .or. dl4 .lt. .001) go to 9000
      scal   = dl2 / dl1
c
      do 215 i=1,3,1
          ve1(i) = ve1(i) / dl1
          ve2(i) = ve2(i) / dl2
          ve3(i) = ve3(i) / dl3
          ve4(i) = ve4(i) / dl4
  215 continue
c
c...Get (Z) vector of old & new planes
c
      vz1(1) = ve1(2)*ve3(3) - ve1(3)*ve3(2)
      vz1(2) = ve1(3)*ve3(1) - ve1(1)*ve3(3)
      vz1(3) = ve1(1)*ve3(2) - ve1(2)*ve3(1)
      a1     = dsqrt(f_dot(vz1,vz1))
      if (a1 .lt. .001) go to 9000
      vz2(1) = ve2(2)*ve4(3) - ve2(3)*ve4(2)
      vz2(2) = ve2(3)*ve4(1) - ve2(1)*ve4(3)
      vz2(3) = ve2(1)*ve4(2) - ve2(2)*ve4(1)
      a2     = dsqrt(f_dot(vz2,vz2))
      if (a2 .lt. .001) go to 9000
      do 225 i=1,3
         vz1(i) = vz1(i) / a1
         vz2(i) = vz2(i) / a2
  225 continue
c
c...Get rotation of Z axis
c
      call vecang (ve1,i3,a1)
      call vecang (ve2,i3,a2)
      a      =  a1
      a1     = 0. - a1
      a2     = 0. - a2      
c
c...Rotate vectors to ZX plane
c
      call vecadj (ve1,vc1,a1,i3)
      call vecadj (ve2,vc2,a2,i3)
      call vecadj (vz1,vs1,a1,i3)
      call vecadj (vz2,vs2,a2,i3)
c
c...Get rotation of Y axis
c
      call vecang (vc1,i2,b1)
      call vecang (vc2,i2,b2)
      b      = b1
      b1     = 0. - b1
      b2     = 0. - b2
c
      call vecadj (vs1,vz1,b1,i2)
      call vecadj (vs2,vz2,b2,i2)
c
c...Get the total rotation of plane vector
c
      call vecang (vz1,i3,c1)
      call vecang (vz2,i3,c2)
      c      = c1 - c2
c
c...Get translation matrix for rotation
c...about Z and Y axes from original position
c
      call gtmatr (i3,a,tran,mat1)
      call gtmatr (i2,b,tran,mat2)
      call matmul (mat1,mat2,mat3)
c
c...Get translation matrix for rotation
c...about Z from ZX plan to final position
c
      call gtmatr (i3,c,tran,mat1)
      call matmul (mat3,mat1,mat2)
      call gtmatr (i2,b2,tran,mat1)
      call matmul (mat2,mat1,mat3)
      call gtmatr (i3,a2,tran,mat2)
      call matmul (mat3,mat2,mat1)
c
      do 325 i=1,12
          gmat(i)=ml(i) * scal
  325 continue
c
c...Translate origin and add to output matrix
c
      call matpta (gpt1,tmp,gmat,i2)
      tmp(1) = gpt1(4) - tmp(1)
      tmp(2) = gpt1(5) - tmp(2)
      tmp(3) = gpt1(6) - tmp(3)
      call ptmatb (tmp,org,mat1,i2)
c
      gmat(4) = 0. - org(1)
      gmat(8) = 0. - org(2)
      gmat(12) = 0. - org(3)   
c
c...End of routine
c
 8000 return
c
c...Error in definition
c
 9000 kerr  = 1
      go to 8000
      end

      

