C*********************************************************************
C*    NAME         :  chkent.f
C*       CONTAINS:
C*                 chkent
C*                 chkpat
C*    COPYRIGHT 1999 (c) NCCS.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       chkent.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:39
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine chkent(enti,entj,itwist)
c*       Check if a patch is twisted.
C*       Return itwist=1 if so.
C*    PARAMETERS   
C*       INPUT  : 
C*          enti                 - first curve 
C*          entj                 - second curve 
C*       OUTPUT :  
C*          itwist               - =1, if twisting has been detected
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine chkent(enti,entj,itwist)

      include 'com8a.com'

      real*8 enti, entj
      integer*2 itwist

      integer*2 maxpt, maxwd, maxptx
      parameter (maxpt = 50)
      parameter (maxptx = 1000)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2)) ! thus maxwd = 325

      common/wblok/w(maxwd*4+80)
      common/blok/x(16),y(16),z(16),co(16),u,v,xpt,ypt,zpt
     1,vx(2),vy(2),vz(2)

      real*8 w
      real*8 x,y,z

      integer*2 i,ietype, m,n, iwx,jwx, itx,jtx,itwis(2),itim
      integer*2 ksn(4),ient(4),lsc44,lsc48
      integer*4 nclkey
      logical trflg
      real*8 asn, tbuf(3)
      real*4 aw(8*(maxwd+20))
      real*4 s(maxpt),sa,sb

      equivalence(asn,ksn),(sc(53),ient),(aw,w)

      itwist=0

      call fill_array (w(1),maxwd*4+80,0.d0)

      n=1
      asn=enti
      lsc44=ksn(4)

      do 50 m=1,2
      iwx=((maxwd+20)*2)*(m-1) ! thus iwx=690(m-1)
      jwx=iwx*2
      itx=iwx+maxwd
      jtx=jwx+(maxwd*2)
      if(m.eq.2) then
         n=3
         asn=entj
         lsc48=ksn(4)
      endif
      ldw=iwx
c
c..... if line, load 1 loc down
c
      if(ksn(4).eq.5) ldw=iwx+1
      trflg = .true.
      call gtentt(asn, trflg, nclkey, ietype, w(ldw+1))
c
c..... if ncl curve check if trimmed
c
      if (ietype .eq. 8) then
         call ncl_get_tpar (nclkey,tbuf)
         if (tbuf(1) .ne. 0. .or. tbuf(2) .ne. 1.d0)
     -       call rdf_curve (nclkey,tbuf,ldw*2+1)
      endif

      ient(n)=8
      if(ksn(4).ne.8) goto 20
c**********  curve.  put nwds in aw(300) and no activ seg in aw(262)
      aw(jtx+40)=aw(jwx+1)
      aw(jwx+1)=0.
      w(itx+1)=0.
      goto 50

20    if(ksn(4).ne.7) goto 30
c**********  circle.  add 4 params for fwd sense and half-angle phi 
      i=iwx+1
      call cirbld(w(i))
      ient(n)=7
      goto 50

30    if(ksn(4).ne.5) goto 98
c**********  line.   create a 2-pt curve
      i=iwx
      j=jwx
      w(i+8)=w(i+2)+w(i+5)
      w(i+9)=w(i+3)+w(i+6)
      w(i+10)=w(i+4)+w(i+7)
      aw(j+21)=w(i+5)/3.
      aw(j+22)=w(i+6)/3.
      aw(j+23)=w(i+7)/3.
c
c..... also seg1
c
      aw(j+9)=aw(j+21)
      aw(j+10)=aw(j+22)
      aw(j+11)=aw(j+23)
      aw(j+12)=1.
      aw(j+13)=0.
      aw(j+14)=1.
      w(i+1)=0.
      w(itx+1)=0.
      w(itx+20)=0.
      aw(j+2)=1.
      aw(jtx+40)=2.

50    continue
c
c..... start at s=0, and gen s-list along cv1, cv2
c
      i=1
      s(1)=0.
60    sa=s(i)
c
c..... do cv1 and cv2 sb values.
c
      call sbsolv(sa,sb,1,0)
      sb1 = sb
      call sbsolv(sa,sb,3,0)
      sb3 = sb
      i = i+1
      if (i .gt. maxpt) goto 998
      s(i) = sb3
      if (sb1 .lt. sb3) s(i)=sb1

      if (s(i) .lt. .9999) goto 60
      ns = i
      npat = ns-1
c
c..... avoid very small last patch.
c
      if (npat.lt.2) goto 70
      rho = (1.-s(npat))/(1.-s(npat-1))
      if (rho .gt. .2) goto 70
      s(npat) = 1.-(1.-s(npat-1))*.2
70    continue
c
c..... Check patches for twisting. First check the endpoints.
c
      itwis(1) = 0   
      itwis(2) = 0      
      itim = 1

75    iwx=0
      call crvpnt(0.,iwx,0,0)
      x(1)=w(maxwd+12)
      y(1)=w(maxwd+13)
      z(1)=w(maxwd+14)
      call crvpnt(1.0,iwx,0,0)
      x(13)=w(maxwd+12)
      y(13)=w(maxwd+13)
      z(13)=w(maxwd+14)
      iwx=(maxwd+20)*2
      if (itim .eq. 1) then
        call crvpnt(0.,iwx,0,0)
      else
        call crvpnt(1.,iwx,0,0)
      endif
      x(4)=w(iwx+maxwd+12)
      y(4)=w(iwx+maxwd+13)
      z(4)=w(iwx+maxwd+14)
      if (itim .eq. 1) then
        call crvpnt(1.,iwx,0,0)
      else
        call crvpnt(0.,iwx,0,0)
      endif
      x(16)=w(iwx+maxwd+12)
      y(16)=w(iwx+maxwd+13)
      z(16)=w(iwx+maxwd+14)
      call chkpat(itwist)
      itwis(itim) = itwis(itim) + itwist

      do 80 i=1,npat
         ipat=i
         sa=s(i)
         sb=s(i+1)
c
c..... cv1, load pt1, pt13
c
         iwx=0
         call crvpnt(sa,iwx,0,0)
         x(1)=w(maxwd+12)
         y(1)=w(maxwd+13)
         z(1)=w(maxwd+14)
         call crvpnt(sb,iwx,0,0)
         x(13)=w(maxwd+12)
         y(13)=w(maxwd+13)
         z(13)=w(maxwd+14)
c
c..... repeat for cv2, pt4,pt16
c
         iwx=(maxwd+20)*2
         if (itim .eq. 1) then
           call crvpnt(sa,iwx,0,0)
         else
           call crvpnt(1.-sa,iwx,0,0)
         endif
         x(4)=w(iwx+maxwd+12)
         y(4)=w(iwx+maxwd+13)
         z(4)=w(iwx+maxwd+14)
         if (itim .eq. 1) then
           call crvpnt(sb,iwx,0,0)
         else
           call crvpnt(1.-sb,iwx,0,0)
         endif
         x(16)=w(iwx+maxwd+12)
         y(16)=w(iwx+maxwd+13)
         z(16)=w(iwx+maxwd+14)
c
c..... check whether the patch is twisted
c
         call chkpat(itwist)
         itwis(itim) = itwis(itim) + itwist
80    continue
      if (itim.eq.1) then
        if (itwis(1).eq.0 .or. itwis(1).gt.npat) goto 99
        itim = 2
        goto 75
      endif
      if (itwis(1).gt.itwis(2)) then
        itwist = 1
      else
        itwist = 0
      endif
      goto 99

c..... error exit
998   ifl(2) = 156
      goto 99
 
c..... error exit
98    ifl(2)=12
      err=.true.

99    return
      end


C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine chkpat(itwist)
c*       Check if a patch is twisted.
C*       Return itwist=1 if so.
C*    PARAMETERS   
C*       INPUT  : 
C*          none 
C*       OUTPUT :  
C*          itwist               - =1, if twisting has been detected
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine chkpat(itwist)

      include 'com8a.com'

      common/blok/x(16),y(16),z(16),co(16),u,v,xpt,ypt,zpt
     1,vx(2),vy(2),vz(2)

      real*8 x,y,z, cosab, cosq, sinab, c_a,c_b, da,db, d 
      real*8 cosab1, sinab1, c_a1,c_b1, da1,db1, d1 
      real*8 x_end(3),y_end(3),w_end(3),z_end(3)
      real*8 a(3),b(3),c(3), x_da(3),w_db(3)
      real*8 a1(3),b1(3), x_da1(3),w_db1(3)
      real*8 dxy,dwz,dxz,dwy,dxw,dyz, toler

      call xyzvc (x(1),y(1),z(1), x_end)
      call xyzvc (x(4),y(4),z(4), y_end)
      call xyzvc (x(13),y(13),z(13), w_end)
      call xyzvc (x(16),y(16),z(16), z_end)

      toler=sc(27)
      itwist=0

      dxw=f_dist(x_end,w_end)
      if (dxw .lt. toler) goto 99
      dyz=f_dist(y_end,z_end)
      if (dyz .lt. toler) goto 99
      dxy=f_dist(x_end,y_end)
      if (dxy .lt. toler) goto 99
      dwz=f_dist(w_end,z_end)
      if (dwz .lt. toler) goto 99

      dxz=f_dist(x_end,z_end)
      dwy=f_dist(w_end,y_end)
c
c..... Calculate d - the distance between segments [x,y] and [w,z]
c
      call vcmnvc(y_end,x_end,a)
      call unitvc(a,a)
      call vcmnvc(z_end,w_end,b)
      call unitvc(b,b)

      cosab=f_dot(a,b)
      sinab = 0.
      cosq = cosab*cosab
      if (cosq .lt. 1.) sinab=dsqrt(1-cosq)
      if (sinab .le. 1.d-5) goto 99

      call vcmnvc(w_end,x_end,c)
      c_a=f_dot(c,a)
      c_b=f_dot(c,b)

      da=(c_a-c_b*cosab)/(sinab**2)
      if (da .le. 0.0) da=0.0
      if (da .ge. dxy) da=dxy
      call uvcplvc(x_end, a, x_da, da)

      db=(c_a*cosab-c_b)/(sinab**2)
      if (db .le. 0.0) db=0.0
      if (db .ge. dwz) db=dwz
      call uvcplvc(w_end, b, w_db, db)

      d=f_dist(x_da,w_db)
      if (d.gt.dxw .or. d.gt.dyz) goto 99
c
c..... Calculate d1 - the distance between segments [x,z] and [w,y]
c
      call vcmnvc(z_end,x_end,a1)
      call unitvc(a1,a1)
      call vcmnvc(y_end,w_end,b1)
      call unitvc(b1,b1)

      cosab1=f_dot(a1,b1)
      sinab1 = 0.
      cosq = cosab1*cosab1
      if (cosq .lt. 1.) sinab1 = dsqrt(1-cosq)
      if (sinab1 .le. 1.d-5) then
          itwist=1
          goto 99
      endif

      c_a1=f_dot(c,a1)
      c_b1=f_dot(c,b1)

      da1=(c_a1-c_b1*cosab1)/(sinab1**2)
      if (da1 .le. 0.0) da1=0.0
      if (da1 .ge. dxz) da1=dxz
      call uvcplvc(x_end, a1, x_da1, da1)

      db1=(c_a1*cosab1-c_b1)/(sinab1**2)
      if (db1 .le. 0.0) db1=0.0
      if (db1 .ge. dwy) db1=dwy
      call uvcplvc(w_end, b1, w_db1, db1)

      d1=f_dist(x_da1,w_db1)
c
c..... If d is less than d1 we consider the patch twisted 
c
      if (d1 .gt. d) itwist=1

99    return
      end
