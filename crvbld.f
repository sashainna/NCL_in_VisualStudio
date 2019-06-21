C*********************************************************************
C*    NAME         :  crvbld.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       crvbld.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:44
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine crvbld(ivf)
C*       Build missing slope constraints for 
C*               SURF/cv1,slp1,ln2,0,ci3,slp3,...
C*       This routine is called by geogn2 if at least one slope
C*       constraint is zero.
C*       The flag array ktwist tells which entities should be reversed.
C*
C*    PARAMETERS   
C*       INPUT  : 
C*          ivf       - number of non-zero slope entities.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine crvbld(ivf)

      include 'com4a.com'

      integer*2 ivf

      integer*2 maxpt, maxptx, maxpn, maxwd
      parameter (maxpt = 50)
      parameter (maxptx = 1000)
      parameter (maxwd=(maxpt*6+(maxpt+1)/2))
      parameter (maxpn = 20)

      common/wblok/w(4*(maxwd+20))
      real*8 w
 
      common/pblok/x,y,z,a,b,c,dx,dy,dz,ch,q,inv
      real*8 x(maxptx),y(maxptx),z(maxptx)
      real*4 a(maxptx),b(maxptx),c(maxptx)
      real*4 dx(maxptx),dy(maxptx)
      real*4 dz(maxptx),ch(maxptx),q(maxpt*36)
      integer*2 inv(maxptx)
 
      common/vblok/vv(maxpn*12)
      real*4 vv

      real*8 hj
      common/hjblok/hj(2*maxpt)
      integer*2 ktwist
      common/twblok/ktwist(2*maxpt)

      real*8 asn,tbuf(3)
      real*4 tau
      real*4 aw(8*(maxwd+20)), qq(12)
      integer*2 iis(8*maxpt), ksn(4)
      integer*4 nclkey
      integer*2 ient,nenh,nent,nwds,ietype

      equivalence (w,aw)
      equivalence (asn,ksn),(hj,iis)
c
c..... The default value of ifl(346) is zero, it means "Reverse boundary
c..... and slope curves to avoid twisting". The value ifl(346)=1 means
c..... "Do not reverse curves".
c

      asn=sc(10)
      nent=ksn(3)
      nenh=nent/2
c
c..... build q-table, 4 points per each real entity. (points on
c..... a slope curve are delta xyz from its base curve points.)
c
      iwx=0
      do 90 i=1,nent
c
c..... if hj(i) zero, no action
c
         if(iis(i*4-3).eq.0.and.iis(i*4-2).eq.0) goto 90
c
c..... get entity into w - starting at column 1
c
         asn=hj(i)
         ient=ksn(4)
         sc(53)=asn
         call gtgeom(asn,w(1),nclkey,nwds,ietype)
c
c..... if pt or vec or pv, enter 4 times in qq-tbl
c
         if(ient.eq.3.or.ient.eq.4) then 
         do 43 j=1,3
             qq(j)=w(j)
             qq(j+3)=w(j)
             qq(j+6)=w(j)
             qq(j+9)=w(j)
43       continue
         goto 80
         endif
c
c..... point-vectors are treated as vectors
c
         if(ient.eq.21) then 
         do 45 j=1,3
             qq(j)=w(j+3)
             qq(j+3)=w(j+3)
             qq(j+6)=w(j+3)
             qq(j+9)=w(j+3)
45       continue
         goto 80
         endif
 
         if(ient.ne.5) goto 50
c
c..... entity is line. calc 4 pts.
c
         do 48 j=1,3
         if (ktwist(i).eq.0 .or. ifl(346).eq.1) then
            qq(j)=w(j)
            qq(j+3)=w(j)+w(j+3)/3.
            qq(j+6)=w(j)+w(j+3)*.666667
            qq(j+9)=w(j)+w(j+3)
         else
            qq(j+9)=w(j)
            qq(j+6)=w(j)+w(j+3)/3.
            qq(j+3)=w(j)+w(j+3)*.666667
            qq(j)=w(j)+w(j+3)
         endif
48       continue
              
         goto 80

50       if(ient.ne.7) goto 60
c
c..... circle.  complete it and solve 4 pts. fwd sense
c
         call cirbld(w)
         do 55 jj=0,3
         tau=jj
         tau=tau/3.0
         if (ktwist(i).eq.1 .and. ifl(346).eq.0) tau=1.0-tau
         call circpt(tau,0,0,0)
            qq(3*jj+1)=w(142)
            qq(3*jj+2)=w(143)
            qq(3*jj+3)=w(144)
55       continue
         goto 80
c
c..... must be curve
c
60       if(ient.eq.8) goto 62
         ifl(2)=12
         goto 999
c
c..... ready it, do 4 pts
c
62       call ncl_get_tpar (nclkey,tbuf)
         if (tbuf(1) .ne. 0. .or. tbuf(2) .ne. 1.d0)
     -       call rdf_curve (nclkey,tbuf,1) 
         w(maxwd+20)=0.
         aw((maxwd+20)*2)=aw(1)
         aw(1)=0.

         do 70 jj=0,3
         tau=jj
         tau=tau/3.0
         if (ktwist(i).eq.1 .and. ifl(346).eq.0) tau=1.0-tau
         call crvpnt(tau,0,0,0)
            qq(3*jj+1)=w(maxwd+12)
            qq(3*jj+2)=w(maxwd+13)
            qq(3*jj+3)=w(maxwd+14)
70       continue

c
c..... move qq(1-12) to q-tbl.
c
80       iqx=12*i-12
         do 82 j=1,12
             q(iqx+j)=qq(j)
82       continue 
c
c..... if slope entity and not a vector, convert to delta form
c
         if(ient.eq.4.or.ient.eq.21.or.i/2*2.ne.i) goto 90
         do 84 j=1,12
             k=iqx+j
             q(k)=q(k)-q(k-12)
84       continue 
90    continue
c
c..... take 4 u-cuts thru q data via slpfar
c
      do 110 i=1,4
c
c..... load xyzabc from q
c
          do 102 j=1,nenh
              k=3*i+24*j-26
              x(j)=q(k)
              y(j)=q(k+1)
              z(j)=q(k+2)
              a(j)=q(k+12)
              b(j)=q(k+13)
              c(j)=q(k+14)
102       continue
          call slpfar(nenh,ivf,iret)
c
c..... error in slpfar step
c
          if(iret.ge.1) then    
             ifl(2)=iret+46
             goto 999
          endif
c
c..... add any solved vecs to q
c
          do 104 j=1,nenh
              if(inv(j).eq.1) goto 104
              k=3*i+24*j-14
              q(k)=a(j)
              q(k+1)=b(j)
              q(k+2)=c(j)
104       continue
110   continue
c
c..... correct 2nd,3rd to polgon pts and add to vv-tbl
c
      ivvx=-12
      do 130 i=2,nent,2
         if(hj(i).ne.0.) goto 130
c
c..... sc is zero. set pointers to qtbl, vvtbl
c
         k=12*i-12
         ivvx=ivvx+12
c
c..... fix sc word now in hj
c
         asn=0.
         ksn(1)=ivvx
         ksn(4)=22
         hj(i)=asn
c
c..... move 12-set to a(temp), upgrade pts 2,3 to polgon state
c
         do 121 j=1,12
             a(j)=q(k+j)
121      continue
         do 122 j=1,3
             hvec=(-5.*a(j)+18.*a(j+3)-9.*a(j+6)+2.*a(j+9))/6.
             a(j+6)=(2.*a(j)-9.*a(j+3)+18.*a(j+6)-5.*a(j+9))/6.
             a(j+3)=hvec
122      continue
c
c..... add this 12-set to vv
c
         do 124 j=1,12
             vv(ivvx+j)=a(j)
124      continue
130   continue

999   return
      end
