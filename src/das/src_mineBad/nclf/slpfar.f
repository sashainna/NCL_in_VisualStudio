C*********************************************************************
C*    NAME         :  slpfar.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       slpfar.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:43
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine slpfar(num,ivf,iret)
C*       Fills in missing slope vectors for SURF/cv1,0,cv2,cv3,cv4,0,...
C*       or SURF/FIT,cv1,cv2,... surface. 
C*       This routine receives a curve obtained by a cut
C*       through the boundary curves (the x,y,z arrays), plus the
C*       existent slope vectors (the a,b,c arrays). It calculates 
C*       a non-zero slope vector for each point on the curve that
C*       did not have one (those existent on input are not touched).
C*    PARAMETERS   
C*       INPUT  : 
C*          num     - the number of points on the cutting curve (i.e.,
C*                    the number of bounding curves)
C*          ivf     - the number of non-zero slope vectors at start. 
C*       OUTPUT :  
C*          iret     - a flag: iret>0 means an error
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none 
C*********************************************************************
C
       subroutine slpfar(num,ivf,iret)

       include 'com4a.com'

       integer*2 num,ivf,iret

       parameter (maxpt = 50)
       parameter (maxptx = 1000)
       parameter (maxpn = 20)
   
       common/pblok/x,y,z,a,b,c,dx,dy,dz,ch,q,inv

       real*8 x(maxptx),y(maxptx),z(maxptx)
       real*4 a(maxptx),b(maxptx),c(maxptx),dx(maxptx)
       real*4 dy(maxptx),dz(maxptx),ch(maxptx),q(maxpt*36)
       integer*2 inv(maxptx)
c
c..... The flag inv(i)=1 if the i-th slope is non-zero, 
c..... otherwise inv(i)=0.
c
       real*8 v(3)
       integer*2 i2v4 /4/

C
C...There should be an error if there is less than two entities.
C...JLS 2/26/99
C
       if (num.lt.2) then
          iret=1
          goto 99
       endif 

       iret=0

       ismall = 0
       if (num.eq.2 .or. (num.eq.3.and.ivf.eq.0)) ismall=1

       n=num
       m=n-1

       do 10 i=1,m
       dx(i)=x(i+1)-x(i)
       dy(i)=y(i+1)-y(i)
       dz(i)=z(i+1)-z(i)
       if (ifl(72).eq.1) then
         v(1) = dx(i)
         v(2) = dy(i)
         v(3) = dz(i)
         call conent (v, sc(68), i2v4)
         dx(i) = v(1)
         dy(i) = v(2)
         dz(i) = v(3)
       endif
       ch(i)=sqrt(dx(i)**2+dy(i)**2+dz(i)**2)
       if (ch(i).ge..001) goto 10
       iret=2
       goto 99
10     continue

       dx(n)=dx(m)
       dy(n)=dy(m)
       dz(n)=dz(m)
       ch(n)=ch(m)

       do 20 i=1,n
       if (ivf.gt.0 .and. inv(i).gt.0) goto 14
C
C...If n is equal to 2 nothing ever happens here, which results
C...in the vv array having all zeros and an error message called
C...in vcvpnt. I don't know why it was written that way but everything
C...seems to work much better if the condition is just limited to
C...IF (i.eq.1).  JLS 2/26/99
C
C       if (i.eq.1.or.i.eq.n) goto 20

       if (i.eq.1) goto 20
       ro=(ch(i-1)/ch(i))**2
       a(i)=dx(i-1)+ro*dx(i)
       b(i)=dy(i-1)+ro*dy(i)
       c(i)=dz(i-1)+ro*dz(i)
14     tl=sqrt(a(i)**2+b(i)**2+c(i)**2)
       if (ismall.eq.1) goto 15
       if (a(i)*dx(i)+b(i)*dy(i)+c(i)*dz(i).lt.0.)tl=-tl
15     if (tl.ne.0) goto 16
c
c..... Error if a calculated slope vector has zero length.
c
       iret=3
       goto 99

16     a(i)=a(i)/tl
       b(i)=b(i)/tl
       c(i)=c(i)/tl
20     continue

       iknt=0
       vchg=1.
       goto 42
30     vchg=0.

       do 40 j=2,m
       if (ivf.gt.0 .and. inv(j).gt.0) goto 40
       i=j-1
       k=j+1
       cal=(a(i)*dx(i)+b(i)*dy(i)+c(i)*dz(i))/ch(i)
       adis=.531*ch(i)/(.593+cal)
       al=ch(i)*(1.104+.13*cal)/(.851+cal)
       sa=ch(i)*(3.565+.24*cal)/(2.805+cal)
       dax=dx(i)-a(i)*adis
       day=dy(i)-b(i)*adis
       daz=dz(i)-c(i)*adis
       cbe=(dx(j)*a(k)+dy(j)*b(k)+dz(j)*c(k))/ch(j)
       bdis=.531*ch(j)/(.593+cbe)
       bl=ch(j)*(1.104+.13*cbe)/(.851+cbe)
       sb=ch(j)*(3.565+.24*cbe)/(2.805+cbe)
       ro=al*sa/(bl*sb)
       dbx=(dx(j)-bdis*a(k))*ro+dax
       dby=(dy(j)-bdis*b(k))*ro+day
       dbz=(dz(j)-bdis*c(k))*ro+daz
       tl=sqrt(dbx**2+dby**2+dbz**2)
       dbx=dbx/tl
       dby=dby/tl
       dbz=dbz/tl
       vdel=abs(dbx-a(j))+abs(dby-b(j))+abs(dbz-c(j))
       if (vdel.gt.vchg) vchg=vdel
       a(j)=dbx
       b(j)=dby
       c(j)=dbz
40     continue

42     if (ivf.gt.0 .and. inv(1).gt.0) goto 50
       co=(dx(1)*a(2)+dy(1)*b(2)+dz(1)*c(2))/ch(1)
       a(1)=2.*co*dx(1)/ch(1)-a(2)
       b(1)=2.*co*dy(1)/ch(1)-b(2)
       c(1)=2.*co*dz(1)/ch(1)-c(2)

50     if (ivf.gt.0 .and. inv(n).gt.0) goto 60
       co=(a(m)*dx(m)+b(m)*dy(m)+c(m)*dz(m))/ch(m)
       a(n)=2.*co*dx(m)/ch(m)-a(m)
       b(n)=2.*co*dy(m)/ch(m)-b(m)
       c(n)=2.*co*dz(m)/ch(m)-c(m)

60     if (ismall.eq.1) goto 99
c
c..... If calculated values stabilize, then finish iterating.
c..... If not, and the number of iterations is less than maxpt,
c..... do it again. If calculated slope vectors would not stabilize,
c..... set iret=4, and proceed to exit.
c
       if (vchg.lt.1.e-5) goto 70
       iknt=iknt+1
       if (iknt.lt.maxpt) goto 30
       iret=4

70     do 74 j=1,n
       i=j-1
       if (j.eq.1) goto 72
       if (a(j)*dx(i)+b(j)*dy(i)+c(j)*dz(i).lt.0.) then
          iret=5
          goto 99
       endif
72     if (a(j)*dx(j)+b(j)*dy(j)+c(j)*dz(j).lt.0.) then     
          iret=5
          goto 99
       endif
74     continue


99     return
       end

