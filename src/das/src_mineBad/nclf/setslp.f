C*********************************************************************
C*    NAME         :  setslp.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       setslp.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:40
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setslp(inv,num,ivf,iret)
c        note:  setslp is a direct copy of slpset.  name chg to avoid
c               mulitple reference in link.               5-nov-84
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
      subroutine setslp(inv,num,ivf,iret)

      include 'com4a.com'

      parameter (maxpt = 50)
      parameter (maxptx = 1000)

       common/pblok/x,y,z,a,b,c,dx,dy,dz,ch

       real*8 x(maxptx),y(maxptx),z(maxptx)
       real*4 a(maxptx),b(maxptx),c(maxptx),dx(maxptx)
       real*4 dy(maxptx),dz(maxptx),ch(maxptx)
       integer*2 inv (maxptx)

       ishow=iret
       iret=1
       if (num.lt.2) goto 99
       iret=0
       ismall = 0
       if (num-3)7,4,8
4      if (ivf)7,7,8
7      ismall=1
8      n=num
       m=n-1
       do 10 i=1,m
       dx(i)=x(i+1)-x(i)
       dy(i)=y(i+1)-y(i)
       dz(i)=z(i+1)-z(i)
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
       if (inv(i).gt.0)goto 14
       if (i.eq.1.or.i.eq.n) goto 20
       ro=(ch(i-1)/ch(i))**2
       a(i)=dx(i-1)+ro*dx(i)
       b(i)=dy(i-1)+ro*dy(i)
       c(i)=dz(i-1)+ro*dz(i)
14     tl=sqrt(a(i)**2+b(i)**2+c(i)**2)
       if (ismall.eq.1) goto 15
       if (a(i)*dx(i)+b(i)*dy(i)+c(i)*dz(i).lt.0.)tl=-tl
15     if (tl.ne.0) goto 16
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
       if (inv(j).gt.0) goto 40
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
       if (vdel.gt.vchg)vchg=vdel
       a(j)=dbx
       b(j)=dby
       c(j)=dbz
40     continue
42     if (inv(1).gt.0) goto 50
       co=(dx(1)*a(2)+dy(1)*b(2)+dz(1)*c(2))/ch(1)
       a(1)=2.*co*dx(1)/ch(1)-a(2)
       b(1)=2.*co*dy(1)/ch(1)-b(2)
       c(1)=2.*co*dz(1)/ch(1)-c(2)
50     if (inv(n).gt.0) goto 60
       co=(a(m)*dx(m)+b(m)*dy(m)+c(m)*dz(m))/ch(m)
       a(n)=2.*co*dx(m)/ch(m)-a(m)
       b(n)=2.*co*dy(m)/ch(m)-b(m)
       c(n)=2.*co*dz(m)/ch(m)-c(m)
60     continue
       if (ismall.gt.0) goto 99
       if (vchg.lt.1.e-5) go to 70
       iknt=iknt+1
       if (iknt.lt.maxpt) go to 30
       iret=4
70     do 74 j=1,n
       i=j-1
       if (j.eq.1) goto 72
       if (a(j)*dx(i)+b(j)*dy(i)+c(j)*dz(i).lt.0.) goto 90
72     if (a(j)*dx(j)+b(j)*dy(j)+c(j)*dz(j).lt.0.) goto 90
74     continue
       goto 99
90     iret=5
99     return
       end
