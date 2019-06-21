
C*********************************************************************
C*    NAME         :  ppwrt.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ppwrt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:27
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ppwrt (itsk)
C*       description
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine ppwrt (itsk)
c
      include 'com8a.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)

      real*8 e,f,a,b,asn
      real*4 ad(300)
      integer*2 ksc(500),kd(600),ksn(4)
      equivalence (sc,ksc),(d,ad,kd),(asn,ksn)
c
      equivalence  (idfr,kd(1)),(nds,kd(3)), (idx,kd(5)),  (icx,kd(6)),
     1            (ibx,kd(7)),(iax,kd(8)), (idpp,kd(9)), (icpp,kd(10)),
     2             (ibpp,kd(11)),(iapp,kd(12))
c
c                       bra on itsk
      jtsk=itsk+1
c
c      goto(10,20,30,40,50,60),jtsk
      if (jtsk .eq. 1) goto 10
	  if (jtsk .eq. 2) goto 20
	  if (jtsk .eq. 3) goto 30
	  if (jtsk .eq. 4) goto 40
	  if (jtsk .eq. 5) goto 50
	  if (jtsk .eq. 6) goto 60
c
c                  pp commands before 'depth'
10    ist=6
      ind=idx-2
      goto 70
c                  depth pp's
20    ist=idx+1
      ind=icx-2
      goto 70
c                  cutang pp's
30    ist=icx+1
      ind=ibx-2
      goto 70
c                  retrct pp's
40    ist=ibx+2
      ind=iax-2
      goto 70
c                  return pp's
50    ist=iax+2
      ind=nds-1
      goto 70
c
c *****************  itsk=5    issue fedrat/rapid and exit
60    asn=0.
      call putcl(2000,5,1,asn)
      goto 99
c                  find and o/p 2000 recs between ist and ind
70    i=ist
72    asn=d(i)
      if(ksn(1).ne.2000)goto 80
      call putcl(2000,ksn(2),ksn(3),d(i+1))
      i=i+ksn(3)-1
80    i=i+1
      if(i.le.ind)goto 72
c
99    return
      end
