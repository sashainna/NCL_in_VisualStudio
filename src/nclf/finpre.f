C*********************************************************************
C*    NAME         :  finpre.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       finpre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:04
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine finpre
C*       called by lthctl to prepare for lth/fin execution.
C*
C*       get lathe/fin list into d-tbl, shape into f-tbl.
C*       prepare both for fincut
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
      subroutine finpre
c
      include 'com4a.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)

      real*8 asn,e,f,a,b
      real*4 ad(300),bsn(2)
      integer*2 ksc(500),kd(600),ksn(4),kf(4000)
      equivalence (sc,ksc),(d,ad,kd),(asn,bsn,ksn),(f,kf)
      integer*4 nclkey
      integer*2 nwds,ietype
c
c
      nds=ksc(39)
      if(nds.lt.71) goto 10
c              error.  input list is too long
      ifl(2)=15
      goto 99
c              get lathe/finish list into d(1) from next ranfil rec
10    irec=ifl(4)+1
      call getran(d,irec)
c              read again if nec.
      if(nds.lt.36) goto 12
      irec=irec+1
      call getran(d(36),irec)
12    continue
c
c             get shape into f-tbl
      asn=d(1)
c      call getent(f,ksn(3),ksn(1),ksn(2),0)
      call gtdesc(asn,nclkey,nwds,ietype)
      call gtshpt(f,nclkey)
c
c                do not allow pp-command at start of shape
      if(kf(32).ne.2000.and.kf(44).ne.2000) goto 20
      ifl(2)=252
      goto 99
c                same at end.  chk back from end for 1st valid item.
20    nf=kf(3)
      j=nf+1
      num=nf-10
      do 30 i=1,num
      j=j-1
      asn=f(j)
      k=ksn(4)
      if(k.eq.5.or.k.eq.7) goto 40
      if(k.ne.2000) goto 30
c                error. 1st found on back check is 2000
      ifl(2)=253
      goto 99
30    continue

40    continue
c             fix f-tbl via finfix
      call finfix
c
c             if 'invers' given, reverse the shape order
      do 50 i=6,nds
      asn=d(i)
      if(ksn(1).eq.822)goto 60
50    continue
c             invers not found. exit
      goto 99
60    call fininv
c
99    return
      end
