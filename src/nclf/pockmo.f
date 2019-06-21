
C*********************************************************************
C*    NAME         :  pockmo.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       pockmo.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:26
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pockmo
c*          pocket collapsed ring data is in bu-tbl.
c*          this routine generates motion per same.
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
      subroutine pockmo
c                                                           23-jan-84
      include 'com4a.com'
      include 'mocom.com'

      common/pokom/tdat,bu,x(20),y(20),a(20),b(20),c(20)
     1,hx(20),hy(20),rgt,amt,finamt,dist,salmin,psk,dnat
     2,npts,inpts,npold,ibux,stepno, first
      real*8 asn,tdat(3),bj(35)
      real*4 ad(300),bu(900)
      integer*2 ksn(4),kd(600),stepno,ksc(500)
      logical first
      integer*2 ipu
      character*2 cpu
      equivalence(asn,bsn,ksn),(jb,bj),(d,ad,kd),(sc,ksc),(ipu,cpu)

c
c             far-out old loc for pokout mem
      ad(291)=987.
c             issue plunge fedrat if given.
      first=.true.
      cpu = 'pu'
      stepno=0
      if(sc(14).gt.0.)call fedmut (sc(14))
c
      itim=0
      ibx=ibux
      bsn=bu(ibux)
      np=ksn(2)
c             point to pt1 last ring
      ib1x=ibux-2*np
c             if 2 central pts, goto pt2 first.
c               if np=0, skip this step             30-may-85
      if(np.lt.1)goto 15
      if(np.ne.2)goto 10
      tdat(1)=bu(ibux-2)
      tdat(2)=bu(ibux-1)
      call motout
c             goto pt1
10    tdat(1)=bu(ib1x)
      tdat(2)=bu(ib1x+1)
      call motout
c                issue genl pocket fedrat, if any
15    if(sc(15).gt.0.)call fedmut (sc(15))
c                if more than 2 pts ring1, goto those pts
      if(np.gt.2)goto 22
c                bump up to next ring
20    ibx=ibx-1-2*np
      if(ibx.lt.2)goto 40
      bsn=bu(ibx)
      np=ksn(2)
c                goto these pts
22    j1x=ibx-2*np
      j=j1x
      do 25 i=1,np
      tdat(1)=bu(j)
      tdat(2)=bu(j+1)
      call motout
      j=j+2
      if(ifl(2).gt.0)goto 99
25    continue
c                back to pt1
      tdat(1)=bu(j1x)
      tdat(2)=bu(j1x+1)
      call motout
      goto 20

c              if finish amt real, do finish pass
40    if(finamt.lt..001) goto 90
c              issue finish fedrat, if any.
      if(sc(16).gt.0.)call fedmut (sc(16))
      do 50 i=1,inpts
      tdat(1)=hx(i)
      tdat(2)=hy(i)
50    call motout
c               back to pt1
      tdat(1)=hx(1)
      tdat(2)=hy(1)
      call motout
c                if no error, record last loc in sc(1,2,3)
90    if(ifl(2).gt.0)goto 99
      if(kd(280).gt.0)goto 95
      sc(1)=ad(291)
      sc(2)=ad(292)
      sc(3)=ad(293)
      goto 98
c                tipped case.  trf te pt and add to sc
95    sc(1)=tdat(1)*d(71)+tdat(2)*d(74)+tdat(3)*d(77)
      sc(2)=tdat(1)*d(72)+tdat(2)*d(75)+tdat(3)*d(78)
      sc(3)=tdat(1)*d(73)+tdat(2)*d(76)+tdat(3)*d(79)
c                print last pt if print/small 29-jan-85
98    if (ifl(154).eq.0.and.motdfl) then
        istprt = stepno + 1
        write(cout,1010) istprt,(sc(i),i=1,3)
1010    format (2x,i4,3x,3(f10.4,2x))
        call putmsg (cout,45,ifl(139),0)
      endif
c                reset line disply number at exit
99    ifl(139)=0
      return
      end
