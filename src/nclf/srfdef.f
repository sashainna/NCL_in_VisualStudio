C*********************************************************************
C*    NAME         :  srfdef.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       srfdef.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:44
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine srfdef
c*       set up and verify defining entities for a surface.
c*                                                                   
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
      subroutine srfdef

      include 'com8a.com'

      integer*2 maxpt,maxwd
      parameter (maxpt = 50)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))

      common/wblok/w(4*(maxwd+20))
      real*8 w 

      integer*2 ksn(4),ient(4),offset,xval,axval
      integer*4 nclkey
      integer*2 ietype,nw

      real*4 aw(8*(maxwd+20))
      real*8 asn 
      real*8 ptveln(6),hj(36),tbuf(3)
      logical noeras, trflg

      equivalence (asn,ksn),(w,aw),(sc(53),ient),(hj,jjb)
      equivalence (ksn4,ksn(4))

      noeras=.false.
      trflg=.true.
c
c..... w-array is the work area for 4 defining geometric entities
c
      do 700 i=1,4
         asn=sc(i+10)
c
c..... if ityp=22, this is a vecv - no action
c
         if(ksn4.eq.22) goto 700
c
c..... if ityp 23, this is a vvcv 
c
         if(ksn4.ne.23) goto 100
         iwx=(maxwd+20)*(i-1)
         jwx=2*iwx
         kwx=4*iwx
c
c..... get vvcv into jjb, then to w(iwx)
c
         call getran(jjb,ksn(1))
         do 40 n=1,18
40          w(iwx+n)=hj(n)
         goto 700
100      continue
         ipg=ksn(1)
         iel=ksn(2)
         ient(i)=8
         if (ksn4.eq.7) ient(i)=7
         indx=1+((maxwd+20)*(i-1))
         if (ranpts) then
             call getent (w(indx), ksn(3), ksn(1), ksn(2), ksn(4))
         else
             call gtentt(asn, trflg, nclkey, ietype, w(indx))
         endif
         if (ksn4.eq.7.or.ksn4.eq.8) goto 600
c
c..... vp 12-mar-93 PV support, only vector part of PV is used
c
         nw = ksn(3)
         if (ksn4 .eq. 21) then
            nw = 3
            w(indx) = w(indx+3)
            w(indx+1) = w(indx+4)
            w(indx+2) = w(indx+5)
         endif
c
c..... if it is a point/vector/line make it look like a curve
c
         do 150 k=1,nw
150          ptveln(k)=w(indx+k-1)
         if (ksn4.ne.3.and.ksn4.ne.4.and.ksn4.ne.21) goto 300
c
c..... check to be sure point or vector is not a boundary
c..... setting entity
c
         if (i.eq.2.or.i.eq.4) go to 250
            ifl(2)=144
            err=.true.
            go to 99999
c
c..... copy boundary curve and 
c..... offset points by point or vector values
c
250      nseg=aw((indx*2)-2)
         offset=(nseg+1)/2
c
c..... if prior entity circle, this will also be circle
c
         if(ient(i-1).ne.7) goto 258
         ient(i)=7
         n=indx-1
c
c..... move 15 params
c
         do 252 l=indx,indx+14
252         w(l)=w(l-(maxwd+20))
c
c..... this entity is a point or a vector
c
         if(ksn4.eq.4.or.ksn4.eq.21) goto 255
c
c..... *point*  corrections to w(1-15)
c
         roff=w(n+11)-w(n+8)*w(n+1)-w(n+9)*w(n+2)-w(n+10)*w(n+3)
         w(n+1)=(w(n+1)+ptveln(1))/2.
         w(n+2)=(w(n+2)+ptveln(2))/2.
         w(n+3)=(w(n+3)+ptveln(3))/2.
         w(n+7)=w(n+7)/2.
         w(n+11)=w(n+8)*w(n+1)+w(n+9)*w(n+2)+w(n+10)*w(n+3)+roff/2.
         goto 700
c
c..... *vector*  corrections
c
255      w(n+1)=w(n+1)+ptveln(1)
         w(n+2)=w(n+2)+ptveln(2)
         w(n+3)=w(n+3)+ptveln(3)
         w(n+11)=w(n+11)+w(n+8)*ptveln(1)+w(n+9)*ptveln(2)+
     1           w(n+10)*ptveln(3)
         goto 700
c
c..... move s values from curve data to dummy curve area
c
258      do 260 iv=indx,indx+offset
260         w(iv)=w(iv-(maxwd+20))
         aw((indx*2)-1)=nseg
         do 275 iv=0,nseg-1
            xval=indx+(iv*6)+offset
            if (ksn4.eq.4.or.ksn4.eq.21) go to 270
c
c..... for a point create a dummy curve halfway between the
c..... boundary curve and the point
c
            axval=xval*2-1
            w(xval)=(w(xval-(maxwd+20))+ptveln(1))/2.
            w(xval+1)=(w(xval-(maxwd+19))+ptveln(2))/2.
            w(xval+2)=(w(xval-(maxwd+18))+ptveln(3))/2.
            aw(axval+6)=aw(axval-(maxwd*2+34))/2.
            aw(axval+7)=aw(axval-(maxwd*2+33))/2.
            aw(axval+8)=aw(axval-(maxwd*2+32))/2.
            aw(axval+9)=aw(axval-(maxwd*2+31))
            go to 275
c
c..... for a vector create a dummy curve offset from the
c..... boundary curve by the vector distances
c
270         w(xval)=w(xval-(maxwd+20))+ptveln(1)
            w(xval+1)=w(xval-(maxwd+19))+ptveln(2)
            w(xval+2)=w(xval-(maxwd+18))+ptveln(3)
            w(xval+3)=w(xval-(maxwd+17))
            w(xval+4)=w(xval-(maxwd+16))
275         w(xval+5)=w(xval-(maxwd+15))
         go to 600
300      if (ksn4.ne.5) go to 400
c
c..... if it is a line create a 2 point dummy curve
c
         aw(indx*2-1)=2.
         aw(indx*2)=1.
         w(indx+1)=ptveln(1)
         w(indx+2)=ptveln(2)
         w(indx+3)=ptveln(3)
         aw((indx+4)*2-1)=ptveln(4)/3.
         aw((indx+4)*2)=ptveln(5)/3.
         aw((indx+5)*2-1)=ptveln(6)/3.
         aw((indx+5)*2)=1.
         aw((indx+6)*2-1)=0.
         aw((indx+6)*2)=1.
         w(indx+7)=ptveln(1)+ptveln(4)
         w(indx+8)=ptveln(2)+ptveln(5)
         w(indx+9)=ptveln(3)+ptveln(6)
         aw((indx+10)*2-1)=aw((indx+4)*2-1)
         aw((indx+10)*2)=aw((indx+4)*2)
         aw((indx+11)*2-1)=aw((indx+5)*2-1)
         go to 500
400      ifl(2)=130
         goto 99999
500      if (ifl(2).ne.0) goto 99999
600      w(indx+maxwd)=0.
         if(ient(i).eq.7) goto 650
c
c..... check if curve is trimmed
c
         k=2*(indx-1)
         if (ksn4.eq.CURVE .and. .not. ranpts) then
           call ncl_get_tpar (nclkey,tbuf)
           if (tbuf(1) .ne. 0. .or. tbuf(2) .ne. 1.d0)
     -       call rdf_curve (nclkey,tbuf,k+1)
         endif
 
         aw(k+(maxwd+20)*2)=aw(k+1)
         aw(k+1)=0.
         goto 700
c
c..... entity is a circle - finish it up   
c
650      continue
c
c..... fwd sense
c
         call cirbld(w(indx))
700   continue

      if (.not.geodsp) go to 99999
      call putmsg ('** curve load complete **',30,17,noeras)

99999 return
      end
