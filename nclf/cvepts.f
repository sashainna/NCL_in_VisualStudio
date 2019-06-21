C*********************************************************************
C*    NAME         :  cvepts.f
C*       CONTAINS:
C*    COPYRIGHT 1986 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cvepts.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:47
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cvepts(nclkey,spt,ept)
C*     Return start and end points and tangent vectors of NCL curve. 
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey - Unibase key of curve.
C*       OUTPUT :  
C*          spt    - Start point and associated tangent vector.
C*          ept    - End point and associated tangent vector.
C*    RETURNS      : none
C*    SIDE EFFECTS : Uses common area dspcom.
C*    WARNINGS     : none
C********************************************************************/

      subroutine cvepts (nclkey, spt, ept)


      include 'com4a.com'
      include 'dspcom.com'


      integer*4 nclkey
      real*8 spt(6), ept(6)

      call gtgeo(nclkey, w(1))

      nseg=aw(1)
		ix1=(nseg+1)/2
		jx1=ix1*2
		ix2=ix1+6*(nseg-1)
		jx2=iwx2*2

		do 100 i=1,3
		spt(i)=w(ix1+i)
		spt(i+3)=aw(jx1+i+6)
		ept(i)=w(ix2+i)
		ept(i+3)=aw(jx2+i+6)
100   continue

      return
      end
