
C*********************************************************************
C*    NAME         :  ptonln.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ptonln.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:31
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptonln(lndesc,ptdesc,ptx,pty,ptz)
c*       the purpose of this subroutine is to solve a point
c*       or a projection of the point on a line.  epm  10-1-85
C*    PARAMETERS   
C*       INPUT  : 
c*         ptdesc  = the descriptor of the point
c*         lndesc  = the descriptor of the line
C*       OUTPUT :  
c*         ptx     = the return point x value
c*         pty     = the return point y value
c*         ptz     = the return point z value
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine ptonln(lndesc,ptdesc,ptx,pty,ptz)

      include 'com8a.com'

      real*8 ptdesc,lndesc,ptx,pty,ptz,lpt,lln
      integer*2 ipt(4),iln(4)
      equivalence (lpt,ipt),(lln,iln)
      real*8 cln(6)
      real*8 cpt(3)
      real*8 ro
      integer*2 ietype
      integer*4 nclkey
      logical trflg

c           move the arguments to local areas (isn't fortran great?)
      lln = lndesc
      lpt = ptdesc

c           get the canonical form of the input line and point
cuni      call getent (cln,6,iln(1),iln(2),iln(4))
cuni      call getent (cpt,3,ipt(1),ipt(2),ipt(4))
      trflg = .true.
      call gtentt(lln, trflg, nclkey, ietype, cln(1))
      call gtentt(lpt, trflg, nclkey, ietype, cpt(1))

c           compute the output point on the line
      ro= (cln(4)*(cpt(1)-cln(1))+cln(5)*(cpt(2)-cln(2))+
     1   cln(6)*(cpt(3)-cln(3)))/(cln(4)**2+cln(5)**2+cln(6)**2)
      ptx = cln(1)+cln(4)*ro
      pty = cln(2)+cln(5)*ro
      ptz = cln(3)+cln(6)*ro
99999 return
      end




