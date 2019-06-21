
C*********************************************************************
C*    NAME         :  star.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       star.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:45
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine star(xnd,ynd)
c        called by dsplsh to mark start and end of shape disply
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
      subroutine star(xnd,ynd)

      include 'com8a.com'

      real*8 buf(3)

c
      scale=sc(106)
      if(scale.eq.0.)scale=1.
      db=.05/scale
      buf(3)=0.
      buf(1)=xnd+db
      buf(2)=ynd
c      call plotm(buf,.false.)
      call glina3(buf(1),buf(2),buf(3))
      buf(1)=xnd-db
      buf(2)=ynd
c      call plotm(buf,.false.)
      call glina3(buf(1),buf(2),buf(3))
      buf(1)=xnd
      buf(2)=ynd
c      call plotm (buf,.false.)
      call glina3(buf(1),buf(2),buf(3))
      buf(1)=xnd
      buf(2)=ynd+db
c      call plotm(buf,.false.)
      call glina3(buf(1),buf(2),buf(3))
      buf(1)=xnd
      buf(2)=ynd-db
c      call plotm(buf,.false.)
      call glina3(buf(1),buf(2),buf(3))
      buf(1)=xnd
      buf(2)=ynd
c      call plotm(buf,.false.)
      call glina3(buf(1),buf(2),buf(3))
      return
      end
