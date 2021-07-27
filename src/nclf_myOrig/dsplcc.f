C*********************************************************************
C*    NAME         :  dsplcc.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dsplcc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:59
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dsplcc(nclkey,dtype)
C*      display circle and curve geometry types on the plotter
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
       subroutine dsplcc(nclkey,dtype, tfmat)

c  dtype    type of display
c
c   7      disply/ci
c   8      disply/cv

      include 'com4a.com'
c      include 'dspcom.com'
      parameter (maxpt=50)
      parameter (maxwd=(maxpt*6+(maxpt+1)/2))

      common/dspcom/ws(20),w(maxwd)

      real*8 w,ws
      real*4 aw(300),aws(40)
      equivalence (w,aw),(ws,aws)

       integer*2 dtype
       integer*4 nclkey
       real*8 tfmat(12)


cuni       call getent (w(1),nw,pg,el,isub)
       call gtgeo(nclkey,w(1))
c          call the stroking routine
c
c....added transformation
c....Yurong 10/6/97
c
      call dstrcc(nclkey, dtype, tfmat)
99999  return
       end
