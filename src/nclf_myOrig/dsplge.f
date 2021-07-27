C*********************************************************************
C*    NAME         :  dsplge.f
C*       CONTAINS:
C*                    dsplge
C*                    dstrge
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dsplge.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:59
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dsplge (nclkey, dtype)
C*       display point, vector, line and plane
C*       geometry types 
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
       subroutine dsplge (nclkey, dtype,tfmat)

c  dtype   type of display

c   3       disply/pt
c   4       disply/ve
c   5       disply/ln
c   6       disply/pl

      include 'com8a.com'

       real*8 dbuf(16)
       integer*4 nclkey
       real*8 tfmat(12)
       real*8 p(3)
       real*8 pout(3)
       integer*2 dtype, pg, el, nw, peq(4)
       equivalence (pg,peq(1)),(el,peq(2)),(nw,peq(3)),(peq,sc(11))

c         first check if displaying a point
      if (dtype.eq.3) then
          if (ifl(127).eq.0) then
c
c...Added tranformation
c...Yurong 9/30/97
c
              call drwpt(nclkey, tfmat)
c              if(lablpt) then

                 call gtgeo (nclkey, dbuf)
c
c...Added tranformation
c...Yurong 9/30/97
c
                 p(1) = dbuf(1)
                 p(2) = dbuf(2)
                 p(3) = dbuf(3)
                 call cctmtf(p, tfmat, pout)
                 call drwlab(pout(1),pout(2),pout(3), nclkey)

c              endif
              goto 99999
          endif
      endif
c              check if data coming from ranfil or data base file
      if (ifl(190).eq.0) then
          call gtgeo (nclkey, dbuf)
      else
cuni      call dbgent (dbuf(1),nw,pg,el,isub)
      endif
c              display data
c
c...Added tranformation
c...Yurong 9/30/97
c
      call dstrge (dtype, nclkey, dbuf, tfmat)

99999    if (debug) then
         write(cout, 9010) dbuf(1),dbuf(2)
9010     format('dsplge ',2f7.2)
         call putmsg(cout,80,23,0)
      endif

      return
      end
