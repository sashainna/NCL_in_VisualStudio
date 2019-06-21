C*********************************************************************
C*    NAME         :  cintck.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cintck.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:40
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cintck
C*     checks for circ int this pass.                                   
C*          reqs:   1.  circul mode on.  (by default or declaration)   
C*                  2.  ps is a plane                                 
C*                  3.  ds is a true circle in ps plane view         
C*                  4.  ta is perpto ps plane                       
C*                  5.  dntcut is not in effect
C*     all conditions met, turn on ifl(95).  these conditions replace  
C*       those previously in effect which required constant zm value. 
C*    PARAMETERS   
C*       INPUT  : 
C*          ci    = Circle axis (IJK).
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine cintck (ci)

      include 'com4a.com'
      include 'mocom.com'

      real*8 ci(3)

      real*8 asn
      integer*2 jd(600),ksn(4)
      logical lv92
      real*4 ad(300)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      equivalence(asn,ksn)
      real*4 ps(3),ta(3)

c          if circul mode off or ps not pl, exit
      if(ifl(94).eq.0.or.jd(1).ne.6)goto 99
c
c..... if a GO motion, exit
c
      if (ifl(280) .eq. 1) goto 99
c
c...No circular with DNTCUT
c
      lv92 = sc(169) .lt. 9.249
      if (ifl(42) .eq. 1 .and. .not. lv92) go to 99
c
c...If TLAXIS/,MODIFY anything other than translate up
c...then exit
c
      if (ifl(104) .eq. 1 .and.
     1   (tool(12) .ne. 0. .or. tool(13) .ne. 0. .or.
     2    tool(15) .ne. 0. .or. tool(16) .ne. 0.)) go to 99
c
c...If variable tlaxis mode or tlaxis is modified, exit
c
      if (ifl(23).eq.TA_THRU_PT .or. ifl(23).eq.TA_THRU_CV .or.
     x    ifl(23).eq.TA_INTERPOL .or. lmdnow .or. lcvgid) goto 99
c
c...No circular when motion/combined - qar 93161
c
      if (mocom) goto 99

c          get psnorm, circ axis, and ta
      do 20 i=1,3
      ps(i)=s(i,1)
cc      ci(i)=d(i+55)
20    ta(i)=sc(i+3)
c          chk pspl for parlel circle.
      co=ps(1)*ci(1)+ps(2)*ci(2)+ps(3)*ci(3)
      if(abs(co).lt..99999) goto 99
c          if ta normps, automatically ok
      if(ifl(23).eq.1)goto 40
c           chk ta vs. psnorm
      co=ps(1)*ta(1)+ps(2)*ta(2)+ps(3)*ta(3)
      if(abs(co).lt..99999) goto 99
c          ok for circ int this pass.
c          turn on ifl(95) and 3000-2 cl record will output from
c          mover (via cirrec) instead of 5000-5 type.
40    ifl(95)=1

99    return
      end
