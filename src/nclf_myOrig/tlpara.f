
C*********************************************************************
C*    NAME         :  tlpara.f
C*       CONTAINS: routine to retrieve tool parameters
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       tlpara.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:48
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine tlpara(maxang, maxdp, mxloop, mindp,warn,
C*                             step,numpts,thpds, thds, thcs, toler, gougck,
C*                             iter)
C*       retrieve tool parameters
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          maxang                     maximum angle (in degrees)
C*          maxdp
C*          mxloop                     maximum number of iterations to 
C*                                     cut down the MAXDP value.
C*          mindp                      minmum value for MAXDP
C*          warn                       the switch for warning on/off
C*          step                       switch for outputting every calc pt
C*          numpts                     maximun number of points to generate
C*          thps                       part surface offset
C*          thds                       drive surface offset
C*          thcs                       check surface offset
C*          toler(2)                   machining tolerance
C*          gougck                     gough checking(on|off)
C*          iter                       number of iterations between cutter 
C*                                     displays.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine tlpara(maxang, maonce, maxdp, mdonce, mxloop, mindp,
     1                  warn, istep, numpts, nponce, thps, thds, thcs,
     2                  toler, gougck, iter)

      include 'com8a.com'
c
      real*8 maxang, maonce, maxdp, mdonce, toler(2), thps, thds,
     1       thcs(5)
      real*4 mindp
      integer*2 numpts, nponce, gougck(3), iter
      integer*2  warn,mxloop
      integer*2 istep

      integer*2 isc162(4)
      real*4 asc162(2)
      equivalence  (sc(162),asc162,isc162)
c
      maonce = sc(201)
      maxang = sc(80)
      mdonce = sc(105)
      maxdp  = sc(54)
      mxloop = isc162(1)
      mindp  = asc162(2)
      warn   = isc162(2)
		istep = 0
      if (lstep) istep  = 1
      nponce = ifl(368)
      numpts = ifl(91)
      thps = sc(23)
      thds = sc(24)
      thcs(1)  = sc(25)
      thcs(2)  = sc(177)
      thcs(3)  = sc(178)
      thcs(4)  = sc(179)
      thcs(5)  = sc(180)
      toler(1) = sc(27)
      toler(2) = sc(167)
      gougck(1) = ifl(6)
      gougck(2) = idsgck
      gougck(3) = icsgck
      iter = ifl(129)

      return
      end
