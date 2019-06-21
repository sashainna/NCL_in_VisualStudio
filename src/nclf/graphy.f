C*********************************************************************
C*    NAME         :  graphy.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       graphy.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:09
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine grfset
C*     set graphic option flag off or on        
C*     depending on which version of this routine is linked.         
C*                                                                   
C*     if the odl file calls out "graphy" the graphics option switch 
C*     will be set to on and lathe commands will be enabled.         
C*                                                                   
C*     if the odl file calls out "graphn" the graphics option switch 
C*     be set to off and graphics option commands will cause error   
C*     messages to be produced.                                      
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
      subroutine grfset

      include 'com8a.com'

c ***********************************************************************
c
c on on on on on on on on on on on on on on on on on on on on on on on on
c on on on on on on on on on on on on on on on on on on on on on on on on
c
c          this version of grfset will cause the graphics option switch
c          to be set to on so graphics option commands will work
c
c on on on on on on on on on on on on on on on on on on on on on on on on
c on on on on on on on on on on on on on on on on on on on on on on on on
c
c **********************************************************************

      ifl(170)=1

      return
      end
