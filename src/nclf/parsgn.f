C*********************************************************************
C*    NAME         :  parsgn.f
C*       CONTAINS:
C*    COPYRIGHT 1986 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       parsgn.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:24
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine parsgn
c*      generates next valid name to pthru when parsing a generic
c*      name.  
C*    PARAMETERS   
C*       INPUT  : 
c*          basnam - character*7 (equivalenced to sc(116)
c*                   base name to be used to search for matching full
c*                   name.  valid base names are:
c*                      pt, ve, ln, pl, ci, cv, sf, sh, all, x*
c*                         where x in x* may be from 1 to 6 characters.
C*          genfst - logical in common block
c*                   if true, this is the first time to convert this 
c*                            base name to a full name.
c*                   if false, the base name has been used in prior calls
c*                            to this routine since last parse was done.
C*       OUTPUT :  
C*          token - character*20
c*                   next full name (including subscript, if any) that 
c*                   satisfies the base name requirements.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine parsgn

      include 'com4a.com'

      real*8 asw
      integer*4 nclkey
      equivalence (nclkey, asw)

      if (genfst) then

c           turn off flag indicating this routine has been entered before
c           for this base name
         genfst = .false.

c           go get next valid name from ranfil hash table
      else
         

      endif

99999 return
      end
