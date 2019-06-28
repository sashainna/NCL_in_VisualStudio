C*********************************************************************
C*    NAME         :  ncvclf.f
C*       CONTAINS:
C*    COPYRIGHT 1992 (c) NCCS.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        ncvclf.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:20
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ncvclf (kfl,captnm,kerr)
c*       Controls the output of the intermediate Apt Source files
C*       during an NCL to NCLIPV session.
C*    PARAMETERS
C*       INPUT  :
C*          kfl     I*4   D1   1 = TOOLPATH ON, start a new clfile.
C*                             2 = TOOLPATH OFF, output intermediate
C*                                 clfile.
C*
C*          captnm  C*n   D1   Apt Source file name to create.
C*
C*       OUTPUT :
C*          kerr    I*4   D1   Returns non-zero when an error occured.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine ncvclf (kclf,captnm,kst,ken,kerr)
c
      include 'com8a.com'
c
      integer*2 kclf
      integer*4 kerr,kst(2),ken(2)
c
      character*80 captnm
c
      integer*2 inum,ifl69,ifl88,ifl261
c
      logical isav
c
c...Create APT source file
c
      kerr   = 0
      ifl69  = ifl(69)
      ifl88  = ifl(88)
      ifl261 = ifl(261)
      ifl308 = ifl(308)
      isav   = aptcom
      ifl(69) = 2
      ifl(88) = 1
      aptcom = .false.
      ifl(261) = 0
c
c...Create APT source file
c
      inum   = 2
      call aptsrc (inum,captnm,kclf,kst,ken)
c
c...Reset clfile and APT source flags
c
      ifl(69) = ifl69
      ifl(88) = ifl88
      ifl(261) = ifl261
      ifl(308) = ifl308
      aptcom = isav
c
c...End of routine
c
 8000 return
      end
