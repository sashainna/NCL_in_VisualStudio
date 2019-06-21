C*********************************************************************
C*    NAME         :  ptdesc.f
C*       CONTAINS: routine to put descriptors
C*    COPYRIGHT 1986 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ptdesc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:30
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptdesc(nclkey, itype, r8desc)
C*       Given the UNIBASE key (NCLKEY) and type of entity (ITYPE)
C*       return the packed descriptor.
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey               UNIBASE key of entity
C*          itype                type of entity
C*       OUTPUT :  
C*          r8desc               packed descriptor of entity
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine ptdesc(nclkey, itype, r8desc)

      include 'com4a.com'

      integer*4 nclkey
      integer*2 itype
      real*8 r8desc

      real*8 r8temp
      integer*4 i4temp(2)
      integer*2 i2temp(4)

      equivalence (r8temp, i4temp), (r8temp, i2temp)

      r8temp = r8desc
      i4temp(1) = nclkey
      i2temp(4) = itype
      r8desc = r8temp

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptdsc3(nclkey, nwds, itype, r8desc)
C*       Given the UNIBASE key (NCLKEY), size (NWDS) and type of entity (ITYPE)
C*       return the packed descriptor.
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey               UNIBASE key of entity
C*          nwds                 number of words
C*          itype                type of entity
C*       OUTPUT :  
C*          r8desc               packed descriptor of entity
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine ptdsc3(nclkey, nwds, itype, r8desc)

      include 'com4a.com'

      integer*4 nclkey
      integer*2 nwds, itype
      real*8 r8desc

      real*8 r8temp
      integer*4 i4temp(2)
      integer*2 i2temp(4)

      equivalence (r8temp, i4temp), (r8temp, i2temp)

      i4temp(1) = nclkey
      i2temp(3) = nwds
      i2temp(4) = itype
      r8desc = r8temp

      return
      end
