C*********************************************************************
C*    NAME         :  gtdesc.f
C*       CONTAINS: routine to get descriptors
C*    COPYRIGHT 1986 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       gtdesc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:09
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtdesc(r8desc, nclkey, nwds, itype)
C*       Given a packed descriptor (R8DESC) return the UNIBASE key
c*       (NCLKEY) and type (ITYPE) of entity.
C*    PARAMETERS   
C*       INPUT  : 
C*          r8desc               packed descriptor of entity
C*       OUTPUT :  
C*          nclkey               UNIBASE key of entity
C*          nwds                 number of words to return
C*          itype                type of entity
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine gtdesc(r8desc, nclkey, nwds, itype)

      include 'com4a.com'

      real*8 r8desc
      integer*4 nclkey
c      integer*2 nclkey
      integer*2 itype,nwds

      real*8 r8temp
      integer*4 i4temp(2)
      integer*2 i2temp(4)

      equivalence (r8temp, i4temp), (r8temp, i2temp)

      r8temp = r8desc
      nclkey = i4temp(1)
      itype = i2temp(4)
      nwds = i2temp(3)

      return
      end
