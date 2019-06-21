C*********************************************************************
C*    NAME         :ranstr.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ranstr.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:34
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ranstr(name,isub,ietype,nclkey,is)
C*       This routine stores an identifier in the ranfil hash table.
C*       It is called by unicad that has already put the canonical data
C*       into the unibase.
C*
C*       RANSTR() is only called by C (indirectly through LABWF() and 
C*          LABCAM()).
C*
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
      subroutine ranstr(name,isub,ietype,nclkey,is)

c      include 'com8.com'
      include 'com.com'

      character*64 name
      integer*4 isub, nclkey
      integer*2 ietype,is

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      integer*2 if41
      integer*2 irest(4)
      integer*4 i4rest,lsub
      equivalence (rest,i4rest,irest)

      savid2 = name
      isvsub = 0
      lsub = isub
      if (isub.lt.0) lsub = -isub-1
      token2 = savid2
      ivxsub = isvsub
      if (lsub .gt. 0) then
        isvsub = lsub
        ivxsub = 0
        call vstchk

        if (ist.ne.14) then
          isvsub = 0
          ifl(9)=ifl(11)
          ifl(10)=ifl(12)
          idst=14
          rest=0.
          irest(3)=32767
          irest(4)=14
          call vstore
          isvsub = lsub
        endif
        ivxsub = lsub
      endif
c
      call vstchk
c
c...Allow scalars to overwrite themselves
c...since there is no facility for generating
c...new names for scalars.
c...Bobby  -  2/26/92
c
      if41   = ifl(41)
      if (ist .eq. 2 .and. ietype .eq. 2) if41 = 1

      if (ist.ne.1.and.if41.eq.0) then
c           found it in hash table and canon is off
         is=1
         go to 99999
      else
         is = 0
         i4rest = nclkey
         irest(3) = 1
         if (ietype.eq.3.or.ietype.eq.4) then
             irest(3) = 3
         else if (ietype.eq.5) then
             irest(3) = 6
         else if (ietype.eq.6) then
             irest(3) = 4
         else if (ietype.eq.7) then
             irest(3) = 11 
         else if (ietype.eq.10) then
             irest(3) = 12
c...added for point vec
c...Yurong
         else if (ietype.eq.21) then
             irest(3) = 6
         else if (ietype.eq.DATAST) then
             call dtgtnw(nclkey, irest(3))
         endif
         irest(4) = ietype
         idst = ietype
         keyold = keyhld
         istold = ist
         ifl(9) = ifl(11)
         ifl(10) = ifl(12)
c
c...Set ifl(299) to designate that vstore() was called from CADD
c...This is so that attributes will not be updated as well as
c...scalars not being stored in the Unibase.
c
         ifl(299)=1
         call vstore
         ifl(299)=0
      endif
99999 continue
      return
      end
