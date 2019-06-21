c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       labcam.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:13
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1988 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: labcam                                            **
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C          Generate a label for a CAM entity, and store it
C          in the ranfile & return label to the caller. called from
C          ur_lp02b when merging.
C **********************************************************************
C **********************************************************************
 
      subroutine labcam(ietype, label, isub, nclkey, istatus)
 
      include 'com4a.com'
 
      integer*2   ietype,is
      character*64 label
      integer*2   istatus
      integer*4   isub, nclkey
c                                 initialize the subscript.
      token2 = label
      ivxsub = 0
      call vstchk
      if (ist .eq. 1.or.ist .eq. 14) then
          istatus = 0
          goto 20
      endif
c                                  Loop until we find an unused name.
c                                  put the subscript (if any) in the 4th
c                                  integer of token.
10    call namgen(1,ietype,token2,ivxsub)
c                        since namgen is called with 1 option subscript is
c                        incremented before the name is saved.
      isub = ivxsub
      if (isub .gt. 0) isub = isub - 1
      istatus = 1
      ivxsub = 0
      label = token2
c                       store the new name in the ranfile
20    call ranstr(label, isub, ietype, nclkey, is)
c
c...If this entity is a scalar and we cannot
c...store it in the Unibase, because this label
c...is already used as a different geometric type
c...(Line, Plane, etc.), then we will not attempt
c...to store it in the Unibase.
c...Bobby  -  2/26/92
c
      if (is .eq. 1 .and. ietype .ne. 2) goto 10
      label = token2
 
99    return
      end
