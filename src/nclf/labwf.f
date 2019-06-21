c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       labwf.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:13
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1988 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: labwf                                             **
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C          Generate a label for a wireframe entity, set up for later 
C          store in the ranfile & return label to the caller.
C **********************************************************************
C **********************************************************************
 
      subroutine labwf(ietype, label, isub, nclkey, where, istatus)
 
      include 'com4a.com'
 
      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      integer*2 ietype, istatus
      character*64 label, tempid
      integer*4 isub, nclkey, lsub
      integer*4 where

c... check for (internal) unknown label logic
      if (unklbl) then
        label = '@UN'
        isub = 0
        goto 99
      endif
c
      if (defwf) then
          label = savid2
          isub = isvsub
          if (ifl(296).eq.1 .and. ifl(41).eq.0 .and.
     1        label(1:1) .ne. '@') then
            token2 = label
            ivxsub = isub
            call vstchk
            if (ist.ne.1 .and. (ist.ne.14 .or. ivxsub.ne.0)) then
              call error (8)
              istatus = -8
            endif
          endif
          idst = ietype
          goto 99
      endif
c                                If called from ur_lp02b first check the
c                                ranfile for existing labels.
      istatus = 0
      if (where .eq. 1) then
          token2 = label
          ivxsub = isub
          call vstchk
          if (ist .eq. 1) goto 20
      endif
c                                  Loop until we find an unused name.
c                                  put the subscript (if any) in the 4th
c                                  integer of token.
c...This time there is no change in logic unless something is wrong
c...and there is always something wrong..., so I just encapsulated
c...search and name generation logic in namgen_s to use here and
c...in umu_multi_fillet. The next 3 lines of code are new.
c...vp 24-oct-96
c 
c10    call namgen(1,ietype,token,itok(4))
c10    call namgen(0,ietype,token,itok(4))
c                        since namgen is called with 1 option subscript is
c                        incremented before the name is saved.
   10 call namgen_s (ietype, tempid, lsub)
      if (ifl(2).gt.0)  return
      token2  = tempid
      ivxsub = lsub
      istatus = 1

20    if (where .eq. 1) then
         call ranstr(token2, isub, ietype, nclkey, is)
         if (is.eq. 1) goto 10
         label = token2
         goto 99
      endif
c
c...removed - vp 21-apr-95
c
c     call vstchk
c     if (ist.ne.1) goto 10

      savid2 = token2
      isvsub = ivxsub
      idst = ietype
      keyold = keyhld
      istold = ist
      ifl(9) = ifl(11)
      ifl(10) = ifl(12)
      label = token2
      isub = ivxsub
 
99    return
      end

C **********************************************************************
C **                                                                  **
C **  FUNCTION NAME: upsubs                                           **
C **                                                                  **
C **  PURPOSE OF FUNCTION: update the subscript                       **
C **                                                                  **
C **********************************************************************
      subroutine upsubs
 
      include 'com.com'

 
      isvsub = isvsub+1

      return
      end

C **********************************************************************
C **                                                                  **
C **  FUNCTION NAME: stsavi(buf)                                      **
C **                                                                  **
C **  PURPOSE OF FUNCTION: update the label                           **
C **                                                                  **
C **********************************************************************
      subroutine stsavi(buf)

      include 'com.com'
      character*64 buf
      
      savid2 = buf
      isvsub = 0

      return
      end

C **********************************************************************
C **                                                                  **
C **  FUNCTION NAME: namgen_s                                         **
C **                                                                  **
C **  PURPOSE OF FUNCTION:                                            **
C **       Generate a label exactly like it is in assgn cheking if it **
C **       exists in ranfile                                          ** 
C **********************************************************************
 
      subroutine namgen_s (ietype, label, isub)
 
      include 'com4a.com'
 
      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld, svinx
      integer*2 istold

      integer*2 irest(4),ietype
      character*64 label, tempid, savetk
      integer*4 isub,savesub
      equivalence (rest,irest)
c
      savetk = token2 
      savesub = ivxsub
      ivxsub = 0 

10    call namgen(0,ietype,token2,ivxsub)
      if (ivxsub.gt.999999) then
           ifl(2) = 85
           return
      endif
c
      tempid  = token2
      svinx = ivxsub 
      call vstchk
      token2   = tempid
      savid2  = token2
	ivxsub = svinx
      isvsub = ivxsub
      if (ist .ne. 1 .and. ist .ne. 14 .or.
     -      ist .eq. 14 .and. ivxsub .eq. 0) then
          call namgen(1,ietype,token2,ivxsub)
          if (ivxsub.gt.999999) then
               ifl(2) = 85
               return
          endif
          go to 10
      end if
      ifl(9) = ifl(11)
      ifl(10) = ifl(12)
      if (ivxsub .gt. 0 .and. ist .eq. 1) then
         isvsub = 0
         isvst = ietype
         idst = 14
         rest = 0.
         irest(3) = 0
         irest(4) = 14
         call vstore
         idst = isvst
         token2 = tempid
         savid2 = token2
         ivxsub = svinx
         isvsub = svinx
      end if
      if (ivxsub .gt. 0) then
         call vstchk
         if (ist .ne. 1 .and. ist .ne. 14 .or.
     -      ist .eq. 14 .and. ivxsub .eq. 0) then

            call namgen (1, ietype, token2, ivxsub)
            if (ivxsub.gt.999999) then
                 ifl(2) = 85
                 return
            endif
            go to 10
         end if
         ifl(9) = ifl(11)
         ifl(10) = ifl(12)
      end if
c
      label  = token2
      isub   = ivxsub
c
      token2  = savetk
      ivxsub = savesub
c
      return
      end
