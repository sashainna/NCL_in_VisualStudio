C*********************************************************************
C*    NAME         :  fillet.f
C*       CONTAINS:  fillet  
C*    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       fillet.f , 25.2
C*    DATE AND TIME OF LAST MODIFICATION
C*       10/13/15 , 11:07:54
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : fillet 
C*      parse FILLET command & and call processing routine.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine fillet (intat) 
c
      include 'com.com'
c
      integer*2 intat
c
      integer*4 isub, nent,nclkey,list(100), stat, prefil
      integer*2 i,nwds,ietype
c
      real*8 radius
c
      character*64 cmname
      integer*4 cmsub
      character*64 fillnm 
c      character*6 nname
      integer*2 parslb,iret
c
      isub = 0
      ifl(296) = 0
      cmname = SAVID2
      cmsub = isvsub
      if (intat .eq. 1) then
          defwf = .true.
          isub   = isvsub
          fillnm = cmname(1:64)
          if (isub .lt. 1) then
            if (sc(169) .lt. 9.349) goto 9085
            iret = parslb(fillnm,isub)
            ifl(296) = iret
          endif
      endif

      ifl(44) = 9
      radius  = 0.
c
c...Get radius of fillets
c
      call parsit
      if (.not. SCALAR) go to 9007
      radius = TV
      if (ifl(264) .eq. 1) radius = radius/25.4d0
      if (NXTEOS) go to 9001
c
c...Get list of curves (LN,CI only acceptable for now)
c
      nent  = 0
      do 505 i=1,1000
         call parsit
         if (ityp .eq. 2 .and. (ist .eq. 5 .or. ist .eq. 7)) then
            call gtdesc (TV,nclkey,nwds,ietype)
            nent = nent + 1
            list(nent) = nclkey
         else
            go to 9001
         end if
         if (NXTEOS) go to 800
  505 continue
c
c...Create fillets
c...disable driver calls of geognx, vstore & dspent
c
  800 stat = prefil (list,nent,radius,fillnm,isub)
      if (stat .ne. 0) go to 9000
      if (err .and. stat .ne. 0) goto 8000

c      if (stat .eq. 0) call vstore

      if (intat.eq.0) then
          SAVID2 = cmname
          isvsub = cmsub
      endif
      defwf  = .true.
      ifl(262) = 1
      ifl(329) = 1
      go to 8000
c
 9000 isvinx = 0
      ier    = stat
      call error (ier)
      go to 8000
c
 9001 call error (192)
      go to 8000
c
 9007 call error (7)
      go to 8000
c
 9085 call error (85)
      go to 8000
c
 8000 ifl(296) = 0
      return
      end
