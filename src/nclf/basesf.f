C*********************************************************************
C*    NAME         :basesf.f
C*       CONTAINS:
C*         basesf  sfouts
C*    COPYRIGHT 1984 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       basesf.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:37
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine basesf
C*       Create a basesf surface.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine basesf

      include 'com.com'

      integer*4 nsfkey, nclkey
      integer*2 nwds, ietype, ktv(4), iblnk
      equivalence (tv,ktv)
      data iblnk /1/

      call gtdesc (sc(11),nsfkey,nwds,ietype)
      defwf = .true.
      call trmext (nsfkey, nclkey)
      if (nclkey.eq.0) goto 9163
      ktv(3) = 1
      ietype = 9
      call ptdesc (nclkey, ietype, tv)
      rest = tv
      if (.not. ldspsf) call blkgeo (nclkey, iblnk)

99    return

9163  ifl(2) = 163
      err = .true.
      goto 99

      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine sfouts
C*       Extract component surfaces from a NET surface or Composite solid.
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
      subroutine sfouts

      include 'com8a.com'

      integer*4 nclkey
      integer*2 nwds,ietype,jcomp
      integer*2 i2v1 /1/

      integer*4 isub, svinx
      character*64 cmname
      character*64 nname
      integer*2 parslb,iret,ierrno
      logical lb0set
c
c...Initialize routine
c
      asn = sc(11)
c
c...Initialize labeling
c
      isub = 0
      cmname = savid2
      svinx = isvsub
      lb0set = (ifl(296) .eq. 0)
c
c...Don't use automatic labeling
c...since the prefix 'SF' is added
c...to the assigned labels, but
c...solids would be called 'SO'
c
c      if (ifl(296) .eq. 1) then
        defwf = .true.
        isub = isvsub
        nname = cmname
        if (isub .eq. 0) then
          iret = parslb(nname,isub)
          isvsub = isub
          ifl(296) = iret
        else
          ifl(296) = 0
        endif
c      endif
c
c...Get number of component surfaces
c
      call gtdesc(asn, nclkey, nwds, ietype)
      call gtccnm (nclkey, nwds)
      if (nwds .eq. 0) go to 9000
      ncv =  isc10(3)
c
c...Get all component surfaces
c
      if (ncv .eq. 0) then
          do j = 1,nwds
              call sfoutj (nclkey,j,nname,isub,ierrno)
              ifl(2) = ierrno
              if (ifl(2) .gt. 0) goto 9000
              if (ifl(296) .gt. 0) ifl(296) = ifl(296)+1
          enddo
c
c...Get single component surface
c
      else if (ncv .eq. 1) then
          j = isc10(4)
          if (j .gt. nwds) j = nwds
          if (j .lt. 1) j = 1
          call sfoutj (nclkey,j,nname,isub,ierrno)
          ifl(2) = ierrno
          if (ifl(2) .gt. 0) goto 9000
c
c...Get list of component surfaces
c
      else
          do i=1,ncv,1
            call getskj (i,j)
            call sfoutj (nclkey,j,nname,isub,ierrno)
            ifl(2) = ierrno
            if (ifl(2) .gt. 0) goto 9000
            if (ifl(296) .gt. 0) ifl(296) = ifl(296)+1
          enddo
      endif
c
c...Reset labeling
c
      if (lb0set) then
          savid2 = cmname
          isvsub = svinx
      endif
      defwf = .true.
      ifl(329) = 1
c
c...End of routine
c
 8000 call delsky
      ifl(296) = 0
      return
c
c...No composites to extract
c
 9000 if (ifl(2).lt.1) ifl(2) = 5
      err=.true.
      go to 8000
      end
