C*********************************************************************
C*    NAME         :  savecm.f
C*       CONTAINS:
C*             savecm
C*
C*    COPYRIGHT 2008 (c) Numerical Control Computer Sceinces Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       savecm.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       08/17/15 , 17:52:51
C********************************************************************/
c
c***********************************************************************
c
c   SUBROUTINE:  savecm
c
c   FUNCTION:  This routine processes the follosing SAVE commands.
c
c                   SAVE/SOLID
c
c                   SAVE/STL
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine savecm
c
      include 'com8a.com'
c
      integer*2 ietype,ierr,nsf,j,istk,ifl44,iunits
      integer*4 nclkey
c
      real*8 tol,pbuf(35)
c
      byte lfnam(MAX_LEN)
c
      character*(MAX_LEN) fnam,fnamo
c
      integer*2 ALLV,FIXTURV,LAYERV,SOLIDV,STLV,STOCKV
      parameter (ALLV=816)
      parameter (FIXTURV=898)
      parameter (LAYERV=902)
      parameter (SOLIDV=123)
      parameter (STOCKV=321)
      parameter (STLV=330)
c
c...Initialize routine
c
      ifl44 = ifl(44)
      ifl(44) = 9
      istk = 1
      call parsit
c
c...SAVE/SOLID
c
      if (ityp .eq. 1 .and. ist .eq. SOLIDV) then
          call svpars
          call parsit
c
c......SAVE/SOLID,type
c
          if (ityp .eq. 1 .and.
     1        (ist .eq. FIXTURV .or. ist .eq. STOCKV)) then
              istk = 1
              if (ist .eq. FIXTURV) istk = 2
          else
              call rtpars
          endif
c
c......SAVE/SOLID,file
c
          ldtext = .true.
          call parsit
          ldtext = .false.
          if (.not. lstrng) go to 9040
          j = 0
          fnam = ' '
          call gttext (fnam,j)
c
c......SAVE/SOLID,LAYER
c
          call parsit
          if (ityp .eq. 1 .and. ist .eq. LAYERV) then
              call parsit
              if (ityp .eq. 5 .and. ist .eq. 1) call parsit
              if (.not. scalar) go to 9010
              nclkey = tv
              call addsky (1,nclkey,2,nsf)
              call parsit
c
c......SAVE/SOLID,ALL
c
          else if (ityp .eq. 1 .and. ist .eq. ALLV) then
              call addsky (3,nclkey,2,nsf)
              call parsit
c
c......SAVE/SOLID,so-list
c
          else if (ityp .eq. 2 .and. ist .eq. 33) then
              do while (ityp .ne. 7)
                  if (ityp .ne. 2 .or. ist .ne. 33) go to 9020
                  call gtentt (tv,trflg,nclkey,ietype,pbuf)
                  call addsky (0,nclkey,2,nsf)
                  call parsit
              enddo
c
c......Solid expected
c
          else
              go to 9020
          endif
c
c......No solids specified
c
          if (nsf .eq. 0) go to 9020
c
c......Save solids
c
          call fparse (fnam,fnamo,' ','.stk')
          call ctob (fnamo,lfnam)
          call nclf_save_solid(istk,lfnam,nsf,ierr)
          if (ierr .eq. 2) go to 9060
          if (ierr .ne. 0) go to 9020
c
c...SAVE/STL
c
      else if (ityp .eq. 1 .and. ist .eq. STLV) then
          call svpars
          call parsit
c
c......SAVE/STL,type
c
          istk = 0
          if (scalar) then
              if (itv .eq. 1 .or. itv .eq. 2) then
                  istk = itv - 1
                  call svpars
                  call parsit
              endif
          else
              call rtpars
          endif
c
c......SAVE/STL,tol
c
          tol = sc(27) * 5.
          if (scalar) then
              if (tv .le. 0.) goto 9050
              tol = tv
          else
              call rtpars
          endif
          if (ifl(264) .eq. 1) tol = tol / 25.4
c
c......SAVE/STL,file
c
          ldtext = .true.
          call parsit
          ldtext = .false.
          if (.not. lstrng) go to 9040
          j = 0
          fnam = ' '
          call gttext (fnam,j)
c
c......SAVE/STL,sf-list
c
          call parsit
          call sflist (3,0,nsf,ierr)
          if (ierr .ne. 0) goto 9020
c
c......No solids specified
c
          if (nsf .eq. 0) go to 9020
c
c......Save STL file
c
          call fparse (fnam,fnamo,' ','.stl')
          call ctob (fnamo,lfnam)
          iunits = ifl(264)
          call nclf_save_stl(lfnam,nsf,iunits,istk,tol,ierr)
          if (ierr .eq. 2) go to 9060
          if (ierr .ne. 0) go to 9080
c
c...Unrecognized word
c
      else
          go to 9030
      endif
c
c...End of routine
c
 8000 call delsky
      ifl(44) = 9
      return
c
c...Scalar expected
c
 9010 call error (282)
      err = .true.
      go to 8000
c
c...Solid expected
c
 9020 call error (531)
      err = .true.
      go to 8000
c
c...Could not create solid
c
 9030 call error (532)
      err = .true.
      go to 8000
c
c...Text string (filename) expected
c
 9040 call error (513)
      err = .true.
      go to 8000
c
c...Positive value expected
c
 9050 call error (112)
      err = .true.
      go to 8000
c
c...Positive value expected
c
 9060 call error (145)
      err = .true.
      go to 8000
c
c...Positive value expected
c
 9070 call error (6)
      err = .true.
      go to 8000
c
c...Could not create STL file
c
 9080 call error (555)
      err = .true.
      go to 8000
      end
