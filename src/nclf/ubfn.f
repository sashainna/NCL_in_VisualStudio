c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       ubfn.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:49
c**
c*****************************************************
c** copyright (c) 1994 NCCS
c **********************************************************************
c **********************************************************************
c **  subroutine name: ubfn                                           **
c **                                                                  **
c **  purpose of subroutine: to open/close the requested Unibase file **
c **                                                                  **
c **********************************************************************
c **********************************************************************

      subroutine ubfn

      include 'com8a.com'

      integer*2 j,ierr,ivc,ftype
      integer*4 nc,strlen1
      character*(MAX_PATH) temp,fname
      character*1 c
      integer*4 flag
c 
c...Check if it is a close request
c
      istrt = inx
c
      ldtext = .true.
      call parsit
      if (ityp .eq. 7) then
         call error(449)
         goto 800
      endif
      err    = .false.
      ifl(2) = 0
      ivc    = ist
cc      ist    = 99
      if (ityp .eq. 1 .and. ivc .eq. 831) go to 250
c
c...Name specified, so get it
c
      if (lstrng) then
        j = 0
        temp = ' '
        call gttext(temp,j)
        ipos = 0
      else
        temp  = cimage(1:nccimg)
        ipos  = index (temp(1:nccimg),'/')
        j = nccimg
      endif
  115 ipos  = ipos + 1
      c     = temp(ipos:ipos)
      if (ipos .lt. j .and. (c .eq. ' ' .or. c .eq. '	')) 
     x      go to 115
      fname = temp(ipos:j)
      i     = index (fname,',')
      if (i .gt. 0) fname(i:) = ' '
c
c...Add default extension
c...and try to open file
c
      call fparse (fname,fname,' ','.u')      
      call flname (5,fname,fname)
c
c...vp 2/10/98 add ftype to support ASCI unibase files
c
      nc = strlen1(fname)
      call ur_op04(fname,nc,ftype,ierr)
      if (ierr .gt. 1) ifl(316) = 0
      if (ierr .ne. 0 .and. ist .eq. 1) ist = 2
      if (ierr .eq. 1) then
         call error(452)
      else if (ierr .eq. 2) then
         call error(449)
      else if (ierr .eq. 3) then
         call error (145)
      endif
      if (ierr .gt. 0) then 
          go to 800
      else if (ierr .lt. 0) then
          call ubact (2)
          call f77_resu
          go to 200
      end if
c
c...file opened successfuly, if old file
c...switch unibase and load this file
c
      call f77_fdst
      call ubact (2)
      call f77_resu 
c
c...vp 2/10/98 add ftype to support ASCI unibase files
c
      call ub2lod (ftype,ierr)
      if (ierr .ne. 1) call error(-454)
  200 call ubact (1)
      call ncl_ublist_init
      call ncl_ubkey_init
      go to 800 
c
c...Close secondary unibase, make sure it is active.
c
  250 call ubact (2)
      call ur_cl04(ierr)
      if ( ierr .eq. 1) then
         call error(450)
      else if (ierr .gt. 1) then
         call error(451)
      endif
      ifl(316) = 0
c
c...Deallocate memory used for secondary unibase
c
c      call memrel
      call ubact (1)
      call ncl_ublist_free
      call ncl_ubkey_free
c
  800 continue
      ldtext = .false.
      flag = 2
      call uv_upd_scdv(flag)
      return
      end
