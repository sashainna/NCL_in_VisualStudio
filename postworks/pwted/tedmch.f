c*********************************************************************
c    NAME:  tedmch.f
c    Description:
c            MDF file manipulation routines.
c    CONTAINS:
c      ptd_lodmch     ptd_savmch  ptd_edit_mdf  ptd_load_mdf  ptd_save_mdf
c      ptdf_copy_mdf  cpymdf
c
c    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
c    MODULE NAME AND RELEASE LEVEL
c       tedmch.f , 24.2
c    DATE AND TIME OF LAST  MODIFICATION
c       06/09/14 , 16:41:48
*********************************************************************/
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_lodmch (cmach,kpm,gpm,cpm,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine loads the input Machine descriptor file.
c
c   INPUT:  cmach   I*4  D1  -  Name of Machine descriptor file to
c                               load.  ' ' = No descriptor file specified.
c                               '~' = Filename specified in LCMPFI.
c
c           kfl     I*4  D1  -  1 = Load .OBJ file along with MDF file.
c
c   OUTPUT: kpm     I*4  Dn  -  Integer common array from MDF file.
c
c           gpm     R*8  Dn  -  Real common array from MDF file.
c
c           cpm     C*1  Dn  -  Character common array from MDF file.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ptd_lodmch (cmach,kpm,gpm,cpm,kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      integer*4 kerr,kfl,kpm(2000)
c
      real*8 gpm(1000)
c
      character*1 cpm(8000)
      character*(*) cmsg,cmach
c
      integer*4 IUNIT,NKPOSM,NCPOSM,NPOSMA,LMNPT,LUPD
c
      data IUNIT  /0087/, NKPOSM /0171/, NPOSMA /0172/
      data NCPOSM /0173/, LMNPT  /141/,  LUPD   /3001/
c
      integer*4 irecl,nc,i,iuns,mkp,mpo,mcp,is,rindex
      integer*4 nc1,nc2,inkp,inpo,incp,strlen1,nupd,nrev
c
      character*8 lupdat
      character*20 att(4),lnum
      character*40 lname
      character*(MAX_PATH) fnam
c
      mkp    = 2
      mpo    = 4
      mcp    = 1
cc      lsav   = ' '
      do 100 i=1,40,1
          lname(i:i) = cpm(LMNPT+i-1)
  100 continue
c
c...Input Machine was not specified
c
      iuns   = kpm(IUNIT)
      if (cmach .eq. ' ') then
          if (lname .eq. ' ') lname = '0'
c
c...Set up input file name
c
      else
cc          lsav   = lname
          if (cmach .ne. '~') then
              nc     = strlen1(cmach)
              LCMPFI = 'PWORKS_' // cmach(1:nc) // '.MDF'
          endif
c
c...Open input file
c
          att(1) = 'direct'
          att(2) = 'none'
          att(3) = 'unformatted'
          att(4) = 'old'
          irecl  = 8000
          call opnfil (LUNSC1,LCMPFI,att,irecl,cmsg,kerr)
          if (kerr .eq. -2) then
              call fparse (LCMPFI,LCMPFI,DVDATA,'.MDF')
              call opnfil (LUNSC1,LCMPFI,att,irecl,cmsg,kerr)
          endif
          if (kerr .ne. 0) go to 8000
c
c...Load the first record of Common KPOSMP
c
          inkp = kpm(NKPOSM)
          inpo = kpm(NPOSMA)
          incp = kpm(NCPOSM)
          call rdcom (LUNSC1,1,kpm(1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c...Check version of Common arrays
c
          if (kpm(NKPOSM) .gt. mkp) mkp = kpm(NKPOSM)
          if (kpm(NPOSMA) .gt. mpo) mpo = kpm(NPOSMA)
          if (kpm(NCPOSM) .gt. mcp) mcp = kpm(NCPOSM)
          kpm(NKPOSM) = inkp
          kpm(NPOSMA) = inpo
          kpm(NCPOSM) = incp
c
c...Load the Common arrays
c
          do 205 i=2,mkp
              call rdcom (LUNSC1,i,kpm((i-1)*2000+1),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
  205     continue
          is     = mkp + 1
          do 215 i=is,mkp+mcp
              call rdcom (LUNSC1,i,cpm((i-is)*8000+1),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
  215     continue
          is     = is + mcp
          do 300 i=is,mkp+mcp+mpo,1
              call rdcom (LUNSC1,i,gpm((i-is)*1000+1),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
  300     continue
c
cc          if (lsav(1:1) .ne. ' ') then
cc              do 315 i=1,40,1
cc                  cpm(LMNPT+i-1) = lsav(i:i)
cc  315         continue
cc          endif
          if (IOPFL(2) .ne. 3) then
             if (iuns .ne. kpm(IUNIT)) call metini (iuns)
             kpm(IUNIT)  = iuns
          end if
      endif
      call preini
c
c...Open Post macro file
c
      if (kfl .eq. 1) then
          att(1) = 'direct'
          att(2) = 'none'
          att(3) = 'unformatted'
          att(4) = 'old'
          irecl  = 512
          fnam   = LCMPFI
          nc1    = rindex(fnam,'.')
          if (nc1.eq.0) goto 8000
          fnam(nc1+1:) = ' '
          nc2    = rindex(fnam,'_')
          if (nc2.eq.0) goto 8000
          lnum   = fnam(nc2+1:nc1-1)
          nc     = nc1-nc2-1
          fnam   = 'pmacro_' // lnum(1:nc) // '.OBJ'
          call opnfil (LUNSC4,fnam,att,irecl,cmsg,kerr)
          if (kerr .eq. -2) then
              call fparse (fnam,fnam,DVDATA,'.OBJ')
              call opnfil (LUNSC4,fnam,att,irecl,cmsg,kerr)
          endif
c
c......Load macro file
c
          if (kerr .eq. 0) then
              call lodcmp (LUNSC4,cmsg,kerr)
          else
            if (kerr .eq. -2) then
              JHED(1) = 0
              JHED(7) = 0
            endif
            kerr = 0
          endif
      endif
c
c...End of routine
c
 8000 call clsfil (LUNSC1)
      call clsfil (LUNSC4)
c
c...Make sure MDF file is not from
c...a future version of Mpost
c
      if (kerr .eq. 0) then
          do 8100 i=1,8,1
              lupdat(i:i) = cpm(LUPD+i-1)
 8100     continue
          call cnvday (lupdat,nupd)
          call cnvday (REVDAT,nrev)
          if (nupd .gt. nrev) then
              kerr   = -1
              call errtxt ('MDFVER',cmsg)
              call errstr (cmsg,lupdat,0)
              call errstr (cmsg,REVDAT,0)
          endif
      endif
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_savmch (klun,kpm,gpm,cpm,cmsg,kerr)
c
c   FUNCTION:  This routine saves an MDF file to an already opened file.
c
c   INPUT:  klun    I*4  D1  -  Unit number of MDF file.
c
c           kpm     I*4  Dn  -  Integer common array for MDF file.
c
c           gpm     R*8  Dn  -  Real common array for MDF file.
c
c           cpm     C*1  Dn  -  Character common array for MDF file.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ptd_savmch (klun,kpm,gpm,cpm,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,kerr,kpm(2000)
c
      real*8 gpm(1000)
c
      character*1 cpm(8000)
      character*(*) cmsg
c
      integer*4 IUNIT,MACH,NKPOSM,NCPOSM,NPOSMA
c
      data MACH   /0086/, IUNIT  /0087/, NKPOSM /0171/, NPOSMA /0172/
      data NCPOSM /0173/
c
      integer*4 nc,i,inc
c
c...Save common arrays
c
      do 200 i=1,kpm(NKPOSM),1
          call wrcom (klun,i,kpm((i-1)*2000+1),cmsg,kerr)
          if (kerr .ne. 0) goto 8000
  200 continue
      inc = kpm(NKPOSM) + 1
      nc  = kpm(NKPOSM) + kpm(NCPOSM)
      do 220 i=inc,nc,1
          call wrcom (LUNSC1,i,cpm((i-inc)*8000+1),cmsg,kerr)
          if (kerr .ne. 0) goto 8000
  220 continue
      inc = inc + kpm(NCPOSM)
      nc  = kpm(NKPOSM) + kpm(NCPOSM) + kpm(NPOSMA)
      do 300 i=inc,nc,1
          call wrcom (LUNSC1,i,gpm((i-inc)*1000+1),cmsg,kerr)
          if (kerr .ne. 0) goto 8000
  300 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_edit_mdf (kfl,kerr)
c
c   FUNCTION:  This routine calls mpost to allow the user to edit the
c              input or output machine description.
c
c   INPUT:  kfl     I*4  D1  -  1 = Edit input MDF, 2 = Edit output MDF.
c
c   OUTPUT: kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine ptd_edit_mdf (kfl,kerr)
c
      include 'menu.inc'
      include 'post.inc'
      include 'ptedpost.inc'
c
      equivalence (LMNAME,CPOSMP(0141)), (OLMNAME,OCPOSMP(0141))
c
      character*40 LMNAME,OLMNAME
c
      integer*4 kfl,kerr
c
      character*256  cmsg
c
      integer*4 nc, irecl, strlen1
c
      character*1 newline
      byte nl
c
      equivalence (nl, newline)
      character*256 comln
      data nl /10/
c
      character*80 tempfile
      character*20 att(4),lnum
      character*40 lmach
c
c...Initialize routine
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'write'
      irecl  = 8000
      if (kfl .eq. 1) then
          lmach = LMNAME
      else
          lmach = OLMNAME
      endif
c
c...Open production MDF file
c
cc  100 mnum = mnum - 1
cc      if (mnum.eq.0) goto 9500
      nc     = strlen1(lmach)
      tempfile = 'PWORKS_' // lmach(1:nc) // '.MDF'
      call opnfil (LUNSC1, tempfile,att,irecl,cmsg,kerr)
      if (kerr .eq. -2) then
          att(4) = 'new'
          call opnfil(LUNSC1, tempfile, att, irecl, cmsg, kerr)
      endif
      if (kerr.ne.0) goto 9000
c
c...Save common area
c
      if (kfl .eq. 1) then
          call ptd_savmch (LUNSC1,KPOSMP,POSMAP,CPOSMP,cmsg,kerr)
      else
          call ptd_savmch (LUNSC1,OKPOSMP,OPOSMAP,OCPOSMP,cmsg,kerr)
      endif
      if (kerr .ne. 0) go to 9000
c
c...Call up mpost routine
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      comln = 'mpost '//lnum(1:nc)
C      nc = nc + 6
C      call Csystem(comln, nc, kerr)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      comln = 'mpost '//lmach(1:nc)//newline
      nc = nc + 7
      call Csystem(comln, nc, kerr)
C WNT-END
c
c...use pwmenu routine to load machine
c
      if (kerr .eq. -1) goto 8000
      if (kfl .eq. 1) then
          call ptd_lodmch (lmach,KPOSMP,POSMAP,CPOSMP,0,cmsg,kerr)
      else
          call ptd_lodmch (lmach,OKPOSMP,OPOSMAP,OCPOSMP,0,cmsg,kerr)
      endif
c
c...Delete temporary MDF file
c...this is the file already exist before,
c...don't delete
cc
Cc VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
Cc8000 att(1) = 'direct'
Cc     att(2) = 'none'
Cc     att(3) = 'unformatted'
Cc     att(4) = 'old'
Cc     irecl  = 8000
Cc     call opnfil (LUNSC1, tempfile,att,irecl,cmsg,kerr)
Cc     if (kerr .ne. -2) then
Cc        call clsdel (LUNSC1)
Cc     endif
Cc VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
Cc WNT-START
cc 8000 open (unit=LUNSC1,file=tempfile(1:nc+11),
cc     1      status='OLD', err=9500, DISPOSE = 'DELETE')
cc      close (unit=LUNSC1)
Cc WNT-END
 8000 call clsfil( LUNSC1)
      return
 9000 call clsfil(LUNSC1)
 9500 kerr = 2
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_load_mdf(cfil,kfl,cmsg1,cmsg2,kerr)
c
c   FUNCTION:  This routine loads the input and/or output MDF file.
c
c   INPUT:  cfil    B*256 D1  -  Input machine description filename.
c
c           kfl     I*4   D1  -  1 = Load input, 2 = Output, 3 = Both.
c
c   OUTPUT: kerr    I*4   D1  -  Returns non-zero when an error occurred.
c
c           cmsg1   B*80  D1  -  Error message
c
c           cmsg2   B*80  D1  -  Error message
c
c***********************************************************************
c
      subroutine ptd_load_mdf(cfil,kfl,cmsg1,cmsg2,kerr)
c
      include 'menu.inc'
      include 'post.inc'
      include 'ptedpost.inc'
c
      integer*4 kerr,kfl
c
      byte cfil(MAX_PATH),cmsg1(80),cmsg2(80)
c
      equivalence (TL    ,POSMAP(3601)), (TLNO  ,POSMAP(3841))
c
      real*8 TL(120),TLNO(120)
c
      integer*4 nc,ivnc,irnc,strlen1
c
      real*8 tn(120),tln(120)
c
      character*80 msg1,rmsg,vmsg
      character*(MAX_PATH) ldir,lfil,lext
c
c...Setup filename
c
      call pwdbtc (cfil,LCMPFI,nc)
      call fbreak (LCMPFI,ldir,lfil,lext)
      if (lext .eq. ' ') then
          nc = strlen1(lfil)
          lfil = 'PWORKS_' // lfil(1:nc)
          lext = '.MDF'
      endif
      call fparse (lfil,LCMPFI,ldir,lext)
cc      call ctoi (LCMPFI,inum,ierr)
cc      if (ierr .ne. 0) inum = -2
c
c...Load machine descriptor
c
      if (kfl .eq. 1 .or. kfl .eq. 3) then
          call copyn (TLNO,tn,120)
          call copyn (TL,tln,120)
          call ptd_lodmch ('~',KPOSMP,POSMAP,CPOSMP,1,msg1,kerr)
          call update
          call copyn (tn,TLNO,120)
          call copyn (tln,TL,120)
      else
          call ptdf_copy_mdf (3)
          call ptd_lodmch ('~',KPOSMP,POSMAP,CPOSMP,0,msg1,kerr)
          call update
          call ptdf_copy_mdf (1)
          call ptdf_copy_mdf (5)
      endif
c
c.........Error loading Machine Descriptor File
c
      if (kerr .ne. 0) then
          call pwdctb (msg1,cmsg1)
          if (kerr .eq. -1) then
              call errtxt ('SETTINGS',rmsg)
              call pwdctb (rmsg,cmsg2)
          else
              call errhnd (vmsg,ivnc,rmsg,irnc)
              if (irnc .ne. 0) then
                  call pwdctb (rmsg,cmsg2)
              else if (ivnc .ne. 0) then
                  call pwdctb (vmsg,cmsg2)
              else
                  cmsg2(1) = 0
              endif
          endif
      else
        call ptd_savdef
        if (kfl .eq. 3) call ptdf_copy_mdf (1)
      endif

c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_save_mdf(cfil, kerr)
c
c   FUNCTION:  This routine saves the machine description.
c
c   INPUT:  cfil    C*256    -  Filename
c
c   OUTPUT: kerr    I*4  D1  -  Returns 0 for no error.
c
c***********************************************************************
c
      subroutine ptd_save_mdf(cfil, kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      byte cfil(MAX_PATH)
      character*(MAX_PATH) filn
      character*256  cmsg

      equivalence (NKPOSM, KPOSMP(0171)), (NPOSMA,KPOSMP(0172))
      equivalence (NCPOSM, KPOSMP(0173)), (MACH, KPOSMP(0086))
      equivalence (MPOSMP, CPOSMP(0001))

      integer*4 MACH, NKPOSM, NCPOSM, NPOSMA, MPOSMP(2000)
      character*20 att(4)
      integer*4 inc, i, nc, irecl
      integer*4 kerr
c
      call pwdbtc(cfil, filn, nc)
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'new'
      irecl = 8000
      call opnfil(LUNSC1, filn, att, irecl, cmsg, kerr)
      if (kerr.ne.0) goto 9000
      do 200 i = 1, NKPOSM
          call wrcom (LUNSC1, i, KPOSMP((i-1)*2000+1), cmsg, kerr)
          if (kerr .ne. 0) goto 9000
  200 continue
      inc = NKPOSM + 1
      do 220 i = inc, NKPOSM+NCPOSM
          call wrcom (LUNSC1, i, MPOSMP((i-inc)*2000+1), cmsg, kerr)
          if (kerr .ne. 0) goto 9000
  220 continue
      inc = inc + NCPOSM
      do 300 i = inc, NKPOSM+NCPOSM+NPOSMA, 1
          call wrcom (LUNSC1, i, POSMAP((i-inc)*1000+1), cmsg, kerr)
          if (kerr .ne. 0) goto 9000
  300 continue
      call clsfil(LUNSC1)
      return
 9000 call clsfil(LUNSC1)
      kerr = 2
      return

      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptdf_copy_mdf (kfl)
c
c   FUNCTION:  This routine copies an MDF common to another MDF common.
c
c   INPUT:    kfl     I*4  D1   -  1 = Copy input to output
c                                  2 = Copy output to input
c                                  3 = Copy input to temp
c                                  4 = Copy output to temp
c                                  5 = Copy temp to input
c                                  6 = Copy temp to output
c
c   OUTPUT:   none
c
c***********************************************************************
c
      subroutine ptdf_copy_mdf (kfl)
c
      include 'menu.inc'
      include 'post.inc'
      include 'ptedpost.inc'
c
      integer*4 kfl
c
      equivalence (NKPOSM,KPOSMP(0171)), (NPOSMA,KPOSMP(0172))
      equivalence (NCPOSM,KPOSMP(0173))
c
      equivalence (MPOSMP,CPOSMP), (OMPOSMP,OCPOSMP)
      equivalence (BMPOSMP,BCPOSMP)
c
      integer*4 NKPOSM,NPOSMA,NCPOSM,MPOSMP(2000),OMPOSMP(2000),
     1          BMPOSMP(2000)
c
c...Input to output
c
      if (kfl .eq. 1) then
          call cpymdf (KPOSMP,POSMAP,MPOSMP,OKPOSMP,OPOSMAP,OMPOSMP)
c
c...Output to input
c
      else if (kfl .eq. 2) then
          call cpymdf (OKPOSMP,OPOSMAP,OMPOSMP,KPOSMP,POSMAP,MPOSMP)
c
c...Input to temp
c
      else if (kfl .eq. 3) then
          call cpymdf (KPOSMP,POSMAP,MPOSMP,BKPOSMP,BPOSMAP,BMPOSMP)
c
c...Output to temp
c
      else if (kfl .eq. 4) then
          call cpymdf (OKPOSMP,OPOSMAP,OMPOSMP,BKPOSMP,BPOSMAP,BMPOSMP)
c
c...Temp to input
c
      else if (kfl .eq. 5) then
          call cpymdf (BKPOSMP,BPOSMAP,BMPOSMP,KPOSMP,POSMAP,MPOSMP)
c
c...Temp to output
c
      else if (kfl .eq. 6) then
          call cpymdf (BKPOSMP,BPOSMAP,BMPOSMP,OKPOSMP,OPOSMAP,OMPOSMP)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cpymdf (kpos,gpos,kcpos,kopos,gopos,kcopos)
c
c   FUNCTION:  This routine copies an MDF common to another MDF common.
c
c   INPUT:    kpos    I*4  Dn   -  Integer common (KPOSMP).
c
c             gpos    I*4  Dn   -  Real common (POSMAP).
c
c             kcpos   I*4  Dn   -  Integer representation of character
c                                  common (CPOSMP).
c
c   OUTPUT:   kopos   I*4  Dn   -  Integer common (KPOSMP).
c
c             gopos   I*4  Dn   -  Real common (POSMAP).
c
c             kcopos  I*4  Dn   -  Integer representation of character
c                                  common (CPOSMP).
c***********************************************************************
c
      subroutine cpymdf (kpos,gpos,kcpos,kopos,gopos,kcopos)
c
      include 'post.inc'
c
      integer*4 kpos(*),kcpos(*),kopos(*),kcopos(*)
c
      real*8 gpos(*),gopos(*)
c
      equivalence (NKPOSM,KPOSMP(0171)), (NPOSMA,KPOSMP(0172))
      equivalence (NCPOSM,KPOSMP(0173))
c
      integer*4 NKPOSM,NPOSMA,NCPOSM
c
      integer*4 i
c
c...Copy KPOSMP array
c
      do 100 i=1,NKPOSM*2000,1
          kopos(i) = kpos(i)
  100 continue
c
c...Copy POSMAP array
c
      do 200 i=1,NPOSMA*1000,1
          gopos(i) = gpos(i)
  200 continue
c
c...Copy CPOSMP array
c
      do 300 i=1,NCPOSM*2000,1
          kcopos(i) = kcpos(i)
  300 continue
c
c...End of routine
c
 8000 return
      end
