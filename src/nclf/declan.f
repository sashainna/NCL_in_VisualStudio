C*********************************************************************
C*    NAME         :  declan.f
C*       CONTAINS:
C*             declan  gtanot  showan  prinan
C*
C*    COPYRIGHT 2000 (c) Numerical Control Computer Sceinces Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       declan.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:52
C********************************************************************/
c
c***********************************************************************
c
c   SUBROUTINE:  declan
c
c   FUNCTION:  This routine defines a symbol using the command
c              an1=ANOTE/"text", ...
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine declan
c
      include 'com8a.com'
      include 'wrksys.com'
c
      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold
c
      integer*2 MAXENT
      parameter (MAXENT=50)
c
      integer*2 ietype,nwds,ierr,ilettp,ilintp,isavsb(4),ipfl(3),
     1          istsv,idsv
      integer*4 nclkey,cvkey,isub,iat,pjkey(4),inum,skold,letsub,linsub,
     1          skhld
c
      logical trflg
c
      real*8 pt(6),ptc(6),ptl(6),ptv(6),pval(6)
c
      character*64 sym,letpv,linpv
      character*1024 cnote
      byte lsym(64),lnote(1024)
c
      integer*2 ATV,CURVEV,LINEV,LETTERV,PROJCTV
      parameter (ATV=189)
      parameter (CURVEV=608)
      parameter (LETTERV=1043)
      parameter (LINEV=605)
      parameter (PROJCTV=888)
c
      equivalence (saveid,isavsb)
c
c...Initialize routine
c
      call curnam (sym,isub)
      trflg = .false.
      iat = 1
      ilintp = 0
      ilettp = 0
      ipfl(1) = -1
      cvkey = 0
      pt(1) = 0.
      pt(2) = 0.
      pt(3) = 0.
      pt(4) = 0.
      pt(5) = 0.
      pt(6) = 1.
c
c...ANOTE/"text"
c
      if (lstrng) then
          ix = 0
          cnote = ' '
          call gttext (cnote,ix)
      else
          cnote = token2
      endif
c
c...Parse remaining parameters
c
      do while (nextyp .ne. 11)
c
c......ANOTE/AT
c
          call parsit
          if (ityp .ne. 1) go to 9030
          if (ist .eq. ATV) then
              call parsit
              if (ityp .eq. 1) then
                  if (ist .eq. LETTERV) then
                      iat = 0
                  else if (ist .eq. LINEV) then
                      iat = 1
                  else
                      go to 9010
                  endif
              else if (ityp .eq. 2 .and. (ist .eq. 3 .or. ist .eq. 21))
     1                then
                  iat = 2
                  call gtentt (tv, trflg, nclkey, ietype, pt)
c
c.........Metric conversion
c
                  if (ifl(264) .eq. 1) then
                      pt(1) = pt(1) / 25.4
                      pt(2) = pt(2) / 25.4
                      pt(3) = pt(3) / 25.4
                  endif
c
c.........Modsys
c
                  if (lwrk) call conent (pt,wrkmx,POINT)
c
c.........Refsys
c
cc                  if (ifl(72) .eq. 1) call conent (pt,sc(56),POINT)
              else
                  go to 9010
              endif
c
c......ANOTE/LETTER,pv
c
          else if (ist .eq. LETTERV) then
              call parsit
              if (ityp .ne. 2) go to 9040
              if (ist .eq. 1 .or. (ifl(41) .eq. 1 .and. (ist .eq. 3 .or. 
     1            ist .eq. 21))) then
                  ilettp = 21
                  if (ist .eq. 3) ilettp = 3
                  letpv = token2
                  letsub = ivxsub
              else
                  if (ist .eq. 3 .or. ist .eq. 21) go to 9050
                  go to 9040
              endif
c
c......ANOTE/LINE,pv
c
          else if (ist .eq. LINEV) then
              call parsit
              if (ityp .ne. 2) go to 9040
              if (ist .eq. 1 .or. (ifl(41) .eq. 1 .and. (ist .eq. 3 .or.
     1            ist .eq. 21))) then
                  ilintp = 21
                  if (ist .eq. 3) ilintp = 3
                  linpv = token2
                  linsub = ivxsub
              else
                  if (ist .eq. 3 .or. ist .eq. 21) go to 9050
                  go to 9040
              endif
c
c......ANOTE/CURVE,cv
c
          else if (ist .eq. CURVEV) then
              call parsit
              if (ityp .ne. 2 .or. (ist .ne. 5 .and. ist .ne. 7))
     1            go to 9060
              call gtdesc (tv,cvkey,nwds,ietype)
c
c......ANOTE/PROJCT,sf
c
          else if (ist .eq. PROJCTV) then
              call uaf_get_txt_defstyle (inum)
              if (inum .eq. 0) go to 9090
              call prjprs (3,ipfl,ptv,pval,ierr)
              if (ierr .ne. 0) go to 9070
c
c......Unrecognized minor word
c
          else
              go to 9080
          endif
      enddo
c
c...Define annotation
c
 1000 call ctob (sym,lsym)
      call ctob (cnote,lnote)
      pjkey(1) = 0
      pjkey(2) = 0
      pjkey(3) = 0
      pjkey(4) = 0
      if (ipfl(1) .ne. -1) then
          call gtdesc (ptv(2),pjkey(1),nwds,ietype)
          if (ipfl(1) .eq. 1) call gtdesc (ptv(3),pjkey(2),nwds,ietype)
          if (ipfl(3) .eq. 5) call gtdesc (ptv(5),pjkey(3),nwds,ietype)
          if (ptv(6) .ne. 0) call gtdesc (ptv(5),pjkey(4),nwds,ietype)
      endif
      call uaf_notes (lsym,isub,lnote,iat,pt,ptc,ptl,cvkey,ipfl,pjkey,
     1                pval(3),nclkey,ierr)
      if (ierr .ne. 0) go to 9020
c
c...Set visible flag
c
      if (.not. dsplan) call blkgeo(nclkey,1)
c
c...Store next character/line attach points
c
      if (ilettp .ne. 0 .or. ilintp .ne. 0) then
          skold = keyold
          skhld = keyhld
          istsv = istold
          idsv  = idst
      endif
      if (ilettp .ne. 0) then
          call storit (ptc,letpv,letsub,ilettp,0)
          if (ifl(2) .ne. 0) go to 8000
      endif
      if (ilintp .ne. 0) then
          call storit (ptl,linpv,linsub,ilintp,0)
          if (ifl(2) .ne. 0) go to 8000
      endif
      if (ilettp .ne. 0 .or. ilintp .ne. 0) then
          keyold = skold
          keyhld = skhld
          istold = istsv
          idst   = idsv
      endif
c
c...End of routine
c
      savid2 = sym
      ivxsub = isub
      idst   = 30
      call ptdesc (nclkey,idst,rest)
      ifl(262) = 1
 8000 return
c
c...Text expected
c
 9000 ifl(2) = 513
      err = .true.
      go to 8000
c
c...Position expected
c
 9010 ifl(2) = 525
      err = .true.
      go to 8000
c
c...Could not create entity
c
 9020 ifl(2) = 12
      err = .true.
      go to 8000
c
c...Vocabulary word expected
c
 9030 ifl(2) = 232
      err = .true.
      go to 8000
c
c...Point(-vector) expected
c
 9040 ifl(2) = 526
      err = .true.
      go to 8000
c
c...Identifier already defined
c
 9050 ifl(2) = 8
      err = .true.
      go to 8000
c
c...Line or Circle expected
c
 9060 ifl(2) = 192
      err = .true.
      go to 8000
c
c...Error parsing PROJCT clause
c
 9070 ifl(2) = ierr
      err = .true.
      go to 8000
c
c...Invalid minor word
c
 9080 ifl(2) = 436
      err = .true.
      go to 8000
c
c...Invalid minor word
c
 9090 ifl(2) = 528
      err = .true.
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gtanot (gtv,cstr,klen,klin)
c
c   FUNCTION:  This routine retrieves the text string of an annotation.
c
c   INPUT:  gtv      R*8   D1  -  Annotation descriptor.
c
c           klin     I*2   D1  -  Line number of annotation text to
c                                 return.  0 = Return entire text.
c
c   OUTPUT: cstr     C*n   D1  -  Annotation text.
c
c           klen     I*2   D1  -  Length of string.
c
c***********************************************************************
c
      subroutine gtanot (gtv,cstr,klen,klin)
c
      include 'com8a.com'
c
      integer*2 klin,klen
c
      real*8 gtv
c
      character*(*) cstr
c
      integer*2 nwds,itype,inc,is
      integer*4 nclkey,nc
c
      character*512 cnote
      byte lnote(512)
c
c...Get key of annotation
c
      call gtdesc (gtv,nclkey,nwds,itype)
c
c...Get annotation text
c
      call uaf_get_notes(nclkey,lnote)
      call btoc (lnote,cnote,nc)
c
c...Return annotation text
c
      if (klin .eq. 0) then
        cstr = cnote
        klen = nc
c
c...Return single line of annotation text
c
      else
        is = 1
        inc = 1
        do 100 i=1,nc-1,1
          if (lnote(i) .eq. 10) then
            if (inc .eq. klin) then
              if (i .eq. 1) then
                cstr = ' '
                klen = 0
              else
                cstr = cnote(is:i-1)
                klen = i - is + 1
              endif
              go to 200
            endif
            inc = inc + 1
            is = i + 1
          endif
  100   continue
        cstr = cnote(is:nc)
        klen = nc - is + 1
  200   continue
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  showan (knl)
c
c   FUNCTION:  This routine displays the canonical data of an annotation.
c
c   INPUT:  none.
c
c   OUTPUT: knl      I*2   D1  -  Last line number written to.
c
c***********************************************************************
c
      subroutine showan (knl)
c
      include 'com8a.com'
c
      integer*4 knl
c
      integer*2 nwds,itype
      integer*4 nclkey,nc
c
      character*512 cnote
      byte lnote(512)
c
c...Get key of annotation
c
      call gtdesc (tv,nclkey,nwds,itype)
c
c...Get annotation text
c
      call uaf_get_notes(nclkey,lnote)
      call btoc (lnote,cnote,nc)
c
c...Output note text
c
      knl = 17
      if (nc .gt. 0) then
          write (cout,10) cnote(1:nc)
   10     format ('"',A,'"')
          nci = nc + 2
          call putmsg (cout,nci,knl,0)
          knl = knl + 1
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prinan (kkey,cnam,csub,cref)
c
c   FUNCTION:  This routine prints the canonical data of an annotation.
c
c   INPUT:  kkey     I*4   D1  -  Key of solid entity.
c
c           cnam     C*n   D1  -  Label of annotation.
c
c           csub     C*n   D1  -  Subscript string of annotation.
c
c           cref     C*n   D1  -  Refsys string.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine prinan (kkey,labstr, nc, cref)
c
      include 'com8a.com'
c
      integer*4 kkey
c
      character*(*) labstr,cref
c
      integer*4 nc,nc1,strlen1,ipos,ix,is,ie,ie1,mxtext
c
      character*512 cnote
      byte lnote(512)
c
c...Get annotation text
c
      ix = 10
      call uaf_get_notes (kkey,lnote)
      call btoc (lnote,cnote,nc1)
c
c...Output note text
c
      mxtext = 48
      if (ifl(106).gt.80) then
          mxtext = ifl(106)-32
      endif
      is = 1
      do while (is .lt. nc1)
          ipos = index(cnote(is:),char(ix))
          if (ipos .ne. 0) then
              ie = is + ipos - 2
          else
              ie = nc1
          endif
          ie1 = ie
          if (ie1-is+1 .gt. mxtext) then
              ie1 = is + mxtext - 1
              cnote(ie1:ie1) = '*'
          endif
          if (is .eq. 1) then
              if (nc.le.14) then
                  write (cout,10) labstr(1:14),cref,cnote(is:ie1)
   10             format (a,1x,'anote     ',1x,a5,1x,a)
              else
                  call prtge1 (labstr)
                  write (cout,11) cref, cnote(is:ie1)
   11             format (15x, 'anote     ',1x,a5,1x,a)
              endif
          else
              write (cout,20) cnote(is:ie1)
   20         format (32x,a)
          endif
          call prtge1 (cout)
          is = ie + 2
      enddo
c
c...End of routine
c
 8000 return
      end
