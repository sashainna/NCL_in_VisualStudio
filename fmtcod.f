c
c***********************************************************************
c
c   FILE NAME:  fmtcod
c   CONTAINS:
c           ptd_fmticod (kreg,gval,ddat,knc)
c           fmtocod (kreg,gval,cdat,knc)
c           fmtincod (kreg,gval, oreg, cdat, knc)
c           ptd_gtregv (kreg,gval,cdat, knc, istart, iend, bcase)
c           gtregv (kreg,gval,cdat, knc, istart, iend, bcase)
c           fstrstr(cdat, nc1, cstr, nc2, bcase, indx)
c           ptd_inendchar(endbk)
c           ptd_outendchar(endbk)
c           ptd_inpack(pack)
c           ptd_outpack(pack)
c           ptd_defmtcod (kreg,gval,ddat,knc, bcase, kerr)
c           defmtcod (kreg,gval,cdat,knc, bcase, kerr)
c           ptd_ctocd (cbuf,knc,kreg,gval,kerr)
c           function flstrcmpi(str1, str2, nc)
c           function flstrcmp (str1, str2, nc)
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c       fmtcod.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c       12/09/13 , 11:53:50
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_fmtincod (kreg,gval,cdat,knc)
c
c   FUNCTION:  This routine formats a code, including beginning and
c              trailing characters, to correct letter address
c              it follows input format
c
c   INPUT:  kreg    I*4  D1   -  Register to format for output.  For
c                                kreg < 0 register value, it will find
c                                correct register and give a correct
c                                formated letter address
c
c           gval    R*8  D1   -  Register value.
c
c   OUTPUT: cdat    C*n  D1   -  Text string representation of register
c                                and value.
c
c           oreg                 exact register value (not G, or M code
c                                always > 0
c           knc     I*4  D1   -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine ptd_fmticod (kreg,gval,ddat,knc)

C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     integer*4 kreg, knc
C     real*8 gval
C     character*80 ddat
C     integer*4 oreg
C     call fmtincod (kreg,gval, oreg, ddat,knc)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      integer*4 kreg, knc
      real*8 gval
      byte ddat(80)
      character*80 cdat
      integer*4 oreg
C
      call fmtincod (kreg,gval, oreg, cdat,knc)
      call pwdctb(cdat, ddat)
C WNT-END
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fmtocod (kreg,gval,cdat,knc)
c
c   FUNCTION:  This routine formats a code, including beginning and
c              trailing characters, for output.
c              This function is following fmtcod function in
c              pworks/premsc.f
c
c   INPUT:  kreg    I*4  D1   -  Register to format for output.  For
c                                kreg < 0 register value is not output
c
c           gval    R*8  D1   -  Register value.
c
c   OUTPUT: cdat    C*n  D1   -  Text string representation of register
c                                and value.
c
c           knc     I*4  D1   -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine fmtocod (kreg,gval,cdat,knc)
c
      include 'menu.inc'
      include 'post.inc'
      include 'ptedpost.inc'
c
      equivalence (REGBNC,OKPOSMP(2001))
      equivalence (FMTDES,OKPOSMP(2133)), (REGENC,OKPOSMP(3522))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 REGBNC(MAXFMT),REGENC(MAXFMT)
c
      equivalence (REGST ,OCPOSMP(7011)), (REGEN ,OCPOSMP(9219))
c
      character*24 REGST(MAXFMT),REGEN(MAXFMT)
c
      integer*4 kreg,knc
c
      real*8 gval
c
      character*(*) cdat
c
      character*1 bslash
      character*60 obuf,lbuf
      integer*4 ireg, i,nc
c
C WNT-START
      data bslash /'\'/
C WNT-END
C SGI-SUN-HPX-IBM-START
C     data bslash /'\\'/
C SGI-SUN-HPX-IBM-END

c
c...Format register for output
c
      ireg = kreg
      if (kreg .lt. 0) then
          call gtocdreg (kreg,gval,ireg)
      else
          call ftoc (gval,cdat,knc,FMTDES(1,ireg))
      end if
      if (ireg .lt. 0) then
          cdat = ' '
          knc   = 0
          go to 8000
      endif
c
      do 100 i=1,2,1
          if (i .eq. 1) then
              nc     = REGBNC(ireg)
              if (nc .ne. 0) lbuf   = REGST(ireg)(1:nc)
          else
              nc     = REGENC(ireg)
              if (nc .ne. 0) lbuf   = REGEN(ireg)(1:nc)
          endif
c
          if (nc .ne. 0) then
              if (lbuf(nc:nc) .eq. bslash) then
                  if (nc .gt. 1 .and. lbuf(nc-1:nc-1) .eq. bslash) then
                      nc     = nc     - 1
                  else
                      lbuf(nc:nc) = ' '
                  endif
              endif
c
              if (knc .ne. 0) then
                  if (i .eq. 1) then
                      obuf   = lbuf(1:nc) // cdat(1:knc)
                  else
                      obuf   = cdat(1:knc) // lbuf(1:nc)
                  endif
              else
                  obuf   = lbuf(1:nc)
              endif
              cdat   = obuf
              knc    = knc    + nc
          endif
  100 continue
c
c...End of routine
c
 8000 return
      end

c***********************************************************************
c
c   SUBROUTINE:  fmtincod (kreg,gval,cdat,knc)
c
c   FUNCTION:  This routine formats a code, including beginning and
c              trailing characters, to correct letter address
c              it follows input format
c
c   INPUT:  kreg    I*4  D1   -  Register to format for output.  For
c                                kreg < 0 register value, it will find
c                                correct register and give a correct
c                                formated letter address
c
c           gval    R*8  D1   -  Register value.
c
c   OUTPUT: cdat    C*n  D1   -  Text string representation of register
c                                and value.
c
c           oreg                 exact register value (not G, or M code
c                                always > 0
c           knc     I*4  D1   -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine fmtincod (kreg,gval, oreg, cdat, knc)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGBNC,KPOSMP(2001))
      equivalence (FMTDES,KPOSMP(2133)), (REGENC,KPOSMP(3522))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 REGBNC(MAXFMT),REGENC(MAXFMT)
c
      equivalence (DUMMY ,POSMAP(0003)), (REGBNC,KPOSMP(2001))
c
      real*8 DUMMY
c
      equivalence (REGST ,CPOSMP(7011)), (REGEN ,CPOSMP(9219))
c
      character*24 REGST(MAXFMT),REGEN(MAXFMT)
c
      integer*4 kreg,knc, oreg
c
c
      real*8 gval
c
      character*(*) cdat
c
      character*60 obuf
      integer*4 ireg
c
c...Format register
c
      ireg = kreg
      if (kreg .lt. 0) then
          call gticdreg(kreg, gval, oreg, cdat, knc)
          return
      end if
c
      oreg = kreg
      if (gval.ne.DUMMY) then
          call ftoc (gval,cdat,knc,FMTDES(1,ireg))
      else
          knc = 0
      endif
c
      if (REGBNC(ireg) .ne. 0) then
          if (knc .ne. 0) then
             obuf   = REGST(ireg)(1:REGBNC(ireg)) // cdat(1:knc)
          else
             obuf   = REGST(ireg)(1:REGBNC(ireg))
          end if
          cdat(1:30)   = obuf
          knc    = knc    + REGBNC(ireg)
      endif
c
      if (REGENC(ireg) .ne. 0) then
          if (knc .ne. 0) then
             obuf   = cdat(1:knc) // REGEN(ireg)(1:REGENC(ireg))
          else
             obuf   = REGEN(ireg)(1:REGENC(ireg))
          end if
          knc    = knc    + REGENC(ireg)
          cdat(1:30)   = obuf
      endif
c
c...End of routine
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_gtregv (kreg,gval,cdat, knc, istart, iend, bcase)
c
c   FUNCTION:  This routine gets a register value.
c
c   INPUT:  cdat    C*n  D1   - String containing register.
c           knc     I*4  D1   - Number of characters in cdat.
c           istart  I*4  D1   - starting character of register
c           bcase   I*4  D1   - =1 respect case, =0 ignore case
c
c   OUTPUT: kreg    I*4  D1   - Register number or -1000 if invalid
c           gval    R*8  D1   - Register value.
c           iend    I*4  D1   - Ending character position of register.
c
c***********************************************************************
c
      subroutine ptd_gtregv (kreg,gval,cdat, knc, istart, iend, bcase)

      include 'pted.inc'
      include 'post.inc'

C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     integer*4 kreg,knc, iend, bcase, istart
C     real*8 gval
C     character*256 cdat
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      integer*4 kreg,knc, iend
      real*8 gval
      integer*4 bcase, istart
      character*256 ddat
      byte cdat(256)
      integer*4 nc
C WNT-END
      equivalence (MSGSTI,CPOSMP(0976)),(MSGENI,CPOSMP(1500))
      character*10 MSGSTI, MSGENI
      equivalence (NCBMSGI,KPOSMP(0843)),(NCEMSGI,KPOSMP(0844))
      integer*4 NCBMSGI, NCEMSGI
      integer*4 i2

C WNT-START
      call pwdbtc(cdat, ddat, nc)
      if (NCBMSGI .lt. knc .and.
     x    ddat(1:NCBMSGI) .eq. MSGSTI(1:NCBMSGI)) then
        istart = 1
        kreg = 999
        gval = 0.0d0
        i2 = index(ddat(NCBMSGI+1:knc),MSGENI(1:NCEMSGI))
        if (i2 .gt. 0) then
          iend = i2 + NCBMSGI + NCEMSGI - 1
        else
          iend = knc
        endif
      else
        call codin (ddat(istart:),knc,bcase,0,kreg,gval,iend,
     1              TBLOCK,MINTER)
      endif
C WNT-END
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     if (NCBMSGI .lt. knc .and.
C    x    cdat(1:NCBMSGI) .eq. MSGSTI(1:NCBMSGI)) then
C       istart = 1
C       kreg = 999
C       gval = 0.0d0
C       i2 = index(cdat(NCBMSGI+1:knc),MSGENI(1:NCEMSGI))
C       if (i2 .gt. 0) then
C         iend = i2 + NCBMSGI + NCEMSGI - 1
C       else
C         iend = knc
C       endif
C     else
C       call codin (cdat(istart:), knc, bcase,0,kreg,gval,iend,
C    1              TBLOCK,MINTER)
C     endif
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  gtregv (kreg,gval,cdat,knc, start, end, bcase)
c
c   FUNCTION:  This routine parse string, it may include
c              more than one input letter address, including
c              beginning and trailing characters and register
c              value, and return Letter address' end position,
c              Register Number and Value.
c
c   INPUT:   cdat    C*n  D1   -  Text string representation of register
c                                 and value. The string
c                                 may include more than one
c                                 letter address
c
c            knc     I*4  D1   -  Number of characters in 'cdat'.
c   OUTPUT:  kreg    I*4  D1   -  Register Number.
c
c            gval    R*8  D1   -  Register value.
c            iend    I*4  D1   -  Letter address end position
c
c
c***********************************************************************
c
      subroutine gtregv (kreg,gval,cdat, knc, istart, iend, bcase)
c
      include 'post.inc'
      include 'menu.inc'
c
      equivalence (REGBNC,KPOSMP(2001))
      equivalence (FMTDES,KPOSMP(2133)), (REGENC,KPOSMP(3522))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 REGBNC(MAXFMT),REGENC(MAXFMT)
c
      equivalence (REGST ,CPOSMP(7011)), (REGEN ,CPOSMP(9219))
c
      character*24 REGST(MAXFMT),REGEN(MAXFMT)
c
      integer*4 kreg,knc, iend, istart, bcase
      real*8 gval
      character*(*) cdat
c
      integer*4 i, m, n, kerr, vlen, pos, indx, next, isnumchar
      integer*4 matched, flstrcmp, flstrcmpi
      character*1 c
      character*20 creg
      character*60 bbuf, ebuf, cval
      character*80 cmsg


      kerr = 1
      istart = 0
  200 istart = istart + 1
      if (istart.gt.(knc-1)) goto 750
c
c.....try to match the begin letter
c
      do 700 i=1, MAXFMT
         if (REGBNC(i).eq.0) goto 310
         if (knc.le.REGBNC(i)) goto 650
         bbuf = REGST(i)(1:REGBNC(i))
         ebuf = REGEN(i)(1:REGENC(i))
         if (bcase.eq.1) then
            matched = flstrcmp(bbuf, cdat(istart:), REGBNC(i))
         else
            matched = flstrcmpi(bbuf, cdat(istart:), REGBNC(i))
         endif
         if (matched.ne.1) goto 650
  310    next = REGBNC(i)+istart
c
c...next character of begin character
c...must be numerical character
c
         if (isnumchar(cdat(next:next)).ne.1) goto 650
c
c.....try to match the end letter
c.....if no end character, break at the next non numerical chars
c....."0123456789+-." or " " or end of this string
c
         if (REGENC(i).eq.0) then
            do 320 pos = next, knc
               c = cdat(pos:pos)
               if ((isnumchar(c).eq.0).or. (c.eq.' ')) then
                  indx = pos
                  goto 500
               endif
  320       continue
c
c...end of string
c
            indx = knc + 1
         else
            indx = next
            call fstrstr(cdat, knc, ebuf, REGENC(i), bcase, indx)
            if (indx.eq.-1) goto 650
         endif
c
c...must have value
c
  500    vlen = indx - istart - REGBNC(i)
         if (vlen.eq.0) goto 650
c
c.....see if reg value is in correct format
c
         m = 1
         do 600 pos = next, indx-1
            cval(m:m) = cdat(pos:pos)
            m = m  + 1
  600    continue
         do 620 n = m, 30
             cval(n:n) = ' '
  620    continue
         kerr = 0
         call ctof(cval, gval, FMTDES(1,i), kerr, cmsg)
  650    if (kerr.eq.0) goto 800
  700 continue
      if (kerr.ne.0)  goto 200
  750 kreg = -1000
      iend = -1
      return
  800 kreg = i
      istart = istart - 1
      iend = REGENC(i) + indx-1
      creg = REGID(i)
      return
      end
c***********************************************************************
c
c   E-FUNCTION:   isnumchar(cchar)
c
c   FUNCTION:  This routine check if cchar is a numerical characters
c              "0123456789+-."
c
c   INPUT:  cstr    C*n  D1  -  Character string to determine the length
c                               of.
c
c   OUTPUT: strlen  I*4  D1  -  Length of 'cstr'.
c
c***********************************************************************
c
      integer*4 function isnumchar(cchar)
c
      character cchar
      character*13 number
      integer*4 i
c
      data number /'0123456789+-.'/
      do 200 i=1,13
         if (cchar.eq.number(i:i)) then
            isnumchar = 1
            return
         endif
  200 continue
      isnumchar = 0
      return
      end
c
c***********************************************************************
c
c   subroutine  fstrstr(cdat, nc1, cstr, nc2, bcase, indx)
c
c   FUNCTION:  locates the first occurrence in string cdat
c              in string cstr, search begin at position indx.
c
c   INPUT:  cdat    C*n  D1   -  Text string search in
c           nc1:    I*4  D1   -  length of cdat
c           cstr    C*n  D1   -  Text string search for
c           nc2:    I*4  D1   -  length of cstr
c           indx:   I*4  D1   -  begin search  position
c
c   OUTPUT: indx:   I*4  D1   -  first occurrence position
c
c***********************************************************************
c
      subroutine fstrstr(cdat, nc1, cstr, nc2, bcase, indx)

      character*(*) cdat, cstr
      integer*4 nc1, nc2, bcase, indx, inc
      integer*4 matched, flstrcmp, flstrcmpi

      inc = indx
  100 if (inc.gt.(nc1-nc2+1)) goto 300

      if (bcase.ne.1) then
         matched = flstrcmp(cdat(inc:inc+nc2-1), cstr, nc2)
      else
         matched = flstrcmpi(cdat(inc:inc+nc2-1), cstr, nc2)
      endif
      if (matched.ne.1) goto 200
      inc = inc + nc2
      goto 100
  200 indx = inc
      return
  300 indx = -1
      return
      end
c
c***********************************************************************
c
c   subroutine  ptd_inendchar(endbk)
c
c   FUNCTION:  returns the end of block character.
c
c   INPUT:  none
c
c   OUTPUT: endbk:   B*5  D1   -  end of block character.
c
c***********************************************************************
c
      subroutine ptd_inendchar(endbk)

      include 'post.inc'
      include 'menu.inc'

      byte endbk(5)
      character*5 LEOB
      integer*4 NEOB
      equivalence (LEOB  ,CPOSMP(0971)), (NEOB, KPOSMP(0841))
      call pwdctb (LEOB,endbk)
      return
      end
c
c***********************************************************************
c
c   subroutine  ptd_outendchar(endbk)
c
c   FUNCTION:  returns the output MDF end of block character.
c
c   INPUT:  none
c
c   OUTPUT: endbk:   B*5  D1   -  end of block character.
c
c***********************************************************************
c
      subroutine ptd_outendchar(endbk)

      include 'ptedpost.inc'
      include 'menu.inc'

      byte endbk(5)
      character*5 LEOB
      integer*4 NEOB
      equivalence (LEOB  ,OCPOSMP(0971)), (NEOB, OKPOSMP(0841))
      call pwdctb (LEOB,endbk)
      return
      end
c
c***********************************************************************
c
c   subroutine  ptd_inpack(pack)
c
c   FUNCTION:  returns the input MDF packed flag.
c
c   INPUT:  none
c
c   OUTPUT: pack:   I*4  D1   -  packed flag.
c
c***********************************************************************
c
      subroutine ptd_inpack(pack)

      include 'post.inc'
      integer*4 pack
c
c...punch file packed/unpacked
c
      equivalence (IPACKD,KPOSMP(1101))
      integer*4 IPACKD

      pack = IPACKD
      return
      end
c
c***********************************************************************
c
c   subroutine  ptd_outpack(pack)
c
c   FUNCTION:  returns the output MDF packed flag.
c
c   INPUT:  none
c
c   OUTPUT: pack:   I*4  D1   -  packed flag.
c
c***********************************************************************
c
      subroutine ptd_outpack(pack)

      include 'ptedpost.inc'
      include 'menu.inc'
      integer*4 pack
c
c...punch file packed/unpacked
c
      equivalence (IPACKD,OKPOSMP(1101))
      integer*4 IPACKD

      pack = IPACKD
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  ptd_defmtcod (kreg,gval,cdat,knc, bcase, kerr)
c
c   FUNCTION:  This routine parse a letter address, it include
c              beginning and trailing characters and register
c              value, and return Register Number and Value.
c
c   INPUT:  cdat    C*n  D1   -  Text string representation of register
c                                and value.
c           knc     I*4  D1   -  Number of characters in 'cdat'.
c
c   OUTPUT: kreg    I*4  D1   -  Register Number.
c           gval    R*8  D1   -  Register value.
c           kerr    I*4  D1   -  0: valid letter address, return
c                                valid kreg,gval
c                                1: valid letter address, but value is not
c                                in correct format
c                                -1: not a letter address
c
c***********************************************************************
c
      subroutine ptd_defmtcod (kreg,gval,ddat,knc, bcase, kerr)

C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     integer*4 kreg, knc, kerr, bcase
C     real*8 gval
C     character*200 ddat
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      integer*4 kreg, knc, kerr
      real*8 gval
      integer*4 bcase
      byte ddat(200)
      character*200 cdat
      integer*4 nc
C
      call pwdbtc(ddat, cdat, knc)
      call defmtcod (kreg,gval,cdat,knc, bcase, kerr)
C
      return
C WNT-END
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      call defmtcod (kreg,gval,ddat,knc, bcase, kerr)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END

      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  defmtcod (kreg,gval,cdat,knc, bcase, kerr)
c
c   FUNCTION:  This routine parse a letter address, it include
c              beginning and trailing characters and register
c              value, and return Register Number and Value.
c
c   INPUT:  cdat    C*n  D1   -  Text string representation of register
c                                and value.
c           knc     I*4  D1   -  Number of characters in 'cdat'.
c
c   OUTPUT: kreg    I*4  D1   -  Register Number.
c           gval    R*8  D1   -  Register value.
c           kerr    I*4  D1   -  0: valid letter address, return
c                                valid kreg,gval
c                                1: valid letter address, but value is not
c                                in correct format
c                                -1: not a letter address
c
c***********************************************************************
c
      subroutine defmtcod (kreg,gval,cdat,knc, bcase, kerr)
c
      include 'post.inc'
      include 'menu.inc'
c
      equivalence (REGBNC,KPOSMP(2001))
      equivalence (FMTDES,KPOSMP(2133)), (REGENC,KPOSMP(3522))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 REGBNC(MAXFMT),REGENC(MAXFMT)
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      equivalence (REGST ,CPOSMP(7011)), (REGEN ,CPOSMP(9219))
c
      character*24 REGST(MAXFMT),REGEN(MAXFMT)
c
      integer*4 kreg,knc, bcase, kerr
      real*8 gval
      character*(*) cdat
c
      integer*4 i, m, n, matched, flstrcmp, flstrcmpi, savreg
      integer*4 pos, indx, next, isnumchar
      character*1 c
      character*20 creg
      character*60 bbuf, ebuf, cval, savval
      character*80 cmsg

      savreg = -1
      kerr = 1
c
c.....try to match the begin letter
c
      do 700 i=1, MAXFMT
         if (REGBNC(i).eq.0) goto 310
          if (knc.lt.REGBNC(i)) goto 650
          bbuf = REGST(i)(1:REGBNC(i))
          ebuf = REGEN(i)(1:REGENC(i))
      if (bcase.eq.1) then
         matched = flstrcmp(bbuf, cdat, REGBNC(i))
      else
         matched = flstrcmpi(bbuf, cdat, REGBNC(i))
      endif
          if (matched.ne.1) goto 650
  310    next = REGBNC(i)+1
c
c...next character of begin character
c... must be numerical character
c
c.....changed
c.....correct register with no value
c.....set error = 0 but value is DUMM
c.....12/09/99
c         if (isnumchar(cdat(next:next)).ne.1) goto 650
          if (isnumchar(cdat(next:next)).ne.1) then
               cval(1:30) = ''
               kerr = 0
               goto 630
          endif
c
c.....try to match the end letter
c.....if no end character, break at the next non numerical chars
c....."0123456789+-." or end of this string
c
         if (REGENC(i).eq.0) then
         do 320 pos = REGBNC(i)+1, knc
            c = cdat(pos:pos)
            if (isnumchar(c).eq.0) then
c
c...cdat only has one letter address
c...so if we can find a character is not
c...a numerical character, continue search
c
               goto 650
            endif
  320    continue
c
c...end of string
c
         indx = knc + 1
         else
             indx = REGBNC(i)
             call fstrstr(cdat, knc, ebuf, REGENC(i), bcase, indx)
             if (indx.eq.-1) goto 650
             if (indx.le.knc) goto 650
         endif
c
c...must have value
c
  500    if (indx-REGBNC(i).eq.1) goto 650
c
c.....see if reg value is in correct format
c
         m = 1
         do 600 pos = REGBNC(i)+1, indx-1
            cval(m:m) = cdat(pos:pos)
            m = m  + 1
  600    continue
         do 620 n = m, 30
             cval(n:n) = ' '
  620    continue
         kerr = 0
         call ctof(cval, gval, FMTDES(1,i), kerr, cmsg)
  630    if (kerr.eq.0) then
             call chkreg(i, gval, cdat, knc, bcase, kerr)
             if (kerr.eq.0) then
c
c...if all register not exactly match, let correct regiter with
c...no value overwrite the wrong register
c
                if (cval.ne.'') then
                    goto 800
                else
                    savreg = i
                    savval = cval
                endif
             else if ((kerr.ne.-1).and.(savreg.eq.-1)) then
c
c...not exactly mactch some special code/register
c...get the first match register
c...if kerr = -1, discard this register, continue serach
c
                savreg = i
                savval = cval
             endif
         endif
  650    kerr = 1
  700 continue
c
c...we found match register but have not convert value
c...because these register do not match the specific register
c
      if (savreg.ne.-1) then
          kreg = savreg
          kerr = 0
          if (cval.ne.'') then
              call ctof(savval, gval, FMTDES(1,kreg), kerr, cmsg)
          else
              gval = DUMMY
              kerr = 2
          endif
          return
      endif
      kreg = -1000
      kerr = -1
      return
  800 kreg = i
      creg = REGID(i)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_ctocd (cbuf,knc,kreg,gval,kerr)
c
c   FUNCTION:  This routine is the C to fortran interface for ctocd()
c              which converts a text representation of a register
c              into a register number and value.
c
c   INPUT:  cbuf    C*n  D1   -  Text string representation of register
c                                and value.
c           knc     I*4  D1   -  Number of characters in 'cbuf'.
c
c   OUTPUT: kcod    I*4  D1   -  Register Number.
c           gval    R*8  D1   -  Register value.
c           kerr    I*4  D1   -  Returns 1 if an error occurs.
c
c***********************************************************************
c
      subroutine ptd_ctocd (cbuf,knc,kreg,gval,kerr)

C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     integer*4 knc,kreg,kerr
C     real*8 gval
C     character*200 cbuf
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      integer*4 kreg, knc, kerr
      real*8 gval
      byte cbuf(200)
      character*200 cdat
      integer*4 nc
C
      call pwdbtc(cbuf, cdat, knc)
      call ctocd (cdat, knc, kreg,gval, kerr)
      return
C WNT-END
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      call ctocd (cbuf, knc, kreg,gval, kerr)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  function flstrcmpi(str1, str2, nc)
c
c   FUNCTION:  Compare 2 strings, ignoring case.
c
c   INPUT:  str1    C*n  D1   -  First string.
c           str2    C*n  D1   -  second string.
c           nc      I*4  D1   -  Number of characters to compare
c
c   OUTPUT: none
c
c   RETURNS:  1 if strings compare, else 0.
c
c***********************************************************************
c
      integer*4 function flstrcmpi(str1, str2, nc)

      integer*4 nc
      character*(*) str1, str2
      integer*4 ic, i
      character csav

      i = 1
  100 if (i.gt.nc) goto 300
      if (str1(i:i).ne.str2(i:i)) then
        ic = ichar(str1(i:i))
        csav = str1(i:i)
        if (ic .ge. ichar('a') .and. ic .le. ichar('z')) then
           str1(i:i) = char(ic-32)
        else if (ic .ge. ichar('A') .and. ic .le. ichar('Z')) then
           str1(i:i) = char(ic+32)
        endif
        if (str1(i:i).ne.str2(i:i)) then
           str1(i:i) = csav
           goto 200
        endif
        str1(i:i) = csav
      endif
      i = i+1
      goto 100
  200 flstrcmpi = 0
      return
  300 flstrcmpi = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  function flstrcmp(str1, str2, nc)
c
c   FUNCTION:  Compare 2 strings.
c
c   INPUT:  str1    C*n  D1   -  First string.
c           str2    C*n  D1   -  second string.
c           nc      I*4  D1   -  Number of characters to compare
c
c   OUTPUT: none
c
c   RETURNS:  1 if strings compare, else 0.
c
c***********************************************************************
c
      integer*4 function flstrcmp(str1, str2, nc)

      integer*4 nc
      character*(*) str1, str2
      integer*4 i

      i = 1
  100 if (i.gt.nc) goto 300
      if (str1(i:i).ne.str2(i:i)) goto 200
      i = i+1
      goto 100
  200 flstrcmp = 0
      return
  300 flstrcmp = 1
      return
      end
