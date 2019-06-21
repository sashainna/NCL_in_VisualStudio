c
c***********************************************************************
c
c   FILE NAME: mpost.f
c   CONTAINS:
c               npw_close  npw_get_form    npw_get_help  npw_get_menu
c               npw_init   npw_put_answer  npw_put_form
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        mpost.f , 26.2
c     DATE AND TIME OF LAST  MODIFICATION
c        07/23/18 , 13:47:21
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  npw_close
c
c   FUNCTION:  C to Fortran interface routine.  Closes all open files.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine npw_close
c
      call clsfil (0)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  npw_get_form(knrec,crec,knfld,ktfld,kfst,kfen,kftyp,
c                             cflab,knchc,kdchc,chcstr)
c
c   FUNCTION:  C to Fortran interface routine.  Retrieves the layout for
c              the specified Form Menu.  The layout should already be
c              defined using the 'npw_get_menu' routine.  This routine
c              simply returns the global variables previously setup.
c
c   INPUT:  None.
c
c   OUTPUT: knrec   I*4  D1  -  Number of records in this form (ex. MAXFMT
c                               for Tape Registers).
c
c           crec    I*4  D1  -  Text of each record.
c
c           knfld   I*4  D1  -  Number of fields per record.
c
c           ktfld   I*4  D1  -  Total number of fields in form.
c
c           kfst    I*4  D20 -  Starting position within 'crec' of each
c                               field.
c
c           kfen    I*4  D20 -  Ending position within 'crec' of each
c                               field.
c
c           kftyp   I*4  D20 -  Type of field: 1 = Push Button, 2 =
c                               Multiple choice, 3 = Label only, 4 =
c                               Text string, 5 = Dynamic Multiple choice
c
c           cflab   C*1  D20 -  Label for each field.
c
c           knchc   I*4  D20 -  Number of choices per field (kftyp = 2).
c
c           kdchc   I*4  D20.MAXFMT - Default choice for field (kftyp = 2)
c
c           chcstr  C*1  D10.20 - Text of choices (kftyp = 2)
c
c***********************************************************************
c
      subroutine npw_get_form(knrec,crec,knfld,ktfld,kfst,kfen,kftyp,
     1                        cflab,knchc,kdchc,chcstr)
c
c
      include 'menu.inc'
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-START
      integer*4 knrec,knfld,kfst(20),kfen(20),kftyp(20),knchc(20),
     1          kdchc(20,MAXFMT),ktfld
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-END
c
c...CHCSTR(#chars,choices,choices_per_record,records)
c
c...extend buffer size
c...Yurong
c...      byte crec(80,66),cflab(20,20),chcstr(10,10,20)
      byte crec(132,MAXFMT),cflab(20,20),chcstr(20,20,60)
c
      integer*4 i,j
c
c...Return form parameters
c
      knrec  = FRMREC
      knfld  = FRMNFD
      ktfld  = FRMWID
      do 100 i=1,knfld,1
          kfst(i) = FRMST(i) - 1
          kfen(i) = FRMEN(i) - 1
          kftyp(i) = FRMTYP(i)
          call pwdctb (FRMLAB(i),cflab(1,i))
          if (kftyp(i) .eq. 2 .or. kftyp(i) .eq. 5) then
              knchc(i) = FRMNCH(i)
              do 75 j=1,knrec,1
                  kdchc(i,j) = FRMDCH(i,j)
   75         continue
              do 80 j=1,knchc(i),1
                  call pwdctb (FRMCHC(j,i),chcstr(1,j,i))
   80         continue
          endif
  100 continue
      do 400 i=1,knrec,1
          call pwdctb (FRMBUF(i),crec(1,i))
  400 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  npw_get_help (cprm,cbuf,knc,kflag,cmsg,kerr)
c
c   FUNCTION:  C to Fortran interface routine.  Gets the Help Text for
c              a specific menu.  Returns it one line at a time.  This
c              routine must be called multiple times to get all Help
c              Text.
c
c   INPUT:  cprm    C*1  Dn  -  Prompt level for help.
c
c           kflag   I*4  D1  -  Set to 0 upon first entry.  Set to 1
c                               on each subsequent entry.
c
c   OUTPUT: cbuf    C*1  Dn  -  Help text.
c
c           knc     I*4  D1  -  Number of chars in 'cbuf'.
c
c           cmsg    C*1  Dn  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns -1 when end of Help Text is
c                               reached, and non-zero for any other
c                               error.
c
c***********************************************************************
c
      subroutine npw_get_help (cprm,cbuf,knc,kflag,cmsg,kerr)
c
      include 'menu.inc'
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-START
      integer*4 knc,kflag,kerr
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-END
C WNT-START
      integer*4 temp
C WNT-END
      character*1 endchr
      byte endbyte
      equivalence (endchr, endbyte)
c
      byte cprm(20),cbuf(80),cmsg(80)
c
      integer*4 ilev(10),nc,nlev
c
      character*20 sbuf, tmp
c
c...Get Help index record
c
      call pwdbtc (cprm,sbuf,nc)
      call asclev (sbuf,nc,ilev,nlev,kerr)
      if (kerr .ne. 0) then
c
c...changed for VAX
c...Yurong
c
c...          call pwdctb ("Invalid window ID returned.",cmsg)
          tmp = 'Invalid window ID returned.'
          call pwdctb (tmp,cmsg)
          kerr   = 1
          go to 8000
      endif
c
c...Get the Help text
c
      IMHFLG = kflag
      call helpi (sbuf,ilev(1),kerr)
c
c...Set up the return parameters
c
      call pwdctb (MHLPBF,cbuf)
      if (MHLPNC .eq. 80) then
c
c...pwdctb routine only have 79 real character returns
c...so if we have 80 character return, assign it ourselives
c
          endchr = MHLPBF(80:80)
          cbuf(80) = endbyte
      endif

C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     knc    = MHLPNC
C     call pwdctb (MFERTX,cmsg)
C     kflag  = IMHFLG
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      temp    = MHLPNC
      call pwdctb (MFERTX,cmsg)
      kflag  = IMHFLG
      knc = temp
C WNT-END
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  npw_get_menu(klevl,knlev,knbut,cpr,kbid,kbtyp,cbdef,
c                             knchc,kbdchc,chcstr,ctitle,cwid,kwtyp,
c                             cmsg,kerr)
c
c   FUNCTION:  C to Fortran interface routine.  Retrieves the layout for
c              the specified Prompt Menu.  The layout should already be
c              defined using the 'npw_get_menu' routine.  This routine
c              simply returns the global variables previously setup.
c
c   INPUT:  klevl   I*4  D10 -  Menu level of Prompts.
c
c           knlev   I*4  D1  -  Depth of 'klevl'.
c
c   OUTPUT: knbut   I*4  D1  -  Number of entries in Form.
c
c           cpr     C*1  D60 -  Text of buttons/prompts.
c
c           kbid    I*4  D60 -  Button/prompt number.
c
c           kbtyp   I*4  D60 -  Type of button.  1 = Push Button,
c                               2 = Multiple choice, 3 = Label only,
c                               4 = Text string, 5 = Dynamic Multiple
c                               choice.
c
c           cbdef   C*1  D60 -  Default answer for prompt (kbtyp = 4).
c
c           knchc   I*4  D60 -  Number of choices (kbtyp = 2).
c
c           kbdchc  I*4  D60 -  Default choice for prompt (kbtyp = 2).
c
c           chcstr  C*1  D10.60 - Text of choices (kftyp = 2).
c
c           ctitle  C*1  Dn  -  Title of window
c
c           cwid    C*1  Dn  -  ID of window (1.2).
c
c           kwtyp   I*4  D1  -  Type of window.  1 = Push Buttons,
c                               2 = Prompt, 3 = Form.
c
c           cmsg    C*1  Dn  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero on error.
c
c***********************************************************************
c
      subroutine npw_get_menu(klevl,knlev,knbut,cpr,kbid,kbtyp,cbdef,
     1                        knchc,kbdchc,chcstr,ctitle,cwid,kwtyp,
     2                        cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      integer*4 klevl(10),knlev,knbut,kwtyp,kbtyp(60),kbid(60),
     1          knchc(60),kerr,kbdchc(60)
c
c...extend buffer size
c...Yurong
c...      byte cpr(80,60),cbdef(80,60),chcstr(10,10,60),ctitle(80),cwid(10),
c...     1     cmsg
      byte cpr(80,60),cbdef(80,60),chcstr(20,20,60),ctitle(80),cwid(10),
     1     cmsg(82)
c
      integer*4 i,isav,nc,nprm
c
      character*12 dat,pgm
      character*80 sopt,sbuf
c
      byte lopt(80),lpgm(10),ldat(12)
      equivalence (sopt,lopt), (pgm,lpgm), (dat,ldat)
c
      data sopt /'LATHE2,MLATHE,MILL3,MILL4,MILL5,MILL7'/
c
      integer*4 j,ipt,itxt,ntxt
c
c...Initialize routine
c
      kerr   = 0
      do 100 i=1,knlev,1
          MLEVL(i) = klevl(i)
  100 continue
      NLEVL  = knlev
      IMSCAN = 1
c
c...Initialize prompt data
c
      do 200 i=1,60,1
          JBTACT(i) = 0
          JBTYPE(i) = 0
  200 continue
c
c...Call the main menu program
c
      IMTYP  = 0
      isav   = NLEVL
      call master
c      if (IMTYP .ne. 1) NLEVL = NLEVL + 1
c          if (isav .eq. 1 .and. IMTYP .eq. 2 .and. NLEVL .gt. 1)
c     1        NLEVL = NLEVL - 1
      if (IMTYP .eq. 2) then
          NLEVL = isav   + 1
      else if (IMTYP .eq. 0) then
          NLEVL  = 1
          IMENDT(1,1) = 0
      endif

c      if (IMTYP .ne. 1) NLEVL = isav
c
c...Return menu structure
c
      knbut  = 0
      nprm   = IMENDT(1,NLEVL) - 1
c      if (IMTYP .eq. 1) nprm = nprm - 1
      do 500 i=1,nprm,1
          if (JBTACT(i) .eq. 1 .or. IMTYP .eq. 1) then
              knbut  = knbut  + 1
c
c......Check for extra text messages
c
              if ((JBTYPE(i) .eq. 4 .or. JBTYPE(i) .eq. 2 .or.
     1            JBTYPE(i) .eq. 5) .and. IPRDAT(1,i+1) .gt. 1) then
                  ntxt   = IPRDAT(1,i+1)
                  ipt    = IPRDAT(2,i+1)
                  nc     = IPRTXT(ipt)
                  do 430 j=2,ntxt,1
                      ipt    = ipt    + (nc-1) / 4 + 2
                      itxt   = (ipt-1) * 4 + 5
                      nc     = IPRTXT(ipt)
                      sbuf   = '     ' // LPRTXT(itxt:itxt+nc-1)
                      call pwdctb (sbuf,cpr(1,knbut))
                      kbtyp(knbut) = 3
                      kbid(knbut) = 0
                      knbut   = knbut   + 1
  430             continue
              endif
c
c......Store prompt data
c
              ipt    = IPRDAT(2,i+1)
              itxt   = (ipt-1) * 4 + 5 - 1
              call pwdctb(lprtxt(itxt+1:itxt+iprtxt(ipt)),cpr(1,knbut))
              kbid(knbut) = i
c
c......Menu button type
c
              if (IMTYP .eq. 1) then
                  kbtyp(knbut) = 1
c
c......Prompting field type
c
              else
                  kbtyp(knbut) = JBTYPE(i)
                  call pwdctb (SBDEF(i),cbdef(1,knbut))
c
c.........Store valid selections for Choice Field
c
                  if (JBTYPE(i) .eq. 2 .or. JBTYPE(i) .eq. 5) then
                      knchc(knbut) = JBNCHC(i)
                      kbdchc(knbut) = JBDCHC(i)
                      do 450 j=1,JBNCHC(i),1
                          call pwdctb (SBCDEF(j,i),chcstr(1,j,knbut))
  450                 continue
                  endif
              endif
          endif
  500 continue
c
c...Return window parameters
c
      kwtyp  = IMTYP
      NLEVL  = knlev  + 1
      call disban
      call pwdctb (SAPRM(IBANPT),ctitle)
c      if (NLEVL .eq. 1) then
c          sbuf = '0'
c      else
          call levasc (klevl,knlev,sbuf)
c      endif
      call pwdctb (sbuf,cwid)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  npw_init (cprt,ctitle,cmbuf,cmsg,kerr)
c
c   FUNCTION:  C to Fortran interface routine.  Initializes the Motif
c              version of MakePost.
c
c   INPUT:  cmbuf   C*1  Dn  -  Runtime command line.
c
c   OUTPUT: cprt    C*1  Dn  -  Copyright notice.
c
c           ctitle  C*1  Dn  -  Window title for top level Menu.
c
c           cmsg    C*1  Dn  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero on error.
c
c***********************************************************************
c
      subroutine npw_init (cprt,ctitle,cmbuf,cmsg,kerr)
c
      include 'menu.inc'
c
      integer kerr
c
      byte cprt(82),ctitle(82),cmbuf(MAX_PATH+80),cmsg(82)
c
      integer*4 iwrn
c
      character*80 msg,wmsg
c
c...Store MCR buffer
c
      call pwdbtc (cmbuf,MCRBUF,MCRNC)
c
c...Initialize program
c
      iwrn   = 0
      MOTIF  = 1
      call mninit (msg,kerr)
      if (kerr .eq. -1) then
          iwrn   = 1
          wmsg   = msg
          kerr   = 0
      endif
      if (kerr .ne. 0) go to 8000
c
c...Get window title
c
      call disban
      call pwdctb (SAPRM(IBANPT),ctitle)
c
c...Get Copyright notice
c
      call getcpr (msg)
      call pwdctb (msg,cprt)
c
c...End of routine
c
 8000 if (iwrn .eq. 1) then
          kerr   = -1
          msg    = wmsg
      endif
      call pwdctb (msg,cmsg)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  npw_put_answer(cwid,knbut,kbid,cans,cmsg,kpr,kerr)
c
c   FUNCTION:  C to Fortran interface routine.  Returns the answers to
c              a Prompt Menu.
c
c   INPUT:  cwid    C*1  Dn  -  Menu level which contains prompts.
c
c           knbut   I*4  D1  -  Number of prompts in menu.
c
c           kbid    I*4  D60 -  Prompt numbers for answers.
c
c           cans    C*1  D60 -  Answers for prompts.
c
c   OUTPUT: cmsg    C*1  Dn  -  Text of error message.
c
c           kpr     I*4  D1  -  Prompt number which caused error.
c
c           kerr    I*4  D1  -  Returns non-zero on error.
c
c***********************************************************************
c
      subroutine npw_put_answer(cwid,knbut,kbid,cans,cmsg,kpr,kerr)
c
      include 'menu.inc'
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-START
      integer*4 kbid(MAXFMT),kerr,kpr,knbut
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-END
c
      byte cwid(10),cans(132,MAXFMT),cmsg(80)
c
      integer*4 nc,ilev,i
c
      character*80 sbuf, tmp
c
c...Initialize routine
c
      kerr   = 0
      IMSCAN = 2
      MFERR  = 0
c
c...Define main level above actual prompting level
c
      call pwdbtc (cwid,sbuf,nc)
      call asclev (sbuf,nc,MLEVL,ilev,kerr)
      if (kerr .ne. 0) then
c
c...changed for VAX
c...Yurong
c
c          call pwdctb ("Invalid window ID returned.",cmsg)
          tmp = 'Invalid window ID returned.'
          call pwdctb (tmp, cmsg)
          kpr    = 0
          go to 8000
      endif
c
c...Loop thru answers
c
      do 500 i=1,knbut,1
          if (kbid(i) .ne. 0) then
              NLEVL  = ilev    + 1
              MLEVL(NLEVL) = kbid(i)
              call pwdbtc (cans(1,i),MFANS,MFKNC)
              call master
              if (MFERR .ne. 0) then
                  call pwdctb (MFERTX,cmsg)
                  kpr    = i      - 1
                  kerr   = MFERR
                  go to 8000
              endif
          endif
  500 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  npw_put_form(cwid,cans,knbut,cmsg,kpr,kerr)
c
c   FUNCTION:  C to Fortran interface routine.  Returns the answers to
c              a Form Menu.
c
c   INPUT:  cwid    C*1  Dn  -  Menu level of Form.
c
c           cans    C*1  DMAXFMT -  Form entry text (answers).
c
c           knbut   I*4  D1  -  Number of entries in Form.
c
c   OUTPUT: cmsg    C*1  Dn  -  Text of error message.
c
c           kpr     I*4  D1  -  Form entry which caused error.
c
c           kerr    I*4  D1  -  Returns non-zero on error.
c
c***********************************************************************
c
      subroutine npw_put_form(cwid,cans,knbut,cmsg,kpr,kerr)
c
      include 'menu.inc'
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-START
      integer*4 kerr,kpr,knbut
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-END
c
      byte cwid(10),cans(132,MAXFMT),cmsg(80)
c
      integer*4 nc,ilev,i
c
      character*132 sbuf, tmp
c
c...Initialize routine
c
      kerr   = 0
      IMSCAN = 2
      MFERR  = 0
c
c...Define main level above actual prompting level
c
      call pwdbtc (cwid,sbuf,nc)
      call asclev (sbuf,nc,MLEVL,ilev,kerr)
      if (kerr .ne. 0) then
c
c...changed for VAX
c...Yurong
c
c          call pwdctb ("Invalid window ID returned.",cmsg)
          tmp = 'Invalid window ID returned.'
          call pwdctb (tmp, cmsg)
          kpr    = 0
          go to 8000
      endif
c
c...Fill form with answers
c
      do 500 i=1,knbut,1
          call pwdbtc (cans(1,i),FRMBUF(i),nc)
  500 continue
c
c...Return answers to application
c
      call master
      if (MFERR .ne. 0) then
          call pwdctb (MFERTX,cmsg)
          kpr    = MFERPT
          kerr   = MFERR
          go to 8000
      endif
c
c...End of routine
c
 8000 return
      end
