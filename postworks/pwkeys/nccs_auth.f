c
c***********************************************************************
c
c   FILE NAME: nccs_auth.for
c   CONTAINS:
c               nccs_auth  na_autini  na_disscr  na_help
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        nccs_auth.f , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:09
c
c***********************************************************************
c
c***********************************************************************
c
c   PROGRAM: nccs_auth
c
c   FUNCTION:  This program manages the internal licensing data base
c              'CUSTOMER_LICENSE.DBA' for all customers.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C VAX-START
C     program nccs_auth
c
C     include 'menu.inc'
c
C     integer*4 ic,ierr,irnc,ivnc,irec,ifl,flag
c
C     character*80 msg,rmsg,vmsg
c
c...Initialize routine
c
C     call init
C     call na_autini
c
c...Allocate terminal
c
C     CURSES = 1
C     call trmatt (CURSES)
c
c...Open data base
c
C     call opndat (1,msg,ierr)
C     if (ierr .ne. 0) go to 9000
c
c...Clear out form
c
C     call clrfrm
c
c...Display the initial screen
c
C     call na_disscr
c
c...Get the users input
c
C     irec   = 1
C 100 call form (irec,ifl)
c
c...^C
c...Exit
c
C     if (ifl .eq. 0) then
C         call clrscr
C         go to 8000
c
c...PF1 A
c...Add record
c
C     else if (ifl .eq. 1) then
c
c......Make sure password matches
c
C         call pwdpas (FRMBUF,SAPNC,FRMBUF(51),ierr)
C         if (ierr .eq. 0) then
C             if (FRMBUF(51) .ne. FRMBUF(9)) ierr = 9
C         endif
C         if (ierr .ne. 0) then
C             call messag (ierr)
C             go to 100
C         endif
C         call addrec (msg,ierr)
C         if (ierr .ne. 0) go to 9000
C         go to 100
c
c...PF2
c...Help
c
C     else if (ifl .eq. 2) then
C         call na_help
C         go to 100
c
c...PF1 S
c...Super user
c
C     else if (ifl .eq. 3) then
C         NLEVL  = 2
C         FRMREC = 10
c
c......Open data base for read/write
c
C         call clsfil (LUNSC1)
C         call opndat (3,msg,ierr)
C         if (ierr .ne. 0) go to 9000
c
c......Open print control file
c......LSP.COM
c
C         call opnprt (msg,ierr)
C         if (ierr .ne. 0) go to 9000
c
c......Redisplay form
c
C         call disprm (9)
C         call disfld (9)
C         call disprm (10)
C         call disfld (10)
C         go to 100
c
c...PF3
c...Find next match
c
C     else if (ifl .eq. 4) then
C         flag = 1
C         call search (flag, msg,ierr)
C         if (ierr .ne. 0) go to 9000
C         go to 100
c
c...PF4
c...Clear form
c
C     else if (ifl .eq. 5) then
C         call clrfrm
C         go to 100
c
c...PF1 D
c...Delete record
c
C     else if (ifl .eq. 6) then
C         call delrec (msg,ierr)
C         if (ierr .ne. 0) go to 9000
C         go to 100
c
c...PF1 P
c...Print record
c
C     else if (ifl .eq. 7) then
C         call fprint (1,msg,ierr)
C         if (ierr .ne. 0) go to 9000
C         go to 100
c
c...PF1 E
c...Generate password
c
C     else if (ifl .eq. 8) then
C         call pwdpas (FRMBUF,SAPNC,FRMBUF(9),ierr)
C         if (ierr .eq. 0) then
C             SAPNC(9) = 19
C             call disfld (9)
C         else
C             call messag (ierr)
C         endif
C         go to 100
c
c...^W
c...Redisplay page
c
C     else if (ifl .eq. 10) then
C         call na_disscr
C         go to 100
c
c...PF1 U
c...Purge data base
c
C     else if (ifl .eq. 11) then
C         call purg (msg,ierr)
C         if (ierr .ne. 0) go to 9000
C         go to 100
c
c...PF1 PF4
c...Display selection fields
c
C     else if (ifl .eq. 12) then
C         call selfrm
C         go to 100
c
C     else
C         go to 100
C     endif
c
c...End of routine
c
C8000 call trmrst
C     if (NLEVL .eq. 2 .and. IBANPT .eq. 1)
C    1        close (unit=LUNSC3,dispose='delete')
C     call clsfil (0)
C     call exit
c
c...An error occurred during
c...execution of program
c
C9000 call trmmsg (' ')
C     call trmmsg (msg)
C     if (ierr .lt. 0) then
C         call errhnd (vmsg,ivnc,rmsg,irnc)
C         if (ivnc .ne. 0) call trmmsg (vmsg)
C         if (irnc .ne. 0) call trmmsg (rmsg)
C     endif
c
C     call trmmsg (' ')
C     go to 8000
C     end
C VAX-END
c
c***********************************************************************
c
c   SUBROUTINE:  na_autini
c
c   FUNCTION:  This routine initializes the Customer License data base
c              manager.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C VAX-START
C     subroutine na_autini
c
C     include 'menu.inc'
c
c...Initialize form
c
C     NLEVL  = 1
C     FRMREC = 8
C     IPRLIN = 22
c
C     IPRMDS(1) = 4
C     IPRMDS(2) = 6
C     IPRMDS(3) = 8
C     IPRMDS(4) = 10
C     IPRMDS(5) = 12
C     IPRMDS(6) = 14
C     IPRMDS(7) = 16
C     IPRMDS(8) = 18
C     IPRMDS(9) = 20
C     IPRMDS(10) = 4
c
C     FRMST(1) = 10
C     FRMST(2) = 11
C     FRMST(3) = 11
C     FRMST(4) = 10
C     FRMST(5) = 18
C     FRMST(6) = 22
C     FRMST(7) = 10
C     FRMST(8) = 12
C     FRMST(9) = 11
C     FRMST(10) = 64
c
C     FRMEN(1) = 49
C     FRMEN(2) = 30
C     FRMEN(3) = 30
C     FRMEN(4) = 69
C     FRMEN(5) = 19
C     FRMEN(6) = 32
C     FRMEN(7) = 20
C     FRMEN(8) = 23
C     FRMEN(9) = 29
C     FRMEN(10) = 71
c
c...Initialize selection fields
c
C     do 400 i=1,10,1
C         MLEVL(i) = 0
C         FRMBUF(i+20) = ' '
C         SAPNC(i+20) = 0
C 400 continue
c
c...Initialze prompts
c
C     SAPRM(1) = 'Data base file does not exist.  Do you want to ' //
C    1           'create it?'
C     SAPRM(2) = 'Record already exists.  Do you want to replace it?'
C     SAPRM(3) = 'Are you sure you want to delete this record?'
C     SAPRM(4) = 'Are you sure you want to purge the data base?'
C     SAPRM(5) = 'Hit return to continue.'
c
c...Initialzie messages
c
C     SAERR(1) = '*ERROR*  Invalid company name.'
C     SAERR(2) = '*ERROR*  Invalid hardware device.'
C     SAERR(3) = '*ERROR*  Invalid software name.'
C     SAERR(4) = '*ERROR*  Invalid option list.'
C     SAERR(5) = '*ERROR*  Invalid number of users.'
C     SAERR(6) = '*ERROR*  Invalid termination date.'
C     SAERR(7) = '*ERROR*  Invalid version date.'
C     SAERR(8) = '*ERROR*  Invalid system ID.'
C     SAERR(9) = '*ERROR*  Invalid password.'
C     SAERR(10) = '*ERROR*  Invalid license number.'
C     SAERR(21) = 'Record was not added to data base.'
C     SAERR(22) = 'New company has been added.'
C     SAERR(23) = 'New software product has been added.'
C     SAERR(24) = 'Software product was replaced.'
C     SAERR(25) = 'No match found.'
C     SAERR(26) = 'Record successfully deleted.'
C     SAERR(27) = 'Company successfully deleted.'
C     SAERR(28) = 'Data base successfully purged.'
C     SAERR(29) = 'Record successfully printed.'
c
c...End of routine
c
C8000 return
C     end
C VAX-END
c
c***********************************************************************
c
c   SUBROUTINE:  na_disscr
c
c   FUNCTION:  This routine displays the screen for the Customer License
c              data base manager.
c
c   INPUT:  clwin   B*1  D512  -  License manager memory for the softare
c                                 product to show the users for.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C VAX-START
C     subroutine na_disscr
c
C     integer*4 i
C     character*80 lhd1,lhd2,lhd3
c
C     data lhd1 /'**************************  NCCS LICENSE MANAGEMENT  *
C    1**************************'/
C     data lhd2 /'******************************************************
C    1**************************'/
c.c   data lhd3 /'<PF2> = Help | <PF1><PF3> = Select | <PF3> = Search |
c.c  1<PF4> = Clear | ^C = Exit'/
C     data lhd3 /'^P = Help | ^F = 2nd Func | ^F A = Add | ^N = Search
C    1 | ^U = Clear | ^C = Exit'/
c
c...Display border
c
C     call clrscr
C     call plott (1,1)
C     call dmpbuf (lhd1,80)
C     call plott (2,1)
C     call dmpbuf (lhd2,80)
c
C     call plott (23,1)
C     call dmpbuf (lhd2,80)
C     call plott (24,1)
C     call dmpbuf (lhd3,80)
c
c...Display prompts
c
C     call disprm (-1)
C     call disfld (-1)
c
c...End of routine
c
C8000 return
C     end
C VAX-END
c
c***********************************************************************
c
c   SUBROUTINE:  na_help
c
c   FUNCTION:  This routine displays the help text for the Customer
c              License data base manager.
c
c   INPUT:  clwin   B*1  D512  -  License manager memory for the softare
c                                 product to show the users for.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C VAX-START
C     subroutine na_help
c
C     include 'menu.inc'
c
C     integer*4 ie,il,nc,i,j,strlen1
c
c.c      character*80 ld(29)
C     character*80 ld(27)
c
c.c      data ld /'Editing keys',
c.c     1         '------------',
c.c     2         ' ',
c.c     3         'LEFT ARROW  = Move cursor left.         UP ARROW   = Mov
c.c     4e cursor up 1 line.',
c.c     5         'RIGHT ARROW = Move cursor right.        DOWN ARROW = Mov
c.c     6e cursor down 1 line.',
c.c     7         'BACKSPACE   = Move to beg of line.      LINE FEED  = Mov
c.c     8e to end of line.',
c.c     9         'RETURN      = Same as DOWN ARROW.',
c.c     *         ' ',
c.c     1         'DELETE = Delete char to the left.       ^D = Delete curr
c.c     2ent character.',
c.c     3         'PF1 LEFT ARROW = Delete to EOL.         PF1 RIGHT ARROW
c.c     4= Delete to BOL.',
c.c     5         ' ',
c.c     6         '^A  = Toggle Insert/Overstrike mode.',
c.c     7         ' ',
c.c     8         'PF2 = Help (This text.).                ^C = Exit.',
c.c     9         '^W = Redisplay page.',
c.c     *         ' ',
c.c     1         'PF1 PF3 = Toggle selected fields.  A selected field, dis
c.c     2played in reverse video,',
c.c     3         '          is used when searching for the next match.  If
c.c     4 no fields are selected,',
c.c     5         '          then the first software product of the next co
c.c     6mpany will be displayed.',
c.c     7         '          To search for all software products for a cert
c.c     8ain company, select any',
c.c     9         '          of the fields beneath the COMPANY field.  The
c.c     *text used for field',
c.c     1         '          matching is the text in the field when it was
c.c     2originally selected.',
c.c     3         ' ',
c.c     4         'PF3 = Search for next match.            PF1 PF4 = Displa
c.c     5y selected field text.',
c.c     6         'PF4 = Clear form and reset search position.',
c.c     7         ' ',
c.c     8         'PF1 E ^X = Calculate password.          PF1 P = Print re
c.c     9cord.',
c.c     *         'PF1 A    = Add record.                  PF1 D = Delete r
c.c     1ecord.',
c.c     2         'PF1 U    = Purge data base.'/
C     data ld /'Editing keys',
C    1         '------------',
C    2         ' ',
C    3         'LEFT ARROW  = Move cursor left.         UP ARROW   = Mov
C    4e cursor up 1 line.',
C    5         'RIGHT ARROW = Move cursor right.        DOWN ARROW = Mov
C    6e cursor down 1 line.',
C    7         'BACKSPACE   = Move to beg of line.      LINE FEED  = Mov
C    8e to end of line.',
C    9         'RETURN      = Same as DOWN ARROW.',
C    *         ' ',
C    1         'DELETE = Delete char to the left.       ^D = Delete curr
C    2ent character.',
C    3         '^F LEFT ARROW = Delete to EOL.          ^F RIGHT ARROW =
C    4 Delete to BOL.',
C    5         ' ',
C    6         '^A = Toggle Insert/Overstrike mode.',
C    7         ' ',
C    8         '^P = Help (This text.).                 ^C = Exit.',
C    9         '^W = Redisplay page.',
C    *         ' ',
C    1         '^F ^N = Toggle selected fields.  A selected field, displ
C    2ayed in reverse video,',
C    3         '        is used when searching for the next match.  The
C    4text used for field',
C    1         '        matching is the text in the field when it was or
C    2iginally selected.',
C    3         ' ',
C    4         '^N    = Search for next match.          ^F ^U = Display
C    5selected field text.',
C    *         '^F A  = Add record.                     ^F D  = Delete r
C    1ecord.',
C    6         ' ',
C    7         '^U    = Clear form and reset search position.',
C    8         'PF1 E ^X = Calculate password.          PF1 P = Print re
C    9cord.',
C    *         'PF1 A    = Add record.                  PF1 D = Delete r
C    1ecord.',
C    2         'PF1 U    = Purge data base.'/
c
c...Display help text
c
C     ie     = 24
C     if (NLEVL .eq. 2) ie = 27
C     il     = IPRMDS(1)
C     do 100 j=IPRMDS(1),IPRMDS(9),1
C         call plott (j,1)
C         call clreol
C 100 continue
C     do 300 i=1,ie,1
C         if (il .eq. IPRMDS(9)) then
C             call prompt (5,ians)
C             do 200 j=IPRMDS(1),IPRMDS(9),1
C                 call plott (j,1)
C                 call clreol
C 200         continue
C             il     = IPRMDS(1)
C         endif
C         call plott (il,1)
C         nc     = strlen1(ld(i))
C         call dmpbuf (ld(i),nc)
C         il     = il     + 1
C 300 continue
C     call prompt (5,ians)
C     call na_disscr
c
c...End of routine
c
C8000 return
C     end
C VAX-END
