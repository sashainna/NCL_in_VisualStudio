C*********************************************************************
C*    NAME         :  fixpp.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        fixpp.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:04
C********************************************************************/
c*
C*********************************************************************
C*    E_SUBROUTINE     : fixpp (knrec,errflg)
c*       Updates the loop, macro, label, and if pointers after a *EDT.
C*    PARAMETERS
C*       INPUT  :
C*          knrec    = Number of records in pp file.
C*       OUTPUT : none
C*          errflg   = 1 = Error occurred reading file.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine fixpp (knrec,errflg)
 
      include 'com4a.com'
c
      integer*2 errflg
      integer*4 knrec
c
      integer*2 ktv(4),mode,head8,head9,iflg,ildflg,inot
      integer*4 tsub,lsub,esub,irec,isvrec,callin,ioerr,zip
      integer*4 stnum,nxtnum,endnum,lpstnm,isub,tmp,jrec,jsc10(6)
C
      logical lpnd,macal,icurln
c
      character*1 buf1(MAX_LEN)
      character*(MAX_LEN) buf
      character*64 macnam, tname, lname, dolimv, blank, loopst
      character*64 errlab
c
      equivalence(buf,buf1)
      equivalence (tv,ktv)
      equivalence (jsc10,sc(10))
      equivalence (sc182,errlab)
c
      data blank /'        '/
      data zip /-1/, m1 /-1/
      data dolimv /'DOLIMITV'/
      data loopst /'LOOPST'/
c
      integer*2 IFV,THENV,ELSEIFV,ELSEV,ENDIFV
      parameter (IFV     = 825)
      parameter (THENV   = 938)
      parameter (ELSEIFV = 939)
      parameter (ELSEV   = 940)
      parameter (ENDIFV  = 941)
c
c...Initialize routine
c
      icurln = .false.
      macal = .false.
      lpnd = .false.
      errflg = 0
      ildflg = 1
      if (ifl(38).ne.1 .and. ifl(45).ne.1) ildflg = 2
      call ifinit
c
c...Reset parsing
c
      irec = 0
   18 inx = 0
      cin = ' '
      isvrec = irec + 1
c
c...Get the next record
c 
   20 if (irec .ge. knrec) go to 8000
      ioerr = nclf_getsrc (irec,buf,nc,irctyp,0)
      if (ioerr .ne. 0) go to 9000
      if (irec .eq. nline) icurln = .true.
      if (irctyp .ge. 20 .and. irctyp .lt. 30) lpnd = .true.
      if (irctyp .ge. 30 .and. irctyp .lt. 40) macal = .true.
      irec=irec+1
c
c...Check and move each non-blank, non-comment character to the
c...CIN buffer so it can be parsed.
c
      do 40 i=1,nc,1
          if (buf1(i).eq.' ' .or. buf1(i).eq.'	') goto 40
          if (buf1(i).eq.'%') goto 45
          if (buf1(i).ne.'$') goto 35
          if (buf1(i+1).eq.'$' .and. i .lt. nc) goto 45
          goto 20
   35     inx=inx+1
          if (inx.lt.1536) call convrt(buf1(i), ain(inx), 1)
c
c......Check for fixed field words that may be legally followed
c......by a single dollar sign that does NOT mean continuation.
c
          if (inx .eq. 6) then
              if (cin(1:6) .eq. 'PARTNO' .or.
     1            cin(1:6) .eq. 'PPRINT' .or.
     2            cin(1:6) .eq. 'INSERT' .or.
     3            cin(1:6) .eq. 'LETTER') goto 45
          endif
   40 continue
   45 inx=1
c
c...Parse 1st token
c
      call parser
c
c......MACRO statement
c
   41 if (ityp .eq. 2 .and. ist .eq. 11) then
          macnam = token2
          buf    = token2
          isub   = 0
c
c.........Update the MACRO parameter data header record
c.........which contains the numbers of the MACRO statement
c.........and the first statement after the MACRO statement
c
          call ncl_getmc_hd (jsc10(1), jsc10(2), jsc10(3), mode,
     1             head8, head9, callin)
          call nclf_src_line_to_rec (isvrec,jsc10(1))
          call nclf_src_line_to_rec (irec+1,jsc10(2))
          call ncl_storehd (jsc10(1), jsc10(2), jsc10(3), mode,
     1                  head8, head9, callin)
c
c......TERMAC statement
c
      else if (ityp .eq. 1 .and. ist .eq. 807) then
c
c.........Update TERMAC statement number field in
c.........MACRO parameter header record
c............Make sure MACRO has been defined
c
             call ncl_getmc_hd (jsc10(1), jsc10(2), jsc10(3),
     1              mode, head8, head9, callin)
             call nclf_src_line_to_rec (irec,jsc10(3))
             call ncl_storehd (jsc10(1), jsc10(2), jsc10(3),
     1              mode, head8, head9, callin)
c
c.........Update the looping region stack for this MACRO
c.........with the numbers of the MACRO statement;
c.........The first statement after the MACRO statement
c.........and the TERMAC statement.
c
              stnum  = jsc10(1)
              nxtnum = jsc10(2)
              call nclf_src_line_to_rec (irec,endnum)
              tmp = 0
              call lrupdt (macnam, tmp, blank, tmp, stnum,
     1              nxtnum,endnum, m1,m1,blank, tmp)
c
c......Active CALL statement
c
      else if (ityp .eq. 1 .and. ist .eq. 824 .and. macal) then
          macal = .false.
c
c.........Get the MACRO name
c
          call parser
          call parser
c
c.........Update the MACRO dummy records NLINE field
c.........with the line after the current line
c
          ivxsub = 0
          buf    = token2
          isub   = 0
          call ncl_getmc_hd (jsc10(1), jsc10(2), jsc10(3), mode,
     1                 head8, head9, jsc10(6))
          call nclf_src_line_to_rec (irec+1,jsc10(6))
          call ncl_storehd (jsc10(1), jsc10(2), jsc10(3), 
     1            mode, head8, head9, jsc10(6))
c
c......LOOPST statement
c......Save its statement number
c
      else if (ityp .eq. 1 .and. ist .eq. 808) then
          lpstnm = irec
c
c......Active LOOPND statment
c
      else if (ityp .eq. 1 .and. ist .eq. 809 .and. lpnd) then
c
c.........Update the looping region LOOPST/LOOPND entry
c.........withe the record numbers of the LOOPST statement;
c.........The first statement after the LOOPST statement and
c.........the LOOPND statement.
c
          lpnd   = .false.
          call nclf_src_line_to_rec (lpstnm+1,nxtnum)
          call nclf_src_line_to_rec (irec,endnum)
          tmp = 0
          call lrupdt (loopst, tmp, blank, tmp, lpstnm,
     1            nxtnum,endnum,m1,m1,blank, tmp)
c
c......DO statement
c
      else if (ityp .eq. 1 .and. ist .eq. 844 .and. lpnd) then
          lpnd   = .false.
c
c.........Parse / , LABEL , Index
c
          call parser
          call parser
          if ((ityp .eq. 2 .and. ist .eq. 13) .or.
     1        (ityp .eq. 3 .and. nextyp .eq. 9)) then
              tname  = token2
              tsub = ivxsub
              call parser
              if (ityp .eq. 2 .and. ist .eq. 2) then
c
c.........Update the looping region stack with the number
c.........of the DO loop statement and the next statement
c.........after the DO loop statement
c
                  lname  = token2
                  lsub = ivxsub
                  call nclf_src_line_to_rec (irec+1,nxtnum)
                  call nclf_src_line_to_rec (isvrec,jrec)
                  endnum = -1
                  tmp = 0
                  call lrupdt (tname,tsub,lname,lsub,jrec,
     1                  nxtnum,endnum,m1,m1,blank, tmp)
              endif
          endif
c
c......LABEL
c
      else if ((ityp .eq. 2 .and. ist .eq. 13) .or.
     1         (ityp .eq. 3 .and. (nextyp .eq. 7 .or.
     2                             nextyp .eq. 13))) then
c
c.........If this is a numeric label, call vstchk to get
c.........the page and element pointer so the label's
c.........associated word can be updated
c
          if (ityp .ne. 2) call vstchk
c
c.........Update any active DO Loops that this label terminates
c.........Only update if this is a valid label
c.........Bobby  -  2/9/93
c
          if (ityp .eq. 2 .and. ist .eq. 13) then
              tname  = token2
              tsub = ivxsub
              call nclf_src_line_to_rec (irec,endnum)
              tmp = 0
              call lrupdt (tname,tsub,dolimv,tmp,zip,zip,
     1             endnum, m1,m1,blank, tmp)
c
c.........Update LABEL ID associated word with new
c.........number of statement
c
              call getran (jb,ktv(1))
              call nclf_src_line_to_rec (isvrec,jb4(ktv(2)))
              call putran (jb,ktv(1))
          endif
c
c.........Strip label terminator (":" or ")") off
c.........then send to the top to check for
c.........TERMAC, CALL or LOOPND following the label
c
          if (nextyp .eq. 7 .or. nextyp .eq. 13) call parser
          call parser
          err    = .false.
          ifl(2) = 0
          go to 41
      else if (ityp.eq.1 .and. (ist.eq.IFV.or.ist.eq.ELSEIFV .or.
     x         ist.eq.ELSEV .or.ist.eq.ENDIFV)) then
          iflg = 1
          nxteos = .false.
          if (ildflg.eq.2 .and. icurln) ildflg = 3
          call ifelse(iflg, isvrec, ildflg, inot)
      endif
c
c...Go get next record
c
      go to 18
c
c...End of routine
c
c...If one or more MACRO(s), DO Loop(s) or a
c...LOOPST/LOOPND is executing, reset looping
c...region limits
c...
c...   ifl(38) = 2: MACRO executing
c...   ifl(45) = 3: DO Loop executing
c...   ifl(45) = 2: LOOPST/LOOPND executing
c
 8000 if (ifl(38) .eq. 2 .or. ifl(45) .eq. 3 .or. ifl(45) .eq. 2)
     1        call lrdata (tname,tsub,lname,lsub,ifl4(5),endnum,ifl4(6),
     2                     ifl(254),ifl(255),errlab,esub,1)
      return
c
c...Error reading file
c
 9000 errflg = 1
      end
