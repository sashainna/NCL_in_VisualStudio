C*********************************************************************
C*    NAME         :  onerr.f
C*       CONTAINS:
C*            onerr
C*    COPYRIGHT 1999 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       onerr.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:23
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine onerr
C*      Parse an ON/ERROR statement.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine onerr
c
      include 'com8a.com'
c
      character*64 errlab
      equivalence (sc182,errlab)
c
      integer*2 iemod,iwarn
      integer*4 stnum,nxtnum,endnum
      integer*4 tsub, lsub, esub
c
      character*64 elab,tname,lname
c
      integer*2 ERRORV,THENV,STOPV,CONTIN,WARN,NWARN
      parameter (ERRORV=937)
      parameter (THENV=938)
      parameter (STOPV=2)
      parameter (CONTIN=846)
      parameter (WARN=852)
      parameter (NWARN=853)
c
c...Make sure Macro or Loop is active
c
      if (ifl(38) .eq. 0 .and. ifl(45) .eq. 0) go to 9300
c
c...Get mandatory parameters
c...ERROR,THEN
c
      if (nextyp .ne. 5) go to 9000
      call parsit
      if (ityp .ne. 1 .or. ist .ne. ERRORV) go to 9100
      if (nextyp .ne. 9) go to 9200
      call parsit
      call parsit
      if (ityp .ne. 1 .or. ist .ne. THENV) go to 9100
      if (nextyp .ne. 9) go to 9200
c
c...Get error condition
c
      call parsit
      call parsit
      if (ityp .eq. 1) then
c
c.......STOP
c
          if (ist .eq. STOPV) then
              iemod = 0
c
c......CONTIN
c
          else if (ist .eq. CONTIN) then
              iemod = 1
c
c.......Unrecognized
c
          else
              go to 9100
          endif
c
c......Label
c
      else if ((ityp .eq. 2 .and. ist .eq. 13) .or. ityp .eq. 3) then
          if (ityp .eq. 3) then
              call vstchk
              if (ist .ne. 13) go to 9100
          endif
          elab = token2
          iemod = 2
      endif
c
c...Check for optional WARN/NOWARN
c
      iwarn = 0
      if (nextyp .ne. 11) then
          if (nextyp .ne. 9) go to 9200
          call parsit
          call parsit
          if (ityp .eq. 1) then
              if (ist .eq. WARN) then
                  iwarn = 0
              else if (ist .eq. NWARN) then
                  iwarn = 1
              else
                  go to 9100
              endif
          else
              go to 9100
          endif
      endif
c
c...Finished parsing command
c...Set variables
c
      if (iemod .eq. 0 .and. iwarn .eq. 1) go to 9100
      ifl(254) = iemod
      ifl(255) = iwarn
      errlab = elab
      call lrdata (tname,tsub,lname,lsub,stnum,nxtnum,endnum,
     1             iemod,iwarn,elab,esub,1)
      call lrupdt (tname,tsub,lname,lsub,stnum,nxtnum,endnum,
     1             ifl(254),ifl(255),errlab,esub)
c
c...End of routine
c
 8000 return
c
c...Slash expected
c
 9000 call error (22)
      go to 8000
c
c...Invalid syntax
c
 9100 call error (25)
      go to 8000
c
c...Comma expected
c
 9200 call error (311)
      go to 8000
c
c...Macro not in effect
c
 9300 call error (80)
      go to 8000
      end
