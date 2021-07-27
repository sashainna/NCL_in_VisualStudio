C*********************************************************************
C*    NAME         :  condif.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       condif.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:43
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine condif
C*        This routine handles if statements of the forms:
C*          if (arithmetic-expression) s1,s2,s3
C*                A branch is made to statement label "s1" if the
C*                 value of the expression is negative.
C*                A branch is made to statement label "s2" if the
C*                 value of the expression is zero.
C*                A branch is made to statement label "s3" if the
C*                 value of the expression is positive.
C*          if (logical-expression) s1
C*                A branch is made to statement label "s1" if the
C*                 value of the expression is "TRUE".
C*                The next statement is processed if the value of
C*                 the expression is "FALSE".
C*          if (identifier) s1
C*                A branch is made to statement label "s1" if the
C*                 identifier is a valid scalar or geometric entity
C*                 identifier.
C*                The next statement is processed if the identifier
C*                 is undefined.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine condif

      include 'com8a.com'

      real*8 temp, logtv
      character*64 svtkn(3)
      integer*2 ktv(4), errnum, isvix, i, j, inxsv, found1,comma
      integer*4 itemp(3),k,ifl5,ifl6,svsub(3)
      logical logif
      equivalence(ktv,tv)


      logif = .false.

c         If no MACRO or LOOP is being defined, return an error.
      if (ifl(378).ne.1 .and.ifl(38) .eq. 0 .and.
     x    ifl(45) .eq. 0) then
          errnum = 80
          go to 8888
      endif

c         Parse the expression inside the parenthesis.
      call parsit

c         If a scalar value is returned, go check if it was
c         a logical or numeric expression.
      if (ityp .eq. 3 .or.
     x    ityp .eq. 4) then

c         If the parser returns a parser error value,
c         check if the token is an identifier.
      else if (ityp.eq.2 .or. ityp .eq. 8) then
          if (ityp .eq. 8) then
            call vstchk
          else
            nextyp = 7
          endif
          if (ityp .eq. 2) then

c                 Clear the error flag and ifl(2) that was set by
c                 the parser parsing an identifier where it expected
c                 an expression.  If this is not done, the source
c                 line and nline won't be updated. 
              ifl(2) = 0
              err = .false.

c                 If it is an undefined identifier, set 
c                 it to recognize a logical false value.
              if (ist .eq. 1) then
                  logtv =-1.

c                 If it is a defined geometric identifier,
c                 set it to recognize a logical true value.
              else if ((ist .ge. point .and.
     x                  ist .le. matrix) .or.
     x                  ist .eq. patern  .or.
     x                  ist .eq. shape   .or.
     x                  ist .eq. pntvec) then
                  logtv = 1.
              else
                  errnum = 83
                  go to 8888
              endif

c                 Set the logical expression flag on and parse the
c                 next token which should be a left parenthesis.
              ifl(210) = 1
              if (nextyp .ne. 7) then
                  errnum = 310
                  go to 8888
              endif
              if (ain(inx).eq.')') call parser

c                 After the ending parenthesis was parsed, restore 
c                 the logical value that was set by the test for 
c                 an identifier.
              tv = logtv
          else
              errnum = 83
              go to 8888
          endif
      else
          errnum = 83
          go to 8888
      endif
      ifl(44)=9

c         Load the final value of the evaluated expression in
c         tv to be used to decide which label to branch to.
      temp=tv

c         The call to parsit evaluated the expression.  If the 
c         expression contained a logical operator, ifl(210) was set 
c         to a value of 1 by expres.   epm 3-20-90
c
c         If i = 1, branch to the first operand of the IF statement
c            i = 2, branch to the second operand of the IF statement
c            i = 3, branch to the third operand of the IF statement
      if (ifl(210) .eq. 1) then
          logif=.true.
          i=1
          ifl(210)=0
          if ((ifl(374).ne.0 .and. temp.ne.0).or.
     1        (ifl(374).eq.0 .and.temp.eq.1.d0)) then
            inxsv = inx
            ifl(111)=1
            found1=0
            call chknst(found1)
            if (err) go to 99999
            ifl(111)=0
            inx = inxsv
          endif
      else if (temp .gt. -sc(170) .and.
     x         temp .lt.  sc(170)) then
          i=2
      else if (temp .lt. 0) then
          i=1
      else
          i=3
      endif

c         Parse the statement labels on the IF statement.
c         There should only be one label for a logical IF.
      comma = 0
   90 isvix = inx
      do 100,j=1,3
c
c...     set the flag to parse the label with a ':' when lang=cadra
c
          if (ifl(374) .ne. 0) ifl(376)=1
          call parsit
          ifl(376)=0
          svtkn(j)=token2
          svsub(j)=ivxsub
c
c...    allow optional comma for IF statement 
c...    eg IF (a'lt'b) ,a=0
c
          if(j.eq.1.and.ityp .eq.5 .and. ist.eq.9) then 
            comma = 1
            goto 90
          endif
          if (ityp .eq. 3) then
              call vstchk
              if (ist .eq. 13) then
                ityp=2
                if (comma .eq.1.and.j.eq.1) then
                  errnum = 13
                  go to 8888
                endif
                if (ifl(378).eq.1.and.j.eq.1) then
                  errnum = 80
                  go to 8888
                endif
              endif
          endif
          if (ityp .eq. 2 .and.
     x         ist .eq. 13) then
              if (comma .eq.1.and.j.eq.1) then
                errnum = 13
                go to 8888
              endif
              if (ifl(378).eq.1.and.j.eq.1) then
                errnum = 80
                go to 8888
              endif
              call getran(jb,ktv(1))
              call nclf_src_rec_to_line(jb4(ktv(2)),itemp(j))
              if (itemp(j) .eq. 0) then
                  errnum = 81
                  go to 8888
              endif
          else if (logif) then
c
c... Logical IF followed by a statement.
c
            if (ityp .eq. 8) then
                ifl(2) = 0
                err = .false.
            endif
            if ((ifl(374).ne.0 .and. temp.eq.0).or.
     1        (ifl(374).eq.0 .and.temp.eq.-1))goto 99999
            inx = isvix
            ncsfl(3) = 1
            ifl(324) = 0
            goto 99999
          else if (ityp .eq. 2 .and.
     x              ist .eq. 1) then
              errnum = 8
              go to 8888
          else
              errnum = 82
              go to 8888
          endif
          if (logif) go to 110
100   continue

c         Check to make sure nothing follows the statement label(s).
110   if (nextyp .ne. 11) then
          errnum = 4
          go to 8888
      endif

      call nclf_src_rec_to_line (ifl4(5),ifl5)
      call nclf_src_rec_to_line (ifl4(6),ifl6)
      if (itemp(i)-1 .ge. ifl5 .and.
     x    itemp(i)-1 .le. ifl6) goto 200

c
c...Find the JUMPTO label
c...in the source file
c...6/26/91  -  Bobby
c
      savetv = tv
      call labchk (ifl5,ifl6,svtkn(i),svsub(i),k)
      if (k .ne. 0) then
          tv     = savetv
          itemp(i) = k
          inx    = 1
          ain(1) = ' '
      else
          errnum = 111
          go to 8888
      endif

200   continue
      call ifjump(itemp(i),j)
      if (j.ne.0) then
C
C...error jump into if-then-else not allowed
C
        errnum = 509
        goto 8888
      endif
      call putw2(nline)
      ifl(47)=1

c         If this was a false logical IF expression,
c         don't branch to the label.
      if (logif .and. temp .eq. -1) goto 99999

c         Otherwise, set nline to the proper statement
c         number of the label to branch to.
      nline=itemp(i)
      lskip = .true.
      go to 99999

8888  call error (errnum)

99999 continue
      return
      end
