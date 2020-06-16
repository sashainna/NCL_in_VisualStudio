C*********************************************************************
C*    NAME         :  parser.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
c*       parser.f , 25.1
c*    DATE AND TIME OF LAST MODIFICATION
c*       04/29/15 , 15:10:24
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine parser
c*      This is the main parser routine.  It parses the 'CIN' array and
c*      returns the next item in the 'TOKEN2' array.  'ITYP' contains
c*      the type of the item, and 'IST' contains the subtype.  If the item
c*      is numeric, 'ITV' contains the integer value of the item and 'TV'
c*      contains the real value.  For all types, 'NEXTYP' contains the
c*      type of the delimiter found after the token.  Following is a list
c*      of types:
c*
c*      ITYP = 1    vocabulary word     IST = vocabulary word number
c*      ITYP = 2    identifier          IST = 1 unknown identifier
c*                                            2 scaler
c*                                            3 point
c*                                            4 vector
c*                                            5 line
c*                                            6 plane
c*                                            7 circle
c*                                            8 curve
c*                                            9 surf
c*                                           10 matrix
c*                                           11 macro
c*                                           12 macro parameter
c*                                           13 label
c*                                           14 reserved id
c*                                           15 index id
c*                                           16 vocab word as macro param
c*                                           17 undeclared id as macro param
c*                                           18 shape
c*                                           19 post word in shape list
c*                                           20 patern
c*                                           21 point vector
c*                                           22 pocket
c*                                           23 Data statement
c*                                           24 text variable
c*                                           30 Annotation (text)
c*                                           31 Symbol Master
c*                                           32 Symbol Placement
c*                                           33 Visual Solid
c*      ITYP = 3    integer             IST = 1 unsigned
c*                                            2 positive signed
c*                                            3 negative signed
c*      ITYP = 4    real                same as integer
c*      ITYP = 5    special character   IST = 1  '='
c*                                            2  '+'
c*                                            3  '-'
c*                                            4  '*'
c*                                            5  '/'
c*                                            6  '('
c*                                            7  ')'
c*                                            8  '.'
c*                                            9  ','
c*                                           10  '**'
c*                                           13  ':'
c*                                           14  '''
c*                                           15 '['
c*                                           16 ']'
c*                                           17 '^'
c*                                           18 '&'
c*                                           19 '{'
c*                                           20 '}'
c*      ITYP = 6    unrecognizable symbol
c*      ITYP = 7    end of input
c*      ITYP = 8    parser error
c*      ITYP = 9    string         
c*      ITYP = 10    wildcard token         
c*
c*      The nextyp values are the same as the subtypes for the special
c*      characters, nextyp=11 means end of input and nextyp=12 means its
c*      a letter or a number.
c*
c*      NEXTYP = 11
c*      wdtype, default to 0: parser wildcard and if the wildcard as "abc*" 
c*                              and abc is a scalar, 
c*                              return 'abc' not treat "abc*" as wildcard
c*              if set to 1: will parse the whole string as the wildcard 
c*                          if there is a '*' within the string
c*              if set to -1: will not parse the string as the wildcard 
c*                          in any case
C*
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************

      subroutine parser

      include 'com8a.com'
      include 'status.com'

      real*4 tapeq(2),tapnow
      equivalence (tapeq,sc(55)),(tapnow,tapeq(2))
      character*1 atok2(64)
      !character*6 atok2
      character*6 word6
      real*8 val
      integer*4 nc, strlen1,comment,cut
      integer*2 ilenth,typ, len
      equivalence (token2,atok2)
      equivalence (ain, word6)
      logical lop, qflg, wildp
c
c...Initialization
c 
      ivxsub = 0
      lop = .false.
      wildp = .false.
      token2 = ' '
      ivxsub = 0
      itv=0
      tv=0
      ist=0
      nextyp=0
      comment = ifl(183)
      cut = 0
      if (ifl(191) .eq. 0) then
          if (word6(1:6).eq.'PARTNO' .or.
     x        word6(1:6).eq.'PPRINT' .or.
     x        word6(1:6).eq.'INSERT' .or.
     x        word6(1:6).eq.'LETTER' .or.
     x        word6(1:6).eq.'REMARK' .or.
     x        word6(1:6).eq.'TITLES' ) comment = 1
      endif
c
c...Check for REMARK/OFF
c...REMARK/ON is handled in 'driver'
c
      if (word6(1:6) .eq. 'REMARK' .and. IFL(191) .eq. 0) then
         call prsrem (cin(1:nccin),i)
         if (i .eq. 2) then
             if (pgindx .ge. 1 .and. pgmode(pgindx) .eq. 5)
     1           pgindx = pgindx - 1
             ifl(383) = 0
c     The next else added on Feb.18, 2020 by S. Soiguine            
         else if (i .eq. 1) then
             ifl(383) = 1
         endif
      endif
c
c...Comment block
c.....Allow utility commands to be executed - ASF 1/13/14.
c
      !if (ifl(383).eq.1.and.(ain(inx).ne.'*'.or.inx.ne.1)) then cc That was modified by S. Soiguine on Feb.18, 2020
      if (ifl(383).eq.1) then
         ityp=7
         nextyp=11
         goto 99999
      endif
C
C... ignor preceeding'TAB' as well as 'SPACE'when not Comment such as PPRINT
c
c  5   if (((ain(inx).eq.' ').or.(ain(inx).eq.'	'))   
c     x        .and.(strlen1(cin(inx:nccin)).ne.0).and.comment.ne.1) then
c         inx=inx+1
c         go to 5
c      endif

      if (inx.eq.1) then
         geomnum = 0
      endif
10    isvinx=inx
c
c...added check for wildcard such as '*' or '*x' (x could be
c...any letter, number, '#', '_', '.')
c...start '*' followed by letters or just '*' (it will followed by seperate 
c...character ' ' or ',' or end char '$'
c...if the '*' is beginning of the line, it is not wildcard but
c...it's a '*' command
c...it the previous token is a scalar, it is not wildcard but
c...it's a '*' operator
c
      if (inx.ne.1 .and. (.not.pscalar) .and. (.not.rightp) .and.
     1    ifl(370) .ne. 1 .and. ifl(191) .eq. 0 .and. ain(inx).eq.'*')
     2        then
         length=1
         atok2(1)=ain(inx)
         if (.not.((ain(inx+1).ge.'A'.and.ain(inx+1).le.'Z')
     1      .or.(ain(inx+1).ge.'0'.and.ain(inx+1).le.'9')
     2      .or.ain(inx+1).eq.'_'.or.ain(inx+1).eq.'#'
     3      .or.ain(inx+1).eq.'.'.or.ain(inx+1).eq.'$'
     4      .or.ain(inx+1).eq.'('.or.ain(inx+1).eq.')'
     5      .or.ain(inx+1).eq.' '.or.ain(inx+1).eq.','
     6      .or.ain(inx).eq.'	')) then
             go to 4199
         endif
  100    inx = inx + 1
         if ((ain(inx).ge.'A'.and.ain(inx).le.'Z')
     1          .or.(ain(inx).ge.'0'.and.ain(inx).le.'9')
     2          .or.ain(inx).eq.'_'.or.ain(inx).eq.'#'
     3          .or.ain(inx).eq.'('.or.ain(inx).eq.')'
     4          .or.ain(inx).eq.'.'.or.
     5          (ain(inx).eq.'*'.and.ain(inx+1).ne.'*')) then
             if ((ain(inx).eq.')').and.(.not. wildp)) then
                 ityp = 10
                 goto 510
             endif
             length=length+1
             atok2(length) = ain(inx)
             if (ain(inx).eq.'(') wildp = .true.
             goto 100
         endif
         if ((ain(inx).eq.' ') .or. (ain(inx).eq.',')
     x       .or.(ain(inx).eq.'$').or.(ain(inx).eq.'	')
     x       .or.(ain(inx).eq.';')
     x       .or.(ain(inx).eq.char(177))) then
c
c...wildcard end, goto check next type
c
             ityp = 10
             goto 510
         else
c
c...error
c
            ifl(2)=109
            ityp=8
            isvinx=inx
            go to 99999
         endif
      endif
C                                                      ********  letter
c...Checks to see if ain(inx) is a letter, if it
c...is a letter than inx will be incremented otherwise
c...it will go to 4099 to continue check 
c
      if (ifl(191) .eq. 0) then
          if ((ain(inx).lt.'A' .or. ain(inx).gt.'Z') .and.
     1         ain(inx) .ne. '@' .and. ain(inx) .ne. '#' .and.
     2         ain(inx) .ne. '_') go to 4099
      else if (ain(inx) .eq. ',' .or.  ain(inx) .eq. '"' .or.
     1         ain(inx) .eq. char(177)) then
          go to 4099
      endif
      length=1
      atok2(1)=ain(inx)
      !atok2(1:6)=ain(1:6)
200   inx=inx+1
c
c...for case like "PPRINT***********************************"
c...in V9.6, we only have 6 chars for token label, so we can
c...have "PPRINT***********************************"
c...or ""PPRINTthis is a test"" without error
c...so keep with V9.6
c...yurong
c
      if (ifl(191) .eq. 0) then
           if (token2(1:6).eq.'PARTNO' .or.
     x        token2(1:6).eq.'PPRINT' .or.
     x        token2(1:6).eq.'INSERT' .or.
     x        token2(1:6).eq.'LETTER' .or.
     x        token2(1:6).eq.'REMARK' .or.
     x        token2(1:6).eq.'TITLES' ) then
          comment = 1
          goto 250
      endif
          
      endif
c
c...added check for wildcard such as 'x*' or 'x*y' (x could be
c...any letter, '#', '@', y could be any letter, number, '#', '_', '.')
c
      if (ain(inx).eq.'*' .and. cut.eq.0 .and. ifl(191) .eq. 0 )then
c
c......if the '*' is the last chararcter in the line, consider as wildcard
c
         if (inx.eq.nccin) then
c
c...wildcard end, goto check next type
c
c
c......Test for Macro parameter
c......As token part of wildcard
c
                ivxsub = 0
                call vstchk
                nc = strlen1(token2)
                length = nc

                length = length + 1
                atok2(length)=ain(inx)
                inx = inx + 1
                ityp = 10
                goto 510
         endif
c
c...check if the token before is a scalar, if it is, then this '*' is
c...not a wildcard symbol but a operator, goto 1099 to contine token
c...but ignore '*' ('*' as next token)
c
         if (token2(1:1) .eq. '#') then
             nc = strlen1(curmac)
             if (nc.gt.0) then
                token2 = curmac(1:nc) // token2
                len = length + nc
             else
                goto 1099
             endif
         else
             len = length
         endif
         call nclf_get_scalar_value(token2, len, val, typ)
         if ((typ.eq.1 .and. wdtype.ne.1) .or. ifl(370).eq.1) then
             goto 1099
         else
             if (token2(1:1) .eq. '#') then
                 nc = strlen1(curmac)
                 if (nc.gt.0) then
                    token2 = curmac(1:nc) // token2
                    length = length + nc
                 else
                    goto 1099
                 endif
             endif
             call nclf_get_mcpam_value(token2, length, val, typ)
             if ((typ.eq.1).and.(wdtype.ne.1)) goto 1099
             ityp = 10
             length = length + 1
             atok2(length)=ain(inx)
c
c...parsing wildcard token
c
  110        inx = inx + 1
             if ((ain(inx).ge.'A'.and.ain(inx).le.'Z')
     1          .or.(ain(inx).ge.'0'.and.ain(inx).le.'9')
     2          .or.ain(inx).eq.'_'.or.ain(inx).eq.'#'
     3          .or.ain(inx).eq.'('.or.ain(inx).eq.')'
     4          .or.ain(inx).eq.'.'.or.
     5          (ain(inx).eq.'*'.and.ain(inx+1).ne.'*')) then
                if ((ain(inx).eq.')').and.(.not. wildp)) then
                     ityp = 10
                     goto 510
                endif
                if (ain(inx).eq.'(') wildp = .true.
                length=length+1
                atok2(length) = ain(inx)
                goto 110
             endif
             if ((ain(inx).eq.' ') .or. (ain(inx).eq.',')
     x            .or.(ain(inx).eq.'$').or.(ain(inx).eq.'	')
     x            .or.(ain(inx).eq.';')
     x            .or.(ain(inx).eq.char(177))) then
c
c......Test for Macro parameter
c......As token part of wildcard
c
                if (token2(length:length) .eq. '*') then
                    token2(length:length) = ' '
                    ivxsub = 0
                    call vstchk
                    nc = strlen1(token2)
                    length = nc + 1
                    token2(length:length) = '*'
                endif
c
c...wildcard end, goto check next type
c
                ityp = 10
                goto 510
             else
c
c...error
c
               ifl(2)=109
               ityp=8
               isvinx=inx
               go to 99999
             endif
         endif
      endif
c
c......we will accept ABC(*) as wild card
c
      if ((ain(inx).eq.'(').and.(ain(inx+1).eq.'*') .and.
     1    ifl(191) .eq. 0) goto 210
c
c...After inx has been incremented it is going to check
c...again to see if it is a letter, number, or _, #,or (.)
c...if it isn't it will go to 1099.
c
      if (ifl(191) .eq. 0) then
          if ((ain(inx).lt.'A' .or. ain(inx).gt.'Z') .and.
     1        (ain(inx).lt.'0' .or. ain(inx).gt.'9') .and.
     2         ain(inx) .ne. '_' .and. ain(inx) .ne. '#' .and.
     3         ain(inx) .ne. '.') go to 1099
      else if (ain(inx) .eq. ',' .or.  ain(inx) .eq. '"' .or.
     1         ain(inx) .eq. char(177)) then
          go to 1099
      endif
c
c...Change maximum length to 20 characters
c...Bobby  -  1/4/93
c
c   210   if (length.eq.6) go to 215

c
c...Change maximum length to 63 characters
c...Yurong
c
c...when more than 63chars, we cut the token to 63chars
c...but continue parser until next symbol
c...do not output error message
c
  210 if (length.eq.63) then
          cut = 1
          go to 200
c...          goto 215
      endif
c
c...if lop is true, then this was a token that was
c...preceeded by a + or - that was followed by a digit.
c...for example "-1abc".   epm 11-15-85
c
      if (lop .and. ifl(191) .eq. 0) then
          length = 1
          inx=isvinx + 1
          ityp = 5
          go to 510
      endif

      length=length+1
      atok2(length) = ain(inx)
      go to 200
1099  continue
c
c... if inx is a '~' it means that this identifier is
c... a nested id (@pt1~~...) where the trailing '~'s
c... need to be ignored
c
215   if (ifl(191) .eq. 0) then
        if ((ain(inx).eq.'~').and.comment.ne.1) then
c     1    .or.(((ain(inx).eq.' ').or.(ain(inx).eq.'	'))   
c     2        .and.(strlen1(cin(inx:nccin)).ne.0).and.comment.ne.1)) then
          inx=inx+1
          go to 215
        endif
      endif
c
c...Call vocsch to see if it a vocabulary word
c...if lang = cadra check for vocab in the end
c
250   if (ifl(374) .eq. 0) then
        call vocsch

c
c... if token is 'tapeft' make it a scaler with the value
c... in tapnow (punched tape footage generated by motion
c...  commands to this point.)
c
        if (ityp.eq.1.and.ist.eq.660) then
            ityp=4
            tv=tapnow
        endif
      else
        ityp = 2
      endif
c
c...parse for Labels by adding a : to the end of the token if lang = cadra
c
      if (ifl(376) .ne. 0) then
         atok2(length+1) = ':'
         ityp = 2
         goto 1011
      endif
c
c...parse for macro param by adding a _ at then end of the token if lang=cadra
c
      if (ifl(374) .ne. 0 .and.(ifl(375) .ne. 0 .or.ifl(38).eq.2)) then
        atok2(length+1) = '_'
        ityp = 2
        goto 1011
      endif
c
c...If ityp=2, then vstchk is called to see if it is 
c...a predefined word 
c
 1011 if (ityp.eq.2) then
c
c...see if the label start with #
c...if yes, check the current macro and check the 
c...new label "macro#label"
c...Yurong
c
         if (token2(1:1) .eq. '#' .and. ifl(191) .eq. 0) then
            nc = strlen1(curmac)
            if (nc.gt.0) then
               token2 = curmac(1:nc) // token2
            else
               ifl(2)=54
               ityp=8
               isvinx=inx
               inx = inx + 1
               go to 99999
            endif
         endif
         ivxsub = 0
         call vstchk
      endif

c
c...Call vocsch to see if it a vocabulary word for lang = cadra
c
      if (ifl(374) .ne. 0 .and. ist.eq.1 .and. ityp.eq.2 .and.
     1    (atok2(length+1).ne.'_'.or.atok2(length+1).ne.':') .and. 
     2    .not.((length.eq.inx-1).and.(ain(inx).eq.'='))) then
        call vocsch

c
c... if token is 'tapeft' make it a scaler with the value
c... in tapnow (punched tape footage generated by motion
c...  commands to this point.)
c
        if (ityp.eq.1.and.ist.eq.660) then
            ityp=4
            tv=tapnow
        endif
      endif
c
c... if lang = cadra and a macro is being called parse for macro param 
c... if it is not a label
c
      if (ifl(376) .ne. 0 .and.atok2(length+1).eq.':'.and.
     1    ist.eq.1 .and.ityp.eq.2) then
        if (ifl(375) .ne. 0 .or.ifl(38).eq.2) then
          atok2(length+1) = '_'
        else
          atok2(length+1) = ' '
        endif
        goto 1011
      endif
c
c... if lang = cadra and a macro is being called parse token without _ 
c... if it is not a macro param
c
      if (ifl(374) .ne. 0 .and.ifl(38).eq.2 .and. atok2(length+1).eq.'_'
     1        .and. ist.eq.1 .and.ityp.eq.2) then
        atok2(length+1) = ' '
        goto 1011
      endif

      goto 4499
c
c...Checks to see if ain(inx) is a number
c...If not, it goes to 4199 for special character check
c
4099  if (.not.(ain(inx).ge.'0'.and.ain(inx).le.'9')) go to 4199

C                                                         ****** number
      length=1
      ilenth=1
 290  atok2(length)=ain(inx)
      ityp=3
300   inx=inx+1

      if((ain(inx).ge.'0'.and.ain(inx).le.'9').and.(cut.eq.0))then
          length=length+1
          ilenth=ilenth+1
          atok2(length)=ain(inx)
          go to 300
      endif
c
c...allow max 15 digits, but if more than 9 digits,
c...save as real number
c
      if ((ilenth .gt. 15).or.
     x       (length .gt. 20)) then
          ifl(2)=381
          ityp=8
          go to 99999
      endif
      if (ilenth .gt. 9) then
           if (.not.(ain(inx).eq.'.')) then
              ityp=4
              goto 3099
           endif 
      endif
      if (.not.(ain(inx).eq.'.')) go to 3099
      ilenth=-99
      if (.not.(ityp.eq.3)) go to 2099
      length=length+1
310   ityp=4
      atok2(length)='.'
      go to 300
2099  continue
      ifl(2)=109
      ityp=8
      isvinx=inx
      go to 99999
3099  continue
      if (ifl(191) .eq. 0) then
          if ((ain(inx).ge.'A' .and. ain(inx).le.'Z') .or.
     1         ain(inx) .eq. '_') go to 210
      else if (ain(inx) .ne. ',' .and.  ain(inx) .ne. '"' .and.
     1         ain(inx) .ne. char(177)) then
          go to 210
      endif

      if (ityp.eq.3) then
          call chr2int(atok2,length,itv)
          tv=itv
      else if (ityp.eq.4) then
          call chr2real(atok2,length,tv)
          itv=tv
      endif

      go to 4499
C
C... Allow parser to recognize '[' and ']'
C... Sharon - 01Jul91
c... and ':'
c... Bobby  -  7/9/91
C
4199  if (.not.((ain(inx).ge.'('.and.ain(inx).le.'/')
     x       .or.ain(inx).eq.'='.or.ain(inx).eq.'''' .or.ain(inx).eq.'^'
     x       .or.ain(inx).eq.'['.or.ain(inx).eq.']' .or.
     x       ain(inx).eq.'{'.or.ain(inx).eq.'}' .or.
     3       ain(inx) .eq. ':')) go to 4299

C                                                 **** special char
C... Checks to see if it is any of the special
C... Characters (=,+,-,*,/,(,),.,,,:,',
C... [,],^) and then sets ist if it is. 
C
      if (ain(inx).eq.'=') then
          ist = 1
      else if (ain(inx).eq.'+') then
          ist = 2
      else if (ain(inx).eq.'-') then
          ist = 3
      else if (ain(inx).eq.'*') then
c
c... Nested if
c... To check if it is Double * i.e. (**)
c
          if (ain(inx+1).eq.'*') then
              ist=10
              inx=inx+1
          else
              ist = 4
           endif
c
c... End of nest if
c
      else if (ain(inx).eq.'/') then
          ist = 5
      else if (ain(inx).eq.'(') then
          ist = 6
      else if (ain(inx).eq.')') then
          ist = 7
      else if (ain(inx).eq.'.') then
          ist = 8
      else if (ain(inx).eq.',') then
          ist = 9
      else if (ain(inx).eq.':') then
          ist = 13
      else if (ain(inx).eq.'''') then
          ist = 14
      else if (ain(inx).eq.'[') then
          ist = 15
      else if (ain(inx).eq.']') then
          ist = 16
   	else if (ain(inx).eq.'^') then
       	ist = 17
   	else if (ain(inx).eq.'&') then
       	ist = 18
      else if (ain(inx).eq.'{') then
          ist = 19
      else if (ain(inx).eq.'}') then
          ist = 20
      endif
c
c... The end of the special character check
c
      atok2(1)=ain(inx)

      if (ist.eq.10) then
          atok2(2)='*s'
      endif

      if ((ist.eq.8).and.
     x    (ain(inx+1).ge.'0'.and.ain(inx+1).le.'9')) then
          ist = 1
          length=1
          ilenth=-99
          go to 310
      endif

      if (ist.eq.2.or.ist.eq.3) then
c
C...Nested if
c
          if (ain(inx+1).ge.'0'.and.ain(inx+1).le.'9') then
              length=2
              ilenth=1
              inx=inx+1

              lop = .true.
              go to 290
          endif
c
C... End of nested if
C... Second nested if
c
         if (ain(inx+1).eq.'.'.and.ain(inx+2).ge.'0'.and.
     1        ain(inx+2).le.'9') then
              atok2(1)=ain(inx)
              inx=inx+1
              length=2
              ilenth=-99
             go to 310
         endif
c
C... End of second nested if
c
      endif

      ityp=5
      inx=inx+1
      length=1
      go to 4499
c
c...  Added to handle STRING (ityp = 9). Paul.
c
4299  if (ain(inx).eq.'"') then
         inx = inx + 1
         ilenth = inx+MAX_FILE
         if (ilenth.gt.1536) ilenth = 1536
         j = 0
         qflg = .false.
         tokstr = ' '
         do 4300 i = inx,ilenth
            if(ain(i) .eq. '"') then
              if (ain(i+1).eq.'"') then
                if (qflg) then
                  qflg = .false.
                  goto 4300
                 endif
                 qflg = .true.
              else
                if (qflg) then
                  qflg = .false.
                  goto 4300
                endif
                goto 4350
              endif
            endif
            j = j+1
            if (j .le. 64) atok2(j) = ain(i)
            tokstr(j:j) = ain(i)
4300     continue
c
         ifl(2)=441
         ityp=8
         isvinx=inx
         go to 99999
c
4350     ityp = 9
         length = j
         inx = i+1
         go to 4499
c
      else if (ain(inx).eq.' '.or.ain(inx).eq.'%'.or.ain(inx).eq.'$'
     1    .or.ain(inx).eq.';'.or.ain(inx).eq.'	'.or.
     2    ain(inx).eq.char(177)) then
         ityp=7
         nextyp=11
         if (ain(inx).eq.';') ncsfl(3) = 1
         goto 99999
      endif
C                                                    *** unrecognizable
         ityp=6
         atok2(1)=ain(inx)
         length=1
         inx=inx+1
4499  continue

510    if ((ain(inx).eq.'~').and.comment.ne.1) then
c     1    .or.(((ain(inx).eq.' ').or.(ain(inx).eq.'	'))   
c     2        .and.(strlen1(cin(inx:nccin)).ne.0).and.comment.ne.1))
c                then
          inx=inx+1
          go to 510
      endif     
c
c...Special characters in text commands are part of text
      if (ityp .eq. 1 .and. inx .eq. 7 .and.
     1    (ist .eq. 1045 .or. ist .eq. 1044 .or. ist .eq. 1046 .or.
     2     ist .eq. 1043 .or. ist .eq. 1091 .or. ist .eq. 1092)) then
          nextyp = 11
          goto 99998
      endif
c
C... Sets nextyp for special characters
c
      if (ain(inx).eq.',') then
         nextyp=9
      else if (ain(inx).eq.'/') then
         nextyp=5
      else if (ain(inx).eq.'=') then
         nextyp=1
      else if (ain(inx).eq.' '.or.ain(inx).eq.'%'.or.ain(inx).eq.'$'
     1    .or.ain(inx).eq.';' .or. ain(inx).eq.'	' .or.
     2    ain(inx) .eq. char(177)) then
         nextyp=11
         if (ain(inx).eq.';') ncsfl(3) = 1
      else if (ain(inx).eq.'+') then
         nextyp=2
      else if (ain(inx).eq.'-') then
         nextyp=3
      else if (ain(inx).eq.':') then
         nextyp=13
      else if (ain(inx).eq.'*') then
c
C...Checking to see if it is double ** i.e.(**)
c
         if (ain(inx+1).eq.'*') then
             nextyp=10
         else
             nextyp=4
         endif

      else if (ain(inx).eq.'(') then
         nextyp=6
      else if (ain(inx).eq.')') then
         nextyp=7
      else if (ain(inx).eq.'.') then
         nextyp=8
      else if (ain(inx).eq.'''') then
       nextyp=14
      else if ((ain(inx).ge.'A'.and.ain(inx).le.'Z').or.
     1         (ain(inx).ge.'0'.and.ain(inx).le.'9').or.
     2         (ain(inx).eq.'#') ) then
c
C... Nextype = 12, means that it is a letter or number 
c
         nextyp=12
      else if (ain(inx).eq.'[') then
        nextyp = 15
      else if (ain(inx).eq.']') then
        nextyp = 16
      else if (ain(inx).eq. '^') then
        nextyp = 17
      else if (ain(inx).eq. '&') then
        nextyp = 18
      else if (ain(inx).eq. '{') then
        nextyp = 19
      else if (ain(inx).eq. '}') then
        nextyp = 20
      endif

99998 if (nextyp.ne.0.and.nextyp.eq.ifl(44)) inx=inx+1

99999 continue
		pscalar = .false.
		rightp = .false.
      if ((ityp .eq. 2 .and. ist.eq.2).or.
     x      ityp .eq. 3 .or. ityp .eq. 4) pscalar = .true.
      if (ityp .eq. 5 .and. ist.eq.7) rightp = .true.
      if (debug) then
         write (cout,9010) token2,ityp,ist,ifl(44),nextyp,inx
9010      format ('parser token=',a6,' ityp=',i2,' ist=',i4,
     1            ' ifl(44)=',i2,' nextyp=',i4,' inx=',i3)
         call putmsg (cout,80,22,0)
      endif

      return
      end
