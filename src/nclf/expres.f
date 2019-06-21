C*********************************************************************
C*    NAME         :  expres.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       expres.f , 25.1
C*     DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:02
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine expres
C*      This routine handles arithmetic expressions.  It is called by
C*      the PARSER and it returns the value of the expression in TV
C*      with ITYP equal to 4.  The basic philosophy is to parse the
C*      expression and build up the stack array, which holds the nest level
C*      and the associated operator, and the value array, which holds the
C*      values of the corresponding operands.  See the EVAL.F routine
C*      for details on the contents and usage of the stack arrays.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine expres (incsub)

      include 'com8a.com'

      integer*2 incsub

      integer*2 opers(80), irest(4)
      integer*2 opptr,isv
      integer*4 jsif,jfl91, sinx, svsb
      real*8 ropers(20), values(21),rtv
      equivalence (ropers, opers)
      character*64 stok, tempid, cvals(20), svid
      character*2 atoken
      character*3 btoken
      logical sub, lsub, ldfl

      equivalence (rest,irest)
      equivalence (jfl91,ifl(9))
      equivalence (token2,atoken,btoken)

      integer*2 i2v0/0/,i2v1/1/

      integer*2 MAX1F, MIN1F, SIGNF, LNTHF, ANGLF
      parameter (MAX1F=570, MIN1F=571, SIGNF=572, LNTHF=560, ANGLF=559)
      integer*2 THRU, ALL, DISTF, TYPEF, SRFTYP
      parameter (THRU=152, ALL=816, DISTF=575, TYPEF=98, SRFTYP=875)
      integer*2 VOCABF, FINDEXV, RINDEXV, STRCMPV
      parameter (VOCABF=549,FINDEXV=573,RINDEXV=574,STRCMPV=581)
      integer*2 TEXTF, XTYPE, TDISTF
      parameter (TEXTF=38, XTYPE=99, TDISTF=585)
c
C...Initialization
c
      isvifl=ifl(44)
      ifl(210)=0
      ifl(44)=0
      ilvl=0
      sub=.false.
      lsub=.false.
      opptr=0

c
c... Set operation and operands array stacks to zeros.
c
      do 10,i=1,20
          ropers(i)=0.
          values(i)=0.
10    continue

c
c... If the token is a ( then bump the nest level counter.
c
100   if (ityp .eq. 5 .and.ist .eq. 6) then 
          ilvl=ilvl+1
c
c......Protect against subscripted DATA scalars (d(IPT))
c
          ldfl = ldtflg
          call parser
          ldtflg = ldfl
          go to 100
      endif

c
c... If the token is a singel quote (') then check if the
c... next token is an 'NOT' operand
c
      if (ityp .eq. 5 .and. ist .eq. 14) then
          call parser
          if(length .eq.3 .and. btoken .eq. 'NOT') then
              call parser
              if (ityp .eq. 5 .and. ist .eq. 14) then
                  call parser
              else
                  ifl(2) = 404
                  go to 99999
              endif
           opers(opptr*4+1)=ilvl
           opers(opptr*4+2)=-2
           opers(opptr*4+3)=9
           opptr=opptr+1
           go to 100
          else
              ifl(2) = 404
              go to 99999
          endif
      endif

c
c...If the token is a plus or minus sign (+, -) then 
c...load that operator in the stack.
c
      if (ityp .eq. 5 .and.
     x    (ist .eq. 2 .or.
     x     ist .eq. 3)) then
          if (ist .eq. 3) then
              values(opptr+1)=-1.
              opers(opptr*4+1)=ilvl
              opers(opptr*4+2)=4
              opptr=opptr+1
          endif
          call parser
          go to 100
      endif
c                                                         *** FUNCTION

c
c... If the token is a function then call the appropriate
c... function routine.
c
      if (ityp.eq.1) then

        if (voc .eq. 561) then
          call dotf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. 563) then
          call numf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. 565) then
          call atang2f
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. 566) then
          call canf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. MAX1F .or. voc.eq.MIN1F) then
          call nmxmnf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. SIGNF) then
          call nsignf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. LNTHF) then
          call nlnthf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. ANGLF) then
          call nanglf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. DISTF) then
          call ndistf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif
        
        if (voc .eq. TDISTF) then
          call tndistf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. TYPEF) then
          call ntypef (rtv)
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. XTYPE) then
          call xtypef
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. VOCABF) then
          call nvcabf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. FINDEXV) then
          call findexf(i2v0)
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. RINDEXV) then
          call findexf(i2v1)
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. STRCMPV) then
          call strcmpf
          if (ifl(2) .ne. 0) go to 99999
          ifl(210) = 1
          go to 140
        endif
c
c...jingrong primitive type
c
        if (voc .eq. SRFTYP) then
          call nsrftyp
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. TEXTF) then
          call ntextf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .ge. 550 .and. voc .lt. 570) then
          values(opptr+1)=0.
          opers(opptr*4+1)=ilvl
          opers(opptr*4+2)=ist
          opptr=opptr+1
          isvix=opptr
          call parser
          go to 100
        endif

        if (lsub.and.(voc.eq.ALL.or.voc.eq.THRU)) then
C
C...If this is an ALL, make sure to get the lowest subscript number,
C...this isn't neccessarily always going to be 1 as it was
C...set before. JLS 4/20/99
C
          if (voc.eq.ALL) call subchk(tempid,ivxsub)
          if (ivxsub.gt.0) then
             values(opptr+1) = ivxsub
             ivxsub = 0
          else
             values(opptr+1) = 1.0d0
          endif
          incsub = 2
          if (voc.eq.ALL) incsub = 3
          goto 160
        endif

      endif
c                                                         *** SUBSCRIPT

c
c...If the token an identifier and the next token is
c...a left parenthesis then check if it is undefined
c...or a valid subscripted identifier.
c
      if (ityp .eq. 2 .and.
     x    nextyp .eq. 6) then
          if (ist .eq. 1) then
            if (ifl(370).ne.1) then
              isv = idst
              stok = token2
              sinx = ivxsub
              jsif = jfl91
c102706
              svid=savid2
              svsb=isvsub
              savid2=token2
              isvsub=0
              ifl(9)=ifl(11)
              ifl(10)=ifl(12)
              idst=14
              rest=0.
              irest(3)=32767
              irest(4)=14
              call vstore
              idst=isv
              jfl91 = jsif
              token2 = stok
              ivxsub = sinx
              savid2=svid
              isvsub=svsb
            endif
          else if (ist .eq. 14) then
              tempid = token2
          else
              ifl(2)=89
              goto 99999
          endif
          cvals(opptr+1)=token2
          opers(opptr*4+1)=ilvl
          opers(opptr*4+2)=12
          opptr=opptr+1
          lsub = .true.
          call parser
          go to 100
      endif

c
c... If the token is not a scalar variable or value then
c... go to the next operation.
c
      if (.not. ((ityp .eq. 2 .and. ist .eq. 2) .or.
     x           (ityp .eq. 3 .or.
     x            ityp .eq. 4))) go to 2099
140       values(opptr+1)=tv

c
c... If the next token is a valid operand then go 
c... store it in the operation array.
c... Valid operands are: (+, -, *, /, (, ), **, ' , ^)
c... The (^) has been added to convert seconds into degrees.
c
150       if (.not. (nextyp .lt. 2 .or.
     x              (nextyp .gt. 7 .and.
     x               nextyp .ne. 10 .and.
     x               nextyp .ne. 14 .and.
     x               nextyp .ne. 17))) go to 1099

c
c...Store the end of stack pointer value (11) then go
c...process operation stack.
c
160   opers(opptr*4+1)=ilvl
      opers(opptr*4+2)=11
      if (isvifl .eq. nextyp) inx=inx+1
      go to 200

1099  continue
      if (nextyp.eq.7 .and. ilvl.eq.0) then
        if (incsub.gt.1 .or. lexpr) goto 160
      endif
      call parser
      if (ityp .eq. 5 .and.
     x     ist .eq. 7) then
          if (ilvl .lt. 1 .and.
     x        ifl(111) .ne. 0) then
              nextyp=7
              inx=inx-1
              go to 160
          endif
          ilvl=ilvl-1
          if (ldtflg.and.lsub.and.ilvl.eq.0) goto 160
          go to 150
      endif
      opers(opptr*4+1)=ilvl
c
c...Is the token a special character of scalar value?
c...This begins large if statement
c
      if (ityp .eq. 5 .or. ityp .eq. 3 .or.ityp .eq. 4) then 
c
c...Check to see if it is a  carret 
C...and set opers(opptr*4+2) =13
c...so that in eval second conversion is done 
c
          if (ityp .eq. 5 .and. ist .eq.17) then 
              opers(opptr*4+2)=13
c
c...Single quote '
c
          else if (ityp .eq. 5 .and. ist .eq. 14) then 
c
c...Call parser to check if it is a number
c...following the single quote
c...If it is a number, then the number is in minutes and
C...will be converted to degrees
c
              call parser 
c
C...If it is a number then set opers(opptr*4+2) = 14 
C...so that in eval conversion is done.  Else if
C...the token is a single quote(') then parse for a
C...logical operator.
C
              if (ityp.eq.3 .or. ityp.eq.4) then
                  opers(opptr*4+2) = 14
c
C...Reset inx
c
                 inx=isvinx
              else
                 opers(opptr*4+2)=-1
c
c... If a logical operator is seen, set ifl(210) 
c... to 1 for the CONDIF routine.
c
                 Ifl(210)=1
                 if (length .eq. 2) then
                    if (atoken .eq. 'LT') then
                      opers(opptr*4+3) = 1
                    else if (atoken .eq. 'GT') then
                      opers(opptr*4+3) = 2
                    else if (atoken .eq. 'EQ') then
                      opers(opptr*4+3) = 3
                    else if (atoken .eq. 'NE') then
                      opers(opptr*4+3) = 4
                    else if (atoken .eq. 'GE') then
                      opers(opptr*4+3) = 5
                    else if (atoken .eq. 'LE') then
                      opers(opptr*4+3) = 6
                    else if (atoken .eq. 'OR') then
                      opers(opptr*4+3) = 7
                      opers(opptr*4+2) = -3
                    else
                      ifl(2) = 404
                      go to 99999
                    endif
                 else if (length .eq.3 .and. btoken .eq. 'AND') then
                    opers(opptr*4+3) = 8
                    opers(opptr*4+2) = -3
                 else
                   ifl(2) = 404
                  go to 99999
                 endif


c
c...Check to make sure logical operator is 
c...followed by a single quote (').
c
                 if (nextyp .ne. 14) then
                   ifl(2) = 404
                   go to 99999
                 else 
                   call parser
                 endif
              endif

          else
              opers(opptr*4+2)=ist
          endif
c
c... Is the token a scalar value?
c
          if (ityp .eq. 3 .or. ityp .eq. 4) then 
              tv=dabs(tv)
              go to 190
          endif
      endif
c
c... The end of the large if statement
c
      call parser
      go to 2199
2099  continue

      if (ityp .eq. 2 .and.
     x     ist .eq. 1) then
          ifl(2)=9
      else
          ifl(2)=61
      endif

      go to 99999

2199  continue
190   opptr=opptr+1
      go to 100

c ******************************************************************
c     The operation/operand array stacks are now built.
c     Call GETNXT and EVAL to process them.
c ******************************************************************

200   if (ilvl .ne. 0 .and.
     x    ifl(111) .eq. 0) then
          if (lsub.and.ilvl.eq.1) then
            if (incsub.eq.0) incsub = 1
            ilvl = 0
          else
            ifl(2)=60
            go to 99999
          endif
      endif
      values(21)=values(1)
      do 400,i=1,20

c
c...Point to the next sequential operation.
c
          call getnxt(opers, values, opptr)
          if (opptr .eq. 0) then
              if (sub .or.ifl(379).ne.0) then
C                  ityp=2
              else
                  tv=values(21)
c
c...Changed logic to use dint()
c...because of integer overflow on the VAX
c...Bobby  -  10/7/93
c
                   ityp   = 4
                   if (dint(tv) .eq. tv) then
                       call rtoi (tv,itv)
                       ityp   = 3
                   endif
c                  itv=tv
c                  values(21)=itv
c                  if (values(21) .eq. tv) then
c                      ityp=3
c                  else
c                      ityp=4
c                  endif
              endif
              go to 410
          endif

c
c...Evaluate the operation.
c
          call eval(opers, values, cvals, opptr, sub)
          if (ifl(2) .ne. 0) go to 410
400   continue
410   continue
      if (incsub.ne.1 .or. sub) goto 99999
c
C...error - invalid syntax construct.
c
9061  ifl(2) = 61

99999 ifl(44)=isvifl
      if (ifl(2) .ne. 0) then
          if (ifl(111) .ne. 0 .or.
     x        ifl(38)  .eq. 1 .or.
     x        ifl(45)  .eq. 1) then
              ifl(2)=0
              errcom=' '
              ityp=6
          else
              err=.true.
          endif
      endif

      return
      end
