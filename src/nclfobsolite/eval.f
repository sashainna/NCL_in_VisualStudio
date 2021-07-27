C*********************************************************************
C*    NAME         :  eval.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       eval.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:01
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine eval(opers,values,opptr,sub)
C*    PROCESSING:
C*      This routine performs a single operation.  There are two arrays
C*      that contain operation information (the OPERS array) and data
C*      to operate on (the VALUES array).  Each operation consists of
C*      an operation and two operands.  The operation to perform is
C*      at the entry in the OPERS array pointed to by OPPTR.  The first
C*      operand is the entry in the VALUES array pointed to by OPPTR.
C*      The second operand that is used is selected by searching the 
C*      operation portion of the OPERS array starting at the OPPTR'th 
C*      entry and continuing to the end of the array.  A non-zero 
C*      operation entry in the OPERS array denotes the corresponding 
C*      VALUES array entry is the operand to use.
C*
C*      The res .lt.  of the operation are stored in the VALUES array 
C*      at the location of the second operand.
C*
C*         OPERS array format:
C*
C*           There are 20 real*8 entries broken into 4 integer*2 
C*           values for a total of 80 integer*2 values.  Each 
C*           real*8 entry stands for 1 operation:
C*
C*             1st        2nd        3rd        4th
C*             I*2        I*2        I*2        I*2
C*
C*            Level    Operation   Operator   Not used
C*             
C*              where:
C*
C*                Level:
C*                  Is the depth of nested operation.  That is how
C*                  many levels of nested parenthesis the operation is.
C*
C*                Operation:
C*                  Is the class of operation to be performed.  This 
C*                  can be one of the following:
C*                     -1 = Logical operation (i.e. GT, EQ, AND)
C*                     -2 = Logical NOT operation
C*                     -3 = Logical AND or OR operation
C*                      2 = ADD
C*                      3 = SUBTRACT
C*                      4 = MULTIPLE
C*                      5 = DIVIDE
C*                     12 = SUBSCRIPT (load value of subscripted scalar
C*                                     in VALUES entry)
C*                     13 = CONVERSION of SECONDS into DEGREES
C*                     14 = CONVERSION of MINUTES into DEGREES
C*                     10 = EXPONENTIATE (raise to the power of) 
C*                    550 = EXPF
C*                    551 = LOG
C*                    552 = ABS (absolute value)
C*                    553 = ATAN (arc tangent)
C*                    554 = TAN
C*                    555 = SQRT
C*                    556 = COS
C*                    557 = SIN
C*                    558 = ASIN (arc sin)
C*                    562 = ACOS (arc cos)
C*                    564 = INT (integer value of real)
C*                    567 = NINT (rounded integer value of real)
C*                    568 = LOG10F (log base 10)
C*                    569 = CBRTF (cube root)
C*        
C*                Operator:
C*                  Is the kind of logical operation to be performed.
C*                  This can be one of the following:
C*                      1 = LT
C*                      2 = GT
C*                      3 = EQ
C*                      4 = NE
C*                      5 = GE
C*                      6 = LE
C*                      7 = OR
C*                      8 = AND
C*                      9 = NOT
C*                  Non-logical operations do not use this field.
C*
C*    PARAMETERS
C*       INPUT  :
C*          opers
C*          cvals
C*          opptr
C*       OUTPUT :
C*          values
C*          sub
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine eval (opers, values, cvals, opptr, sub)

      include 'com8a.com'

      integer*2 opers(80), ktv(4), oprtn, oprtor
      integer*2 i, jj, k, l, op1, op2, opptr, iex
      real*8 values(21), tval, tdif, ttol, riex
      character*64 cvals(21)
      logical sub

      equivalence (ktv, tv)

      integer*2 LOG10F, EXPF, CBRTF
      parameter (LOG10F = 568, EXPF = 550, CBRTF = 569)

      op1 = opptr
      oprtn  = opers((opptr-1)*4+2)
      oprtor = opers((opptr-1)*4+3)

c
c...Find the next OPERS array entry that has a non-zero operation
c...class.  This will give the index into the VALUES array that
c...contains the second operand to use.
c
      do 10,i=opptr,20
          if (opers(i*4+2) .ne. 0) then
              op2=i+1
              go to 11
          endif
10    continue
11    continue

      sub=.false.
      
      if (oprtn .lt. 0) then
c                                                **********************
c                                                   logical operator
c                                                **********************
c...a value of -1 is false, 1 is true
c
          tval = -1
c
c... for cadra 
c... a value of 0 is false, 1 is true
c
          if (ifl(374).ne.0) tval = 0
          tdif = values(op1)-values(op2)
          ttol = sc(170)
          if (sc(169).lt.8.29999) ttol = 0.0d0
c                                                         'lt' 
          if (oprtor .eq. 1) then
              if (tdif .lt. -ttol) tval=1
c                                                         'gt' 
          else if (oprtor .eq. 2) then
              if (tdif .gt. ttol) tval=1
c                                                         'eq' 
          else if (oprtor .eq. 3) then
              if (dabs(tdif) .le. ttol) tval=1
c                                                         'ne'
          else if (oprtor .eq. 4) then
              if (dabs(tdif) .gt. ttol) tval=1
c                                                         'ge'
          else if (oprtor .eq. 5) then
              if (tdif .ge. -ttol) tval=1
c                                                         'le'
          else if (oprtor .eq. 6) then
              if (tdif .le. ttol) tval=1
c                                                         'or'
          else if (oprtor .eq. 7) then
              if (values(op1) .eq. 1 .or.
     x            values(op2) .eq. 1) tval=1
c                                                         'and'
          else if (oprtor .eq. 8) then
              if (values(op1) .eq. 1 .and.
     x            values(op2) .eq. 1) tval=1
c                                                         'not'
          else if (oprtor .eq. 9) then
              tval = -values(op2)
          endif
          values(op2)=tval
c                                                **********************
c                                                  arithmetic operator
c                                                **********************
      else if (oprtn .eq. 2) then
c                                                    add
         values(op2)=values(op1)+values(op2)
c                                                    subtract
      else if (oprtn .eq. 3) then
         values(op2)=values(op1)-values(op2)
c                                                    multiply
      else if (oprtn .eq. 4) then
         values(op2)=values(op1)*values(op2)
c                                                    divide
      else if (oprtn .eq. 5) then
          if (values(op2) .eq. 0) then
              ifl(2)=63
          else
              values(op2)=values(op1)/values(op2)
          endif 
C                                                    Convert seconds
c...Converts the seconds into degrees
c
      else if (oprtn .eq. 13) then
         if (values(op1) .ge. 0) then
             values(op2)=values(op1) + (values(op2)/3600)
         else
             values(op2)=values(op1) - (values(op2)/3600)
         endif
C                                                    Convert minutes
C...Converts the minutes into degress
c
      else if (oprtn .eq. 14) then
         if (values(op1) .ge. 0) then
             values(op2)=values(op1) + (values(op2)/60)
         else
             values(op2)=values(op1) - (values(op2)/60)
         endif


c                                                **********************
c                                                miscellaneous operator
c                                                **********************
c                                                    subscript
      else if (oprtn .eq. 12) then
cuni         rtoken=values(op1)
          token2 = cvals(op1)
          ivxsub = 0
c
c...check the reserv params
c
          call vstchk
          if (values(op2) .gt. 1000000.) then
              ifl(2) = 85
              go to 99999
          endif
          ivxsub=values(op2)
          if (ivxsub .lt. 1 .or.
     x        ivxsub .gt. 1000000) then
               write(errcom,201) ivxsub,1000000
201            format('sub:',i5,' reserv:',i5)
               ifl(2)=85
               go to 99999
          endif
          call vstchk
          if (ist .eq. 2) then
              values(op2)=tv
              ityp=2
          else
              l=0
              do 100 k=1,20
                  if (opers(k*4-2) .ne. 0) then
                      l=l+1
                      if (opers(k*4-2) .eq. 11) go to 110
                  endif
100           continue
110           if (l .gt. 2) then
                  if (ist .eq. 1) then
                      ifl(2)=9
                  else
                      ifl(2)=7
                  endif
                  go to 99999
              endif
          endif
          sub=.true.
c                                                    exponent
      else if (oprtn .eq. 10) then

c
c...if the base is negative, check for an integer exponent
c
          if (values(op1) .lt. 0.) then
              iex=values(op2)
              riex=iex

c
c...if exponent is integer use integer*2 to raise to
c...exponent
c
              if (riex .eq. values(op2)) then
                  values(op2)=values(op1)**iex

c
c...if exponent is not integer issue error message
c
              else
                  ifl(2)=211
              endif

c
c...if base is not negative use real*8 to raise to exponent
c
          else
              values(op2)=values(op1)**values(op2)
          endif

c                                                **********************
c                                                   function operator
c                                                **********************
c                                                    log
      else if (oprtn .eq. 551) then
          values(op2)=dlog(values(op2))
c                                                    abs
      else if (oprtn .eq. 552) then
          values(op2)=dabs(values(op2))
c                                                    atan
      else if (oprtn .eq. 553) then
          values(op2)=datan(values(op2))
c              convert to degrees
          values(op2)=values(op2)*57.295779513d0
c                                                    tan
      else if (oprtn .eq. 554) then
c           convert to -180 to 180 range and give error
c           if close to -90 or 90
          jj=values(op2)
          jj=(jj/180)*180
          values(op2)=values(op2)-jj
          if (dabs(90.-dabs(values(op2))) .lt. 1.d-6) then
              ifl(2)=314
              go to 99999
          endif
c
c...convert to radians
c
          values(op2)=values(op2)*.01745329252d0
          values(op2)=dtan(values(op2))
c                                                    sqrt
      else if (oprtn .eq. 555) then
          if (values(op2) .lt. 0) then
              ifl(2)=64
          else
              values(op2)=dsqrt(values(op2))
          endif
c                                                    cos
      else if (oprtn .eq. 556) then
c
c...convert to radians

          values(op2)=values(op2)*.01745329252d0
          values(op2)=dcos(values(op2))
c                                                    sin
      else if (oprtn .eq. 557) then
c
c...convert to radians
c
          values(op2)=values(op2)*.01745329252d0
          values(op2)=dsin(values(op2))
c                                                    asin
c...added a check for incorrect values. kathy
c
      else if (oprtn .eq. 558) then
          if (values(op2) .lt. -1 .or.
     x        values(op2) .gt. 1) then
              ifl(2)=364
              goto 99999
          endif

          values(op2)=dasin(values(op2))
c
c...convert to degrees
c
          values(op2)=values(op2)*57.295779513d0
c                                                    acos
c...added a check for incorrect values. kathy
c
      else if (oprtn .eq. 562) then
          if (values(op2) .lt. -1 .or.
     x        values(op2) .gt. 1) then
              ifl(2)=365
              goto 99999
          endif

          values(op2)=dacos(values(op2))
c
c...convert to degrees
c
          values(op2)=values(op2)*57.295779513d0
c                                                    int
      else if (oprtn .eq. 564) then
          call rtoi (values(op2),itv)
          values(op2)=itv
c                                                    nint
      else if (oprtn .eq. 567) then
          tval = values(op2)
          if (tval.ge.0.0d0) then
            tval = tval+.5d0
          else
            tval = tval-.5d0
          endif
          call rtoi (tval,itv)
          values(op2)=itv
c                                                    LOG10
      else if (oprtn .eq. LOG10F) then
          if (values(op2) .le. 0.0d0) then
              ifl(2)=112
          else
              values(op2)=dlog10(values(op2))
          endif
c                                                    EXP
      else if (oprtn .eq. EXPF) then
          values(op2)=dexp(values(op2))
c                                                    CBRT
      else if (oprtn .eq. CBRTF) then
          tval = values(op2)
          values(op2)=dabs(tval)**(1.d0/3.d0)
          if (tval.lt.0.0d0) values(op2)=-values(op2)
      endif
      opers((opptr-1)*4+2)=0

99999 continue
      return
      end
