C*********************************************************************
C*    NAME         :  getnxt.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       getnxt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:08
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getnxt (opers, values, opptr)
C*      This routine searches opers and returns the index of the
C*      element with the highest level and the highest operator
C*      within that level.
C*    PARAMETERS
C*       INPUT  :
C*          opers
C*          values
C*          opptr
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine getnxt (opers, values, opptr)

      include 'com8a.com'

      integer*2 opers(80), opptr
      real*8 values(21)

      opptr=0
      j=0

c             The operators that are minus are the logical operators
c               - those with a value of -1 are lt,gt,ne,eq,ge,le
c               - a value of -2 is 'not'
c               - those with a value of -3 are 'and' & 'or'.
c             The theory is to handle all of the expression operators first,
c             and then handle the logical operators.     epm    3-20-90
      k=-4
      do 100,i=0,19
          if (opers(i*4+2).eq.11) go to 99999
          if (opers(i*4+2).ne.0) then
              if (opers(4*i+1).gt.j) then
                  j=opers(4*i+1)
                  k=opers(4*i+2)
                  opptr=i+1
              else if (opers(4*i+1).eq.j) then
                  if (opers(4*i+2).gt.k) then
                      k=opers(4*i+2)
                      opptr=i+1
                  endif
              endif
          endif
100   continue
99999 if (opptr.eq.0) values(21)=values(i+1)

      return
      end
