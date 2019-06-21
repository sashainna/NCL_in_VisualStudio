C*********************************************************************
C*    NAME         :  lrfind.f
C*       CONTAINS:
C*    COPYRIGHT 1991 (c) Numerical Control Computer Sciences
C*    MODULE NAME AND RELEASE LEVEL
C*       lrfind.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:15
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE: subroutine lrfind (tname, ifnd)
C*
C*    PURPOSE
C*       To determine whether a macro is currently being called.
C*                                                                    
C*       See LRPUSH.FOR for documentation on the format of the stack.
C*
C*    PARAMETERS   
C*       INPUT  : 
C*          TNAME = Character*64                                      
C*                    The name of the MACRO to find
C*       OUTPUT :  
C*          IFND  = Integer*2
C*                    1 iff macro is currently being called; else 0.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine lrfind (tname, ifnd)

      include 'com.com'

      character*64 tname
      integer*2 ifnd

      integer*2 enttot, stkrec, itsv, issv
      integer*4 sinx
c
      character*64 rstr
      character*64 svtok
c
      ifnd = 0
c          Load total number of entries in looping region stack.
      enttot = ifl(174)
      if (enttot .eq. 0) goto 99999

c          Load number of first Ranfil record of looping region stack.
      stkrec = ifl(66)
      if (stkrec .eq. 0) goto 99999

c          Read a stack Ranfil record.
100   call getran (jbr, stkrec)
      rstr = jbc(1:64)         
      if (rstr .eq. tname) then
          ifnd = 1
          goto 99999
      endif
c          Load the pointer to a record containing more stack data.
      stkrec = jb(143)

c          Decrement the number of total entities by the 11 that were
c          checked in the last record.  Either read and scan another record
c          if there is one or exit the routine if all entries have been
c          checked.
      enttot = enttot - 1
      if (enttot .gt. 0) goto 100
c
c...Macro is not found
c...Make sure it is defined
c
99999 if (ifnd .eq. 0) then
          svtok = token2
          sinx = ivxsub
          token2 = tname
          itsv  = ityp
          issv  = ist
          ivxsub = 0
          call vstchk
          if (ityp .ne. 2 .or. ist .ne. 11) ifnd = -1
          token2 = svtok
          ivxsub = sinx
          ityp  = itsv
          ist   = issv
      endif
c
c...End of routine
c
      return
      end
