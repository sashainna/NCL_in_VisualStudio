C*********************************************************************
C*    NAME         :  lrpush.f
C*       CONTAINS:
C*    COPYRIGHT 1991 (c) Numerical Control Computer Sciences
C*     MODULE NAME AND RELEASE LEVEL 
C*       lrpush.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:15
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE: subroutine lrpush (tname, tsub, lname, lsub, stnum, nxtnum,
C*                                     endnum)
C*
C*    PURPOSE
C*       The stack of data is kept in a chained set of Ranfil records.
C*       The first Ranfil record of the stack is pointed to by IFL(66
C*       Each Ranfil record can hold up to 8 sets of looping region
C*       data.  Each set consists of 32 bytes of information in the
C*       format:                                                  
C*       Bytes    Contents                                       
C*       1-64    Looping region type indicator:                  
C*              The MACRO name if it is a MACRO region.        
C*              The ending statement label name if it is a    
C*              DO LOOP region.                              
C*              The word "LOOPST  " if it is a LOOPST/LOOPND
C*              region.                                    
C*       65-128   Looping region specific information:      
C*              Blank if it is a MACRO region.           
C*              DO LOOP limit variable name if it is a DO LOOP
C*              region.                                      
C*              Blank if it is a LOOPST/LOOPND region.      
C*       129-192 (33-40)  ON/ERROR label.
C****
C*       193-196 (17-20)  Starting statement number of looping region
C*              This is typically what IFL(67) will need to
C*              contain.                                  
C*       197-200 (21-24)  First executable statement of the looping region.
C*       201-204 (25-28)  Ending statement number of the looping region.  
C*              This is typically what IFL(68) will need to    
C*              contain.                                      
C*       205-206 (29-30)  ON/ERROR mode (STOP, CONTIN, LABEL).
C*       207-208 (31-32)  ON/ERROR message output (WARN, NOWARN).
C*       212-215 jb4(53)  tname's subscription
C*       216-219 jb4(54)  lname's subscription
C*                                                       
C*     The 143rd integer*2 of each Ranfil record contains the record
C*     number of the next chained record in the stack or zero if   
C*     it is the last record of the stack.                      
C*
C*    PARAMETERS   
C*       INPUT  : 
C*          TNAME = Character*64                                      
C*                    The name of the MACRO if it is the stack entry
C*                     of a MACRO.                                 
C*                    The DO LOOP ending label name if it is the stack
C*                     entry of a DO LOOP.                           
C*                    The value "LOOPST  " if it is the stack entry of
C*                     a LOOPST.                                     
C*          LNAME = Character*64                                     
C*                    Blank if a MACRO or LOOPST/LOOPND entry.     
C*                    The name of the limit variable if it is a DO
C*                     LOOP entry.                               
C*          STNUM = Integer*4                                   
C*                    Number of looping region's beginning statement
C*                    number.                                      
C*          NXTNUM = Integer*2                                    
C*                    Number of first executable statement in looping
C*                    region.                                       
C*          ENDNUM = Integer*2                                     
C*                    Number of looping region's ending statement.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine lrpush (tname, tsub, lname, lsub, stnum, 
     x               nxtnum, endnum)

      include 'com4a.com'

      character*64 tname, lname,csc155
      equivalence (sc155,csc155)
      integer*2 enttot, stkrec, savrec
      integer*4 stnum, nxtnum, endnum, tsub, lsub
c **********************************************************************

c          Bump the number of looping region stack entries by 1.
      ifl(174) = ifl(174) + 1
      enttot = ifl(174)

c          Load number of first Ranfil record of looping region stack.
      stkrec = ifl(66)
c
c.....Added for temporary variables in loops - ASF 12/31/13
c
      if (ifl(38).eq.0.and.enttot.eq.1) curmac = tname
c          If no looping region stack has been started yet, create the
c          first record.
      if (stkrec .eq. 0) then
          ifl(66) = ifl(4) + 1
          stkrec = ifl(66)
          goto 500
      endif
      
c          Read stack ranfil record.
100   call getran (jbr, stkrec)
      if (enttot .gt. 1) then          
          savrec = stkrec
          stkrec = jb(143)
          if (stkrec .ne. 0) then
              enttot = enttot - 1
              goto 100
          else
              enttot = enttot - 1
              jb(143) = ifl(4) + 1
              stkrec = jb(143)
              call putran (jbr, savrec)
          endif
      else
          goto 800
      endif
c          Clear new looping stack data record.
500   ifl(4) = ifl(4) + 1

      jbc(1:64)        = '        '
      jbc(65:128)      = '        '
      jbc(129:192)      = '        '
      jb4(49) = -1
      jb4(50) = -1
      jb4(51) = -1
      jb4(53) = 0
      jb4(54) = 0
      jb4(55) = 0
          
      jb(103) = 0
      jb(104) = 0
      jb(143) = 0.
c          Update looping stack data record fields with passed data.
800   if (tname .ne. '        ') then
          jbc(1:64) = tname
          jb4(53) = tsub
      endif
      if (lname .ne. '        ') then
          jbc(65:128) = lname
          jb4(54) = lsub
      endif
      if (stnum .ne. -1)         jb4(49) = stnum
      if (nxtnum .ne. -1)        jb4(50) = nxtnum
      if (endnum .ne. -1)        jb4(51) = endnum
                                 jb(103) = 0
                                 jb(104) = 0
      call putran (jbr, stkrec)
c
c...Reset ON/ERROR flags
c
      ifl(254) = 0
      ifl(255) = 0
c
c...Clear out the ending DO loop label
c...if this is a Macro call
c...This allows for the "Called" Macro to
c...contain the same label as the DO loop
c...ending label
c...6/25/91  -  Bobby
c
      if (tname .ne. 'LOOPST' .and. lname .eq. ' ') then
          csc155 = ' '
          if (tname .ne. ' ') curmac = tname
      endif

99999 return
      end
