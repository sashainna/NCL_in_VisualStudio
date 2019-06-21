C*********************************************************************
C*    NAME         :  lrupdt.f
C*       CONTAINS:
C*    COPYRIGHT 1991 (c) Numerical Control Computer Sciences
C*     MODULE NAME AND RELEASE LEVEL 
C*       lrupdt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:15
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE: subroutine lrupdt (tname, lname, stnum, nxtnum,
C*                                     endnum,iefl,iemsg,elabel)
C*
C*    PURPOSE
C*       To modify data on the stack that contains information about 
C*       looping regions for LOOPs and DO LOOPs.                      
C*       Only data that contains non-blank, non-zero values is changed.
C*                                                                    
C*       See LRPUSH.FOR for documentation on the format of the stack.
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
C*          TSUB  = Integer*4
C*                     Label of 'tname'
C*          LNAME = Character*64                                     
C*                    Blank if a MACRO or LOOPST/LOOPND entry.     
C*                    The name of the limit variable if it is a DO
C*                    entry.  This will contain the value 'DOLIMITV'
C*                    if this call is to update the ending statement
C*                    number of all DO LOOPs that are active with the
C*                    ending label name given in TNAME.              
C*          LSUB  = Integer*4
C*                     Label of 'lname'
C*          STNUM = Integer*4                                   
C*                    Number of looping region's beginning statement
C*                    number.                                      
C*          NXTNUM = Integer*4                                    
C*                    Number of first executable statement in looping
C*                    region.                                       
C*          ENDNUM = Integer*4                                     
C*                    Number of looping region's ending statement.
C*          IEFL   = Integer*2
C*                    ON/ERROR flag (STOP, CONTIN, LABEL)
C*          IEMSG  = Integer*2
C*                    ON/ERROR message flag (WARN, NOWARN)
C*          ELABEL = Character*64
C*                    ON/ERROR label.
C*          ESUB  = Integer*4
C*                     Label of 'elabel'
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine lrupdt (tname, tsub,lname, lsub, stnum, nxtnum, endnum,
     1                   iefl, iemsg, elabel, esub)

      include 'com4a.com'

      character*64 tname, lname, elabel
      integer*2 stkrec, iefl, iemsg
      integer*4 stnum, nxtnum, endnum, tsub, lsub, esub
      integer*2 enttot, entrec
      logical change

c **********************************************************************

c          Load total number of entries in looping region stack.
      enttot = ifl(174)

c          Load number of first Ranfil record of looping region stack.
      stkrec = ifl(66)

c          Read a stack Ranfil record.
100   call getran (jbr, stkrec)
      change = .false.
      entrec = enttot
      if (entrec .gt. 1) entrec = 1

c          Scan all entries on this stack record for a match of the TNAME 
c          and LNAME.  If a match is found, update the stack entry.
c          If LNAME is 'DOLIMITV', modify entry if TNAME only matches.  This
c          is for updating a DO LOOP entry when only the variable name (TNAME)
c          is known.
      if ((jbc(1:64) .eq. tname).and.(jb4(53).eq.tsub)) then
          if (((jbc(65:128) .eq. lname).and.(jb4(54).eq.lsub)) .or.
     x            lname .eq. 'DOLIMITV') then

c                      Update looping stack data record fields with passed data.
              if (stnum .ne. -1)  jb4(49) = stnum
              if (nxtnum .ne. -1) jb4(50) = nxtnum

c                      Update the looping region ending statement number field
c                      if the ENDNUM value passed in is non-zero or it is a
c                      DO LOOP ending statement label update made during a *EDT
c                      type operation. (LNAME will be set to 'DOLIMITV')
c
c                      This is done so an active DO LOOP's ending statement 
c                      number can be properly updated when LOADPP is restoring
c                      the part program file.  During that operation, only the
c                      ending statements label name is known.  To ensure that
c                      only the active DO LOOP entries in the stack that are 
c                      associated with that ending statement label are updated,
c                      the ENDNUM value is set to a -1 when LOADPP updates the
c                      stack entry for the DO LOOP that has the ending 
c                      statement label in question.  That way, when LRUPDT is
c                      called with the statement label, only those DO LOOP 
c                      entries that have been processed by LOADPP and not had
c                      their ending statement value set, will be updated.
                  if ((lname .ne. 'DOLIMITV' .and. 
     x                 endnum .ne. -1) .or. 
     x                (lname .eq. 'DOLIMITV' .and. 
     x                 jb4(51) .eq. -1))   
     x                 jb4(51) = endnum
                  if (iefl .ne. -1) then
                      jb(103) = iefl
                      jb(104) = iemsg
                      jbc(129:192) = elabel
                      jb4(55) = esub
                  endif
                  change = .true.
              endif
          endif

      if (change) call putran (jbr, stkrec)

c          Load the pointer to a record containing more stack data.
      stkrec = jb(143)

c          Decrement the number of total entities by the 8 that were
c          checked in the last record.  Either read and scan another record
c          if there is one or exit the routine if all entries have been
c          checked.
      enttot = enttot - 1
      if (enttot .gt. 0) goto 100

      return
      end
