C*********************************************************************
C*    NAME         :  utilty.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       utilty.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:52
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine utilty (stat)
c*       this routine parses all control ()      
c*       commands and causes the desired action to take place.         
c*                                                                   
c*       valid control commands are:                                   
c*         1.  data base show                                        
c*        1a.  unibase show
c*         2.  show                                                  
c*         3.  reset                                                 
c*         4.  set                                                   
c*         5.  run                                                   
c*         6.  insert                                                
c*         7.  backup                                                
c*         8.  pause                                                 
c*         9.  stop                                                  
c*        10.  start                                                 
c*        11.  quit                                                  
c*        12.  help                                                  
c*        13.  return
c*                                                                   
c*   last revision: to add a set/reset for apt comments              
c*                                                                   
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
       subroutine utilty (stat)

      include 'com8a.com'

       integer*2 stat, isv131, ixsv, ifl383
       character*6 svtkn,tkn
       character*2 tkn2,stkn2
       equivalence (tkn2,tkn,token2)
       equivalence (stkn2,svtkn)
       logical set

c     initialize skip command flag. kathy
      lskip = .false.
      ifl383 = ifl(383)
      ifl(383) = 0

c          clear lines 2 and 3 if graphic option put messages on them
      if (ifl(172).eq.1) then
          call putmsg (' ',1,2,0)
          call putmsg (' ',1,3,0)
          ifl(172)=0
      endif
c
c...'**' Commands are processed as follows:
c...
c...   Defining Macro:  Put in source file and not executed.
c...   Executing Macro: Put in source file and executed.
c...   Normal Mode:     Put in source file and executed.
c...
c... Bobby  -  9/19/91
c
       if (ityp .eq. 5 .and. ist .eq. 10) then
           if (fromt) then
               ncsfl(5) = 0
               call putw2 (nline)
               ixsv = inx
               call putsrc
               inx = ixsv
               ifl(44) = 5
               err = .false.
           endif
           if (ifl(152) .eq. 1 .or. ifl(38) .eq. 1 .or.
     1          ifl(152) .eq. 3 .or. ifl(45) .eq. 1) go to 99999
       endif
c
c...Parse next token
c.....Allow utility commands even with REMARK/ON in effect - ASF 1/13/14
c
20     stat=0
       call parser

       IF (TKN .EQ. 'RETURN') THEN
           stat = 999
           goto 99999
       endif
c                                                ******** unibase show ***
       if (tkn.ne.'UBS'.and.tkn.ne.'UBSHOW'.and.tkn2.ne.'UB') go to 10
       go to 15
c                                                ******** data base show
   10  IF (TKN.NE.'DBS'.AND.TKN.NE.'DBSHOW'.AND.TKN2.NE.'DB') GO TO 50
c
c      Added for *command in macro and loop. kathy
c
   15  if (.not.fromt.and.(ifl(152).eq.1.or.ifl(38).eq.1
     x	   .or.ifl(152).eq.3.or. ifl(45).eq.1))goto 99999

       ifl(1)=4
       call dbcall(4)
       go to 99999
50     svtkn=tkn
       isvl=length
c                  ifl(44)=9 sets the parser to ignore commas
60     ifl(44)=9
       ldtflg = .true.
       ldtext = .true.
       call parsit
       ldtflg = .false.
       ldtext = .false.
c                                                ********* show
       IF (SVTKN .EQ. 'SHOW' .OR.
     1     STKN2 .EQ. 'SH' .OR.
     2     SVTKN .EQ. 'S') THEN
c
c      added for *command in macro and loop. kathy
c
           if (.not.fromt.and.(ifl(152).eq.1
     x         .or.ifl(38).eq.1.or.ifl(152).eq.3.or. 
     x          ifl(45).eq.1))goto 99999

           isv131 = ifl(131)
           ifl(131) = 0
c
c...Added for NCL-VT by Paul
c...12/04/91
c
           call ersw3(15,1)

           if (ityp.eq.2 .and. ist.eq.DATAST) then
             call shdata
           else
             call ushow(stat)
           endif
           ifl(131) = isv131
           if (stat.eq.1) go to 20
c
c...added for wild card
c
           if (wildnum.gt.0) go to 60
c
c...added for COLF, LAYF
c
           if (geomnum.gt.0) go to 60
c                                                ********* set-reset
       ELSE IF ((SVTKN.EQ.'SET'.AND.ISVL.EQ.3.).OR.
     1          (STKN2.EQ.'SE'.AND.ISVL.EQ.2).OR.
     2          (SVTKN.EQ.'RESET'.AND.ISVL.EQ.5).OR.
     3          (STKN2.EQ.'RE'.AND.ISVL.EQ.2)) THEN
           IF (SVTKN .EQ. 'SET') THEN
               set = .true.
           else
               set = .false.
           endif
c
c      added for *command in macro and loop. kathy
c
           if (.not.fromt.and.(ifl(152).eq.1.or.ifl(38).eq.1
     x	       .or.ifl(152).eq.3.or. ifl(45).eq.1))goto 99999

           call uset (set)
c                                               *********** rest
       else
           call urest(svtkn,isvl,stat)
           if (stat.eq.1) go to 20
       endif
99999  ifl(383) = ifl383
       return
       end
