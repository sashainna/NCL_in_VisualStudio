
C*    NAME         :  domode.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
c*       domode.f , 25.1
c*    DATE AND TIME OF LAST MODIFICATION
c*       04/29/15 , 15:09:57
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine domode
C*      update the processing mode and program
C*      state on line 5 of the terminal
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
      subroutine domode

      INCLUDE 'com4a.com'

      EQUIVALENCE (ISVMOD,IFL(48))
      CHARACTER*13 MSTATE
      CHARACTER*16 PSTATE
      CHARACTER*64  macro,macname
      character*80 tmpname
      integer*4    strlen1,delta
      equivalence (sc165,macname)
c
c...Added check for NCL-VT mode
c...Paul  -  10/25/91
c
c...And added changes for NCL501+ mode
c...Paul -   02/19/92
c
c...VX
c
      if (ifl(35) .eq. 2 .and. ifl(350) .le. 1 .and. ifl(322) .eq. 0)
     1        then
c
      IF (IFL(37).EQ.1) THEN
          MSTATE=' OVERWRITE    *'        
      ELSE IF (IFL(37).EQ.2) THEN
          MSTATE=' INSERT       *'
      ELSE IF (IFL(37).EQ.3) THEN
          MSTATE=' RUNNING      *'
      ELSE IF (IFL(37).EQ.4) THEN
          MSTATE=' NON-PROCESS  *'
      ELSE IF (IFL(37).EQ.6) THEN
          MSTATE=' RUNNING STEP *'
      ENDIF
      ISVMOD=IFL(37)
 
C          SET PROGRAM STATE
      macro =' '
      IF (IFL(152).EQ.0) THEN
          PSTATE=' SEQUENTIAL '
      ELSE IF (IFL(152).EQ.1) THEN
          PSTATE=' MACRO DEF:'         
          MACRO = macname
      ELSE IF (IFL(152).EQ.2) THEN
          PSTATE=' MACRO CALL '
          MACRO = macname
      ELSE IF (IFL(152).EQ.3) THEN
          PSTATE=' LOOP DEFINITION '
      ELSE IF (IFL(152).EQ.4) THEN
          PSTATE=' LOOP EXECUTION '
      ENDIF
      IFL(153)=IFL(152)
      
      do 5 i=1,80
5     tmpname(i:i) = ' '

      delta = (29 - strlen1(ppfnam)) / 2

      if (delta .gt. 0) then
         do 10 i=1,delta
10       tmpname(I:I) = ' '
         do 20 i=delta,delta+strlen1(ppfnam)
20       tmpname(I:I) = ppfnam(I-delta+1:I-delta+1)
      else
         tmpname = ppfnam
      endif
c 
c....Added for NCL501+ mode
c....02/19/92 Paul 
c
      if(ifl(350) .eq. 0 ) then
          if (strlen1(macro) .eq. 0) 
     x      WRITE (COUT,1111) MSTATE,PSTATE,TMPNAME
          if (strlen1(macro) .ne. 0) 
     x      WRITE (COUT,1112) MSTATE,PSTATE,macro,TMPNAME
      endif
      if(ifl(350) .eq. 1 ) then
          if (strlen1(macro) .eq. 0)
     x      WRITE (COUT,1113) MSTATE,PSTATE,TMPNAME
          if (strlen1(macro) .ne. 0) 
     x      WRITE (COUT,1114) MSTATE,PSTATE,macro,TMPNAME
      endif

1111  FORMAT ('**',A15,A20,'* NCL-VT * ',A29,' **')
1112  FORMAT ('**',A15,A12,' ',A6,' * NCL-VT * ',A29,' **')
1113  FORMAT ('**',A15,A20,'* NCL501+* ',A29,' **')
1114  FORMAT ('**',A15,A12,' ',A6,' * NCL501+* ',A29,' **')

      CALL PUTMSG(COUT,80,5,0)

      goto 99999
      endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF (IFL(35).EQ.1) GO TO 99999

      IF (IFL(37).EQ.1) THEN
          MSTATE='CONSOLE INPUT'
      ELSE IF (IFL(37).EQ.2) THEN
          MSTATE='INSERT '
      ELSE IF (IFL(37).EQ.3) THEN
          MSTATE='RUNNING '
      ELSE IF (IFL(37).EQ.4) THEN
          MSTATE='NON-PROCESS '
      ELSE IF (IFL(37).EQ.6) THEN
          MSTATE='RUNNING STEP '
      ENDIF
      ISVMOD=IFL(37)

C          SET PROGRAM STATE
      IF (IFL(152).EQ.0) THEN
          PSTATE='SEQUENTIAL '
      ELSE IF (IFL(152).EQ.1) THEN
          PSTATE='MACRO DEFINITION'
      ELSE IF (IFL(152).EQ.2) THEN
          PSTATE='MACRO CALL '
      ELSE IF (IFL(152).EQ.3) THEN
          PSTATE='LOOP DEFINITION '
      ELSE IF (IFL(152).EQ.4) THEN
          PSTATE='LOOP EXECUTION '
      ENDIF
      IFL(153)=IFL(152)
      WRITE (COUT,1010) MSTATE,PSTATE
1010  FORMAT ('***** PROCESSING MODE: ',A13,'  **',
     X              '  PROGRAM STATE: ',A16,' *****')
    
c
c...VX
c
      IF (IFL(268).EQ.1 .and. ifl(322) .eq. 0) CALL PUTMSG(COUT,80,5,0)

99999 return
      end
c
c***********************************************************************
c
c   E-FUNCTION:  domode_val()
c
c   FUNCTION:  This routine returns the do mode IFL(37) value
c
c   INPUT:  none
c
c   OUTPUT: do mode  domode ()
c
c***********************************************************************
c
      integer*4 function  insmode_val()
c
      INCLUDE 'com4a.com'
c
      insmode_val = IFL(25)
      return
      end
