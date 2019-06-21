C*********************************************************************
C*    NAME         :  lrdata.f
C*       CONTAINS:
C*    COPYRIGHT 1991 (c) Numerical Control Computer Sciences
C*     MODULE NAME AND RELEASE LEVEL 
C*       lrdata.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:15
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE: subroutine lrdata (tname, tsub, lname, lsub, stnum, nxtnum,
C*                                     endnum, iefl, iemsg, elabel, esub, kinc)
C*
C*    PURPOSE
C*       To return the contents of the entry from the stack of data 
C*       kept about looping regions.                               
C*                                                                    
C*       See LRPUSH.FOR for documentation on the format of the stack.
C*
C*    PARAMETERS   
C*       INPUT  : 
C*          kinc  = Number of call/loop off of stack to return data from.
C*                  A value of 1 returns the active call/loop.
C*
C*       OUTPUT :  
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
C*                    entry.  This will contain the value 'DOLIMITV'
C*                    if this call is to update the ending statement
C*                    number of all DO LOOPs that are active with the
C*                    ending label name given in TNAME.              
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
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine lrdata (tname, tsub,lname, lsub,stnum, nxtnum, endnum,
     1                   iefl, iemsg, elabel, esub, kinc)

      include 'com4a.com'

      character*64 tname, lname,csc155, elabel
      integer*4 cinx155
      equivalence (sc155,csc155)
      equivalence (inx155,cinx155)
      integer*2 entry, stkrec, iefl, iemsg, kinc
      integer*4 stnum, nxtnum, endnum
      integer*4 tsub, lsub, esub
c
      integer*2 tb(144)
      integer*4 tb4(72)
      real*8 tbr(36)
      character*288 tbc
c
      equivalence (tb,tbr,tb4,tbc)

c **********************************************************************

      stkrec = ifl(66)
      entry  = ifl(174)

c          If no looping region record exists, report internal error.
      if (stkrec .eq. 0) then
          call error (406)
          goto 99999
      endif      
c          Read stack ranfil record.
100   call getran (tbr, stkrec)
c              This record contains the entry.  Load the entry into the
c              calling parameter fields and return.
      if (entry .gt. kinc) then
          entry = entry - 1
          stkrec = tb(143)

c              If no extension record number exist, report internal error.
          goto 100

c              This record contains the entry.  Load the entry into the
c              calling parameter fields and return.
      else
          if (entry .gt. 0) then
              tname  = tbc(1:64)
              lname  = tbc(65:128)
              elabel = tbc(129:192)
              tsub = tb4(53)
              lsub = tb4(54)
              esub = tb4(55)
              stnum  = tb4(49)
              nxtnum = tb4(50)
              endnum = tb4(51)
              iefl   = tb(103)
              iemsg  = tb(104)
c
c...If this is a DO loop then
c...restore the ending label
c...6/25/91  -  Bobby
c
              if (tname .ne. 'LOOPST' .and. lname .ne. ' ' .and.
     1                kinc .eq. 1) then
                  csc155 = tname
                  cinx155 = tsub
              endif
          else
              tname = '        '
              lname = '        '
              stnum = -1
              nxtnum = -1
              endnum = -1
              iefl   = 0
              iemsg  = 0
              elabel = '        '
              lsub = 0
              tsub = 0
              esub = 0
          endif
      endif

99999 return
      end
