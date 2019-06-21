C*********************************************************************
C*    NAME         :  vocsch.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       vocsch.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:54
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine vocsch
c*       check contents in variable 'token' for a 
c*       match with vocabulary words                                    
C*    PARAMETERS   
C*       INPUT  : 
c*          token - contains the character string to be matched for  
c*                  against the vocabulary tables.                   
C*       OUTPUT :  
c*          ityp - set to a value of 1 if found otherwise set to 2. 
c*           ist - set to the vocabulary word numeric value if a     
c*                  match was found.                                 
c*                                                                   
c*   see the parsit subroutine for more details.                     
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine vocsch

      include 'com8a.com'
      include 'vaxvoc.com'
      integer*2 cur,inc
      integer*4 nc, strlen1
      character*24 token6
      equivalence (token2,token6)

c          set ityp & ist to unknown identifier.
      ityp=2
      ist=1
c
c...if the token length > 6, return as unknown identifier.
c
      nc = strlen1 (token2)
      if (nc.gt.24) return
c
c...Search vocabulary common
c
      cur=512
      inc=256
      do 100,i=1,12
          if (token6.lt.vvcnam(cur)) then
              cur=cur-inc
          else if (token6.gt.vvcnam(cur)) then
              cur=cur+inc
          else if (token6.eq.vvcnam(cur)) then
              ityp=1
              ist=vvcnum(cur)
              go to 99999
          endif
          inc=inc/2
100   continue
99999 continue

      return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  asvoc (inum,cvoc)
c
c   FUNCTION:  Finds and routines the vocabulary word whose number
c              matches 'inum'.
c
c   INPUT:  inum    I*2  D1  -  Vocabulary word number to search for.
c
c   OUTPUT: cvoc    C*6  D1  -  Vocabulary word.
c
c***********************************************************************
c
		subroutine asvoc (inum,iwrd,cvoc)
c
      include 'com8a.com'
      include 'vaxvoc.com'
c
      integer*2 inum
c
		integer*2 iwrd
c
      character*(24) cvoc
c
      integer*2 isw
c
c...added by Yurong 7/18/97
		integer*2 nmaj
		integer*2 ival(15)
		character*6 cwrd(15)
c
		data nmaj /15/
		data ival /16,1,17,3,5,15,7,2,6,10,128,130,129,818,841/
c
		data cwrd /'BREAK','END','GOHOME','OPSTOP',
     -           'RAPID','RESET','RETRCT','STOP',
     -           'SWITCH','UNLOAD','PEN','PENDWN',
     -           'PENUP','SEQUNC','UNITS'/
c
c...Standalone major word
c...added by Yurong
c
		if (iwrd .eq. 1 .and. inum .lt. 1000) then
			do 199 i=1,nmaj,1
				if (inum .eq. ival(i)) then
					cvoc = cwrd(i)
					go to 8000
				endif
  199     continue
		endif		
c
c...Search for vocabulary word
c
      isw    = 0
      do 100 i=1,1024,1
          if (inum .eq. vvcnum(i)) then
              cvoc(1:24)   = vvcnam(i)(1:24)
              if (cvoc(3:6) .ne. '   ') go to 8000
              isw    = vvcnum(i)
          endif
  100 continue
c
c...Can't find vocabulary word
c
      if (isw .eq. 0) then
          write (cvoc,10) inum
   10     format ('*',i4.4,'*')
      endif
c
c...End of routine
c
 8000 return
      end
