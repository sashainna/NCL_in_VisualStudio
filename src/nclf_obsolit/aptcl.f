C*********************************************************************
C*    NAME         :  aptcl.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       aptcl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:37
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine aptcl
C*       purpose of program: to create an apt source and clfile name
C*       by appending the part program file name prefix with appropriate
C*       suffix.
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
C
      subroutine aptcl(clname)

      include 'com4a.com'


      logical brafnd
      integer*2 perpos,i
      integer*4 strlen1
      character*(MAX_PATH) clname

c                     Correct the clfile name using the
c                     prefix of part program file name.
      if (clfnam .eq. 'no_name.cl{'.and. defnam .ne. '.pp') then
          brafnd=.false.
          do 270 i=1,strlen1(defnam),1
              if (defnam(i:i).eq.'[') brafnd=.true.
              if (defnam(i:i).eq.']') brafnd=.false.
              if ((defnam(i:i).eq.'.' .or. defnam(i:i).eq.' ')
     x             .and. .not.brafnd) then
                   perpos=i
                   goto 280
              endif
270       continue
280       continue
c                     Correct the apt source file name using the
c                     prefix of part program file name.
          if (ifl(88) .eq. 1.and. asfnam .eq. 'no_name.as{') then
		     asfnam = defnam
             asfnam(perpos:)='.as'
          endif
          clname  = 'no_name.cl'
          clfnam=defnam
          clfnam(perpos:)='.cl'
      else
          clname = clfnam
      endif

      return
      end
