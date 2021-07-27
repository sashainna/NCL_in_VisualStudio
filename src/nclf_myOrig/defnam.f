C*********************************************************************
C*    NAME         :  defnam.f
C*       CONTAINS:  defnam  
C*    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*        defnam.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:09:55
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : modnam 
C*      Parse DEFNAM command & and call processing C routine.
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
      subroutine modnam 
c
      include 'com.com'
c
      integer*2 isub,j,idumm,igtyp,isv,ie
c
      character*64 cmname
      character*64 gname
      character*255 txt
c
      equivalence (gname,cmname)
c
      if (NEXTYP .ne. 5) then
         ISVINX = INX
         call error (22)
         go to 8000
      end if
      ifl(44) = 9
c
c...Get geometry specificator 
c...and string to set default name
c
  100 call parsit
      irn     = 61
      if (NXTEOS) go to 9001

      if (ityp .eq. 1) then
          call vctoid (ist,igtyp,idumm)
          irn     = 1
          if (igtyp .eq. 0 .or. ist .eq. 402) go to 9001
          ldtext = .true.
          call parsit
          ldtext = .false.
          if (lstrng) then
            j = 0
            call gttext(txt,j)
            cmname = txt(1:j)
          else
            if (ityp .gt. 2) go to 9087
            cmname = token2
          endif
c
c...suppress error 9 output if EOS and name is too long
c
          ist    = 0
          isub   = 0
          if (NXTEOS) go to 500
          isv    = INX
          call parsit
          if (ityp .eq. 1) then
              call vctoid (ist,j,idumm)
              if (j .ne. 0) then
                 INX = isv
                 NXTEOS = .false.
              else
                 if (ist .ne. 810) go to 9455
                 isub   = 1
              end if
          else
              irn = 61
              go to 9001
          end if
      else
          go to 9001
      end if
c
c...Update name modals data
c
  500 if (isub .eq. 0) then
          if (index(gname,' ') .gt. 21) go to 9456  
      end if
      call modtoc (isub,igtyp,gname,ie) 
      if (ie .eq. 1) then
          err = .true.
          go to 8000
      endif
      if (NXTEOS) go to 8000
      go to 100
c
c...Errors      
c
 9001 call error (irn)
      go to 8000
c
 9087 call error (87)
      go to 8000
c
 9455 call error (455)
      go to 8000
c
 9456 call error (456)
      go to 8000
c
 8000 return
      end
