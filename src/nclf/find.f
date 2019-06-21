C*********************************************************************
C*    NAME         :  find.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       find.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:03
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : integer*4 function find (patrnc, mode,
C*                                              drectn, start) 
C*     to locate a character string in the partpgm.ncl file          
C*                                                                   
C*     where:                                                        
C*        patrnc = the character string to be located                
C*        mode = specifies if the patern is a token or a buried      
C*               character string.                                   
C*                 0 = patern may be surrounded by any other         
C*                     characters                                    
C*                 non 0 = patern may not be surrounded by any       
C*                         alphanumeric characters (token)           
C*        drectn = direction of search                               
C*                 +1 = read partpgm records in ascending order      
C*                 -1 = read partpgm records in descending order     
C*        start = partpgm record number to start searching from      
C*                                                                   
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
      integer*4 function find (patrnc, mode, drectn, start)   

      include 'com4a.com'

      character*1 tchar
      character*(MAX_LEN) cmsg
      character*(MAX_LEN) patrnc, savpat
      integer*2 mode, len, dlen, patpos, lpos, drectn
      integer*4 sav353
      integer*4 start, iline, ncc
      logical wraped

      save savpat
      data savpat/' '/

      wraped = .false.

      if (patrnc .eq. ' ') patrnc = savpat
      len = dlen (patrnc, 65)
      if (len .eq. 0) goto 99999
      iline = start + drectn
      sav353 = ifl4(2)

500   if (iline .eq. start + drectn .and. wraped) goto 2000
      call getsrc (cmsg, ncc, iline-1,0)
      if (srceof) goto 900
      call convrt(cmsg,cmsg,ncc)
800   patpos = index (cmsg, patrnc(1:len))
      if (patpos .gt. 0) then
          if (mode .eq. 0) then

c                  simple match
              find = iline
              goto 99998
          else

c                  token match
              lpos = patpos - 1
              if (lpos .ne. 0) then
                  tchar = cmsg (lpos:lpos)
                  if (tchar .ge. 'A' .and. tchar .le. 'Z' .or.
     x                tchar .ge. '0' .and. tchar .le. '9') then
                      cmsg = cmsg(patpos+1:)
                      goto 800
                  endif
              endif
              tchar = cmsg (patpos + len:patpos + len)
              if (tchar .ge. 'A' .and. tchar .le. 'Z' .or.
     x            tchar .ge. '0' .and. tchar .le. '9') then
                  cmsg = cmsg(patpos+1:)
                  goto 800
              endif
              find = iline
              goto 99999
          endif
      endif
      iline = iline + drectn
      if (iline.gt.0) goto 500

c          end of file found without a match
900   if (wraped) goto 2000

c         this should happen only when the partpgm is empty
      wraped = .true.
      if (drectn .eq. 1) then
          iline = 1
      else
          iline = ifl4(1)
      endif
      goto 500

2000  if (len .gt. 20) len = 20
      write (errcom, 1010) ' pattern: "', patrnc(1:len), '"'
1010  format (a, a, a)
      ist = 99
      if (sav353 .eq. find) then
          call error (-294)
      else
          call error (-295)
      endif
      ifl(141) = dlen (cin(1:nccin), nccin)
      err = .true.
      find = 0

99998 savpat = patrnc
99999 return
      end
