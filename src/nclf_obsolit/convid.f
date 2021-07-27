C*********************************************************************
C*    NAME         :  convid.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       convid.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:43
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine convid
C*      this routine extracts the number of an   
C*      automatically generated type identifier.  for example, if the 
C*      identifier is 'pt123', a 123 would be returned in itv.  it    
C*      the identifier is not of this type, itv gets 0.              
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
      subroutine convid
      include 'com8a.com'
      character*1 atoken(64)
c...allow more than 2 labels
      character*64 f
      integer*2 id
      integer*4 sub
c
      itv=0
      call convlab(token2, f, sub)
      call nmtoid (id,f)
c
      if (ist .eq. id) then
          if (f.ne.' ') then
              itv = sub
          endif
      endif
99999 continue
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine convlab(token, base, sub)
C*      this routine extracts the number of an identifier  
C*      for example, if the identifier is 'p123',
C*      123 would be returned in 'sub', 'p' will be return in 'base'    
C*      if the identifier is not of this  type (such as 'p1ab', 'ab'...)
C*      the base will be empty (base=' ')   
C*    PARAMETERS   
C*       INPUT  : 
C*          token: token to be check
C*       OUTPUT :  
C*          sub: number
C*          base: base name
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine convlab(token, base, sub)
      include 'com8a.com'
      character*64 token, base, numstr
      integer*2 nc
      integer*4 i,j,k,sub
c
      sub=0
      base = ' '
      j = 0
      k = 0
      do 100 i=64,1,-1
          if (token(i:i).eq.' ') then
              if (j.eq.0) goto 100
              goto 99999
          endif
          if (token(i:i).ge.'0'.and.token(i:i).le.'9') then
              if (j.eq.0) j = i
              k = i
          else
              goto 110
          endif
100       continue
110       if ((j.eq.0).or.(k.eq.1)) then
             goto 99999
          endif
          base(1:k-1) = token(1:k-1)
          numstr(1:j-k+1) = token(k:j)
          nc = j - k + 1
          call chr2int(numstr, nc, sub)
99999 continue
      return
      end
