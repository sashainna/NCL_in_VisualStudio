C*********************************************************************
C*    NAME         :  expnam.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       expnam.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:02
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine expnam (a1,l,a2)
C*       receive 8 byte name in argument a1 in ranfile format (6 byte
C*       name & 2 byte integer subscript). convert it to all
C*       characters & return in argument a2 with length in argument l
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
      subroutine expnam (a1,l,a2)
      include 'com8a.com'

      real*8 a1
      character*1 a2(13)
      integer*2 l

      real*8 sa1
      logical la1
      integer*2 ia1(4)
      character*1 ca1(8)
      equivalence(la1,sa1),(la1,ia1),(la1,ca1)
      character*5 ctemp
      character*1 ctemp1(5)
      equivalence (ctemp,ctemp1)

      sa1=a1
      do 50 i=1,6
50      a2(i)=' '

      do 100 l=1,6
        if (ca1(l).eq.' ') goto 110
        a2(l)=ca1(l)
100   continue

110   if (ia1(4).eq.0) then
        l=l-1
      else
        a2(l)='('
        l=l+1
        write(ctemp,1000) ia1(4)
1000    format(i5)
        do 120 i=1,5
          if (ctemp1(i).ne.' ') then
            a2(l)=ctemp1(i)
            l=l+1
          endif
120     continue
        a2(l)=')'
      endif

      if (a2(1).eq.'@') then
        a2(1)='('
        a2(4)=')'
        l=4
      endif

      if (debug) then
        write(cout,9000) a1,ia1(4),(a2(i),i=1,l)
9000    format('expnam ',a6,i6,1x,60a1)
        call putmsg(cout,80,23,0)
      endif

99999 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine expnam4 (ctoken,ksub,cstr,knc)
C*       Formats a valid entity label and subscript for output as a
C*       text string.
C*    PARAMETERS   
C*       INPUT  : 
C*          ctoken   C*n   D1  Entity label.
C*          ksub     I*4   D1  Entity subscript.
C*       OUTPUT :  
C*          cstr     C*n   D1  Formatted output string 'label(sub)'.
C*          knc      I*2   D1  Number of characters in 'cstr'.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine expnam4 (ctoken,ksub,cstr,knc)
c
      include 'com8a.com'
c
      integer*2 knc
      integer*4 ksub
c
      character*(*) ctoken,cstr
c
      integer*4 nc,strlen1
      character*80 ctemp
c
c...Store label
c
      cstr = ctoken
      nc = strlen1(cstr)
c
c...Store subscript
c
      if (ksub .ne. 0) then
          write(ctemp,10) ksub
   10     format('(',i8,')')
          do 100 i=1,10,1
              if (ctemp(i:i) .ne. ' ') then
                  nc = nc + 1
                  cstr(nc:nc) = ctemp(i:i)
              endif
  100     continue
      endif
c
c...End of routine
c
 8000 knc = nc
      return
      end
