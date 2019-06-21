C*********************************************************************
C*    NAME         :  vocab2.f
C*       CONTAINS:
C*    COPYRIGHT 1992 (c) NCCS.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       vocab2.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:53
C********************************************************************/
c.....
c.....                Paul. 08/11/92
c.....
c..... This file includes code for SYN command (subroutine SYNCOM)
c..... and for reading NCLVOC.SYN file during NCL initialization
c..... (subroutine SYNINI). The SYN command has the following syntaxis
c.....
c.....  SYN/NEW_NAME,OLD_NAME(OLD_NUMBER)[,...,...]
c.....
c..... The NCLVOC.SYN file has the following record strucure:
c.....  
c..... NEW_NAME OLD_NAME(OLD_NUMBER)
c.....
c..... The "OLD" parameter can be entered as already known VOCABULRY
c..... WORD or as any positive integer. The "NEW" parameter is the
c..... WORD you want to use instead of OLD WORD or WORD you want to
c..... add to the vocabulary. If the OLD parameter is entered as 
c..... integer, this record is just added to the voacbulary. If the
c..... OLD parameter is entered as OLD WORD, NCL searches in the
c..... current vocabulry for it and, if this word exists, substitutes
c..... it with the NEW WORD.
c.....
c..... The "oldnam" & "oldnum" arrays keep the OLD_WORD. IF this
c..... parameter is given as number, "oldnum" is empty ('      ').
c..... The "newnam" array keeps the NEW_WORD.
c.....
c..... THIS IS THE FIRST PART OF THE FILE. "SYN" COMMAND.
c.....

      subroutine syncom

      include 'com8a.com'
      include 'vaxvoc.com'

      character*24 newnam(50), oldnam(50), tmpnam(1024), swpnamn
      character*24 swpnamo
      integer*4   oldnum(50), count
      integer*2   tmpnum(1024), swpnumo
 
      if (nextyp.ne.5) go to 22
        ifl(44) = 9
c..... get the new name
        count = 0
        call parsit
16      count = count + 1
10      if (ityp .ne. 2 .or. ist .ne. 1) then
c
c...  When *set/lang cadra is enabled we can use SYN to redefine vocab words.
c
          if (ifl(374) .ne. 0) then
            do 17 i=1,1024
              if(vvcnam(i) .eq. token2(1:24)) goto 18
   17       continue 
          endif
           if(ityp .ne. 2) call error(87)
           if(ityp .eq. 2 .and. ist .ne. 1) call error(8)
           goto 99999
        endif
   18   if (ivxsub .ne. 0) then
           ist = 2
           call error(86)
           goto 99999
        endif
        newnam(count) = token2(1:24)
c..... get the old name
   19   call parsit
        if(ityp .ne. 1 .and. ityp .ne. 3 ) then
           call error(35)
           goto 99999
        endif
c....  got the old name 
        if (ityp .eq. 1) then
          oldnum(count) = ist
          oldnam(count) = token2(1:24)
c..... got the number of the old name
        else
           if(ist .eq. 3) then
             call error(217)
             goto 99999
           endif
           oldnum(count) = itv
           oldnam(count) = '      '
      endif
        call parsit
c.....    
c..... If it is not the last pair of names, go get the next one
c.....
        if (ifl(374) .ne. 0 .and.vvcnam(i) .eq. newnam(count)) then
          vvcnum(i) = oldnum(count)
          if(nextyp .ne. 11) goto 10
          goto 99999
        endif
        if(nextyp .ne. 11) goto 16
       
c.....
c..... Sort newnam(50) & oldnam(5) & oldnum(50)
c.....
3333  isw = 0
      do 3334 k=1,count-1,1
         if (newnam(k) .gt. newnam(k+1)) then
            swpnamn = newnam(k)
            swpnumo = oldnum(k)
            swpnamo = oldnam(k)
            newnam(k) = newnam(k+1)
            oldnum(k) = oldnum(k+1)
            oldnam(k) = oldnam(k+1)
            newnam(k+1) = swpnamn
            oldnum(k+1) = swpnumo
            oldnam(k+1) = swpnamo
            isw = 1
         endif
3334  continue
      if (isw .eq. 1) go to 3333
c....
c....Merge Old vocabulary with new synonims
c....
c.... n - pointer to the "tmpnam" & "tmpnum"
c.... i - pointer to the "vvcnam" & "vvcnum"
c.... j - pointer to the "newnam" & "oldnam" & "oldnum"
c....
         n = 1
         i = 1
         j = 1
31       continue
         if(i .gt. 1024) goto 15
         if(j .gt. count) goto 26
         if(newnam(j) .le. vvcnam(i)) goto 15
26       tmpnam(n) = vvcnam(i)
         tmpnum(n) = vvcnum(i)
         i = i + 1
         n = n + 1
         if(n .gt. 1024) goto 41
         goto 31
15       tmpnam(n) = newnam(j)
         tmpnum(n) = oldnum(j)
         j = j + 1
         n = n + 1
         if(n .gt. 1024) goto 41
         goto 31
           
41       if(j .le. count) then
           call error(422)
           goto 99999
         else if(i .le. 1024) then
              if (vvcnam(i)(1:6) .ne. 'ZZZZZZ') then
                  call error(422)
                  goto 99999
              endif
         endif
c.....     
c..... If everything is o'k - copy tmpnam & tmpnum into vvcnam & vvcnum
c.....
cc         do 51 k=1,1024,1
         if (n .gt. 1024) n = 1024
         do 51 k=1,n,1
            vvcnam(k) = tmpnam(k)
            vvcnum(k) = tmpnum(k)
51       continue
         goto 99999

22       isvinx=inx
         call error(22)

99999    return
         end
c.....
c..... END OF THE CODE FOR THE "SYN" COMMAND .
c.....
c
c.....
c..... START OF THE CODE FOR THE NCLVOC.SYN FILE.
c.....
      subroutine synini

      include 'com8a.com'
      include 'vaxvoc.com'

      character*(MAX_PATH) vfile
      character*24 newnam(1024), oldnam(1024), swpnamn
      character*24 swpnamo, tmpnam(1024)
      integer*2   tmpnum(1024), swpnumo
      integer*2   oldnum(1024), count
      integer*4   kerr
      character*100 msgline
      character*24 tmpnam1, tmpnam2, tmpstr
      integer*2   tmpnum1, isw
      integer*2   fld1st, fld1ln, fld2st, fld2ln
c.....
c..... Open "nclvoc.syn" file if it is present
c.....
      call flname(11, 'nclvoc{', vfile)
      call flopen(voclun, vfile, 'OLD', 'SEQUENTIAL', 'FORMATTED', 80,
     x'NULL', ioerr)
      if (ioerr .ne. 0) goto 99999
c.....
c.....If this file is open - read it.
c.....
        count = 0
        do 13 j=1,1024
          tmpnum1  = 0
          tmpnam1 = ' '
          tmpnam2 = ' '
          msgline = ' '
          read (voclun,1213,end=3333) msgline
1213      format (a100)
c....
c.... Delete any control characters
c....
          do 16 i=1,100
             if(msgline(i:i) .lt. char(32) ) msgline(i:i) = char(32)
16        continue
c....
c....Convert the input line to the fields.
c....
          fld1st = nindex(msgline,' ')
          if (fld1st .eq. 0) then
              if (ifl(35).eq.0.or.(ifl(35).eq.2.and.ifl(350).ne.0)) then
                 write(msgline,1001) j
                 call puterm(msgline)
c                 call nwait(4)
                 msgline = ' '
                 call puterm(msgline)
              endif
              goto 13
          endif
          fld1ln = index(msgline(fld1st:100),' ')-1
          if (fld1ln .eq. 0) fld1ln = 100-fld1st
             
          tmpnam1 = msgline(fld1st:fld1st+fld1ln)
          call convrt(tmpnam1,tmpstr,24)
          tmpnam1 = tmpstr
 
          fld2st = nindex(msgline(fld1st+fld1ln+1:100),' ')
          if (fld2st .eq. 0) then
              if (ifl(35).eq.0.or.(ifl(35).eq.2.and.ifl(350).ne.0)) then
                 write(msgline,1002) j
                 call puterm(msgline)
c                 call nwait(4)
                 msgline = ' '
                 call puterm(msgline)
              endif
              goto 13
          endif
          fld2ln = index(msgline(fld1st+fld1ln+fld2st:100),' ')-1
          if (fld2ln .eq. 0) fld2ln = 100-fld2st
          tmpnam2 =
     #      msgline(fld2st+fld1st+fld1ln:fld2st+fld1st+fld1ln+fld2ln)
c....
c....Convert the field "Number" to the number
c....
          call ctoi(tmpnam2,tmpnum1,kerr)
c....
c....Converted o'k - meens that tmpnam2 is number
c....
          if (kerr .eq. 0) then
            if(tmpnum1 .le. 0) then
               if (ifl(35).eq.0.or.(ifl(35).eq.2.and.ifl(350).ne.0))then
                  write(msgline,1002) j
                  call puterm(msgline)
c                  call nwait(4)
                  msgline = ' '
                  call puterm(msgline)
               endif
               goto 13
            endif
            tmpnam2 = '      ' 
c.....       
c.....Converted with  an error - tmpnam2 is name
c.....
          else
          call convrt(tmpnam2,tmpstr,24)
          tmpnam2 = tmpstr
            do 2223 i=1,1024
              if(vvcnam(i) .eq. tmpnam2) then
                  tmpnum1 = vvcnum(i)
                  goto 14
              endif
2223        continue
            if (ifl(35).eq.0.or.(ifl(35).eq.2.and.ifl(350).ne.0)) then
               write(msgline,1001) j
               call puterm(msgline)
c               call nwait(4)
               msgline = ' '
               call puterm(msgline)
            endif
            goto 13
          endif
14        count = count + 1
          newnam(count) = tmpnam1
          oldnam(count) = tmpnam2
          oldnum(count) = tmpnum1
13    continue
c....
c....After we read all changes from NCLVOC.SYN file to the 
c....temp. arrays we have to sort it in alphabetical order
c....and to merge it with current vocabulary array.
c....
3333  continue
      close (unit=voclun)
      isw = 0
      do 3334 k=1,count-1,1
         if (newnam(k) .gt. newnam(k+1)) then
            swpnamn = newnam(k)
            swpnumo = oldnum(k)
            swpnamo = oldnam(k)
            newnam(k) = newnam(k+1)
            oldnum(k) = oldnum(k+1)
            oldnam(k) = oldnam(k+1)
            newnam(k+1) = swpnamn
            oldnum(k+1) = swpnumo
            oldnam(k+1) = swpnamo
            isw = 1
         endif
3334  continue
      if (isw .eq. 1) go to 3333
c....
c....Merge Old vocabulary with new synonims
c....
c.... n - pointer for "tmpnam" & "tmpnum"
c.... i - pointer for "vvcnam" & "vvcnum"
c.... j - pointer for "newnam" & "oldnam" & "oldnum"
c....
         n = 1
         i = 1
         j = 1
31       continue
c........if we find the name from the vvcnam in the newnam,
c........just go to the next name in the vvcnam.
         do 11 k=1,count,1
            if(vvcnam(i) .eq. newnam(k)) then
               i=i+1
               goto 31
            endif
11       continue
c........If we are somewere in the middle of the vvcnam &
c........newnam - go ahead. If one of arrays is finished,
c........just go get the next record from other one.
         if(i .gt. 1024) goto 15
         if(j .gt. count) goto 26
         if(newnam(j) .le. vvcnam(i)) goto 15
26       tmpnam(n) = vvcnam(i)
         tmpnum(n) = vvcnum(i)
         i = i + 1
         n = n + 1
         if(n .gt. 1024) goto 41
         goto 31
15       tmpnam(n) = newnam(j)
         tmpnum(n) = oldnum(j)
         j = j + 1
         n = n + 1
         if(n .gt. 1024) goto 41
         goto 31
           
41       if(j .le. count) then
           call error(422)
           goto 99999
         else if(i .le. 1024) then
              if (vvcnam(i)(1:6) .ne. 'ZZZZZZ') then
                  call error(422)
                  goto 99999
              endif
         endif
c.....     
c..... If everything is o'k - copy tmpnam & tmpnum into vvcnam & vvcnum
c.....
         do 51 k=1,1024,1
            vvcnam(k) = tmpnam(k)
            vvcnum(k) = tmpnum(k)
51       continue
99999 return
ccc
1001  format('NCLVOC.SYN: Vocabulary word syntax error. Line ',i4,
     #       ' is ignored.')
1002  format('NCLVOC.SYN: Vocabulary value syntax error. Line ',i4,
     #       ' is ignored.')
ccc
      end
c.....
c..... END OF THE CODE FOR NCLVOC.SYN FILE   !!!!!!!!!!!
c.....
