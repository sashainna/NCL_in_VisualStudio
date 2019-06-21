C*********************************************************************
C*    NAME         :  fmtsrc.f
C*       CONTAINS:
C*           fmtsrc  fmtold  fmtnew
C*    COPYRIGHT 2009 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       fmtsrc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:05
C********************************************************************/
C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmtsrc
c*       Format a source line prior to output.
C*    PARAMETERS   
C*       INPUT  : 
c*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine fmtsrc
c
      include 'com8a.com'
c
      integer*2 ialign,ivocab,ilabel,imajor
c
      equivalence (imajor,ifl(355)), (ivocab,ifl(356))
      equivalence (ilabel,ifl(357)), (ialign,ifl(358))
c
c...Use old style of formatting
c
      if (ialign.eq.0 .and. ivocab.eq.0 .and. ilabel.eq.0 .and.
     1        imajor.eq.0) then
          call fmtold
      else
          call fmtnew
      endif
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmtold
c*       Format a source line according to the 'old' style (*SET/INDENT)
C*    PARAMETERS   
C*       INPUT  : 
c*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine fmtold
c
      include 'com8a.com'
c
      integer*2 indall,indsep,sepcol,begin,endpt
      integer*4 ip,strlen1,bgncnt,endcnt,i,j,nc,nl,shift
c
      character*1 aw21(100)
      character*(MAX_LEN) ctemp,w2tmp
c
      equivalence (indall,ifl(230)), (indsep,ifl(231))
      equivalence (aw21, w2)
c
c...Initialize routine
c
      call nclf_getw2 (1,w2,ncw2,it)
      endcnt = ncw2
      w2(ncw2+1:) = ' '
c
c.....Skip formatting if the line is a command that takes a text string
c.....as input - ASF 1/14/14.
c
      begin = 1
      endpt = 6
      w2tmp(begin:endpt) = w2(begin:endpt)
      call nupper(w2tmp,begin,endpt)
      if (((w2tmp(1:1).eq.'$'.and.w2tmp(2:2).eq.'$').or.
     1      w2tmp(1:1).eq.'%').or.(w2tmp(1:6) .eq. 'INSERT' .or.
     2      w2tmp(1:6).eq.'LETTER'.or.w2tmp(1:6).eq.'PARTNO'.or.
     2      w2tmp(1:6).eq.'PPRINT'.or.w2tmp(1:6).eq.'INCLUD'.or.
     3      w2tmp(1:6).eq.'REMARK'.or.w2tmp(1:4).eq.'DBFN'  .or.
     4      w2tmp(1:4).eq.'UBFN'  .or.w2tmp(1:4).eq.'READ'  .or.
     5      w2tmp(1:5).eq.'LOADU' .or.w2tmp(1:6).eq.'PROMPT'.or.
     6      w2tmp(1:6).eq.'TITLES'.or.w2tmp(1:5).eq.'SAVEU')) goto 8000
c
c...*SET/INDENT,ALL
c
      if (indall .gt. 0) then 
          ip = nindex(w2,' ')
c
c...Find the begining and end of the line
c
          bgncnt = ip - 1
          shift = indall - 1
c
c......If the line does not start in column one
c......shift evrything to left by bgncnt
c
          if ((shift + endcnt) .le. ifl(106)) then 
              if (bgncnt .gt. 1) then
                  do 100 i = 1, (endcnt-bgncnt)
                      aw21(i) = aw21(i+bgncnt)
  100             continue
c
                  endcnt = endcnt - bgncnt
                  w2(endcnt+1:) = ' '
              endif
c
              do 200 i = endcnt, 1, - 1
                  aw21(i + shift) = aw21(i)
  200         continue
              w2(1:shift) = ' '
              endcnt = endcnt + shift
          endif
      endif
c
c......*SET/INDENT,SEP
c
      if (indsep .gt. indall) then
          sepcol = 0
          do 500 i = 1, endcnt
              if (aw21(i) .eq. '=' .or.
     x            aw21(i) .eq. '/' .or.
     x            aw21(i) .eq. ',') then
                  sepcol = i
                  go to 600
              endif
  500     continue
  600     if (sepcol .gt. 0 .and. sepcol .lt. indsep) then
              shift = indsep - sepcol
              do 700 i = endcnt, sepcol, -1
                  aw21(i + shift) = aw21(i)
  700         continue
              w2(sepcol:sepcol+shift-1) = ' '
          endif
      endif
c
c...Shift lines down if greater than ifl(106) cols
c
      nc = strlen1(w2)
      if (nc .gt. ifl(106)) then
          call nclf_putw2 (1,w2,nc,it)
          do 900 i=1,ifl(123),1
              nl = ifl(123)
              call nclf_getw2 (i,w2,ncw2,it)
              w2(ncw2+1:) = ' '
              if (ncw2 .gt. ifl(106)) then
                  do 800 j=ncw2,1,-1
                      if ((w2(j:j) .eq. '=' .or. w2(j:j) .eq. '/' .or.
     1                    w2(j:j) .eq. ',') .and. j .lt. ifl(106))
     2                        then
                          if (i .eq. nl) then
                              w2tmp = w2(j+1:ncw2)
                              ifl(123) = ifl(123) + 1
                          else
                              call nclf_getw2 (i+1,w2tmp,nc,it1)
                              w2tmp(nc+1:) = ' '
                              if (w2(ncw2:ncw2) .eq. '$') ncw2 = ncw2-1
                              ctemp = w2(j+1:ncw2) // w2tmp
                              w2tmp = ctemp
                          endif
                          nc = strlen1(w2tmp)
                          call nclf_putw2 (i+1,w2tmp,nc,it1)
                          w2(j+1:) = '$'
                          nc = j + 1
                          call nclf_putw2 (i,w2,nc,it)
                          go to 900
                      endif
  800             continue
              endif
  900     continue
c
c...Write modified line
c
      else
          call nclf_putw2 (1,w2,nc,it)
      endif
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine fmtnew
c*       Format a source line according to the 'new' style.
C
C        We want to be able to format the command line. The available
C        formatting options are for labels, vocabulary, and major words
C        the user may select either upper or lower case. And the user
C        may select how much space each parameter should take up.
C        JLS 2/16/00 
c
C*    PARAMETERS   
C*       INPUT  : 
c*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine fmtnew
c
      include 'com8a.com'
c
      integer*2 indall,indsep,imajor,ivocab,ilabel,isav,fslash,ialign,
     1          irtrn,icomb,ispace,idiff, icnt, ibegin(768),istrt(768),
     2          iend(768),ik,isep,isum,inc72,jj,jk,icmt,ibsav,imsav,
     3          issav,iqot,incsv, isemic
      integer*4 i,j,nc,ncc,strlen1
c
      character*1 cstrg(1536)
      character*(MAX_LEN) svcmsg,lcmt
      character*1536 ccstrg
c
      equivalence (indall,ifl(230)), (indsep,ifl(231))
      equivalence (imajor,ifl(355)), (ivocab,ifl(356))
      equivalence (ilabel,ifl(357)), (ialign,ifl(358))
c
      equivalence (cstrg,ccstrg)
c
c...Initialize routine
c
      irtrn = ifl(123)
      inc72 = 1
      if (indall.gt.1) then
         isum = indall -1
      else
         isum = 0
      endif
      icnt = 1
      inx = 1
      ispace = 0
      fslash = 0
      isep = 0
      call nclf_getw2 (1,w2,ncw2,it)
      w2(ncw2+1:) = ' '
      svcmsg = w2(1:ncw2)
      nextyp = 0
c
c...ifl(123) contains the number of lines to write
c...Put everything that is in w2 into cstrg.
c
      ccstrg = ' '
      ncc = 0
      icmt = 0
      do 200 i=1,ifl(123),1
          call nclf_getw2 (i,w2,ncw2,it)
          w2(ncw2+1:) = ' '
          do 100 j=1,ncw2,1
              cstrg(ncc+j) = w2(j:j)
              if (w2(j:j) .eq. '$' .and. w2(j+1:j+1) .eq. '$') icmt = 1
              if (w2(j:j) .eq. '$' .and. icmt .eq. 0) then
                  ncw2 = j
                  go to 110
              endif
  100     continue
  110     ncc = ncc + ncw2
  200 continue
c
c...Remove spaces from array
c...And check for comments
c
      jj = 0
      icmt = 0
      iqot = 0
      do 300 ik=1,ncc,1
          if (icmt .ne. 0) then
              icmt = icmt + 1
              lcmt(icmt:icmt) = cstrg(ik)
              jj = jj + 1
          else if (cstrg(ik) .eq. ' ' .and. iqot .eq. 0) then
              jj = jj + 1
          else if ((cstrg(ik) .eq. '$' .and. cstrg(ik+1) .eq. '$')
     1             .or. cstrg(ik) .eq. '%') then
              icmt = 1
              jk = ik - 1
              if (jk .lt. 0) jk = 0
              jj = jj + 1
              lcmt(icmt:icmt) = cstrg(ik)
          else if (cstrg(ik) .eq. '$') then
              jj = jj + 1
          else
              if (cstrg(ik) .eq. '"') iqot = 1 - iqot
              cstrg(ik-jj)=cstrg(ik)
          endif
  300 continue
c
c...Blank out remaining portion of array
c
      ncc = ncc - jj
c
c...Put cstrg into ain so that it may be parsed
c
      cin = ccstrg(1:ncc)
      nccin = ncc
C
C...Upper case cin so that parser will recognize words.
C
      call nupper (cin(1:nccin),1,nccin)
C
C...If it is a comment restore it to original state.
C...Same with text commands
C
      if (((ain(1).eq.'$'.and.ain(2).eq.'$').or.ain(1).eq.'%') .or.
     1     (cin(1:6) .eq. 'INSERT' .or. cin(1:6) .eq. 'LETTER' .or.
     2      cin(1:6) .eq. 'PARTNO' .or. cin(1:6) .eq. 'PPRINT' .or.
     3      cin(1:6) .eq. 'INCLUD' .or. cin(1:6) .eq. 'REMARK' .or.
     4      cin(1:4) .eq. 'DBFN' .or. cin(1:4) .eq. 'UBFN' .or.
     5      cin(1:4) .eq. 'READ' .or. cin(1:5) .eq. 'LOADU' .or.
     6      cin(1:6) .eq. 'PROMPT' .or. cin(1:6) .eq. 'TITLES' .or.
     7      cin(1:5) .eq. 'SAVEU')) goto 8000
      inx = 1
      icomb = 0
      ifl(44) = 0

      do while (inx .le. nccin)
          isav = inx
          isemic = 0
          call parser
          if (token2.eq.' ') then
              if (ain(inx) .eq. ';') isemic = 1
              inx = inx +1
              if (isemic .eq. 0) go to 1000
          endif
          if (token2 .eq. '/') fslash = 1
          if (ain(inx) .eq. '$') cstrg(inx) = ' '
c
c...If it is a delimeter other than a comma
c...Then treat it as part of the previous token
c...along with the next token (ex. b=ON, 3/4, etc.)
c
          if ((ityp .eq. 5 .and. ist .ne. 5 .and. ist .ne. 9 .and.
     1        isep .eq. 1 .and. icnt .gt. 1) .or. icomb .eq. 2 .or.
     2        isemic.eq.1) then
              icnt   = icnt   - 1
              if (icomb .eq. 0) icomb  = 1
          endif
C
C...Indent the separator if neccessary, and indicate that the
C...separator has been found.
C
          if ((token2.eq.'/'.or.token2.eq.'='.or.token2.eq.',').and.
     x         isep.eq.0) then
              isep = 1
              if (inx+indall .lt. indsep) isum = indsep-isav
          endif
c
c...If combining parameters (b=ON)
c...Don't restore istrt and ibegin
c
          if (icomb .eq. 0) then
              istrt(icnt) = isav
              ibegin(icnt) = isum+istrt(icnt)
              issav = istrt(icnt)
              ibsav = ibegin(icnt)
              imsav = isum
              incsv = inc72
          else if (icomb .eq. 1) then
              icomb = 2
          else if (icomb .eq. 2) then
              istrt(icnt) = issav
              ibegin(icnt) = ibsav
              isum = imsav
              inc72 = incsv
              icomb = 0
          endif
          iend(icnt) = inx - 1
C
C...If this is a parameter, then we need to align it
C
          if (fslash.eq.1.and.(ityp.eq.1.or.ityp.eq.2.or.ityp.eq.3
     x        .or.ityp.eq.4)) then
C
C...idiff is the location of the end of this token.
C...See how much space needs to be added.
C
              idiff = ibegin(icnt)+(iend(icnt)-istrt(icnt))
C
C...If idiff is greater than 72 we are going to have to adjust idiff
C...to determine how many spaces are added.
C
              if (idiff .gt. ifl(106))
     1            idiff = idiff - ((inc72-1)*ifl(106))
              if (ialign.gt.0) then
                  ispace = mod(idiff,ialign)
              else
                  ispace = 0
              endif
              if (ispace.gt.0) ispace = ialign - ispace
              ibegin(icnt) = ibegin(icnt) + ispace
C
C...Have to make allowances for when the string is expanded
C...further than 72 characters. Add current isum plus the number
C...of spaces that are needed to get to the location of the
C...the next line (ie. 73,145,...) plus the number of spaces
C...for indall
C
              idiff = ibegin(icnt)+(iend(icnt)-istrt(icnt))
              if (idiff.ge.((inc72*ifl(106)))) then
                  isum = isum + 
     x                ((inc72*ifl(106)) - (ibegin(icnt-1)+
     x                (iend(icnt-1)-istrt(icnt-1))))
                  if (indsep.gt.1) then
                      isum = isum + indsep
                      ibegin(icnt) = inc72 * ifl(106) + indsep + 1
                  else if (indall.gt.1) then
                      isum = isum+indall-1
                      ibegin(icnt) = inc72*ifl(106)+indall
                  else
                      ibegin(icnt) = inc72*ifl(106)+1
                  endif
                  if (icomb .eq. 0) ibsav = ibegin(icnt)
                  idiff = ibegin(icnt)+(iend(icnt)-istrt(icnt))
                  if (idiff .gt. ifl(106))
     1                idiff = idiff - ((inc72)*ifl(106))
                  if (ialign.gt.0) then
                      ispace = mod(idiff,ialign)
                  else 
                      ispace = 0
                  endif
                  if (ispace.gt.0) ispace = ialign - ispace
C
C...The parameters are essentially being aligned by the
C...commas that follow them, if there is no comma (end of command)
C...then we need to decrease ispace by one, otherwise the last
C...character will be lined up with the comma column
C
cc                  if (nextyp.eq.11) then
cc                      if (ispace.gt.0) then
cc                          ispace = ispace-1
cc                      else if (ialign.gt.0) then
cc                          ispace = ialign -1
cc                      endif
cc                  endif
                  ibegin(icnt) = ibegin(icnt) + ispace
                  if (icomb .eq. 0) ibsav = ibegin(icnt)
                  inc72 = inc72+1
              endif
              isum = isum + ispace
c
c...Make sure delimiter does not go past 72
c
          else
              idiff = ibegin(icnt)+(iend(icnt)-istrt(icnt))
              if (idiff.ge.((inc72*ifl(106)))) then
                  isum = isum + 
     1               ((inc72*ifl(106)) - (ibegin(icnt-1)+
     2                (iend(icnt-1)-istrt(icnt-1))))
                  if (indall.gt.1) then
                      isum = isum+indall-1
                      ibegin(icnt) = inc72*ifl(106)+indall
                  else
                      ibegin(icnt) = inc72*ifl(106)+1
                  endif
                  inc72 = inc72+1
              endif
          endif
          icnt = icnt+1
C
C...If it is a vocabulary word and we need to put it in lower
C...case, do so. Pass into nlower only each individual token.
C
          if (ityp.eq.1) then
              if ((nextyp.eq.5).or.(fslash.eq.0)) then
C
C...It is a major word, so if major words are supposed
C...to be lower case, convert it to lower case.
C
                  if (imajor.eq.2) then
                      call nlower(cstrg,isav,inx)
                  else if (imajor.eq.1) then
                      call nupper(cstrg,isav,inx)
                  endif
C
C...If it is either a loadu/, saveu/, or ubfn/ command, we don't
C...want to change the case of the label
C
                  if (ist.eq.847.or.ist.eq.848.or.ist.eq.862) then
                      ccstrg(inx-1:) = svcmsg(inx-1:)
                      istrt(icnt) = inx -1
                      iend(icnt-1) = inx - 2
                      iend(icnt) = MAX_LEN
                      if (indsep .le. inx+indall) then
                          ibegin(icnt) = isum+istrt(icnt)
                          if (icomb .eq. 0) ibsav = ibegin(icnt)
                      else
                          ibegin(icnt) = indsep 
                          if (icomb .eq. 0) ibsav = ibegin(icnt)
                      endif
                      icnt = icnt+1
                      inx = nccin
                   endif
               else if (ivocab.eq.2) then
                   call nlower(cstrg,isav,inx)
               else if (ivocab.eq.1) then
                   call nupper(cstrg,isav,inx)
               endif
C
C...If it is a label and labels should be lower case
C...call nlower
C
          else if (ityp.eq.2) then
              if (ilabel.eq.2) then
                  call nlower(cstrg,isav,inx)
               else if (ilabel.eq.1) then
                   call nupper(cstrg,isav,inx)
               endif
          endif
1000      continue
      enddo
c
c...Load temp array with the string with all the
c...proper spaces added
c
      svcmsg = ' '
      irtrn = inc72
      icnt = icnt - 1
      do 1100 i=1,icnt,1
          nc = iend(i) - istrt(i)
          svcmsg(ibegin(i):ibegin(i)+nc) = ccstrg(istrt(i):iend(i))
 1100 continue
C
C...Place the formatted string into the 'w2' array
C
      do 1200 i=1,irtrn,1
          ibeg = ifl(106)*(i-1) + 1
          nc = strlen1(svcmsg(ibeg:ibeg+ifl(106)-1))
          w2 = svcmsg(ibeg:ibeg+nc-1)
          if (i .lt. irtrn) then
              w2(nc+1:nc+1) = '$'
              nc = nc + 1
          endif
          call nclf_putw2 (i,w2,nc,irctyp)
 1200 continue
c
c...Add comment
c
      if (icmt .ne. 0) then
          if (nc .ne. 0) nc = nc + 1
          if (nc .lt .jk) nc = jk
          w2(nc+1:) = lcmt(1:icmt)
          nc = nc + icmt
          call nclf_putw2 (irtrn,w2,nc,irctyp)
      endif
      ifl(123) = irtrn
c
c...End of routine
c
 8000 return
      end
