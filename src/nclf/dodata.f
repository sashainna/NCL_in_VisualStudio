C*********************************************************************
C*    NAME         :  dodata.f
C*       CONTAINS:
C*            dodata  doquot  rmquot  shdata
C*    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dodata.f , 25.6
C*    DATE AND TIME OF LAST  MODIFICATION
C*       08/01/17 , 13:50:28
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dodata
C*      Parse a data statement.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine dodata

      include 'com.com'
      include 'sfsio.com'
      include 'vocab.com'

      integer*4 ix, nrndex, strlen1, lblfl, sub, i1
      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld, isub
      integer*2 istold
      character*(MAX_LEN) buf, vocb
      character*(MAX_PATH) nfile
      character*64 labl,ttoken

      character*27 ldel
      data ldel /'`~!@#$%^&*()=|\/?<>[]{};:- '/
      integer*2 n1, irest(4), spos, bpos, cpos, ispcfl
      integer*2 sline, eline, indx, dts, epos, tmp, iret, dpos
      integer*2 mpos1, bpos1
      integer*2 parslb, stist, filfl, delim, tpos, vocon, labon
      integer*2 lstpos, len, ltrfl, blnk, ttind
      logical*2 first
      integer*4 tsub,nc
      real*8 val 
      equivalence (rest,irest)

      ifl(44) = 9
      filfl = 0
      n1 = 0
      dot = 0
      islsh = 0
      idot = 0
      buf = cin
      first = .true.
      tsub = isvsub
10    ldtext = .true.
      call parsit
      ldtext = .false.
      if (ifl(2).gt.0) goto 999
      if (.not.vocab.and..not.geom.and..not.scalar .and.ityp.ne.9 .and.
     x    (ityp.ne.2 .and. ist.ne.1)) goto 9035
      n1 = n1+1
c
c.....Check for file name to read from   
c........filfl not equal to zero means we are not reading from a file
c........and are reading in text strings   
c
      if (ityp.eq.9.or.(ityp.eq.2 .and. ist.eq.1)) then
        if (filfl.eq.0) goto 40
        if (filfl.eq.1) goto 400
      endif
c
c.....Only create the data element now if we are not reading from a file
c
15    if (first .eq. .true.) then
          first = .false.
          call dtinit(ix)
      endif
c
c......Convert text variables to strings
c
      if (ityp .eq. 2 .and. ist .eq. 24) then
        len = 0
        call gttext (token2,len)
        tv  = 24
        if (len .ne. 0 .and. token2(1:1) .ne. '"') then
            token2 = '"' // token2(1:len) // '"'
        endif
      endif
C   ------   Need to allow for VX geom name. ------
      call dtstor(ix, n1, ityp, ist, tv, nextyp, token2, ivxsub)
      if (.not.nxteos) then
        if (nextyp.ne.9) inx = inx+1
        goto 10
      endif
      if (ain(inx).ne.';') goto 30
      ncsfl(3) = 0
20    inx = inx+1
      if (ain(inx).eq.';') goto 20
      goto 10

30    if (keyold.ne.0) call dtdele (keyold)
      ifl(262) = 1
      call ptdesc (ix, DATAST, rest)
      irest(3) = n1
      idst = DATAST
      goto 999
c
c.....Find file name
c
40    spos = index(cin(1:),'/')
      bpos = index(cin(1:),' ')
      cpos = index(cin(1:),',')
      dpos = index(cin(1:),'.')  
      epos = cpos
      if (cpos.eq.0) epos = bpos  
c
c.....Quoted text given, so ignore " on each end of string       
c
      if (cin(spos+1:spos+1).eq.'"'.and.cin(epos-1:epos-1).eq.'"') then
           spos = spos + 1
           epos = epos - 1
      endif
      sline = 0
      eline = 0
      ispcfl = 1
      vocon = 0     
      labon = 0
c
c.....Search for given file      
c
45    if (eline.gt.0.and.sline.gt.0.and.sline.gt.eline) goto 9061
      if (dpos .eq. 0 .or. dpos .gt. epos) then
          filfl = 1
          go to 400
      endif
      call flname (14, cin(spos+1:epos-1), nfile)
      call flopen (scrlun, nfile, 'OLD', 'SEQUENTIAL', 'FORMATTED',
     x             MAX_LEN, 'NULL', ioerr)
      if (ioerr.ne.0) then
        call flname(13, cin(spos+1:epos-1), nfile)
        call flopen(scrlun, nfile, 'OLD', 'SEQUENTIAL',
     1              'FORMATTED', MAX_LEN, 'NULL', ioerr)
        if (ioerr.ne.0.and.(cin(epos-4:epos-1).ne.'.csv'.and.
     x     cin(epos-4:epos-1).ne.'.txt'.and.cin(epos-4:epos-1).ne.
     x     '.CSV'.and.cin(epos-4:epos-1).ne.'.TXT')) then
          filfl = 1
          goto 400
        else if (ioerr.ne.0) then
          goto 9270
        endif
      endif
c
c.....Check for subscripted label eg. "d1"      
c
      labl = savid2(1:64)
      if (isvsub .ne. 0) then
        iret = 0
      else
        iret = parslb(labl,isvsub)
        ifl(296) = iret
        isub = iret
        token2 = savid2
        ivxsub = 0
        isvsub = 0
        if (iret.eq.0) then
          savid2 = token2
          keyold = keyhld
          ifl(9) = ifl(11)
          ifl(10) = ifl(12)
          idst = DATAST
          rest = 0.
          irest(3) = 32767
          irest(4) = 14
          call vstore
        endif
      endif
c
c.....Position past the filename
c
      if (nextyp .ne. 9 .and. nextyp .ne. 11) then
  101   call parsit
        if (nextyp .ne. 9 .and. nextyp .ne. 11) go to 101
      endif
c      
c.....Check for additional input      
c
      if (nextyp.eq.9) then
        call parsit
c
c.....Check for variable for storing number of data statements      
c
        if (ityp.eq.2.and.(ist.eq.1.or.ist.eq.2)) then
c
c.....Scalar (or unknown - make it a scalar)
c
          sclvar_lab = token2
          sclvar_inx = ivxsub
          sclvar_def = .true. 
c
c.....Check for start line input or error handling type         
c
          if (nextyp.eq.9) then
            call parsit
            if (scalar) then
              goto 50
            elseif (ityp.eq.1.or.ityp.eq.2) then
              if (ist.ne.549) goto 55
              goto 60
            else
              goto 9004
            endif 
          else
            call parsit
          endif  
        endif
c
c.....Check for starting and ending line numbers      
c
50      if (ityp .eq. 7) go to 60
        if (scalar) then
          sline = tv
          if (nextyp.eq.9) then
            call parsit
            if (scalar) then
              eline = tv
              call parsit
            else 
              if (ityp.eq.1.and.ist.ne.549) goto 55
              goto 60
            endif
c
c.....Input found after end line number was given              
c
            if (nextyp.ne.11) then
              if (ityp.eq.1.and.ist.ne.549) goto 55
              if ((ityp.eq.1.and.ist.eq.549) .or. ityp .eq. 7) goto 60
              goto 9004 
            endif       
          else
            call parsit
          endif
        endif
c
c.....Find error handling type and set flags/values        
c
55      if (ityp.eq.1.and.ist.ne.549 .and. ist .ne. 936) then
c
c.....Error option
c
          if (ist.eq.937) then
            ispcfl = 4
            if (nextyp.eq.9) then
              call parsit
              goto 60
            endif
c
c.....Scalar insert option
c
          else if (ist.eq.601) then
            ispcfl = 2
            if (nextyp.eq.9) then
              call parsit
              if (scalar) then
                val = tv
                if (nextyp.eq.9) then
                  call parsit
                  goto 60
                endif
              else 
                goto 9007
              endif
            else 
              goto 9007
            endif
c
c.....Space insert option
c            
          else if (ist.eq.944) then
            ispcfl = 1
            if (nextyp.eq.9) then
              call parsit
              goto 60
            endif
c
c.....Vocab insert option
c
          else if (vocab) then
            ispcfl = 3
            vocb = token2
            stist = ist
            if (nextyp.eq.9) then
              call parsit
              goto 60
            endif
          else
            goto 9061
          endif
        else
          if (ist .eq. 549 .or. ist .eq. 936 .or. ityp .eq. 7) goto 60
          goto 9061 
        endif
      endif
c
c.....VOCAB,ON-OFF
c
60    if (ityp.eq.1.and.ist.eq.549) then
        if (nextyp.eq.9) then
          call parsit
c
c.....VOCABF OFF means store as vocab words as strings 
c          
          if (ityp.eq.1.and.ist.eq.72) then
            vocon = 1
          else
            if (ityp.eq.1.and.ist.ne.71.or.ityp.ne.1) goto 9056
          endif
        else
          goto 9004
        endif      
      endif
c
c.....LABEL,ON-OFF
c
      if (ityp.eq.1.and.ist.eq.936) then
        if (nextyp.eq.9) then
          call parsit
c
c.....LABEL OFF means store labels as strings 
c          
          if (ityp.eq.1.and.ist.eq.72) then
            labon = 1
          else
            if (ityp.eq.1.and.ist.ne.71.or.ityp.ne.1) goto 9056
          endif
        else
          goto 9004
        endif      
      endif
c        
c.....Read file and store each line as a new data statement  
c
      tmp = 0 
      dts = 0
      if (tsub .eq. 0) then
        isvsub = isvsub + 1
      else
        isvsub = tsub
      endif
70    read (scrlun, 1070, end = 80, err = 9289) buf
1070  format(A)
      tmp = tmp + 1
      if (sline.gt.0.and.tmp.lt.sline) goto 70
c
c.....Case sensitive
c
      if (buf.eq.' ') goto 70 
      bpos = nrndex(buf(1:),' ') + 1
      if (bpos.eq.1) goto 70
      do i=1,bpos-1,1
        if (buf(i:i) .eq. '	') buf(i:i) = ','
      enddo
      call touppr(buf,buf)
      cin = buf(1:bpos-1) // char(177)
cc      nccin = strlen1(buf);
      ifl(191) = 1
      inx = 1
      indx = 1
c
c.....Need to increment label instead of subscript      
c
      if (iret.gt.0) then
        isvsub = 0
        ivxsub = 0
        i1 = 1
        call nclf_format_label (labl,isub,savid2,i1)
        isub = isub + 1
        token2 = savid2
      endif
      ix = 0
      ttind = 1
      call dtinit(ix)
cc      do i=1,bpos
      nextyp = 0
      do while (nextyp .ne. 11)
        if (cin(inx:inx) .eq. ',') then
          ityp = 5
          ist = 9
          inx = inx + 1
        else
          ldtext = .true.
          call parsit
          ldtext = .false.
        endif
c
c.....Empty entry found.  Should be filled based on error type
c
        if ((ityp .eq. 5 .and. ist .eq. 9) .or. ityp .eq. 7) then
          if (ispcfl .eq. 3) then
            token2 = vocb
            ityp = 1
            tv = 0
            ist = stist
          else if (ispcfl .eq. 2) then
            tv = val
            ityp = 3
            call rtoc (tv,token2,nc)
          else if (ispcfl .eq. 4) then
				token2 = ',,'
            go to 9267
          else
            ityp = 2
            ist = 24
            tv = 24
            token2 = ' '
          endif
          ifl(2) = 0
          err = .false.
        endif
c
c......Parser returns numbers as text strings
c......(required for parsing some unquoted strings)
c......So check for a valid number
c......Also allow for numeric labels and index id's
c......Bobby - 03/10/15
c
        if (ityp .eq. 2 .and. (ist .eq. 1 .or. ist .eq. 13 .or.
     1      ist .eq. 15)) then
          bpos = nindex(token2(1:),' ')
          nc = strlen1(token2)
c
c.....    QAR 101098: do not convert string to number if string has " " or "-" inside
c
          mpos1 = index(token2(bpos:),'-')
          bpos1 = index(token2(bpos:),' ')
          if (mpos1 .le. 1 .and. bpos1 .gt. nc) then
            call ctor (token2,tv,ierr)
            if (ierr .eq. 0) ityp = 4
          endif
        endif
c
c......Convert text variables to strings
c
        if (ityp .eq. 2 .and. ist .eq. 24 .and. labon .eq. 0) then
          len = 0
          call gttext (token2,len)
        endif
c
c......Token is a text string or unknown type
c
        if (ityp .ge. 5 .or. (ityp .eq. 2 .and. ist .eq. 1) .or.
     1      (ityp .eq. 2 .and. labon .eq. 1) .or.
     2      (ityp .eq. 1 .and. vocon .eq. 1)) then
c
c.........The string can only have 63 or fewer characters so the quotes
c.........are included when the string is stored in dtstor.
c
          call doquot (token2)
          ityp = 2
          ist = 24
          tv = 24
          ifl(2) = 0
          err = .false.
        endif
c
c......Store data element
c
        call dtstor(ix, indx, ityp, ist, tv, nextyp, token2, ivxsub)
        indx = indx + 1
      enddo
      dts = dts + 1
      ifl(191) = 0
c
c.....Added check for matching DATA statement.  Remove the old version
c.....if a match is found - ASF 10/24/13.
c
      call vxchk (savid2, isvsub, keyold, ipg, iel, nwds, ist)
      if (keyold.ne.0) call dtdele (keyold)
      ifl(262) = 1
      call ptdesc (ix, DATAST, rest)
      irest(3) = n1 + indx - 2
      idst = DATAST
      call vstore
      if (iret.eq.0) isvsub = isvsub + 1
      if (eline.gt.0.and.tmp.ge.eline) goto 80
      goto 70
80    close(scrlun)

c
c.....Set the number of data statements scalar      
c
      if (sclvar_def) then
c
c.....Reset token2 and savid2 so that vstchk and vstore
c.....search for and save the correct entities    
c
        savid2 = sclvar_lab
        token2 = sclvar_lab
        ivxsub = sclvar_inx
        isvsub = sclvar_inx
        call vstchk
        rest = dts
        idst = 2
        keyold = keyhld
        istold = ist
        ifl(9) = ifl(11)
        ifl(10) = ifl(12)
        call vstore
        sclvar_def = .false.
      endif     
      defwf = .true.
      goto 999
c
c.....Treat unknown identifiers as strings
c      
  400 call doquot (token2)
      ityp = 2
      tv = 24
      ist = 24
      goto 15
      
c.....End of statement expected
9004  ifl(2) = 4
      call dtdele(ix)
      goto 999
c.....Number or scalar expected
9007  ifl(2)=7
      goto 999 
c.....Number or vocabulary word expected
9035  ifl(2) = 35
      call dtdele(ix)
      goto 999
c.....ON or OFF expected
9056  ifl(2) = 56
      call dtdele(ix)
      goto 999      
c.....Invalid input
9061  ifl(2) = 61
      close(scrlun)
      call dtdele(ix)
      goto 999      
c.....Identifier expected
9087  ifl(2)=87
      goto 999       
c.....Wrong file format
9267  ifl(2) = 267
      close(scrlun)
      call dtdele(ix)
      goto 999           
c.....File not found
9270  ifl(2) = 270
      call dtdele(ix)
      goto 999
c.....Scalar variable expected      
9282  ifl(2) = 282
      call dtdele(ix)
      goto 999 
c.....I/O error reading file
9289  ifl(2) = 289
      close(scrlun)
      call dtdele(ix)
      goto 999       

999   continue
      call ncl_update_data_frm()
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine doquot (ctok)
C*      Restore quotes in a data string token so it is parsed correctly
C*      when it is referenced in the 'preprs' routine.
C*    PARAMETERS
C*       INPUT  :
C*          ctok   = Token to parse.
C*       OUTPUT :
C*          ctok   = Token with added quotes.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine doquot (ctok)
c
      character*(*) ctok
c
      integer*4 nrndex,blnk
c
c...Add quotes around text string
c
      blnk = nrndex(ctok,' ')
      if (blnk .eq. 0) then
        ctok = '""'
      else
        ctok = '"' // ctok(1:blnk) // '"'
      endif
      blnk = blnk + 2
c
c...Add extra quotes to internal quotes
c...i.e. "the "string"" becomes "the ""string"""
c
      ien = blnk
      do 100 i=blnk-1,2,-1
        if (ctok(i:i) .eq. '"') then
          ctok = ctok(1:i) // '"' // ctok(i+1:ien)
          ien = ien + 1
        endif
  100 continue
c
c...End of routine
c
 8000 return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine rmquot (ctok,klen)
C*      Remove quotes in a data string token so that it takes the same
C*      format as when it was parsed.
C*    PARAMETERS
C*       INPUT  :
C*          ctok   = Token to parse.
C*          klen   = Length of 'ctok'.
C*       OUTPUT :
C*          ctok   = Token with removed quotes.
C*          klen   = Length of 'ctok'.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine rmquot (ctok,klen)
c
      character*(*) ctok
c
      integer*2 klen
c
c...Remove quotes from text string
c
      if (ctok(1:1) .eq. '"' .and. ctok(klen:klen) .eq. '"') then
        ctok = ctok(2:klen-1)
        klen = klen - 2
      endif
c
c...Change internal "" quotes to "
c
      ien = klen
      do 100 i=ien,2,-1
        if (ctok(i-1:i) .eq. '""') then
          ctok = ctok(1:i-1) // ctok(i+1:klen)
          klen = klen - 1
        endif
  100 continue
c
c...End of routine
c
 8000 return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine shdata
C*      Show a data statement.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine shdata

      include 'com.com'

      integer*4 keydt,sub, ierr, subscr, i2c,len,qte,rindex1
      integer*4 nc,strlen1
      character*6 chnt
      integer*2 n1, ix, jstat, lbr, rbr, tix, blnk, tinx
      character*64 svtok2
      character*64 label
      character*64 oput
      character*64 oput2
      logical*2 stlstr

      jstat = 0
      svtok2 = token2
      stlstr = lstrng
      blnk = 0
      call gtdesc(tv, keydt, n1, ist)
      lbr = index(cin,'[') + 1
      rbr = index(cin,']') - 1
      if (lbr.gt.1.and.rbr.gt.1) then
          tinx = inx
          inx = lbr
          call parsit
          inx = tinx
          tix = itv - 1
c        call ctoi(cin(lbr:rbr),subscr,ierr)
c        tix = subscr - 1
        if (tix.ge.n1) goto 9085
        n1 = 1
      else
        blnk = index(cin,' ')
        lbr = blnk
        tix = 0
      endif
      do 100,ix=1,n1
        ivxsub = 0
        lstrng = .false.
        call dtgetv (keydt, ix+tix, tv, ityp, nextyp, label, sub)
        nc = strlen1(label)
        length = nc
        call rmquot (label,length)
        if (ityp.eq.1) then
          ist = -tv
c...no longer use tv=24 for text string. It shouldn't use value for type
c...and it cause error. Yurong
c...        else if (ityp.eq.2.and.tv.eq.24) then
        else if (ityp.eq.24) then
          ityp = 9
          if (label(1:1).eq.'"') then
            label(1:) = label(2:)
            qte = rindex1(label,'"')
            if (qte.gt.0) label(qte:qte) = ' '
           endif
c           token2 = svtok2
           tokstr = label
           length = nrndex(label,' ')
           lstrng = .true.
        else if (ityp.eq.2) then
          token2 = label
          ivxsub = sub
          call vstchk         
        else
          ityp = 2
          ist = 2
        endif
        spos = index(cin,'/') + 1
        i2c = ix+tix
        call itoc(i2c,chnt,len)
        token2 = ' '
        if (blnk.eq.0) then
          token2 = cin(spos:lbr-1)//chnt(1:len)//']'
        else
          token2 = cin(spos:lbr-1)//'['//chnt(1:len)//']'
        endif
        call ushow (jstat)
50      if (ix.lt.n1) then
C          cout = 'Hit any key to continue, Q to quit'
C   ------   fix for nclvt etc as in begpas   ------
C          call nclpmt (cout, cout)
C          if (cout(1:1).eq.'q'.or.cout(1:1).eq.'Q') goto 999
          token2 = svtok2
        endif
        ist = 0
100   continue
      lstrng = stlstr
      goto 999
c
c...                   Subscript out of range
c
9085  call error(85)
999   continue
      return
      end
