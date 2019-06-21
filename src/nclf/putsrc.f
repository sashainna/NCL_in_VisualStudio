C*********************************************************************
C*    NAME         :  putsrc.f
C*       CONTAINS:
C*            putsrc  shftw2
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       putsrc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:33
C********************************************************************/
C*********************************************************************
C*    E_SUBROUTINE     : subroutine putsrc
c*       write a record to the source file.    
C*    PARAMETERS   
C*       INPUT  : 
c*          cmsg = the array that contains the data to be output          
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine putsrc

      include 'com8a.com'

      integer*2 dlen
      integer*4 i,j,nc,strlen1,nc1,nc2,nc4,ktyp,idum,ifl123
c
      character*64 dummy
      character*(MAX_LEN) ctemp,csav
c
c...Initialize routine
c
      if (SIMCOM) return
      ifl123 = ifl(123)

      if (debug) then
          write (cout,1212) ifl(123), nline, ncsfl(5), idst, isvtyp
1212      format('putsrc ifl(123)=',i2,' nline=',i4,' ncsfl5=',i1,
     x           ' idst=',i4,' isvtyp=',i4)
          call putmsg(cout,80,20,0)
      endif
      csav = cimage(1:nccimg)
      nc2 = nccimg
C
C...source line number in ifl(212) being written back from
C...a *edit command
C
      if (ifl4(2).gt.0) then
          call wrtpp (cimage,nccimg,ifl4(2)-1,0,0)
          ifl4(2)=0
          go to 99999
      endif

C
C...If ncsfl(5) is equal to one then this is a * command
C...and should not be written to the part program.
C
      if (ncsfl(5) .eq. 1 .and. .not. fromt) nline = nline + 1
      if (ncsfl(5) .eq. 1) return
C
C...if nline le pmode: current statement is from file
C...if nline gt pmode: current statement is from console
C
      if ((.not.fromt).and.isvtyp.eq.0.and.ncsfl(5).ne.2) go to 110
      if (isvtyp.ne.0) then
C
C...bump the auto name gen counter
C
          if (idst.eq.18) then
              ifl(143)=isvtyp
          else if (idst.eq.20) then
              ifl(277)=isvtyp
          else if (idst.eq.21) then
              ifl(309)=isvtyp
          else if (idst.eq.30) then
              ifl(373)=isvtyp
          else if (idst.eq.31) then
              ifl(380)=isvtyp
          else if (idst .le. 10) then
              ifl(10+idst)=isvtyp
          endif
c
c...If there is a jumpto label, write that to cout
c
          call nclf_getw2 (1,w2,ncw2,it)
          if (ifl(281) .ne. 0) cout(1:ifl(281)) = w2(1:ncw2)
c
c...call expnam to build label from possible subscripted geometry.
c...if subscripted, add parenthesis and subscript
c
          call expnm2(savid2, isvsub, dlen,cout(ifl(281)+1:ifl(281)+1))
c
c...Force update of automatically generated name index.
c...Use dummy variables so saveid will not be modified.
c
          call namgen (1, idst, dummy, idum)
c
c...If adding label to line exceeds maximum line size (72)
c...then shift characters down in the w2 array
c
         nc4 = ncw2 + dlen + 1 + ifl(281)
         if (nc4 .gt. ifl(106)) then
             call shftw2 (dlen+1)
             call nclf_getw2 (1,w2,ncw2,it)
         endif
c
c...Write '=' and remainder of statement to cout
c
          cout(dlen+IFL(281)+1:dlen+ifl(281)+1)='='
          cout(dlen+2+IFL(281):)=w2(ifl(281)+1:ncw2)
          w2=cout
          ncw2 = strlen1(w2)
          call nclf_putw2 (1,w2,ncw2,it)
          if (ifl(123).eq.1) then
              cimage=w2
              nccimg = strlen1(cimage)
          endif
C
C...save nline in case of error
C
          isvtyp=0
      endif
c
c...Format the source line
c...If we are stepping thru a part program, we do not want
c...to use the formatting options. Branch past formatting code.
c
      call getsrc (ctemp,nc1,svll-1,0)
      call nclf_getw2 (1,w2,ncw2,it)
      if (ifl(360) .ne. 0 .and. (nc1 .ne. ncw2 .or.
     1    ctemp(1:nc1) .ne. w2(1:nc1))) call fmtsrc
C
C...insert mode
C
      insm = 0
      if (ifl(25).eq.2) then
          insm = 1
          ifl4(1)=ifl4(1)+ifl(123)
      endif
c
c...Write records
c
      insm = ifl123
      if (ifl(25) .eq. 2) insm = 0
      call nclf_storew2 (svll-1,ifl(123),insm)

      if (svll+ifl(123)-1 .gt. ifl4(1)) ifl4(1) = svll+ifl(123)-1

c          If jumpto or if statement changed line number of next
c          executable statement, update svll with new line number now
c          that the jumpto or if statement has been written to the
c          source file
110   if (ifl4(4).ne.-1) call nclf_src_rec_to_line (ifl4(4),svll)
      ifl4(4)=-1
C...
C...To correct the line number in case of *skip/to,no. kathy
C...
C...LSKIP is also used by CONDIF to tell PUTSRC and DRIVER to
C...not update NLINE.  CONDIF sets NLINE to the source 
C...statement number that should be branched to and it should
C...not be altered.
C
      if (.not.lskip) then 
          nline=svll+ifl(123)
      endif

      if (debug) then
          write (cout,1232) ifl(123),svll
1232      format('putsrc: ifl(123)= ',i2,' svll= ',i4)
          call putmsg(cout,80,22,0)
      endif

99999 return
      end
C
C*********************************************************************
C*    SUBROUTINE     : shftw2 (cmsg,ispc,istrt,iend)
C*          Shifts the w2 array down to accomodate entity label assignment
C*          which causes the first line to exceed 72 columns.
c*
C*    PARAMETERS
C*       INPUT  :
C*          klen     I*2  D1  Length of label to add to command line.
C*       OUTPUT : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine shftw2 (klen)
c
      include 'com8a.com'
c
      logical idid
      integer*4 nc,nc1,strlen1,jlen
c
      character*(MAX_LEN) tbuf,sbuf
c
c...Initialize routine
c
      jlen = klen
      tbuf = ' '
c
c...Loop through lines
c
      do 1000 i=1,ifl(123),1
          call nclf_getw2 (i,w2,nc,it)
          if (nc+jlen .gt. ifl(106)) then
              nc1 = nc - ifl(106)
              idol = 0
              if (w2(nc:nc) .eq. '$') then
                  idol = 1
                  w2(nc:nc) = ' '
                  nc = nc - 1
              endif
              inc = nc
              do while (((w2(inc:inc) .ne. ',' .and.
     1            w2(inc:inc) .ne. '/' .and. w2(inc:inc) .ne. '=') .or.
     2            inc+jlen+idol .gt. ifl(106)) .and. inc .gt. 0)
                  inc = inc - 1
              enddo
              if (inc .eq. 0) inc = nc
              if (tbuf .eq. ' ') then
                  sbuf = w2(1:inc)
              else
                  sbuf = tbuf(1:jlen) // w2(1:inc)
              endif
              if (inc .lt. nc) then
                  tbuf = w2(inc+1:nc)
              else
                  tbuf = ' '
              endif
              w2 = sbuf
              nc = strlen1(sbuf)
              if (idol .eq. 1 .or. i .eq. ifl(123)) then
                  nc = nc + 1
                  w2(nc:nc) = '$'
              endif
              jlen = strlen1(tbuf)
c
c...Characters fit on current line
c
          else if (tbuf .ne. ' ') then
              sbuf = tbuf(1:jlen) // w2(1:nc)
              w2 = sbuf
              nc = jlen + nc
              tbuf = ' '
              jlen = 0
          endif
          call nclf_putw2 (i,w2,nc,it)
 1000 continue
c
c...Characters left over
c...Make new line
c
      if (tbuf .ne. ' ') then
          ifl(123) = ifl(123) + 1
          nc = strlen1(tbuf)
          call nclf_putw2 (ifl(123),tbuf,nc,it)
      endif
c
c...End of routine
c
 8000 return
      end
