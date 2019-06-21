c*****************************************************
c**   NAME         :  draft3.f
c**   CONTAINS     :  Handles the DRAFT/... statement.
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       draft3.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:58
c**
c*****************************************************
c**
c** COPYRIGHT (c) 1993 NCCS
c*********************************************************************
c*    E_SUBROUTINE     : subroutine drftan 
c*     Handles the DRAFT/ANOTE,... statement.
c*
c*    We call this routine from the DRAFT routine, file DRAFT.F    
c*
c*********************************************************************

      subroutine  drftan

      include 'com8a.com'

      common/dstcom/keydt, ixdt, nxthld, nxtret
      integer*4 keydt
      integer*2 ixdt, nxthld, nxtret
c
      integer*4 isub, iattr(9), jerr,ankey,indx
      integer*2 nwds,ietype,igeo,j
c
      logical imod
c
      real*8 rattr(6)
c
      character*80 lfont
      byte bfont(80)
c 
      integer*4 ipg,iel,strlen1
      real*8 asw(36)
      integer*2 ksw(144)
      character*8 cvs(35)
      character*256 comstr
c
      equivalence (vs, cvs, jb)
      equivalence (asw,ksw)
c
      integer*2 imodify, icolor, ipen, isize, iatangl, ifont, ilayer,
     1          istyle, istart, iletdir, ialign, ispace, ilinwgt,
     2          ismall
c
      integer*2 iall, istroke, istring, ileft, iright, iup, idown,
     1          inormal, icenter, ibase, imedium, iheavy, iexhvy,
     2          istd, icol(16)
c
      data imodify / 732/, icolor  / 900/, ipen   / 128/, isize  / 196/,
     1     iatangl /   1/, ifont   / 947/, ilayer / 902/, istyle / 948/,
     2     istart  /  57/, iletdir /1041/, ialign /1076/, ispace / 944/,
     3     ilinwgt / 901/, ismall  / 663/

      data iall    / 816/, istroke / 946/, istring / 945/,
     1     ileft   /   8/, iright  /  24/, iup     / 112/,
     2     idown   / 113/, inormal / 820/, icenter / 634/,
     3     ibase   / 868/, imedium /  61/, iheavy  / 921/,
     4     iexhvy  / 922/, istd    / 920/

      data  icol / 904,  905,  906,  907,  908,  909,  910,  911,
     1             912,  913,  914,  915, 1101, 1102, 1103, 1104/
c
c...Initialize routine
c
      call svpars
      igeo = 0
      ankey = 0
      lfont = ' '
      do 100 i=1,9,1
          iattr(i) = -1
  100 continue
      do 120 i=1,6,1
          rattr(i) = -1
  120 continue
c
c...DRAFT/ANOTE,MODIFY
c
      call parsit
      if (ityp .eq. 1 .and. ist .eq. imodify) then
          imod = .true.
          if (nextyp .ne. 1) go to 9000 
          call svpars
          call parsit
          do while (ityp .ne. 1)
              call parsit
              if (ityp .eq. 2 .and. ist .eq. VANOTE) then
                  igeo = 1
                  call gtdesc (tv,ankey,nwds,ietype)
                  call parsit
              else if (ityp .eq. 1 .and. ist .eq. iall) then
                  igeo = 2
                  call parsit
              else if (ityp .ne. 1) then
                  go to 9010
              endif
          enddo
      endif
c
c...Loup through the statement to ensure the syntax is correct.
c
      do while (ityp .ne. 7)
          if (ityp .ne. 1) go to 9020
          if (nextyp .ne. 1) go to 9000
c
c......DRAFT/ANOTE,COLOR
c
          if (ist .eq. icolor) then
              call parsit
              call parsit
              if (scalar) then
                  iattr(1) = itv
                  if (iattr(1) .lt. -1 .or. iattr(1) .gt. 63) go to 9030
              else if (ityp .eq. 1) then
                  do 200 i=1,16,1
                      if (ist .eq. icol(i)) then
                          iattr(1) = i - 1
                          go to 210
                      endif
  200             continue
  210             if (i .gt. 16) go to 9030
              else if ((ITYP .eq. 2 .and. ist.eq.1 ).or.
     x             (ITYP .eq. 2 .and. ist.eq.24 ).or.
     x             (ityp.eq.9)) then
                  if ((ityp.eq.9).or.(ITYP.eq.2 .and. ist.eq.1)) then
                      comstr = token2
                      j = strlen1(token2)
                      isub = ivxsub
                  endif
                  if (ityp.eq.2.and.ist.eq.24) then
                     j = 0
                     call gttext(comstr,j)
                     isub = 0
                  endif
                  call ncl_getclr_inx(comstr, j, isub, indx)
                  if (indx .ge. 16 .and. indx .le.63) then
                      iattr(1) = indx
                  else
                     go to 9030
                  endif   
              else
                  go to 9030
              endif
c
c......DRAFT/ANOTE,PEN
c
          else if (ist .eq. ipen) then
              if (.not. imod) go to 9080
              call parsit
              call parsit
              if (.not. scalar) go to 9040
              if (itv .le. 0 .or. itv .gt. 256) go to 9050
              iattr(2) = itv
c
c......DRAFT/ANOTE,SIZE
c
          else if (ist .eq. isize) then
              call parsit
              call parsit
              if (.not. scalar) go to 9040
              if (tv .le. 0) go to 9060
              rattr(1) = tv
              call parsit
              if (.not. scalar) go to 9040
              if (tv .le. 0) go to 9060
              rattr(2) = tv
c
c......DRAFT/ANOTE,ATANGL
c
          else if (ist .eq. iatangl) then
              call parsit
              call parsit
              if (.not. scalar) go to 9040
              rattr(3) = tv
c
c......DRAFT/ANOTE,FONT
c
          else if (ist .eq. ifont) then
              call parsit
              call parsit
              if (.not. lstrng) go to 9070
              ix = 0
              call gttext(lfont,ix)
              call ctob (lfont,bfont)
              call uaf_check_font (bfont,jerr)
              if (jerr .ne. 0) go to 9070
c
c......DRAFT/ANOTE,STYLE
c
          else if (ist .eq. istyle) then
              call parsit
              call parsit
              if (ityp .ne. 1) go to 9020
              if (ist .eq. istring) then
                  iattr(3) = 0
              else if (ist .eq. istroke) then
                  iattr(3) = 2
              else
                  go to 9080
              endif
c
c......DRAFT/ANOTE,START
c
          else if (ist .eq. istart) then
              call parsit
              call parsit
              if (.not. scalar) go to 9040
              if (itv .lt. 0 .or. itv .gt. 8) go to 9090
              iattr(4) = itv
c
c......DRAFT/ANOTE,LETDIR
c
          else if (ist .eq. iletdir) then
              call parsit
              call parsit
              if (ityp .ne. 1) go to 9020
              if (ist .eq. iright) then
                  iattr(5) = 0
              else if (ist .eq. ileft) then
                  iattr(5) = 1
              else if (ist .eq. iup) then
                  iattr(5) = 2
              else if (ist .eq. idown) then
                  iattr(5) = 3
              else
                  go to 9080
              endif
c
c......DRAFT/ANOTE,ALIGN
c
          else if (ist .eq. ialign) then
              call parsit
              call parsit
              if (ityp .ne. 1) go to 9020
              if (ist .eq. inormal) then
                  iattr(6) = 0
              else if (ist .eq. ileft) then
                  iattr(6) = 1
              else if (ist .eq. icenter) then
                  iattr(6) = 2
              else if (ist .eq. iright) then
                  iattr(6) = 3
              else
                  go to 9080
              endif
c
              call parsit
              if (ityp .ne. 1) go to 9020
              if (ist .eq. inormal) then
                  iattr(7) = 0
              else if (ist .eq. iup) then
                  iattr(7) = 1
              else if (ist .eq. icenter) then
                  iattr(7) = 2
              else if (ist .eq. ibase) then
                  iattr(7) = 3
              else if (ist .eq. idown) then
                  iattr(7) = 4
              else
                  go to 9080
              endif
c
c......DRAFT/ANOTE,SPACE
c
          else if (ist .eq. ispace) then
              call parsit
              call parsit
              if (.not. scalar) go to 9040
cc              if (tv .le. 0) go to 9060
              rattr(4) = tv
              call parsit
              if (.not. scalar) go to 9040
cc              if (tv .le. 0) go to 9060
              rattr(5) = tv
c
c......DRAFT/ANOTE,LINWGT
c
          else if (ist .eq. ilinwgt) then
              call parsit
              call parsit
              if (scalar) then
                  if (itv .lt. 0 .or. itv .gt. 15) go to 9030
              else if (ityp .eq. 1) then
                  if (ist .eq. istd) then
                      itv = 0
                  else if (ist .eq. imedium) then
                      itv = 1
                  else if (ist .eq. iheavy) then
                      itv = 2
                  else if (ist .eq. iexhvy) then
                      itv = 3
                  else
                      go to 9080
                  endif
              else
                  go to 9020
              endif
              iattr(8) = itv
c
c......DRAFT/ANOTE,LAYER
c
          else if (ist .eq. ilayer) then
              if (.not. imod) go to 9080
              call parsit
              call parsit
              if (.not. scalar) go to 9040
              if (itv .lt. 0 .or. itv .gt. 9999) go to 9100
              iattr(9) = itv
c
c......DRAFT/ANOTE,SMALL
c
          else if (ist .eq. ismall) then
              call parsit
              call parsit
              if (.not. scalar) go to 9040
              if (tv .le. 0) go to 9060
              rattr(6) = tv
c
c......Invalid minor word
c
          else
              go to 9020
          endif
c
c......Get next parameter
c
cc          call parsit
          call parsit
      enddo
c
c...Set the default annotation attributes
c
      if (igeo .eq. 0) then
          ankey = 0
          call uaf_set_txt_attr (ankey,iattr,rattr,bfont,ierr)
c
c......Set by annotation list
c
      else if (igeo .eq. 1) then
          call rtpars
          call parsit
          do while (ityp .ne. 1)
              call parsit
              if (ityp .eq. 2) then
                  call gtdesc (tv,ankey,nwds,ietype)
                  call uaf_set_txt_attr (ankey,iattr,rattr,bfont,ierr)
                  if (ierr .ne. 0) go to 9110
              endif
          enddo
c
c......Set ALL annotations
c
      else
          call vxlfst
          do while (ietype .ne. 1)
              call vxlnxt (token2,isub,ankey,nwds,ietype,ipg,iel)
              if (ietype .eq. VANOTE) then
                  call uaf_set_txt_attr (ankey,iattr,rattr,bfont,ierr)
                  if (ierr .ne. 0) go to 9110
              endif
          enddo
      endif
      if (ierr .ne. 0) go to 9110
c
c...End of routine
c
 8000 return
c
c...Equals expected
c
 9000 call error (6)
      go to 8000
c
c...Anote expected
c
 9010 call error (523)
      go to 8000
c
c...Vocabulary word expected
c
 9020 call error (178)
      go to 8000
c
c...Vocabulary word expected
c
 9030 call error (428)
      go to 8000
c
c...Scalar expected
c
 9040 call error (7)
      go to 8000
c
c...Invalid pen number
c
 9050 call error (181)
      go to 8000
c
c...Positive value required
c
 9060 call error (112)
      go to 8000
c
c...Font expected
c
 9070 call error (524)
      go to 8000
c
c...Invalid vocabulary word
c
 9080 call error (182)
      go to 8000
c
c...Input value of of range
c
 9090 call error (445)
      go to 8000
c
c...Invalid layer number
c
 9100 call error (431)
      go to 8000
c
c...Coule not modify attributes
c
 9110 if (ierr .eq. 1) then
          call error (527)
      else
          call error (528)
      endif
      go to 8000
      end
