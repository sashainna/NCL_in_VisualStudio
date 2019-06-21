C*********************************************************************
C*    NAME         :  ifelse.f
C*       CONTAINS:
C*      subroutine ifelse(iflg, iline, ilodpp, inot)
C*      subroutine ifinit
C*      subroutine ifjump(lnum, kerr)
C*      subroutine ifunmt(lstart, lend, lnum)
C*    COPYRIGHT 2003 (c) Numerical Control Computer Sciences Inc.
C*    All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*        ifelse.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:11
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ifelse
C*     Handle if-then-else statements.
C*    PARAMETERS
C*       INPUT  :
C*          iflg    - =1 if declaring loop or macro, else 0.
C*          iline   - Line number of current statement.
C*          ilodpp  - =0 if not loadpp.
C*                    =1 if loadpp and loop or macro declaration.
C*                    =2 if loadpp and loop or macro exec before
C*                       current line.
C*                    =3 if loadpp and loop or macro exec on or after
C*                       current line.
C*       OUTPUT :
C*          inot    - =1 if no THEN found for IF statement, else 0
C*          iline   - Line number of next statement to execute.
C*          ilodpp  - =4 if last ENDIF for IF statements containing
C*                       current line has been found during loadpp.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine ifelse(iflg, iline, ilodpp, inot)

      include 'com.com'
      include 'ifcom.com'

      integer*2 iflg, ilodpp, inot
      integer*4 iline

      real*8 svtv
      integer*4 nlif, kerr, endflg, lix, psav(2), jline
      integer*2 itypsv, istsv, savifl
      logical*2 saverr
      integer*2 IFV/825/,THENV/938/,ELSEIFV/939/,ELSEV/940/,ENDIFV/941/

      itypsv = ityp
      istsv  = ist
      ifl(370) = iflg
      if (ifl(38) .ne. 0) call mclspt (psav)

      if (ist.eq.IFV) then
        call svpars
        savifl = ifl(2)
        saverr = err
        do while (.not. nxteos)
          idtype = -1
          call parsit
          savifl = ifl(2)
          saverr = err
          ifl(2) = 0
          err = .false.
        enddo
        call rtpars
        ifl(2) = savifl
        err = saverr
        if (.not. vocab .or. ist.ne.THENV) then
          ityp = itypsv
          ist  = istsv
          inot = 1
          ifl(2) = 0
          err = .false.
          goto 8000
        endif
      endif

      inot = 0
c
c...declaring if statement ------
c
      if (iflg.eq.1) then
        if (istsv.eq.IFV) then
          if (ifidec.ge.MAXIF) goto 9505
          ifidec = ifidec+1
          call nclf_src_line_to_rec (iline,ifdec(ifidec))
          ifstat(ifidec) = 1
          
          call ncl_ifstor(ifdec(ifidec))
          if (ilodpp.eq.2 .or. ilodpp.eq.3) then
            if (ifindx.ge.MAXIF) goto 9505
            ifindx = ifindx+1
            ifa(ifindx) = ifdec(ifidec)
          endif
        else
          endflg = 0
          if (ifidec.lt.1) goto 9506
          nlif = ifdec(ifidec)
          if (istsv.eq.ELSEIFV) then
            if (ifstat(ifidec).gt.2) goto 9507
            ifstat(ifidec) = 2
          elseif (istsv.eq.ELSEV) then
            ifstat(ifidec) = 3
          else
            if (ifidec.gt.0) then
              ifstat(ifidec) = 0
              ifidec = ifidec - 1
            endif
            endflg = 1
            if (ilodpp.eq.2 .or. ilodpp.eq.3) then
                if (ifindx.gt.0) ifindx = ifindx-1
                if (ilodpp.eq.3 .and. ifindx.eq.0) ilodpp = 4
            endif
          endif
          call nclf_src_line_to_rec (iline,jline)
          call ncl_ifpush(nlif,jline,endflg,kerr)
        endif
      else
c
c...Executing if statement ------
c
        lix = ifindx
        if (istsv.eq.ENDIFV) then
          if (.not. nxteos) goto 9004
          if (ifindx.gt.0) then
            call ncl_ifgtend(ifa(ifindx),nlif,kerr)
            if (kerr.eq.1.or.nlif.le.0) goto 9504
            call nclf_src_rec_to_line (nlif,iline)
            iline = iline + 1
            lskip = .true.
            ifl4(10) = 0
            ifindx = ifindx-1
          endif
        else
          if (istsv.eq.ELSEV .and. .not. nxteos) goto 9004
          if (istsv.eq.IFV) then
            lix = lix+1
            if (lix.ge.MAXIF) goto 9505
            call nclf_src_line_to_rec (iline,ifa(lix))
            jmpflg(lix) = .false.
          elseif (jmpflg(lix)) then
            call ncl_ifgtend(ifa(lix),nlif,kerr)
            if (kerr.eq.1.or.nlif.le.0) goto 9504
            call nclf_src_rec_to_line (nlif,iline)
            lskip = .true.
            jmpflg(lix) = .false.
            go to 8000
          endif
          if (istsv.eq.IFV .or. istsv.eq.ELSEIFV) then
            call parsit
            if (ifl(210).ne.1) then
                if (ityp .eq. 8) then
                    call vstchk
                else
                    nextyp = 7
                endif
                if (ityp .eq. 3 .or. ityp .eq. 4) then
                    if (tv .ne. 0.) tv = 1.
                else if (ityp .eq. 2) then
                    ifl(2) = 0
                    err = .false.
                    if (ist .eq. 1) then
                        tv = 0.
                    else if ((ist .ge. point .and. ist .le. matrix) .or.
     1                  ist .eq. patern .or. ist .eq. shape .or.
     2                  ist .eq. pntvec) then
                        tv = 1.
                    else
                        goto 9404
                    endif
                    if (ain(inx) .eq. ')') call parser
                else
                    goto 9404
                endif
            endif
            if (ifl(2).ne.0) goto 9999
            svtv = tv
            call parsit
            if (.not. vocab .or. ist.ne.THENV) goto 9508
            if (.not. nxteos) goto 9004
            ifindx = lix
            if (svtv.eq.1.d0) then
              jmpflg(ifindx) = .true.
              goto 8000
            endif
            call nclf_src_line_to_rec (iline,jline)
            call ncl_ifgtln(ifa(ifindx),jline,nlif,kerr)
            if (kerr.eq.1.or.nlif.le.0) goto 9506
            call nclf_src_rec_to_line (nlif,iline)
            lskip = .true.
          endif
        endif
      endif

8000  continue
      ifl(370) = 0
      if (ifl(38) .ne. 0) call mclrpt (psav)
      return
c
c...Error - End of statement expected.
c
9004  call error (4)
      goto 8000
C
C... Error - Invalid logical expression
C
9404  call error (404)
      goto 8000
c
c...Error - Missing endif
c
9504  call nclf_src_rec_to_line (ifa(lix),jline)
      write(errcom,1010) jline
1010  format(I6)
      call error (504)
      goto 8000
c
c...Error - if-then nested too deeply
c
9505  call error (505)
      goto 8000
c
c...Error - if-then statement not in effect
c
9506  call error (506)
      goto 8000
c
c...Error - elseif must follow if
c
9507  call error (507)
      goto 8000
C
C... Error - THEN expected
C
9508  call error (508)
      goto 8000
C
C... Error from parsit
C
9999  call error(ifl(2))
      goto 8000
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ifinit
C*     Initialize if-then-else statements.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine ifinit

      include 'com.com'
      include 'ifcom.com'

      ifindx = 0
      ifidec = 0

      call ncl_ifinit

99999 continue
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ifjump
C*     Return an error for a jumpto into an if-then-else range. Reset
C*     if-then-else stack for jump out of if-then-else range.
C*    PARAMETERS
C*       INPUT  :
C*          lnum    - Line number of jumpto destination.  Requires the
C*                    actual line number, not the index into the source
C*                    file.
C*       OUTPUT :
C*          kerr    - 1 if jumpto is into if-then-else, else 0
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine ifjump(lnum, kerr)

      include 'com.com'
      include 'ifcom.com'

      integer*4 lnum
      integer*2 kerr

      call ncl_iflabchk(nline,lnum,ifindx,kerr)

99999 continue
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ifrset
C*     reset all if-then blocks in a line number range.
C*    PARAMETERS
C*       INPUT  :
C*          lstart    - Start of macro or looping region.
C*          lend      - End of macro or looping region.
C*       OUTPUT :
C*          none.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine ifrset(lstart, lend)

      include 'com.com'
      include 'ifcom.com'

      integer*4 lstart, lend

      if (ifindx.gt.0) call ncl_ifreset(lstart, lend, ifindx)

99999 continue
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine iffix(numlns,linen)
C*     Fix if tables when lines are inserted or deleted from the part
C*     program file.
C*    PARAMETERS
C*       INPUT  :
C*          numlns    - Number of lines inserted.
C*          lend      - Line number where insertion or deletion occurred.
C*       OUTPUT :
C*          none.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine iffix(numlns,linen)

      include 'com.com'
      include 'ifcom.com'

      integer*4 numlns, linen

      call ncl_fixif(numlns,linen)
      do j=1,ifindx
        if (ifa(j).ge.linen) ifa(j) = ifa(j) + numlns
      enddo

99999 continue
      return
      end
