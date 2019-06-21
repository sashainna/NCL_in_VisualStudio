C*********************************************************************
C*    NAME         :  dotext
C*       CONTAINS:
C*    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       dotext.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:09:57
C*********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     :
C*       this routine handles declaration of text variables.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine dotext

      include 'com8a.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      real*8 r8(10),rtv
      character*512 str
      character*256 str2, fmtstr
      character*2560 txta
      integer*4 nclkey
      integer*2 j, ix, ixf, ixt, iflg(10), ierr,ilin
      integer*2 FORMATV/849/,SUBSTRV/580/
      integer*2 LPARENV/6/,RPARENV/7/,COMMAV/9/,AMPV/18/,LBRAKV/15/
      integer*2 RBRAKV/16/
c
      ldtext = .true.
      ix = 0
100   continue
      if (lstrng) then
        call gttext(str, ix)
        if (ifl(2).gt.0) goto 99999
c
c...txt=anote
c
      else if (ityp .eq. 2 .and. ist .eq. VANOTE) then
        rtv  = tv
        ilin = 0
        if (nextyp .eq. LBRAKV) then
          call parsit
          call parsit
          if (.not.SCALAR) goto 9007
          ilin = itv
          if (nextyp .ne. RBRAKV) goto 9518
          call parsit
        endif
        call gtanot (rtv,str,ix,ilin)
        if (ifl(2).gt.0) goto 99999
c
c...FORMAT
c
      else if (ityp.eq.1.and.ist.eq.FORMATV) then
        if (nextyp.ne.LPARENV) goto 9309
        call parser
        call parsit
        if (.not.lstrng) goto 9513
        j = 0
        call gttext(fmtstr, j)
        j = j+1
        fmtstr(j:j) = char(0)
        if (nextyp.ne.COMMAV) goto 9311
        call parsit
        ixf = 1
        ixt = 0
        do while (nextyp.ne.RPARENV)
          lexpr = .true.
          call parsit
          lexpr = .false.
          if (SCALAR) then
             r8(ixf) = tv
             iflg(ixf) = -1
          else if (lstrng) then
             iflg(ixf) = ixt
             call gttext(txta, ixt)
             if (ifl(2).gt.0) goto 99999
             ixt = ixt+1
             txta(ixt:ixt) = char(0)
          else
             goto 9007
          endif
          if (nextyp.eq.COMMAV) call parsit
          if (nextyp.ne.RPARENV) then
            ixf = ixf+1
            if (ixf.gt.10) goto 9061
          endif
        enddo
c        if (nextyp.ne.RPARENV) goto 9310
        call ncl_fmtstr(fmtstr,ixf,iflg,r8,txta,str(ix+1:),k,ierr)
        if (ierr.gt.0) goto 9516
        ix  = ix+k
        call parsit
      else if (ityp.eq.1.and.ist.eq.SUBSTRV) then
        if (nextyp.ne.LPARENV) goto 9309
        call parser
        call parsit
        if (.not.lstrng) goto 9513
        j = 0
        call gttext(str2, j)
        if (nextyp.ne.COMMAV) goto 9311
        call parsit
        call parsit
        if (.not.SCALAR) goto 9007
        i1 = itv
        i2 = 0
        if (nextyp.eq.COMMAV) then
          call parsit
          lexpr = .true.
          call parsit
          lexpr = .false.
          if (.not.SCALAR) goto 9007
          i2 = itv
        endif
        if (nextyp.ne.RPARENV) goto 9310
        if (i1.gt.0 .and. i1.le.j .and. i2 .ge. i1) then
          if (i2.gt.j) i2 = j
          str(ix+1:) = str2(i1:i2)
          ix = ix+i2-i1+1
        endif
        call parsit
      else 
        goto 9061
      endif
      if (ix.gt.255) goto 9517
      if (nxteos) goto 200
      if (nextyp .ne. AMPV) goto 9514
      call parsit
      call parsit
      if (ifl(2).gt.0) goto 99999
      goto 100

200   continue
      call ncl_pttext(str,ix,keyold,nclkey)
      if (nclkey.eq.0) goto 9515
      idst = TEXTVAR
      call ptdsc3(nclkey,1,idst,rest)
      goto 99999

C                            Number or scalar expected
9007  continue
        ifl(2) = 7
        goto 99999
C                            Invalid syntax
9061  continue
        ifl(2) = 61
        goto 99999
C                            Left paren expected
9309  continue
        ifl(2) = 309
        goto 99999
C                            Right paren expected
9310  continue
        ifl(2) = 310
        goto 99999
C                            Comma expected
9311  continue
        ifl(2) = 311
        goto 99999
C                            Text or text var expected
9513  continue
        ifl(2) = 513
        goto 99999
C                            Ampersand expected
9514  continue
        ifl(2) = 514
        goto 99999
C                            Error creating text entity
9515  continue
        ifl(2) = 515
        goto 99999
C                            Invalid format
9516  continue
        ifl(2) = 516
        goto 99999
C                            Text too long
9517  continue
        ifl(2) = 517
        goto 99999
C                            Right bracket expected
9518  continue
        ifl(2) = 462
        goto 99999

99999 continue
c
      ldtext = .false.
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : gttext(str, ix)
C*       this routine parses text or a  text variable.
C*    PARAMETERS
C*       INPUT  :
C*          ix       - last position in str currently used
C*       OUTPUT :
C*          str      - parsed text or data from text variable
C*          ix       - last position used in str
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine gttext(str,ix)

      include 'com8a.com'

      character*256 str
      integer*2 ix
c
      integer*4 nclkey
c
      character*255 str2
      integer*2 j,k,len, nwds, ietype,ilin
      integer*2 STRINGV/9/
      integer*2 COLONV/13/, LBRCKTV/15/, RBRCKTV/16/

      real*8 rtv

      if (ityp.eq.STRINGV) then
        len = length
        str2 = tokstr
      else if (ityp.eq.2.and.ist.eq.TEXTVAR) then
        call gtdesc(tv,nclkey,nwds,ietype)
        len = 255
        call ncl_gttext(nclkey,str2,len)
      else if (ityp .eq. 2 .and. ist .eq. VANOTE) then
        rtv  = tv
        ilin = 0
        if (nextyp .eq. LBRCKTV) then
          call parsit
          call parsit
          if (.not.SCALAR .or. itv .lt. 0) goto 9224
          ilin = itv
          if (nextyp .ne. RBRCKTV) goto 9462
          call parsit
        endif
        call gtanot (rtv,str2,len,ilin)
        if (ifl(2).gt.0) goto 99999
      endif
      j = 1
      if (nextyp.eq.LBRCKTV) then
        call parsit
        call parsit
        if (.not.scalar.or.itv.lt.1) goto 9224
        j = itv
        if (nextyp.ne.COLONV) goto 9512
        call parsit
        call parsit
        k = len
        if (scalar) then
          if (itv.lt.1) goto 9224
          if (itv.le.len) k = itv
          call parsit
        endif
        if (ityp.ne.5 .and. ist.ne.RBRCKTV)  goto 9462
        len = 0
        if (k.ge.j) then
          len = k-j+1
          if (len.gt.255) len = 255
        endif
      endif
      if (len.gt.0) then
        k = ix+len
        str(ix+1:k) = str2(j:)
        ix = k
      endif
      goto 99999
C                            Positive integer expected
9224  continue
        ifl(2) = 224
        goto 99999
C                            Right bracket expected
9462  continue
        ifl(2) = 462
        goto 99999
C                            Colon expected
9512  continue
        ifl(2) = 512
        goto 99999
c
99999 continue
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : shtext
C*       this routine does a *SHOW of text or a text variable.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine shtext

      include 'com8a.com'

      character*256 str
      integer*2 j, k, nc
      integer*2 STRINGV/9/
      integer*4 strlen1

c      if (ityp.eq.STRINGV) then
c        cout = '              Text String'
c      else
c
c.....Print label along with type
c
      cout = ' '
      call expnm2 (token2, ivxsub, j, cout)
      nc     = strlen1(cout) + 6
      if (ityp.eq.STRINGV) then
        cout(nc:) = 'Text String'
      else
        cout(nc:) = 'Text Variable'
      endif
      j = 0
      call gttext(str,j)
      if (ifl(2).gt.0) goto 99999
      call putmsg (cout,80,17,0)
      if (j.le.0) then
        cout = '""'
        call putmsg (cout,2,17,0)
      else
        cout(1:1) = '"'
        i = 1
        do while (j.gt.0)
          k = j
          if (k.gt.70) k=70
          cout(2:k+1) = str(i:)
          cout(k+2:) ='"'
          call putmsg (cout,k+2,17,0)
          i = i+k
          j = j-k
        enddo
      endif
c
99999 continue
      return
      end
