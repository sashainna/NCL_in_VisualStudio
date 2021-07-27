c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       exprs2.f , 25.1
c**     DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:02
c**
c*****************************************************
c**
c** copyright (c) 1986 mills data systems corporation
c**
c*************************************************************************
c                              exprs2
c                              ******
c      another version of subroutine 'expres' to allow 'recursion'
c      when evaluating function arguments. expression is ended by
c      a comma or an extra closing paren.
c
c*************************************************************************
 
      subroutine exprs2
 
      include 'com8a.com'
 
      common /exprcs/ iex
      integer*2 iex

      real*8 stack(20,20), values(21,20),rtv
      integer*2 istack(80,20),ilvl(20),istix(20)
      equivalence(stack,istack)
      character*64 tempid
      character*64 cvals(21,20)
      logical sub(20)
      integer*2 ktv(4),irest(4)
      equivalence (tv,ktv),(rest,irest)

      integer*2 MAX1F, MIN1F, SIGNF, LNTHF, ANGLF
      parameter (MAX1F=570, MIN1F=571, SIGNF=572, LNTHF=560, ANGLF=559)
      integer*2 THRU, ALL, DISTF, TYPEF, SRFTYP
      parameter (THRU=152, ALL=816, DISTF=575, TYPEF=98, SRFTYP=875)
      integer*2 VOCABF, FINDEXV, RINDEXV, STRCMPV
      parameter (VOCABF=549,FINDEXV=573,RINDEXV=574,STRCMPV=581)
      integer*2 XTYPE, TDISTF
      parameter (XTYPE=99, TDISTF=585)
 
      iex=iex+1
      if (iex.gt.20) then
        ifl(2)=59
        goto 99999
      endif
      ilvl(iex)=0
      isvifl=ifl(44)
      ifl(44)=0
      sub(iex)=.false.
      istix(iex)=0
100   if (ityp.eq.5.and.ist.eq.6) then
        ilvl(iex)=ilvl(iex)+1
        call parser
        go to 100
      endif
      if (ityp.eq.5.and.(ist.eq.2.or.ist.eq.3)) then
        if (ist.eq.3) then
          values(istix(iex)+1,iex)=-1
          istack(istix(iex)*4+1,iex)=ilvl(iex)
          istack(istix(iex)*4+2,iex)=4
          istix(iex)=istix(iex)+1
        endif
        call parser
        go to 100
      endif
      if (ityp.eq.1) then
        if (ist.eq.561) then
          call dotf
          if (ifl(2).ne.0) goto 99999
          goto 140
        endif
        if (ist.eq.563) then
          call numf
          if (ifl(2).ne.0) goto 99999
          goto 140
        endif
        if (ist.eq.565) then
          call atang2f
          if (ifl(2).ne.0) goto 99999
          goto 140
        endif
        if (ist.eq.566) then
          call canf
          if (ifl(2).ne.0) goto 99999
          goto 140
        endif
        if (ist.eq.MAX1F .or. ist.eq.MIN1F) then
          call nmxmnf
          if (ifl(2).ne.0) goto 99999
          goto 140
        endif
        if (ist.eq.SIGNF) then
          call nsignf
          if (ifl(2).ne.0) goto 99999
          goto 140
        endif
        if (voc .eq. LNTHF) then
          call nlnthf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif
        if (voc .eq. ANGLF) then
          call nanglf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif
        if (voc .eq. DISTF) then
          call ndistf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif
        if (voc .eq. TDISTF) then
          call tndistf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif
        if (voc .eq. TYPEF) then
          call ntypef (rtv)
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif
        if (voc .eq. XTYPE) then
          call xtypef
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif
        if (voc .eq. VOCABF) then
          call nvcabf
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. FINDEXV) then
          call findexf(i2v0)
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. RINDEXV) then
          call findexf(i2v1)
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (voc .eq. STRCMPV) then
          call strcmpf
          if (ifl(2) .ne. 0) go to 99999
          ifl(210) = 1
          go to 140
        endif
c
        if (voc .eq. SRFTYP) then
          call nsrftyp
          if (ifl(2) .ne. 0) go to 99999
          go to 140
        endif

        if (ist.ge.550.and.ist.lt.570) then
          values(istix(iex)+1,iex)=0
          istack(istix(iex)*4+1,iex)=ilvl(iex)
          istack(istix(iex)*4+2,iex)=ist
          istix(iex)=istix(iex)+1
          isvix=istix(iex)
          call parser
          go to 100
        endif
      endif

      if (ityp.eq.2.and.nextyp.eq.6) then
c                                                        *** subscript
        if (ist.eq.1) then
          if (ifl(370).ne.1) then
            savid2=token2
            isvsub=0
            ifl(9)=ifl(11)
            ifl(10)=ifl(12)
            idst=14
            rest=0.
            irest(3)=32767
            irest(4)=14
            call vstore
          endif
        else if (ist .eq. 14) then
          tempid = token2
        else
          ifl(2)=89
          goto 99999
        endif
        cvals(istix(iex)+1,iex)=token2
        istack(istix(iex)*4+1,iex)=ilvl(iex)
        istack(istix(iex)*4+2,iex)=12
        istix(iex)=istix(iex)+1
        call parser
        go to 100
      endif
      if (.not.((ityp.eq.2.and.ist.eq.2).or.
     1     (ityp.eq.3.or.ityp.eq.4))) go to 170
140     values(istix(iex)+1,iex)=tv
150     if (.not. (nextyp .lt. 2 .or.
     x            (nextyp.gt.7.and.nextyp.ne.10.and.nextyp .ne.14)
     x        .or.(nextyp.eq.7.and.ilvl(iex).eq.0))) go to 1099
c150     if (.not. (nextyp .lt. 2 .or.
c     x            (nextyp .gt. 7 .and.
c     x             nextyp .ne. 10 .and.
c     x             nextyp .ne. 14))) go to 1099
160     istack(istix(iex)*4+1,iex)=ilvl(iex)
        istack(istix(iex)*4+2,iex)=11
        if (isvifl.eq.nextyp) inx=inx+1
        go to 200
1099    continue
        call parser
        if (ityp.eq.5.and.ist.eq.7) then
c               if (ilvl.lt.1.and.ifl(111).ne.0) then
c                   nextyp=7
c                   inx=inx-1
c                   go to 160
c               endif
            ilvl(iex)=ilvl(iex)-1
            go to 150
        endif
        istack(istix(iex)*4+1,iex)=ilvl(iex)
        if (ityp.eq.5.or.ityp.eq.3.or.ityp.eq.4) then
            istack(istix(iex)*4+2,iex)=ist
            if (ityp.eq.3.or.ityp.eq.4) then
                tv=dabs(tv)
                go to 190
            endif
        endif
        call parser
        go to 180
170   continue
        if (ityp.eq.2.and.ist.eq.1) then
            ifl(2)=9
        else
            ifl(2)=61
        endif
        go to 99999
180   continue
190   istix(iex)=istix(iex)+1
      go to 100
200   if (ilvl(iex).ne.0.and.ifl(111).eq.0) then
        ifl(2)=60
        go to 99999
      endif
      values(21,iex)=values(1,iex)
      do 400,i=1,20
           call getnxt(istack(1,iex),values(1,iex),istix(iex))
           if (istix(iex).eq.0) then
               if (sub(iex)) then
                   ityp=2
               else
                   tv=values(21,iex)
                   itv=tv
                   values(21,iex)=itv
                   if (values(21,iex).eq.tv) then
                       ityp=3
                   else
                       ityp=4
                   endif
               endif
               go to 410
           endif
           call eval(istack(1,iex),values(1,iex),cvals(1,iex),
     x                istix(iex),sub(iex))
           if (ifl(2).ne.0) go to 410
400   continue
410   continue
99999 continue
      if (ifl(2).ne.0) then
           if (ifl(111).ne.0.or.ifl(38).eq.1.or.ifl(45).eq.1) then
               ifl(2)=0
               errcom=' '
               ityp=6
           else
               err=.true.
           endif
      endif
      iex=iex-1
      return
      end
