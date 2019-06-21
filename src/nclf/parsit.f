C*********************************************************************
C*    NAME         :  parsit.f
C*       CONTAINS:
C*        parsit  sthru   gtsmax  skpsep  svpars  rtpars  substx
C*        txt2cin parsit2 preprs
C*
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       parsit.f , 25.4
C*    DATE AND TIME OF LAST  MODIFICATION
C*       08/01/17 , 13:52:30
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine parsit
c*       Call parser and handles expressions.
c*       it handled the 'thru' claude here too
c*              ifl(105)  -  flag, 1 if the names are subscripted,
c*                                 2 if they are auto generated.
c*              ifl4(106)  -  contains the lower range
c*              ifl4(107)  -  contains the upper range
c*              ifl4(108)  -  contains the increment (either +1 or -1)
c*              sc116   -  contains the token representation of the
c*                           identifier if subscripted
c*      idtype, default to 0: represents the sub type of the 'thru'
c*                   -1 = if there are thru claud, just return 'thru' word value
c*                    0 = any type
c*                   > 0 = geometry type
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine parsit

      include 'com8a.com'

      common/dstcom/keydt, ixdt, nxthld, nxtret
      integer*4 keydt,sub, ipg,iel
      integer*2 ixdt, nxthld, nxtret
      character*64 label,tmpstr

      integer*2 i, k, isv2
      character*256 str
      real*8 hldtv
      character*64 svtkn
      character*2 idum
      equivalence (sc116,svtkn)
      logical lgeo
      integer*2 iss,nxs,iid, iflsav
      character*64 t2s
      integer*4 isitv,nclkey,sgnum
      integer*2 IFV, ELSEIFV, STRINGV, FORMATV, SUBSTRV
      parameter (IFV     = 825)
      parameter (ELSEIFV = 939)
      parameter (STRINGV = 9)
      parameter (FORMATV = 849)
      parameter (SUBSTRV = 580)

      wildwd = .false.
      filtgeo = .false.
      isitv = -1
      sgnum = geomnum
cc      if (ifl(191) .eq. 0) call preprs
      geomnum = sgnum
c
c...see if there are geomlist to return COLF/LAYF/MARF is in effect
c
c
      if ((geomnum.gt.0).and.(.not. filtwd)) then
         call getgeomlst
         goto 99999
      endif
c
c...see if there are wildcard is in effect
c
      if ((wildnum.gt.0).and.(.not. filtwd)) then
         call getwildlst
         goto 99999
      endif
c *************************************************************************
c          no 'thru' clause is in effect
c *************************************************************************
      if (.not.(ifl4(107).eq.0) .and. (idtype.ge.0)) go to 1299
      iflsav=ifl(325)
cccccccccccccccccccccc
cccccccccccccccccccccc
      ltxtsb = .false.
c
c....data statement
c
   10 if (ifl(324).gt.0) then
c
c...   return token from current data statement
c
          if (nxtret.gt.0) then
              ityp = 5
              if (nxtret.eq.11) ityp = 7
              ist = nxtret
              nxtret = 0
              goto 99999
          endif
          ixdt = ixdt+1
   30     call dtgetv (keydt, ixdt, tv ,ityp, nextyp,label, sub)
          if (ityp.eq.1) then
             ist = tv
             token2 = label
             ivxsub = sub
          else if (ityp.eq.2) then
             token2 = label
             ivxsub = sub
             call vstchk
             if ((ityp.eq.2).and.(ist.eq.DATAST)) then                  
                 call vxchk (token2, ivxsub,keydt,ipg,iel,nwds, ist)
                 goto 30
             endif          
          else
             ityp = 2
             ist = 2
             token2 = label
             ivxsub = sub
          endif
          if (ixdt.ge.ifl(324)) then
             ifl(324) = 0
             nextyp = nxthld
          endif
          if (nextyp.eq.11) then
             ncsfl(3) = 1
             nxtret = 11
          else if (nextyp.ne.ifl(44)) then
             nxtret = nextyp
          endif
          goto 99999
      endif
c
c...   If we are in an inclusive thru, return the next entity.
c
      if (ifl(325).lt.0) then
        call sthru (ifl(325))
        if (ifl(2).gt.0) goto 99999
        goto 50
      endif

      call parser

c
c... added itype .ne.5 for when it is a carret
c
      if (ist.eq.17.and.ityp.ne.1 .and.ityp.ne.5) then
          ivxsub=0
          call vstchk
      endif

      if (ityp.eq.5.and.ist.eq.1) goto 50
      if (filtwd) goto 99999
c
c...check if it is COLF and handle it
c
      if (ityp.eq.1 .and.ist.eq.942) then
          if (filtype.lt.0) goto 99999
          call geomf(1)
          if (geomnum.gt.0) then
             call getgeomlst
          endif
          goto 99999
      endif
c
c...check if it is LAYF and handle it
c
      if (ityp.eq.1 .and.ist.eq.943) then
          if (filtype.lt.0) goto 99999
          call geomf(2)
          if (geomnum.gt.0) then
             call getgeomlst
          endif
          goto 99999
      endif
c
c...check if it is MARKF and handle it
c
      if (ityp.eq.1 .and.ist.eq.960) then
          if (filtype.lt.0) goto 99999
          call geomf(5)
          if (geomnum.gt.0) then
             call getgeomlst
          endif
          goto 99999
      endif
c
c...check if it is CLIPF and handle it
c
      if (ityp.eq.1 .and.ist.eq.949) then
          if (filtype.lt.0) goto 99999
          call geomf(3)
          if (geomnum.gt.0) then
             call getgeomlst
          endif
          goto 99999
      endif
c
c...check if it is FILTER function and handle it
c
      if (ityp.eq.1 .and.ist.eq.950) then
          if (filtype.lt.0) goto 99999
          call geomf(4)
          if (geomnum.gt.0) then
             call getgeomlst
          endif
          goto 99999
      endif
c
c...check if it is wildcard string and handle it
c
      if (ityp.eq.10) then
          if (wdtype.lt.0) goto 99999
          if (EDTEXE) goto 99999
          if (filtype.lt.0) goto 99999
          call pwild
          if (wildnum.gt.0) then
             call getwildlst
          endif
          goto 99999
      endif
c
c...The following check for an operator to see if
c...it needs to be evaluated.
c   
      if ((nextyp.eq.2.or.
     1     nextyp.eq.3.or.
     2     nextyp.eq.4.or.
     3     (nextyp.eq.5.and.ityp.ne.1.and..not.ldtflg).or.
     4     (nextyp.eq.6.and..not.
     -     (ityp.eq.1.and.(ist.eq.IFV.or.ist.eq.ELSEIFV))).or.
     5     (ityp.eq.5.and.ist.eq.6.and.nextyp.eq.8).or.
     6     nextyp.eq.10.or.nextyp.eq.14.or.nextyp.eq.17
     -     .or.(ityp.eq.5.and.ist.eq.14)
     7     .or.nextyp.eq.12.and.ityp.eq.5.and.(ist.eq.6.or.ist.eq.2
     8     .or.ist.eq.3))
     9    .and.
     a     (.not.(ityp.eq.1 .and. (ist.gt.1042 .and. ist.lt.1047
     b       .or.ist.eq.1091.or.ist.eq.1092.or.ist.eq.FORMATV
     c       .or.ist.eq.SUBSTRV)))) then

c
c...test on cont lines 'a'&'b' is for partno, pprint, letter, insert
c...remark or titles.
c...analyze the expression
c
          i=length
          if (ifl(354) .eq. 1) then
              iss = ist
              t2s = token2
              nxs = nextyp
              isv2 = ivxsub
          endif
          ifl(379) = 0
          call expres (ifl(325))
          if (ifl(379).eq.0)length=i
          ifl(379) = 0
          if (ifl(2).ne.0) then
            ityp=8
            if (ifl(354) .eq. 1) then
                ist = iss
                token2 = t2s
                ivxsub = isv2
                nextyp = nxs
            endif
            goto 99999
          endif
c
c...   Set up inclusive thru
c
          sav325 = ifl(325)
c
c...when decmac or define loop, the code is not execute, so
c...ignore the result return, otherwise, it will cause error
c...when parsing because some value/geo may not define yet
c
c...          if (ifl(325).gt.0) call sthru(ifl(325))
          if ((ifl(325).gt.0).and.(ifl(38).ne.1.and.ifl(45).ne.1)) then
               call sthru(ifl(325))
          endif
          if (ityp.eq.2 .and. ist.eq.1 .and. sav325.eq.3) then
              goto 99999 
          endif
      endif
c
c...   If its a data statement & we are not defining it, return first data
c...   statement element or element specified in following [].
c
   50 if (.not.ldtflg.or.ifl(44).eq.5.and.nextyp.ne.1) then
         if (ityp.eq.2 .and. ist.eq.DATAST) then
            if (ifl(38).eq.3) goto 99999
            call gtdesc (tv, keydt, ifl(324), ist)
            if (nextyp.eq.15) then
               if (ifl(44).ne.15) call parser
               call parser
               call expres(ifl(325))
               if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3.and.ityp.ne.4)
     x            goto 9224
               if (itv.lt.1.or.itv.gt.ifl(324)) goto 9085
               if (nextyp.ne.16) goto 9462
               ixdt = itv-1
               ifl(324) = itv
               if (ifl(44).ne.16) call parser
           else
               ixdt = 0
           endif
           nxthld = nextyp
           nxtret = 0
           goto 10
         endif
      endif

      if (.not.ldtext.and.(ityp.eq.9.or.ityp.eq.2.and.ist.eq.TEXTVAR))
     x     then
         if (ityp.eq.9) then
            k = length
            str = token2(1:k)
         else
            call gtdesc(tv,nclkey,nwds,ietype)
            k = 255
            call ncl_gttext(nclkey,str,k)
         endif
         if (k.gt.0 .and. k.le.64) then
            isv1 = ityp
            t2s = token2
            isv2 = ivxsub
            hldtv = tv
            call convrt(str,token2,k)
            token2(k+1:) = ' '
            call vocsch
            if (ityp.eq.2) then
               ivxsub=0
               call vstchk
            endif
            if (ityp.eq.2.and.ist.eq.1) then
               token2 = t2s
               ivxsub = isv2
               tv = hldtv
               ityp = isv1
               ist = TEXTVAR
            else
               ltxtsb = .true.
            endif
         endif
      endif
cccccccccccccccccc
cccccccccccccccccc
      if (idtype.lt.0) goto 99999 
      if (ifl(370).eq.1) go to 99999
c
c...the convid change the itv we get from parser, so save it
c...and reset
c
      isitv = itv
c
c...handle 'thru' claud
c
      if (ist.eq.1.and.nextyp.ne.11.and.
     1             (ifl(325).ne.0 .or. ifl(354) .eq. 1)) then
         call nmtoid (iid, token2)
         if (iid.gt.1 .and. ifl(354) .eq. 1) then
            ist = iid
            goto 15
         else if (ifl(354) .eq. 1) then
            goto 15
         endif
         if (ifl(325).eq.0) then
             errcom=' '
             call error (244)
             goto 99999
         else if (iflsav.ne.-1) then
             errcom=' '
             call error (244)
             goto 99999
         else
             goto 10
         endif
      endif
c
c...If the last entity of an inclusive thru
c...is not defined, then ignore it
c
   15 if (ityp .eq. 2 .and. ist .eq. 1 .and. iflsav .ne. 0
     1             .and. ifl(325) .eq. 0) then
         ityp = 7
         nextyp = 11
         go to 399
      endif
      if (.not.(ityp.eq.3.and.itv.gt.0)) go to 99
C
C...it's an integer denoting a generated id
C
      if (idtype.gt.0) then
         ifl4(106)=itv
         ist=idtype
         call buildn(2)
         if (err) go to 99999
         s116 = idtype
         ifl(105)=2
         ityp=2
      else
         go to 99999
      endif
      go to 1199
99    if (.not.(ityp.eq.2)) go to 499
C
C...it's an item of the right type
C
199   if (.not.(ivxsub.ne.0)) go to 299
C
C...it's a subscripted item of the right type
C
      ifl4(106) = ivxsub
      svtkn=token2
      ifl(105)=1
      go to 399
  299 continue
      ibr=1
c...here return any type of geom from thru
c...  100 if (idtype.eq.0) then
  100     call nmtoid (iid,token2)
          if (iid .eq. ist) then
              ist = iid
          else if (ist .ne. 2) then 
              if (ibr.eq.1) then
c
c...added support for non-standard label names
c
                 ifl(105)=0
                 call convlab(token2, thrlbl, sub)
                 if (thrlbl.ne.' ') then
                      ifl4(106) = sub
                      ifl(105)=3
                      s116 = ist
                 endif
              else if (thrlbl.ne.' ') then
                 call convlab(token2, tmpstr, sub)
                 if (thrlbl.ne.tmpstr) then
                     errcom=' '
                     call error(41)
                 endif
                 s116 = ist
                 itv = sub
                 go to 401
              else
                 errcom=' '
                 call error(41)
              endif
              goto 99999
          endif
          svtkn=token2
          s116 = ist
c...          s116 = idtype
c...temp change
c...      else
c...          s116=idtype
c...      endif
      go to (200,400) ibr
200   continue
      call convid
      if (err) go to 99999
      if (itv.ne.0) then
C
C...it's a generated id of the right type
C
         ifl4(106)=itv
         s116 = ist
         ifl(105)=2
      else
         ifl(105)=0
      endif
399   continue
      go to 1199
C
C...if it is a generic geometry type or 'all' call parsgn
C...to generate the next valid geometry name
C
499   call vctoid (ist,iid,idum)
      call ifidge (iid,lgeo)
      if (.not.(ityp .eq. 1 .and.
     -       (lgeo .or. ist .eq. 816))) go to 599
      call parsgn
      go to 1199
C
C...it's a 'thru'
C
599   if ((.not.(ityp.eq.1.and.ist.eq.152)) .or.
     x     (idtype.lt.0 .and.ityp.eq.1.and.ist.eq.152)) 
     x   go to 1199
      if (ifl(370).eq.1) go to 1199
      isave=isvinx
c...temp change      call parser
      call parsit2
      if (ist.eq.1) then
         call nmtoid (iid, token2)
         if (iid.gt.1) then
             ist=iid
         endif
      endif    
C 
C...check to make sure that the item before the thru
C...was of a valid type.
C 
      if (idtype .ne. 0 .and.
     x      s116.ne.idtype.and.ifl(105).ne.1) then
         isvinx=isave
         call error(114)
         go to 99999
      endif
      if (ifl(105).eq.0) then
         isvinx=isave
         errcom = ' '
         call error(41)
         go to 99999
      endif
C 
C...save nextyp in ifl(122)
C 
      ifl(122)=nextyp
      if (.not.(ityp.eq.3)) go to 699
    
C
C...it's an integer denoting the ending item
C
      if (idtype.ne.0) then
         if (ifl(105).ne.2) then
            call error(114)
            go to 99999
         endif
         ifl4(107)=itv
         ityp=2
         ist=idtype
      else
         go to 99999
      endif
      go to 1099
699   if (.not.(ityp.eq.2)) go to 999
C
C...it's a subscripted item
C
      if (.not.(ivxsub.ne.0)) go to 799
      if (svtkn.ne.token2) then
         call error(114)
         go to 99999
      endif
      ifl4(107)=ivxsub
      go to 899
799   continue
      ibr=2
      go to 100
400   call convid
      if (err) go to 99999
401   if (itv.ne.0) then
         ifl4(107)=itv
         if (s116.ne.idtype .and. 
     x        idtype.ne.0) then
            call error(114)
            go to 99999
         endif
      else
         call error(41)
         go to 99999
      endif
899   continue
      go to 1099
999   continue
      if (idtype.ne.0) call error(41)
      go to 99999
1099  continue
C
C...check to see if next token is incr
C...epm  9-29-84
C
      ifl4(108)=1
      isvitv=itv
      isvist=ist
      isvix=inx
c...temp change      call parser
      call parsit2
C
C...its incr -  set up ifl(108) with value
C
      if (ityp.eq.1.and.ist.eq.66) then
c...temp change          call parser
          call parsit2
          if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.
     1             ist.eq.2)) then
              if (itv.le.0) then
                 call error(112)
                 go to 99999
              endif
              ifl4(108)=itv
           else
              call error(7)
              go to 99999
           endif   
C
C...its not incr- reset inx
C
      else
         inx=isvix
      endif
      if (ifl4(107).lt.ifl4(106)) then
         ifl4(108)=-ifl4(108)
      else if (ifl4(106).ge.ifl4(107)) then
         call error(114)
         go to 99999
      endif
C
C...restore itv and ist for buildn
C
      itv=isvitv
      ist=isvist
C
C...set nextyp to ',' for the thru range
C
      nextyp=9
      go to 500
 1199 continue
      go to 1399
c *************************************************************************
c          a 'thru' clause is in effect - build next valid name and check
c          if it's the last name
c *************************************************************************
 1299 ist=idtype
      if (ist.eq.0) ist=s116
  500 ifl4(106)=ifl4(106)+ifl4(108)
      itv=ifl4(106)
      call buildn(ifl(105))
      if (ist.eq.1) then
         call nmtoid (iid, token2)
         if (iid.gt.1 .and. ifl(354) .eq. 1) then
              ist=iid
         endif
      endif
C
C...Ignore non-existent data during a thru clause.
C...If ist is equal to 1 and ifl4(106) is equal to
C...ifl(107) then the last entry in the subscripted
C...thru clause is not valid and an error will be
C...called.   
C
      if (ist .eq.1 .and. ifl(354) .eq. 0) then
         if (ifl4(106).ne.ifl4(107)) then
c
c.....Switched so the search for a valid label or index continues at the
c.....next value instead of stopping the search - ASF 1/14/14.
c            ist=isvist
c            goto 10
            goto 500
         else
            call error (244)
            goto 99999
         endif
      endif
      if ((ifl4(108).lt.0 .and.
     1    (ifl4(106).le.ifl4(107) .or.
     2    ifl4(106)+ifl4(108) .lt. ifl4(107))) .or.
     3    (ifl4(108).gt.0 .and.
     4    (ifl4(106).ge.ifl4(107) .or.
     5     ifl4(106)+ifl4(108) .gt. ifl4(107)))) then
C
C...the thru is done
C
         ifl4(107)=0
         ifl4(105)=0
C
C...restore nextyp
C
         nextyp=ifl(122)
      endif
      nxteos = .false.
      if (nextyp .eq. 11 .or.
     1    (nextyp .eq. 7 .and. ifl(111) .eq. 1)) nxteos = .true.
      geom = .true.
 1399 continue
c
99999 continue
c
c...determine token type and set logicals
c
      if (isitv.ge.0) itv = isitv
      vocab = ityp .eq. 1
      geom  = .false.
      if (ityp .eq. 2) call ifidge (ist,geom)
      scalar = .false.
      if ((ityp .eq. 2 .and. ist .eq. 2) .or.
     x          ityp .eq. 3 .or. ityp .eq. 4) scalar = .true.
      nxteos = .false.
      if (nextyp.eq.11 .or. (nextyp.eq.7.and.ifl(111).eq.1))
     1    nxteos = .true.
      lstrng = .false.
      if (ityp.eq.STRINGV .or. (ityp.eq.2.and.ist.eq.TEXTVAR) .or.
     1    (ityp.eq.2 .and. ist.eq.VANOTE)) lstrng = .true.
c
c....reset idtype
c
      idtype = 0
      wdtype = 0     
      filtype = 0
      return
c
c...                   Subscript out of range
c
9085  ifl(2) = 085
      goto 9000
c
c...                   Positive Integer Expected
c
9224  ifl(2) = 224
      goto 9000
c
c...                   ']' expected
c
9462  ifl(2) = 462
      goto 9000
9000  err = .true.
c
c....reset idtype
c
      idtype = 0
      wdtype = 0
      filtype = 0
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine sthru (incsub)
c*       Handle inclusive subscripts.
C*    PARAMETERS
C*       INPUT  :
C*          incsub   - < 0 - return next subscript
C*                     = 1 - range starts with scalar
C*                     = 2 - range starts with THRU
C*                     = 3 - range starts with ALL
C*       OUTPUT :
C*          incsub   - 0 when last subscript is returned.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine sthru (incsub)

      include 'com.com'

      integer*2 incsub

      common/subcom/ hldtok, hldsub, iend, incr, icur, nxthld, idelim
      character*64 hldtok
      integer*2 nxthld, idelim
      integer*4 iend, incr, icur, hldsub

      real*8 htv
      integer*2 i2, idir, istsv, itypsv, ifl44
      integer*2 THRU, ALL, INCRV, DECRV
      parameter (THRU  = 152)
      parameter (ALL   = 816)
      parameter (INCRV = 66)
      parameter (DECRV = 65)

c
c.....if the call is from macro or loop declaration
c.....treat as ended because it will take so long time
c.....to pass XYZ(ALL) statement, it will return XYZ(1) to XYZ(1000000)
c.....and all value is not define yet
c
      if (EDTEXE) then
        incsub = 0
        nextyp = nxthld
        return
      endif
      if (incsub.gt.0) goto 100
c
c...   Return the next entity
c
      if (ifl(44).eq.0) then
c
c...   If caller is not skipping delimeters, return comma every second call.
c
        idelim = 1-idelim
        if (idelim.eq.1) then
          ityp = 5
          ist = nextyp
          goto 999
        endif
      endif
      icur = icur+incr
      token2 = hldtok
      ivxsub = icur
      call vstchk
      if (incr.gt.0.and.icur.ge.iend.or.incr.lt.0.and.icur.le.iend) then
        incsub = 0
        nextyp = nxthld
      endif
      goto 999

100   continue
c
c...   parse inclusive thru & set up to return a list of entities
c
      idelim = 0
      i2 = 2
      istsv = ist
      itypsv = ityp
      hldtok = token2
      hldsub = ivxsub
      htv = tv
      ifl44 = ifl(44)
      ifl(44) = 9
      if (ifl44.eq.0.and.nextyp.eq.9) call parser
      icur = 1
      incr = 1
      idir = 1
      if (incsub.eq.3) then
        call gtsmax(hldtok, hldsub, iend)
      else
        if (incsub.eq.1) then
          icur = ivxsub
          call parser
          if (ityp.ne.1 .or. ist.ne.THRU) goto 9061
        endif
        call parser
        if (ityp.eq.1 .and. ist.eq.ALL) then
          call gtsmax(hldtok, hldsub, iend)
        else
          call expres(i2)
          if ((ityp.ne.2.or.ist.ne.2).and.ityp.eq.3.and.ist.eq.4)
     x        goto 9007
          iend = itv
        endif
      endif
      if (nextyp.eq.9) then
        inx = inx-1
        call parser
        if (ityp.ne.1) goto 9335
        if (ist.ne.INCRV .and. ist.ne.DECRV) goto 9335
        if (ist.eq.DECRV) idir = -1
        call parser
        call expres(i2)
        if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3.and.ityp.ne.4)
     x        goto 9007
        incr = itv*idir
      endif
      if (nextyp.ne.7) goto 9310
      call parser
      nxthld = nextyp
      nextyp = 9
      token2 = hldtok
      ivxsub = hldsub
      call vstchk
      tv = htv
      ist = istsv
      ityp = itypsv
      incsub = -1
      ifl(44) = ifl44
      if (incr.gt.0.and.icur.ge.iend.or.incr.lt.0.and.icur.le.iend) then
        incsub = 0
        nextyp = nxthld
      endif
      goto 999

C                      ---  Number or scalar expected
9007  ifl(2) = 7
      goto 999
C                      ---  Invalid syntax format
9061  ifl(2) = 61
      goto 999
C                      ---  Right paren expected
9310  ifl(2) = 310
      goto 999
C                      ---  INCR expected
9335  ifl(2) = 335
      goto 999

999   return
      end
C -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine gtsmax (hldtok, hldsub, ival)

      include 'com.com'

      character*64 hldtok
      integer*4 hldsub
      integer*4 ival

      integer*2 ihld
      integer*2 ktv(4)
      equivalence (tv,ktv)

      token2 = hldtok
      ihld = hldsub
      ivxsub = 0
      call vstchk
      if (ityp.eq.2 .and. ist.eq.14 .and. ifl(354) .eq. 0) then
        ival = ktv(1)
        if (ival.lt.ihld) ival = ihld
      else
        ival = 1000000
      endif

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine skpsep
c*       Skip past the current delimeter. If we are processing a data
c*       statement, zero the next delimeter variable, otherwise increment
c*       inx.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine skpsep

      include 'com.com'

      common/dstcom/keydt, ixdt, nxthld, nxtret
      integer*4 keydt
      integer*2 ixdt, nxthld, nxtret

      if (ifl(324).gt.0) then
        nxtret = 0
      else
        inx = inx + 1
      endif

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine svpars
c*       Save the current parsing postition.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine svpars

      include 'com.com'

      common/dstcom/keydt, ixdt, nxthld, nxtret
      integer*4 keydt
      integer*2 ixdt, nxthld, nxtret

      common/dstsav/isvix, k324, kxdt, kxthld, kxtret, kncsfl,
     1              k105, k325, kextyp
      integer*2 isvix, k324, kxdt, kxthld, kxtret, kncsfl, k105,
     1          k325, kextyp
      common/dstsav1/k106, k107, k108
      integer*4 k106, k107, k108
      common /subwild/ nextnum, knextnum, kwname, wname
      common /subgeom/ geominx, kgeominx, kgeomnum      
      integer*4 nextnum, knextnum, kgeominx, geominx, kgeomnum
      character*64 wname, kwname
      
      isvix = inx
      k324   = ifl(324)
      kxdt   = ixdt
      kxthld = nxthld
      kxtret = nxtret
      kextyp = nextyp
      kncsfl = ncsfl(3)
      k105 = ifl(105)
      k106 = ifl4(106)
      k107 = ifl4(107)
      k108 = ifl4(108)
      k325 = ifl(325)
      knextnum = nextnum
      kwname = wname
      kgeominx = geominx
      kgeomnum = geomnum
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine rtpars
c*       Restore the current parsing postition.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine rtpars

      include 'com.com'

      common/dstcom/keydt, ixdt, nxthld, nxtret
      integer*4 keydt
      integer*2 ixdt, nxthld, nxtret

      common/dstsav/isvix, k324, kxdt, kxthld, kxtret, kncsfl,
     1              k105, k325, kextyp
      integer*2 isvix, k324, kxdt, kxthld, kxtret, kncsfl, k105, 
     1          k325, kextyp
      common/dstsav1/k106, k107, k108
      integer*4 k106, k107, k108
      
      common /subwild/ nextnum, knextnum, kwname, wname
      common /subgeom/ geominx, kgeominx, kgeomnum      
      integer*4 nextnum, knextnum, kgeominx, geominx, kgeomnum
      character*64 wname, kwname

      inx      = isvix
      ifl(324) = k324
      ixdt     = kxdt
      nxthld   = kxthld
      nxtret   = kxtret
      nextyp   = kextyp
      ncsfl(3) = kncsfl
      ifl(105) = k105
      ifl4(106) = k106
      ifl4(107) = k107
      ifl4(108) = k108
      ifl(325) = k325
      nextnum = knextnum
      wname = kwname
      geominx = kgeominx
      if (geominx.eq.0) then
          geomnum = 0
      else
          geomnum = kgeomnum
      endif
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine substx
c*       Expand text variables in curly braces in the input
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : global input cin is rewritten
C*    WARNINGS     : none
C********************************************************************/

      subroutine substx

      include 'com.com'

      integer*4 nclkey,psav(2),strlen1
      integer*2 nwds,ietype,k,ifirst,isva
      logical ldtsav
      equivalence (ifl(124),ifirst)
      character*256 str
      logical*2 lerr

c
c..... save the current macro pointer, as it can be changed by the
c..... parser (if a token is a macro name - parser calls vxchk which
c..... changes the current macro pointer to the token macro)
c
      if (ifl(38) .ne. 0) call mclspt (psav)

 20   call parser

      if (ityp.eq.5 .and. ist.eq.19) then
        lerr = err
        err = .false.
        isva = isvinx
        ldtsav = ldtext
        ldtext = .true.
c
c..... get a (maybe subscripted) text variable; do not expand it in parsit
c
        call parsit2
        err = lerr
        ldtext = ldtsav
        if (ityp.eq.2 .and. ist.eq.TEXTVAR .and. nextyp.eq.20) then
          ifirst = -1
          call gtdesc(tv,nclkey,nwds,ietype)
          k = 255
          call ncl_gttext(nclkey,str,k)
          isvinx = isva
          call txt2cin (str,k)
          inx = isvinx
          goto 20
c
c.......Added case so data retrieved from a data statement
c.......can be used. ASF 7/30/13
c
        else if (geom.or.vocab.or.scalar.or.lstrng) then
          k = strlen1(token2)
          isvinx = isva
          str = ''//token2(1:k)
          call txt2cin (str,k)
          inx = isvinx
          goto 20
        endif
      endif

      if (nextyp .ne. 11) goto 20

      if (ifirst .eq. -1) then
c
c..... find new location of the first '('
c
        ifirst = 0
        j = 0
30      j = j + 1
c
c..... allow wildcard like ABC(*), in this case, don't save ifirst
c
        if (ain(j).eq.'(' .and. ain(j+1).ne.'*') ifirst = j
        if (ifirst.eq.0 .and. j.lt.ifl(140)) goto 30
      endif

      inx = 1
c
c..... reset the current macro pointer
c
      if (ifl(38) .ne. 0) call mclrpt (psav)
c
c.....Check for data elements
c
      call preprs

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine txt2cin (str,k)
c*       Substitute the current token in the input by the string
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : global input cin is rewritten
C*    WARNINGS     : none
C********************************************************************/

      subroutine txt2cin (str,k)

      include 'com.com'

      character*256 str
      integer*2 k

      integer*2 j,l,numsv
      character*1 svain(1536)
      character*256 str1

      do j = inx+1,ifl(140)
        l = j - inx
        svain(l) = ain(j)
      enddo

      numsv = l

      call touppr(str,str1)

      do j = 1,k
        l = isvinx + j - 1
        ain(l) = str1(j:j)
      enddo

      do j = 1,numsv
        l = l + 1
        ain(l) = svain(j)
      enddo

      nccin = l
      cin(nccin+1:) = ' '
      ifl(140) = l

      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine parsit2
c*       Call parser and handles expressions.
c*       it handled the 'thru' claude here too
c*              ifl(105)  -  flag, 1 if the names are subscripted,
c*                                 2 if they are auto generated.
c*              ifl4(106)  -  contains the lower range
c*              ifl4(107)  -  contains the upper range
c*              ifl4(108)  -  contains the increment (either +1 or -1)
c*              sc116   -  contains the token representation of the
c*                           identifier if subscripted
c*      idtype, default to 0: represents the sub type of the 'thru'
c*                   -1 = if there are thru claud, just return 'thru' word value
c*                    0 = any type
c*                   > 0 = geometry type
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine parsit2

      include 'com8a.com'
      integer*2 isidtype,iss116
      
      if (err) return
      isidtype = idtype
      iss116 = s116
      idtype = -1
      call parsit
      idtype = isidtype
      s116 = iss116
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine preprs
C*       Pre-parse cin to replace data elements with their values so
C*       they can be accessed in any function. *SHOW commands will not
C*       be parsed since *SHOW can already access the correct values.
C*       Other entries with '[]' will be ignored.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine preprs

      include 'com.com'

      common/dstcom/keydt, ixdt, nxthld, nxtret
      integer*4 keydt, sub, ipg, iel, sublen, isub, strlen1, nc,savidx
      integer*2 ixdt, nxthld, nxtret, hldkey, eql, rpars, lpars, tifl
      integer*2 rbr, lbr, rpar, lpar, lstlbr, lablps, chars, lstrbr
      integer*2 lbr2,ifirst,istsav
      real*8 hldtv(25)
      character*96 label,tlabel, tlabel2
      character*64 svtok, savtok
      character*21 substr
      character*1536 tcin
      logical*2 shfl,curly,ipprin
      equivalence (ifl(124),ifirst)
      
      tinx = inx
      svtok = token2
      isub = ivxsub
      if (ifl(38) .eq. 1 .or. ifl(45) .eq. 1) goto 99999
      tcin = cin
      datst = 0
      nextyp = 0
      sinx = inx
      rbr = 0
      lbr = 0
      lbr2 = 0
      rpar = 0
      lpar = 0
      lstrbr = 0
      lstlbr = 0
      shfl = .false.
      curly = .false.
      ipprin = .false.
c
c.....Data statments do not need to be preparsed for show since
c.....they already have a show routine that can handle elements
c.......Statements starting with INSERT, PPRINT, etc. should also
c.......be ignored. - Andrew 11/28/12
c.......Preparsing should be done for *SHOW if there are nested
c.......data elements - Andrew 3/21/13
c      
      if (cin(1:2).eq.'*S'.or.cin(1:3).eq.'**S') shfl = .true.
      inx = 1
      call parser
      if (ityp.eq.1.and.((ist.ge.1043.and.ist.le.1046).or.
     x    ist.eq.1091)) then
         ipprin = .true.
      endif
      if (cin(1:1).eq.'{') curly = .true.
      inx = sinx
c
c.....The first ']' found will give the deepest nested element
c.....that has a ']'
c.......Changed to fix test case - ASF 1/14/14
c      
100   rbr = index(cin(lstrbr+1:),']') + lstrbr
      eql = index(cin,'=')
c
c.....Currently ignore data elements to the left of the equal sign unless
c.....they are in curly braces '{}'.
c
      if ((curly.eq..false..and. .not.ipprin .and.eql.gt.rbr).or.
     1    rbr.gt.nccin) goto 99999
      if (rbr.gt.lstrbr) then
        rpars = 0
        lpars = 0
c
c.......Removed to fix test case - ASF 1/14/14
c        rbr = rbr + lstrbr
        chars = 0
        lbr = rbr - 1
c
c.....Find the indices of brackets and parens for later parsing
c.......Removed to fix test case - ASF 1/14/14
c        
c        if (lbr.gt.lstlbr.and.lstlbr.gt.0) lbr = lstlbr - 1
        do while (cin(lbr:lbr).ne.'['.and.lbr.gt.0)
          lbr = lbr - 1
        end do
        if (lbr.eq.0) goto 9540
        lablps = lbr - 1
        if (cin(lbr-1:lbr-1).eq.')') then
          rpars = 1
          rpar = lbr - 1
          lpar = rpar - 1
c
c.....Subscript may be nested so the label search needs to start
c.....after all right parenthsis have had their corresponding left
c.....parenthesis found
c
          do while (lpars.lt.rpars.and.lpar.gt.0)
              if (cin(lpar:lpar).eq.')') then
                  rpars = rpars + 1
              else if (cin(lpar:lpar).eq.'(') then
                  lpars = lpars + 1
              endif
              lpar = lpar - 1
          end do
          if (lpar.gt.0.and.lpars.eq.rpars) then
              lablps = lpar - 1
          else
              goto 9309
          endif
        endif
c
c.....Find the label for the possible data statement
c        
        do while (lablps.gt.0.and.chars.lt.63.and.
     x   ((cin(lablps:lablps).ge.'A'.and.cin(lablps:lablps).le.'Z').or.
     x    (cin(lablps:lablps).ge.'0'.and.cin(lablps:lablps).le.'9').or.
     x     cin(lablps:lablps).eq.'_'.or. cin(lablps:lablps).eq.'#'))
          chars = chars + 1
          lablps = lablps - 1
        end do
        if (ipprin .and. cin(lablps:lablps) .ne. '@') then
          lstlbr = lbr
          lstrbr = rbr
          goto 100
        endif

        lablps = lablps + 1
c
c.....Check for a data element
c
        inx = lablps
        call parser
        if (ityp.eq.2.and.ist.eq.14) then
          if (lpar.gt.0) then
            call expres(ifl(325))
          else
            lstlbr = lbr
            lstrbr = rbr
            goto 100
          endif
        endif
        if (ipprin) lablps = lablps - 1
c
c.....No data element found so change nothing and coninue searching
c.....in cin for data elements beyond this level/point
c        
        if (ityp.ne.2.or.ist.ne.DATAST) then
          if (nextyp.ne.11) then
            lstlbr = lbr
            lstrbr = rbr
            goto 100
          endif
          goto 99999
        endif
        
        if (shfl) then
            lbr2 = lbr - 1
            do while (cin(lbr2:lbr2).ne.'['.and.lbr2.gt.0)
                lbr2 = lbr2 - 1
            end do
            if (lbr2 .le. 0) goto 99999
        endif
c
c.....Get element and replace its entry in cin with its value
c.....stored in its label
c.......Changed call to gtdesc so ifl(324) is not modified.  This will
c.......prevent parsit from trying to access a data element after the
c.......preparsing has already replaced it - Andrew 4/1/13
c        call gtdesc (tv, keydt, ifl(324), ist) 
        call gtdesc (tv, keydt, tifl, ist)
        inx = lbr
        call parser
        call parser
        call expres(ifl(325))
        call dtgtnw(keydt,n)
        if (itv .lt. 1 .or. itv .gt. n) go to 9085
        call dtgetv(keydt,itv,tv,ityp,nextyp,label,sub)
        length = nrndex(label,' ')
        if (sub.gt.0) then
          call itoc(sub,substr,sublen)
          write (tlabel,1001) label(1:length),substr(1:sublen)
1001      format(a<length>,'(',a<sublen>,')')
          label = tlabel
          length = length + sublen + 2
        endif
c
c.....If length = 0, then no element was found
c        
        if (length.gt.0) then
          if (ipprin) then
            call rmquot (label,length)
            cimage(lablps+length:) = cimage(rbr+1:nccimg)
            cimage(lablps:lablps+length-1) = label(1:length)
            call touppr (cimage,cin)
            nccimg = strlen1(cimage)
            nccin = strlen1(cimage)
          else
c...if it is directly assignment
c...treat it differently
            if ((cin(lablps-1:lablps-1).eq.'=').and.(ityp.eq.2)) then                
               savtok = token2
               savidx = ivxsub
               istsav = ist
               token2 = label
               ivxsub = sub
               call vstchk
               if (ist.ne.2) then
c
c...if it is geometry, the directory assignment is not allowed
c...but we will allow and assigned value as 0.0 if it is geometry
c...per Ken's request
c...Yurong
                    if (((ist.gt.2).and.(ist.le.10)) .or.
     1                    (ist.eq.18) .or. (ist.eq.20) .or. (ist.eq.33)                           
     2                    .or. (ist.eq.21) .or. (ist.eq.22)) then  
                       label = '0.0'
                       length = 3
                    endif
               endif 
               token2 = savtok
               ivxsub = savidx
               ist = istsav
            endif
            if ((cin(lablps-1:lablps-1).eq.'=').and.(ityp.eq.1)) then          
               write (tlabel2,1005) tv
1005           format (f6.0)
               length =  strlen1(tlabel2)
               call remspc (tlabel2(1:length),label)
               length = strlen1(label)
            endif
            cin(lablps+length:) = cin(rbr+1:)
            cin(lablps:lablps+length-1) = label(1:length)
c
c.....      QAR 101098: need to recalculate length for the changed cin
c
            nccin = strlen1(cin)
          endif
          lstrbr = 0
          lstlbr = 0
          if (ifirst .ge. lablps)
     1        ifirst = ifirst + (length-(rbr-lablps+1))
        else 
          goto 9224
        endif
        goto 100
      else 
        goto 99999
      endif       
c
c...                   Subscript out of range
c
9085  call error(85)
      goto 9000
c
c...                   Positive Integer Expected
c
9224  call error(224)
      goto 9000
c
c...                   ']' expected
c
9309  call error(309)
      goto 9000
c
c...                   '(' expected
c
9462  call error(462)
      goto 9000
c
c...                   '[' expected
c
9540  call error(540)
      goto 9000
      
9000  err = .true.
99999 inx = tinx
      token2 = svtok
      ivxsub = isub
      return
      end
