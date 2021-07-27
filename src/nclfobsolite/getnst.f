C*********************************************************************
C*    NAME         :  getnst.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       getnst.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:07
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getnst (found1,it)
C*      the purpose of this subroutine is to parse the input line and
C*      handle any nested geometry items. it does so by replacing the
C*      nested item in the string by a special name starting with a
C*      '@' and storing the canonical form in the variable symbol table
C*      under that name.  for example, the input string:
C*
C*          ln/intof,(point/1,2,3),pta
C*
C*      gets converted to:
C*
C*         ln/intof,@pt1,pta
C*
C*      getnst gets called from driver until the arguement 'found1'
C*      is returned as zero. Argument it is passed back from beginp
C*      to  driver so entity can be displayed if necessary.
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
      subroutine getnst (found1,it)

      include 'com8a.com'

      integer*2 found1,ifirst
      integer*2 it,isx, ix, idum
      equivalence (ifl(124),ifirst)
      integer*2 ipscnt
      equivalence (ipscnt,ifl(305))
      integer*2 IFV/825/,PSISV/713/,LABELV/13/,COLONV/13/,RPARENV/7/

      if (found1.eq.0) then
c             first call; zero item counters and get the input line
          ifl(121)=0
          if (err.or.ifl(2).ne.0) then
              err=.false.
              ifl(2)=0
          endif
          ipscnt=0
          ldtext = .true.
          call parsit
          ldtext = .false.
          ldtflg = .false.
c
c...get past label and delimiter, if any
c
          if (ityp.eq.2 .and. ist.eq.LABELV) then
            call parsit
            if ((nextyp.eq.COLONV.or.nextyp.eq.RPARENV).and.
     x           nextyp.ne.ifl(44)) call parsit
           endif
          ifl(369) = 1535
          if (vocab) then
c
c...Set ifl(369) to the closing paren of the IF statement so that
c...nested items following the IF will be handled later.
c
            if (ist.eq.IFV) then
              j = 1
              do i=inx+1,nccin,1
                if (ain(i).eq.' ') goto 10
                if (ain(i).eq.'(') then
                  j = j+1
                else if (ain(i).eq.')') then
                  j = j-1
                  if (j.eq.0) then
                    ifl(369) = i
                    goto 10
                  endif
                endif
              enddo
10            continue
            endif
c
c...set flag if it is a psis statement & use different name prefix
c
            if (ist.eq.PSISV) ipscnt=1
          endif
      endif
      if (ifirst.eq.0.or.ifl(38).eq.1.or.ifl(45).eq.1) then
c              there was no '(' in the input line or its a loop or macro
          irslt=0
          inx=1
      else
c               see if there's a nested item anywhere on the line
          inx = ifirst
          call beginp (irslt,it,isx)
          if (err) go to 99999
      endif
      if (irslt.eq.1) then
c               irslt=1 means there is a nested item on the line
100       call beginp (irslt,it,isx)
c                see if there's an inner nest
          if (err) go to 99999
          if (irslt.eq.1) go to 100
c                if irslt=0 then there was no inner nest, so process it
          if (it.eq.3) then
c                   its a nested named scalar
               rest=tv
               idst=2
          else
              call idtovc (idum,idst,ix)
              if (ipscnt.gt.0.and.(idst.eq.6.or.idst.eq.9)) then
                  ipscnt=ipscnt+1
c
c......Allow for up to 99 nested definitions
c......Bobby  -  9/29/97
c
                  if (ipscnt.gt.99) ipscnt=1
              else
                  ifl(ix) = ifl(ix) + 1
c
c......Allow for up to 99 nested definitions
c......Bobby  -  9/29/97
c
                  if (ifl(ix) .gt. 99) ifl(ix) = 1
              endif
              ifl(44)=9
c               call pthru if cv or sf (possible thru condition)
              if (idst.eq.8) then
                  idtype = 3
                  call parsit
              else if (idst.eq.9) then
                  idtype = 8
                  call parsit
              else
                  call parsit
              endif
              if (idst.eq.3) then
                  call declpt
              else if (idst.eq.4) then
                  call declve
              else if (idst.eq.5) then
                  call declln
              else if (idst.eq.6) then
                  call declpl
              else if (idst.eq.7) then
                  call declci
              else if (idst.eq.8) then
                  if (isc10(1).eq.619) then
                    call declss
                  else
                    call declcv
                  endif
              else if (idst.eq.9) then
                  call declsf
              else if (idst.eq.10) then
                  call declmx
              else if (idst.eq.18) then
                  if (ifl(142).eq.0) then
                      call error(235)
                      go to 99999
                  endif
                  call declsh
              else if (idst.eq.20) then
                  call declpn
              else if (idst.eq.21) then
                  call declpv
              else if (idst.eq.33) then
                  call declso
              else
                  call error(89)
                  go to 99999
              endif
              if (err) go to 99999
          endif
c                 the next block of code puts points inx back to
c                 the ')' if its not already there
          if (ityp.eq.5.and.ist.eq.7) then
              if (nextyp.eq.ifl(44)) then
                  inx=inx-2
              else
                  inx=inx-1
              endif
          endif
c              rebuild cin to contain the new name
          if (it.eq.1) then
c                   its an un-named item
c
c...Changed to (2) digits for numeric portion of name
c...Bobby  -  9/29/97
c
cc              cin(isx:isx+3)=saveid
cc              icnt=inx-(isx+3)
cc              isx=isx+4
              cin(isx:isx+4)=savid2
              icnt=inx-(isx+4)
              isx=isx+5
          else
c                 its a named item, so shift it over in the input line
c                 (if it's a scalar, leave the parentheses so variables
c                  indexed by named scalars will work.)
120           if (idst.ne.2) ain(isx)=ain(isx+1)
              isx=isx+1
              if (ain(isx+1).ne.'=') go to 120
              if (idst.eq.2) then
                ain(isx+1)=')'
                isx=isx+2
              endif
              icnt=inx-isx+1
          endif
c              now fill the space with '~'
          do 150 j=isx,isx+icnt-1
150           ain(j)='~'
c                keep the number of deleted chars in case of error
          ifl(121)=ifl(121)+icnt
          found1=1
          inx=ifirst
      else
          found1=0
      endif
99999 continue
c
c.....reset this number to 0 because we will not get all of FILTER item
c.....but need stop it
c
      if (geomnum.gt.0) then
          call reset_geomlst
      endif
      return
      end
