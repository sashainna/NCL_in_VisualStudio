C*********************************************************************
C*    NAME         :  assgn
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       assgn.f , 25.2
C*    DATE AND TIME OF LAST MODIFICATION
C*       11/22/17 , 11:04:42
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : assgn
C*       this route handles assignment statements.  assignment statements
C*       include declaration of scalar and geometry variables, and the re-
C*       definition of those variables.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine assgn

      include 'com8a.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      integer*4 nclkey,tmpsubx
      character in(80)
      equivalence(in,cin)
      character out(80)
      equivalence(out,cout)
      integer*2 irest(4)
      equivalence(irest,rest)
      character*64  tempid
      integer*2 i,skpsyn,irbsp,ixf,idum,sist
      equivalence (skpsyn,ifl(175))
      integer*2 i2v1,iwlev
      integer*2 POCKET,DATAV,DECOMPV,FORMATV,SUBSTRV,SOLIDV,COLORV
c
      parameter (DATAV = 858)
      parameter (DECOMPV = 618)
      parameter (FORMATV = 849)
      parameter (POCKET = 738)
      parameter (SOLIDV = 123)
      parameter (SUBSTRV = 580)
      parameter (COLORV = 900)
c
      data i2v1 /1/
      data iwlev /0/

      integer*2 cicvln
      logical*2 ldelold

c
      keyold = 0
      istold = 1
      sc(10) = 0.
      irbsp  = 0
      sist   = 0
      ifl(296) = 0
      ldelold = .FALSE.
c
c...reset multiple fillet display flag
c...vp 24-oct-96
c
      ifl(329) = 0
      if (ityp.ne.1) go to 500
      if (ist .eq. 576) then
          call clone(2)
      else if (ist .eq. 577) then
          call clone(3)
      else if (ist .eq. 1008) then
          call revers(2)
      else 
          go to 100
      endif
      goto 99999

c ******************************************** if its auto name gen
  100     call vctoid (ist,idst,ixf)
          sist   = ist
          isvtyp = 1
c
c...Added support for NURB curve & surface definitions
c...Bobby  -  3/5/92, Ian 10-Mar-92
c
          if (ist .eq. 614 .or. ist .eq. 628) irbsp = 1 
          if (ist .eq. 619) irbsp = 2 
c
          isvidx = isvinx
c
c...Autonaming is not allowed with FILLET statement
c...Bobby  -  5/6/96
c
  150     if (ist .eq. 402 .and. sc(169).lt.9.349) then
              ifl(2) = 425
              go to 99999
          endif
c...Autonaming not allowed for DATA          
          if (ist .eq. 858) then
              ifl(2) = 425
              go to 99999
          endif
c
c     the 0 passed to namgen means get the next name but don't
c     bump the unibase counter until after the call to vstore.
c
          call namgen (0, idst, token2, ivxsub)
          if (ivxsub.gt.999999) then
             ifl(2) = 85
             goto 99999
          endif
          tempid= token2
          tmpsubx = ivxsub
          ivxsub = 0
          call vstchk
          token2 = tempid
          savid2 = token2
          ivxsub = tmpsubx
          ISVSUB = ivxsub
c
c    If the identifier is previously defined and it is
c    not subscripted.
c
          if (ist .ne. 1 .and. ist .ne. 14 .or. 
     x            (ist .eq. 14 .and. ivxsub .eq. 0)) then
              call idtovc (idum,idst,ixf)
              ifl(ixf) = ifl(ixf) + 1
              isvtyp = ifl(ixf) + 1
c
c     force namgen to update label counter and get next name
c
              call namgen (1, idst, token2, ivxsub)
              if (ivxsub.gt.999999) then
                 ifl(2) = 85
                 goto 99999
              endif
              go to 150
          endif
          ifl(9) = ifl(11)
          ifl(10) = ifl(12)
c
c            if subscripted save reseve id in ifl(9) , ifl(10)
c            location the first time
c
          if (ivxsub .gt. 0.and. ist.eq.1) then
             ivxsub=0
             isvsub=0
             isvst = idst
             idst=14
             rest=0.
             irest(3)=32767
             irest(4)=14
             call vstore
             idst = isvst
             token2 = tempid
             savid2 = token2
             ivxsub = tmpsubx
             ISVSUB = ivxsub
          endif
c             if subscripted get the page and element number to save the
c             geometry.
          if (ivxsub .gt. 0) then
             call vstchk
             if (ist .ne. 1 .and. ist .ne. 14 .or.
     1           (ist .eq. 14 .and. ivxsub .eq. 0)) then
c                call idtovc (idum,idst,ixf)
                 ifl(ixf) = ifl(ixf) + 1
                 isvtyp = ifl(ixf) + 1

c                  force namgen to update label counter and get next name
                 call namgen (1, idst, token2, ivxsub)
                 if (ivxsub.gt.999999) then
                    ifl(2) = 85
                    goto 99999
                 endif
                 go to 150
             endif
             ifl(9) = ifl(11)
             ifl(10) = ifl(12)
          endif
          if (nextyp.ne.5) then
               isvinx = inx
               ifl(2)=22
               go to 99999
          endif
          ifl(44) = 9
          if (idst.eq.8) then
              idtype = 3
              call parsit
          else if (idst.eq.9) then
              idtype = 8
              call parsit
          else if ((ityp.eq.2).and.(idst.eq.11)) then
              idtype = -1
              call parsit
          else
              call parsit
          endif
          if (err) go to 99999
          if (idst.eq.POINT) go to 200
          if (idst.eq.VECTOR) go to 210
          if (idst.eq.LINE) go to 220
          if (idst.eq.PLANE) go to 230
          if (idst.eq.CIRCLE) go to 240
          if (idst.eq.CURVE) go to 250
          if (idst.eq.SURF) go to 260
          if (idst.eq.MATRIX) go to 270
          if (idst.eq.SHAPE) go to 190
          if (idst.eq.PATERN) go to 180
          if (idst.eq.PNTVEC) go to 280
          if (idst.eq.VANOTE) go to 290
          if (idst.eq.VSYMBOL) go to 300
          if (idst.eq.VPLACE) go to 310
          if (idst.eq.VSOLID) go to 320
          go to 600
c ******************************************* if its an identifier
500   if (.not.(ityp.eq.2)) go to 600
          savid2=token2
          isvidx=isvinx
          isvsub = ivxsub
          idst=ist
          ifl(296) = 1
c
c...   Save off key & subtype for vstore.
c
          if (idst .ne. 1) then
            keyold = keyhld
            istold = ist
          endif
c
c...check if it is voc word (voc word could be saved as scalar when inside
c...a data statement)
c
          if (idst.eq.2) then
             call sclvoc (keyhld, i)
             if (i.eq.1) then
                go to 4000
             endif
          endif          
          ifl(9)=ifl(11)
          ifl(10)=ifl(12)
          if (nextyp.eq.1) then
              ifl(44)=0
              ldtext = .true.
              call parsit
              call parsit
              ldtext = .false.
C
C...Save ist incase this is an NSHAPE.  JLS 5/7/99
C
              sist = ist
              if (ifl(2).ne.0) go to 99999
              if (ityp .eq. 1 .and. nextyp .eq. 5) then
                  call skpsep
                  ifl(44)=9
              endif
          else
              isvinx=inx
              ifl(2)=6
              go to 99999
          endif
          if (ityp.ne.1) goto 3000

          if (ist .eq. 840) then ! the REDEF command
            if (ifl(296).eq.1 .and. idst.eq.9) then
              if (savid2(1:1) .ne. '@') then
                if (ifl(41) .eq. 0) then
                  isvinx=isvidx
                  ifl(2)=8
                  goto 99999
                else
                  nclkey = keyold
                  ldelold = .TRUE.
                endif
              endif
              idst = 1
            endif
            idum = 0
            call redef(idum)           
            if (ldelold) call dlgeom (keyold)
            go to 910
          endif 

          if (ist.eq.575.or.ist.eq.576.or.ist.eq.1008.or.ist.eq.POCKET
     x        .or.ist.eq.DATAV .or. ist.eq.402 .or. ist .eq. DECOMPV
     2        .or.ist.eq.COLORV)    goto 1000
          if (ist.eq.FORMATV .or.ist.eq.SUBSTRV) goto 3000
c                  its a vocab word and not DIST, CLONE, REMOVE, or POCKET
              if (idst.ne.1.and.ifl(41).eq.0) then
                  if (savid2(1:1) .ne. '@') then
                      isvinx=isvidx
                      ifl(2)=8
                      go to 99999
                  endif
              endif
              if (ist.eq.806) then
                  i=11
              else
                  call vctoid (ist,i,idum)
                  if (ist .eq. 614 .or. ist .eq. 628) irbsp = 1
                  if (ist .eq. 619) irbsp = 2 
              endif
c
c..... for some commands we allow to overwrite a curve-like entity by 
c..... another such (e.g., a line by a circle, or a curve by a line);
c..... these are commands that create multiple curve-like entities
c..... uniformly labelled as curves
c
              if (idst.ne.1 .and. idst.ne.i) then
                if (ist.eq.608 .and. nextyp.eq.5 .and.
     x              (idst.eq.14 .or. cicvln(idst).eq.1)) then
                  inxa = isvinx
                  inxb = inx
                  call parsit
                  if (vocab .and. (ist.eq.653 .or. ist.eq.727)) then
                    if (ist.eq.727) call parsit
                    if (vocab .and. (ist.eq.653 .or.
     x                  ist.eq.612 .or. ist.eq.816)) then
                      isvinx = inxa
                      inx = inxb
                      nextyp = 5
                      goto 177
                    endif
                  endif
                endif
                ifl(2)=89
                go to 99999
              endif
177           idst=i
              if (nextyp.ne.5.and.idst.ne.11) then
                  isvinx=inx
                  ifl(2)=22
                  go to 99999
              endif
              if (ityp .eq. 1 .and. ist .eq. 616) ldtext = .true.
              ifl(44)=9
              if (idst.eq.8) then
                  idtype = 3
                  call parsit
              else if (idst.eq.9) then
                  idtype = 8
                  call parsit
              else if ((ityp.eq.2).and.(idst.eq.11)) then
                  idtype = -1
                  call parsit
              else
                  call parsit
              endif
              ldtext = .false.
              if (err) go to 99999
c
c...PATERN
c
180           if (idst.eq.PATERN) then
                  call declpn
                  go to 910
              endif
c
c...SHAPE
c
190           if (idst.eq.SHAPE) then
                  if (ifl(142).ne.0) then
                      call declsh
                      if (sist.eq.615) then
                          isc10(1) = 615
                      endif

                  else
                      ifl(2)=235
                      go to 99999
                  endif
                  go to 910
              endif
c
c...POINT
c
200           if (idst.eq.POINT) then
                  call declpt
                  go to 910
              endif
c
c...VECTOR
c
210           if (idst.eq.VECTOR) then
                  call declve
                  if (isc10(2).eq.11) then
c                        its a ve thru a point and normal to a surf
c                        mocntl must be called
                      isc10(1)=575
                      isc10(2)=2
                  endif
                  go to 910
              endif
c
c...LINE
c
220           if (idst.eq.LINE) then
                  call declln
                  go to 910
              endif
c
c...PLANE
c
230           if (idst.eq.PLANE) then
                  call declpl
                  go to 910
              endif
c
c...CIRCLE
c
240           if (idst.eq.CIRCLE) then
                  if (sist .ne. 402) then
                      call declci
                  else
                      INX = ISVINX
                      call fillet (0)
                  end if
                  go to 910
              endif
c
c...CURVE
c
250           if (idst.eq.CURVE) then
                  if (irbsp .eq. 1) isc10(1) = 628
				      if (irbsp .eq. 2) isc10(1) = 619 

                  if (irbsp .eq. 2) then
                      call declss
                  else
                      call declcv
                  end if
                  go to 910
              endif
c
c...SURF
c
260           if (idst.eq.SURF) then
                  if (irbsp .eq. 0) then
                      call declsf
                  else
                      call declnf
                  end if 
                  go to 910
              endif
270           if (idst.eq.MATRIX) then
                  call declmx
                  go to 910
              endif
c
c...PNTVEC
c
280           if (idst.eq.PNTVEC) then
                  call declpv
                  go to 910
              endif
c
c...ANOTE
c
290           if (idst.eq.VANOTE) then
                  call declan
                  go to 910
              endif
c
c...SYMBOL
c
300           if (idst.eq.VSYMBOL) then
                  call declsy
                  go to 910
              endif
c
c...PLACE
c
310           if (idst.eq.VPLACE) then
                  call placsy
                  go to 910
              endif
c
c...SOLID
c
320           if (idst.eq.VSOLID) then
                  call declso
                  go to 910
              endif
c
c...MACRO
c
              if (idst.eq.VMACRO) then
                  call decmac
                  go to 910
c
c...Unrecognized assignment
c
              else
                  ifl(2)=1
                  go to 99999
              endif
910           continue
              go to 5000
c
1000      if (ityp .ne. 1) goto 3000
          if (ist .eq. DATAV) then
            call dodata
            goto 5000
          else if (ist.eq.POCKET) then
            call mosyn2
            if (ifl(2).eq.0) call razpok (i2v1,iwlev)
            goto 5000
          else if (ist.eq.COLORV) then
            call decclr
            goto 5000           
          endif
          if (ist .eq. DECOMPV) then
              call decomp
              go to 5000
          endif
c
c...DIST
c
          if (ist .eq. 575) then
c                       its a dist function
              call dist
              if (nextyp.ne.11) then
                  isvinx=inx
                  ifl(2)=4
                  go to 99999
              endif
              rest = tv
              idst = 2
              go to 5000
c
c...CLONE
c
          else if (ist.eq.576) then
              call clone(1)
              go to 5000
c
c...REVERS
c
          else if (ist.eq.1008) then
              call revers(1)
              goto 5000
c
c...FILLET
c
          else if (ist .eq. 402) then
              call fillet (1)
              goto 5000
          endif
3000  continue
          if ((idst.eq.1 .or. idst.eq.TEXTVAR) .and.
     x        (lstrng .or. (ityp .eq. 2 .and. ist .eq. VANOTE) .or.
     x        (ityp.eq.1.and.(ist.eq.FORMATV.or.ist.eq.SUBSTRV)))) then
            call dotext
            if (ifl(2).gt.0) goto 99999
            goto 5000
          endif
          if (.not.((ityp .gt. 1 .and. ityp .lt. 5) .and.
     x             (idst .eq. 2 .or. idst .eq. 1))) go to 4000
              if (ityp .eq. 2 .and. ist .ne. 2) then
                  if (ist.eq.1) then
                      ifl(2)=9
                  else
                      ifl(2)=7
                  endif
                  go to 99999
              endif
              if ((ifl(111) .eq. 0 .and. nextyp .ne. 11) .or.
     x            (ifl(111) .eq. 1 .and. nextyp .ne. 7)) then
                  isvinx=inx
                  ifl(2)=4
                  goto 99999
              endif
              rest = tv
              idst = 2
              go to 5000
4000          continue
c...should include scalar too
c...              if (idst.gt.2.and.idst.lt.13) then
              if (idst.ge.2.and.idst.lt.13) then
                  ifl(2)=89
              else
                  ifl(2)=12
              endif
              go to 99999
5000      continue
600   continue

99999 if (ifl(2).gt.0 .or. (ifl(2).lt.0 .and. sc(169).lt.9.149d0)) then
          call error (ifl(2))
          ifl(2)=0
          err=.true.
      endif
c      ifl(296) = 0
c
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : assgn0
C*       this routine just generates a label - called when the command
C*       requires a new label but the assgn is not called.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine assgn0

      include 'com8a.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      character in(80)
      equivalence(in,cin)
      character out(80)
      equivalence(out,cout)
      integer*2 irest(4)
      equivalence(irest,rest)
      character*64  tempid
      integer*2 skpsyn
      equivalence (skpsyn,ifl(175))
      integer*2 ixf,idum,sist
      integer*4 tmpsubx

      integer*2 i2v1
      data i2v1 /1/
c

          keyold = 0
          istold = 1
          sist   = ist
          ist = 609
c ******************************************** if its auto name gen
          call vctoid (ist,idst,ixf)
          isvtyp = 1
c
          isvidx = isvinx
c
c     the 0 passed to namgen means get the next name but don't
c     bump the unibase counter until after the call to vstore.
c
300       call namgen (0, idst, token2, ivxsub)
          if (ivxsub.gt.999999) then
             ifl(2) = 85
             goto 99999
          endif
          tempid= token2
          tmpsubx = ivxsub
          ivxsub = 0
          call vstchk
          token2 = tempid
          savid2 = token2
          ivxsub = tmpsubx
          ISVSUB = ivxsub
c
c    If the identifier is previously defined and it is
c    not subscripted.
c
          if (ist .ne. 1 .and. ist .ne. 14 .or. 
     x            (ist .eq. 14 .and. ivxsub .eq. 0)) then
              call idtovc (idum,idst,ixf)
              ifl(ixf) = ifl(ixf) + 1
              isvtyp = ifl(ixf) + 1
c
c     force namgen to update label counter and get next name
c
              call namgen (1, idst, token2, ivxsub)
              if (ivxsub.gt.999999) then
                 ifl(2) = 85
                 goto 99999
              endif
              go to 300
          endif
          ifl(9) = ifl(11)
          ifl(10) = ifl(12)
c
c            if subscripted save reseve id in ifl(9) , ifl(10)
c            location the first time
c
          if (ivxsub .gt. 0.and. ist.eq.1) then
             ivxsub=0
             isvst = idst
             idst=14
             rest=0.
             irest(3)=32767
             irest(4)=14
             call vstore
             idst = isvst
             token2 = tempid
             savid2 = token2
             ivxsub = tmpsubx
             ISVSUB = ivxsub
          endif
c             if subscripted get the page and element number to save the
c             geometry.
          if (ivxsub .gt. 0) then
             call vstchk
             if (ist .ne. 1) then
                 ifl(ixf) = ifl(ixf) + 1
                 isvtyp = ifl(ixf) + 1
c                  force namgen to update label counter and get next name
                call namgen (1, idst, token2, ivxsub)
                if (ivxsub.gt.999999) then
                   ifl(2) = 85
                   goto 99999
                endif
                go to 300
             endif
             ifl(9) = ifl(11)
             ifl(10) = ifl(12)
          endif


99999 ist = sist
c
      return
      end

