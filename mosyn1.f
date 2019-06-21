C*********************************************************************
C*    NAME         :  mosyn1.for
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        mosyn1.for , 25.8
C*    DATE AND TIME OF LAST  MODIFICATION
C*        01/20/17 , 11:18:39
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     :
c*      this routine handles the parsing and syntax checking for
c*      motion and motion related statements.  it builds the syscom in
c*      preparation for the motion generation section in the following
c*      format:
c*          sc(10)
c*                first word=type of motion statement:
c*                      701 = from
c*                                ist 1 = from/x,y,z
c*                                        from/pt1
c*                                ist 2 = from/pt1,0,0,1
c*                                        from/2,3,4,0,0,1
c*                                        from/2,3,4,ve1
c*                                        from/pt1,ve1
c*                                        from/pv1
c*                      703 = goto
c*                                ist 1 = goto/x,y,z
c*                                ist 2 = goto/pt1
c*                                ist 2 = goto/pv1
c*                                ist 3 = goto/patern
c*                      704 = gofwd
c*                                ist 1 = gofwd/ds,past,cs
c*                      705 = golft
c*                                ist 1 = golft/ds,past,cs
c*                      706 = gorgt
c*                                ist 1 = gorgt/ds,past,cs
c*                      707 = goback
c*                                ist 1 = goback/ds,past,cs
c*                      710 = godlta
c*                                ist 1 = godlta/x,y,z
c*                                ist 2 = godlta/1
c*                                ist 3 = godlta/pl1
c*                                ist 4 = godlta/ve1
c*                                ist 4 = godlta/pv1
c*                      711 = indirv      indirv/ve1
c*                                        indirv/a,b,c
c*                      712 = indirp      indirp/pt1
c*                                        indirp/x,y,z
c*                      713 = psis   sc(35) gets the surface
c*                      716 = cutter sc(11) thru sc(17) have arguments
c*                      717 = thick  sc(23) thru sc(25) have arguments
c*                      731 = toler  sc(27) gets value
c*                      739 = scrub
c*                                  scrub/sf,n  sc(10) = 1st int*2 = 739
c*                                                       2nd int*2 =   1
c*                                                       3rd int*2 = no use
c*                                                       4th int*2 = no use
c*                                              sc(11) = surface address
c*                                              sc(12) = value of n
c*
c*                                  scrub/sf,n,pt1,pt2,pt3,pt4
c*                                              sc(10) - sc(12) same as
c*                                                      scrub/sf,n except
c*                                                      isc10(2)=2
c*                                              sc(13) - sc(16) addresses
c*                                                    of pt1, pt2, pt3,
c*                                                    pt4 respectivly
c*                     1021 = disply
c*
c*                                  disply/pt
c*                                              sc(10) - 1st i*2 = 1021
c*                                                       2nd i*2 = 3
c*                                                       3rd i*2 = not used
c*                                                       4th i*2 = not used
c*                                              sc(11) - pt address
c*
c*                                  disply/ve
c*                                              sc(10) - same as
c*                                                       disply/pt except
c*                                                       2nd i*2 = 4
c*                                              sc(11) - ve address
c*
c*                                  disply/ln
c*                                              sc(10) - same as
c*                                                       disply/pt except
c*                                                       2nd i*2 = 5
c*                                              sc(11) - ln address
c*
c*                                  disply/ci,m
c*                                              sc(10) - same as
c*                                                       disply/pt except
c*                                                       2nd i*2 = 7
c*                                              sc(11) - ci address
c*                                              sc(12) - value of m
c*
c*                                  disply/cv,m
c*                                              sc(10) - same as
c*                                                       disply/pt except
c*                                                       2nd i*2 = 8
c*                                              sc(11) - cv address
c*                                              sc(12) - value of m
c*
c*                                  disply/sf,n,m
c*                                              sc(10) = sc(10) same as
c*                                                       disply/pt except
c*                                                       2nd i*2 = 9
c*                                              sc(11) - sf address
c*
c*                                  disply/sf    sc(10) = same as
c*                                                        disply/pt except
c*                                                        2nd i*2 = 10
c*                                               sc(11) = surface address
c*                                               sc(12) = value of n
c*                                               sc(13) = value of m
c*
c*                                  disply/sf,n,m,pt1,pt2,pt3,pt4
c*                                              sc(10) - same as
c*                                                       disply/pt except
c*                                                       2nd i*2 = 11
c*                                              sc(11) - sf address
c*                                              sc(12) - value of n
c*                                              sc(13) - value of m
c*                                              sc(14) - sc(17) addresses
c*                                                       of pt1, pt2, pt3,
c*                                                       pt4 respectivly
c*
c*                                  disply/shape
c*                                              sc(10) - same as
c*                                                       disply/pt except
c*                                                       2nd i*2 = 18
c*
c*                                  disply/patern
c*                                              sc(10) - same as
c*                                                       disply/pt except
c*                                                       2nd i*2 = 20
c*
c*                                  disply/axis,x,y,z
c*                                              sc(10) - same as
c*                                                       disply/pt except
c*                                                       2nd i*2 = 21
c*                                              sc(11) - x-axis value
c*                                              sc(12) - y-axis value
c*                                              sc(13) - z-axis value
c*
c*                                  disply/geotyp,n,m
c*                                              geotyp may be pt, ve, ln,
c*                                               ci, cv or sf
c*                                              sc(10) - same as
c*                                                       disply/pt except
c*                                                       2nd i*2 = 20 +
c*                                                       geometry subtype
c*                                              sc(11) - value of n (if
c*                                                       geometry subtype
c*                                                       ci, cv or sf
c*                                              sc(12) - value of m
c*                                                       geometry subtype
c*                                                       is sf
c*
c*                                  disply/all,cm,sn,sm
c*                                              sc(10) - same as
c*                                                       disply/pt except
c*                                                       2nd i*2 = 40
c*                                              sc(11) - value of cn
c*                                                       (used like 'n' in
c*                                                       disply/cv,n
c*                                                       command)
c*                                              sc(12) - value of sn
c*                                                       (used like 'n' in
c*                                                       disply/sf,n,m
c*                                                       command)
c*                                              sc(13) - value of sm
c*                                                       (used like 'm' in
c*                                                       disply/sf,n,m
c*                                                       command)
c*
c*
c*                                 741 = maxang         sc(80) gets angle
c*
c*
c*      for the motion statements:
c*          sc(11)   =  drive surface
c*          sc(12)   =  modifier
c*          sc(13)   =  check surface
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
      subroutine mosyn1
 
      include 'com8a.com'
      include 'comgt.com'
      include 'cutter.com'
      include 'suvcom.com'
      include 'mocom.com'
 
      common/cptcom/chkpt
      real*8 chkpt(5)
 
      common/avdcom/avflgs
      logical avflgs(5)
 
      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld, strlen1
      integer*2 istold
 
      common/autofwd/scauto,csauto,avauto,chkauto,muauto,mvauto,
     *       nlkahd,lcheck,nlstcs
      real*8 scauto(1008),chkauto(504)
      real*4 muauto(504),mvauto(504)
      integer*2 nlkahd(504),nlstcs,nlook
      logical avauto(504),csauto(504),lcheck,llavoid
 
      real*8 FEEDR
      equivalence (FEEDR,MOTMAP(24))
      integer*2 ktv(4),itemp(4),isc11(4),len
      real*8 temp
      equivalence (tv,ktv)
      equivalence (temp,itemp),(isc11,sc(11))
      integer*2 i,j,nwds,ierr,isvist,ifv,nents,isv,ietype,ichk,isvix
      integer*2 ifrst,numr8s,cscond
      character*256 txt
      real*8 pv(6),htv,buf(14),svtv,f_dist,f_dot,f_mag,d1,dhld
      real*8 plend(6), cibuf(11),sc10
      real*4 u4
      integer*2 isc133(4)
      real*4 r4133(2)
      equivalence (sc(133),r4133),(sc(133),isc133)
      integer*4 nclkey,keyent,nup,nupt,nvp,nvpt,i0
      logical trflg
      real*8 asn
      real*4 bsn(2)
      integer*2 ksn(4)
      equivalence (asn,bsn,ksn)
      logical llstep, lgfacmp, leos
      integer*2 STEP, ON, OFF, NEARPT, TO, TLONPS, TLOFPS, AVOID
      integer*2 FIND, LOOK, GFA, CHECK, ONCEV, NOPSV,comma
      parameter (STEP = 92)
      parameter (ON = 71)
      parameter (OFF = 72)
      parameter (NEARPT = 866)
      parameter (TO     = 714)
      parameter (TLONPS = 724)
      parameter (TLOFPS = 725)
      parameter (AVOID  = 327)
      parameter (FIND  = 872)
      parameter (LOOK  = 871)
      parameter (GFA  = 873)
      parameter (CHECK  = 1023)
      parameter (ONCEV  = 325)
      parameter (NOPSV  = 726)
C
C...A flag to catch all tokens when working with a multiple
C...thick statement. It needs to be set to 0 incase it has
C...previously been used.
C
      integer*2 thflag
      thflag = 0
      i0 = 0
 
      trflg = .false.
      llavoid = .false.
      comma =0
 
      isvist=ifl(21)
C
C...Autops
C
      if (ist.eq.745) then
        isc10(1)=745
        fautops = .true.
        goto 8299
      endif
C
C...TLONPS or TLOFPS
C
      if (ist.eq.TLONPS.or.ist.eq.TLOFPS) then
        if (nxteos) then
          if (ist.eq.TLONPS) then
            ifl(342) = 1
            fautops = .false.
          else
            ifl(342) = 0
            fautops = .false.
         endif
        endif
        sc(10) = 0.0d0
        goto 8299
      endif
C
C...strip off tlrgt,tllft,tlon,nops
C
      if (ist.gt.717.and.ist.lt.721.or.ist.eq.726) then
          if (ist.eq.718) then
              ifl(219)=0
          else if (ist.eq.719) then
              ifl(219)=-1
          else if (ist.eq.720) then
              ifl(219)=1
          endif
          if (ist.eq.726) then
C
C...nops     epm   8-15-87
C
             ifl(276) = 1
             fautops = .false.
          else if (ifl(215).eq.0) then
C
C...we're not doing implied check surface
C
              ifl(21)=ifl(219)
          endif
          if (nextyp.eq.11) then
              sc(10)=0
              go to 88889
          endif
          if (nextyp.ne.9) then
              ierr=57
              go to 99998
          endif
          inx=inx+1
          call parsit
          if (ityp.ne.1.or.((ist.lt.701.or.ist.gt.710).and.ist.ne.GFA))
     x                 then
              ierr=27
              go to 99998
          endif
      endif
      isc10(1)=ist
      isc10(2)=1
C
C...if the next token is a '/' and an auto display is not in progress
C
      if (nextyp.ne.5.and.ifl(135).ne.2) then
          isvinx=inx
          ierr=22
          go to 99998
      endif
      ifl(44)=9
C
C...added ist ne 1095. kathy
C
      if (ifl(135).ne.2 .and. ist .ne. 1095 .and. ist .ne. 1021) then
         isv = inx
         call parsit
      endif
 
      if (isc10(1).eq.750) then
C
C...rmill
C
           call rmill
           go to 99999
      else if ((isc10(1).gt.703.and.isc10(1).lt.710)
     *         .or. isc10(1).eq. GFA) then
        sc(11)=tv
        lgfacmp = .false.
c
C... if Auto Gofwd, set lautgf and set the first go direction to GOFWD.
c
        if (isc10(1).eq.GFA) then
           isc10(1) = 704
           lautgf = .true.
           lcheck = .false.
           nlstcs = 0
           nlook = 1
           nents = 0
c
c... Check for initial AVOID, FIND or LOOK allowed for composite curve
c... combined drive and check surface.
c
c           do while (vocab) ! This test 'do while' compiles
c           do while (vocab .and. ist.eq.327) ! these 2 get a strange
c           do while (vocab .and.             ! compiler error on SGI only
c     *              (voc.eq.AVOID.or.voc.eq.FIND.or.voc.eq.LOOK))
   10      continue
           if (vocab .and.
     *        (voc.eq.AVOID.or.voc.eq.FIND.or.voc.eq.LOOK)) then
              if (voc.eq.AVOID) then
                  llavoid = .true.
              else if (voc.eq.FIND) then
                  llavoid = .false.
              else if (voc.eq.LOOK) then
                  call parsit
                  if (.not.scalar) then
                    ierr = 7
                    goto 99998
                  endif
                  if (itv.gt.0 .and. itv.lt.6) then
                    nlkahd(1) = itv
                    nlook = itv
                  endif
              endif
              call parsit
              if (itv.gt.0 .and. itv.lt.6) then
                nlkahd(1) = itv
                nlook = itv
              endif
              goto 10
          endif
c          enddo
        endif
C
C...gofwd,golft,gorgt,goback
C...goup, godown
C
          if (ityp.ne.2.or.ist.lt.5.or.ist.gt.9) then
              ierr=28
              go to 99998
          endif
c          if (sc(223) .eq. -10000) goto 99999
          if (lautgf .and. ist.eq.CURVE) then
            call gtdesc(tv,nclkey,nwds,ietype)
            call gtccnm (nclkey, nents)
            if (nents.gt.1) then
               do i=1,nents
                 call gtcent (nclkey, i, keyent, buf, ietype,ierr)
                 if (keyent.lt.0) keyent = -keyent
                 call cvallpv(keyent, ietype, sc, pv)
                 if (ietype.eq.CIRCLE) then
                   call ptdsc3(keyent,1,ietype,htv)
                   call gtentt(htv, trflg, keyent, ietype, cibuf)
                   if (f_mag(cibuf(8)).gt.0.0) then
                     d1 = f_dot(pv,cibuf(8)) - cibuf(11)
                     if (d1.lt.0.0d0) then
                       if (f_dist(buf(1),pv).lt.f_dist(buf(7),pv)) then
                         call vctovc(buf(1),pv)
                       else
                         call vctovc(buf(7),pv)
                       endif
                     endif
                   endif
                 endif
                 d1 = f_dist(sc, pv)
                 if (i.eq.1 .or. d1.lt.dhld) then
                   dhld  = d1
                   isvix = i
                   call conv8_8(pv,plend,6)
                 endif
               enddo
               call gtcent (nclkey, isvix, keyent, buf, ietype,ierr)
               if (ierr.eq.0) then
                 if (keyent.lt.0) keyent = -keyent
                 call ptdsc3(keyent,1,ietype,sc(11))
                 lgfacmp = .true.
                 htv = tv
               endif
            endif
          endif
          if (ist.eq.8 .or.ist.eq.9) then
            ierr = ist
            call parsuv(ierr,dsuv,dsu,dsv)
            if (ierr.gt.0) goto 99998
          endif
          call parsit
          leos = ityp.eq.7.or.(scalar.and.nextyp.eq.11)
          if (leos .and. .not.lgfacmp) then
c          if (ityp.eq.7.or.((ityp.eq.3.or.ityp.eq.4.or.
c     1              (ityp.eq.2.and.ist.eq.2)).and.nextyp.eq.11)) then
               if (ifl(276).eq.1) then
                 if (nxteos) goto 99999
                 goto 77777
               endif
C
C...its an implied check surface
C
 
               if (ifl(215).eq.0) then
C
C...if its the first, set ifl(215) to 1, notifying
C...driver not to call mocntl
C
                   ifl(215)=1
C
C...save the drive surface descriptor
C
                   sc(129)=sc(11)
C
C...save the motion type
C
                   ifl(217)=isc10(1)
C
C...save nline in case of error
C
                   call nclf_src_line_to_rec (nline-ifl(123)+1,ifl4(13))
                else
C
C...already in an implied c/s range.  set up
C...sc table for mocntl
C
                    ifl(215)=2
                    isc10(4)=isc10(1)
                    isc10(1)=ifl(217)
                    ifl(217)=isc10(4)
                    sc(13)=sc(11)
                    sc(11)=sc(129)
                    sc(129)=sc(13)
                    ksn(2)=1
                    ksn(3)=0
                    ksn(4)=0
C
C...determine the c/s state (on, to or past)
C
                    if (ifl(217).eq.704) then
C
C...gofwd - it must be tanto
C
                        ksn(1)=646
                    else if (ifl(219).eq.0) then
C
C...tlon  - it must be 'on'
C
                        ksn(1)=71
                    else if (ifl(219).eq.-1) then
C
C...tllft
C
                        if (ifl(217).eq.706) then
C
C...past
C
                            ksn(1)=715
                        else
C
C...to
C
                            ksn(1)=714
                        endif
                    else
C
C...tlrgt
C
                        if (ifl(217).eq.705) then
C
C...past
C
                            ksn(1)=715
                        else
C
C...to
C
                            ksn(1)=714
                        endif
                    endif
                    sc(12)=asn
                endif
                if (ityp.ne.7) then
C
C...its a tag on feed rate
C
                     go to 77777
                endif
           else
               ichk=0
               if (lgfacmp) then
                 do i=isvix+1,nents
                   call gtcent (nclkey, i, keyent, buf, ietype,ierr)
                   if (ierr.eq.0) then
                     ichk = ichk+1
                     if (keyent.lt.0) keyent = -keyent
                     call ptdsc3(keyent,1,ietype,htv)
                     scauto(ichk*2) = htv
                     asn = 0.d0
                     ksn(2) = 1
                     scauto(ichk*2-1) = asn
                     csauto(ichk) = .false.
                     nlkahd(ichk) = nlook
                     avauto(ichk) = llavoid
                     if (ichk.le. 5)  then
                       sc(10+ichk*2) = asn
                       sc(11+ichk*2) = htv
                       csflgs(ichk) = .false.
                     endif
                   endif
                 enddo
                 isc10(2) = ichk
                 lgfacmp = .false.
                 if (leos) goto 25
               endif
20             ichk=ichk+1
               avauto(ichk) = .false.
               if (ichk.le.5) avflgs(ichk) = .false.
               ksn(1)=0
c
c... if Auto Gofwd, see if it's one of the final muilti CS.
c
               if (lautgf) then
                  if (vocab.and.voc.eq.CHECK) then
                      lcheck = .true.
                      call parsit
                  endif
                  if (lcheck) then
                      nlstcs = nlstcs +1
                      nlkahd(ichk) = 0
c
c... if Auto Gofwd, get multi cs number (look ahead)
c
                  else if (vocab.and.voc.eq.LOOK) then
                      call parsit
                      if (.not.scalar) then
                        ierr = 7
                        goto 99998
                      endif
                      nlkahd(ichk) = tv
                      call parsit
                      if (nlkahd(ichk).lt.1 .or. nlkahd(ichk).gt.5)
     *                    nlkahd(ichk) = nlook
                      nlook = nlkahd(ichk)
                  else
                      nlkahd(ichk) = nlook
                  endif
              endif
c
c.. avoid/find
c
              if (vocab) then
                  if (voc.eq.AVOID) then
                      llavoid = .true.
                      call parsit
                  else if (voc.eq.FIND) then
                      llavoid = .false.
                      call parsit
                  endif
               endif
               avauto(ichk) = llavoid
               if (ichk.le.5) avflgs(ichk) = llavoid
C
C...to, past, tanto, on, or pstan (?)
C
               if (ist.eq.714.or.ist.eq.715.or.ist.eq.646.or.
     1             ist.eq.71.or.ist.eq.729) then
                   if (ist .eq. 729) then
                       call gtdesc (sc(35),nclkey,nwds,ietype)
                       if (ietype .eq. SURF) then
                           call sftype(nclkey,ietype)
                           if (ietype .eq. NETSF) then
                               ierr=556
                               go to 99998
                           endif
                       endif
                   endif
                   ksn(1)=ist
                   call parsit
               endif
               ksn(2)=1
               if (ityp.eq.3.or.ityp.eq.4.or.
     1             (ityp.eq.2.and.ist.eq.2)) then
C
C...n,intof
C
                   ksn(2)=tv
                   call parsit
                   if (ityp.ne.1.and.ist.ne.727) then
                       ierr=188
                       goto 99998
                   endif
                   call parsit
               endif
C
C...check sf
c...Don't allow NET surface with PSTAN
C
               if (ksn(1) .eq. 729) then
                   if (ist .eq. SURF) then
                       call gtdesc(tv,nclkey,nwds,ietype)
                       call sftype(nclkey,ietype)
                       if (ietype .eq. NETSF) then
                           ierr=556
                           go to 99998
                       endif
                   endif
               endif
c
c...Point CS
C
               if (ityp.ne.2.or.((ist.lt.5.or.ist.gt.9)
     x                       .and.ist.ne.POINT)) then
                   ierr=30
                   go to 99998
               endif
               if (ist.eq.POINT) then
                   if (ksn(1).ne.ON .and. ksn(1).ne.0) then
                       ierr = 460
                       goto 99998
                   endif
                   ksn(1) = ON
               else
                   if (ksn(1).eq.0.and. .not. lautgf) ksn(1) = TO
               endif
               nents = 0
               if (ist.eq.CURVE .and. lautgf .and. .not.lcheck) then
                  call gtdesc(tv,nclkey,nwds,ietype)
                  call gtccnm (nclkey, nents)
               endif
               if (nents.gt.0) then
                 do i=1,nents
                   call gtcent (nclkey, i, keyent, buf, ietype,ierr)
                   if (ierr.eq.0) then
                     if (keyent.lt.0) keyent = -keyent
                     call ptdsc3(keyent,1,ietype,htv)
                     scauto(ichk*2) = htv
                     scauto(ichk*2-1) = asn
cc                     scauto(ichk*2-1) = 0.0d0
                     csauto(ichk) = .false.
                     nlkahd(ichk) = nlook
                     avauto(ichk) = llavoid
                     if (ichk.le. 5)  then
                       sc(11+ichk*2) = htv
                       csflgs(ichk) = .false.
                     endif
                     ichk = ichk+1
                   endif
                 enddo
                 ichk = ichk - 1
               else
                 scauto(ichk*2) = tv
                 csauto(ichk) = .false.
                 if (ichk.le. 5)  then
                    sc(11+ichk*2) = tv
                    csflgs(ichk) = .false.
                 endif
               endif
 
               if (ist.eq.CURVE .or.ist.eq.SURF) then
                   ierr = ist
                   call parsuv (ierr,csauto(ichk),muauto(ichk),
     x                          mvauto(ichk))
                   if (ichk .le. 5) then
                       csflgs(ichk) = csauto(ichk)
                       mcsu(ichk) = muauto(ichk)
                       mcsv(ichk) = mvauto(ichk)
                   endif
 
                   if (ierr.gt.0) goto 99998
C
C...Added for OpenNCL integration
C...Bobby  -  9/16/97
C
                   if (ichk.eq.1) then
                       csuv = csflgs(1)
                       csu  = mcsu(1)
                       csv  = mcsv(1)
                   endif
               endif
               call parsit
               chkauto(ichk) = 0.0d0
               if (ichk .le. 5) chkpt(ichk) = 0.0d0
               if (ityp.eq.1 .and. ist.eq.NEARPT) then
                   call parsit
                   if (ityp.ne.2.or.ist.ne.POINT) then
                   ierr=20
                   goto 99998
               endif
               chkauto(ichk) = tv
               if (ichk .le. 5) chkpt(ichk) = tv
               call parsit
           endif
           ksn(3)=0
           ksn(4)=0
           if (ityp.eq.3) then
               if (ichk.gt.1 .or. nextyp.ne.11) then
                   svtv=tv
                   call vstchk
                   if (ityp.ne.2 .or. ist.ne.13) then
                       ityp=3
                       ist=0
                       tv=svtv
                   endif
               endif
           endif
           if (ityp.eq.2 .and. ist.eq.13) then
               call jmpchk
               if (ifl(2).gt.0) goto 99998
               ksn(3)=ktv(1)
               ksn(4)=ktv(2)
               call parsit
           endif
           if (nents .eq. 0) then
               scauto(ichk*2-1)=asn
               if (ichk.le.5) sc(10+ichk*2)=asn
           endif
 
           isc10(2)=ichk
C
C...check for tag on feedrat
C
           if ((ityp.eq.3.or.ityp.eq.4.or.ityp.eq.2.and.ist.eq.2)
     1         .and.nextyp.eq.11) goto 30
           if (ityp.ne.7 .and. ((.not. lautgf .and.ichk.lt.5)
     *         .or. (lautgf.and.ichk.lt.504))) goto 20
c
c.. Autogofwd set last cs number.
c
   25      if (lautgf) then
               if (nents.gt.0) then
                 cscond = CS_PAST
                 u4 = 0.
                 call crvevl(nclkey,u4,buf(4),buf)
                 u4 = 1.
                 call crvevl(nclkey,u4,buf(7),buf)
                 if (f_dist(buf(4),buf(7)) .lt. sc(27)) then
                   d1 = f_dist(buf(4),plend)
                   if (d1.gt.sc(27)*10.d0) then
                     do i=1,isvix
                       call gtcent (nclkey, i, keyent, buf, ietype,ierr)
                       if (ierr.eq.0) then
                         if (keyent.lt.0) keyent = -keyent
                         call ptdsc3(keyent,1,ietype,htv)
                         ichk = ichk+1
                         scauto(ichk*2) = htv
                         asn = 0.d0
                         ksn(2) = 1
                         scauto(ichk*2-1) = asn
                         csauto(ichk) = .false.
                         nlkahd(ichk) = nlook
                         avauto(ichk) = llavoid
                         if (ichk.le. 5)  then
                           sc(10+ichk*2) = asn
                           sc(11+ichk*2) = htv
                           csflgs(ichk) = .false.
                         endif
                       endif
                     enddo
                     call unitvc(plend(4),buf)
                     buf(4)  = f_dot(buf,plend)
                     token2   = '@PL'
                     ivxsub = 0
                     call vstchk
                     keyold  = keyhld
                     if (ist.eq.1) ist = 0
                     istold  = ist
                     savid2  = token2
                     isvsub = ivxsub
                     nwds    = 4
                     call ptgeom(PLANE,buf,keyent,nwds)
                     call ptdsc3(keyent,nwds,PLANE,htv)
                     rest    = htv
                     idst    = PLANE
                     call vstore
                     call blkgeo(keyent,1)
                     ierr = ifl(2)
                     cscond = CS_ON
                   else
                     i = 1
                     call gtcent (nclkey, i, keyent, buf, ietype,ierr)
                     if (ierr.eq.0) then
                       if (keyent.lt.0) keyent = -keyent
                       call ptdsc3(keyent,1,ietype,htv)
                     endif
                   endif
                 else
                   call unitvc(buf,buf)
                   buf(4)  = f_dot(buf,buf(7))
                   token2   = '@PL'
                   ivxsub = 0
                   call vstchk
                   keyold  = keyhld
                   istold  = ist
                   savid2  = token2
                   isvsub = ivxsub
                   nwds    = 4
                   call ptgeom(PLANE,buf,keyent,nwds)
                   call ptdsc3(keyent,nwds,PLANE,htv)
                   rest    = htv
                   idst    = PLANE
                   call vstore
                   call blkgeo(keyent,1)
                   ierr = ifl(2)
                 endif
                 if (ierr.ne.0) goto 99998
                 ichk = ichk+1
                 isc10(2) = ichk
                 scauto(ichk*2) = htv
                 asn = 0.d0
                 ksn(1) = cscond
                 ksn(2) = 1
                 scauto(ichk*2-1) = asn
                 csauto(ichk) = .false.
                 nlkahd(ichk) = 1
                 avauto(ichk) = llavoid
                 if (ichk.le. 5)  then
                   sc(10+ichk*2) = asn
                   sc(11+ichk*2) = htv
                   csflgs(ichk) = .false.
                 endif
                 j = ichk
                 do i=1,ichk-1
                   j = j-1
                   if (nlkahd(i).gt.j) nlkahd(i) = j
                 enddo
               endif
               if (lcheck) then
                   isc10(2) = ichk - nlstcs + 1
               else
                   nlstcs = 1
               endif
               nlkahd(isc10(2)) = nlstcs
           endif
  30       if (ifl(215).ne.0) then
C
C...end implied check range.
C
               ifl(215)=3
               isc10(4)=isc10(1)
               isc10(1)=ifl(217)
               ifl(217)=isc10(4)
               sc(161)=sc(12)
               asn=sc(13)
               sc(13)=sc(11)
               sc(11)=sc(129)
               sc(129)=asn
C
C...determine the c/s state (on, to or past)
C
               ksn(2)=1
               ksn(3)=0
               ksn(4)=0
C
C...gofwd - it must be tanto
C
               if (ifl(217).eq.704) then
                   ksn(1)=646
C
C...tlon  - it must be 'on'
C
               else if (ifl(219).eq.0) then
                   ksn(1)=71
C
C...tllft
C
               else if (ifl(219).eq.-1) then
C
C...past
C
                   if (ifl(217).eq.706) then
                       ksn(1)=715
C
C...to
C
                   else
                       ksn(1)=714
                   endif
C
C...tlrgt
C
               else
C
C...past
C
                   if (ifl(217).eq.705) then
                       ksn(1)=715
C
C...to
C
                   else
                       ksn(1)=714
                   endif
               endif
               sc(12)=asn
           endif
C
C...check for tag on feedrat
C
           if ((ityp.eq.3.or.ityp.eq.4.or.ityp.eq.2.and.ist.eq.2)
     1         .and.nextyp.eq.11) goto 77777
           endif
C
       else if (isc10(1).eq.703.or.isc10(1).eq.701) then
C
C...from - goto
C
           ifv    = 0
C
C...vp 19-apr-93 PV added next 7 lines
C
           if (ityp.eq.2.and.ist.eq.3 .or. ist.eq.21) then
cuni               call getent(sc(11),nwds,pg,el,ist)
               call gtentt(tv, trflg, nclkey, ietype, sc(11))
               if (ist .eq. 21) then
                   ifv = 1
                   isc10(2) = 2
               end if
               if (nextyp.eq.11) go to 88889
               call parsit
           else if (ityp.eq.2.and.ist.eq.20) then
C
C...goto/patern   epm  8-15-87
C
               call gotopn
               if (err) goto 99999
               goto 88889
           else if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4)
     1         then
               do 100 i=1,3
                   sc(i+10)=tv
                   if (nextyp.eq.11.and.i.gt.1) then
                       if (i.eq.2) sc(13)=sc(3)
                       go to 88889
                   endif
                   call parsit
                   if (i.eq.3) go to 105
                   if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3.and.
     1                 ityp.ne.4) then
                           ierr=7
                           go to 99998
                    endif
100            continue
               ierr=4
               go to 99998
           else
               ierr=33
               go to 99998
           endif
C
C...must be a vector or i,j,k
C
105        isc10(2)=2
           if (ityp.eq.2.and.ist.eq.4) then
C
C...vp 19-apr-93 if PV vector is not allowed, next 4 lines
C
               if (ifv .eq. 1) then
                   ierr = 7
                   go to 99998
               end if
cuni               call getent(sc(14),nwds,pg,el,ist)
               call gtentt(tv, trflg, nclkey, ietype, sc(14))
               go to 88888
           else if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.
     1             4) then
C
C...vp 19-apr-93 if PV get feed rate only, next line
C
               if (ifv .eq. 1) go to 77777
               do 150,i=1,3
                   sc(i+13)=tv
                   if (nextyp.eq.11) then
                       if (i.eq.1) then
C
C...feed rate
C
                           isc10(2)=1
                           go to 77777
                       else if (i.eq.3) then
                           go to 88889
                       endif
                   endif
                   call parsit
                   if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3.and.
     1                 ityp.ne.4) then
                       ierr=7
                       go to 99998
                   endif
150            continue
               go to 77777
           else
               ierr=7
               go to 99998
           endif
       else if (isc10(1).eq.710) then
C
C...godlta
C
           do 200 i=11,13
               if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.
     1             ityp.eq.4) then
                   sc(i)=tv
                   if (nextyp.eq.11) then
                       if (i.eq.11) then
                           isc10(2)=2
                       else
                           if (i.ne.12) then
                               isc10(3)=i-10
                           else
C
C...its a gd/z,fr
C
                               isc10(2)=2
                               isc10(3)=1
C                              call putcl(2000,1009,2,tv)
C...save feed rate in sc(123) epm 1-22-85
C
                               sc(123)=tv
                               call setfed (sc(123))
C
C...Do not call 'fedmut' here
C...because it does not always output
C...tagged feedrates if they are the same.
C...caused problems with INDEX and COPY.
C...Bobby  -  8/13/97
C
cc                               call fedmut (sc(123))
                               FEEDR = sc(123)
                               call putcl (2000,1009,2,tv)
                           endif
                       endif
                       go to 88889
                   endif
               else if (ityp.eq.2.and.(ist.eq.4.or.ist.eq.6.or.ist.eq.9
     1                  .or.ist.eq.21).and.i.eq.11) then
C
C...its godlta surf, plane or vector
C
                   isc10(2)=4
                   if (ist.eq.6.or.ist.eq.9) isc10(2)=3
                   sc(11)=tv
                   go to 88888
               else
                   ierr=7
                   go to 99998
               endif
               call parsit
200        continue
C
C...if it got here, it must be a 3 param gd with tag on fedrat
C
           isc10(3)=3
           isc10(2)=1
           go to 77777
C
       else if (isc10(1).eq.741) then
C
C...maxang
C
           if (.not. scalar) then
               ierr=7
               go to 99998
           endif
           d1 = tv
           if (.not. nxteos) then
             call parsit
             if (.not. vocab .or. ist.ne.ONCEV) then
                ierr = 61
                goto 99998
             endif
           else
             sc(201) = d1
           endif
           sc(80)=d1
           sc(10)=0
C
       else if(isc10(1).eq.712) then
C
C...indirp
C
           if (ityp.eq.2.and.ist.eq.3) then
cuni               call getent(sc(11),nwds,pg,el,ist)
               call gtentt(tv, trflg, nclkey, ietype, sc(11))
               go to 88888
           else if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3
     1         .or.ityp.eq.4) then
               do 300 i=1,3
                   sc(10+i)=tv
                   if (nextyp.eq.11.and.i.eq.3) then
                       go to 88889
                   endif
                   call parsit
                   if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3.and.
     1                 ityp.ne.4) then
                       ierr=7
                       go to 99998
                   endif
300            continue
               ierr=4
               go to 99998
           else if (ityp.eq.2.and.ist.eq.1) then
               ierr=9
               go to 99998
           else
               ierr=20
               go to 99998
           endif
C
       else if (isc10(1).eq.711) then
C
C...indirv
C
           if (ityp.eq.2.and.(ist.eq.4 .or. ist.eq.21)) then
cuni               call getent(sc(11),nwds,pg,el,ist)
               if (ist .eq. 4) then
                 call gtentt(tv, trflg, nclkey, ietype, sc(11))
               else
                 call gtentt(tv, trflg, nclkey, ietype, pv)
                 sc(11) = pv(4)
                 sc(12) = pv(5)
                 sc(13) = pv(6)
               end if
               go to 88888
           else if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3
     1         .or.ityp.eq.4) then
               do 400 i=1,3
                   sc(i+10)=tv
                   if (nextyp.eq.11.and.i.eq.3) then
                       go to 88889
                   endif
                   call parsit
                   if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3.and.
     1                 ityp.ne.4) then
                       ierr=7
                       go to 99998
                   endif
400            continue
               ierr=4
               go to 99998
           else if (ityp.eq.2.and.ist.eq.1) then
               ierr=9
               go to 99998
           else
               ierr=11
               go to 99998
           endif
 
       else if (isc10(1).eq.731) then
C
C...toler
C
           if (.not.scalar) then
               ierr=7
               go to 99998
           endif
           sc(11)=tv
           isc10(1)=731
           isc10(2) = 0
           if (.not.nxteos) then
             call parsit
             if (.not.scalar) then
               ierr=7
               go to 99998
             endif
             sc(12) = tv
             isc10(2) = 1
           endif
C
       else if (isc10(1).eq.713) then
C
C...psis
C
           if (ityp.eq.2.and.(ist.gt.4.and.ist.lt.10)) then
               sc(11)=tv
               if (ist.eq.9) then
                 ierr = 9
                 call parsuv(ierr,psuv,psu,psv)
                 if (ierr.gt.0) goto 99998
C
C...parse curve on ds parameters to process implied PS
C
               else if (ist .eq. 8) then
                 ierr = 8
                 call parsuv(ierr,psuv,psu,psv)
                 if (ierr.gt.0) goto 99998
               endif
               isc10(1)=713
           else
               ierr=36
               go to 99998
           endif
C
       else if (isc10(1).eq.716) then
C                                                        ********** cutter
C...Changed for expanded cutter definitions
C...Bobby  -  1/4/3
C
           ldtext = .true.
           do 430 i=11,19,1
               sc(i) = 0.
  430      continue
           do 450 i=1,9
               if (err) then
                   call error(ifl(2))
                   go to 99999
               endif
               lsc(i) = token2
               len = strlen1(token2)
               call nclf_savelsc (i,token2,len)
               if (lstrng) then
                   j=0
                   call gttext(txt,j)
 
c
c...lsc is global value define as 80x25 and not easily changed because
c....fortran data space already there
c...use CString to save them in order to easy to change later, but keep the lsc
c...as it is in order not to change the old logic
c
                   if (j.gt.79) then
                       lsc(i) = txt(1:77) // '&&'
                   else
                       lsc(i) = txt(1:j)
                   endif
                   call nclf_savelsc(i, txt, j)
                   sc(i+10) = -10000
               else if (scalar) then
                   sc(i+10)=tv
               else if (ityp .eq. 1) then
                   sc(i+10) = ist - 10000
               else if ((ityp .eq. 2 .and.
     1             (ist .eq. 1 .or. ist .eq. 14)) .or. ityp .eq. 8) then
                   call nclf_format_label (lsc(i),ivxsub,lsc(i),i0)
                   sc(i+10) = -10000
               else if (ityp .eq. 2) then
                   sc(i+10) = -10000 - ist
                   call nclf_format_label (lsc(i),ivxsub,lsc(i),i0)
c
c... cuter/dia,corner-rad,,,,angle,height for 7 param apt cutter
c
               else if (ityp .eq. 5 .and. ist.eq.9)then
                   sc(i+10) = 0
                   if (nextyp.eq.9.and. comma.eq.0) then
                     comma = 1
                     goto 450
                   endif
                   if (comma.eq.1)comma = 0
               else
                   ldtext = .false.
                   ierr=7
                   go to 99998
               endif
               if (nextyp.eq.11) then
                   ldtext = .false.
                   isc10(3)=i
                   go to 88889
               endif
               call parsit
450        continue
C
C...Store remainder of CUTTER/TOOL parameters
C
           if (sc(11) .eq. 617-10000 .or. sc(11) .eq. 876-10000) then
C
C...Change counter from 9,23  to 10,24 due to adding one more parameter
C
               do 455 i=10,24,1
                   lsc(i) = token2
                   if (nextyp .eq. 11) then
                       ldtext = .false.
                       isc10(3) = i
                       go to 88889
                   endif
                   call parsit
  455          continue
           endif
           ldtext = .false.
           ierr=4
           go to 99998
C
       else if(isc10(1).eq.717) then
C
C...Thick
C...with implied check surface, we can't let
C...mocntl handle thick any longer.  therefore,
C...it is now entirely handled here.   epm 2-9-85
C
 
           sc(10)=0
C
C...Set sc(177) thru sc(180) to zero to eliminate any
C...previous multiple thick statements.
C
           do 460, i=177,180
460            sc(i)=0
           if (ist.eq.72) then
C
C...Thick/off
C
               do 465,i=130,132
465                sc(i)=0.
           else
               do 500 i=130,132
                   if ((ityp.eq.2.and.ist.eq.2) .or.
     1                  ityp.eq.3.or.ityp.eq.4) then
                       sc(i)=tv
                   else
                       ierr=7
                       go to 99998
                   endif
 
C
C...Parse next token unless all 3 have been parsed
C
                   if (nextyp .ne. 11 .and. i .ne. 132) call parsit
500            continue
C
C...If nextyp is equal to 11 then we want to make sure that all
C...the values of sc(177) thru sc(180) are set equal to
C...to the default value (sc(132)).
C
               if (nextyp.eq.11) then
                  sc(177)=sc(132)
                  sc(178)=sc(132)
                  sc(179)=sc(132)
                  sc(180)=sc(132)
                  goto 503
               endif
C
C...An array for a multiple thick statement.
C...SC(177) thru SC(180) will hold the additional
C...CS values.
C
               do 502 i=177,180
                   if ((ityp.eq.2.and.ist.eq.2) .or.
     x               ityp.eq.3.or.ityp.eq.4) then
                      if (thflag.eq.0) then
                         call parsit
                         thflag=1
                      endif
                      sc(i)=tv
                   endif
                   if (nextyp.eq.11) then
                     if (i.ne.180) then
C
C...If multiple thick is being used but not all five
C...Check Surfaces are assigned values, we want to set
C...the unassigned values to the default value (sc(132)).
C
                       do 501, j=i,179
                          sc(j+1)=sc(132)
501                    continue
                     endif
                     goto 503
                   endif
 
                   if (nextyp .ne. 11 .and. i .ne. 180) call parsit
502            continue
503            continue
               call parsit
               if (ityp.ne.7) then
                   ierr=4
                   go to 99998
               endif
           endif
           if (ifl(215).eq.0) then
C
C...Placing the PS,DS,and CS values in SC(23) thru SC(25)
C
               do 510,i=23,25
510               sc(i)=sc(i+107)
           endif
C
       else if (isc10(1).eq.736) then
C
C...maxdp
C...defaults: maxdp/4,auto,10,.01,nowarn
C
           if((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4)then
               sc(54)=tv
               sc(10)=0.
               if (nextyp.eq.11) then
                 sc(105) = sc(54)
                 goto 99999
               endif
               llstep = lstep
               ASN=SC(162)
               ifl(44) = 0
               CALL PARSIT
               if (ityp.eq.1.and.ist.eq.STEP) then
                 if (nextyp.ne.9) then
                   ierr = 311
                   goto 99998
                 endif
                 call parsit
                 call parsit
                 if (ityp.ne.1.or.(ist.ne.ON.and.ist.ne.OFF)) then
                   ierr = 56
                   goto 99998
                 endif
                 llstep = ist.eq.ON
                 call parsit
                 IF (NEXTYP.NE.11) call parsit
               endif
               IF (ITYP.NE.7) THEN
C
C...OFF
C
                 IF (ITYP.EQ.1.AND.IST.EQ.72) THEN
                   ASN=0
                   call parsit
                   IF (NEXTYP.NE.11) call parsit
C
C...AUTO or place holding comma
C
                 else if (ityp .eq. 1 .and. ist .eq. 88 .or.
     1                    ityp .eq. 5 .and. ist .eq. 9) then
                   IF (NEXTYP.EQ.11) THEN
C
C...if last setting was "OFF", reset the defaults to
C...original defaults of 10 loops, 10 * TOLER and NOWARN
C
                     if (asn .eq. 0.) then
                       KSN(1)=10
                       BSN(2)=SC(27)*10.
                     endif
                   else
C
C...bump input character pointer if next token is a comma
C...and last token was not missing.
C
                     if (ist .ne. 9 .and. nextyp .eq. 9) inx=inx+1
C
C...look for max-loops value or place holding comma
C
                     CALL PARSIT
                     IF (.not.((ITYP .eq. 2 .and. IST .eq. 2) .or.
     1                         (ITYP .eq. 5 .and. IST .eq. 9) .or.
     2                          ITYP .eq. 3                   .or.
     3                          ITYP .eq. 4)) THEN
                       IERR=3
                       GOTO 99998
                     ENDIF
                     IF (ITYP .NE. 5) then
                       if (itv .gt. 0) then
                          KSN(1)=ITV
                       endif
                     endif
                     IF (NEXTYP.NE.11) THEN
C
C...look for min-dp value or place holding comma
C...bump input character pointer if next token is a comma
C...and last token was not missing.
C
                       if (ist .ne. 9 .and. nextyp .eq. 9) inx=inx+1
                       CALL PARSIT
                       IF (.not.((ITYP .eq. 2 .and. IST .eq. 2) .or.
     1                           (ITYP .eq. 5 .and. IST .eq. 9) .or.
     2                            ITYP .eq. 3                   .or.
     3                            ITYP .eq. 4)) THEN
                         IERR=3
                         GOTO 99998
                       ENDIF
                       if (ityp .ne. 5) then
                          if (tv .gt. 0.0) then
                               BSN(2)=TV
                          endif
                       endif
C
C...look for warn/nowarn token
C
                       if (nextyp.eq.9) call parsit
                       if (ityp.ne.7) call parsit
                       if (ityp.ne.7) then
                         if (ityp .eq. 1 .and.
     1                       (ist .eq. 852 .or. ist .eq. 853)) then
                           if (ist .eq. 852) then
                             ksn(2) = 1
                           else
                             ksn(2) = 0
                           endif
                           if (nextyp.eq.9) call parsit
                           if (ityp.ne.7) call parsit
                         endif
                       endif
                     ENDIF
                   ENDIF
                 ENDIF
               ENDIF
               if (ityp.ne.7) then
                 if (.not.nxteos) then
                    ierr = 61
                    goto 99998
                 endif
                 if (.not.vocab.or.ist.ne.ONCEV) then
                    sc(105) = sc(54)
                    sc(214) = asn
                    lstepv = llstep
                 endif
               else
                 sc(105) = sc(54)
                 sc(214) = asn
                 lstepv = llstep
               endif
             SC(162)=ASN
             lstep = llstep
           else
               ierr=7
               go to 99998
           endif
C
       else if (isc10(1).eq.737) then
C
C...numpts
C
           if (.not. scalar) then
               ierr=53
               go to 99998
           endif
           j = itv
           if (.not. nxteos) then
             call parsit
             if (.not. vocab .or. ist.ne.ONCEV) then
                ierr = 61
                goto 99998
             endif
           else
             ifl(368) = j
           endif
           ifl(91)=j
           sc(10)=0
C
C...disply or dispdb
C
       else if ((isc10(1).eq.1021) .or. (isc10(1).eq.1095) .or.
     1          (isc10(1).eq.1090)) then
 
          if (isc10(1).eq.1021 .or. isc10(1) .eq. 1095) then
              ifl(190)=0
          else
              call vxload
              sc(10)=0.0
          endif
           isc10(2)=ist
           sc(11)=tv
C
C...check for auto disply, if on get disply variables from ifl's
C
          if (ifl(135).eq.2) then
              isc10(2)=isc11(4)
              if (isc11(4).gt.5 .and. isc11(4).lt.9) then
                  sc(12)=ifl(136)
              else if (isc11(4).eq.9) then
                  call gtsfdp (nup,nupt,nvp,nvpt)
C                 sc(12)=ifl(137)
C                 sc(13)=ifl(138)
                  sc(12) = nup
                  sc(13) = nvp
                  sc(14) = nupt
                  sc(15) = nupt
              endif
              go to 99999
          endif
C
C...put name in saveid for labelling
C
c102706
          savid2=token2
          isvsub = ivxsub
C
C...visibl/geometry
C...implemented. kathy
C
          if (.not. (ist .eq. 1095 .or. ist.eq.1021)) go to 88888
             isc10(3) = 0
             isc10(2)=38
C            inx=isv
             sc10 = sc(10)
             call disply
             sc(10) = sc10
             if (ist.eq.71.or.ist.eq.72.or.ist.eq.88.or.ist.eq.144
     x           .or.ist.eq.617) goto 6099
C
C...disply/cutter
C
5099       if (.not.(ityp.eq.1.and.ist.eq.716)) go to 5199
             isc10(2)=19
C
C
C...Changed for NCL501+ mode
C...Paul - 03/17/92
C...Old version was:
C   if (ifl(35).eq.0) call dspcut
C
             if (ifl(35).eq.0 .or. (ifl(35).eq.2 .and. ifl(350).eq.1))
     x       call dspcut
             goto 99999
C
C...disply/axis,x,y,z
C...(does nothing in 502)
C
5199       if (.not.(ityp.eq.1.and.ist.eq.132)) go to 88889
               isc10(2)=21
               if (ifl(264).eq.0) then
                   sc(11)=1.
                   sc(12)=1.
                   sc(13)=1.
               else
                   sc(11)=25.4
                   sc(12)=25.4
                   sc(13)=25.4
               endif
               do 2100 i=1,3
                   if (nextyp.eq.11) goto 99999
                   call parsit
                   if ((ityp.eq.2.and.ist.eq.2).or.
     1                  ityp.eq.3.or.
     2                  ityp.eq.4) then
                     sc(i+10)=tv
                   else
                     ierr=3
                     go to 99998
                   endif
2100           continue
               if (nextyp.ne.11) then
                 ierr=4
                 goto 99998
               endif
               go to 99999
C
6099       continue
C
C...if not recognizable disply syntax send it to clfile as
C...post command
C
              ifl(44)=9
              ifrst=1
              do 2500 i=11,22
                  if (nextyp.eq.11 .and. ifrst.eq.0) then
                      numr8s=i-10
                      call putcl (2000,1021,numr8s,sc(11))
                      go to 99999
                  endif
                  if (ifrst.eq.0) then
                      if (nextyp.eq.9) then
                          call parsit
                      else
                          isvinx=inx
                          ierr=57
                          go to 99998
                      endif
                  endif
                  ifrst=0
 
C
C...if it is a vocabulary word store it in the low order
C...integer*2 for the post to be able to distinguish it
C...from a scalar value
C
                  if (ityp.eq.1) then
                      temp=0.
                      itemp(1)=ist
                      sc(i)=temp
                  else if ((ityp.eq.2 .and. ist.eq.2) .or.
     x                      ityp.eq.3. .or. ityp.eq.4) then
                      sc(i)=tv
                  else
                      ierr=7
                      go to 99998
                  endif
2500          continue
              ierr=115
              go to 99998
6199      continue
      endif
 
88888  if (.not.(isc10(1).gt.700.and.isc10(1).lt.711.and.
     1     nextyp.ne.11)) go to 8299
C
C...it may be a tag on feed rate
C
           call parsit
77777      if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.ist.eq.2)) then
               if (ifl(215).eq.3.or.ifl(215).eq.2) then
C
C...it's a motion statement that ends an implied
C...check surface range.  don't output the feed
C...rate yet.  driver will do it after it completes
C...the previous move.     epm    5-29-85
C
C...store the feed rate in the second r*4 of sc(133)
C...epm  1-7-86
C
C...set the rapid feed rate flag back to false
C...so the motion color would get reset. kathy
C
                   rpfron = .false.
                   isc133(1)=1
                   isc133(2)=0
                   r4133(2)=tv
               else
C
C...set the rapid feed rate flag back to false
C...so the motion color would get reset. kathy
C
                   rpfron = .false.
C                   call putcl(2000,1009,2,tv)
                   sc(123)=tv
                   call setfed (sc(123))
Cc                   call fedmut (sc(123))
                   FEEDR = sc(123)
                   call putcl (2000,1009,2,tv)
               endif
           else
               ierr=4
               go to 99998
           endif
8299   continue
       if (nextyp.ne.11) then
           ierr=4
           go to 99998
       endif
88889  go to 99999
 
99998  call error (ierr)
       ifl(21)=isvist
99999  continue
cc 99999  call uw_newline_flush()
       return
       end
 
