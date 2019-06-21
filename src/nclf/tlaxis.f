C*********************************************************************
C*    NAME         :  tlaxis.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       tlaxis.f , 26.14
C*    DATE AND TIME OF LAST  MODIFICATION
C*       06/17/19 , 14:07:44
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine tlaxis
c*      this routine handles the parsing and syntax checking for
c*      tlaxis statements. it builds the syscom in preparation for
c*      the motion generation section in the following format:
c*
c*          sc(10)
c*                first word=721
c*                                tlaxis/a,b,c       isc10(2) gets 1
c*                                tlaxis/ve1         isc10(2) gets 1
c*                                tlaxis/same        isc10(2) gets 2
c*                                tlaxis/normal,ps   isc10(2) gets 2
c*                                tlaxis/aa,30,ps    isc10(2) gets 3
c*                                ta/tanto,ds,1      isc10(2) gets 4
c*                                ta/tanto,ds,1,fan  isc10(2) gets 5
c*                                ta/tanto,ds,1,perpto,ve1    gets 6
c*                                ta/tanto,ds,1,parelm        gets 7
c*                                ta/normal,ps,perpto,ve1     gets 8
c*                                ta/combin,1,.5,.25          gets 9
c*                                ta/combin,1,parelm,.5,.25   gets 10
c*                                ta/a,b,c,normal             gets 1,isc10(4)=1
c*                                ta/ve1,normal               gets 1,isc10(4)=1
c*                                ta/same,normal              gets 1,isc10(4)=1
C*                                TLAXIS/AA,A1,PS,PERPTO,VE1  ISC10(2) GETS 11
C*                                TLAXIS/AA,A1,PS,CLDIST,D1   ISC10(2) GETS 12
C*                                      "        ,PERPTO,VE1  ISC10(2) GETS 13
C*                                ta/THRU,pt1                 isc10(2) gets 14
C*                                ta/INTERP,ve1               isc10(2) gets 15
c*
c*                any tlaxis statement may have a modify clause at the
c*                end that specifies the parameters by which to modify the
c*                output point. the syntax is:
c*
c*                      tlaxis/ve1,modify,a,b,c,d,e
c*
c*                         a = amount of offset to the right
c*                         b = amount of offset in the forward direction
c*                         c = amount of offset in the up direction
c*                         d = angle of tilt to the right
c*                         e = angle of tilt in the forward direction
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
       subroutine tlaxis

      include 'com8a.com'
      include 'gidcom.com'

      real*4 asc(400)
      equivalence (sc,asc)

      real*8 spshld,tlkhld1,tlkhld2
      real*4 ut,vt
      integer*2 i,ierr, ktmod, ktlock, ktrad
      integer*4 nclkey
      integer*2 nwds,ietype
      logical lgide, lchk, lflg, ltsec, prpfl
      integer*2 THRU, NOW, MODIFY, RIGHT, FWD, PERPTO, GUIDE
      integer*2 TLLFT, TLON, TLRGT, CONTCT, OFFSET, ON, OFF
      integer*2 GOUGCK, INTERP, CLDIST, PS, LOCK, FAN, OMIT, LAST
      integer*2 RETAIN, ENDV, LINEAR, RADIUS, AUTO, CENTER, SMOOTH
      parameter (THRU   = 152)
      parameter (NOW    = 161)
      parameter (MODIFY = 732)
      parameter (RIGHT  = 24)
      parameter (FWD    = 651)
      parameter (PERPTO = 630)
      parameter (GUIDE  = 865)
      parameter (TLLFT  = 719)
      parameter (TLON   = 718)
      parameter (TLRGT  = 720)
      parameter (CONTCT = 856)
      parameter (OFFSET = 666)
      parameter (ON     = 71)
      parameter (OFF    = 72)
      parameter (GOUGCK = 838)
      parameter (INTERP = 752)
      parameter (CLDIST = 1077)
      parameter (PS     = 728)
      parameter (LOCK   = 114)
      parameter (FAN    = 723)
      parameter (OMIT   = 172)
      parameter (ENDV   = 499)
      parameter (RETAIN = 329)
      parameter (LINEAR = 76)
      parameter (RADIUS = 23)
      parameter (AUTO   = 88)
      parameter (CENTER = 634)
      parameter (SMOOTH = 1085)
      parameter (LAST   = 52)
      
      !spshld   = sc(195)
      !ktlock  = ifl(365)
      !tlkhld1 = sc(196) 
      !tlkhld2 = sc(197) 
      !tlkhld1 = sc(198)
      !ktmod = ifl(364) 
      !ktrad = ifl(366)

c                                                        ********** tlaxis
      !lgide  = .false.
      !lchk   = .false.
      !ltsec  = .false.
      !prpfl  = .false.
      !ktlock = -1
      !ifl(289) = 0
      
c                                                        ********** tlaxis
c...we should not initial to false, but the current value, initial here will reset value we set before
c...Yurong
      lgide  = .false.
      lchk   = .false.
      !ltsec  = .false.
      !prpfl  = .false.
      !lgide = lcvgid
      !lchk = ldschk
!c...for secondary-ps, need reset to false. Yurong
!c...ltsec = lsecps
      ltsec  = .false.
      prpfl = prplst
c...flag value should not initial here, initial here will reset value we set before
c...yurong
      !ktlock = -1
      !ifl(289) = 0
      
      sc(10) = 0.0d0
      isc10(1)=ist
      ifl(44)=9
      idtype = -1
      call parsit
      if (ityp.eq.1.and.(ist.eq.MODIFY.or.ist.eq.RIGHT
     1   .or.ist.eq.FWD.or.ist.eq.GUIDE.or.ist.eq.GOUGCK
     2   .or.ist.eq.LOCK)) go to 110

       isc10(2)=1
       isc10(4)=0

           if (ist.eq.730.or.
     1         (ityp.eq.3.and.tv.eq.1 .and. nextyp .eq. 11)) then
c                                                  *** tlaxis/same or tlaxis/1
               if (ifl(304).eq.1) goto 9158
               sc(11)=0.
               isc10(2)=2
           else if (ist.eq.1071) then
c                                                  *** combin
               if (ifl(304).eq.1) goto 9158
               isc10(2)=9
               ifl(295) = 0
               call parsit
               if ((ityp.eq.2.and.ist.eq.2.).or.ityp.eq.3.
     1            .or.ityp.eq.4) then
                  sc(11)=tv
                  call parsit
               else
                  call error(7)
                  go to 99999
               endif
               if (VOCAB.and.ist.eq.PS) then
                   call parsit
                   if (.not.geom .or. (geotyp.ne.SURF.and.
     1                 geotyp.ne.PLANE)) goto 9126
                   ltsec = .true.
                   spshld = tv
                   if (nxteos) goto 99999
                   call parsit
               endif
               if (ityp.eq.1.and.ist.eq.833) then
c                                       **** parelm
                     if (ltsec) goto 9126
                     isc10(2)=10
                     call parsit
               endif
               if (ityp.eq.1.and.ist.eq.CENTER) then
                 call parsit
                 if (ityp.ne.1) goto 9200
                 if (ist.eq.ON) then
                   ifl(289) = 1
                 else if (ist.eq.AUTO) then
                   ifl(289) = 2
                 else if (ist.ne.OFF) then
                   goto 9200
                 endif
                 call parsit
               endif
               if ((ityp.eq.2.and.ist.eq.2.).or.ityp.eq.3.
     1            .or.ityp.eq.4) then
                  sc(12)=tv
                  call parsit
               else
                  call error(7)
                  go to 99999
               endif
               if ((ityp.eq.2.and.ist.eq.2.).or.ityp.eq.3.
     1            .or.ityp.eq.4) then
                  sc(13)=tv
                  call parsit
               else
                  sc(13)=sc(12)
               endif
c
c..... smooth interpolation for fan
c
               if (VOCAB.and.ist.eq.SMOOTH) then
                 ifl(295) = 5
                 asc(280) = 1
                 call parsit
                 if (scalar) then
                   ifl(295) = tv
                   call parsit
                   if (.not.scalar) goto 9200
                   if (tv.lt.0.0001 .or. tv.gt.1) tv = 1
                   if (ifl(295).lt.2) then
                     ifl(295) = 0
                     asc(280) = 0
                   else
                     asc(280) = tv
                   endif
                   call parsit
                 endif
               endif
c                       if its a modify
              if (ityp.eq.1.and.(ist.eq.MODIFY.or.ist.eq.RIGHT
     1            .or.ist.eq.FWD.or.ist.eq.GUIDE
     2            .or.ist.eq.GOUGCK.or.ist.eq.LOCK)) go to 110
              if (ityp.ne.7) then
                  call error(4)
                  go to 99999
              endif
           else if (ist.eq.820.or.ist.eq.39) then
c                                                  *** normal,ps or normps
               if (ifl(304).eq.1) goto 9158
               if (ist.eq.39) goto 100
               call parsit
               if (ist.ne.728) then
                 ierr=38
                 go to 99998
               endif
  100          sc(11)=1.
               isc10(2)=2
               if (nextyp.ne.11) then
                   call parsit
c                                                  ***  normal,ps,secondary-ps
                   if (geom .and. (geotyp.eq.SURF.or.
     1                 geotyp.eq.PLANE)) then
                     ltsec = .true.
                     spshld = tv
                     if (nxteos) goto 99999
                     call parsit
                   endif
c                                                  ***  normal,ps,pe,ve1
                   if (ityp.ne.1.or.(ist.ne.PERPTO.and.ist.ne.MODIFY
     1               .and.ist.ne.RIGHT.and.ist.ne.FWD.and.ist.ne.GUIDE
     2               .and.ist.ne.GOUGCK.and.ist.ne.LOCK)) goto 9198
                   if (ist.eq.PERPTO) then
c                              its a perpto
                       call parsit
c                                                  ***  normal,ps,pe,ve1,last
                       if (ityp .eq. 1 .and. ist .eq. LAST) then
                           prpfl = .true.
                           call parsit
                       endif

                       if (ityp.eq.2.and.(ist.eq.4.or.ist.eq.21)) then
                           isc10(2)=8
                           sc(12)=tv
                       else
                           ierr=11
                           go to 99998
                       endif
                   else
c                        ** its a modify,right, fwd or guide
                       go to 110
                   endif
               endif
           else if (ityp.eq.1.and.ist.eq.1) then
c                              ****  atangl
               if (ifl(304).eq.1) goto 9158
               sc(14) = 0.
               call parsit
               if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4)
     1             then
                   sc(11)=tv
                   call parsit
                   if (ist.ne.728) then
                       call error(195)
                       go to 99999
                   endif
                   ISC10(2)=3
                   IF (NEXTYP.EQ.11) GOTO 99999
                   CALL PARSIT
                   if (geom .and. (geotyp.eq.SURF.or.geotyp.eq.PLANE))
     1                 then
                     ltsec = .true.
                     spshld = tv
                     if (nxteos) goto 99999
                     call parsit
                   endif
                   IF (ITYP.EQ.1.AND.IST.EQ.CLDIST) THEN
C                                            CLDIST
                     CALL PARSIT
                     IF ((ITYP.EQ.2.AND.IST.EQ.2).OR.ITYP.EQ.3.OR.
     X                       ITYP.EQ.4) THEN
                       ISC10(2)=12
                       SC(13)=TV
                       IF (NEXTYP.EQ.11) GOTO 99999
                       CALL PARSIT
                     ELSE
                       CALL ERROR(7)
                       GOTO 99999
                     ENDIF
                   ENDIF
                   if (ityp.eq.1 .and. ist.eq.856) then
c                                      contact mode
                     sc(14) = 1.
                     if (nxteos) goto 99999
                     call parsit
                   endif
                   IF (ITYP.ne.1) goto 9200
                   IF (IST.ne.PERPTO) goto 110
C                                          PERPTO,VE1
                     CALL PARSIT
c                                          PERPTO,ve1,LAST
                     if (ityp .eq. 1 .and. ist .eq. LAST) then
                       prpfl = .true.
                       call parsit
                     endif
                     if (ityp.ne.2.or.ist.ne.4.and.ist.ne.21) then
                       CALL ERROR(11)
                       GOTO 99999
                     ENDIF
                     SC(12)=TV
                     IF (ISC10(2).EQ.3) ISC10(2)=11
                     IF (ISC10(2).EQ.12) ISC10(2)=13
               else
                   call error(7)
                   go to 99999
               endif
           else if (ityp.eq.1.and.ist.eq.646) then
c                              ***  tanto
               if (ifl(304).eq.1) goto 9158
               call parsit
               if (ist.ne.729) then
                   call error(196)
                   go to 99999
               endif
               call parsit
               if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4)
     1             then
                   sc(11)=tv
                   isc10(2)=4
               else
                   call error(7)
                   go to 99999
               endif
               if (nextyp.ne.11) then
                   call parsit
                   if (VOCAB.and.ist.eq.PS) then
                     call parsit
                     if (.not.geom .or. (geotyp.ne.SURF.and.
     1                   geotyp.ne.PLANE)) goto 9126
                     ltsec = .true.
                     spshld = tv
                     if (nxteos) goto 99999
                     call parsit
                   endif
                   if (ityp.eq.1.and.ist.eq.723) then
c                                              ****** fan
                       if (ltsec) goto 9126
                       isc10(2)=5
                       ifl(295) = 0
                   else if (ityp.eq.1.and.ist.eq.630) then
c                                              ****** perpto vector
                       call parsit
c                                              ****** perpto,ve1,last
                       if (ityp .eq. 1 .and. ist .eq. LAST) then
                           prpfl = .true.
                           call parsit
                       endif
                       if (ityp.eq.2.and.(ist.eq.4.or.ist.eq.21)) then
                           sc(12)=tv
                           isc10(2)=6
                       else
                           call error(11)
                           go to 99999
                       endif
                   else if (ityp.eq.1.and.ist.eq.833) then
c                                              ****** parelm
                       if (ltsec) goto 9126
                       isc10(2)=7
                   else if (ityp.eq.1.and.(ist.eq.MODIFY.or.ist.eq.RIGHT
     1                 .or.ist.eq.FWD.or.ist.eq.GUIDE
     2                 .or.ist.eq.GOUGCK.or.ist.eq.LOCK)) then
c                               ** modify, right or fwd
                       go to 110
                   else
                       call error(198)
                       go to 99999
                   endif
               endif
           else if (ityp.eq.1.and.ist.eq.THRU) then
             isc10(2) = 14
             isc10(4) = 0
             call parsit
             if (.not. scalar .and.
     1         (ityp.ne.2 .or. ist.ne.3.and.ist.ne.21.and.ist.ne.5
     2         .and.ist.ne.7.and.ist.ne.8)) goto 9020
             if (.not. scalar) then
               if (ist.eq.3 .or. ist.eq.21) then
                 call gtgeom (tv, sc(11), nclkey, nwds, ietype)
               else
                 sc(11) = tv
                 isc10(4) = 1
                 sc(12) = 0.0d0
                 if (.not.nxteos) then
                   call parsit
                   if (.not.scalar) goto 9004
                   sc(12) = tv
                 endif
               endif
             else
               sc(11) = tv
               do 510 i=12,13
                 call parsit
                 if (.not.scalar) goto 9007
                 sc(i) = tv
510            continue
             endif
           else if (VOCAB.and.ist.eq.INTERP) then
             isc10(2) = 15
             ifl(295) = 0
             isc10(4) = 0
             call parsit
             if (.not. scalar .and.
     1         (.not.GEOM .or. ist.ne.VECTOR.and.ist.ne.21)) goto 9011
             if (.not. scalar) then
               call gtgeve(tv, sc(11), nclkey,nwds,ietype)
             else
               sc(11) = tv
               sc(13) = 0.0d0
               do 520 i=12,13
                 if (i.eq.12 .or. .not.nxteos) then
                   call parsit
                   if (.not.scalar) goto 9007
                   sc(i) = tv
                 endif
520            continue
             endif
             sc(14) = 0.0d0
             if (.not.nxteos) then
               call parsit
               if (scalar) then
                 sc(14) = tv
                 if (.not.nxteos) call parsit
               endif
               if (VOCAB.and.ist.eq.SMOOTH) then
                 ifl(295) = 5
                 asc(280) = 1
                 call parsit
                 if (scalar) then
                   ifl(295) = tv
                   call parsit
                   if (.not.scalar) goto 9200
                   if (tv.lt.0.0001 .or. tv.gt.1) tv = 1
                   if (ifl(295).lt.2) then
                     ifl(295) = 0
                     asc(280) = 0
                   else
                     asc(280) = tv
                   endif
                 endif
               else
                 goto 9004
               endif
             endif
           else if (ityp.eq.2.and.(ist.eq.4.or.ist.eq.21)) then
               call gtgeve(tv, sc(11), nclkey,nwds,ietype)
           else if (scalar) then
               sc(11) = tv
               do 600 i=12,13
                   call parsit
                   if (.not.scalar) goto 9007
                   sc(i) = tv
600            continue
           else
               if (ityp.eq.2.and.ist.eq.1) then
                   ierr=9
               else
                   ierr=126
               endif
               go to 99998
           endif
      if (nextyp.eq.11) go to 99999
c
c... Check for NORMAL
c
           call parsit
           if (ityp.eq.1.and.ist.eq.820) then
             if (ifl(304).eq.1) goto 9158
             if (isc10(2).ne.1 .and.
     x          (isc10(2).ne.2.and.sc(11).ne.0.)) goto 9200
             isc10(4)=1
             if (nextyp.eq.11) goto 99999
             call parsit
           endif

c                                      ****  modify, right, fwd or guide
110        sc(20) = 0.0d0
           sc(21) = 0.0d0
112        if (ityp.ne.1) goto 9200
           if (ist.eq.MODIFY) goto 200
           if (isc10(2) .eq. 1 .or.
     1        (isc10(2) .eq. 2 .and. sc(11) .ne. 1.) .or.
     2        (isc10(2) .eq. 0 .and. ifl(23) .eq. 0)) go to 9126
           if (ist.eq.GUIDE) goto 120
           if (ist.eq.GOUGCK) goto 140
           if (ist.eq.LOCK) goto 150
           if (ist.eq.CENTER) goto 160
           if (ist.eq.smooth) goto 170
           if (ist.ne.RIGHT.and.ist.ne.FWD) goto 9200
c
c...   RIGHT or FWD
c
115        isc10(3)=2
           i = 20
           if (ist.eq.FWD) i = 21
           call parsit
           if (.not.scalar) goto 9007
           sc(i)=tv
           if (nxteos) goto 99999
           call parsit
           goto 112
c
c...   GUIDE
c
120        call parsit
c
c...add OFF to reset all guide value
c...Yurong
c
           if (ityp.eq.1) then
               if (ist.eq.OFF) then
                  gidasw = 0.0
                  gidh = 0.0
                  gidsid = 0
                  gidthk = 0.0d0
                  gthkon = 0
                  lgide  = .false.
                  ltltct = .false.
                  if (nxteos) goto 99999
                  call parsit
                  goto 112
               else 
                  goto 9021
               endif
           endif           
           if (ityp.ne.2.or.ist.ne.lINE.and.ist.ne.CIRCLE.and.
     1         ist.ne.CURVE) goto 9021
           gidasw = tv
           if (ist.eq.CURVE) then
             gidu0 = .5
             giduon = 0
             ierr = 8
             call parsuv(ierr,lflg,ut,vt)
             if (ierr.gt.0) goto 99998
             if (lflg .and. sc(169).ge.9.25) then
               giduon = 1
               gidu0 = ut
               gidu = gidu0
             endif
           endif
           gidh = 0.0
           gidsid = 0
           gidthk = 0.0d0
           gthkon = 0
           lgide  = .true.
           ltltct = .false.
           if (nxteos) goto 99999
           call parsit
           if (ityp.eq.1) then
             if (ist.eq.MODIFY) goto 200
             if (ist.eq.RIGHT.or.ist.eq.FWD) goto 115
             if (ist.eq.GOUGCK) goto 140
             if (ist.eq.CONTCT.or.ist.eq.OFFSET) then
               ltltct = ist.eq.CONTCT
               if (nxteos) goto 99999
               call parsit
             endif
             if (ityp.eq.1) then
               if (ist.eq.MODIFY) goto 200
               if (ist.eq.RIGHT.or.ist.eq.FWD) goto 115
               if (ist.eq.GOUGCK) goto 140
               if (ist.eq.TLLFT) then
                 gidsid = 1
               else if (ist.eq.TLON) then
                 gidsid = 2
               else if (ist.eq.TLRGT) then
                 gidsid = 3
               else
                 goto 9007
               endif
               if (nxteos) goto 99999
               call parsit
             endif
             if (ityp.eq.1) goto 112
           endif
           if (.not.scalar) goto 9007
           gidthk = tv
           gthkon = 1
           if (nxteos) goto 99999
           call parsit
           goto 112
c
c...   GOUGCK
c
140        call parsit
           if (ityp.ne.1) goto 9200
           if (ist.eq.ON) then
             lchk = .true.
           else if (ist.eq.OFF) then
             lchk = .false.
           else 
             goto 9200
           endif
           if (nxteos) goto 99999
           call parsit
           goto 112
c
c...   LOCK
c
150        continue
           call parsit
           if (vocab .and. ist.eq.OFF) then
             ktlock = 0
             if (nxteos) goto 99999
             call parsit
             goto 112
           endif
           if (vocab .and. ist.eq.ENDV) then
             if (ifl(365) .gt. 0) ktlock = 3
             if (nxteos) goto 99999
             call parsit
             goto 112
           endif
           if (.not.scalar) goto 9007
           ktlock = 2
           tlkhld1 = tv
           tlkhld2 = 0.0d0
           ktmod = ifl(364)
           ktrad = ifl(366)
           if (nxteos) goto 99999
           call parsit
           if (vocab .and. (ist.eq.LINEAR.or.ist.eq.RADIUS)) then
             ktrad = 0
             if (ist.eq.RADIUS) ktrad = 1
             if (nxteos) goto 99999
             call parsit
           endif
           if (scalar) then
             tlkhld2 = tv
             if (nxteos) goto 99999
             call parsit
           endif
           if (tlkhld2.gt.0.0d0) then
             if (vocab .and. (ist.eq.INTERP.or.ist.eq.FAN)) then
               ktmod = 0
               if (ist.eq.FAN) ktmod = 1
               if (nxteos) goto 99999
               call parsit
             endif
           endif
           if (vocab .and. (ist.eq.OMIT.or.ist.eq.RETAIN)) then
             if (ist.eq.OMIT) ktlock = 1
             if (nxteos) goto 99999
             call parsit
           endif
           goto 112
c
c...   CENTER
c
160        if (isc10(2).ne.5 .and. isc10(2).ne.9 .and. isc10(2).ne.10)
     *       goto 9200     
           call parsit
           if (ityp.ne.1) goto 9200
           if (ist.eq.ON) then
             ifl(289) = 1
           else if (ist.eq.AUTO) then
             ifl(289) = 2
           else if (ist.ne.OFF) then
             goto 9200
           endif
           if (nxteos) goto 99999
           call parsit
           goto 112
c
c...   smooth
c
170        if (isc10(2).ne.5 .and. isc10(2).ne.9 .and. isc10(2).ne.10)
     *       goto 9200     
           ifl(295) = 5
           asc(280) = 1
           call parsit
           if (scalar) then
             ifl(295) = tv
             call parsit
             if (.not.scalar) goto 9200
             if (tv.lt.0.0001 .or. tv.gt.1) tv = 1
             if (ifl(295).lt.2) then
               ifl(295) = 0
               asc(280) = 0
             else
               asc(280) = tv
             endif
           endif
           if (nxteos) goto 99999
           call parsit
           goto 112
c
c...   MODIFY
c
200        isc10(3)=isc10(3)+1
           do 300 i=15,19
               if (ityp.ne.7) call parsit
               if (ityp.ne.7) then
                 if (.not.scalar) goto 9007
                 sc(i)=tv
               else 
                 sc(i)=0.
               endif
300        continue
c
c...if all modify value is 0, set the modify flag ifl(104) to 0
c...Yurong
c
          if ((sc(15).eq.0.0).and.(sc(16).eq.0.0).and.(sc(17).eq.0.0)
     x        .and.(sc(18).eq.0.0).and.(sc(19).eq.0.0)) then
              ifl(104) = 0
          endif           

      if (nextyp.eq.11) goto 99999
      goto 9004
c                      Error - end of statement expected
9004  ierr = 4
      goto 99998
c                      Error - scalar expected
9007  ierr = 7
      goto 99998
c                      Error - vector expected
9011  ierr = 11
      goto 99998
c                      Error - point expected
9020  ierr = 20
      goto 99998
c                      Error - curve expected
9021  ierr = 21
      goto 99998
c                      Error - Tool axis mode not implemented.
9126  ierr = 126
      goto 99998
c                      Error - not allowed in 3-axis version
9158  ierr = 158
      goto 99998
c                      Error - perpto, fan or modify expected
9198  ierr=198
      go to 99998
c                      Error - modify or end of statement expected
9200  ierr=200
      go to 99998

99998  call error (ierr)
       return

99999  lcvgid = lgide
       ldschk = lchk
       lsecps = ltsec
       prplst = prpfl
       if (lsecps) sc(195) = spshld
       if (ktlock .gt. -1) ifl(365) = ktlock
       if (ktlock.eq.1 .or. ktlock.eq.2) then
         if (tlkhld1 .lt.sc(27)) ifl(365) = 0
         sc(196) = tlkhld1
         sc(197) = tlkhld2
         sc(198) = tlkhld1
         ifl(364) = ktmod
         ifl(366) = ktrad
       endif
       return
       end
