C**
C** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
C**
C*************************************************************************
C                          RMILL
C                          *****
C      THIS ROUTINE PARSES THE RMILL STATEMENT.
C      THE VALID SYNTAX AND SC(10) ASSIGNMENTS ARE:
C
C          RMILL/PS,                                                 $
C                [,TO/ON/PAST],CK1[,TO/ON/PAST],CK2                  $
C                [,TO/ON/PAST],DS1[,TO/ON/PAST],DS2                  $
C                ,MOTION-TYPE<,CLEARANCE-PLANE/DIST,PLUNGE-DIST>     $
C                ,STEPOVER-TYPE,STEPOVER-DIST,                       $
C                ,GENERAL-FEDRAT,POSITION-FEDRAT,PLUNGE-FEDRAT       $
C                [,RETRACT-PLANE/POINT/DIST]
C
C       SC(11)   PS        -  SURF TO BE RMILLED (PART SURFACE) SF OR PL.
C       ISC22(3) <MODIFIER>-  HOLDS THE CS1 REL (ON,TO,PAST) DEFAULT "ON".
C       SC(12)   CK1       -  BOUNDARY ITEM, MAY BE A LN,CI,PL,CV,OR SF.
C       ISC22(4) <MODIFIER>-  HOLDS THE CS2 REL (ON,TO,PAST) DEFAULT "ON".
C       SC(13)   CK2       -  BOUNDARY ITEM, MAY BE A LN,CI,PL,CV,OR SF.
C       ISC22(1) <MODIFIER>-  HOLDS THE DS1 REL (ON,TO,PAST) DEFAULT "ON".
C       SC(14)   DS1       -  LN OR PL WHICH CONTROLS THE START AND DIRECTION
C                              OF THE CUTTING PATH.
C       ISC22(2) <MODIFIER>-  HOLDS THE DS2 REL (ON,TO,PAST) DEFAULT "ON".
C       SC(15)   DS2       -  LN OR PL WHICH CONTROLS THE END OF THE CUT PATH.
C       ISC10(2) MOTION-TYPE- CUTTING MODE: 1=LACE, 2=BACK AND FORTH.
C       SC(16)   CLEARANCE-PLANE/DIST - CLEARANCE PLANE OR DISTANCE
C                             CAN AND MUST ONLY BE SPECIFIED FOR LACE MOTION-TYPE.
C       SC(17)   PLUNGE-DIST - THE PLUNGE DIST 
C                             CAN AND MUST ONLY BE SPECIFIED FOR LACE MOTION-TYPE.
C       ISC10(3) STEPOVER-MODE - TYPE OF STEPOVER BETWEEN PASSES
C                                 1 = FIXED STEPOVER DISTANCE
C                                 2 = STEPOVER BASED UPON SCALLOP HEIGHT 
C       SC(18)   STEPOVER-DIST - SPECIFIES EITHER STEP-OVER DIST OR SCALLOP
C                             HEIGHT BASED UPON STEP-OVER MODE.
C       RSC19(1) GENERAL-FEDRAT - GENERAL MACHINING FEED RATE.
C       RSC19(2) POSITION-FEDRAT - POSITIONING FEEDRATE.
C       RSC19(3) RAMPING-FEDRAT - RAMPING FEEDRATE.
C       SC(21)   RETRACT-PLANE/POINT/DIST - RETRACT PLANE, POINT OR DISTANCE
C                                           (OPTIONAL)
C
C       EXAMPLE: 
C         RMILL/PS1,CV1,LN1,LN2,PL2,1,PLZ,.125,1,.5,30,RAPID,10
C
C       SC(215), SC(216), SC(217)  are used to pass coordinates of near point
C       to regmil with the new optional type of command, for example:
C       RMILL/PS1,CV1,LN1,LN2,PL2,1,PLZ,.125,1,.5,30,RAPID,10, NEARPT, PT
C       or RMILL/PS1,CV1,LN1,LN2,PL2,1,PLZ,.125,1,.5,30,RAPID,10, NEARPT, number1, number2 [, number3]
C
C*************************************************************************
C**
C**     MODULE NAME AND RELEASE LEVEL
C**       rmill.f , 26.2
C**    DATE AND TIME OF LAST MODIFICATION
C**       04/12/18 , 10:18:18
C**
C*************************************************************************
 
       subroutine rmill

       include 'com8a.com'

      common/regthk/ rthk
      
      !common/regblk/ tp1
      
      real*8 rthk(8)
      !,tp1(6)
      real*8 dsthk1,dsthk2,csthk1,csthk2,dsptk1,dsptk2,csptk1,csptk2
      equivalence (rthk(1),csthk1),(rthk(2),csthk2)
      equivalence (rthk(3),dsthk1),(rthk(4),dsthk2)
      equivalence (rthk(5),csptk1),(rthk(6),csptk2)
      equivalence (rthk(7),dsptk1),(rthk(8),dsptk2)

      real*8 nrpt(3)
      equivalence (nrpt(1),sc(215)),(nrpt(2),sc(216))
      equivalence (nrpt(3),sc(217))
      
      !equivalence (nrpt(1),tp1(1)),(nrpt(2),tp1(2))
      !equivalence (nrpt(3),tp1(3))
      
      logical trflg
      
       real*4 rapid,rsc16,rsc19(3),rsc21,pos_fed
       real*8 rval1, rval2
       character*64 lab1, lab2
       integer*2 irapid(2),isc22(4),irsc19(6),ksn(4),ksc(100)
       equivalence (rapid,irapid),(sc(16),rsc16)
       equivalence (sc(19),rsc19(1),irsc19(1))
       equivalence (sc(21),rsc21),(sc(22),isc22)
       equivalence (sc,ksc)
      integer*4 nclkey, ivx1, ivx2
      integer*2 nwds, ietype, iret,fclr
      logical*2 rghthk, fin
      integer*2 ROUGH, FINISH
      parameter (ROUGH  = 320)
      parameter (FINISH = 323)
      parameter (NEARPT = 866)

       rapid = 0.
       irapid(2) = 5
       iret = 0
       rghthk = .false.
       fin = .false.
c                     (PS) test for plane or surface identifier
       if (geom .and. (geotyp .eq. plane .or. geotyp .eq. surf)) then
           sc(11)=tv
           call parsit
       else 
           call error(326)
           go to 99999
       endif
c                     (CK1) modifier test for ON, TO or PAST vocab word
       if (vocab .and. (ist.eq.71.or.ist.eq.714.or.ist.eq.715)) then
           isc22(3) = ist
           call parsit
       else
c            Default to ON.
           isc22(3) = 71
       endif
c                     (CK1) geometry test for line, plane, circle, 
c                           curve or surf id.
       if (geom .and. (geotyp .ge. line .and. geotyp .le. surf)) then
            sc(12) = tv
            call parsit
       else
            call error(324)
            go to 99999
       endif
c                     (CK2) modifier test for ON, TO or PAST vocab word
       if (vocab .and. (ist.eq.71.or.ist.eq.714.or.ist.eq.715)) then
           isc22(4) = ist
           call parsit
       else
c            Default to ON.
           isc22(4) = 71
       endif
c                     (CK2) geometry test for line, plane, circle, 
c                           curve or surf id.
       if (geom .and. (geotyp .ge. line .and. geotyp .le. surf)) then
            sc(13) = tv
            call parsit
       else
            call error(324)
            go to 99999
       endif
c                     (DS1) modifier test for ON, TO or PAST vocab word
       if (vocab .and. (ist.eq.71.or.ist.eq.714.or.ist.eq.715)) then
           isc22(1) = ist
           call parsit
       else
c            Default to ON.
           isc22(1) = 71
       endif
c                     (DS1) geometry test for line or plane id.
       if (geom .and. (geotyp .eq. line .or. geotyp .eq. plane.or.
     x                 geotyp.eq.SURF)) then
            if (geotyp.eq.SURF) then
              call gtdesc(tv,nclkey,nwds,ietype)
              call ncl_get_sf_primtyp(nclkey,ietype)
              if (ietype.ne.3) then
                call error(19)
                goto 99999
              endif
            endif
            sc(14) = tv
            call parsit
       else
            call error(19)
            go to 99999
       endif
c                     (DS2) modifier test for ON, TO or PAST vocab word
       if (vocab .and. (ist.eq.71.or.ist.eq.714.or.ist.eq.715)) then
           isc22(2) = ist
           call parsit
       else
c            Default to ON.
           isc22(2) = 71
       endif
c                     (DS2) geometry test for line or plane id.
       if (geom .and. (geotyp .eq. line .or. geotyp .eq. plane.or.
     x                 geotyp.eq.SURF)) then
            if (geotyp.eq.SURF) then
              call gtdesc(tv,nclkey,nwds,ietype)
              call ncl_get_sf_primtyp(nclkey,ietype)
              if (ietype.ne.3) then
                call error(19)
                goto 99999
              endif
            endif
            sc(15) = tv
            call parsit
       else
            call error(19)
            go to 99999
       endif
c                     (MOTION TYPE) test for scaler id, integer, or real
c                                   and .eq. abs ( 1 or 2 )
       if (scalar .and. (iabs(itv) .eq. 1 .or. iabs(itv) .eq. 2)) then
           isc10(2) = itv
           call parsit
       else
           call error(325)
           go to 99999
       endif
c                         (CLEARANCE PLANE or DISTANCE) test for plane
c                                                       or dist.
c                            (may only exist if motion type is lace)
       sc(16) = 0.
       sc(17) = 0.
       fclr = 1
       if (iabs(isc10(2)).eq.1) then
         if (scalar) then
           fclr = 1
           rsc16 = tv
           call parsit
         else if (geom.and.(geotyp.eq.plane .or. geotyp.eq.SURF)) then
           if (geotyp.eq.SURF) then
             call gtdesc(tv,nclkey,nwds,ietype)
             call ncl_get_sf_primtyp(nclkey,ietype)
             if (ietype.ne.3) then
                call error(281)
                goto 99999
             endif
           endif
           fclr = 0
           sc(16) = tv
           call parsit
         else
             call error(281)
             go to 99999
         endif
c                         (PLUNGE DISTANCE) test for scaler id, integer
c                                           or real.
c                            (may only exist if motion type is lace)
         if (scalar) then
           sc(17) = tv
           call parsit
         else
           call error(7)
           go to 99999
         endif
c
c.....Check for a plunge distance for SCRUB
c.......A value of 0 following the motion type signifies a plunge
c.......distance should be next
c
       else if (iabs(isc10(2)).eq.2.and.scalar.and.tv.eq.0.) then
         call parsit
         if (scalar) then
           sc(17) = tv
           call parsit
         else
           call error(7)
           go to 99999
         endif
       endif
c                     (STEPOVER MODE) test for scaler id, integer
c                                     or real and .eq. 1 or 2.
c                         step-over or scallop height flag.
c                           1 = step-over distance
c                           2 = scallop height
       if (scalar .and. (itv .eq. 1 .or. itv .eq. 2)) then
           isc10(3) = itv
           call parsit
c                         (STEPOVER DISTANCE) test for scaler id,
c                                             integer, or real.
           if (scalar) then
                if (tv.le.0.) then
                    call error(112)
                    goto 99999
                endif
                sc(18) = tv
           else
                call error(7)
                go to 99999
           endif
       else
           call error(325)
           go to 99999
       endif
c                     Get the three feed rates.
c                        General, Positioning & Ramping
       do 100 i=1,3
           call parsit
           if (scalar) then
               rsc19(i) = tv
c                         Test for RAPID vocabulary word for 
c                         positioning feedrate only.
           else if (i .eq. 2 .and. vocab .and. ist .eq. 5) then
c
c...Causes floating point exception on Alpha
c...Bobby  -  1/17/95
c
c               rsc19(i) = rapid
                irsc19(i*2-1) = irapid(1)
                irsc19(i*2) = irapid(2)
           else
               call error(327)
               go to 99999
           endif
100    continue
       sc(21)=0.0d0
       do 110 i=1,8
110    rthk(i) = sc(24)

       if (nxteos )  goto 99999
c                         (RETRACT OPTION) test for plane, point
c                                          or distance.
       call parsit
       if (scalar .or. (geom .and. geotyp .eq. point .or. 
     x           geotyp .eq. plane.or.geotyp.eq.SURF)) then
           iret = 1
         if (scalar) then
           rsc21 = tv
           iret = 2
         else
           if (geotyp.eq.SURF) then
             call gtdesc(tv,nclkey,nwds,ietype)
             call ncl_get_sf_primtyp(nclkey,ietype)
             if (ietype.ne.3) then
                call error(281)
                goto 99999
             endif
           endif
           sc(21) = tv
         endif
         if (nxteos ) goto 99999
         call parsit
       endif
c
c...  Optional rough, finish thicks or NEARPT
c
      if (ityp.ne.1 .or. ist.ne.FINISH .and. ist.ne.ROUGH .and. 
     x     ist.ne.NEARPT) goto 9004
       rghthk = .false.
       if (ist.eq.ROUGH) then
           rghthk = .true.
         do 120 i=1,4
           call parsit
           if (scalar) then
             rthk(i) = tv
           else if (ityp.ne.5 .or. ist.ne.9) then
             goto 9007
           endif
           if (nxteos) goto 200
120      continue
         call parsit
       endif
       if (ityp.ne.1 .or. ist.ne.FINISH .and. ist.ne.NEARPT ) goto 9004
       if (ist.eq.FINISH) then
           fin = .true.
       do 140 i=5,8
           call parsit
           if (scalar) then
             rthk(i) = tv
             if (.not. rghthk) rthk(i-4) = tv
           else if (ityp.ne.5 .or. ist.ne.9) then
             goto 9007
           endif
           if (nxteos) goto 200
140    continue
       call parsit
       endif
c       
c.... The block of code below included to extract point coordinates of the ,NEARPT, PT* 
c.... new additional items in the command 
c.... RMILL/PL1,TO,CV2,TO,CV1,TO,LN2,TO,LN1,1,0.,0.,1,0.25,0.0,RAPID,0.0,NEARPT,PT
c.... or RMILL/PL1,TO,CV2,TO,CV1,TO,LN2,TO,LN1,1,0.,0.,1,0.25,0.0,RAPID,0.0,NEARPT,number1,number2 [, number3]
c..... or RMILL/PL1,TO,CV2,TO,CV1,TO,LN2,TO,LN1,1,0.,0.,1,0.25,0.0,RAPID,0.0,NEARPT,PT, par1, par2 [, par3]
c.... or mixture of the last two variants, numbers and parameters
c.... (the point or pars should be defined somewhere above in pp)
c.... The coordinates are saved in three dimensional array nrpt(3) with items equivalent to 
c.... sc(215), sc(216), sc(217) used to pass the values to the regmil
c
c.... Sasha, July 2017
c       
       if (ist.eq.NEARPT) then
         call parsit
         if (geom .and. geotyp .eq. POINT) then
           call gtentt (tv,trflg,nclkey,ietype,nrpt)
         else
           if (scalar) then
             nrpt(1) = tv
           else if (.not. scalar) then 
             goto 9007
           endif
           call parsit
           if (scalar) then
             nrpt(2) = tv
           else if (.not. scalar) then 
             nrpt(1) = 0.0
             goto 9007
           endif
           call parsit
           if (scalar) then
             nrpt(3) = tv
           endif
         endif
          
         call parsit
         if (nxteos) goto 200
          
       endif


200    continue
c
c...save all current rmill value
c
99999  if ((irsc19(3).eq.irapid(1)).and.(irsc19(4).eq.irapid(2))) then
            pos_fed = -1
       else
            pos_fed = RSC19(2)
       endif
       if (fclr) then
           rval1 = rsc16
           lab1 = ""
       else
           rval1 = sc(16)
           call gtdesc(rval1,nclkey,nwds,ietype)
           call nclf_getlabel(nclkey, lab1, ivx1)           
      endif
       if (iret.ne.1) then
           label2 = ""
       else
           rval2 = sc(21)
           call gtdesc(rval2,nclkey,nwds,ietype)
           call nclf_getlabel(nclkey, lab2, ivx2)
       endif
       call nclf_save_rmill_values(ISC10(2), ISC10(3),fclr,rval1,
     x         lab1, sc(17),sc(18),RSC19(1), pos_fed, RSC19(3), 
     x         iret, rsc21, lab2, rghthk, fin, rthk)       
       return
C                           --  End of statement expected
9004   call error(4)
       return
C                           --  Scalar expected
9007   call error(7)
       return
       end
