C*********************************************************************
C*    NAME         :  declpt.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       declpt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:53
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine declpt
C*      this subroutine parses the point declaration statement            *
C*      the valid syntax combinations are:                                *
C*          1. (name)=point/x,y,z                                         *
C*          2. (name)=point/x,y                                           *
C*          3. (name)=point/te                                            *
C*          4. (name)=point/intof,ln1,ln2                                 *
C*          5. (name)=point/center,ci1                                    *
C*          6. (name)=point/intof,pl1,pl2,pl3                             *
C*          7. (name)=point/intof,pl1,cv1,pt1/ALL                         *
C*          8. (name)=point/xs,intof,ln1,ci1                              *
C*          9. (name)=point/xs,intof,ci1,ci2                              *
C*         10. (name)=point/intof,ci1,cv1,pt1/ALL
C*         11  (name)=point/ci1,aa,30
C*         12. (name)=point/pt1,ci1,45
C*         13. (name)=point/pt1,1.0,1.3
C*         14. (name)=point/xs,endpt,<line or circle>
C*         15. (name)=point/intof,ln1,pl1
C*         16. (name)=point/cv1,<dis>
C*         17. (name)=point/..............?
C*         18. (name)=point/endpt,pv1
C*         19. (name)=point/intof,cv1,cv2,pt1/ALL
C*         20. (name)=point/on,cv1,u
C*         21. (name)=point/on,sf1,u,v
C*         22. (name)=point/projct,pt1<,ve1>,sf1<,pt2>                   *
C*
C*      the following represents the info passed to geogn1 in syscom:
C*             sc(10)             !       sc(11) thru sc(nn)
C*--------------------------------!----------------------------------------
C* type  subtype  numwds  notused !   sc(11)       sc(12)       sc(13)
C* ----  -------  ------  ------- !   ------       ------       ------
C*  601     1       *        *    !   x val        y val        z val
C*  601     2       *        *    !   x val        y val          *
C*  601     3       *        *    !     *            *            *
C*  601     4       *        *    !   ln1 loc      ln2 loc        *
C*  601     5       *        *    !   ci1 loc        *            *
C*  601     6       *        *    !   pl1 loc      pl2 loc      pl3 loc
C*  601     7       *        *    !   pl1 loc      cv1 loc      pt1 loc
C*  601     8       *        *    !   mod num      ln1 loc      ci1 loc
C*  601     9       *        *    !   mod num      ci1 loc      ci2 loc
C*  601    10       *        *    !   ci1 loc      cv1 loc      pt1 loc
C*  601    11       *        *    !   ci1 loc      value          *
C*  601    12       *        *    !   pt1 loc      ci1 loc      angle
C*  601    13     num vals   *    !   x val        y val        z val
C*  601    14       *        *    !   mod num      ci or ln loc   *
C*  601    15       *        *    !   ln1 loc      pl1 loc        *
C*  601    16       *        *    !   cv1 loc      dis          opt pt loc
C*  601    17       *        *    !   pn1 loc      pt #
C*  601    18       *        *    !   pv1 loc       *             * 
C*  601    19       *        *    !
C*  601    20       *        *    !
C*  601    21       *        *    !
C*  601    22       *        *    !   pt1 loc      ve1 opt       sf1
C*
C**************************************************************************
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
      subroutine declpt

      include 'com8a.com'
      include 'suvcom.com'

      real*4 u,v
      real*8 temp_command_holder
      real*8 W0,W1,del
      real*8 t(12)

      logical lflg
      integer*2 iret

      INTEGER*2 ISC11(4)
      EQUIVALENCE (SC(11),ISC11)

      integer*4 nclkey
      integer*2 nwds, ietype, primtyp

      integer*2 PERCNT,DELTA,TIMES
      parameter (PERCNT = 763)
      parameter (DELTA = 34)
      parameter (TIMES = 28)
 
      IF (ITYP.EQ.2.AND.IST.EQ.20) THEN
C                                   **** PATERN
           SC(11) = TV
           CALL PARSIT
           IF (ITYP.EQ.3.OR.ITYP.EQ.2.AND.IST.EQ.2) THEN
c              IF (ITV.LT.1.OR.ITV.GT.ISC11(3)) THEN
c                 CALL ERROR(340)
c                 GO TO 99999
c              ENDIF
               ISC10(3) = ITV
               ISC10(2) = 17
               GO  TO 88889
           ELSE
               CALL ERROR(53)
               GO TO 99999
           ENDIF

       else if (ityp.eq.2.and.ist.eq.7) then
c                                   **** pt/ci1,aa,30
           sc(11)=tv
           call parsit           
c
c... pt/ci1,dis [,pt1]
c
           if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3.or.ityp.eq.4) then
              sc(12) = tv
              isc10(2)=16
              isc10(3)=1
              if (nextyp.eq.11.or.(ifl(111).eq.1.and.nextyp.eq.7))
     *             goto 88889
              call parsit
              if (ityp.eq.2.and.ist.eq.3) then
                 sc(13)=tv
                 isc10(3)=2
                 go to 88889
              endif
              call error(20)
              go to 99999
           endif
           
           if (ityp.ne.1.or.ist.ne.1) then
               call error(12)
               go to 99999
           endif
           call parsit
           if (ityp.eq.3.or.ityp.eq.4.or.
     1         (ityp.eq.2.and.ist.eq.2)) then
               sc(12)=tv
               isc10(2)=11
               go to 88889
           else
               call error(7)
               go to 99999
           endif
       else if (ityp.eq.2.and. (ist.eq.3 .or. ist .eq. 21)) then
           sc(11)=tv
           if (nextyp.eq.11.or.(ifl(111).eq.1
     1         .and.nextyp.eq.7)) then
c                                  *** pt/pt1
             isc10(2)=13
             isc10(3)=0
             goto 88889
           endif
           call parsit
           if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.ist.eq.2))
     1          then
c                                  *** pt/pt1,xval,<yval>,<zval>
               do 50,i=1,3
                   sc(11+i)=tv
                   if (nextyp.eq.11.or.(ifl(111).eq.1
     1                 .and.nextyp.eq.7)) then
c                             end of statement
                       isc10(2)=13
                       isc10(3)=i
                       go to 88889
                   endif
                   call parsit
                   if (.not.(ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2
     1                  .and.ist.eq.2))) then
                      call error(7)
                      go to 99999
                   endif
50             continue
               call error(4)
               go to 99999
           else if (ityp.eq.2.and.ist.eq.7) then
c                                  *** pt/pt1,ci1,45
               sc(12)=tv
               call parsit
               if (ityp.eq.3.or.ityp.eq.4.or.
     1             (ityp.eq.2.and.ist.eq.2)) then
                   sc(13)=tv
                   isc10(2)=12
                   go to 88889
               else
                   call error(7)
                   go to 99999
               endif
           else if (ityp.eq.1.and.ist.eq.DELTA) then
c                                  *** pt/pt1,DELTA,del,TIMES,ve1
               call parsit
               if (ityp.eq.3.or.ityp.eq.4.or.
     1             (ityp.eq.2.and.ist.eq.2)) then
                 del=tv
                 call parsit
                 if (ityp.eq.1.and.ist.eq.TIMES) then
                   call parsit
	             if (ityp.eq.2.and.ist.eq.4) then
                     call gtentt(tv, trflg, nclkey, ietype, t(1))
				   sc(12)= del*t(1)
                     sc(13)= del*t(2)
                     sc(14)= del*t(3)
                     isc10(2)=13
                     isc10(3)=3
                     go to 88889
                   endif
                 endif
               else
                   call error(7)
                   go to 99999
               endif
           else
               call error(159)
               go to 99999
           endif
       else if (ityp.eq.2.and.(ist.eq.8.or.ist.eq.5)) then
c
c... pt/cv1,dis [,pt1]
c... pt/ln1,dis [,pt1]
c
           sc(11) = tv
c
c... aak 17-OCT-97:
c... added u-parsing in "PT/cv[u],dis,pt1": to be used later?
c...
              iret = 8
              call parsuv(iret,csuv,csu,csv)
              if(iret.gt.0) then
                  call error(iret)
                  goto 99999
              endif

           call parsit

           if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3.or.ityp.eq.4) then
              sc(12) = tv
              isc10(2)=16
              isc10(3)=1
              if (nextyp.eq.11.or.(ifl(111).eq.1.and.nextyp.eq.7))
     *             goto 88889
              call parsit
              if (ityp.eq.2.and.ist.eq.3) then
                 sc(13)=tv
                 isc10(3)=2
                 go to 88889
              endif
              call error(20)
              go to 99999
           endif
           call error(7)
           go to  99999
c
c...pt/pv1 (point components only)
c
       else if (ityp .eq. 2 .and. ist .eq. 21) then
           sc(11) = tv
           isc10(2) = 5
           go to 88889
       endif
c
c...pt/endpt,pv1 ...  moved to pt/mod,endpt... isub=14
c
c      if (ist.ne.664) go to 70
c          call parsit
c          if (ityp.eq.2 .and. ist.eq.21) then
c              isc10(2)=18
c              sc(11) = tv
c              go to 88889
c          else
c            call error (418)
c            go to 99999
c          end if
 70    if (ist.ne.650) go to 100
c                                    ***  te
           isc10(2)=3
           go to 88889
100    if (ist.ne.634) go to 200
c                                    ***  center,circle/curve/surface
           call parsit
           if (ityp.eq.2 .and.
     x         (ist.eq.7 .or. ist.eq.8 .or. ist.eq.9)) then
               isc10(2)=5
               sc(11)=tv
               go to 88889
           else
               call error(159)
               go to 99999
           endif
200    if (ist.ne.727 .and. ist.ne.758) goto 300
           if (ist .eq. 758) then
c..... 3D intof: cv1, cv2, optional nearpt
             sc(10) = 0
             isc10(2) = 19
             isc10(4) = 1
             call parsit
             if (ityp.eq.2 .and. 
     x           (ist.eq.5.or.ist.eq.7.or.ist.eq.8)) then
               sc(11) = tv
             else
               call error(21)
               go to 99999
             endif
             call parsit
             if (ityp.eq.2 .and. 
     x           (ist.eq.5.or.ist.eq.7.or.ist.eq.8)) then
               sc(12) = tv
             else
               call error(21)
               go to 99999
             endif
             if (nxteos) then
               isc10(3)=1 ! no nearpt
               goto 88889
             else
               call parsit
               if (ityp.eq.2.and. (ist.eq.3.or.ist.eq.21)) then
                 sc(13)=tv
                 goto 88889
               else if (ityp.eq.1 .and. ist.eq.816) then     
                   isc10(3)=2
                   go to 88889
               else
                 call error(20)
                 goto 99999
               endif
             endif
           endif
c..... traditional intof
           call parsit
           if (ityp.eq.2 .and. (ist.eq.5 .or. ist.eq.21)) then
c                                      **** intof, ln1 or pv1
               sc(11)=tv
               call parsit
               if (ityp.eq.2 .and. (ist.eq.5 .or. ist.eq.21)) then
c                                      **** intof, ln1, ln2
                   sc(12)=tv
                   isc10(2)=4
                   go to 88889
               else if (ityp.eq.2.and.(ist.eq.6.or.ist.eq.9)) then
c                                      **** intof, ln1, pl1
                   if (ist.eq.9) then
                     call gtdesc(tv,nclkey,nwds,ietype)
                     call ncl_get_sf_primtyp(nclkey,primtyp)
                     if (primtyp.ne.3) then
                       call error(19)
                       go to 99999
                     endif
                   endif
                   sc(12)=tv
                   isc10(2)=15
                   go to 88889
               else
                   call error(19)
                   go to 99999
               endif
           else if(ityp.eq.2.and.(ist.eq.6.or.ist.eq.9)) then
c                                      ***  intof, pl1
               if (ist.eq.9) then
                   call gtdesc(tv,nclkey,nwds,ietype)
                   call ncl_get_sf_primtyp(nclkey,primtyp)
                   if (primtyp.ne.3) then
                     call error(19)
                     go to 99999
                   endif
               endif
               sc(11)=tv
               call parsit
               if (ityp.eq.2.and.ist.eq.5) then
c                                      ***  intof, pl1, ln1
                   sc(12)=sc(11)
                   sc(11)=tv
                   isc10(2)=15
                   goto 88889
               else if (ityp.eq.2.and.(ist.eq.6.or.ist.eq.9)) then
c                                      ***  intof, pl1, pl2
                   if (ist.eq.9) then
                     call gtdesc(tv,nclkey,nwds,ietype)
                     call ncl_get_sf_primtyp(nclkey,primtyp)
                     if (primtyp.ne.3) then
                       call error(19)
                       go to 99999
                     endif
                   endif
                   sc(12)=tv
                   call parsit
                   if (ityp.eq.2.and.(ist.eq.6.or.ist.eq.9)) then
c                                      ***  intof, pl1, pl2, pl3
                       if (ist.eq.9) then
                         call gtdesc(tv,nclkey,nwds,ietype)
                         call ncl_get_sf_primtyp(nclkey,primtyp)
                         if (primtyp.ne.3) then
                           call error(19)
                           go to 99999
                         endif
                       endif
                       sc(13)=tv
                       isc10(2)=6
                       go to 88889
                   else
                       call error(19)
                       go to 99999
                   endif
               else if (ityp.eq.2.and.ist.eq.8) then
c                                      ***  intof, pl1, cv1
                        sc(12)=tv
                        call parsit
                        if (ityp.eq.2.and. 
     -                      (ist.eq.3 .or. ist.eq.21)) then
c                                      ***  intof, pl1, cv1, pt1
                            sc(13)=tv
                            isc10(2)=7
                            go to 88889                   
                        else
                            call error(20)
                            go to 99999
                        endif
                    else
                        call error(21)
                        go to 99999
               endif
           else if (ityp.eq.2.and.ist.eq.7) then
c                                   ***  intof, ci1, cv1,pt1
               sc(11)=tv
               call parsit
               if (ityp.eq.2.and.ist.eq.8) then
                   sc(12)=tv
               else
                   call error(21)
                   go to 99999
               endif
               call parsit
               if (ityp.eq.2.and. (ist.eq.3 .or. ist.eq.21)) then
                   sc(13)=tv
                   isc10(2)=10
                   go to 88889                   
               else
                   call error(20)
                   go to 99999
               endif
c
c...PT/INTOF,cv,cv/ln/ci/pl,pt/ALL
c
           else if (ityp.eq.2.and.ist.eq.8) then
               sc(11)=tv
               call parsit
C
C...Also want to be able to intersect a curve with
C...a point vector.  JLS 6/30/99
C
               if (ityp .eq. 2 .and. (ist .eq. 8 .or. 
     x             ist .eq. 5.or.ist.eq.21.or.
     x             ist .eq. 7 .or.ist .eq. 6)) then
                   sc(12)=tv
               else
                   call error(21)
                   go to 99999
               endif
               call parsit
               if (ityp.eq.2 .and. (ist.eq.3 .or. ist.eq.21)) then
                   sc(13)=tv
                   isc10(2)=19
                   go to 88889
               else if (ityp.eq.1 .and. ist.eq.816) then     
                   isc10(3)=2
                   isc10(2)=19
                   go to 88889
               else
                   call error(20)
                   go to 99999
               endif
           endif
           call error(12)
           go to 99999
300    if (ityp.ne.1.or.ist.lt.638.or.ist.gt.643)
     1     go to 640
c                                   **** mod
       sc(11)=ist
       call parsit
       if (ityp.eq.1.and.ist.eq.727) then
c                                   **** mod, intof
           if (sc(11).eq.640.or.sc(11).eq.643) then
c                      ***zs and zl not allowed here
              call error(213)
               go to 99999
           endif
           call parsit
           if (ityp.eq.2 .and. (ist.eq.5 .or. ist.eq.21)) then
c                                   **** mod, intof, ln1
               isc10(2)=8
           else if (ityp.eq.2.and.ist.eq.7) then
c                                   **** mod, intof, ci1
                    isc10(2)=9
                else
                    call error(192)
                    go to 99999
           endif
       else if (ist.eq.664) then
c                                   **** endpt
           call parsit
           if (ityp.eq.2.and.
     x        (ist.eq.5.or.ist.eq.7.or.ist.eq.8.or.ist.eq.21)) then
c                                   **** circle or line or pvec or curve
               sc(12)=tv
               isc10(2)=14
               go to 88889
           else
               call error(192)
               go to 99999
           endif
c
c...Unrecognized command
c
       else
           call error(188)
           go to 99999
       endif
       sc(12)=tv
       call parsit
       if (ityp.eq.2.and.ist.eq.7) then
           sc(13)=tv
           go to 88889
       else
           call error(159)
           go to 99999
       endif
c
c...PT/ON,cv|sf,u (,v)
c
 640   if (ityp.eq.1.and.ist.eq.71) then
           W0 = -.0001
           W1 = 1.0001
           isc10(3) = 0
           call parsit
           if (ityp .eq. 2 .and. (ist.eq.8 .or. ist .eq. 5 .or.
     1         ist .eq. 7 .or. ist .eq. 9)) then
               it = ist
               sc(11)=tv
               isc10(2)=20
               call parsit
               if (vocab .and. voc.eq.PERCNT) then
                 if (it.eq.5 .or. it.eq.7) then
                   call error (21)
                   goto 99999
                 endif
                 isc10(3) = 1
                 W1 = 100.0001
                 call parsit
               endif
               if (.not.scalar .or. tv.lt.W0 .or. tv.gt.W1) then
                   call error (463)
                   go to 99999
               endif
               sc(12) = tv
               if (it .eq. 9) then
                   call parsit
                   if (.not.scalar .or. tv.lt.W0 .or. tv.gt.W1) then
                       call error (463)
                       go to 99999
                   endif
                   isc10(2)=21
                   sc(13) = tv
               endif
               go to 88889
           else
               call error(21)
               go to 99999
           endif

       endif

c
c...  PT/PROJCT,PT(,VE),SF(,PT)   (isc10(2) = 22)
c...  Project a point onto a surface.  Project along an optional vector
c...  or (default) along the normal to the surface.  Or use the (optional)
c...  near point.
c...  Ed Ames 18 Jul 00
c.....  Added solid as valid entity to project to - Andrew 12/13/12
c
       if ((ityp.eq.1).and.(ist.eq.888)) then
c
c...  Used for checking surface parameters.  iret=9 means surface (not curve)
c
            iret = 9
            call parsit
            if (ityp.ne.2 .or. (ist.ne.3 .and. ist.ne.21)) then
                 call error (20)
                 goto 99999
            else
                 sc(11) = tv
                 call parsit
                 if (ityp.ne.2 .or. (ist.ne.4 .and. ist.ne.21 
     1                         .and. ist.ne.6 .and. ist.ne.9
     2                         .and. ist.ne.33)) then
                      call error(186)
                      goto 99999
                 endif
                 isc10(2) = 22
c
c...  Optional projection vector.  PT/PROJCT,PT,VE,SF(,PT)
c
                 if (ist.eq.4 .or. ist.eq.21) then
                      sc(12) = tv
                      sc(16) = 0
                      call parsit
                      if (ityp.ne.2 .or. 
     1                   (ist.ne.9.and.ist.ne.6.and.ist.ne.33)) then
                           call error(186)
                           goto 99999
                      endif
                      temp_command_holder = tv
                      lflg = .FALSE.
                      if (ist .eq. 9) then
                           call parsuv (iret,lflg,u,v)
c
c... In case u, v parameters included with surface (ex. sf1[0.2,0.4])
c... lflg will be true.  Otherwise, lflg = false, and u = v = 0.5.
c... parsuv will modify the global variable tv, losing all info of surf
c... If no u, v given, tv won't be changed, but if given tv -> 0.0.
c... If plane (not surface), keep lflg false and use default u,v.
c
                           if (iret.gt.0) then
                                call error(iret)
                                goto 99999
                           endif
                      endif
                      sc(13) = temp_command_holder
                      if (lflg) then
                           sc(14) = u
                           sc(15) = v    
                      else
                           sc(14) = 0.5
                           sc(15) = 0.5
                      endif
                      call parsit
                      if (ityp.eq.2 .and. 
     1                    (ist.eq.3 .or. ist.eq.21)) then
                           sc(16) = tv
                           goto 88889
                      endif
                      goto 88889
                 endif
c
c...  PT/PROJCT,PT,SF(,PT).
c...  Check for u, v parameters with the surface. If none, u = v = 0.5.
c...  parsuv will modify the global variable tv, losing all info of surf
c...  If no u, v given, tv won't be changed, but if given tv -> 0.0.
c...  If plane (not surface), keep lflg false and give default u,v.
c
                 temp_command_holder = tv
                 lflg = .FALSE.
                 if (ist .eq. 9) then
                      call parsuv (iret,lflg,u,v)
                      if (iret.gt.0) then
                           call error(iret)
                           goto 99999
                      endif
                 endif
                 sc(12) = temp_command_holder
                 if (lflg) then
                      sc(13) = u
                      sc(14) = v
                 else
                      sc(13) = 0.5
                      sc(14) = 0.5
                 endif
c
c... Check if at end of line (ie. pt/projct,pt1,sf1 )
c
                 sc(15) = 0
                 if (.not.nxteos) then
                      call parsit
                      if ((ityp.eq.2) .and.
     1                    ((ist.eq.3) .or. (ist.eq.21))) then
                           sc(15) = tv
                           goto 88889
                      else
                        call error(20)
                        goto 99999
                      endif
                 endif
                 goto 88889
            endif      
            goto 99999
c
c...  End of PT/PROJCT,PT,SF   isc10(2) = 22.
c
       endif      
 
c************************************** must be a x,y(,z)
       isc10(2)=2
       do 460 i=1 , 3
       if (ityp.ne.3.and.ityp.ne.4) go to 430
c                                   ***   integer or real
           sc(10+i)=tv
           if (nextyp.eq.11.or.(nextyp.eq.7.and.ifl(111).eq.1))
     1         go to 480
           go to 460
430    if (ityp.ne.2) go to 450
           if (ist.ne.2) go to 440
c                                   ***  scaler
                sc(10+i)=tv
                if (nextyp.eq.11.or.(nextyp.eq.7.and.ifl(111).eq.1))
     1              go to 480
                go to 460
440        if (ist.eq.1) call error(9)
           if (ist.ne.1) call error(12)
           go to 99999
450        call error(7)
           go to 99999
460    if (i.lt.3) call parsit
       call error(4)
       go to 99999
480    if (i.lt.2) call error(7)
       if (i.eq.3) isc10(2)=1
88889  if (ifl(111).eq.0) then
           if (nextyp.ne.11) then
4              call error(4)
               go to 99999
           endif
      else if ((nextyp.ne.7) .and.
     1     (.not.(ityp.eq.5 .and. ist.eq.7)  )) then
           call error(4)
           go to 99999
       endif
       isc10(1)=601
99999  return
       end
