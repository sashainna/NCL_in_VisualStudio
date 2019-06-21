C*********************************************************************
C*    NAME         :  declve.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       declve.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:09:54
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine declve
C* **  purpose of program: to parse a vector declaration.  the valid   **
C* **    declaration formats are:                                      **
C*          1.  (name =) vector/a,b,c
C*          2.  (name =) vector/a,b
C*          3.  (name =) vector/fwd
C*          4.  (name =) vector/tlaxis
C*          5.  (name =) vector/pt1,pt2
C*          6.  (name =) vector/perpto,pl1,posz
C*          7.  (name =) vector/unit,<vector-exp>
C*          8.  (name =) vector/ve1,cross,ve2
C*          9.  (name =) vector/parlel,intof,pl1,pl2,negx
C*         10.  (name =) vector/ve1,+ or -,ve2
C*         11.  (name =) vector/pt1,sf1
C*         12.  (name =) vector/ve1,times,d
C*         13.  (name =) vector/tanto,cv,pt  (or tanto,pt,cv)
C*         14.  (name =) vector/pv1
C*         15.  (name =) vector/ve1
C*         16.  (name =) vector/atangl,alf,ln1,posx
C*
C*      the following represents the info passed to geogn1 in syscom:
C*             sc(10)             !       sc(11) thru sc(nn)
C*--------------------------------!----------------------------------------
C* type  subtype  numwds  notused !   sc(11)       sc(12)       sc(13)
C* ----  -------  ------  ------- !   ------       ------       ------
C*  611     1       *        *    !   a value      b value      c value
C*  611     2       *        *    !   a value      b value         *
C*  611     3       *        *    !      *            *            *
C*  611     4       *        *    !      *            *            *
C*  611     5       *        *    !   pt1 loc      pt2 loc         *
C*  611     6       *        *    !   pl1 loc      mod num         *
C*  611     7       *        *    !   i val        j val        k val
C*  611     8       *        *    !   ve1 loc      ve2 loc         *
C*  611     9       *        *    !   pl1 loc      pl2 loc      dir mod
C*  611    10       *        *    !   ve1 loc      + or -       ve2 loc
C*  611    11       *        *    !   pt1 loc      sf1 loc         *
C*  611    12       *        *    !   ve1 loc      d value         *
C*  611    13       *        *    !   pt  loc      cv loc          *
C*  611    14       *        *    !   pv  loc         *            *
C*  611    15       *        *    !   ve1 loc         *            *
C*  611    16       *        *    !   alf val      ln1 loc       dir mod
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
      subroutine declve

      include 'com8a.com'
      include 'suvcom.com'


      integer*4 nclkey
      integer*2 nwds,ietype,ix
      logical trflg
      integer*2 primtyp

      trflg = .true.
      isc10(3) = 0
      isc10(4) = 0
      if (ityp.eq.1) then
c**************************************** if its a 'fwd'
          if (ist.eq.651) then
              isc10(2)=3
c**************************************** if its a 'tlaxis'
          else if (ist.eq.721) then
              isc10(2)=4
c***************************************  if its a 'unit'
          else if (ist.eq.30) then
              call parsit
              if (ityp.eq.2 .and. (ist.eq.4 .or. ist.eq.21)) then
                  isc10(2)=7
                  call gtentt(tv, trflg, nclkey, ietype, sc(11))
                  if (ist .eq. 21) then 
                      sc(11) = sc(14)
                      sc(12) = sc(15)
                      sc(13) = sc(16)
                  end if
              else
                  call error(11)
                  go to 99999
              endif
c***************************************  if its a 'perpto'
          else if (ist.eq.630) then
              call parsit
              if (ityp.eq.2.and.(ist.eq.6.or.ist.eq.9)) then
                  if (ist.eq.9) then
                    call gtdesc(tv,nclkey,nwds,ietype)
                    call ncl_get_sf_primtyp(nclkey,primtyp)
                    if (primtyp.ne.3) then
                      call error(19)
                      go to 99999
                    endif
                  endif
                  sc(11)=tv
              else
                  call error(19)
                  go to 99999
              endif
              call parsit
              if (ist.gt.653.and.ist.lt.660) then
                  sc(12)=ist
                  isc10(2)=6
              else
                  call error(18)
                  go to 99999
              endif
c ************************************  parlel,intof 2 planes
          else if (ist.eq.637) then
              call parsit
              if (ityp.eq.1.and.ist.eq.727) then
                  call parsit
              else
                  call error(188)
                  go to 99999
              endif
              if (ityp.eq.2.and.(ist.eq.6.or.ist.eq.9)) then
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
              else
19                call error(19)
                  go to 99999
              endif
              if (ityp.eq.2.and.(ist.eq.6.or.ist.eq.9)) then
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
              else
                  call error(19)
                  go to 99999
              endif
              if (ityp.eq.1.and.ist.gt.653.and.ist.lt.660) then
                  sc(13)=ist
                  isc10(2)=9
              else
                  call error(18)
                  go to 99999
              endif
C ************************************  TANTO,CV,PT or TANTO,PT,CV
          else if (ist.eq.646) then
              isc10(2)=13
              call parsit
              if (geom .and. geotyp .eq. curve) then
                sc(12)=tv
                iret = 8
                call parsuv(iret,dsuv,dsu,dsv)
                if (iret.gt.0) then
                  call error(iret)
                  goto 99999
                endif
                CALL PARSIT
                if (.not.(geom .and. (ist.eq.3 .or. ist.eq.21))) then
                  CALL ERROR(20)
                  GOTO 99999
                ENDIF
                SC(11)=TV
              else if (geom .and. (ist.eq.3 .or. ist.eq.21)) then
                SC(11)=TV
                CALL PARSIT
                if (.not.(geom .and. geotyp .eq. curve)) then
                  CALL ERROR(21)
                  GOTO 99999
                ENDIF
                SC(12)=TV
                iret = 8
                call parsuv(iret,dsuv,dsu,dsv)
                if (iret.gt.0) then
                  call error(iret)
                  goto 99999
                endif
              ELSE
                CALL ERROR(21)
                GOTO 99999
              ENDIF
c***************************************  ATANGL,alf,ln,POSX
          else if (ist.eq.1) then
              call parsit
              if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.
     1             ist.eq.2)) then
                sc(11) = tv
                call parsit
                if (ityp.ne.2.or.ist.ne.5)then
                  call error(17)
                  go to 99999
                endif
                sc(12) = tv
                call parsit
                if (ityp.ne.1.or.(ist.ne.638.and.ist.ne.639.and.
     1                            ist.ne.641.and.ist.ne.642.and.
     2                            ist.ne.654.and.ist.ne.655.and.
     3                            ist.ne.657.and.ist.ne.658))then
                  call error(18)
                  go to 99999
                endif
                sc(13) = ist
                isc10(2) = 16
              else
                call error(7)
                go to 99999
              endif
          else
              call error(12)
              go to 99999
          endif
c
c...This part is significantly changed to support PV. vp 3.16.93
c...if its a point/ve/pv
c
      else if (ityp .eq. 2 .and. ist .gt. 2) then
         sc(11)=tv
         if (ist .eq. 21) isc10(3) = 3
         if (ist.eq.3 .or. ist .eq. 21) then
c
c...ve/pv only
c
            if (NXTEOS .and. ist .eq. 21) then
               isc10(2) = 14
               call gtentt(tv, trflg, nclkey, ietype, sc(11))
               sc(11) = sc(14)
               sc(12) = sc(15)
               sc(13) = sc(16)
               go to 88888
            end if
c
c...Get Second token after pt/pv,
c......process when pt/pv
c
            call parsit
            if (ityp .eq. 2) then
                if (ist .eq. 21) isc10(3) = 3
                if (ist.eq.3 .or. ist .eq. 21) then
                   sc(12)=tv
                   isc10(2)=5
                   go to 88888
c
c......ve/pv thru a point normal to the surface    epm  6-27-85
c
                else if (ist .eq. 9) then
                   sc(12)=tv
                   iret = 9
                   call parsuv(iret,dsuv,dsu,dsv)
                   if (iret.gt.0) then
                       call error(iret)
                       goto 99999
                   endif
                   isc10(2)=11
                   go to 88888
                endif
c
c......minor word (-,+,cross,times)
c
            else if (ityp .eq. 1) then
               go to 50
            else
               call error(12)
               go to 99999
            end if
         else if (ist .ne. 4) then
            call error (12)
            go to 99999
         end if
c
c...ve2=ve/ve1. Paul. 10/09/93
c
         if (ist .eq. 4 .and. NXTEOS) then
            isc10(2) = 15
            call gtentt(tv, trflg, nclkey, ietype, sc(11))
            go to 88888
         end if
c
c...Get second token after ve,
c.......process '-,+,TIMES,CROSS'.
c
         call parsit
   50    if (ityp .eq. 1) then
c
c.....ve/pv TIMES scalar
c
            if (ist .eq. 28) then
               call parsit
               if (SCALAR .or.
     X              (ityp .eq .2 .and. ist .eq .2)) then
                   sc(12)=tv
                   isc10(2)=12
                   go to 88888
               endif
               call error(7)
               go to 99999
c
c......ve/pv CROSS ve/pv
c
            else if (ist .eq. 819) then
               isc10(2)=8
               ix    = 12
               go to 1200 
c
c......ve/pv PLUS or MINUS ve/pv
c
            else if (ist .eq. 19 .or. ist .eq. 10) then
               isc10(2)=10
               sc(12)=ist
               ix    = 13
               go to 1200
            else
               call error(225)
               go to 99999
            endif
c
c...Get 3-rd token after word
c
 1200       call parsit
            if (ityp.eq.2 .and. (ist.eq.4 .or. ist.eq.21)) then
               sc(ix)=tv
               if (ist .eq. 21) isc10(4) = 3
            else
               call error(11)
               go to 99999
            endif
         else
            call error(12)
            go to 99999
         endif
      else
c
c...end of changes by vp.
c************************************** must be a i,j(,k)
c
          sc(13) = 0.0d0
          do 200 i=1 , 3
c                                   *** integer or real
          if (ityp.eq.3.or.ityp.eq.4) then
              sc(10+i)=tv
              go to 100
          endif
          if (ityp.eq.2) then
              if (ist.eq.2) then
c                                   ***  scaler
                  sc(10+i)=tv
                  go to 100
              endif
              if (ist.eq.1) then
c                                   ***  unknown identifier
                  call error(9)
                  go to 99999
              endif
12            call error(12)
              go to 99999

          else if (nextyp.ne.11.or.(ifl(111).eq.1.and.nextyp.ne.7)
     1             .or.i.lt.3) then
              call error(7)
              go to 99999
          else
              isc10(2)=2
              go to 88889
          endif
100       call parsit
          if ((i.eq.3.and.ityp.eq.7.and.ifl(111).eq.0).or.
     1        (ityp.eq.5.and.ist.eq.7.and.ifl(111).ne.0)) then
              isc10(2)=1
              go to 88889
c      ***** the following code checks for end of statement
c      ***** epm  10-20-82
          else if (i.eq.3.and.ityp.ne.7.and.(ifl(111).eq.0.or.
     1        nextyp.ne.7)) then
              call error(4)
              go to 99999
c      *****    end of patch
          endif

200       continue
      endif
88888 if (ifl(111).ne.0) then
          if (nextyp.ne.7) then
              call error(4)
              go to 99999
          endif
      else if (nextyp.ne.11) then
4         call error(4)
          go to 99999
      endif
88889 isc10(1)=611

99999 return
      end
