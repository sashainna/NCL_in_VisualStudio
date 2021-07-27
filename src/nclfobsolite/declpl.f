C*********************************************************************
C*    NAME         :  declpl.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       declpl.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:09:53
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine declpl
C*      this routine parses plane definition.  the valid syntax
C*      combinations are:
C*          1.  (name =) plane/a,b,c,d
C*          2.  (name =) plane/pt1,pt2,pt3
C*          3.  (name =) plane/pt1,parlel,pl1
C*          4.  (name =) plane/parlel,pl1,xl,2
C*          5.  (name =) plane/pt1,perpto,ve1
C*          6.  (name =) plane/pt1,pt2,perpto,pl1
C*          7.  (name =) plane/pt1,perpto,pl1,pl2
C*          8.  (name =) plane/ln1<,perpto,pl1>
C*          9.  (name =) plane/pv1
C*         10.  (name =) plane/pl1
C*         11.  (name =) plane/fit,cv1<,planar>
C*
C*      the following represents the info passed to geogn1 in syscom:
C*             sc(10)             !       sc(11) thru sc(nn)
C*--------------------------------!----------------------------------------
C* type  subtype  numwds  notused !   sc(11)       sc(12)       sc(13)
C* ----  -------  ------  ------- !   ------       ------       ------
C*  603     1       *        *    !   sc(11) thru sc(14)= a,b,c,d values
C*  603     2       *        *    !   pt1 loc      pt2 loc      pt3 loc
C*  603     3       *        *    !   pt1 loc      pl1 loc         *
C*  603     4       *        *    !   pl1 loc      mod num      value
C*  603     5       *        *    !   pt1 loc      ve1 loc         *
C*  603     6       *        *    !   pt1 loc      pt2 loc      pl1 loc
C*  603     7       *        *    !   pt1 loc      pl1 loc      pl2 loc
C*  603     8     1 or 2     *    !   ln1 loc      pl1 loc         *
C*  603     9       *        *    !   pv1 loc         *            *
C*  603    10       *        *    !   pl1 loc         *            *
C*  603    11       *        *    !   cv1 loc       value          *
C*                                !
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
       subroutine declpl
       include 'com8a.com'

	common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

	character*64 svtkn
	integer*2 irest(4)
      real*8 rtok
      equivalence (token2,leq1),(rtok,leq1),(rest,irest)

       integer*2 isc11(4)
       equivalence(isc11,sc(11))
       integer*4 nclkey
       integer*2 nwds,ietype,primtyp, idst0


c             initialize isc10(4) which will be set to 1
c             if there is a tag point for plane definition.
c
       isc10(3)=0
       isc10(4)=0

	 idst0 = idst
       svtkn = savid2
       sc(199) = 0
c************************************************* if its pl/a,b,c,d
       if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.ist.eq.2)) then
           do 100 i=1,4
               sc(10+i)=tv
               if (i.eq.4.and.(nextyp.eq.11.or.ifl(111).ne.0.and.
     1             nextyp .eq.7)) then
                   isc10(2)=1
                   go to 88889
               endif
c
c                    If there is a tag point 
               if (i.eq.4.and.nextyp.eq.9) then
                   isc10(2)=1
                   isc10(4)=1
                   go to 88888
               endif
               call parsit
               if (ityp.ne.3.and.ityp.ne.4.and.(ityp.ne.2.or.ist.ne.2))
     1             then
                       call error(7)
                       go to 99999
               endif
100        continue
           call error(4)
           go to 99999
c******************************************** if its parlel to a plane
       else if (ityp.eq.1.and.ist.eq.637) then
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
               call parsit
           else
               call error(19)
               go to 99999
           endif
           if (ist.gt.637.and.ist.lt.644) then
               sc(12)=ist
               call parsit
           else
               call error(18)
               go to 99999
           endif
           if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.ist.eq.2))
     1         then
               isc10(2)=4
               sc(13)=tv
               go to 88888
           else
               call error(7)
               go to 99999
           endif
c******************************************** if its fit
       else if (ityp.eq.1.and.ist.eq.834) then
           call parsit
           if (ityp.eq.2.and.(ist.eq.8.or.ist.eq.9.or.ist.eq.7)) then
               isc10(2)=12
               sc(11)=tv
               if (nxteos) goto 88888
               call parsit
c
c...Get name of var to store if curve/surface is planar
c
               if (ityp.eq.2.and.(ist.eq.1.or.ist.eq.2)) then
c
c...Scalar (or unknown - make it a scalar)
c
                 idst = 2
                 ist = 2
                 savid2 = token2
                 ifl(9)  = ifl(11)
                 ifl(10) = ifl(12)
                 rest = 0.
                 keyold = keyhld
                 istold = ist
                 call vstore
                 sc(199) = rtok
                 idst = idst0
                 savid2 = svtkn
c
c...Simple variable expected
c
               else
                 call error (282)
                 goto 99999
               endif
               goto 88888 
           else
               call error(640)
               go to 99999
           endif
c**************************************************** if its a point
       else if (ityp.eq.2 .and. (ist.eq.3 .or. ist .eq. 21)) then
           sc(11)=tv
           if (ist .eq. 21 .and. NXTEOS) then
               isc10(2) = 9
               isc10(4) = 1
               sc(15) = tv 
               go to 88888
           end if
           call parsit
c                                       *****if its pt1,pt2,pt3
           if (ityp.eq.2 .and. (ist.eq.3 .or. ist .eq. 21)) then
               if (NXTEOS) then
                   if (isc11(4) .eq. 21) then
                      isc10(2) = 9
                      isc10(4) = 1
                      sc(15) = tv
C
C...Used to be goto 88888, but that canceled out the
C...display point, so goto 88889.  JLS 2/22/99
C
                      go to 88889
                   else
                      call error (12)
                      go to 99999
                   end if
               end if
               sc(12)=tv
               call parsit
               if (ityp.eq.2 .and. (ist.eq.3 .or. ist.eq.21)) then
                   sc(13)=tv
                   isc10(2)=2
                   isc10(4)=1
                   go to 88888
               else if (ityp.eq.1.and.ist.eq.630) then
c                                      *****  perpto plane thru 2 points
                   call parsit
                   if (ityp.eq.2.and.(ist.eq.6.or.ist.eq.9)) then
                       sc(13)=tv
                       if (ist.eq.9) then
                         call gtdesc(tv,nclkey,nwds,ietype)
                         call ncl_get_sf_primtyp(nclkey,primtyp)
                         if (primtyp.ne.3) then
                           call error(19)
                           go to 99999
                         endif
                       endif
                       isc10(2)=6
                       isc10(4)=1
                       go to 88888
                   endif
                   call error(19)
                   go to 99999
               else
                   call error(20)
                   go to 99999
               endif
c****************************************** if its perpto
           else if (ist.eq.630) then
               call parsit
               if (ityp.eq.2 .and. (ist.eq.4 .or. ist.eq.21)) then
                   isc10(2) = 5
                   sc(12) = tv
                   isc10(4) = 1
                   if (ist .eq. 21) isc10(3) = 3
                   go to 88888
               else if (ityp.eq.2.and.(ist.eq.6.or.ist.eq.9)) then
c                                      *** perpto 2 planes
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
C 
C...Since PL/LN,perpto,PL is a valid command then PL/PV,perpto,PL 
C...should also be a valid command. This if statement will handle
C...that command.  JLS 2/22/99
C
                   if (nextyp.eq.11.and.(ist.eq.0.or.ist.eq.3.or.ist
     x                 .eq.21)) then
                      isc10(2)=8
                      isc10(3)=2
C
C...If ist is equal to 3 or 21, there is a display point.
C
                      if (ist.eq.3.or.ist.eq.21) then
                         sc(15)=tv
                         isc10(4)=1
                      endif
                      goto 88889
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
                       sc(13)=tv
                       isc10(2)=7
                       isc10(4)=1
                       go to 88888
                   endif
                   call error(19)
                   go to 99999
               else
                   call error(11)
                   go to 99999
               endif
c****************************************** if its parlel
           else if (ist.eq.637) then
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
                       isc10(2)=3
                       sc(12)=tv
                       isc10(4)=1
                       go to 88888
                    else if (ityp.eq.2 .and. 
     x                       (ist.eq.4 .or. ist.eq.21)) then
                       isc10(2)=11
                       sc(12)=tv
                       call parsit
                       if (ityp.ne.2 .or. 
     x                       (ist.ne.4 .and. ist.ne.21)) then
                          call error(11)
                          goto 99999
                       endif
                       sc(13)=tv
                       go to 88888
                    else
                       call error(19)
                       go to 99999
                    endif
           else if (ityp.eq.2.and.ist.eq.1) then
                    call error(9)
                    go to 99999
                else
                    call error(12)
                    go to 99999
           endif
c**************************************************** if its a plane
        else if (ityp.eq.2 .and.(ist.eq.6.or.ist.eq.9)) then
           if (ist.eq.9) then
             call gtdesc(tv,nclkey,nwds,ietype)
             call ncl_get_sf_primtyp(nclkey,primtyp)
             if (primtyp.ne.3) then
               call error(19)
               go to 99999
             endif
           endif
           sc(11)=tv
           isc10(2) = 10
           go to 88888
c
        else if (ityp.eq.2.and.ist.eq.5) then
c***************************************   thru a line perpto pl
           sc(11)=tv
           isc10(2)=8
           isc10(3)=1
           if ((ifl(111).ne.0.and.nextyp.eq.7).or.
     1         (ifl(111).eq.0.and.nextyp.eq.11))
     2         go to 88889
           call parsit
           if (ityp.eq.1.and.ist.eq.630) then
c                 perpto
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
                   sc(12)=tv
                   isc10(3)=2
               else
                   call error(19)
                   go to 99999
               endif
C
C...It is possible to have a display point so we don't
C...want to just suppose that it should be the end of the
C...statement. JLS 2/22/99
C			
           else if (ityp.eq.2.and.(ist.eq.3.or.ist.eq.21)) then
               sc(15)=tv
               isc10(4)=1
               goto 88889
           else
               call error(4)
               go to 99999
           endif
       else if (ityp.eq.2.and.ist.eq.1) then
           call error(9)
           go to 99999
       else
           call error(12)
           go to 99999
       endif
88888  if (ifl(111).ne.0) then
C
C...Used to just look for a ')' but also need to check if there is 
C...a display point, so if nextyp is a 9, i.e. a comma, keep going. JLS 5/17/99
C
           if (nextyp.ne.7.and.nextyp.ne.9) then
               call error(4)
               go to 99999
c 
c                     If the PLANE is nested within another command
           else if (nextyp .eq. 7 .and. isc10(4).eq.1) then
               sc(15) = sc(11)
           else
              call parsit
           	  if (ityp .eq. 2 .and. (ist.eq.3 .or. ist.eq.21)) then
                 if (isc10(4) .ne. 1) isc10(4) = 1
                 sc(15) = tv
              else if (ityp .ne. 5 .or. ist .ne. 7) then
                 call error(20)
                 go to 99999
              endif
           endif
       else if (nextyp.ne.11) then
           if (isc10(4) .ne. 1) isc10(4) = 1
c
c            check for the tag point if not available default to first
c            point
c
           call parsit
           if (ityp .eq. 2 .and. (ist.eq.3 .or. ist.eq.21)) then
              sc(15) = tv
           else
              call error(20)
              go to 99999
           endif
       else if (nextyp.eq.11.and.isc10(4) .eq. 1) then
           sc(15) = sc(11)
c       else if (nextyp .ne. 11) then
c4          call error(4)
c           go to 99999
       endif
88889  isc10(1)=603
99999  return
       end
