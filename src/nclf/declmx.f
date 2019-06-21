C*********************************************************************
C*    NAME         :  declmx.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       declmx.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:09:53
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine declmx
C*      this routine parses matrix declaration. the valid syntax
C*      combinations are:
C*          1. (name =) matrix/a,b,c,d,e,f,g,h,i,j,k,l
C*          2. (name =) matrix/invers,mx1
C*          3. (name =) matrix/mx1,mx2
C*          4. (name =) matrix/pt1,(ve1/pv1),(ve2/pv2)
C*          5. (name =) matrix/mirror,xyplan,<zxplan>,<yzplan>
C*          6. (name =) matrix/mirror,pl1
C*          7. (name =) matrix/scale,.5
C*          8. (name =) matrix/xyrot,45
C*          9. (name =) matrix/trans,2,4<,10>
C*         10. (name =) matrix/pv1,ve2
C*         11. (name =) matrix/pv1,pv2
C*         12. (name =) matrix/mx1
C*         13. (name =) matrix/pt1,pt2,pt3,pt1a,pt2a,pt3a

C*
C*      the following represents the info passed to geogn1 in syscom:
C*             sc(10)             !       sc(11) thru sc(nn)
C*--------------------------------!----------------------------------------
C* type  subtype  numwds  notused !   sc(11)       sc(12)       sc(13)   sc(14)
C* ----  -------  ------  ------- !   ------       ------       ------   ------
C*  612     1       *       *     !   sc(11)-sc(22) = 12 element locations
C*  612     2       *       *     !   mx1 loc
C*  612     3       *       *     !   mx1 loc      mx2 loc
C*  612     4       *       *     !   pt1 loc      ve1 loc      ve2 loc
C*  612     5     num mods  *     !   mod num      mod num      mod num
C*  612     6       *       *     !   pl1 loc
C*  612     7       *       *     !   X-scale      Y-scale      Z-scale  pt1 loc
C*  612     8       *       *     !   mod num      angle
C*  612     9     num par         !   x val        y val        (z val)
C*  612     10      *             !   pv1 loc      ve2 loc
C*  612     11      *             !   pv1 loc      pv2 loc
C*  612     12      *             !   mx1 loc
C*  612     13      *             !   pt1 loc      pt2 loc      pt3 loc  pt1a loc 
C*                                                              pt2a loc pt3a loc
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
      subroutine declmx
c      include 'com8a.com'
      include 'com.com'

      integer*4 nclkey
      integer*2 i,primtyp,nwds,ietype

      if (ist.eq.822) then
c                                                            **  invers
           call parsit
           if (ityp.eq.2.and.ist.eq.10) then
               sc(11)=tv
               isc10(2)=2
           else if (ityp.eq.2.and.ist.eq.1) then
               call error(9)
               go to 99999
           else
               call error(94)
               go to 99999
           endif
      else if (ityp.eq.2.and.ist.eq.10.and. NXTEOS) then
c                                                            ** mx1
           sc(11)=tv
           isc10(2)=12
      else if (ityp.eq.2.and.ist.eq.10) then
c                                                            ** mx1,mx2
           sc(11)=tv
           call parsit
           if (ityp.eq.2.and.ist.eq.10) then
               sc(12)=tv
               isc10(2)=3
           else if (ityp.eq.2.and.ist.eq.1) then
               call error(9)
               go to 99999
           else
               call error(94)
               go to 99999
           endif
      else if (ityp.eq.2.and.ist.eq.3) then
c
c...vp 9-apr-93 pointvector added                        *** pt1,ve1,ve2
c
           sc(11)=tv
           call parsit
           if (ityp.eq.2 .and. (ist.eq.4 .or. ist.eq.21)) then
               sc(12)=tv
               call parsit
           else if (ityp.eq.2 .and. ist.eq.3) then
               sc(12)=tv
               call parsit
           else
               call error(11)
               go to 99999
           endif
           
           if (ityp.eq.2 .and. (ist.eq.4 .or. ist.eq.21)) then
               sc(13)=tv
               isc10(2)=4
           else if (ityp.eq.2 .and. ist.eq.3) then
               sc(13)=tv
               call parsit
           else
59             call error(11)
               go to 99999
           endif
           
           if (.not. NXTEOS) then
                if (ityp.eq.2 .and. ist.eq.3) then
                    sc(14)=tv
                    call parsit
                else
                    call error(11)
                    go to 99999
                endif
           
                if (ityp.eq.2 .and. ist.eq.3) then
                   sc(15)=tv
                   call parsit
                else
                    call error(11)
                    go to 99999
                 endif
           
                if (ityp.eq.2 .and. ist.eq.3) then
                    sc(16)=tv
                    isc10(2)=13
                else
                    call error(11)
                    go to 99999
                endif
           endif
           
      else if (ityp .eq. 2 .and. ist .eq. 21) then
c
c...MX/pv,(ve,pv)  vp 9-apr-93
c
           sc(11)=tv
           isc10(2) = 10
           call parsit
           if (ityp .eq. 2 .and. (ist.eq.21 .or. ist.eq.4)) then
               sc(12) = tv
               if (ist .eq. 21) isc10(2) = 11
           else
               call error(11)
               go to 99999
           endif
      else if (ityp.eq.1.and.ist.eq.821) then
c                                                        ***  mirror
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
               isc10(2)=6
           else
               do 75,i=1,3
                   if (ityp.eq.1.and.(ist.eq.33.or.ist.eq.37.or.
     1                 ist.eq.41)) then
                       sc(10+i)=ist
                       if (nextyp.eq.11) go to 80
                       call parsit
                   else
                       call error(164)
                       go to 99999
                   endif
75             continue
80             isc10(2)=5
               isc10(3)=i
           endif
      else if (ityp.eq.1.and.ist.eq.25) then
c                                                        ***  scale
           call parsit
           if (ityp .eq. 2 .and. (ist .eq. 3 .or. ist .eq. 21)) then
               sc(14) = tv
               call parsit
           else
               sc(14) = 0.
           endif
           if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.ist.eq.2))
     1         then
               sc(11)=tv
c
c......Expanded SCALE matrix
c......to allow for different scales
c......for each axis
c......Bobby  -  1/6/2000
c
               sc(12) = tv
               sc(13) = tv
               isc10(2)=7
               if (nextyp .ne. 11 .and.
     1             (ifl(111) .eq. 0 .or. nextyp .ne. 7)) then
                   call parsit
                   if (ityp.eq.3.or.ityp.eq.4.or.
     1                 (ityp.eq.2.and.ist.eq.2)) then
                       sc(12) = tv
                       sc(13) = tv
                   else
                       call error(7)
                       go to 99999
                   endif
                   if (nextyp .ne. 11) then
                       call parsit
                       if (ityp.eq.3.or.ityp.eq.4.or.
     1                     (ityp.eq.2.and.ist.eq.2)) then
                           sc(13) = tv
                       else
                           call error(7)
                           go to 99999
                       endif
                   endif
               endif
           else
7              call error(7)
               go to 99999
           endif
      else if (ityp.eq.1.and.(ist.gt.732.and.ist.lt.736)) then
c                                             ***  xyrot yzrot zxrot
           sc(11)=ist
           call parsit
           if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.ist.eq.2))
     1         then
               sc(12)=tv
               isc10(2)=8
           else
               call error(7)
               go to 99999
           endif
      else if (ityp.eq.1.and.ist.eq.649) then
c                                                 ***  transl,x,y,z
           do 90,i=1,3
               call parsit
               if (ityp.eq.3.or.ityp.eq.4.or.
     1             (ityp.eq.2.and.ist.eq.2)) then
                   sc(10+i)=tv
               else
                   call error(7)
                   go to 99999
               endif
               if (nextyp.eq.11.or.(ifl(111).eq.1.and.
     1               nextyp.eq.7)) then
                   if (i.eq.1) then
                        call error(7)
                        go to 99999
                   else
                        isc10(2)=9
                        isc10(3)=i
                        go to 88888
                   endif
               endif
90         continue
           call error(4)
           go to 99999
      else
c                                                  *** canonical form
           do 100 i=1,12
               if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.ist
     1         .eq.2)) then
                   sc(10+i)=tv
               else
                   call error(7)
                   go to 99999
               endif
               call parsit
100        continue
           isc10(2)=1
      endif
88888 if (ifl(111).ne.0) then
           if (isc10(2).eq.1) then
               if (ityp.ne.5.or.ist.ne.7) then
                    call error(4)
                    go to 99999
               endif
           else
               if (nextyp.ne.7) then
                    call error(4)
                    go to 99999
               endif
           endif
      else if (nextyp.ne.11) then
4          call error(4)
           go to 99999
      endif
      isc10(1)=612
99999 return
      end
