C*********************************************************************
C*    NAME         :  dist.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       dist.f , 25.1
C*     DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:56
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dist
C*      this routine handles the dist statement.  the valid syntax
C*      constructs are:
C*
C*           a=dist/pt1,pt2
C*           a=dist/pt1,pl1
C*           a=dist/pl1,pl2
C*           a=dist/pt1,sf1         epm   6-25-85
C*           a=dist/ln1,ln2         epm   11-6-87
C*           a=dist/pv1,pv2         vvp   9-30-91
C*
C*      in the third case, the planes must be parallel.  at this time,
C*      12-22-83, a dist statement may not be nested, as it is treated
C*      as a normal named scalar assignment. the resulting distance is
C*      returned in tv.
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
       subroutine dist

       include 'com.com'

      if (sc(169).lt.9.549) then
          call disto
      else
          call distn
      endif
      return
      end

C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine distn
C*      this routine handles the dist statement.
C*      function used after V9.5
C*     The valid syntax constructs are:
C*
C*           a=dist/pt1,pt2
C*           a=dist/pt1,pl1
C*           a=dist/pl1,pl2
C*           a=dist/sf1,sf2
C*           a=dist/pt1,sf1         epm   6-25-85
C*           a=dist/ln1,ln2         epm   11-6-87
C*           a=dist/pv1,pv2         vvp   9-30-91
C*
C*      in the third case, the planes must be parallel.  at this time,
C*      12-22-83, a dist statement may not be nested, as it is treated
C*      as a normal named scalar assignment. the resulting distance is
C*      returned in tv.
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
       subroutine distn

       include 'com8a.com'
       include 'suvcom.com'

       real*8 tr(2)
       integer*2 it(2),ksn(4),nwds,ietype
       integer*2 ipt
       integer*4 nclkey
       equivalence (ksn,tv)

       do 100,i=1,2
           call parsit
           if (ityp .ne. 2 .or.
     x        (ist .ne. 3 .and.
     x         ist .ne. 4 .and.
     x         ist .ne. 5 .and.
     x         ist .ne. 6 .and.
     x         ist .ne. 7 .and.
     x         ist .ne. 8 .and.
     x         ist .ne. 21 .and.
     x         ist .ne. 9 .and.
     x         ist .ne. 33)) go to 9345
            it(i)=ist
            tr(i)=tv
c
c...if it is a curve, it allow only curve itself or 2 entities
c
            if (ist.eq.8) then
              if (i.eq.1) then
c
c...check if there is only one entiry
c
                  if (nextyp.eq.11) then
                     goto 200
                  else
                     goto 100
                  endif
              endif
            endif
50          if (ist.eq.9) then
              iret = 9
              call parsuv(iret,dsuv,dsu,dsv)
              if (iret.gt.0) then
                ifl(2) = iret
                goto 99999
              endif
            endif
100    continue
       call distf (tr(1), tr(2))
       go to 99999

200    continue
       ipt=20
       if (nextyp.ne.11) then
           call parsit
           if ((ityp.eq.2.and.ist.eq.2)
     1         .or.ityp.eq.3.or.ityp.eq.4) then
              ipt=tv
           else
              ifl(2)=4
              go to 99999
           endif
       endif
       call crvlen (tr(1), ipt, tv, ifl(2))
       go to 99999
9345   ifl(2)=345

99999  return
       end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine disto
C*      this routine handles the dist statement.  the valid syntax
C*      constructs are:
C*      function used before V9.6
C*
C*           a=dist/pt1,pt2
C*           a=dist/pt1,pl1
C*           a=dist/pl1,pl2
C*           a=dist/pt1,sf1         epm   6-25-85
C*           a=dist/ln1,ln2         epm   11-6-87
C*           a=dist/pv1,pv2         vvp   9-30-91
C*
C*      in the third case, the planes must be parallel.  at this time,
C*      12-22-83, a dist statement may not be nested, as it is treated
C*      as a normal named scalar assignment. the resulting distance is
C*      returned in tv.
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
       subroutine disto

       include 'com8a.com'
       include 'suvcom.com'

       real*8 r(6,2),co,tr(2),sec1,sec2,rho,cosd,den,dx,dy,dz
       integer*2 it(2),ksn(4)
       equivalence (ksn,tv)
       integer*4 nclkey
       integer*2 nwds, ietype
       logical trflg

       trflg = .true.
       do 100,i=1,2
           call parsit
           if (ityp .ne. 2 .or.
     x        (ist .ne. 3 .and.
     x         ist .ne. 5 .and.
     x         ist .ne. 6 .and.
     x         ist .ne. 8 .and.
     x         ist .ne. 21 .and.
     x         ist .ne. 9)) go to 9345
            it(i)=ist
            tr(i)=tv
            if(ist.eq.3.or.ist.eq.5.or.ist.eq.6 .or. ist.eq.21) then
c                its a point, line or plane
               call gtentt(tv, trflg, nclkey, ietype, r(1,i))
            endif
            if (ist.eq.8) then
              if (i.eq.1) goto 200
              goto 9345
            endif
50          if (ist.eq.9) then
              call gtdesc(tv,nclkey,nwds,ietype)
              call ncl_get_sf_primtyp(nclkey,ietype)
              if (ietype.eq.3) then
                call gtplt(tv, ifl(72), r(1,i))
                it(i) = PLANE
              endif
              iret = 9
              call parsuv(iret,dsuv,dsu,dsv)
              if (iret.gt.0) then
                ifl(2) = iret
                goto 99999
              endif
            endif
100    continue

      if ((it(1).eq.3 .or. it(1) .eq. 21) .and.
     -    (it(2).eq.3 .or. it(2) .eq. 21)) then
c                dist between two points (or pvs)
           tv=dsqrt((r(1,1)-r(1,2))**2+(r(2,1)-r(2,2))**2+
     1          (r(3,1)-r(3,2))**2)
       else if (it(1).eq.6.and.it(2).eq.6) then
c                dist between two planes
           co=r(1,1)*r(1,2)+r(2,1)*r(2,2)+r(3,1)*r(3,2)
           if (dabs(co).gt..9999995d0) then
c                 the planes are parallel
               if (co.le.0) r(4,2)=-r(4,2)
               tv=abs(r(4,1)-r(4,2))
           else
               ifl(2)=250
               go to 99999
           endif
       else if (it(1).eq.9.or.it(2).eq.9) then
c                its a dist between a point and a surface, and
c                they may be in either order
           if (it(1)+it(2).ne.12 .and. it(1)+it(2).ne.30) then
               ifl(2)=61
               go to 99999
           else
               if (it(1).eq.9) then
                   sc(11)=tr(2)
                   sc(12)=tr(1)
               else
                   sc(11)=tr(1)
                   sc(12)=tr(2)
               endif
               isc10(1)=575
               isc10(2)=1
           endif
       else if (it(1).eq.5.and.it(2).eq.5) then
c                its a dist between two lines
c                make sure they are parlel
           sec1=sqrt(r(4,1)**2+r(5,1)**2+r(6,1)**2)
           sec2=sqrt(r(4,2)**2+r(5,2)**2+r(6,2)**2)
           cosd=(r(4,1)*r(4,2)+r(5,1)*r(5,2)+r(6,1)*r(6,2))/
     1            (sec1*sec2)
           if (abs(cosd).lt..999999999d0) then
               call error(344)
               go to 99999
           endif
           den=r(4,2)**2+r(5,2)**2+r(6,2)**2
           rho=(r(4,2)*(r(1,1)-r(1,2))+r(5,2)*(r(2,1)-r(2,2))+
     1          r(6,2)*(r(3,1)-r(3,2)))/den
           dx=r(1,2)+rho*r(4,2)-r(1,1)
           dy=r(2,2)+rho*r(5,2)-r(2,1)
           dz=r(3,2)+rho*r(6,2)-r(3,1)
           tv=sqrt(dx**2+dy**2+dz**2)

       else
c                must be distance between a point and a plane.
c                note that they may be in either order
           if ((it(1).eq.3.or.it(1).eq.21).and.it(2).eq.6) then
               ipt=1
               ipl=2
           else if (it(1).eq.6.and.(it(2).eq.3.or.it(2).eq.21)) then
               ipt=2
               ipl=1
           else
               goto 9345
           endif
           tv=abs(r(4,ipl)-(r(1,ipl)*r(1,ipt)+r(2,ipl)*
     1           r(2,ipt)+r(3,ipl)*r(3,ipt)))
       endif
       go to 99999
c                                  dist/cv (length of curve
200    continue
       ipt=20
       if (nextyp.ne.11) then
           call parsit
           if ((ityp.eq.2.and.ist.eq.2)
     1         .or.ityp.eq.3.or.ityp.eq.4) then
              ipt=tv
           else
              ifl(2)=4
              go to 99999
           endif
       endif
       call crvlen (tr(1), ipt, tv, ifl(2))
       go to 99999

9345   ifl(2)=345

99999  return
       end
