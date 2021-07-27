C*********************************************************************
C*    NAME         :  declci.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       declci.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:09:52
C*********************************************************************
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine declci
C*      this routine parses circle declaration. the valid syntax
C*      constructs are:
C*          1.  (name=) circle/pt1,pt2,pt3
C*          2.  (name=) circle/pt1,ve1,pt2  (either pt may have the vector)
C*          3.  (name=) circle/x,y,<z>,1    (x,y,<opt z> radius)
C*          4.  (name=) circle/xs,ln1,ys,ln2,radius,1
C*          5.  (name=) circle/xs,in,ci1,out,ci2,radius,1
C*          6.  (name=) circle/xl,ln1,yl,out,ci1,radius,1
C*          7.  (name=) circle/center,pt1,pt2
C*          8.  (name=) circle/center,pt1,tanto,ln1
C*          9.  (name=) circle/center,pt1,large,tanto,ci1
C*         10.  (name=) circle/xl,pt1,pt2,radius,1
C*         11.  (name=) circle/center,pt1,radius,1
C*         12.  (name=) circle/tanto,ln1,xl,pt1,radius,1
C*         13.  (name=) circle/xl,ln1,yl,ln2,xl,ln3
C*         14.  (name=) circle/xl,ln1,yl,cv1,pt1,ra,1
C*         15.  (name=) circle/pt1,xl,in,ci1,ra,1
C*         16.  (name=) circle/canon,x,y,z,i,j,k,r,<a,b,c,d>
C*         17.  (name=) circle/canon,pt1,ve1,r,<pl1>
C*         18.  (name=) circle/ci1,<xval>,<yval>,<zval>
C*         19.  (name=) circle/in,ci1,yl,cv1,pt1,radius,1 
C*         20.  (name=) circle/tanto,cv1,yl,pt1,radius,1 
C*         21.  (name=) circle/center,pt1,tanto,cv1
C*         22.  (name=) circle/xs,cv1,ys,cv2,pt3,radius,1
C*         23.  (name=) circle/xl,ln1,yl,ln2,out,ci3
C*         24.  (name=) circle/xl,ln1,in,ci2,out,ci3
C*         25.  (name=) circle/out,ci1,in,ci2,out,ci3
C*         26.  (name=) circle/offset,ci1,in,dis
C*
C*      the following represents the info passed to geogn1 in syscom:
C*             sc(10)             !       sc(11) thru sc(nn)
C*--------------------------------!----------------------------------------
C* type  subtype  numwds  notused !   sc(11)       sc(12)       sc(13)
C* ----  -------  ------  ------- !   ------       ------       ------
C*  604     1       *       *     !   pt1 loc      pt2 loc      pt3 loc
C*  604     2       *       *     !   pt1 loc      ve1 loc      pt2 loc
C*  604     3    3 or 4     *     !   x value      y value      <z or rad val>
C*  604     4       *       *     !   mod num      ln1 loc      mod num-etc
C*  604     5       *       *     !   mod num      mod num      ci1 loc-etc
C*  604     6       *       *     !   mod num      ln1 loc      mod num-etc
C*  604     7       *       *     !   pt1 loc      pt2 loc         *
C*  604     8       *       *     !   pt1 loc      ln1 loc         *
C*  604     9       *       *     !   pt1 loc      lg or sm     ci1 loc
C*  604    10       *       *     !   mod num      pt1 loc      pt2 loc   ra
C*  604    11       *       *     !   pt1 loc      radius          *
C*  604    12       *       *     !   ln1 loc      mod num      pt1 loc   ra
C*  604    13       *       *     !   sc(11-16) contain 3 sets of mods and lines
C*  604    14       *       *     !   mod num      ln1 loc      mod num .....
C*  604    15       *       *     !   pt1 loc      mod num      in/out  ......
C*  604    16  6,7 or 11    *     !   x value      y value      z value ......
C*  604    17  6,7 or 11    *     !   pt1 loc      ve1 loc      radius  ......
C*  604    18    num vals   *     !   ci1 loc      xval         yval       zval
C*  604    19       *       *     !   mod num      ci1 loc      mod num .....
C*  604    20       *       *     !   cv1 loc      mod num      pt1 loc   ra
C*  604    21       *       *     !   pt1 loc      cv1 loc         *
C*  604    22       *       *     !   mod num      ci1 loc      mod num .....
C*  604    23       *       *     !   mod num      ln1 loc      mod num .....
C*  604    24       *       *     !   mod num      ln1 loc      mod num .....
C*  604    25       *       *     !   mod num      ci1 loc      mod num .....
C**************************************************************************
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C**************************************************************************
C
       subroutine declci
       include 'com8a.com'
       integer*2 lpv(3)
       integer*4 j, ncl_put_asn
       real*8 ttv
       integer*2 isub,mtv(4)
       equivalence(isub,isc10(2)), (ttv,mtv)
       integer*2 isc12(4)
       equivalence (sc(12),isc12) 
       integer*4 nclkey
       integer*2 nwds,ietype,primtyp
C*******************************
c
c... aak 21-OCT-1997:
c... made fix related to getting rid of the ranfile in the previous version:
c... sc(11),...,  are written to a buffer via "ncl_put_asn" function
c
      call ncl_free_uv
c
      isc10(4) = 0
      lpv(1) = 0
      lpv(2) = 0
      lpv(3) = 0
c
c*************** if the 1-st token is a point: cases 1,2,15
c
      if (ityp.eq.2 .and. (ist.eq.3 .or. ist .eq. 21)) then
           sc(11) = tv
           if (ist .eq. 21) lpv(1) = 1
           call parsit
           isc10(3) = 2
c
c......get the 2-nd token if it is point or vector: cases 1,2
c
           if (ityp.eq.2 .and. 
     -        (ist.eq.3 .or. ist.eq.4 .or. ist.eq.21)) then
               if (ist .eq. 21) lpv(2) = 1
               if (ist .eq. 4 .or. ist .eq. 21) then
                   isc10(2)=2
               else
                   isc10(2)=1
               endif
               sc(12)=tv
               ttv = tv
c
c......get the 2-nd token if it is a direction modifier
c......case 15: (ci/pt1,xl,in,ci,ra,1)
c
           else if (ityp.eq.1.and.(ist.gt.637.and.ist.lt.644)) then
               sc(12)=ist
               call parsit
               if (ityp.eq.1.and.(ist.eq.652.or.ist.eq.653)) then
                   sc(13)=ist
               else
                   call error(472)
                   go to 99999
               endif
               call parsit
               if (ityp.eq.2.and.ist.eq.7) then
                   sc(14)=tv
               else
                   call error(159)
                   go to 99999
               endif
               call parsit
               if (ityp.ne.1.or.ist.ne.23) then
                   call error(197)
                   go to 99999
               endif
               call parsit
               if (ityp.eq.3.or.ityp.eq.4.or.
     1             (ityp.eq.2.and.ist.eq.2)) then
                   sc(15)=tv
               else
                   call error(7)
                   go to 99999
               endif
               isc10(2)=15
               go to 88888
           else
               call error(24)
               go to 99999
           endif
c
c...check end of statement, if not
c...get 3-rd token for subtypes 1,2
c
           if (NXTEOS) then
               if (lpv(1) + lpv(2) .ne. 0) then
                   isc10(2) = 2
                   if (lpv(1) + lpv(2) .eq. 2) mtv(3) = 0 
                   sc(12) = ttv
                   go to 88888
               else
                   call error (20)
                   go to 99999
               end if
           end if
           call parsit
           ttv = tv
           isc10(3) = 3
           if (ityp.eq.2 .and. 
     -         (ist.eq.3 .or. ist.eq.4 .or. ist.eq.21)) then
               if (ist.eq.3 .or. ist.eq.21) then
                   sc(13) = tv
                   if (ist .eq. 21) then
                       isc10(2) = 2 
                       if (lpv(1)+lpv(2) .ne. 0) then 
                           call error (4)
                           go to 99999
                       else if (isc12(4) .eq. 4) then
                           mtv(3) = 0
                       else
                           mtv(3) = 3
                       end if 
                       sc(13) = ttv
                   end if 
               else if (isc12(4).eq.4) then
                   call error(20)
                   go to 99999
               else
                   sc(13) = tv
                   isc10(2) = 2
               endif
               go to 88888
           else
               call error(24)
               go to 99999
           endif
c*************** if the 1-st token is a scalar             
c*************** this code handles case 3: circle/x,y,<z>,r
c
      else if (ityp.eq.2.and.ist.eq.2.or.(ityp.eq.3.or.ityp.eq.4))
     1     then
c
           isc10(2)=3
           sc(11)=tv
           do 100 i=12,14
               call parsit
               if (ityp.eq.2.and.ist.eq.2.or.(ityp.eq.3
     1             .or.ityp.eq.4)) then
                   sc(i)=tv
               else
                   call error(7)
                   go to 99999
               endif
               if (i.eq.13.and.((ifl(111).eq.0.and.nextyp.eq.11).or.
     1             (ifl(111).ne.0.and.nextyp.eq.7))) then
                   isc10(3)=3
                   go to 88889
               endif
100        continue
           isc10(3)=4
           go to 88888
      else if (ityp.eq.1.and.(ist.gt.637.and.ist.lt.644)) then
c
c******* if the 1-st token is a direction modifier
c******* this code handles the cases
c                                   4.   circle/xs,ln1,ys,ln2,rad,dist
c                                   5.   circle/xs,in,ci1,out,ci2,rad,dist
c                                   6.   circle/xs,ln1,ys,in,ci1,rad,dist
c                                  10.   circle/xs,pt1,pt2,ra,dist
c                                  13.   circle/xs,ln1,ys,ln2,xs,ln3
c                                  14.   circle/xl,ln1,yl,cv1,pt1,ra,dist
c                                  22.   circle/xl,cv1,yl,cv2,pt3,ra,dist 
c                                  23.   circle/xl,ln1,yl,ln2,out,ci3
c                                  24.   circle/xl,ln1,in,ci2,out,ci3
c***************************************************************************
           sc(11)=ist
           call parsit
           if (ityp.eq.2.and.ist.eq.8) then
c
c...... case 22: ci tanto 2 curves with a near pt and radius
c
               isc10(2)=22
               sc(12)=tv
               call parsit
               if (ist.gt.637.and.ist.lt.644) then
                   sc(13)=ist
               else
                   call error(18)
                   go to 99999
               endif
               call parsit
               if (ityp.ne.2.or.ist.ne.8) then
                   call error(21)
                   go to 99999
               endif
               sc(14)=tv
               i=16
               call parsit
               if (ityp.eq.2 .and. (ist.eq.3 .or. ist.eq.21)) then
               sc(15)=tv      ! the 5-th token is pt or pv
c................... now go to the radius assignment part
               call parsit
                   go to 150
               else
                   call error(20)
                   go to 99999
               endif
           else if (ityp.eq.2.and.ist.eq.5) then
c
c...... if 2-nd token is a line: Cases 4,6,13,14,23,24
c
               sc(12)=tv
               call parsit
               if (ist.eq.652.or.ist.eq.653) then
c
c...... case 24: ci tangent to a line and 2 circles 
c
                   isc10(2)=24
                   sc(13)=ist
                   call parsit
                   if (ityp.eq.2.and.ist.eq.7) then
                        sc(14)=tv
                   else
                        call error(159)
                        go to 99999
                   endif
                   call parsit
                   if (ist.ne.652.and.ist.ne.653) then
                        call error(472)
                        go to 99999
                   endif
                   sc(15)=ist
                   call parsit
                   if (ityp.eq.2.and.ist.eq.7) then
                        sc(16)=tv
                        go to 88888
                   else
                        call error(159)
                        go to 99999
                   endif

               else if (ist.gt.637.and.ist.lt.644) then
                   sc(13)=ist
               else
                   call error(18)
                   go to 99999
               endif
               call parsit
               if (ityp.eq.2.and.ist.eq.5) then
c
c...... if 4-th token is a line: cases 4, 13, and 23
c
                   sc(14)=tv
                   i=15
                   call parsit
                   if (ityp.eq.1.and.(ist.gt.637.and.ist.lt.644)) then
c
c................................ case 13: circle tanto three lines
c
                     sc(15)=ist
                     call parsit
                     if (ityp.eq.2.and.ist.eq.5) then
                        sc(16)=tv
                        isc10(2)=13
                        go to 88888
                     else
                        call error(17)
                        go to 99999
                     endif
                   else 
     x             if (ityp.eq.1.and.(ist.eq.652.or.ist.eq.653)) then
c
c................................ case 23: circle/xl,ln1,yl,ln2,out,ci3
c
                     sc(15)=ist
                     call parsit
                     if (ityp.eq.2.and.ist.eq.7) then
                        sc(16)=tv
                        isc10(2)=23
                        go to 88888
                     else
                        call error(159)
                        go to 99999
                     endif
                   else 
c
c......................... case 4: circle tanto two lines with a radius
c......................... go to the radius assignment part
c
                     isc10(2)=4
                     i=15
                     go to 150
                   endif
               else if (ityp.eq.2.and.ist.eq.8) then
c
c...... else if 4-th token is a curve:  
c...... case 14: ci tanto a ln and cv near pt with ra
c
                   isc10(2)=14
                   sc(14)=tv
                   i=16
                   call parsit
                   if (ityp.eq.2 .and. (ist.eq.3 .or. ist.eq.21)) then
                       sc(15)=tv      ! the 5-th token is pt or pv
c.................... now go to the radius assignment part
                       call parsit
                       go to 150
                   else
                       call error(20)
                       go to 99999
                   endif
               else if (ist.eq.652.or.ist.eq.653) then
c
c...... case 6: ci tanto a ln and a ci with ra
c
                   sc(14)=ist
                   isc10(2)=6
                   call parsit
                   if (ityp.eq.2.and.ist.eq.7) then
                       sc(15)=tv
                       i=16
c.................... now go to the radius assignment part
                       call parsit
                       go to 150
                   else
                       call error(159)
                       go to 99999
                   endif
               else
                   call error(12)
                   go to 99999
               endif
           else if (ityp.eq.2 .and. (ist.eq.3 .or. ist .eq. 21)) then
c
c...... if 2-nd token is a point or point vector
c...... case 10: two points and radius           
c
				   isc10(2)=10
               sc(12)=tv
               call parsit
               if (ityp.eq.2 .and. (ist.eq.3 .or. ist .eq. 21)) then
                   sc(13)=tv
               else
                   call error(20)
                   go to 99999
               endif
c.............now go to the radius assignment part
               i=14
               call parsit
               go to 150
c
c...... if 2-nd token is a positional modifier, like 'in' or 'out'
c...... case 5: tanto two circles with a radius
c
           else if (ist.eq.652.or.ist.eq.653) then
				   isc10(2)=5
               sc(12)=ist
               call parsit
c
c...... the next, 3-rd, token should be circle
c
               if (ityp.eq.2.and.ist.eq.7) then
                   sc(13)=tv
               else
                   call error(159)  ! circle expected
                   go to 99999
               endif
               call parsit
               if (ist.eq.652.or.ist.eq.653) then
                   sc(14)=ist
               else
                   call error(472)
                   go to 99999
               endif
               call parsit
               if (ityp.eq.2.and.ist.eq.7) then
                   sc(15)=tv
                   i=16
                   call parsit
                   go to 150
               else
                   call error(159)  ! circle expected
                   go to 99999
               endif
           else
               call error(12)
               go to 99999
           endif
c********
      else if (ityp.eq.1.and.(ist.eq.652 .or. ist.eq.653)) then
c
c******* if the 1-st token is a position modifier
c******* this code handles the cases
c                                  19.   circle/in,ci1,yl,cv1,pt1,ra,dist 
c                                  25.   circle/out,ci1,in,ci2,out,ci3
c***************************************************************************
           sc(11)=ist
           call parsit
 
           if (ityp.eq.2.and.ist.eq.7) then
               sc(12)=tv
               call parsit
               if (ist.gt.637 .and. ist.lt.644) then
c
c...... case 19: ci tanto a ci and cv near pt with ra
c
                   isc10(2)=19
                   sc(13)=ist
                   call parsit
                   if (ityp.eq.2.and.ist.eq.8) then
                      sc(14)=tv
                   else
                       call error(21)  ! curve expected
                       go to 99999
                   endif
                   i=16
                   call parsit
                   if (ityp.eq.2 .and. (ist.eq.3 .or. ist.eq.21)) then
                       sc(15)=tv      ! the 5-th token is pt or pv
c.................... now go to the radius assignment part
                       call parsit
                       go to 150
                   else
                       call error(20)
                   endif

               else if (ist.eq.652 .or. ist.eq.653) then
c
c...... case 25:  circle tangent to three circles 
c
                   isc10(2)=25
                   sc(13)=ist
                   call parsit
                   if (ityp.eq.2.and.ist.eq.7) then
                      sc(14)=tv
                      call parsit
                      if (ist.eq.652 .or. ist.eq.653) then
                          sc(15)=ist
                          call parsit
                          if (ityp.eq.2.and.ist.eq.7) then
                             sc(16)=tv
                             goto 88888
                          else
                              call error(159)  ! circle expected
                              go to 99999
                          endif
                      else
                          call error(472)  ! modifier expected
                          go to 99999
                      endif
                   else
                       call error(159)  ! circle expected
                       go to 99999
                   endif
               else
                   call error(18)  ! modifier expected
                   go to 99999
               endif
           else
               call error(159)  ! circle expected
               go to 99999
           endif
      else if (ityp.eq.1.and. ist.eq.666) then
c
c******* if the 1-st token is OFFSET
c******* this code handles the case
c                                  26.   circle/offset,ci1,in,dis
c***************************************************************************
           isc10(2) = 26
           isc10(3) = 2
           sc(12) = 0
           call parsit
           if (ityp.eq.1 .and. (ist.eq.652 .or. ist.eq.653)) then
             sc(12) = 1
             if (ist .eq. 652) sc(12) = -1
           else 
             call error(478)  ! IN or OUT expected
             go to 99999
           endif
           call parsit
           if (ityp.eq.2.and.ist.eq.7) then
             sc(11)=tv
           else
             call error(159)  ! circle expected
             go to 99999
           endif
           call parsit
           if (scalar) then
             sc(12) = sc(12)*tv
             goto 88888
           else
            call error(7)
            goto 99999
           endif

      else if (ityp.eq.1.and.ist.eq.634) then
c                              
c*************** if the 1-st token is 'center' or 'ce'     
c*************** this code handles cases 7,8,9,11,21 
c
          call parsit
          if (ityp.eq.2 .and. (ist.eq.3 .or. ist .eq. 21)) then
c                             
c...center, pt1
c                             
              sc(11)=tv
              call parsit
              if (ityp.eq.2 .and. (ist.eq.3 .or. ist .eq. 21)) then
c                             
c...center, pt1, pt2
c                             
                  isc10(2)=7
                  sc(12)=tv
                  go to 88888
              else if (ityp.eq.1.and.ist.eq.646) then
                       call parsit
                       if (ityp.eq.2.and.ist.eq.5) then
c                             
c...center, pt1, tanto, ln1
c                             
                           isc10(2)=8
                           sc(12)=tv
                           go to 88888
                       else if (ityp.eq.2.and.ist.eq.8) then
c                             
c...center, pt1, tanto, cv1
c                             
                           isc10(2)=21
                           sc(12)=tv
                           go to 88888
                       else
                           call error(472)
                           go to 99999
                       endif
               else if(ityp.eq.1.and.
     1                 (ist.eq.662.or.ist.eq.663)) then
c                             
c...center, pt1, large, tanto, ci1
c                             
                       sc(12)=ist
                       call parsit
                       if (ityp.eq.1.and.ist.eq.646) then
                           call parsit
                           if (ityp.eq.2.and.ist.eq.7) then
                               isc10(2)=9
                               sc(13)=tv
                               go to 88888
                           else
                               call error(159)
                               go to 99999
                           endif
                       else
                           call error(193)
                           go to 99999
                       endif
              else if (ityp.eq.1.and.ist.eq.23) then
c
c...center, pt1,radius
c
                  isc10(2)=11
                  call parsit
                  if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.ist
     1                   .eq.2)) then
                       sc(12)=tv
                  else
                       call error(7)
                       go to 99999
                  endif
              else
                  call error(20)
                  go to 99999
              endif
          else
              call error(20)
              go to 99999
          endif
      else if (ityp.eq.1.and.ist.eq.646) then
c                              
c************ if the 1-st token is "tanto"      
c**********************************************************************
          call parsit
          if (ityp.eq.2 .and. ist.eq.5) then
c
c..... case 12: circle/tanto,ln1,xl,pt1,ra,1
c
              isc10(2)=12
          else if (ityp.eq.2 .and. ist.eq.8) then
c
c..... case 20: circle/tanto,cv1,yl,pt1,ra,1
c
              isc10(2)=20
          else 
              call error(12)
              go to 99999
          endif
          sc(11)=tv
          call parsit
          if (ityp.eq.1.and.(ist.gt.637.and.ist.lt.644)) then
              sc(12)=ist
          else
              call error(18)
              go to 99999
          endif
          call parsit
          if (ityp.eq.2 .and. (ist.eq.3 .or. ist .eq. 21)) then
              sc(13)=tv
          else
              call error(20)
              go to 99999
          endif
c.............now go to the radius assignment part
          i=14
          call parsit
          goto 150
      else if (ityp.eq.1.and.ist.eq.645) then
c
c*********************************  canon
c
          call parsit
          if (ityp.eq.2 .and. (ist.eq.3 .or. ist .eq. 21)) then
c
c...pt1,ve1,r,<pl1>
c
              isc10(2)=17
              isc10(3)=7
              sc(11)=tv
              if (ist .eq. 21) isc10(3) = 6
              call parsit
              if (isc10(3) .eq. 7) then
                  if (ityp.ne.2 .or. (ist.ne.4 .and. ist.ne.21)) then
                      call error(11)
                      goto 99999
                  endif
                  sc(12)=tv
                  call parsit
              end if              
              if (ityp.eq.3 .or. ityp.eq.4 .or.
     1               (ityp.eq.2 .and. ist.eq.2)) then
                  sc(13)=tv
              else
                  call error(7)
                  goto 99999
              endif
              if ((ifl(111).eq.0.and.nextyp.ne.11).or.
     1             (ifl(111).ne.0.and.nextyp.ne.7)) then
                  call parsit
                  if (ityp.ne.2 .or. (ist.ne.6.and.ist.ne.9)) then
                      call error(19)
                      goto 99999
                  endif
                  if (ist.eq.9) then
                     call gtdesc(tv,nclkey,nwds,ietype)
                     call ncl_get_sf_primtyp(nclkey,primtyp)
                     if (primtyp.ne.3) then
                       call error(19)
                       go to 99999
                     endif
                  endif
                  sc(14)=tv
                  isc10(3)=11
              endif
          else if (ityp.eq.3 .or. ityp.eq.4 .or.
     1             (ityp.eq.2 .and. ist.eq.2)) then
c
c...canon,x,y,z,i,j,k,r,<a,b,c,d>
c
              isc10(2)=16
              isc10(3)=7
              sc(11)=tv
              do 200 i=12,21
                  call parsit
                  if (ityp.eq.2.and.ist.eq.2.or.(ityp.eq.3
     1             .or.ityp.eq.4)) then
                    sc(i)=tv
                  else
                     call error(7)
                     go to 99999
                  endif
                  if (i.eq.17.and.((ifl(111).eq.0.and.nextyp.eq.11).or.
     1               (ifl(111).ne.0.and.nextyp.eq.7))) goto 88888
200           continue
              isc10(3)=11
          else
              call error(33)
              goto 99999
          endif
      else if (ityp.eq.2.and.ist.eq.7) then
c
c...ci/ci1,<xval>,<yval>,<zval>
c
          isc10(2)=18
          sc(11)=tv
          isc10(3)=0
          if (nextyp.eq.11.or.(ifl(111).eq.1
     1       .and.nextyp.eq.7)) goto 88888
c                                  *** ci/ci1
          call parsit
          if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.ist.eq.2))
     1          then
c                                  *** ci/ci1,xval,<yval>,<zval>
              do 250,i=1,3
                  sc(11+i)=tv
                  if (nextyp.eq.11.or.(ifl(111).eq.1
     1               .and.nextyp.eq.7)) then
c                             end of statement
                      isc10(3)=i
                      go to 88888
                  endif
                  call parsit
                  if (.not.(ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2
     1                .and.ist.eq.2))) then
                      call error(7)
                      go to 99999
                  endif
250           continue
              call error(4)
              go to 99999
          else
              call error(7)
              go to 99999
          endif
      else if (ityp.eq.2.and.ist.eq.1) then
9         call error(9)
          go to 99999
      else
          call error(12)
          go to 99999
      endif
      goto 88888

c
c******** the radius assignment **********
c
150        if (ityp.eq.1.and.ist.eq.23) then
               call parsit
               if (ityp.eq.2.and.ist.eq.2.or.(ityp.eq.3.or.ityp.eq.4))
     1         	then
               	   sc(i)=tv
           		else
               	   call error(7)
               	   go to 99999
               endif
           else 
               call error(197)
               go to 99999
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
88889 isc10(1)=604

c
c... aak 21-OCT-1997:
c... fix related to getting rid of the ranfile in the previous version:
c
      j = isc10(3)
      j = ncl_put_asn(sc(11),j)

99999 return
      end
