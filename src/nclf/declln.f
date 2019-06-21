C*********************************************************************
C*    NAME         :  declln.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       declln.f , 25.1 
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:09:52 
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine declln
C*     this routine parses line declarations.   
C*     the valid syntax constructs are:                              
C*        1.  (name =) line/pt1,pt2                                  
C*        2.  (name =) line/pt1,parlel,ln1                           
C*        3.  (name =) line/parlel,ln1,xlarge,1                      
C*        4.  (name =) line/fwd                                      
C*        5.  (name =) line/pt1,right,tanto,<ci or cv>               
C*        6.  (name =) line/right,tanto,ci1,left,tanto,ci2           
C*        7.  (name =) line/xaxis,<dist>                             
C*        8.  (name =) line/pt1,atangl,a,45,<line-id or xaxis>       
C*        9.  (name =) line/pt1,perpto,ln1                           
C*       10.  (name =) line/pt1,<tanto or perpto>,cv1,pt2
C*       11.  (name =) line/a,b,c,d<,e,f>
C*       12.  (name =) line/intof,pl1,pl2
C*       13.  (name =) line/xl,ci1,aa,<line>
C*       14.  (name =) line/ln,<xval,<yval,<zval>>>
C*       15.  (name =) line/pl1
C*                                                                   
C*      the following represents the info passed to geogn1 in syscom:
C*             sc(10)             !       sc(11) thru sc(nn)
C*--------------------------------!----------------------------------------
C* type  subtype  numwds  notused !   sc(11)       sc(12)       sc(13)
C* ----  -------  ------  ------- !   ------       ------       ------
C*  602     1       *       *     !  pt1 loc       pt2 loc         *
C*  602     2       *       *     !  pt1 loc       637             *
C*  602     3       *       *     !  ln1 loc       mod num      value
C*  602     4       *       *     !     *
C*  602     5       *       *     !  pt1 loc       mod num      ci1 loc
C*  602     6       *       *     !  mod num       ci1 loc      mod num....
C*  602     7     1 or 2    *     !  mod num       <dist>
C*  602     8       *       *     !  pt1 loc       angle        modnum
C*  602     9       *       *     !  pt1 loc       ln1 loc         *
C*  602    10       *       *     !  pt1 loc       mod num      cv1 loc
C*  602    11     4 or 6    *     !  sc(11) thru sc(16) = scalar values
C*  602    12       *       *     !  pl1 loc       pl2 loc         *
C*  602    13       *       *     !  mod num       ci1 loc      ang val....
C*  602    14     num vals  *     !  ln1 loc       xval         yval   ....
C*  602    15       *       *     !  pl1 loc
C* ******************************************************************
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
       subroutine declln
       include 'com8a.com'
       integer*2 isc13(4),isc14(4),n1,isv
       equivalence (isc13,sc(13))
       equivalence (isc14,sc(14))
       integer*4 nclkey
       integer*2 nwds,ietype,primtyp
c
       isv    = ist
       isc10(3) = 0
       if (ityp.eq.2.and.(isv.eq.5.or.isv.eq.6.or.isv.eq.9)) go to 300
c
c *********************************************** if its a point
c
       if (ityp.eq.2 .and. (ist.eq.3 .or. ist.eq.21)) then
           sc(11)=tv
           if (isv .eq. 21 .and. NXTEOS) go to 320 
           call parsit
           if (isv .eq. 21 .and. SCALAR) then
               isc10(3) = 1
               sc(12) = tv
               n1     = 2
               go to 340
           end if
           if (ityp.eq.2 .and. (ist.eq.3 .or. ist.eq.21)) then
               sc(12)=tv
               isc10(2)=1
           else if (ityp.eq.1.and.ist.eq.1) then
c
c                            *** atangl
c
               call parsit
               if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.
     1             ist.eq.2)) then
                   sc(12)=tv
                   call parsit
                   sc(13)=0
                   if (ityp.eq.7 .or.
     x                 (ifl(111).ne.0 .and. ityp.eq.5 .and. ist.eq.7))
     x                 then
c
c                            *** xaxis is default
c
                       isc13(4)=84
                   else if (ityp.eq.1.and.(ist.eq.84.or.ist.eq.85))
     1                 then
                       isc13(4)=ist
                   else if (ityp.eq.2.and.(ist.eq.5.or.ist.eq.21)) then
                       sc(13)=tv
                   else
                       call error(17)
                       go to 99999
                   endif
               else
                   call error(7)
                   go to 99999
               endif
               isc10(2)=8
           else if (ist.eq.637) then
c
c                          *** parlel
c
               call parsit
               if (ityp.eq.2 .and. (ist.eq.5 .or. ist.eq.21)) then
                   sc(12)=tv
                   isc10(2)=2
               else
                   call error(17)
                   go to 99999
               endif
           else if (ist.eq.8.or.ist.eq.24) then
c                           ** its a right or left
               sc(12)=ist
               call parsit
               if (ist.ne.646) then
                    call error(9)
                    go to 99999
               endif
               call parsit
               if (ityp.eq.2.and.(ist.eq.7.or.ist.eq.8)) then
                   sc(13)=tv
                   isc10(2)=5
               else
                   call error(223)
                   go to 99999
               endif
           else if (ityp.eq.1.and.(ist.eq.630.or.ist.eq.646)) then
c                            **  perpto or tanto
                    sc(12)=ist
                    call parsit
                    if (ityp.eq.2 .and. sc(12).eq.630 .and.
     -                  (ist.eq.5 .or. ist.eq.21)) then
c                            **  perpto, ln1
                        isc10(2)=9
                        sc(12)=tv
                        go to 88888
C
C...Including ist equal to 7 so that a circle may be
C...included into this command. JLS 2/19/99
C
                    else if (ityp.eq.2.and.(ist.eq.8.or.ist.eq.7)) then
c                            ** perpto or tanto curve
                        sc(13)=tv
                        call parsit
                        if (ityp.eq.2.and.(ist.eq.3.or.ist.eq.21)) then
                           sc(14)=tv
                           isc10(2)=10
                        else
                           call error(20)
                           go to 99999
                        endif
                    else
                        call error(17)
                        go to 99999
                    endif
           else if (ityp.eq.2.and.ist.eq.1) then
               call error(9)
               go to 99999
           else
               call error(12)
               go to 99999
           endif
           go to 88888
c
c *****************************if its parlel
c                                            ln/pa,ln1,modif,value
c                                            ln/pa,pv1,modif,value
c
      else if (ityp.eq.1.and.ist.eq.637) then
           call parsit
           if (ityp.eq.2 .and. (ist.eq.5 .or. ist .eq. 21)) then
               sc(11)=tv
               call parsit
               if (ist.gt.637.and.ist.lt.644) then
                   sc(12)=ist
                   call parsit
                   if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.
     1                 ist.eq.2)) then
                       isc10(2)=3
                       sc(13)=tv
                       go to 88888
                   else
7                      call error(7)
                       go to 99999
                   endif
               else
                   call error(18)
                   go to 99999
               endif
           else if (ityp.eq.2.and.ist.eq.1) then
               call error(9)
               go to 99999
           else
17             call error(17)
               go to 99999
           endif
           go to 88888

c ************************************************** if its fwd
      else if (ist.eq.651) then
           isc10(2)=4
           go to 88888
c*************************************************** if its right or left
      else if (ist.eq.8.or.ist.eq.24) then
           i=11
           isc10(2)=6
100        sc(i)=ist
           call parsit
           if (ist.ne.646) then
               call error(9)
               go to 99999
           endif
c
           call parsit
           if (ityp.eq.2.and.ist.eq.7) then
               sc(i+1)=tv
           else
               call error(159)
               go to 99999
           endif
           if (i.eq.13) go to 88888
           call parsit
           if (ist.eq.8.or.ist.eq.24) then
               i=13
               go to 100
           else
               call error(9)
               go to 99999
           endif
           go to 88888
      else if (ityp.eq.1.and.(ist.eq.84.or.ist.eq.85)) then
c                                                    *** xaxis/yaxis
          isc10(2)=7
          sc(11)=ist
          if (nextyp.eq.11.or.(ifl(111).ne.0.and.nextyp.eq.7)) then
              isc10(3)=1
          else
              call parsit
              if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4)
     1            then
                  sc(12)=tv
                  isc10(3)=2
              else
                  call error(7)
                  go to 99999
              endif
          endif
          go to 88888
      else if (ityp.eq.1.and.ist.eq.727) then
c                                                    *** io,pl1,pl2
          isc10(2)=12
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
          else
              call error(19)
              go to 99999
          endif
          go to 88888
      else if (ityp.eq.1.and.(ist.gt.637.and.ist.lt.644)) then
c
c                                       *** mod,ci1,aa,ln1
c                                       *** mod,ci1,aa,pv1
c
          isc10(2)=13
          sc(11)=ist
          call parsit
          if (ityp.eq.2.and.ist.eq.7) then
              sc(12)=tv
          else
              call error(159)
              go to 99999
          endif
          call parsit
          if (ityp.eq.1.and.ist.eq.1) then
c                                               ***  atangl
              call parsit
          else
              call error(12)
              go to 99999
          endif
          if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.
     1        ist.eq.2)) then
              sc(13)=tv
              call parsit
              sc(14)=0
              if (ityp.eq.7.or.(ifl(111).ne.0.and.ityp.eq.5.and.
     1            ist.eq.7)) then
c                      ** xaxis is default
                  isc14(4)=84
              else if (ityp.eq.1.and.(ist.eq.84.or.ist.eq.85))
     1            then
                  isc14(4)=ist
              else if (ityp.eq.2 .and. (ist.eq.5 .or. ist.eq.21)) then
                  sc(14)=tv
              else
                  call error(17)
                  go to 99999
              endif
          else
              call error(7)
              go to 99999
          endif
          go to 88888
      else if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4) then
c
c                                                  ***  line/x,y,x,y
c
         isc10(2)=11
         sc(11)=tv
         do 200 i=12,16
           call parsit
           if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4)
     1          then
               sc(i)=tv
           else
               call error(7)
               go to 99999
           endif
           if (i.eq.14) then
               if ((nextyp.eq.11.and.ifl(111).eq.0).or.
     1             (ifl(111).ne.0.and.nextyp.eq.7)) go to 210
           endif
200      continue
210      isc10(3)=i-10
         go to 88888
      else if (ityp.eq.2.and.ist.eq.1) then
9        call error(9)
         go to 99999
      else
12       call error(12)
         go to 99999
      endif
c
c ****************** if its a line or pvec
c
  300 if (isv.eq.6 .or. isv.eq.9) go to 400 
         sc(11) = tv
  320    n1     = 1
  340    isc10(2) = 14
         if (NXTEOS) goto 88888
c
c                                  *** ln/ln1,xval,<yval>,<zval>
c
         do 350,i=n1,3
             call parsit
             if (SCALAR) then
                sc(11+i)=tv
             else 
                call error(7)
                go to 99999
             endif
c                                  end of statement?
             if (NXTEOS) then
                 isc10(3)=i
                 go to 88888
             endif
350      continue
         call error(4)
         go to 99999
c     endif
c
c...                                                  ***  ln/pl1
c
  400 isc10(2) = 15
      if (ist.eq.9) then
         call gtdesc(tv,nclkey,nwds,ietype)
         call ncl_get_sf_primtyp(nclkey,primtyp)
         if (primtyp.ne.3) then
             call error(19)
             go to 99999
         endif
      endif
      sc(11) = tv
c
88888 if (ifl(111).eq.0) then
          if (nextyp.ne.11) then
4             call error(4)
              go to 99999
          endif

c          this is a nested definition that should end with a ')' (nextyp=7)
      else if (nextyp.ne.7) then

c              check if ')' has already been parsed - if not, give error
          if (.not.(ityp.eq.5 .and. ist.eq.7)) then 
               call error(4)
               go to 99999
          endif
       endif
       isc10(1)=602
99999  return
       end
