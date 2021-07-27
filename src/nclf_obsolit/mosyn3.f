
C*********************************************************************
C*    NAME         :  mosyn3.f
C*       CONTAINS:
C*           gosyn
C*
C*    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       mosyn3.f , 25.3
C*    DATE AND TIME OF LAST  MODIFICATION
C*       10/27/16 , 13:53:17
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gosyn
c*      This routine handles the parsing and syntax checking for
c*      the GO command.  It builds the syscom in preparation for
c*      the motion generation section in the following format:
c*
c*         isc10(1) = ist of GO vocabulary word (702).
c*         isc10(3) = 1 = Feed rate specified
c*         isc10(4) = 1 = No Part Surface specfied (NOPS)
c*         sc(11)   = DS condition
c*         sc(12)   = Drive Surface
c*         ifl(342) = PS condition
c*         sc(13)   = Part Surface
c*         sc(14)   = CS condition
c*         sc(15)   = Drive Surface
c*         sc(16)   = DS Offset
c*         sc(17)   = PS Offset
c*         sc(18)   = CS Offset
c*         isc19(1) = 0 = ENTRY, 1 = EXIT
c*         isc19(2) = 1 = RTRCTO specified
c*         isc19(3) = 1 = RAPTO specified
c*         sc(20:22)     = Retract distance
c*         sc(202:204)   = Rapto distance
c*
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
      subroutine gosyn
c
      include 'com8a.com'
      include 'prfcom.com'
      include 'rrdm.com'
      include 'mocom.com'
c
      integer*2 ktv(4)
      integer*4 jtv(2)
      equivalence (tv,jtv,ktv)
c
      integer*2 isc19(4)
      equivalence (sc(19),isc19)
c
      integer*2 isc133(4)
      real*4 r4133(2)
      equivalence (sc(133),isc133,r4133)
c
      integer*2 ierr
c
      integer*2 START,OFFSET,RAPTO,RTRCTO,VOCEND,TO,ON,PAST,NOPSV
c
      parameter (NOPSV  =  726)
      parameter (OFFSET =  666)
      parameter (ON     =   71)
      parameter (PAST   =  715)
      parameter (RAPTO  =  280)
      parameter (RTRCTO =  295)
      parameter (START  =   57)
      parameter (TO     =  714)
      parameter (VOCEND =  499)
c
c...Check for Slash (/)
c
      if (nextyp.ne.5) go to 9022
c
c...Initialize routine
c
      do 100 i=10,22,1
        sc(i) = 0
  100 continue
      do 110 i=202,206,1
        sc(i) = 0
  110 continue
      isc10(1) = ist
      isc10(3) = 0
      isc19(1) = 0
      isc19(2) = 0
      isc19(3) = 0
      sc(11)=714
      ifl(44) = 9
C
C...Get DS Condition
C
      call parsit
      if (ist .eq. PAST .or. ist .eq. ON .or. ist .eq .TO) then
        sc(11)=ist
        call parsit
      endif
c
c...Get DS
c
      if (ityp.eq.2.and.(ist.gt.4.and.ist.lt.10)) then
        sc(12)=tv
        if (ist.eq.8.or.ist.eq.9) then
          ierr = ist
          call parsuv(ierr,dsuv,dsu,dsv)
          if (ierr.gt.0) goto 9000
        endif
      else
        go to 9028
      endif
c
c...Get PS condition
c
      if (nextyp.ne.11) then
        call parsit
        if (ist .eq. ON .or. ist .eq. TO) then
          if (ist .eq. ON) then
              ifl(342) = 1
          endif
          if (ist .eq. TO) then
              ifl(342) = 0
              fautops = .false.
          endif
          call parsit
        endif
c
c...Get PS
c
        isc10(4) = 0
c
c......NOPS
c
        if (vocab .and. ist.eq.NOPSV) then
          isc10(4) = 1
          fautops = .false.
c
c......Plane, Surface, Spline
c
        else if (ityp.eq.2.and.
     1      (ist.eq.PLANE.or.ist.eq.SURF.or.ist.eq.CURVE)) then
          sc(13)=tv
          if (ist.eq.SURF) then
            ierr = ist
            call parsuv(ierr,psuv,psu,psv)
            if (ierr.gt.0) goto 9000
          endif
        else if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3
     1      .or.ityp.eq.4) then
C
C......Tag on feed rate
C
          isc10(3)=1
          goto 500
        else
          go to 9122
        endif
c
c...Get CS condition
c
        if (nextyp.ne.11) then
          call parsit
          sc(14)=714
          if (ist .eq. PAST .or. ist .eq. ON .or. ist .eq .TO) then
            sc(14)=ist
            call parsit
          endif
c
c...Get CS
c
          if (ityp.eq.2.and.(ist.gt.4.and.ist.lt.10)) then
            sc(15)=tv
            if (ist.eq.8.or.ist.eq.9) then
              ierr = ist
              call parsuv(ierr,csuv,csu,csv)
              if (ierr.gt.0) goto 9000
            endif
          else if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3
     1        .or.ityp.eq.4) then
C
C......Tag on feed rate
C
            isc10(3)=2
            goto 500
          else
            go to 9030
          endif
          isc10(3)=3
        else
          isc10(3)=2
        endif
      else
        isc10(3)=1
      endif
c
c....Check for extended GO statement or
c....tagged on feed rate
c
  500 if (nextyp .ne. 11) then
        call parsit
       if (ityp.eq.3.or.ityp.eq.4.or.(ityp.eq.2.and.ist.eq.2)) then
          if (ifl(215).eq.3.or.ifl(215).eq.2) then
            rpfron = .false.
            isc133(1)=1
            isc133(2)=0
            r4133(2)=tv
          else
            rpfron = .false.
            sc(123)=tv
            call putcl (2000,1009,2,tv)
          endif
          call parsit
        endif
        if (ityp .eq. 7) go to 8000
c
c...Parse other parameters
c
        go to 650
  600   if (nextyp .eq. 11) go to 8000
        call parsit
  650   if (ityp .eq. 7) go to 8000
        if (ityp .ne. 1) go to 9232
c
c...START
c
        if (ist .eq. START) then
          isc19(1) = 0
          go to 600
c
c...END
c
        else if (ist .eq. VOCEND) then
          isc19(1) = 1
          go to 600
c
c...OFFSET
c
        else if (ist .eq. OFFSET) then
          inc    = 16
  700     if (nextyp .eq. 11) go to 8000
          call parsit
  750     if (ityp .eq. 7) go to 8000
c
c......OFFSET,thick
c
          if (scalar) then
            sc(inc) = tv
            inc    = inc    + 1
            go to 700
          else
            go to 650
          endif
c
c...RTRCTO
c
        else if (ist .eq. RTRCTO) then
          if (nextyp .eq. 11) go to 9328
          call parsit
          if (ityp .eq. 7) go to 9328
c
c......RTRCTO,dis
c
          if (scalar) then
            isc19(2) = 1
            sc(20) = tv
c
c......RTRCTO,i,j,k
c
            call parsit
            if (scalar) then
              isc19(2) = 2
              sc(21) = tv
              call parsit
              if (.not. scalar) go to 9007
              sc(22) = tv
            else
              go to 650
            endif
c
c......RTRCTO,pl/sf
c
          else if (geom) then
            if (ist .ne. 6 .and. ist .ne. 9) go to 9328
            isc19(2) = 3
            sc(20) = tv
          else
            go to 9328
          endif
          go to 600
c
c...RAPTO
c
        else if (ist .eq. RAPTO) then
          if (nextyp .eq. 11) go to 9328
          call parsit
          if (ityp .eq. 7) go to 9328
c
c......RAPTO,dis
c
          if (scalar) then
            isc19(3) = 1
            sc(202) = tv
c
c.....RAPTO,i,j,k
c
            call parsit
            if (scalar) then
              isc19(3) = 2
              sc(203) = tv
              call parsit
              if (.not. scalar) go to 9007
              sc(204) = tv
            else
              go to 650
            endif
c
c......RAPTO,pl/sf
c
          else if (geom) then
            if (ist .ne. 6 .and. ist .ne. 9) go to 9328
            isc19(3) = 3
            sc(202) = tv
          else
            go to 9328
          endif
          go to 600
c
c......Invalid minor word
c
        else
          go to 9182
        endif
c.....from label 500        
      else
c...........Output recent fedrat          
            rpfron = .false.
             sc(123)=tv
            !if ((tv .eq. tv) .and. (tv .gt. 0.000001)) then
            if (tv .gt. 0.0001) then
                !sc(123)=tv
                call putcl (2000,1009,2,sc(123))
                sc(223)=sc(123)
            endif
      endif
      if (nextyp.ne.11) go to 9004
c
c...End of routine
c
 8000 return
c
c...Error reported
c
 9000 call error (ierr)
      go to 8000
c
c...End of statement expected
c
 9004 call error (4)
      go to 8000
c
c...Number expected
c
 9007 call error (7)
      go to 8000
c
c...Slash expected (/)
c
 9022 call error (22)
      go to 8000
c
c...Invalid drive surface
c
 9028 call error (28)
      go to 8000
c
c...Invalid check surface
c
 9030 call error (30)
      go to 8000
c
c...Invalid part surface
c
 9122 call error (122)
      go to 8000
c
c...Comma expected
c
 9057 call error (57)
      go to 8000
c
c...Plane or Surface expected
c
 9182 call error (182)
      go to 8000
c
c...Vocabulary word expected
c
 9232 call error (232)
      go to 8000
c
c...Plane or Surface expected
c
 9328 call error (5)
      go to 8000
      end
