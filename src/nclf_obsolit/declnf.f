C*********************************************************************
C*    NAME         :  declnf.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       declnf.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:53
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine declnf
C*      this routine parses NURB surf declarations. the valid syntax
C*      combinations are:
C*          1. (name =) nsurf/edge,[,type],interp,cv1,...,cv4
C*          2. (name =) nsurf/revolv,cv,pt,ve,sang[,eang]
C*          3. (name =) nsurf/<fit,>cv1,cv2,thru,cv5,cv6...
C*      the following represents the info passed to geogn2 in syscom:
C*             sc(10)             !       sc(11) thru sc(nn)
C*--------------------------------!----------------------------------------
C* type  subtype  numwds  flag    !   sc(11)       sc(12)       sc(13-n)
C* ----  -------  ------  ------- !   ------       ------       --------
C*  614     1   num elms   type   !   interp       sc(12)-sc(15) = elements
C*  614     2   num elms    *     !     cv           pt         ve-eang
C*  614     3   num elms  1 = fit !     sc(11)-sc(nn) = elements (max=50)
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
      subroutine declnf
c
      include 'com8a.com'
c
      integer*4 inc,inum1,inum2,spt,jpt,mincv,maxcv,item
      integer*2 rpt
      real*8 jbuff(36)
      integer*2 DISK
      parameter (DISK=1097)


      sc(10) = 0
c
c...s1=NSURF/EDGE
c
      if (ityp .eq. 1 .and. ist .eq. 860) then
c
c......Get optional TYPE of surface
c......
c......   1 = 4 Curve (Only one supported now)
c......
c......and Interpolation format
c......
c......   1 = Cubic
c......   2 = Linear
c
          inum1  = 1
          call parsit
          if (.not. scalar) then
              call error (7)
              goto 99999
          endif
          inum2  = tv
          call parsit
          if (scalar) then
              inum1  = inum2
              inum2  = tv
              call parsit
          endif
          if (inum1 .ne. 1 .or. inum2 .lt. 1 .or. inum2 .gt. 2) then
              call error (61)
              goto 99999
          endif
c
c......Get the input curves
c......Must be 4 at this time
c
          inc    = 11
          item   = 0
          if (inum1 .eq. 1) then
              mincv  = 4
              maxcv  = 4
          endif
  100     item   = item   + 1
          if (item .gt. maxcv) then
              call error (4)
              go to 99999
          endif
          if (ityp .ne. 2 .or. (ist .ne. 5 .and. ist .ne. 7 .and.
     1        ist .ne. 8)) then
              call error (21)
              goto 99999
          endif
          if (err) go to 99999
c
c.........Store the curve in the 'sc' array
c
          inc    = inc    + 1
          sc(inc) = tv
c
c.........Get the next token
c
          if (.not.nxteos) then
              call parsit
              if (err) go to 99999
              go to 100
          endif
c
c......End of statement
c......Make sure the correct number
c......of curves are present
c
          if (item .lt. mincv) then
              call error (21)
              go to 99999
          endif
c
c......Store this command for later processing
c......by the GEO GEN routines
c
          isc10(1) = 614
          isc10(2) = 1
          isc10(3) = item
          isc10(4) = inum1
          sc(11) = inum2
c
c...s1=NSURF/REVOLV
c
      else if (ityp .eq. 1 .and. ist .eq. 861) then
c
c......Get curve to revolve
c
          call parsit
           if (ityp .ne. 2 .or. (ist.ne.POINT .and. ist.ne.PNTVEC
     x         .and.ist.ne.5 .and. ist.ne.7 .and. ist.ne.8)) then
              call error (21)
              go to 99999          
          endif
c
c.....Get center point for revolution
c
          sc(11) = tv
          call parsit
c
c.....Get radii
c
          if (scalar .and. tv.gt.1.d-4) then
            isc10(1) = 614            
            isc10(2) = 2
            isc10(3) = 2
            sc(12) = tv
            if (.not. nxteos) then
              call parsit
              if (scalar .and. tv.gt.1.d-4) then
                isc10(3) = 3
                sc(13) = tv
                if (.not.nxteos) call parsit
              endif
            endif
            goto 99999
c
c..... syntax NS/REV,cv1,(pv|pt,ve|pt,pv),a,b
c
          else if (ityp .ne. 2 .or. (ist.ne.3 .and. ist.ne.21 .and.
     x                                              ist.ne.LINE)) then
            call error (20)
            go to 99999
          endif
          sc(12) = tv
          sc(13) = 0.0 
          if (ist .ne. 21 .and. ist.ne.LINE) then 
c
c.....Get vector of rotation
c.....vp 12-apr-93 PV added
c
             call parsit
             if (ityp .ne. 2 .or. (ist .ne. 4 .and. ist .ne. 21)) then
                 call error (11)
                 go to 99999
             endif
             sc(13) = tv
          end if
c
c......Get starting angle
c
          if (nxteos) then
            sc(14) = 0.
            sc(15) = 360.
          else
            call parsit
            if (.not. scalar) then
              call error (3)
              goto 99999
            endif
            sc(14) = tv
c
c......Get ending angle
c
            call parsit
            if (.not. scalar) then
                call error (3)
                goto 99999
            endif
            sc(15) = tv
          endif
c
c......End of statement
c......Store this command for later processing
c......by the GEO GEN routines
c
          if (.not. nxteos) then
              call error (4)
              goto 99999
          endif
          isc10(1) = 614
          isc10(2) = 2
          isc10(3) = 5
c
c...s1=NSURF/REVOLV
c
      else if (ityp .eq. 1 .and. ist .eq. DISK) then
c
c......Get first geometry item
c
        call parsit
        if (ityp .ne. 2 .or. (ist.ne.POINT .and. ist.ne.PNTVEC .and.
     x                        ist.ne.LINE .and. ist.ne.CIRCLE)) then
          call error (192)
          goto 99999          
        endif

        isc10(1) = 614            
        isc10(2) = 2
        isc10(3) = 1
c
c.....Get center point for revolution
c
        sc(11) = tv
        if (ist .ne. CIRCLE) then
          call parsit
c
c.....Get radius
c
          if (scalar) then
            isc10(4) = 1
          else if (ityp .eq. 2 .and. (ist.eq.3 .or. ist.eq.21)) then
            isc10(4) = 2
          else
            call error (20)
            go to 99999
          endif
          sc(12) = tv
        endif
      else if (ityp .eq. 1 .and. ist .ne. 834) then
          call error (5)
          goto 99999
c
c...s1=NSURF/(FIT),cvs
c
      else
          mincv = 2
          maxcv = 10000
c
c......Check for FIT
c
          isc10(4) = 0
          if (ityp .eq. 1 .and. ist .eq. 834) then
              isc10(4) = 1
              mincv  = 3
              idtype = 8
              call parsit
              if (err) go to 99999
          endif
c
c......Get the input curves
c
          spt    = 11
          jpt    = 0
          rpt    = ifl(4) + 1
          item   = 0
c
c......Valid items for FIT are
c......Lines, Circles, Curves
c
  500     if (isc10(4) .eq. 1) then
              if (ityp .ne. 2 .or. (ist .ne. 5 .and. ist .ne. 7 .and.
     1            ist .ne. 8)) then
                  call error (21)
              endif
c
c......Valid items for NON-FIT are
c......0s, Points, Lines, Circles, and Curves
c
          else
              if ((ityp .eq. 3 .and. tv .ne. 0) .or. (ityp .eq. 2 .and.
     1            (ist .le. 2 .or. ist .gt. 9 .or. ist .eq. 6) .and.
     2             ist .ne. 21)) then
                  call error (21)
                  go to 99999
              endif
          endif
c
c......Check for too many curves
c
          item   = item   + 1
          if (item .gt. maxcv) then
              call error (4)
              go to 99999
          endif
c
c......Store the curve in the 'sc' array
c
          if (spt .lt. 23) then
              sc(spt) = tv
              spt    = spt    + 1
              if (item.eq.2 .and. scalar .and. tv.gt.1.d-4) then
                isc10(1) = 614
                isc10(2) = 2
                isc10(3) = 2
                if (.not. nxteos) then
                  call parsit
                  if (scalar .and. tv.gt.1.d-4) then
                     isc10(3) = 3
                     if (.not.nxteos) call parsit
                  endif
                endif
                return
              endif
c
c......The 'sc' array is full
c......Store the curve in the ranfile
c
          else
              jpt    = jpt    + 1
              if (jpt .gt. 35) then
                  call putran (jbuff,rpt)
                  rpt    = rpt    + 1
                  jpt    = 1
              endif
              jbuff(jpt) = tv
          endif
c
c......Parse the next token
c
          if (.not. nxteos) then
              idtype = 8
              call parsit
              if (err) go to 99999
              go to 500
          endif
c
c......End of statement
c......Make sure correct number of curves
c......were specified
c
          if (item .lt. mincv) then
              call error (21)
              go to 99999
          endif
c
c......Flush buffer to ranfile if necessary
c
          if (jpt .gt. 0) call putran (jbuff,rpt)
c
c......Store this command for later processing
c......by the GEO GEN routines
c
          isc10(1) = 614
          isc10(2) = 3
          isc10(3) = item
      endif
c
c...End of routine
c
99999 return
      end
