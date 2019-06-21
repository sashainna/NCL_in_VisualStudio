C********************************************************************/
C*    NAME         :  clword.f
C*       CONTAINS:
C*        clpwd   clspwd  clcyc   clcyc1  clcycl  clfed   clret
C*        clspn   clcool  clcutc  clldtl  clmach  clmode  clturr
C*        claptc
C*
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       clword.for , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       10/19/15 , 17:24:07
C********************************************************************/
C
c
c***********************************************************************
c
c   SUBROUTINE:  clpwd (rclw,iflg,iwrd,val)
c
c   FUNCTION:  This routine parses a minor word/value parameter and
c              returns the type (MINOR/VALUE) and it's value.
c
c   INPUT:  rclw    R*8  D1  -  Post processor command parameter.
c
c
c   OUTPUT: iflg    I*2  D1  -  0 = This value contains a minor word.
c                               1 = This is a numeric value.
c
c           iwrd    I*2  D1  -  Minor word value when iflg = 0.
c
c           val     R*8  D1  -  Value when iflg = 1 (same as rclw).
c
c***********************************************************************
c
      subroutine clpwd (rclw,iflg,iwrd,val)
c
      integer*2 iflg,iwrd
c
      real*8 rclw,val
c
      integer*2 inum(4),is1,is4
c
      real*8 rnum
c
      equivalence (rnum,inum)
      data is1 /4/, is4 /1/
c
c...Determine if minor word
c
      rnum = rclw
      if (inum(is1) .eq. 0 .and. inum(is4) .ne. 0) then
          iflg = 0
          iwrd = inum(is4)
          val = 0.
      else
          iflg = 1
          val = rclw
          iwrd = 0
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clspwd (rclw,iflg,iwrd,val)
c
c   FUNCTION:  This routine formats and stores a minor word/value parameter
c              according to its type in a single R*8 value.
c
c   INPUT:  rclw    R*8  D1  -  Post processor command parameter.
c
c
c   INPUT:  iflg    I*2  D1  -  0 = This value contains a minor word.
c                               1 = This is a numeric value.
c
c           iwrd    I*2  D1  -  Minor word value when iflg = 0.
c
c           val     R*8  D1  -  Value when iflg = 1 (same as rclw).
c
c   OUTPUT: rclw    R*8  D1  -  Post processor command parameter.
c
c***********************************************************************
c
      subroutine clspwd (rclw,iflg,iwrd,val)
c
      integer*2 iflg,iwrd
c
      real*8 rclw,val
c
      integer*2 inum(4),is1,is4
c
      real*8 rnum
c
      equivalence (rnum,inum)
      data is1 /4/, is4 /1/
c
c...Determine if minor word
c
      if (iflg .eq. 0) then
          inum(is1) = 0
          inum(is4) = iwrd
      else
          rnum = val
      endif
      rclw = rnum
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clcyc (rclw,mxcl,gcnv,iflg,icyc,rcyc)
c
c   FUNCTION:  This routine parses a CYCLE command from a clfile and
c              returns the programmed cycle parameters.
c
c   INPUT:  rclw    R*8  Dn  -  Post processor command parameters from
c                               clfile.
c
c           mxcl    I*4  D1  -  Number of parameters in 'rclw'.
c
c           cnv     R*8  D1  -  Metric conversion factor.
c
c   OUTPUT: iflg    I*2  D1  -  0 = This command was ignored.  1 = This
c                                   was a valid command.
c
c           icyc    I*2  D10 -  Integer cycle parameters.
c
c           rcyc    R*8  D10 -  Real cycle parameters.
c
c***********************************************************************
c
c...ICYC(1) = 0 = Cycle off. 1 = Cycle on.
c...     2  = Cycle mode.  1 = DRILL, 2 = BORE, 3 = DEEP, 4 = THRU,
c...          5 = BORE9, 6 = SHIFT, 7 = MILL.
c...     3  = Retract specified.
c...     4  = Feed rate mode.  0 = None specified, 1 = IPM, 2 = IPR,
c...          3 = TPI.
c...     5  = 1 = Clearance plane is defined.
c...
c...RCYC(1) = Fedto parameter.
c...     2  = Rapto parameter.
c...     3  = Step 1 parameter.
c...     4  = Step 2 parameter.
c...     5  = Retract parameter.
c...     6  = Feed rate.
c...     7  = Retract distance.
c
      subroutine clcyc (rclw,mxcl,cnv,iflg,icyc,rcyc)
c
      include 'com8a.com'
c
      integer*2 icyc(10),iflg
      integer*4 mxcl
c
      real*8 rclw(10),rcyc(10),cnv
c
      integer*2 ic(10),ipsw(50),inum(4),ifed,ip(70),inc,ist1,is1,is4
c
      real*8 rnum,rc(10),rp(70)
c
      equivalence (rnum,inum)
      data is1 /4/, is4 /1/
c
c...Initialize routine
c
cc      cnv    = 1.
cc      if (ifl(264) .ne. 0) cnv = 25.4
      iflg   = 1
      do 100 i=1,10,1
          ic(i) = 0
          rc(i) = 0.
  100 continue
      do 200 i=1,mxcl,1
          rnum   = rclw(i)
          if (inum(is1) .eq. 0 .and. inum(is4) .ne. 0) then
              ipsw(i) = inum(is4)
          else
              ipsw(i) = 0
          endif
  200 continue
c
c...Ignored CYCLE commands
c...
c......CYCLE/OPTION
c......      AUTO
c......      AVOID (for now)
c
      if (ipsw(1) .eq. 144 .or. ipsw(1) .eq. 88 .or.
     1    ipsw(1) .eq. 327 .or. ipsw(1) .eq. 0) go to 9000
c
c...CYCLE/ON
c
      if (ipsw(1) .eq. 71) then
          ic(1) = 1
c
c...CYCLE/OFF
c
      else if (ipsw(1) .eq. 72) then
          ic(1) = 0
c
c...CYCLE/mode
c
      else
          if (mxcl .lt. 5) go to 9000
          ic(1) = 1
          if (ipsw(1) .eq. 151) ic(1) = 2
          ic(2) = 1
          if (ipsw(1) .eq. 82 .or. ipsw(1) .eq. 211 .or.
     1        ipsw(1) .eq. 262 .or. ipsw(1) .eq. 1008 .or.
     2        ipsw(1) .eq. 168) then
              ic(2) = 2
          else if (ipsw(1) .eq. 153) then
              ic(2) = 3
          else if (ipsw(1) .eq. 152) then
              ic(2) = 4
          else if (ipsw(1) .eq. 213) then
              ic(2) = 5
          else if (ipsw(1) .eq. 249) then
              ic(2) = 6
          else if (ipsw(1) .eq. 151) then
              ic(2) = 7
          else if (ipsw(1) .eq. 75) then
              ic(2) = 8
              call clcyc1 (ipsw,rclw,mxcl,ic,rc,cnv,iflg)
              goto 1900
          endif
c
c......CYCLE/mode,fedto,feed,IPM,rapto
c
          if (ipsw(2) .eq. 0) then
c
c.........Find IPM/IPR/TPI location
c
              do 500 i=3,mxcl,1
                  if (ipsw(i) .eq. 73 .or. ipsw(i) .eq. 74 .or.
     1                ipsw(i) .eq. 143) then
                      ifed   = i
                      ic(4) = 1
                      if (ipsw(i) .eq. 74) ic(4) = 2
                      if (ipsw(i) .eq. 143) ic(4) = 3
                      go to 600
                  else if (ipsw(i) .ne. 0) then
                      go to 9000
                  endif
  500         continue
              go to 9000
c
c.........Store cycle parameters
c
  600         rc(1) = rclw(2) / cnv
              if (ifed .lt. mxcl) then
                  if (ipsw(ifed+1) .ne. 0) go to 9000
                  rc(2) = rclw(ifed+1) / cnv
              endif
              if (ifed .ne. 3) then
                  rc(6) = rclw(ifed-1)
                  if (ic(4) .eq. 3) then
                      ic(4) = 2
                      if (rc(6) .ne. 0) rc(6) = 1. / rc(6)
                  endif
              endif
              if (ifed .gt. 4 .and. ic(2) .ge. 3 .and. ic(2) .le. 6)
     1                then
                  if (ifed .gt. 6) ifed = 6
                  do 700 i=3,ifed-2,1
                      rc(i) = rclw(i) / cnv
  700             continue
              else if (ic(2) .ge. 3) then
                  ic(2) = 1
              endif
c
c......CYCLE/mode,FEDTO,...
c
          else
              inc    = 2
c
c.........Break out next parameter list
c
 1000         ist1   = 0
              do 1100 i=inc,mxcl,1
                  if (i .ne. inc .and. ipsw(i) .ne. 0) go to 1200
                  ist1  = ist1  + 1
                  ip(ist1) = ipsw(i)
                  rp(ist1) = rclw(i)
 1100         continue
 1200         inc    = i
c
c.........FEDTO
c
              if (ip(1) .eq. 281) then
                  if (ist1 .lt. 2) go to 8000
                  rc(1) = rp(2) / cnv
c
c.........RAPTO
c
              else if (ip(1) .eq. 280) then
                  if (ist1 .lt. 2) go to 8000
                  rc(2) = rp(2) / cnv
c
c.........IPM
c
              else if (ip(1) .eq. 73 .or. ip(1) .eq. 315) then
                  if (ist1 .lt. 2) go to 8000
                  ic(4) = 1
                  rc(6) = rp(2)
c
c.........IPR
c
              else if (ip(1) .eq. 74 .or. ip(1) .eq. 316) then
                  if (ist1 .lt. 2) go to 8000
                  ic(4) = 2
                  rc(6) = rp(2)
c
c.........TPI
c
              else if (ip(1) .eq. 143) then
                  if (ist1 .lt. 2) go to 8000
                  ic(4) = 2
                  rc(6) = rp(2)
                  if (rc(6) .ne. 0) rc(6) = 1. / rc(6)
c
c.........STEP
c
              else if (ip(1) .eq. 92) then
                  if (ist1 .lt. 2) go to 8000
                  rc(3) = rp(2) / cnv
                  if (ist1 .ge. 3) rc(4) = rp(3) / cnv
c
c.........OFFSET
c
              else if (ip(1) .eq. 666) then
                  if (ist1 .lt. 2) go to 8000
                  rc(3) = rp(2) / cnv
                  if (ist1 .ge. 3) rc(4) = rp(3) / cnv
c
c.........RTRCTO
c
              else if (ip(1) .eq. 295) then
                  if (ist1 .lt. 2) go to 8000
                  ic(3) = 1
                  rc(5) = rp(2) / cnv
              endif
c
c.........Go get next parameter group
c
              if (inc .lt. mxcl) go to 1000
          endif
      endif
 
 1900 continue
c
c...Return cycle parameters
c
      do 2000 i=1,10,1
          icyc(i) = ic(i)
          rcyc(i) = rc(i)
 2000 continue
c
c...End of routine
c
 8000 return
c
c...Unknown CYCLE command
c
 9000 iflg   = 0
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clcyc1 (ipsw,rclw,mxcl,ic,rc,cnv,iflg)
c
c   FUNCTION:  This routine parses a CYCLE command from a clfile and
c              returns the programmed cycle parameters.
c
c   INPUT:  rclw    R*8  Dn  -  Post processor command parameters from
c                               clfile.
c           ipsw             -  vocabulary words in rclw
c           mxcl    I*4  D1  -  Number of parameters in rclw.
c
c   OUTPUT: iflg    I*2  D1  -  0 = This command was ignored.  1 = This
c                                   was a valid command.
c
c           ic     I*2  D10 -  Integer cycle parameters.
c
c           rc     R*8  D10 -  Real cycle parameters.
c
c***********************************************************************
c
c...IC  (1) = 1 = Cycle on.
c...     2  = Cycle mode:  8 = CIRCUL
c...     3  = iramps - number of rotations
c...     4  = Feed rate mode.  0 = None specified, 1 = IPM, 2 = IPR, 3 = TPI.
c...     5  = 1 = UP, 0 = DOWN.
c...     6  = 1 = CLW, 0 = CCLW.
c...     7  = 1 = on, 0 = in.
c...
c...RC  (1) = depth (total Z-increment).
c...     2  = radius.
c...     3  = depth per rotation.
c...     4  = tool diam.
c...     5  = rapto angle.
c...     6  = Feed rate.
c...     7  = Rapto distance.
c...     8  = Retract distance.
c
      subroutine clcyc1 (ipsw,rclw,mxcl,ic,rc,cnv,iflg)
 
      include 'com8a.com'
 
      integer*2 iflg,ic(10),ipsw(50)
      integer*4 mxcl
      real*8 rclw(10),rc(10),cnv
 
      integer*2 i,inc
 
 
      if (ipsw(2) .eq. 0) then
c
c..... "short" command
c
        rc(1) = rclw(2)/cnv
        rc(2) = rclw(3)/(2.*cnv)
        rc(3) = rclw(4)/cnv
        if (rclw(4) .eq. 0) then
          iflg = 0
        else
          ic(3) = rclw(2)/rclw(4) + 0.5
          if (ic(3) .le. 0) iflg = 0
        endif
        rc(4) = 0. ! tool diam
        rc(5) = 45. ! atangl parameter
c
c..... feedrate type
c
        if (ipsw(6) .eq. 73) then
          ic(4) = 1
        else if (ipsw(6) .eq. 74) then
          ic(4) = 2
        else if (ipsw(6) .eq. 143) then
          ic(4) = 3
        else
          iflg = 0
        endif
        rc(6) = rclw(5) / cnv    ! feedrate
        if (ic(4) .eq. 3) then
          ic(4) = 2
          if (rc(6) .ne. 0) rc(6) = 1. / rc(6)
        endif
c
c......Rapto, UP/DOWN, CCLW/CLW
c
        inc    = 7
        if (inc .le. MXCL .and. ipsw(inc) .eq. 0) then
            rc(7) = rclw(inc)/cnv
            inc    = inc    + 1
        else
            rc(7) = .1
        endif
c
c......UP/DOWN
c
        if (inc .le. MXCL .and. ipsw(inc) .eq. 112) then
            ic(5) = 1
            inc    = inc    + 1
        else
            ic(5) = 0
        endif
c
c......CLW/CCLW
c
        if (inc .le. MXCL .and. ipsw(inc) .eq. 60) then
            ic(6) = 1
            inc    = inc    + 1
        else
            ic(6) = 0
        endif
      else
c
c..... "long" NCL-PostWorks command
c
        do i = 2,mxcl
          if (ipsw(i) .eq. 510) then ! depth
            rc(1) = rclw(i+1)/cnv
          else if (ipsw(i) .eq. 205) then ! diameter
            rc(2) = rclw(i+1)/(2.*cnv)
          else if (ipsw(i) .eq. 23) then ! radius
            rc(2) = rclw(i+1)/cnv
          else if (ipsw(i) .eq. 92) then ! step
            rc(3) = rclw(i+1)/cnv
          else if (ipsw(i) .eq. 617) then ! TOOL
            rc(4) = rclw(i+1)
          else if (ipsw(i) .eq. 1) then ! ATANGL
            rc(5) = rclw(i+1)
          else if (ipsw(i).eq.73 .or. ipsw(i).eq.74 .or.
     x             ipsw(i).eq.143) then ! IPM / IPR
            if (ipsw(i) .eq. 73) then
              ic(4) = 1
            else if (ipsw(i) .eq. 74) then
              ic(4) = 2
            else if (ipsw(6) .eq. 143) then
              ic(4) = 3
            else
              iflg = 0
            endif
            rc(6) = rclw(i+1) ! feedrate
            if (ic(4) .eq. 3) then
              ic(4) = 2
              if (rc(6) .ne. 0) rc(6) = 1. / rc(6)
            endif
          else if (ipsw(i).eq.112 .or. ipsw(i).eq.113) then ! UP / DOWN
            if (ipsw(i).eq.112) ic(5) = 1
          else if (ipsw(i).eq.59 .or. ipsw(i).eq.60) then ! CCLW / CLW
            if (ipsw(i).eq.60) ic(6) = 1
          else if (ipsw(i).eq.71 .or. ipsw(i).eq.652) then ! IN / ON
            if (ipsw(i).eq.71) ic(7) = 1
          else if (ipsw(i) .eq. 280) then ! RAPTO
            rc(7) = rclw(i+1)/cnv
          else if (ipsw(i) .eq. 295) then ! RAPTO
            rc(8) = rclw(i+1)/cnv
          else if (ipsw(i) .ne. 0) then
            iflg = 0
          endif
        enddo
        if (rc(3) .eq. 0) then
          iflg = 0
        else
          ic(3) = rc(1)/rc(3) + 0.5
        endif
        if (ic(3) .le. 0) iflg = 0
      endif
 
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clcycl (kmaj,rclw,mxcl,cnv,iflg,icyc,rcyc)
c
c   FUNCTION:  This routine parses a lathe CYCLE command from a clfile and
c              returns the programmed cycle paramters.
c
c   INPUT:  kmaj    I*4  D1  -  Either 1054 (CYCLE) or 1036 (THREAD).
c
c           rclw    R*8  Dn  -  Post processor command parameters from
c                               clfile.
c
c           mxcl    I*4  D1  -  Number of parameters in 'rclw'.
c
c           cnv     R*8  D1  -  Metric conversion factor.
c
c   OUTPUT: iflg    I*2  D1  -  0 = This command was ignored.  1 = This
c                                   was a valid command.
c
c           icyc    I*2  D10 -  Integer cycle parameters.
c
c           rcyc    R*8  D10 -  Real cycle parameters.
c
c***********************************************************************
c
c...ICYC(1) = 0 = Cycle off. 1 = Cycle on.
c...     2  = Cycle mode.  1 = DRILL, 2 = DEEP, 3 = THRU, 4 = FACE,
c...          5 = TURN, 6 = CYCLE/THREAD, 7 = THREAD/lead
c...     3  = Retract specified.
c...     4  = Feed rate mode.  0 = None specified, 1 = IPM, 2 = IPR,
c...          3 = TPI.
c...     5  = Number of repetitions
c...     6  = 1 = Chamfering is enabled.
c...     7  = 1 = Z-axis position is furnished, 2 = Z-axis delta.
c...     8  = 1 = X-axis position is furnished, 2 = X-axis delta.
c...
c...RCYC(1) = Fedto parameter.
c...     2  = Rapto parameter or thread height for threading.
c...     3  = Step 1 parameter.
c...     4  = Step 2 parameter.
c...     5  = Retract parameter or tool angle for threading.
c...     6  = Feed rate or Z-lead for threading.
c...     7  = Rapid rate or X-lead for threading.
c...     8  = Z-axis value.
c...     9  = X-axis value.
c
      subroutine clcycl (kmaj,rclw,kmxcl,cnv,iflg,icyc,rcyc)
c
      include 'com8a.com'
c
      integer*2 icyc(10),iflg
      integer*4 kmaj,kmxcl
c
      real*8 rclw(10),rcyc(10),cnv
c
      integer*2 ic(10),ipsw(50),inum(4),ip(10),inc,ist1,is1,is4,
     1          mxcl
c
      real*8 rnum,rc(10),rp(10)
c
      equivalence (rnum,inum)
      data is1 /4/, is4 /1/
c
c...Initialize routine
c
cc      cnv    = 1.
cc      if (ifl(264) .ne. 0) cnv = 25.4
      mxcl   = kmxcl
      iflg   = 1
      do 100 i=1,10,1
          ic(i) = 0
          rc(i) = 0.
  100 continue
      if (kmaj .eq. 1036) then
          do 150 i=mxcl,1,-1
              rclw(i+2) = rclw(i)
  150     continue
          inum(is1) = 0
          inum(is4) = 1036
          rclw(1) = rnum
          inum(is1) = 0
          inum(is4) = 143
          rclw(2) = rnum
          mxcl   = kmxcl  + 2
      endif
      do 200 i=1,mxcl,1
          rnum   = rclw(i)
          if (inum(is1) .eq. 0 .and. inum(is4) .ne. 0) then
              ipsw(i) = inum(is4)
          else
              ipsw(i) = 0
          endif
  200 continue
c
c...Ignored CYCLE commands
c...
c......CYCLE/OPTION
c......      AUTO
c......      AVOID (for now)
c
      if (ipsw(1) .eq. 144 .or. ipsw(1) .eq. 88 .or.
     1    ipsw(1) .eq. 327 .or. ipsw(1) .eq. 0) go to 9000
c
c...CYCLE/ON
c
      if (ipsw(1) .eq. 71) then
          ic(1) = 1
c
c...CYCLE/OFF
c
      else if (ipsw(1) .eq. 72) then
          ic(1) = 0
c
c...CYCLE/mode
c
      else
          ic(1) = 2
          ic(2) = 1
          if (ipsw(1) .eq. 53) then
              ic(2) = 2
          else if (ipsw(1) .eq. 152) then
              ic(2) = 3
          else if (ipsw(1) .eq. 81) then
              ic(2) = 4
          else if (ipsw(1) .eq. 80 .or. ipsw(1) .eq. 320) then
              ic(2) = 5
          else if (ipsw(1) .eq. 1036) then
              ic(2) = 6
              if (kmaj .eq. 1036) ic(2) = 7
          endif
c
c......CYCLE/mode,FEDTO,...
c
          inc    = 2
c
c.........Break out next parameter list
c
 1000     ist1   = 0
          do 1100 i=inc,mxcl,1
              if (i .ne. inc .and. ipsw(i) .ne. 0) go to 1200
              ist1  = ist1  + 1
              ip(ist1) = ipsw(i)
              rp(ist1) = rclw(i)
 1100     continue
 1200     inc    = i
c
c.........FEDTO
c
          if (ip(1) .eq. 281) then
              if (ist1 .lt. 2) go to 8000
              rc(1) = rp(2) / cnv
c
c.........RAPTO
c
          else if (ip(1) .eq. 280) then
              if (ist1 .lt. 2) go to 8000
              rc(2) = rp(2) / cnv
c
c.........IPM
c
          else if (ip(1) .eq. 73 .or. ip(1) .eq. 315) then
              if (ist1 .lt. 2) go to 8000
              ic(4) = 1
              rc(6) = rp(2) / cnv
              if (ist1 .eq. 3) rc(7) = rp(3) / cnv
c
c.........IPR
c
          else if (ip(1) .eq. 74 .or. ip(1) .eq. 316) then
              if (ist1 .lt. 2) go to 8000
              ic(4) = 2
              rc(6) = rp(2) / cnv
              if (ist1 .eq. 3) rc(7) = rp(3) / cnv
c
c.........TPI
c
          else if (ip(1) .eq. 143) then
              if (ist1 .lt. 2) go to 8000
              ic(4) = 2
              rc(6) = rp(2)
              if (rc(6) .ne. 0) then
                  if (rc(6) .gt. 1.) then
                      rc(6) = 1. / rc(6) / cnv
                  else
                      rc(6) = rc(6) / cnv
                  endif
              endif
              if (ist1 .eq. 3) then
                  rc(7) = rp(3)
                  if (rc(7) .gt. 1.) then
                      rc(7) = 1. / rc(7) / cnv
                  else
                      rc(7) = rc(7) / cnv
                  endif
              endif
c
c.........STEP
c
          else if (ip(1) .eq. 92) then
              if (ist1 .lt. 2) go to 8000
              rc(3) = rp(2) / cnv
              if (ist1 .ge. 3) rc(4) = rp(3) / cnv
c
c.........OFFSET
c
          else if (ip(1) .eq. 666) then
              if (ist1 .lt. 2) go to 8000
              rc(2) = rp(2) / cnv
c
c.........RTRCTO
c
          else if (ip(1) .eq. 295) then
              if (ist1 .lt. 2) go to 8000
              ic(3) = 1
              rc(5) = rp(2) / cnv
c
c.........ZAXIS
c
          else if (ip(1) .eq. 86) then
              if (ist1 .lt. 2) go to 8000
              ic(7) = 2
              rc(8) = rp(2)
c
c.........ZCOORD
c
          else if (ip(1) .eq. 118) then
              if (ist1 .lt. 2) go to 8000
              ic(7) = 1
              rc(8) = rp(2)
c
c.........XAXIS
c
          else if (ip(1) .eq. 84) then
              if (ist1 .lt. 2) go to 8000
              ic(8) = 2
              rc(9) = rp(2)
c
c.........XCOORD
c
          else if (ip(1) .eq. 116) then
              if (ist1 .lt. 2) go to 8000
              ic(8) = 1
              rc(9) = rp(2)
c
c.........REPEAT
c
          else if (ip(1) .eq. 1083) then
              if (ist1 .lt. 2) go to 8000
              ic(5) = rp(2)
c
c.........TOOL
c
          else if (ip(1) .eq. 617) then
              if (ist1 .lt. 2) go to 8000
              rc(5) = rp(2)
c
c.........ON
c
          else if (ip(1) .eq. 71) then
              ic(6) = 1
c
c.........OFF
c
          else if (ip(1) .eq. 72) then
              ic(6) = 0
          endif
c
c.........Go get next parameter group
c
          if (inc .le. mxcl) go to 1000
      endif
c
c...Return cycle parameters
c
      do 2000 i=1,10,1
          icyc(i) = ic(i)
          rcyc(i) = rc(i)
 2000 continue
c
c...End of routine
c
 8000 return
c
c...Unknown CYCLE command
c
 9000 iflg   = 0
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clfed (rclw,mxcl,ftyp,fed)
c
c   FUNCTION:  This routine parses a feed rate command from a clfile and
c              returns the programmed feed rate.
c
c   INPUT:  rclw    R*8  Dn  -  Post processor command parameters from
c                               clfile.
c
c           mxcl    I*4  D1  -  Number of parameters in 'rclw'.
c
c   OUTPUT: ftyp    I*2  D1  -  1 = IPM feed rate.  2 = IPR feed rate.
c
c           fed     R*8  D1  -  Programmed feed rate.
c
c***********************************************************************
c
      subroutine clfed (rclw,mxcl,ftyp,fed)
c
      integer*2 ftyp
      integer*4 mxcl
c
      real*8 rclw(10),fed
c
      integer*2 inum(4),is1,is4
      real*8 rnum
      data is1 /4/, is4 /1/
c
      equivalence (rnum,inum)
c
c...FEDRAT/feed
c
      rnum   = rclw(1)
      if (inum(is1) .ne. 0 .or. inum(is4) .eq. 0) then
          fed   = rclw(1)
          if (mxcl .ge. 2 .and. inum(is1) .eq. 0) then
              if (inum(is4) .eq. 73 .or. inum(is4) .eq. 315) ftyp = 1
              if (inum(is4) .eq. 74 .or. inum(is4) .eq. 316) ftyp = 2
          endif
c
c...FEDRAT/IPM,feed
c
      else if (inum(is4) .eq. 73 .or. inum(is4) .eq. 315 .and.
     1         mxcl .ge. 2) then
          rnum   = rclw(2)
          if (inum(is1) .ne. 0 .or. inum(is4) .eq. 0) then
              ftyp   = 1
              fed = rclw(2)
          endif
c
c...FEDRAT/IPR,feed
c
      else if (inum(is4) .eq. 74 .or. inum(is4) .eq. 316 .and.
     1         mxcl .ge. 2) then
          rnum   = rclw(2)
          if (inum(is1) .ne. 0 .or. inum(is4) .eq. 0) then
              ftyp   = 2
              fed = rclw(2)
          endif
c
c....Fedrat/SCALE,value
c
      else if (inum(is4) .eq. 25 .and. mxcl .ge. 2) then
          rnum   = rclw(2)
          chc = 1
          call nclf_set_ctr_hgt(chc, rnum)
      else if (inum(is4) .eq. 9 .and. mxcl .ge. 2) then
          rnum   = rclw(2)
           chc = 0
          call nclf_set_ctr_hgt(chc, rnum)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clret (rclw,mxcl,kret)
c
c   FUNCTION:  This routine parses a RETRCT command from a clfile and
c              returns the cycle retract condition (ON,OFF).
c
c   INPUT:  rclw    R*8  Dn  -  Post processor command parameters from
c                               clfile.
c
c           mxcl    I*4  D1  -  Number of parameters in 'rclw'.
c
c   OUTPUT: kret    I*2  D1  -  1 = RETRCT/OFF.  2 = RETRCT/ON.
c
c***********************************************************************
c
      subroutine clret (rclw,mxcl,kret)
c
      integer*2 kret
      integer*4 mxcl
c
      real*8 rclw(10)
c
      integer*2 inum(4),is1,is4
      real*8 rnum
      data is1 /4/, is4 /1/
c
      equivalence (rnum,inum)
c
c...Initialize routine
c
      if (mxcl .lt. 1) go to 8000
      rnum   = rclw(1)
c
c...RETRCT/ON
c
      if (inum(is1) .eq. 0) then
          if (inum(is4) .eq. 71) then
              kret   = 2
c
c...RETRCT/OFF
c
          else if (inum(is4) .eq. 72) then
              kret   = 1
          endif
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clspn (rclw,mxcl,grpm,kdir)
c
c   FUNCTION:  This routine parses a spindle command from a clfile and
c              returns the programmed spindle speed.
c
c   INPUT:  rclw    R*8  Dn  -  Post processor command parameters from
c                               clfile.
c
c           mxcl    I*4  D1  -  Number of parameters in 'rclw'.
c
c   OUTPUT: grpm    R*8  D1  -  Programmed spindle speed.
c
c           kdir    I*4  D1  -  0 = CLW, 1 = CCLW.
c
c***********************************************************************
c
      subroutine clspn (rclw,mxcl,grpm,kdir)
c
      integer*4 mxcl
      integer*4 kdir
c
      real*8 rclw(10),grpm
c
      integer*2 inum(4),is1,is4,ist
      real*8 rnum
      data is1 /4/, is4 /1/
c
      equivalence (rnum,inum)
c
c...SPINDL/OFF
c
      rnum   = rclw(1)
      if (inum(is4) .eq. 72 .or. inum(is4) .eq. 246) then
          grpm = 0.
          ist = mxcl + 1
c
c...SPINDL/speed
c
      else if (inum(is1) .ne. 0 .or. inum(is4) .eq. 0) then
          grpm  = rclw(1)
          ist    = 2
c
c...SPINDL/RPM,speed
c
      else if (inum(is4) .eq. 78 .or. inum(is4) .eq. 115 .and.
     1         mxcl .ge. 2) then
          rnum   = rclw(2)
          if (inum(is1) .ne. 0 .or. inum(is4) .eq. 0) grpm = rclw(2)
          ist    = 3
      endif
c
c...SPINDL/...,dir
c
      if (ist .le. mxcl) then
          rnum   = rclw(ist)
          if (inum(is4) .eq. 60) kdir = 0
          if (inum(is4) .eq. 59) kdir = 1
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clcool (rclw,mxcl,kcol)
c
c   FUNCTION:  This routine parses a coolant command from a clfile and
c              returns the programmed coolant flag.
c
c   INPUT:  rclw    R*8  Dn  -  Post processor command parameters from
c                               clfile.
c
c           mxcl    I*4  D1  -  Number of parameters in 'rclw'.
c
c   OUTPUT: kcol    I*4  D1  -  Coolant mode.
c
c***********************************************************************
c
      subroutine clcool (rclw,mxcl,kcol)
c
      integer*4 kcol
      integer*4 mxcl
c
      real*8 rclw(10)
c
      integer*2 inum(4),is1,is4
      real*8 rnum
      data is1 /4/, is4 /1/
c
      equivalence (rnum,inum)
c
c...COOLNT/mode
c
      rnum   = rclw(1)
      if (inum(is4) .eq. 72) then
          kcol = 0
      else if (inum(is4) .eq. 71 .or. inum(is4) .eq. 89) then
          kcol = 1
      else if (inum(is4) .eq. 90) then
          kcol = 2
      else if (inum(is4) .eq. 1011) then
          kcol = 3
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clcutc (rclw,mxcl,ktyp,kmod,kdir)
c
c   FUNCTION:  This routine parses a cutcom command from a clfile and
c              returns the programmed cutcom direction and plane.
c
c   INPUT:  rclw    R*8  Dn  -  Post processor command parameters from
c                               clfile.
c
c           mxcl    I*4  D1  -  Number of parameters in 'rclw'.
c
c   OUTPUT: ktyp    I*4  D1  -  1 = Standard CUTCOM command.
c                               2 = CUTCOM/ADJUST command.
c
c   OUTPUT: kmod    I*4  D1  -  (1) Programmed cutcom plane.
c                               (2) Fixture offset mode.
c
c           kdir    I*4  D1  -  (1) Programmed cutcom direction.
c                               (2) Offset register.
c
c***********************************************************************
c
      subroutine clcutc (rclw,mxcl,ktyp,kmod,kdir)
c
      integer*4 kmod,kdir,ktyp
      integer*4 mxcl
c
      real*8 rclw(10)
c
      integer*2 inum(4),is1,is4,idid
      real*8 rnum
      data is1 /4/, is4 /1/
c
      equivalence (rnum,inum)
c
c...CUTCOM/dir
c
      rnum   = rclw(1)
      idid   = 0
      ktyp   = 1
      if (inum(is4) .eq. 72) then
         kdir = 0
         idid = 1
      else if (inum(is4) .eq. 8) then
         kdir = 1
         idid = 1
      else if (inum(is4) .eq. 24) then
         kdir = 2
         idid = 1
      endif
c
c...CUTCOM/,plane
c
      if (idid .eq. 1 .and. kdir .ne. 0 .and. mxcl .ge. 2) then
          rnum   = rclw(2)
          if (inum(is4) .eq. 33) then
              kmod = 0
          else if (inum(is4) .eq. 37) then
              kmod = 1
          else if (inum(is4) .eq. 41) then
              kmod = 2
          endif
      endif
c
c...CUTCOM/ADJUST
c
      if (idid .eq. 0 .and. inum(is4) .eq. 159) then
          ktyp   = 2
          rnum   = rclw(2)
c
c......CUTCOM/ADJUST,ON
c
          if (inum(is4) .eq. 71) then
              kmod   = 1
              kdir   = 0
c
c......CUTCOM/ADJUST,ON
c
          else if (inum(is4) .eq. 72) then
              kmod   = 0
              kdir   = 0
c
c......CUTCOM/ADJUST,n
c
          else if (inum(is4) .eq. 0) then
              kmod    = 1
              kdir    = rnum
c
c.........CUTCOM/ADJUST,n,MINUS
c
              if (mxcl .gt. 2) then
                  rnum   = rclw(3)
                  if (inum(is4) .eq. 10) kmod = 2
              endif
          endif
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clldtl (rclw,mxcl,ktl,gtl)
c
c   FUNCTION:  This routine parses a loadtl command from a clfile and
c              returns the programmed tool number and length.
c
c   INPUT:  rclw    R*8  Dn  -  Post processor command parameters from
c                               clfile.
c
c           mxcl    I*4  D1  -  Number of parameters in 'rclw'.
c
c   OUTPUT: ktl     I*4  D1  -  Programmed tool number.
c
c           gtl     R*8  D1  -  Programmed tool length.
c
c***********************************************************************
c
      subroutine clldtl (rclw,mxcl,ktl,gtl)
c
      integer*4 ktl
      integer*4 mxcl
c
      real*8 rclw(10),gtl
c
      integer*2 inum(4),is1,is4
      real*8 rnum
      data is1 /4/, is4 /1/
c
      equivalence (rnum,inum)
c
c...LOADTL/tlno
c
      rnum   = rclw(1)
      if (inum(is1) .ne. 0 .or. inum(is4) .eq. 0) then
          ktl   = rclw(1)
          gtl   = 0.
c
c...LOADTL/tlno,len
c
          rnum   = rclw(2)
          if (mxcl .ge. 2 .and.
     1        (inum(is1) .ne. 0 .or. inum(is4) .eq. 0)) then
              gtl    = rclw(2)
c
c...LOADTL/tlno,LENGTH,len
c
          else if (mxcl .ge. 3 .and. inum(is4) .eq. 9) then
              rnum   = rclw(3)
              if (inum(is1) .ne. 0 .or. inum(is4) .eq. 0) gtl = rclw(3)
          endif
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clmach (rclw,mxcl,pstnam,knc1,mdfnam,knc2,kmachs,knmach,kfl)
c
c   FUNCTION:  This routine parses a machin command from a clfile and
c              returns the post-processor name.
c
c   INPUT:  rclw    R*8  Dn  -  Post processor command parameters from
c                               clfile.
c
c           mxcl    I*4  D1  -  Number of parameters in 'rclw'.
c
c           kfl     I*4  D1  -  1 = Store array of machine numbers in 'kmachs'.
c
c   OUTPUT: pstnam  C*40 D1  -  Name of post-processor.
c
c           knc1    I*2  D1  -  Number of characters in 'pstnam'.
c
c           mdfnam  C*40 D1  -  MDF file name.
c
c           knc2    I*2  D1  -  Number of characters in 'mdfnam'.
c
c           kmachs  I*4  D1  -  Array of machine numbers in MACHIN/PWORKS
c                               command.
c
c           knmach  I*4  D1  -  Number of characters in 'mdfnam'.
c
c***********************************************************************
c
      subroutine clmach (rclw,mxcl,pstnam,knc1,mdfnam,knc2,kmachs,
     1                   knmach,kfl)
c
      integer*4 mxcl,kmachs(10),knmach,kfl
c
      real*8 rclw(10)
c
      character*40 pstnam,mdfnam
c
      integer*4 jnum(12),nc,strlen1
c
      real*8 rnum(6)
c
      integer*2 inum(4),is1,is4
c
      data is1 /4/, is4 /1/
c
      character*48 cnam
c
      equivalence (rnum,cnam,jnum,inum)
c
c...MACHIN/pstnam
c
      if (kfl .eq. 1) knmach = 0
      rnum(1) = rclw(1)
      if (jnum(1) .eq. -1 .and. jnum(2) .gt. 8 .and. jnum(2) .le. 40)
     1        then
          nc     = (jnum(2)+7) / 8
          do 100 i=1,nc,1
              rnum(i) = rclw(i+1)
  100     continue
          pstnam = cnam(1:jnum(2))
      else
          pstnam = cnam(1:8)
      endif
      nc     = strlen1(pstnam)
      knc1   = nc
c
c...MACHIN/PWORKS,n
c
      if (pstnam(1:nc) .eq. "PWORKS" .or. pstnam(1:nc) .eq. "PostWork")
     1        then
          jnum(1) = rclw(2)
          call itoc (jnum(1),mdfnam,nc)
          if (kfl .eq. 1) then
              do 200 i=2,mxcl,1
                  rnum(1) = rclw(i)
                  if (inum(is4) .ne. 0) go to 210
                  knmach = knmach + 1
                  kmachs(knmach) = rclw(i)
                  if (knmach .eq. 10) go to 210
  200         continue
  210         continue
          endif
      else
          mdfnam = pstnam
      endif
      knc2    = nc
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clmode (rclw,mxcl,kmach)
c
c   FUNCTION:  This routine parses a mode command from a clfile and
c              returns whether MILL or LATHE were specified.
c
c   INPUT:  rclw    R*8  Dn  -  Post processor command parameters from
c                               clfile.
c
c           mxcl    I*4  D1  -  Number of parameters in 'rclw'.
c
c   OUTPUT: kmach   I*4  D1  -  -1 = Not a valid command, 0 = Mill,
c                               1 = Lathe.
c
c***********************************************************************
c
      subroutine clmode (rclw,mxcl,kmach)
c
      integer*4 kmach
      integer*4 mxcl
c
      real*8 rclw(10),gtl
c
      integer*2 inum(4),is1,is4
      real*8 rnum
      data is1 /4/, is4 /1/
c
      equivalence (rnum,inum)
c
c...MODE/MILL
c
      rnum   = rclw(1)
      if (inum(is4) .eq. 151) then
          kmach = 0
c
c...MODE/LATHE
c
      else if (inum(is4) .eq. 700) then
          kmach = 1
c
c...Unrecognized command
c
      else
          kmach = -1
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clturr (rclw,mxcl,ktl,gtl)
c
c   FUNCTION:  This routine parses a turret command from a clfile and
c              returns the programmed tool number and length.
c
c   INPUT:  rclw    R*8  Dn  -  Post processor command parameters from
c                               clfile.
c
c           mxcl    I*4  D1  -  Number of parameters in 'rclw'.
c
c   OUTPUT: ktl     I*4  D1  -  Programmed tool number.
c
c           gtl     R*8  D1  -  Programmed tool length.
c
c***********************************************************************
c
      subroutine clturr (rclw,mxcl,ktl,gtl)
c
      integer*4 ktl
      integer*4 mxcl
c
      real*8 rclw(10),gtl
c
      integer*2 inum(4),is1,is4
      real*8 rnum
      data is1 /4/, is4 /1/
c
      equivalence (rnum,inum)
c
c...TURRET/tlno
c
      rnum   = rclw(1)
      if (inum(is1) .ne. 0 .or. inum(is4) .eq. 0) then
          ktl   = rclw(1)
          gtl   = 0.
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  claptc (rclw,mxcl,kerr)
c
c   FUNCTION:  This routine converts a 7-parameter APT Cutter to a
c              standard NCL cutter.
c
c   INPUT:  rclw    R*8  D1  -  APT Cutter parameters.
c
c           mxcl    I*4  D1  -  Number of parameters in 'rclw'.
c
c   OUTPUT: rclw    R*8  D1  -  NCL Cutter parameters.
c
c           mxcl    I*4  D1  -  Number of parameters in 'rclw'.
c
c           kerr    I*4  D1  -  Returns 1 if an error occurred during
c                               parsing.
c
c***********************************************************************
c
      subroutine claptc (rclw,mxcl,kerr)
c
      integer*4 mxcl,kerr
c
      real*8 rclw(7)
c
c...Convert 7-parameter cutter
c...To NCL cutter
c
      kerr = 0
      if (mxcl .ne. 7) go to 9000
cc      if (rclw(5) .ne. 0 .or. (rclw(4) .ne. 0 .and.
cc     1    rclw(4) .ne. rclw(2))) go to 9000
      if (rclw(6) .gt. 0. .and. rclw(6) .lt. 90.) then
          rclw(1) = rclw(1) + (2.*rclw(2)) - (2.*(rclw(2)*
     1              tan(.01745329252d0*((90.-rclw(6))/2.))))
      else if (rclw(6) .lt .0 .and. rclw(6) .gt. -90) then
          rclw(1) = rclw(1) - 2.* (rclw(2)*
     1              (tan(.01745329252d0*(-rclw(6)/2.))+1.)/
     2              (tan(.01745329252d0*(90.+rclw(6)))))
      endif
      rclw(3) = rclw(7)
      rclw(4) = rclw(6)
      mxcl = 4
c
c...End of routine
c
 8000 return
c
c...Error converting cutter
c
 9000 kerr   = 1
      go to 8000
      end
