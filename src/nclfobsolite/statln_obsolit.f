C*********************************************************************
C*    NAME         :  statln.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        statln.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:09:35
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine statln
C*     This routine displays the status of several CAM modal conditions.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine statln
 
      include 'com8a.com'
      include 'status.com'
      include 'wrksys.com'
 
      character*1 csline(100), frask, tolcm, thpscm, thdscm, thcscm
      character*3 rsstat, tcstat, frstat, thstat
      character*6 frnum, tolnum, thds, thcs
      character*64 macnam, mcname, doend, dolabl, rsmx, tcmx, tlstat
      character*9 srcmod
      character*15 pgstat
      character*100 sline, savlin
      character*80 zline
      character*17 thps
 
      equivalence (sline, csline)
      equivalence (csline(1), srcmod)
      equivalence (csline(12), pgstat)
      equivalence (csline(27), doend)
      equivalence (csline(24), macnam)
      equivalence (csline(35), rsstat)
      equivalence (csline(38), rsmx)
      equivalence (csline(46), tcstat)
      equivalence (csline(49), tcmx)
      equivalence (csline(57), frstat)
      equivalence (csline(60), frnum)
      equivalence (csline(65), frask)
      equivalence (csline(67), tlstat)
      equivalence (csline(73), tolnum)
      equivalence (csline(78), tolcm)
      equivalence (csline(80), thstat)
      equivalence (csline(83), thps)
      equivalence (csline(88), thpscm)
      equivalence (csline(89), thds)
      equivalence (csline(94), thdscm)
      equivalence (csline(95), thcs)
      equivalence (csline(100), thcscm)
 
      equivalence (sc155, dolabl)
      equivalence (sc165, mcname)
 
      savlin = sline
 
c         Set savlin to blanks to force a redisplay of the status line
c         if it had been erased even though the status line contents
c         may have not changed.  (ifl(301) = 0 means status line was
c         erased.)
      if (ifl(301) .eq. 0) savlin = ' '
      sline = ' '
 
c         -------------------------------
c         Set Source Creation Mode field.
c         -------------------------------
      if (ifl(37) .eq. 2) then
          srcmod = 'Insert'
      else
          srcmod = 'Overwrite'
      endif
c         ------------------------
c         Set Program State field.
c         ------------------------
      if (pgmode(pgindx) .eq. 1) then
          pgstat = 'Macro def:'
          macnam = mcname
      else if (pgmode(pgindx) .eq. 2) then
          pgstat = 'Macro call:'
          macnam = mcname
      else if (pgmode(pgindx) .eq. 3) then
          if (ifl(286) .eq. 0) then
              pgstat = 'Loop definition'
          else
              pgstat = 'DO define to:'
              doend = dolabl
          endif
      else if (pgmode(pgindx) .eq. 4) then
          if (ifl(286) .eq. 0) then
              pgstat = 'Loop execution'
          else
              pgstat = 'DO execute to:'
              doend = dolabl
          endif
      else if (pgmode(pgindx) .eq. 5) then
          pgstat = 'Comment Section'
      else
          pgstat = 'Sequential'
      endif
 
c         -----------------------------
c         Set REFSYS Matrix name field.
c         -----------------------------
      rsstat = 'RS/'
      if (ifl(72) .eq. 0) then
          rsmx = 'NOMORE'
      else
          rsmx = rsname
      endif
 
c         -----------------------------
c         Set TRACUT Matrix name field.
c         -----------------------------
      tcstat = 'TC/'
      if (ifl(73) .eq. 0) then
          tcmx = 'NOMORE'
      else
          tcmx = tcname
      endif
 
c         -----------------------------
c         Set FEDRAT field.
c         -----------------------------
      frstat = 'FR/'
      call fmtnum (sc(123), frnum)
 
c         Put an asterisk after the feedrate if a secondary feedrate
c         is in effect.
      if (ifl(213) .ne. 0) frask = '*'
 
c         -----------------------------
c         Set TOLER field.
c         -----------------------------
      tlstat = 'TOLER/'
      call fmtnum (sc(27), tolnum)
 
c         -----------------------------
c         Set THICK field.
c         -----------------------------
      thstat = 'TH/'
      call fmtnum (sc(23), thps)
      if (sc(23) .eq. sc(24) .and.
     x    sc(23) .eq. sc(25)) goto 888
      thpscm = ','
      call fmtnum (sc(24), thds)
      if (sc(24) .eq. sc(25)) goto 888
      thdscm = ','
      call fmtnum (sc(25), thcs)
888   continue
 
 
c         --------------------------------------
c         Write to error message line on screen.
c         --------------------------------------
 
c            IFL(303) = 0  Means status line is being displayed when it
c                          changes.
c                     = 1  Means status line is not being displayed even
c                          if it changes.
c                     = 2  Current status line contents is to be displayed
c                          one time.  This is set in USHOW.F.
      if ((savlin .ne. sline .and. ifl(303) .eq. 0) .or.
     x     ifl(303) .eq. 2) then
          ifl(302) = 1
c
c...Added check for NCL-VT mode and for NCL501+ mode
c...Paul  -  10/22/91                   02/19/92
c...Old version was:
c   call puterm(sline)
c
                if (ifl(35) .eq. 2 .and. ifl(350) .le. 1 .and.
     1              ifl(322) .ne. 1) then
                   do 56 i=1,80
56                 zline(i:i) = ' '
 
                   zline(01:03) = '** '
                   zline(04:06) = rsstat
                   zline(07:13) = rsmx
                   zline(16:18) = tcstat
                   zline(19:25) = tcmx
                   zline(28:30) = frstat
                   zline(31:37) = frnum
                   zline(41:46) = tlstat
                   zline(47:53) = tolnum
                   zline(57:59) = thstat
                   zline(60:77) = thps
                   zline(14:14) = '*'
                   zline(26:26) = '*'
                   zline(39:39) = '*'
                   zline(55:55) = '*'
                   zline(78:80) = ' **'
                   call putmsg(zline(1:80),80,14,1)
                endif
c
c...Changed for NCL501+ mode
c...Paul - 03/17/92
c
!=VMS,UNX
!                if (ifl(35) .eq. 0 .and. ifl(350) .eq. 0) then
!                   call puterm(sline)
!                endif
!=ALL
          ifl(302) = 0
          ifl(301) = 1
      endif
      call fdisp_stat
      return
      end
c*******************************************************************
      subroutine fmtnum (num,a)
      real*8 num
      character*(*) a
 
      character*20 b
      integer*2 i
 
      if (num.lt.99999.5.and.num.gt.-9999.5) goto 10
      a='?????'
      goto 999
 
10    if (num.ne.0.d0) goto 15
      a='0.0'
      goto 999
 
15    write (b,1010)num
1010  format (f20.8)
 
      do 20 i=1,20
20    if (b(i:i).ne.' ' .and. b(i:i).ne.'0') goto 30
 
30    continue
      if (b(i:i).ne.'-' .or. b(i+1:i+1).ne.'0') goto 40
      i=i+1
      b(i:i)='-'
 
40    a=b(i:i+4)
      a(6:6)=' '
 
999   return
      end
 
 
 
      subroutine fdisp_stat
 
      include 'com8a.com'
      include 'status.com'
      include 'wrksys.com'
 
      character*1 thpscm, thdscm
      character*6 frnum, tolnum1, tolnum2, thds, thcs, thcs2,
     -              thcs3,thcs4, thcs5
      character*64 macnam, mcname, doend, dolabl, rsmx, tcmx
      character*6 ptx, pty, ptz
      character*9 srcmod
      character*15 pgstat
      character*17 thps
      character*80 smsg
      integer*4 j, nc1, nc2, strlen1
 
      equivalence (sc155, dolabl)
      equivalence (sc165, mcname)
c         -------------------------------
c         Set Source Creation Mode field.
c         -------------------------------
      if (ifl(37) .eq. 2) then
          srcmod = 'Insert'
      else
          srcmod = 'Overwrite'
      endif
      nc1 = 10
      nc2 = strlen1 (srcmod)
      call wrtstat("INPUT_MODE", nc1, srcmod, nc2)
c         ------------------------
c         Set Program State field.
c         ------------------------
      if (pgmode(pgindx) .eq. 1) then
          pgstat = 'Macro def:'
          macnam = mcname
          smsg (1:) = 'Macro def:' // mcname
          nc1 = 12
          nc2 = strlen1 (smsg)
          call wrtstat("PROCESS_MODE", nc1, smsg, nc2)
      else if (pgmode(pgindx) .eq. 2) then
          pgstat = 'Macro call:'
          macnam = mcname
          smsg (1:) = 'Macro call:' // mcname
          nc1 = 12
          nc2 = strlen1 (smsg)
          call wrtstat("PROCESS_MODE", nc1, smsg, nc2)
      else if (pgmode(pgindx) .eq. 3) then
          if (ifl(286) .eq. 0) then
              pgstat = 'Loop definition'
              nc1 = 12
              nc2 = strlen1 (pgstat)
              call wrtstat("PROCESS_MODE", nc1, pgstat, nc2)
          else
              pgstat = 'DO define to:'
              doend = dolabl
              smsg (1:) = 'DO define to:' // dolabl
              nc1 = 12
              nc2 = strlen1 (smsg)
              call wrtstat("PROCESS_MODE", nc1, smsg, nc2)
          endif
      else if (pgmode(pgindx) .eq. 4) then
          if (ifl(286) .eq. 0) then
              pgstat = 'Loop execution'
              nc1 = 12
              nc2 = strlen1 (pgstat)
              call wrtstat("PROCESS_MODE", nc1, pgstat, nc2)
          else
              pgstat = 'DO execute to:'
              doend = dolabl
              smsg (1:) = 'DO execute to:' // dolabl
              nc1 = 12
              nc2 = strlen1 (smsg)
              call wrtstat("PROCESS_MODE", nc1, smsg, nc2)
          endif
      else if (pgmode(pgindx) .eq. 5) then
          pgstat = 'Comment Section'
          nc1 = 12
          nc2 = strlen1 (pgstat)
          call wrtstat("PROCESS_MODE", nc1, pgstat, nc2)
      else
          pgstat = 'Sequential'
          nc1 = 12
          nc2 = strlen1 (pgstat)
          call wrtstat("PROCESS_MODE", nc1, pgstat, nc2)
      endif
 
c         -----------------------------
c         Set REFSYS Matrix name field.
c         -----------------------------
      if (ifl(72) .eq. 0) then
          rsmx = 'NOMORE'
      else
          rsmx = rsname
      endif
      nc1 = 6
      nc2 = strlen1 (rsmx)
      call wrtstat("REFSYS", nc1, rsmx, nc2) 
c         -----------------------------
c         Set MODSYS Matrix name field.
c         -----------------------------
      if (.not.lwrk) then
          smsg(1:) = 'OFF'
      else
          smsg(1:) = modlab
      endif
      nc1 = 6
      nc2 = strlen1 (smsg)
      call wrtstat("MODSYS", nc1, smsg, nc2) 
 
c         -----------------------------
c         Set TRACUT Matrix name field.
c         -----------------------------
      if (ifl(73) .eq. 0) then
          tcmx = 'NOMORE'
      else
          tcmx = tcname
      endif
      nc1 = 6
      nc2 = strlen1 (tcmx)
      call wrtstat("TRACUT", nc1, tcmx, nc2) 
 
c         -----------------------------
c         Set FEDRAT field.
c         -----------------------------
      call fmtnum (sc(123), frnum)
      nc1 = 6
      nc2 = strlen1 (frnum)
      call wrtstat("FEDRAT", nc1, frnum, nc2)  
c         -----------------------------
c         Set TOLER field.
c         -----------------------------
      call fmtnum (sc(27), tolnum1)
      call fmtnum (sc(168), tolnum2)
      smsg (1:) = tolnum1(1:6) // ',' //tolnum2(1:6)
      nc1 = 5
      nc2 = strlen1 (smsg)
      call wrtstat("TOLER", nc1, smsg, nc2)   
c         -----------------------------
c         Set THICK field.
c         -----------------------------
      checks = 5
      if (sc(180).eq.0) then
          checks = checks - 1
          if (sc(179).eq.0) then
              checks = checks - 1
              if (sc(178).eq.0) then
                  checks = checks - 1
                  if (sc(177).eq.0) then
                      checks = checks - 1
                  endif
              endif
          endif
      endif
      call fmtnum (sc(23), thps)
      if (sc(23) .eq. sc(24) .and.
     x    sc(23) .eq. sc(25) .and.
     x    checks .eq. 1) then
          smsg (1:) = thps(1:5)
          goto 888
      endif
      thpscm = ','
      call fmtnum (sc(24), thds)
      if (sc(24) .eq. sc(25) .and.
     x    checks .eq. 1) then
          smsg (1:) = thps(1:5) // thpscm //thds(1:5)
          goto 888
      endif
      thdscm = ','
      call fmtnum (sc(25), thcs)
      if (checks .eq. 1) then
          smsg (1:) = thps(1:5) // thpscm //thds(1:5)//thdscm
     x                 // thcs(1:5)
      endif
888   continue
	call fmtnum (sc(177), thcs2)
	call fmtnum (sc(178), thcs3)
	call fmtnum (sc(179), thcs4)
	call fmtnum (sc(180), thcs5)
      if (checks .eq. 2) then
          smsg (1:) = thps(1:5) // thpscm //thds(1:5)//thdscm
     -              // thcs(1:5) // ',' // thcs2(1:5)
      endif
      if (checks .eq. 3) then
          smsg (1:) = thps(1:5) // thpscm //thds(1:5)//thdscm
     -              // thcs(1:5) // ',' // thcs2(1:5) //
     -              ',' // thcs3(1:5)
      endif
      if (checks .eq. 4) then
          smsg (1:) = thps(1:5) // thpscm //thds(1:5)//thdscm
     -              // thcs(1:5) // ',' // thcs2(1:5)
     -              // ',' // thcs3(1:5) // ',' // thcs4(1:5)
      endif
      if (checks .eq. 5) then
          smsg (1:) = thps(1:5) // thpscm //thds(1:5)//thdscm
     -              // thcs(1:5) // ',' // thcs2(1:5)
     -              // ',' // thcs3(1:5) // ',' // thcs4(1:5)
     -             // ',' // thcs5(1:5)
      endif
      nc1 = 5
      nc2 = strlen1 (smsg)
      call wrtstat("THICK", nc1, smsg, nc2)   
c         -----------------------------
c         TOOL_ENDPT
c         -----------------------------
      call fmtnum (sc(1), ptx)
      call fmtnum (sc(2), pty)
      call fmtnum (sc(3), ptz)
      smsg (1:) = ptx(1:6) // ',' //pty(1:6)//',' // ptz(1:6)
      nc1 = 10
      nc2 = strlen1 (smsg)
      call wrtstat("TOOL_ENDPT", nc1, smsg, nc2)    
c         -----------------------------
c         TLAXIS
c         -----------------------------
      call fmtnum (sc(4), ptx)
      call fmtnum (sc(5), pty)
      call fmtnum (sc(6), ptz)
      smsg (1:) = ptx(1:6) // ',' //pty(1:6)//',' // ptz(1:6)
      nc1 = 6
      nc2 = strlen1 (smsg)
      call wrtstat("TLAXIS", nc1, smsg, nc2)     
c         -----------------------------
c         TLAXIS_MODE
c         -----------------------------
      smsg (1:)='TLAXIS/SAME'
      if (ifl(23).eq.0 .and. ifl(293).eq.1) then
         smsg (1:) = 'TLAXIS/SAME,NORMAL'
      endif
      if (ifl(23).eq.1) then
         smsg (1:) ='TLAXIS/NORMAL,PS'
      else if (ifl(23).eq.2) then
         smsg (1:) ='TLAXIS/ATANGL,PS'
      else if (ifl(23).eq.3) then
         smsg (1:) ='TLAXIS/TANTO,DS'
      else if (ifl(23).eq.4) then
         smsg (1:) ='TLAXIS/TANTO,DS,FAN'
      else if (ifl(23).eq.5) then
         smsg (1:) ='TLAXIS/TANTO,DS,PERPTO,VE'
      else if (ifl(23).eq.6) then
         smsg (1:) ='TLAXIS/TANTO,DS,PARELM'
      else if (ifl(23).eq.7) then
         smsg (1:) ='TLAXIS/NORMAL,PS,PERPTO,VE'
      ELSE IF (IFL(23).EQ.8) THEN
         smsg (1:) ='TLAXIS/COMBIN'
      ELSE IF (IFL(23).EQ.9) THEN
         smsg (1:) ='TLAXIS/COMBIN,PARELM'
      ELSE IF (IFL(23).EQ.10) THEN
         smsg (1:) ='TLAXIS/ATANGL,PS,PERPTO,VE'
      ELSE IF (IFL(23).EQ.11) THEN
         smsg (1:) ='TLAXIS/ATANGL,PS,CLDIST'
      ELSE IF (IFL(23).EQ.12) THEN
         smsg (1:) ='TLAXIS/ATANGL,PS,CLDIST,PERPTO,VE'
      ELSE IF (IFL(23).EQ.13) THEN
         smsg (1:) ='TLAXIS/THRU,PT'
      ELSE IF (IFL(23).EQ.14) THEN
         smsg (1:) ='TLAXIS/THRU,CV'
      ELSE IF (IFL(23).EQ.15) THEN
         smsg (1:) ='TLAXIS/INTERP,VE'
      endif
      if (ifl(365).gt.0) then
         j=strlen1(smsg)
         smsg(j+1:)=",LOCK"
      endif
      nc1 = 11
      nc2 = strlen1 (smsg)
      call wrtstat("TLAXIS_MODE", nc1, smsg, nc2)     
 
999   return
      end
