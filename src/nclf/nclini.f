C*********************************************************************
C*    NAME         :  nclini.for
C*       CONTAINS: initialize NCL system at startup
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       nclini.for , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       10/27/16 , 13:56:03
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nclini
c*       sets up all required start up variables
c*       and fields at initial startup and at restart time.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
 
      subroutine nclini
 
      include 'com8a.com'
c      include 'nclrd.com'
      include 'dtcom.com'
      include 'vaxvoc.com'
      include 'rrdm.com'
      include 'cutter.com'
      include 'comgt.com'
      include 'mocom.com'
      include 'status.com'
 
      equivalence (quit,iquit)
      character*1 aain(49),afname(49),acbuf(72)
c      logical brafnd ,leq1
      logical leq1
      logical iin,  lfname
      equivalence (cin,iin,aain),(iassoc,ifl(71)),(acbuf,cbuf)
c      character*1 cdum
c      real*8 woc(5)
c      integer*2 khld(20),perpos
      integer*2 isv35, isv142, isv187, isv226, isv227, isv259, isv260
      integer*2 isv297, isv205, isv264, isv206, isv207, isv304, isv351
      integer*2 isv106,isv387,isv389
      integer*4 numrec,nup,nupt,nvp,nvpt
      character*5 cquit
      equivalence (cquit,cin)
      character*49 fname
      equivalence (fname,afname,lfname)
      character*80 ppemsg,clemsg,cbuf
c
c     added kathy
      character*(MAX_PATH) temp
      integer numf,ipos
      integer*4 mxc,strlen1,ncf
      character*(MAX_PATH) ldir
      integer*4 nc
      character*(MAX_PATH) filename
c
c      logical lpri,npri
c      character*49 ifnam,cfnam,prfnam
      character*49 prfnam
      equivalence (ncsfl(1),ili)
      character*8 csc118
      equivalence (sc(118),leq1),(leq1,csc118)
      real*4 asc162(2)
      integer*2 isc162(4)
      equivalence (sc(162),asc162,isc162)
 
c          kd is used to init ps to z=0. in dtbl
      integer*2 kd(4)
      equivalence (d(1),kd(1))
      character*8 ctime
      character*11 cdate
c      integer*2 itbuf(16)
      character*6 ltbuf
      logical lltbuf,ltime
      equivalence (ltbuf,lltbuf),(ctime,ltime)
 
      character*2 clas
      character*1 lclnam(MAX_FILE),lasnam(MAX_FILE),lprnam(MAX_FILE),
     1            lppnam(MAX_FILE)
      equivalence (lclnam,clfnam),(lasnam,asfnam),(lprnam,prfnam)
      equivalence (lppnam,ppfnam),(clas,cin)
 
      character*(MAX_PATH) vfile,efile
      logical    isvstp
      logical  lbset
      logical  ldset
 
      integer*2 units
      integer*4 kunit,set1,set2
      equivalence (units, ifl(264))
      integer*2 ramp, cclw, out, up, sharp, rapid, mm
      parameter (ramp = 854)
      parameter (cclw = 59)
      parameter (out = 653)
      parameter (up = 112)
      parameter (sharp = 183)
      parameter (rapid = 5)
      parameter (mm = 1)
 
      data ppemsg/'i/o error occured opening part program file '/
      data clemsg/'i/o error occured opening cl file '/
 
c          set the version number of this NCL executable
      call setver
c
c...Initialize OpenNCL and NCL common variables
c...Save the following variables first
c......NCL Version number - sc(119)
c......Lathe Package      - ifl(142)
c......Terminal type      - ifl(187)
c......Data Tablet        - ifl(205)
c......Units              - ifl(264)
c......Stop flag          - stpflg
c......NCLT Task name     - ifl(206)
c......3-Axis Mode        - ifl(304)
c......Prepst options     - ifl(351)
c......VoluMill options   - ifl(389)
c
      sc119=sc(119)
      isv35 = ifl(35)
      isv106 = ifl(106)
      isv142 = ifl(142)
      isv187 = ifl(187)
      isv226 = ifl(226)
      isv227 = ifl(227)
      isv259 = ifl(259)
      isv260 = ifl(260)
      isv297 = ifl(297)
      isv205 = ifl(205)
      isv264 = ifl(264)
      isvstp = stpflg
      isv206 = ifl(206)
      isv207 = ifl(207)
      isv304 = ifl(304)
      isv351 = ifl(351)
      isv387 = ifl(387)
      isv389 = ifl(389)
      rsc119 = sc(119)
      kunit  = units
      call nclxini (kunit,rsc119)
c
c...Restore variables
c
      sc(119)=sc119
      ifl(100) = 1
      ifl(35) = isv35
      ifl(106) = isv106
      if (ifl(106) .eq. 0) ifl(106) = 72
      ifl(142) = isv142
      ifl(187) = isv187
      ifl(226) = isv226
      ifl(227) = isv227
      ifl(259) = isv259
      ifl(260) = isv260
      ifl(297) = isv297
      ifl(205) = isv205
      ifl(264) = isv264
      ifl(206) = isv206
      ifl(207) = isv207
      ifl(304) = isv304
      ifl(351) = isv351
      ifl(387) = isv387
      ifl(389) = isv389
 
c            get current date and time
      call ncdate (cdate)
      call nctime (ctime)
      i=0
      if (ifl(35).eq.0) call stmdax (i, sc)
c            initialize variables
      call flunit
      call inibuf
      init=.true.
      err=.false.
      if (.not.quit) debug=.false.
      srceof=.false.
      iw2inx=0
      idinx=0
      isvtyp=0
      ieof=-1
      isrcix=-1
      nline=1
      mpage=0
      pmode=0
      lbltbl=0
      numrec=0
      nrec=0
      nerr=0
      SIMCOM = .false.
c      ppfnam = 'none'
c      clfnam = 'none'
c      asfnam = 'none'
      mxc = MAX_FILE
      call ldefpp (lppnam,lclnam,lasnam,mxc)
      ncf = strlen1(ppfnam)
      call ptdfnm (ppfnam,ncf)
c
c..... Added by Eduard: the default value of ifl(346)
c
      ifl(346) = 0
      if (sc(169) .lt. 9.050) ifl(346)=1
      call ncl_conv_sfseg_reset
      call nuldsp
c
c...Initialize Expanded clfile
c...Set machining library flag
c...MULTAX/OFF
c...*RESET/AUTOST
c...Clfile off
c
      lexpcl = .false.
      ifl(330) = 0
      ifl(82) = 0
      autost = .false.
      ifl(89) = 0
      ifl(69) = 0
 
c
c     Initialize the flags for error messages. One error per processed line.
      svmsg = 0
      svln =1
 
 
10    call rancls
      close (unit=dblun)
c
c...check if error file exists here
c...if file not opened set errlun = -1
c...Yurong
c
      call flname(4, 'nclerrs{', efile)
      call flopen(errlun, efile, 'OLD', 'DIRECT', 'UNFORMATTED', 80,
     x'NULL',  ioerr)
      if (ioerr.ne.0) then
         write(cout, 200)
         nc = 28
         call ferrmsg(cout,nc)
         errlun = -1
      else
         close (errlun)
      endif
 
  200 format('failed to open "nclerrs.ncl"')
 
      ltbuf=' '
cuni      open (unit=voclun,name='nc00:nclvoc.ncl',status='old',
cuni     x       shared,readonly)
      call flname(1, 'nclvoc{', vfile)
      call flopen(voclun, vfile, 'OLD', 'SEQUENTIAL', 'FORMATTED', 80,
     x'NULL', ioerr)
      if (ioerr .eq. 0) goto 8888
         write(cout, 8887)
c...Call error message display function
c...because if the Window haven't open yet (that is the case here)
c...putmsg do nothing, and we need exit to NCL if this file can't
c...open
c
         nc = 56
         call ferrmsg(cout,nc)
 
c...         call putmsg(cout,56,0,0)
c...8887     format('********failed to open vocabulary file********')
8887     format('****failed to open vocabulary file, Exit to NCL****')
         goto 5000
8888  continue
c.....
c..... 512 changed to 1024. Paul 08/06/92
c.....
      do 12 i=1,1024
        read (voclun,1212,end=1216) vvcnam(i),vvcnum(i)
1212    format (a24,i4)
        if (vvcnam(i)(1:6).eq.'ZZZZZZ' .and.
     x       vvcnum(i).eq.9999) goto 1216
12    continue
 
1216  close (voclun)
c....
c.... 512 changed to 1024. Paul 08/06/92
c....
      do 1218 j=i,1024
        vvcnam(j)='ZZZZZZ'
        vvcnum(j)=9999
1218  continue
c.....
c..... Call synini routine to try to open nclvoc.syn
c..... file and to include all changes from it to vocabulary
c..... Paul 08/12/92
c.....
      call synini
c.....
      do 15 i=1,143
15        jb(i)=0
c          init recs 1-40 ranfil for init vst setting
      do 20 i=1,100
20        call putran(jb,i)
 
c          set beginning geometry storage page
      ifl(4)=41
      ifl(7)=41
 
c          init error limit
      ifl(31)=25
 
c          init warning limit
      ifl(32)=25
 
c          set autol1 on
      autol1 = .true.
 
      ifl(128) = 1
 
c          set cutter circle display default to 999
      ifl(129) = 999
 
c          turn on print flag
c     ifl(218) = 1
 
c          set plotm buffer flush count
      ifl(269)=10
 
c ************************************************************************
c              interactive processing section
c ************************************************************************
 
c         set processing mode flag to interactive (reset by ifl(x)=0 loop)
35    ifl(35)=0
c         if there is a data tablet being used load menus to the tek crt
cuni      if (ifl(170).eq.1) call menuld
 
550   init=.false.
      quit=.false.
c                          set mode to sequential
      ifl(37)=1
c                          set macro flag to 0
      ifl(38)=0
      svmaci = ' '
c                          set motion flag
      motdfl = .true.
      sc(170) = 5.0d-8
c set the autops flag
      fautops = .false.
c                          init ps as z=0 plane    (entity type 6)
      kd(1)=6
      d(5)=1.
c
c     restore the *stop flag
c
      stpflg = isvstp
c                          set label default off.
      call lblini(set1,set2)
      lbset = set1.eq.1
      ldset = set2.eq.1
      lablpt = lbset
      lablve = lbset
      lablln = lbset
      lablci = lbset
      lablcv = lbset
      lablsf = lbset
      lablsh = lbset
      lablpl = lbset
      lablpv = lbset
      lablmx = lbset
      lablpn = lbset
c                          set label leader line default off.
      lbldpt = ldset
      lbldve = ldset
      lbldln = ldset
      lbldci = ldset
      lbldcv = ldset
      lbldsf = ldset
      lbldsh = ldset
      lbldpl = ldset
      lbldpv = ldset
      lbldmx = ldset
      lbldpn = ldset
c                          set disply default on.
      dsplpt = .true.
      dsplve = .true.
      dsplln = .true.
      dsplcv = .true.
      dsplci = .true.
      ldspsf = .true.
      ldspsh = .true.
      dsplpl = .true.
      dsplmx = .false.
      ldsppn = .true.
      ldsppv = .true.
      dsplan = .true.
      dsplsy = .true.
      dsplso = .true.
c                          set aptcom (NCL statements as comments in apt source)
      aptcom = .true.
c                          set aptmark (output the REMARK program-information li
      aptmark = .true.
c                          don't output REMARK statements to APT source
      aptrem = .false.
      ipvcom = .false.
c                          set autouv
      auvset = .true.
c                          set adispl and defaults
      ifl(136) = 0
      ifl(137) = 5
      ifl(138) = 5
      sc(175) = .005
c
c...SET/STOP
c...    PAUSE
c
      onstop = .true.
      onpaus = .true.
c
c...vp 17-mar-94 init CAD/CAM common param for sf disply
c
      nup  = ifl(137)
      nvp  = ifl(137)
      nupt = 0
      nvpt = 0
      call ptsfdp (nup,nupt,nvp,nvpt)
      call initss (sc(175))
c
      ifl(184) = 1
c                          set letsiz & scale to default
 
      ifl(134)=100
      sc(106)=1.
c                          set default grid size
      sc(134) = .25
 
c                          init halt name for debug to blanks
      csc118='        '
      errcom=' '
 
c                          initialize Advanced POCKET modal values
c                          (found in rrdm.com common block)
      nramps = 1
      entry = ramp
      rmpdis = 0.
      clrlvl = 1.d0
      clrlv_lab = ' '
      clrlv_inx = 0
c     if (units .eq. mm) clrlvl = 25.d0
      cltyp = 0
      numlvl = -1
      lvdep = 0
      retdis = .1
      hrtdis = .1
      vrtdis = .1
      hscalp = .001
c     if (units .eq. mm) retdis = 2.5
      pdir = cclw
      hldir = cclw
      lctyp = 0
      lcdir = 0
      sttyp = 0
      ispirl = out
      slift = up
      corner = sharp
      transt = sharp
      maxstp = 0.
      minstp = 0.
c      genfr = 20.
      genfr = 0.
c     if (units .eq. mm) genfr = 500.
      posfr = 0.0
      retrfr = 0.0
      entrfr = -.5
      tranfr = -.5
      finfr = -1.
      frsfr = -1.
      nloops = 9999
      rtract = 1
      arcrad = 0
      slwang = 200
      nwrn = 0
      maxang = 90.
      jctcom = 0
      jctcm1 = 0
      jctcm2 = 0
c
c...   Initialize name list
c
      call vxlini
 
c         Initialize index into program mode array used by STATLN.F
      pgindx = 1
      pgmode(1) = 0
c
c...Initialize looping region
c
      ifl4(4) = -1
      ifl4(5) = -1
      ifl4(6) = -1
      ifl4(7) = -1
      ifl4(8) = -1
      ifl4(9) = -1
      ifl4(11) = -1
      ifl4(12) = -1
      ifl4(13) = -1
c
c...Initialize arcslp/fillet cl record numbers
c
      call ncl_zroptr (isrec)
      call ncl_zroptr (isrec2)
      call ncl_zroptr (isrec3)
c
c...Initialize feedrate and motion variables.
c
      do 505 i=1,200
         MOTMAP(i) = 0.d0
  505 continue
c
c...Initialize cutter display
c
      call mdinit
c
c...Initialize VOLUMILL
c
      call vmpini
      ncdata = 'NCL_TOOL'
      go to 99999
 
5000  quit=.true.
 
99999 continue
      return
      end
c
      subroutine ncdate (ld)
c
      integer*4 id(3)
      character*11 ld
      character*3 cd(12)
      integer vals(8)
      character*12 rc(3)
c
      data cd /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1         'SEP','OCT','NOV','DEC'/
ccc=VMS
ccc      call date (ld)
ccc=UNX
c
c...Changed to match Unix standard
c...Bobby  -  1/14/92
c
      call date_and_time (rc(1),rc(2),rc(3),vals)
      id(1) = vals(3)
      id(2) = vals(2)
      id(3) = vals(1)
      if (id(3) .lt. 1900) id(3) = id(3) + 1900
      if (id(3) .lt. 1970) id(3) = id(3) + 100
      write (ld,1) id(1),cd(id(2)),id(3)
1     format (i2,'-',a3,'-',i4.4)
      return
      end
c
      subroutine nctime (lt)
c
      include 'com8a.com'
 
      common /ntmcom/it,qtime2
      integer*4 it(3)
      real*4 qtime2
 
      character*8 lt
      integer vals(8)
      character*12 rc(3)
      call date_and_time (rc(1),rc(2),rc(3),vals)
      it(1) = vals(5)
      it(2) = vals(6)
      it(3) = vals(7)
      write (lt,2) it(1),it(2),it(3)
2     format (i2,':',i2.2,':',i2.2)
      return
      end
 
c***********************************************************************
c
c   E-FUNCTION:  isquit ()
c
c   FUNCTION:  This routine returns 'quit' value (called by C routine)
c
c   INPUT:  None
c
c   OUTPUT: isquit:
c
c***********************************************************************
c
      logical*4 function isquit ()
c
      include 'com.com'
 
      isquit = quit
 8000 return
      end
