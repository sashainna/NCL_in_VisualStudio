C*********************************************************************
C*    NAME         :  mocntl.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*        CHG.  ADD CALL TO MOCOMB FOR COMBINATION FAN AND
C*               PARELM OR TANDS MOTION         PEM       17-APR-89
C*
C*     MODULE NAME AND RELEASE LEVEL
C*       mocntl.f , 25.2
C*     DATE AND TIME OF LAST MODIFICATION
C*       11/23/15 , 09:07:57
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine mocntl
c*     1.   this is the control routine for motion and related tasks.
c*          called by driver and returns to same with err on/off.
c*
c*     2.   at first entry do initial work
c*
c*     3.   is mainly a branching routine per task in sc(10,+).
c*
c*     4.   task commands are:
c*
c*              701  from/
c*              702  go/             (call strtup)
c*              703  goto/
c*              704 ,5,6,7           (call premov, then, mover)
c*              710  godlta/
c*              711  iv/
c*              712  ip/
c*              713  psis/
c*              716  cutter/         (call cutseg)
c*              717  thick/
c*              721  ta/
c*              731  toler
c*              738  pocket          (call pocket)     12-24-81
c*              739  scrub           (call scrub)       3-22-82
c*              748  fmill
c*              750  rmill
c*              761  profil
c*              765  vmpock
c*
c*              Note: All except toler call a subroutine. If none
c*                      specified, call motimm.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine mocntl

      include 'com8a.com'
      include 'comgt.com'
      include 'mocom.com'
      include 'rrdm.com'
      include 'suvcom.com'
      include 'vmpcom.com'

      common/hptcom/hldctp
      real*8 hldctp(27)
 
      integer*2 toptyp,bottyp,stktyp,conpoc,layer,idum,frmtyp
      integer*2 iflg,iclass,isubcl,iclf,dpsav
      integer*2 ksn(4)
      integer*4 topkey,botkey,nptkey,nn
      integer*4 isv2(2), iept(2)
      real*4 lvsav
      real*8 zbot,ztop,stkoff,maxgap,lastmx,lastmy,lastmz
      real*8 endmx,endmy,endmz,cnvmm
c
      equivalence (asn,ksn)
      equivalence (lastmx, sc(171)), (lastmy, sc(172))
      equivalence (lastmz, sc(173))
      equivalence (isc19 , sc(19))

      logical lgfa,lv92,lv93,lv97

      character*2 ct2
      character*1 ct(5)
      equivalence (ct,ct2)

      isubcl = 1
      lgfa = lautgf
      lv92 = sc(169) .lt. 9.249d0
      lv93 = sc(169) .lt. 9.349d0
      lv97 = sc(169) .lt. 9.749d0
c
C..... HOLD SC(25) IN HCSTHK.  MAY CHG IN GO/SF1 OR GO/S1,S2
C..... RESTORE SC(25) AT MOCNTL EXIT.           23-FEB-88
c
      HCSTHK=SC(25)
c
c..... check if cam is allowed at this terminal
c
      call autcam (i)
c
c....Added for NCL501+ .Paul 
c....02/03/92 
c....Old version was: 
c....if (ifl(35).eq.0 .and. i.eq.0 .and. ifl(268).eq.0) then
c 
      if ((ifl(35).eq.0 .or. (ifl(35) .eq. 2 .and. ifl(350) .eq. 1)) 
     x.and. i.eq.0 .and. ifl(268).eq.0) then
        ifl(2)=259      
        err=.true.
        goto 99999
      endif

c
c..... set vt125 cutter circle count to zero
c
      ifl(130)=0
c
c..... save coordinates of where tool ended last motion so if 
c..... this motion is aborted the display code can be told where to
c..... start from again.  Plotm uses these coordinates when ifl(270) 
c..... is set to -1
c
      endmx = sc(1)
      endmy = sc(2)
      endmz = sc(3)
c
c..... zero nwds in tdat o/p array           9-01-81
c
      ifl(79)=0
c
c..... branch on task type.
c
      asn=sc(10)
      mtyp=ksn(1)

c
C..... IF IFL(135) IS 1 SET UP TO DO A DISPLY IN ERASE MODE.
C..... THIS IS SET IN ALTCOM FROM AN 'ERASE/GEOTYP' COMMAND.
c
      IF (IFL(135).EQ.1) MTYP=1021
 
C************ DISPLY **************
      IF(MTYP.NE.1021) GOTO 5
        CALL DISPLY
        GOTO 99
c
c...GO/sf1,...
c
    5 continue
      if (mtyp.eq.702) then
        call gocntl
        go to 99
      endif
c
c...Basic Move
c
   20 if (mtyp.lt.704.or.mtyp.gt.709) goto 30
      isubcl = 5
c
c...vp 11/17/97 now mocomb is called from motgxx to support multi CS
c... and nearpt logic 
c
25     call motgxx
C
C...ifl(347) is to keep track of motion for ARCSLP/FILLET JLS 4/14/99
C
      if (ifl(347).ge.1. and. ifl(2).lt.1 .and. (ifl(42).eq.0 .or.
     1    ifl(372) .eq. 1)) then
         ifl(347) = ifl(347) + 1
         if (lgfa) ifl(347) = ifl(347) + 1
      endif
c
c... restore NUMPTS, MAXDP and MAXANG ONCE
c
      if (ifl(2).le.0) then
        ifl(91) = ifl(368)
        sc(54)  = sc(105)
        if (.not. lv97) then
            sc(162) = sc(214)
            lstep = lstepv
        endif
        sc(80)  = sc(201)
      endif
      goto 99

c************ cutter **************
30    if (mtyp.ne.716) goto 35
      call cutseg
      goto 99

c************ pocket **************
35    if (mtyp.ne.738) goto 36
        if (ksn(2) .eq. 4) then
          layer = ksn(3) 
          conpoc = ksn(4) 
          call gtdesc(sc(11),botkey,iflg,bottyp)
c
c..... iflg = 100 means based on a surface of revolution 
c
          if (iflg .eq. 100) then
            bottyp = bottyp + 100
            psmult = .false.
            ifl(331) = 0
          endif
          zbot = sc(12)
          call gtdesc(sc(13),topkey,idum,toptyp)
          ztop = sc(14)
          stktyp = sc(15)
          stkoff = sc(16)
          maxgap = sc(17)
          call gtdesc(sc(18),nptkey,idum,frmtyp) 
          call ncl_setptr(imotp,isv2)
          call ncl_zroptr(iept)
          iclf = 0
          if (wmeth .eq. 1) then
              lvsav  = numlvl
              dpsav  = lvdep
              numlvl = VMPPAS(1)
              lvdep  = IVMPFL(10)
          endif
          call watrln(layer,botkey,bottyp,zbot,topkey,toptyp,ztop,
     x       stktyp,stkoff,maxgap,frmtyp,nptkey,conpoc,numlvl,lvdep,
     x       wmeth,ifl(2))
          if (wmeth .eq. 1) then
              numlvl = lvsav
              lvdep  = dpsav
          endif
          if (ifl(2) .gt. 0) then
            call ncl_eqlptr(isv2,imotp,iflg)
            if (iflg.eq.0)
     x         call cldel(iclf,isv2,iept,imotp)
            call ptclos
          endif
          call delsky
          if (ifl(2).ne.0 .and. ityp.eq.2 .and.ist.eq.1) ist = 22
        else
          call pocket
c          call vmill2
        endif
      if (ifl(347).ge.1. and. ifl(2).lt.1 .and. (ifl(42).eq.0 .or.
     1    ifl(372) .eq. 1)) then
         ifl(347) = ifl(347) + 1
      endif
      goto 99

c************ scrub ***************
36    if(mtyp.ne.739) goto 37
      call scrub
      if (ifl(347).ge.1. and. ifl(2).lt.1 .and. (ifl(42).eq.0 .or.
     1    ifl(372) .eq. 1)) then
         ifl(347) = ifl(347) +1
      endif
      goto 99

c************ smill ***************
37    if(mtyp.ne.756) goto 38
      call smill
      if (ifl(347).ge.1. and. ifl(2).lt.1 .and. (ifl(42).eq.0 .or.
     1    ifl(372) .eq. 1)) then
         ifl(347) = ifl(347) +1
      endif
      goto 99
      
c************ lathe ***************
38    if(mtyp.ne.700) goto 39
      call lthctl
      if (ifl(347).ge.1. and. ifl(2).lt.1 .and. (ifl(42).eq.0 .or.
     1    ifl(372) .eq. 1)) then
         ifl(347) = ifl(347) +1
      endif
      goto 99

c************ rmill ***************
39    if(mtyp.ne.750) goto 40
      call regmil
      if (ifl(347).ge.1. and. ifl(2).lt.1 .and. (ifl(42).eq.0 .or.
     1    ifl(372) .eq. 1)) then
         ifl(347) = ifl(347) +1
      endif
c
c... restore NUMPTS, MAXDP and MAXANG ONCE
c
      if (ifl(2).le.0) then
        ifl(91) = ifl(368)
        sc(54)  = sc(105)
        if (.not. lv97) then
            sc(162) = sc(214)
            lstep = lstepv
        endif
        sc(80)  = sc(201)
      endif
      go to 99

c************ patern **************
40    IF (MTYP.NE.703.OR.ISC10(2).NE.3) GOTO 42
      call motpn
      if (ifl(347).ge.1. and. ifl(2).lt.1 .and. (ifl(42).eq.0 .or.
     1    ifl(372) .eq. 1)) then
         ifl(347) = ifl(347) +1
      endif
      goto 99

c************ fmill **************
42    IF (MTYP.NE.748) GOTO 44
      call fmill
      if (ifl(347).ge.1. and. ifl(2).lt.1 .and. (ifl(42).eq.0 .or.
     1    ifl(372) .eq. 1)) then
         ifl(347) = ifl(347) +1
      endif
      goto 99
c
c...PROFIL
c
   44 if (mtyp .ne. 761) goto 46
      call profil
      if (ifl(347).ge.1. and. ifl(2).lt.1 .and. (ifl(42).eq.0 .or.
     1    ifl(372) .eq. 1)) then
         ifl(347) = ifl(347) +1
      endif
      goto 99
c
c...VMPOCK
c
   46 if (mtyp.eq.765) then
         wmeth = 1
         call vmill2
         if (ifl(347).ge.1. and. ifl(2).lt.1 .and. (ifl(42).eq.0 .or.
     1       ifl(372) .eq. 1)) then
            ifl(347) = ifl(347) + 1
         endif
         goto 99
      endif

c************ all others **********
50    call motimm
C
C...ifl(347) is to keep track of motion for ARCSLP/FILLET JLS 4/14/99
C
      if (ifl(347).ge.1. and. ifl(2).lt.1 .and. (ifl(42).eq.0 .or.
     1    ifl(372) .eq. 1)) then
         if (mtyp.eq.703 .or. mtyp.eq.710 .or. mtyp.eq.803) then
           ifl(347) = ifl(347) +1
         endif
      endif
c
c..... temp wrt this motion rec on ranfil for sm sys use.
c
99    continue
      if (ifl(2).ne.0) err=.true.
c
c..... turn off circ int flg now
c
      ifl(95)=0
c
C..... ALSO DEBUGM OFF               7-DEC-82
c
      IFL(85)=0
c
C..... RESTORE CSTHK                 23-FEB-88
c
      SC(25)=HCSTHK
c
c....Added for NCL501+ .Paul
c....02/03/92
c....Old version was:
c....if (ifl(35).eq.0) then
c
      if (ifl(35).eq.0 .or. (ifl(35) .eq. 0 .and. ifl(350) .eq. 2)
     x    .or. (ifl(35) .eq. 2 .and. ifl(350). eq. 1)) then
c          set motion graphics to ending position of prior motion and delete
c          any motion displayed from this statement.  Do NOT do if this was
c          a "tool not in correct position" with a "no" response situation 
c          since there was no graphic motion generated.
c
c...Changed to new motion display logic
c...Bobby  -  12/18/92
c        if (ifl(270).gt.0) call motend
c
        if (ifl(2).gt.0 .and. ifl(2) .ne. 142) then
cc          call moters
          cnvmm = 1.0d0
          if (ifl(264).eq.1) cnvmm = 1.0d0 / 25.4d0
          lastmx = endmx * cnvmm
          lastmy = endmy * cnvmm
          lastmz = endmz * cnvmm
C
C...If ifl(347) is equal to 3 and no rapid, then this is the
C...second motion if a fillet, so erase the motion, it will
C...be replotted in pfillet. JLS 4/14/99
C...Now erase motion if necessary in pfillet IJD 1-OCT-99
C
        else if (ifl(347).lt.3.or.rpfron) then
cc          call motend
        endif
      endif
c
c...Process GOTO record if version > 8.209
c 
      call ncl_tstptr(nrcn,iflg)
      if (iflg .ne. 0 .and. sc(169) .gt. 8.229) then
          call ptpnum (nn)
          if (nn .gt. 0) then
cc            if (lexpcl) then
            if (.not. lv92) then
              iclass = 5200
            else
              iclass = 5000
            endif
            isubcl = 5
            call csdist (iclass, isubcl)
cc            if (lexpcl) then
            if (.not. lv92) then
              do 60 i=1,6
60            hldctp(i+6) = hldctp(i+21)
              iclass = 5220
              isubcl = 3
              numitm = 13
              call putcl (iclass,isubcl,numitm,hldctp)
            endif
          endif
          call ptclos
      endif

99999 continue
      return
      end
