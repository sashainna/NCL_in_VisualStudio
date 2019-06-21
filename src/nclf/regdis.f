c*********************************************************************
c*    NAME:    regdis.f
C*        CONTAINS: regdis
C*    COPYRIGHT 1994 (c) NCCS Inc.
C*     MODULE NAME AND RELEASE LEVEL 
C*       regdis.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:35
c*********************************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine regdis
c*       This routine performs FR/AT...OUT sequence for a single pass
C*       in the RMILL command using local feed rate value.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
      subroutine regdis
c      
      include 'com4a.com'
      include 'comgt.com'
      include 'mocom.com'
c
      real*8 pla(4),plb(4),pt(3),pl(4)
      real*8 pf(3),st(3),tpin(6),tp1(6),tp2(6),tp3(6),tp4(6)
      real*8 sp1(7),sp2(7),apla,aplb,acsa,acsb,aps
      real*8 f1,f2,f3
      integer*2 mpla,mplb,mcsa,mcsb,lt12,lt34,lt13,lt24,iclass,isubcl
      integer*2 kpla(4),kplb(4),kcsa(4),kcsb(4),lseg(2)
      equivalence (kpla,apla),(kplb,aplb),(kcsa,acsa),(kcsb,acsb)

      common/regblk/pl,pt,pla,plb,aps,apla,aplb,acsa,acsb,rho,dro
     1 ,pf,st,tpin,tp1,tp2,tp3,tp4,sp1,sp2,f1,f2,f3,step,istep
     2 ,mpla,mplb,mcsa,mcsb,lt12,lt34,lt13,lt24,lseg

      integer*4 nn
c
      logical lv92
      real*8 fsav
c
      lv92 = sc(169) .lt. 9.249d0
c
c...Set feed rate and perform motion
c
      fsav    = FEEDC(3) 
      call setfed (f1)
      ifl(314) = ifl(317) 
      ifl(315) = ifl(318) 
c
c...vp 11/7/96 bus error fix,
c...make sure that there are points to process
c
      isubcl = 5
cc      if (lexpcl) then
      if (.not. lv92) then
        iclass = 5200
      else
        iclass = 5000
      endif
      call ptpnum (nn)
      if (nn .gt. 0) call csdist (iclass, isubcl)
      call ptclos
c
c...Reset feed rate
c
      ifl(314) = 0
      ifl(315) = 0
      FEEDC(3) = fsav
 8000 return
      end
