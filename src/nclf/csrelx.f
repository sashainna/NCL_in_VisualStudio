C*********************************************************************
C*    NAME         :  csrelx.f
C*       CONTAINS:
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       csrelx.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:46
C*********************************************************************
C
C **********************************************************************
C **  PROGRAM NAME: CSRELX
C **
C **  PURPOSE OF PROGRAM: CALL CSREL & PERFORM GOUGCK IF GOUGCK FLAG IS
C **                      ON AND CS IS A SURFACE.
C **
C **********************************************************************
C **********************************************************************
 
      subroutine csrelx
 
      include 'com4a.com'
      include 'mocom.com'
      include 'csrel.com'

      REAL*8 ASN
      REAL*4 AD(300),ASC(100)
      INTEGER*2 JD(600), KSN(4)

      EQUIVALENCE(SC,ASC),(ASC(63),SBE),(ASC(64),CBE),(ASC(65),CUTN)
      EQUIVALENCE(D,AD,JD),(ITFL,IFL(49)),(IPTK,IFL(50))
      EQUIVALENCE(IFL(51),IA),(IFL(52),IB),(IFL(53),IC),(IFL(54),ISRF)
      EQUIVALENCE (ASN,KSN)

      real*8 toler
      real*4 h,hh(5),best_cs(7),bestuv(2),hb
      integer*2 kk(5),i,j, ichanged
      logical lv90,aband,aband0

      lv90 = sc(169).lt.9.04999
      lhavecspl = .false.
      toler = sc(27)

      if (icsgck.le.1) goto 100
      if (tool(3) .lt. toler*10.) goto 100
      asn = sc(146)
      if (ksn(4).ne.9) goto 100
c
c..... The code below does exactly the same as the previos version:
c..... it calls csrel with different heights, and picks the nearest
c..... CS plane. The only difference is now csrel is called only once
c..... with each height. Eduard 001013
c
      hh(1) = t(27,ia)
      h = hh(1)
      hb = t(27,ib)
      aband0 = labandon
      call csrel
      if (ifl(2) .eq. 466) return
      if (ifl(2) .gt. 0) goto 200
      dis = t(12,ia)
      ichanged = 0

      call conv4_4 (s(1,3),best_cs,7)
      call conv4_4 (t(25,ia),bestuv,2)
      aband = labandon

      do 5 i = 2,5
5     kk(i) = 1

      hh(2) = 0.
      hh(3) = tool(2)
      hh(4) = 0.5*tool(3)
      hh(5) = tool(3)
      do 20 i = 1,3
        do 10 j = i+1,5
          if (abs(hh(j)-hh(i)) .le. toler) kk(j) = 0
10      continue
20    continue

      do 30 i = 2,5
        if (kk(i).eq.1) then
          t(27,ia) = hh(i)
          labandon = aband0
          call csrel
          if (t(12,ia).lt.dis .and. ifl(2).le.0) then
             ichanged = 0
             dis = t(12,ia)
             h = t(27,ia)
             hb = t(27,ib)
             call conv4_4 (t(25,ia),bestuv,2)
             call conv4_4 (s(1,3),best_cs,7)
             aband = labandon
          else
             ichanged = 1
          endif
        endif
30    continue

      if (ichanged.eq.1) then
        t(12,ia) = dis
        t(27,ia) = h
        t(27,ib) = hb
        call conv4_4 (bestuv,t(25,ia),2)
        call conv4_4 (best_cs,s(1,3),7)
        labandon = aband
      endif

      goto 200

100   call csrel
c
c... If checking TANTO check surface, do extra checks to see if tool
c... is gouging some other part of the check surface. For GOUGCK level
c... 3 always do checks, for GOUGCK level 2, only do checks near check
c... surface.
c
200   if (.not.lv90 .and. lttcsf .and.
     1      (icsgck.gt.2.or.itfl.gt.0.and.icsgck.gt.1)) then
        call csavoid
      endif

999   return
      end

C **********************************************************************
C **  PROGRAM NAME: cselect
C **
C **  PURPOSE OF PROGRAM: select the best CS plane
C **
C **********************************************************************
C **********************************************************************
 
      subroutine cselect (n,ktyp,hpl,par,dis,tol)

      include 'com4a.com'
      include 'mocom.com'
      include 'csrel.com'

      real*4 tol
      real*4 hpl(10,7),par(3,7),dis(7)
      integer*2 n
      integer*2 ktyp(7)

      REAL*8 ASN
      INTEGER*2 KSN(4)
      EQUIVALENCE (ASN,KSN)
      INTEGER*2 ia,ib,ic,isrf,itfl,iptk
      EQUIVALENCE (IFL(51),IA),(IFL(52),IB),(IFL(53),IC),(IFL(54),ISRF)
      EQUIVALENCE (ITFL,IFL(49)),(IPTK,IFL(50))
      REAL*4 ASC(100),sbe,cbe,cutn
      EQUIVALENCE (SC,ASC),(ASC(63),SBE),(ASC(64),CBE),(ASC(65),CUTN)
      REAL*4 AD(300)
      INTEGER*2 JD(600)
      EQUIVALENCE (D,AD,JD)

      real*4 disb
      integer*2 i,ibest,ktypb

      lhavecspl = .false.
      ibest = 0
      ktypb = IGNOR
           
      do i = 1,n
        if (ktyp(i) .lt. IGNOR) then
          if (ktyp(i) .lt. ktypb .or.
     x       (ktyp(i).eq.ktypb .and. dis(i) .lt. disb)) then
            lhavecspl = .true.
            ibest = i
            ktypb = ktyp(i)
            disb = dis(i) - tol
          endif
        endif
      enddo

      if (lhavecspl) then
        call conv4_4 (par(1,ibest),t(25,ia),3)
        if (ifl(83).eq.0) then
          call conv4_4 (par(1,ibest),t(25,ib),3)
        endif
        call conv4_4 (hpl(1,ibest),s(1,3),10)
      endif

      return
      end

C **********************************************************************
C **  PROGRAM NAME: CSRELX1
C **
C **  PURPOSE OF PROGRAM: find the best CS projection and CALL CSREL
C **  CALLED IF GOUGCK FLAG IS 4 AND CS IS A SURFACE.
C **
C **********************************************************************
C **********************************************************************
 
      subroutine csrelx1 (tol)
 
      include 'com4a.com'
      include 'mocom.com'
      include 'csrel.com'

      real*4 tol

      REAL*8 ASN
      INTEGER*2 KSN(4)
      EQUIVALENCE (ASN,KSN)
      INTEGER*2 ia,ib,ic,isrf,itfl,iptk
      EQUIVALENCE (IFL(51),IA),(IFL(52),IB),(IFL(53),IC),(IFL(54),ISRF)
      EQUIVALENCE (ITFL,IFL(49)),(IPTK,IFL(50))
      REAL*4 ASC(100),sbe,cbe,cutn
      EQUIVALENCE (SC,ASC),(ASC(63),SBE),(ASC(64),CBE),(ASC(65),CUTN)
      REAL*4 AD(300)
      INTEGER*2 JD(600)
      EQUIVALENCE (D,AD,JD)

      real*4 h0,h,dh,eps
      real*4 hpl(10,7),par(3,7),par0(2),dis(7)
      integer*2 i,n,ifl2
      integer*2 ktyp(7)

      ifl2 = ifl(2)
      call conv4_4 (t(25,ia),par0,2)
      h0 = t(27,ia)
      lhavecspl = .false.
c
c..... starting projection
c 
      call csrfpl (0,ktyp(1),hpl(1,1),par(1,1),dis(1),tol)
      if (ifl(2) .gt. 0) goto 999

      eps = 20*tol
      if (tool(3) .lt. 2*eps) then
        lhavecspl = .true.
        goto 100
      endif
c
c..... tool forward projection
c
      call conv4_4 (par0,t(25,ia),2)
      t(27,ia) = tool(2)
      call csrfpl (1,ktyp(2),hpl(1,2),par(1,2),dis(2),tol)
      if (ifl(2) .gt. 0) ifl(2) = ifl2
c
c..... several tool axis projections
c 
      dh = tool(2)
      if (dh .lt. eps) dh = eps

      n = tool(3)/dh + 0.5
      if (n .lt. 1) n = 1
      if (n .gt. 5) n = 5
      dh = tool(3)/n

      h = 0
      do i = 1,n
        j = i+2
        ktyp(j) = IGNOR
        if (abs(h - h0) .gt. tol) then
        call conv4_4 (par0,t(25,ia),2)
        t(27,ia) = h
        call csrfpl (0,ktyp(j),hpl(1,j),par(1,j),dis(j),tol)
        if (ifl(2) .gt. 0) ifl(2) = ifl2
        endif
        h = h + dh
      enddo

      call cselect (n+2,ktyp,hpl,par,dis,tol)

      if (.not.lhavecspl) goto 999

100   call csrel

999   if (.not.lhavecspl .or. ifl(2).gt.0) then
        call conv4_4 (par0,t(25,ia),2)
        t(27,ia) = h0
        ifl(2) = ifl2
        call csrelx
      endif

      return
      end

C **********************************************************************
C **  PROGRAM NAME: csavoid
C **
C **  PURPOSE OF PROGRAM: Perform extra checks to make sure front of tool
C **                      is not violating check surface.
C **
C **********************************************************************
C **********************************************************************
 
      subroutine csavoid
 
      include 'com.com'
      include 'const.com'
      include 'mocom.com'
      include 'csrel.com'
c
      real*4 ad(300),asc(100)
      real*4 sbe,cbe,cutn
      integer*2 jd(600)
      integer*2 itfl, iptk, ia, ib, ic, isrf

      equivalence(sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)

      integer*2 i, isv, ifl2
      real*8 tchk(30),s8(10),tsav(30),ssav(10)
      real*8 d0, d1, d2, hd1, sc36
      real*4 f_dot4,vec(3)
      real*4 ua(4) /0.0,1.0,0.5,0.5/
      real*4 va(4) /0.5,0.5,0.0,1.0/
      real*4 dot1,dot2,dot3

      real*8 cspt1(3),cspt2(3),f_dist
      integer*2 isf,ifl68,nwds,ietype,icsv
      integer*4 kcs
      logical lv91,lcrad,ltop,lext
      real*8 toler


      lv91 = sc(169).lt.9.149d0
      toler = sc(27)

      if (ifl(2).gt.0) goto 999
      ifl2 = ifl(2)
      lttcsf = .false.
      sc36   = sc(36)
      sc(36) = -1.0d0

      lcsavd = .false.
      call conv4_8(s(1,3),s8,10)
      call conv4_8(t(1,ia),tchk,30)
      d0 = tchk(12)
c
c... See if there is a closer contact with the check surface
c... by calling csrel with u and v set to the center of each edge
c
      isv = -1
      hd1 = 1.0d9
      ifl68 = ifl(68)
c
c..... ifl(68) is used in csrel to check the top; also we use several CS
c..... heights - Dassault MFGNC243
c
      if (.not. lv91) ifl(68) = 1

      lcrad = .not.lv91 .and. (abs(t(27,ia)-tool(2)).gt.toler)
      ltop  = .not.lv91 .and. (abs(t(27,ia)-tool(3)).gt.toler)

10    continue
      do 20,i=1,4
        t(25,ia) = ua(i)
        t(26,ia) = va(i)
        call csrel
        if (ifl(2).gt.0) then
          ifl(2) = ifl2
        else
          dot1 = abs(f_dot4(t(7,ia),s(1,3)))
c
c..... was: "if (abs(f_dot4(t(7,ia),s(1,3))).gt.0.2) then", starting 15.1
c..... the change is for Dassault MFGNC245
c
          if (dot1.gt.0.2 .or. (itfl.gt.0 .and. dot1.gt.0.1)) then
            d1 = t(12,ia)
            if (ifl(80).eq.0 .and. itfl.eq.5 .and. d1.gt.-toler) goto 20

            call vcmnvc4(s(5,3),s(8,3),vec)
            dot2 = f_dot4(t(7,ia),vec)

            if (dot1 .lt. 0.5) then
c
c.... this 'if' block is to prevent trying to avoid a CS which is turning
c.... away from the tool - a problem occurred in FSR 60365/tad.pp
c.... (GOLFT/SF696,TANTO,SF601)
c
c              do j=1,3
c                vec(j) = s(4+j,3) - s(7+j,3)
c              enddo
c              dot1 = f_dot4(t(7,ia),vec)
c
              if (dot2 .lt. 0. and. d1 .lt. -0.5*tool(1)) then
                dot3 = f_dot4(s(1,3),vec)
                if (dot3 .lt. 0.5*tool(1)) goto 20
              endif
            endif
c
c..... added a check: for oml-042408
c
            if (itfl.gt.0 .and. itfl.le.5 .and. llookfd
     x                                  .and. dot2.lt.0) then
              call cssrf2 (d2)
              if (d0.ge.-toler .and. d2.ge.-toler .and. 
     x            d1 .lt. -5*toler) goto 20
            endif

            if (d1.lt.d0 .and. d1.lt.hd1) then
              hd1 = d1
              isv = i
              call conv4_8(s(1,3),ssav,10)
              call conv4_8(t(1,ia),tsav,30)
            endif
          endif
        endif
20    continue
      llookfd = .false.

      if (lcrad) then
        lcrad = .false.
        t(27,ia) = tool(2)
        goto 10
      else if (ltop) then
        ltop = .false.
        t(27,ia) = tool(3)
        goto 10
      endif
c
c... If there is a closer contact, use it, otherwise restore previous
c
      if (isv.gt.0) then
        call conv8_4(tsav,t(1,ia),30)
        call conv8_4(ssav,s(1,3),10)
c
c......If point is not on extension
c......then do not project point to boundary curve
c......Dassault - FlankR19-tol001.cpp
c
        icsv = ic
        ic = ia
        call gtdesc (sc(13),kcs,nwds,ietype)
        call if_proj_on_extension (kcs,ietype,2,lext)
        ic = icsv
        if (lv91 .or. .not. lext) then
          lcsavd = .true.
        else
          d1 = 10000000.0
          do i=1,3
            cspt1(i) = s(4+i,3)
            cspt2(i) = s(4+i,3)
          enddo
          isf = 1
          call ncl_proj_csds_bndr (isf,cspt1,cspt1,ifl2)
          if (ifl2 .le. 0) then
            isf = 0
            call ncl_proj_csds_bndr (isf,cspt2,cspt2,ifl2)
          endif
          if (ifl2 .le. 0) d1 = f_dist(cspt1,cspt2)

          lcsavd = (d1.gt.toler)
        endif
      endif
c
c..... the assignment above was commented out in previous versions.
c..... now lcsavd is used in endpas
c
      if (.not.lcsavd) then
        call conv8_4(tchk,t(1,ia),30)
        call conv8_4(s8,s(1,3),10)
      endif
      lttcsf = .true.
      sc(36) = sc36
      ifl(68) = ifl68

999   return
      end
