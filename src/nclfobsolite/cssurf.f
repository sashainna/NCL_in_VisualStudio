c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       cssurf.f , 25.1
c**     DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:46
c**
c*****************************************************
C
C***********************************************************************
C                                                                      *
C          CALLED BY CSREL WHEN CS IS A SURF. PUTS THE                 *
C          RESULTANT P/N IN S( ,3).  ALSO PUTS U,V,H IN TCOL(IA).      *
C                                                                      *
C          FULL TOOL-TO-CS CALCS ARE RUN ONLY WHEN TOOL NEARS CS.      *
C                                                                      *
C        CHG  3-feb 88   barrel cutter addition                        *
C                                                                      *
C***********************************************************************
 
      subroutine cssurf
 
      include 'com4a.com'
      include 'mocom.com'
      include 'csrel.com'

      integer*2 jd(600)
      real*4 ad(300),hs(7),asc(310),xv(3),fv(3),vpos(3),spt(3)
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      real*4 p99,t99,co12,co,si,hrad,d1,f_dot4,f_mag4
      real*4 hsidtn,x1,y1,xsec,ysec
      logical lv91,goug
      real*8 toler
c
c...Debug variables
c
      integer*4 DEBUGX
      parameter (DEBUGX=0)
      character*80 dbuf
      byte dout(80)

      lv91 = sc(169).lt.9.149d0
      goug = (.not.lv91 .and. icsgck.gt.1 .and. sc(36).ne.0. 
     *        .and.t(27,ia).eq.tool(2))
      toler = sc(27)

c          get entry h-val and misc params
      p99=.9999
      t99 = 70.705375
      BIGNEG=-1.E9
      SFI=ASC(305)
      CFI=.001
      IF (ABS(SFI).LT.1.) CFI=SQRT(1.-SFI*SFI)
      H=T(27,IA)
C                 SET H DELTA VALUE FOR 2 SIGHTINGS OF CKSRF     26-FEB-88
C                  IF UNITS/MM INCREASE*25.4
      HDEL=.05
c      if (ifl(264).eq.1 .or. ifl(362).eq.1) HDEL=1.27
      IF (IFL(264).NE.0) HDEL=1.27
      IF (IFL(83).NE.0.AND.IFL(23).NE.4) H=H+HDEL
      x=t(1,ia)
      y=t(2,ia)
      z=t(3,ia)
      a=t(4,ia)
      b=t(5,ia)
      c=t(6,ia)
      itim=0
      hrad=tool(1)/2.
      u=t(25,ia)
      v=t(26,ia)
c          ept is hcs along ta from te
c           if disk tool + endup, calc h per last cspl estimate.
10    if (asc(66).eq.0.or.itfl.lt.1) goto 105
      if (itim.gt.0.or.sc(36).eq.0.) goto 105
      tbe=sbe/cbe
      sal=a*s(1,3)+b*s(2,3)+c*s(3,3)
      cal=.001
      if (abs(sal).lt.1.) cal=sqrt(1.-sal*sal)
      tal=sal*sc(36)/cal
      if (tal.lt.tbe) goto 103
c           rcorn contact
      h=tool(2)+tool(6)*tal
      goto 105
c           edge contact
103   h=tool(3)+tal*hrad
105   continue
c
c...gougck level2,3 added an look point on the tool ring.jingrong 12/06/99.
c
      if (goug) then
         h = t(27,ia)
         call uvcplvc4(t(1,ia),t(4,ia),xv,h)
         call triple_cross4(t(4,ia),t(7,ia),t(4,ia),fv)
         call unitvc4(fv,fv)
         d1 = -tool(6)*sc(36)
         call uvcplvc4(xv,fv,s(8,3),d1)
         call surfpn(u,v,1)
         if (ifl(2) .eq. 466) goto 99
         if (ifl(282).ne.0 .or. asc(66).ne.0.) goto 99
c
c..... spt is the look point projection onto the surface plane
c
         call point_on_plane4(s(8,3),s(5,3),s(1,3),spt)
         eps = sc(25) + 5.*toler
c
c.... the parameter change below is for QAR 92325, which had the angle=3
c.... and the thick=10
c
         if (sc(31).gt.1. .and. sc(25).gt.10.*toler) eps = sc(25)+toler
c
c... If spt is inside the cutter (or almost), exit with this surface plane.
c
         call vcmnvc4(spt,t(1,ia),fv)
         y1 = f_dot4(t(4,ia),fv)
         x1 = sqrt (f_dot4(fv,fv) - y1*y1)
         hsidtn = tool(2)*(1.-sbe)

         xsec = cutn*cbe + x1*sbe*sbe + y1*cbe*sbe
         ysec = -cutn*sbe + x1*cbe*sbe + y1*cbe*cbe
c
c..... x1,y1 are spt coordinates in the plane through the tool axis and spt.
c..... xsec,ysec is the intersection of the line from spt perpendicular to the
c..... cutter side with the line through the cutter side
c..... Note: the previous formulas were incorrect for an angular tool
c
         llookfd = .true.
         if (ysec .ge. hsidtn - toler) then
           if (x1 .lt. xsec + eps) goto 99
         else if (y1 + eps .gt. 0) then
           call uvcplvc4(fv,t(4,ia),vpos,-y1)
           call unitvc4(vpos,vpos)
           call avcplbvc4 (tool(2),t(4,ia),tool(6),vpos,fv)
           call vcplvc4(t(1,ia),fv,xv)
c
c..... xv is the tool ring point closest to spt
c
           call vcmnvc4(spt,xv,fv)
           if (f_dot4 (fv,vpos).lt.eps) goto 99
           if (f_mag4 (fv) .lt. eps + tool(2)) goto 99
         endif
         llookfd = .false.
c
c..... if this projection is far from surface, we go back and project as 
c..... if no gougck
c
         u=t(25,ia)
         v=t(26,ia)
         goug = .false.
         goto 10
      else
         s(8,3)=x+h*a
         s(9,3)=y+h*b
         s(10,3)=z+h*c
         call surfpn(u,v,1)
C RLS - 03/27
         if (ifl(2) .eq. 466) go to 99
C RLS - END
      end if
c              if not near cs, exit now.  (also exit on itim =3)
      if (ifl(83).eq.0.or.ifl(23).eq.4) goto 99
      itim=itim+1
      if( itim-2) 20,30,99
c             time 1.  sto pv1 data, go do pv2
20    do 22 i=1,7
22    hs(i)=s(i,3)
      h1=h
      h=h-2.*hdel
      goto 10
c             time 2.  ave pv1,pv2 for approx yt
30    h2=h
c      if pl1 parlel pl2, go do average pl and exit    11-22-82
      co12 = abs (hs(1)*s(1,3)+hs(2)*s(2,3)+hs(3)*s(3,3))
      if (co12 .gt. .999999) goto 372
 
      yta=hs(1)+s(1,3)
      ytb=hs(2)+s(2,3)
      ytc=hs(3)+s(3,3)
c             rgt sense
      rx=ytb*c-ytc*b
      ry=ytc*a-yta*c
      rz=yta*b-ytb*a
c             actual yt axis   (perpto rgt and ta)
      yta=rz*b-ry*c
      ytb=rx*c-rz*a
      ytc=ry*a-rx*b
      sec=sqrt(yta*yta+ytb*ytb+ytc*ytc)
      if (sec.gt.0.) goto 32
cccccccccccccccccccccccc  err=51 temp
c          error.  sec=zero or some degen case
31    ifl(2)=51
      goto 99
cccccccccccccccccccccccc
32    yta=yta/sec
      ytb=ytb/sec
      ytc=ytc/sec
c             sfpt 1 in t-sys
      dx=hs(5)-x
      dy=hs(6)-y
      dz=hs(7)-z
      zt1=a*dx+b*dy+c*dz
      yt1=yta*dx+ytb*dy+ytc*dz
c             sfpt 2  in t-sys
      dx=s(5,3)-x
      dy=s(6,3)-y
      dz=s(7,3)-z
      zt2=a*dx+b*dy+c*dz
      yt2=yta*dx+ytb*dy+ytc*dz
c               if no real z-diff, go do ave tanpl      11-22-82
c     if(abs(zt1-zt2).lt..002) goto 372             ijd 14-sep-88
      if(zt1-zt2.lt.toler*2.) goto 372
 
c               guard against zero divides                8-may-87
      tan1=0.
      tan2=0.
      if (yt2.ne.0.) tan2=(zt2-h2)/yt2
      if (yt1.ne.0.) tan1=(zt1-h1)/yt1
      den=tan1-tan2
      if (abs(den).gt..001) goto 42
c          no real curv solution.  restore h, treat cs as average plane
c
372   h=t(27,ia)
      do 38 i=1,7
38    hs(i)=(hs(i)+s(i,3))/2.
      sec=sqrt(hs(1)*hs(1)+hs(2)*hs(2)+hs(3)*hs(3))
      if (sec.lt..001) goto 31
      s(1,3)=hs(1)/sec
      s(2,3)=hs(2)/sec
      s(3,3)=hs(3)/sec
      s(4,3)=s(1,3)*hs(5)+s(2,3)*hs(6)+s(3,3)*hs(7)
C          IF BARREL TOOL, CALC NEW H-VAL FOR NEXT TIME.
C          (SHOULD PROBABLY ALSO RE-CALC FOR STD TOOL BUT SEEMS TOO RISKY 
C           AT THIS LATE DATE)                                1-MAR-88
      IF (IFL(282).EQ.0.OR.IFL(58).EQ.0) GOTO 99
      SI=-(S(1,3)*T(4,IA)+S(2,3)*T(5,IA)+S(3,3)*T(6,IA))
      IF (ABS(SI).GT..9999) GOTO 99
      CO=SQRT(1.-SI*SI)
      TIN=SI/CO
      IF (SI.LT.SBE) GOTO 39
C           RCORN  H-CALC CASE
      H=TOOL(2)+TIN*TOOL(6)
      GOTO 99
39    IF(SI.LT.SFI)GOTO 40
C           RSIDE HCALC CASE
      H=ASC(68)+TIN*ASC(67)
      GOTO 99
C           TOP H-CALC CASE
40    H=TOOL(3)+TIN*hrad
      goto 99
c          calc effective r-ctr
42    yc=(h2-h1)/den
      zc=h1+tan1*yc
      r=sqrt((yt1-yc)**2+(zt1-zc)**2)
c
c             if this is a 'past' ending, flip yt1,yc
      if (sc(36).ne.1.) goto 43
      yt1=-yt1
      yc=-yc
c             convex or concave
43    if (yc.lt.yt1) goto 50
c          convex case.  det rcorn,side,top contact
44    zcn=sbe*(yc-tool(6))+cbe*(zc-tool(2))
c          check for disk cutter + edge contact
      if (asc(66).eq.1..and.zcn.gt.0.) goto 49
      if (zcn.gt.0.) goto 45
c             rcorn contact case
      si=(tool(2)-zc)/(tool(2)+sc(25)+r)
      if (si .gt. p99) then
         tn = t99
      else if (si .lt. -p99) then
         tn = -t99
      else
         co=sqrt(1.-si*si)
         tn=si/co
      endif
      h=tool(2)+tn*tool(6)
      goto 10
c             side or top case
45    IF (IFL(282).EQ.0) GOTO 46
C             ZCN TOP BARREL
      ZCN=SFI*(YC-hrad)+CFI*(ZC-TOOL(3))
      GOTO 47
C             ZCN TOP NON-BARREL
46    ZCN=SBE*(YC-hrad)+CBE*(ZC-TOOL(3))
47    IF (ZCN.GT.0.) GOTO 48
      IF (IFL(282).EQ.1) GOTO 472
C              SIDE CASE  NON-BARREL
      H=ZC+YC*SBE/CBE
      GOTO 10
C              CHOOSE BET R2 AND SIDEFLAT OF BARREL TOOL
472   ZCN=SFI*(YC-ASC(67))+CFI*(ZC-ASC(68))
      IF (ZCN.LT.0.) GOTO 474
C               SIDFLAT CASE BARREL
      H=ZC+YC*SFI/CFI
      GOTO 10
C               R2 CASE  BARREL
474   DEN=YC-ASC(67)
      TN=0.
      IF(DEN.NE.0.)TN=(ZC-ASC(68))/DEN
      H=ZC-TN*YC
      goto 10
c              top case
48    si=(zc-tool(3))/(r+sc(25))
      if (si .gt. p99) then
         tn = t99
      else if (si .lt. -p99) then
         tn = -t99
      else
         co=sqrt(1.-si**2)
         tn=si/co
      endif
      h=tool(3)-tn*tool(1)*.5
      goto 10
c              disk cutter, edge contact
49    tnd=0.
      den=yc-hrad
      if (den.ne.0.) tnd=(zc-tool(3))/den
      h=tool(3)-hrad*tnd
      goto 10
c             concave cs.  decide if corn or top
50    r=r-sc(25)
      hyp=r-tool(2)
      dz=zc-tool(2)
c            if barrel tool, goto 70 for tool-to-cs apply   4-feb-88
      if (ifl(282).eq.1) goto 70
      zcn=sbe*(yc-tool(6))+cbe*(zc-tool(2))
      if (zcn.lt.0.) goto 60
      dysq=hyp*hyp-dz*dz
      if (DEBUGX .eq. 3) then
          write (dbuf,7000) r,hyp,zc
 7000     format ('r = ',f11.4,'   hyp = ',f11.4,'   zc = ',f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7001) dz,dysq
 7001     format ('dz = ',f11.4,'   dysq = ',f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
      if (dysq.le.0.) goto 31
      dy=sqrt(dysq)
      yc=tool(6)-dy
      tgap=r-sqrt((tool(3)-zc)*(tool(3)-zc)+(hrad-yc)*(hrad-yc))
c             if tgap neg, go top route
c                rcorn contact
      if (tgap.lt.0.) goto 60
      tn=dz/dy
      h=tool(2)+tn*tool(6)
      goto 10
c             top contact
60    si=(tool(3)-zc)/r
      if (si .gt. p99) then
         tn = t99
      else if (si .lt. -p99) then
         tn = -t99
      else
         co=sqrt(1.-si*si)
         tn=si/co
      endif

      h=tool(3)-tn*hrad
      goto 10
c
C              CONCAVE CS AND BARREL CUTTER
C               CALC YC FOR RCORN. IF NOT REAL, SET YCR = BIGNEG.
70    YCR=BIGNEG
      DYSQ=HYP*HYP-DZ*DZ
C         ABANDON THIS CASE IF  R.LT.RCORN  OR   DY NEG
      IF(TOOL(2).GT.R.OR.DYSQ.LE.0.)GOTO 72
      DY=SQRT(DYSQ)
      YCR=TOOL(6)-DY
      ZCN=SBE*(YCR-TOOL(6))+CBE*(ZC-TOOL(2))
      IF(ZCN.LT.0.)YCR=BIGNEG
C                YCS FOR SIDE CONTACT
72    YCS=BIGNEG
      HYP=R-ASC(307)
      DZ=ZC-ASC(68)
      DYSQ=HYP*HYP-DZ*DZ
      IF(HYP.LE.0..OR.DYSQ.LE.0.)GOTO 74
      DY=SQRT(DYSQ)
      YCS=ASC(67)-DY
      ZCN=SFI*(YCS-ASC(67))+CFI*(ZC-ASC(68))
      ZCNB=SBE*(YCR-TOOL(6))+CBE*(ZC-TOOL(2))
      IF(ZCN.LT.0..OR.ZCNB.GT.0.)YCS=BIGNEG
C                YCT FOR TOP CONTACT
74    YCT=BIGNEG
      DZ=ZC-TOOL(3)
      DYSQ=R*R-DZ*DZ
      IF(DYSQ.LT.0.)GOTO 76
      DY=SQRT(DYSQ)
      YCT=hrad-DY
76    CONTINUE
C                MOST POSITIVE YC SETS CONTACT MODE
      IF(YCR-YCS)78,80,82
78    IF(YCT.GE.YCS)GOTO 84
C                 R2 SIDE CASE
      TN=0.
      DY=ASC(67)-YCS
      IF(DY.NE.0.)TN=(ZC-ASC(68))/DY
      H=ASC(68)+TN*ASC(67)
      GOTO 88
C                  YCR SAME AS YCS.  PROBABLY BOTH UNREAL.
C                    GOTO TOP CASE IF YCT REAL, OTHERWISE RCORN
80    IF(YCT.GT.BIGNEG)GOTO 84
      GOTO 83
82    IF(YCT.GT.YCR)GOTO 84
C                  RCORN CASE.
83    TN=0.
      DY=TOOL(6)-YCR
      IF(DY.NE.0.)TN=(ZC-TOOL(2))/DY
      H=TOOL(2)+TN*TOOL(6)
      GOTO 88
C                  TOP CASE
84    DY=hrad-YCT
      TN=0.
      IF(DY.NE.0.) TN=(TOOL(3)-ZC)/DY
      H=TOOL(3)-TN*hrad
88    CONTINUE
      GOTO 10
c          sto last used params and exit
99    t(27,ia)=h
      t(25,ia)=u
      t(26,ia)=v
      if (ifl(83).gt.0 .or. iptk.lt.0) goto 999
c          when near cs, put u,v & h in col(b) for next time
      t(25,ib)=u
      t(26,ib)=v
      t(27,ib)=h
999   return
      end

C***********************************************************************
C                                                                      *
C   PURPOSE OF PROGRAM: project a tool axis point on CS surface.
C                                                                      *
C***********************************************************************
 
      subroutine cssrf0 (tol)
 
      include 'com4a.com'
      include 'mocom.com'

      real*4 tol

      integer*2 jd(600)
      real*4 ad(300),hs(7),asc(310)
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      real*4 p99,t99,co12,co,si,hrad,hgt

c          get entry h-val and misc params
      p99=.9999
      t99 = 70.705375
      BIGNEG=-1.E9
      SFI=ASC(305)
      CFI=.001
      IF (ABS(SFI).LT.1.) CFI=SQRT(1.-SFI*SFI)

      u = t(25,ia)
      v = t(26,ia)
      h = t(27,ia)
C                 SET H DELTA VALUE FOR 2 SIGHTINGS OF CKSRF     26-FEB-88
C                  IF UNITS/MM INCREASE*25.4
      HDEL=.05
      if (ifl(264).eq.1 .or. ifl(362).eq.1) HDEL=1.27
      IF (IFL(83).NE.0.AND.IFL(23).NE.4) H=H+HDEL
      x=t(1,ia)
      y=t(2,ia)
      z=t(3,ia)
      a=t(4,ia)
      b=t(5,ia)
      c=t(6,ia)
      itim=0
      hrad = tool(1)/2.
      hgt = tool(3)

10    continue

      s(8,3)=x+h*a
      s(9,3)=y+h*b
      s(10,3)=z+h*c
      call surfpn(u,v,1)
      if (ifl(2) .gt. 0) goto 999
c              if not near cs, exit now.  (also exit on itim =3)
      if (ifl(83).eq.0.or.ifl(23).eq.4) goto 99
      itim=itim+1
      if( itim-2) 20,30,99
c             time 1.  sto pv1 data, go do pv2
20    do 22 i=1,7
22    hs(i)=s(i,3)
      h1=h
      h=h-2.*hdel
      goto 10
c             time 2.  ave pv1,pv2 for approx yt
30    h2=h
c      if pl1 parlel pl2, go do average pl and exit    11-22-82
      co12 = abs (hs(1)*s(1,3)+hs(2)*s(2,3)+hs(3)*s(3,3))
      if (co12 .gt. .999999) goto 372
 
      yta=hs(1)+s(1,3)
      ytb=hs(2)+s(2,3)
      ytc=hs(3)+s(3,3)
c             rgt sense
      rx=ytb*c-ytc*b
      ry=ytc*a-yta*c
      rz=yta*b-ytb*a
c             actual yt axis   (perpto rgt and ta)
      yta=rz*b-ry*c
      ytb=rx*c-rz*a
      ytc=ry*a-rx*b
      sec=sqrt(yta*yta+ytb*ytb+ytc*ytc)
      if (sec.gt.0.) goto 32
cccccccccccccccccccccccc  err=51 temp
c          error.  sec=zero or some degen case
31    ifl(2)=51
      goto 99
cccccccccccccccccccccccc
32    yta=yta/sec
      ytb=ytb/sec
      ytc=ytc/sec
c             sfpt 1 in t-sys
      dx=hs(5)-x
      dy=hs(6)-y
      dz=hs(7)-z
      zt1=a*dx+b*dy+c*dz
      yt1=yta*dx+ytb*dy+ytc*dz
c             sfpt 2  in t-sys
      dx=s(5,3)-x
      dy=s(6,3)-y
      dz=s(7,3)-z
      zt2=a*dx+b*dy+c*dz
      yt2=yta*dx+ytb*dy+ytc*dz
c               if no real z-diff, go do ave tanpl      11-22-82
c     if(abs(zt1-zt2).lt..002) goto 372             ijd 14-sep-88
      if(zt1-zt2.lt.2*tol) goto 372
 
c               guard against zero divides                8-may-87
      tan1=0.
      tan2=0.
      if (yt2.ne.0.) tan2=(zt2-h2)/yt2
      if (yt1.ne.0.) tan1=(zt1-h1)/yt1
      den=tan1-tan2
      if (abs(den).gt..001) goto 42
c          no real curv solution.  restore h, treat cs as average plane
c
372   h=t(27,ia)
      do 38 i=1,7
38    hs(i)=(hs(i)+s(i,3))/2.
      sec=sqrt(hs(1)*hs(1)+hs(2)*hs(2)+hs(3)*hs(3))
      if (sec.lt..001) goto 31
      s(1,3)=hs(1)/sec
      s(2,3)=hs(2)/sec
      s(3,3)=hs(3)/sec
      s(4,3)=s(1,3)*hs(5)+s(2,3)*hs(6)+s(3,3)*hs(7)
C          IF BARREL TOOL, CALC NEW H-VAL FOR NEXT TIME.
C          (SHOULD PROBABLY ALSO RE-CALC FOR STD TOOL BUT SEEMS TOO RISKY 
C           AT THIS LATE DATE)                                1-MAR-88
      IF (IFL(282).EQ.0.OR.IFL(58).EQ.0) GOTO 99
      SI=-(S(1,3)*T(4,IA)+S(2,3)*T(5,IA)+S(3,3)*T(6,IA))
      IF (ABS(SI).GT..9999) GOTO 99
      CO=SQRT(1.-SI*SI)
      TIN=SI/CO
      IF (SI.LT.SBE) GOTO 39
C           RCORN  H-CALC CASE
      H=TOOL(2)+TIN*TOOL(6)
      GOTO 99
39    IF(SI.LT.SFI)GOTO 40
C           RSIDE HCALC CASE
      H=ASC(68)+TIN*ASC(67)
      GOTO 99
C           TOP H-CALC CASE
40    H=TOOL(3)+TIN*hrad
      goto 99
c          calc effective r-ctr
42    yc=(h2-h1)/den
      zc=h1+tan1*yc
      r=sqrt((yt1-yc)**2+(zt1-zc)**2)
c
c             if this is a 'past' ending, flip yt1,yc
      if (sc(36).ne.1.) goto 43
      yt1=-yt1
      yc=-yc
c             convex or concave
43    if (yc.lt.yt1) goto 50
c          convex case.  det rcorn,side,top contact
44    zcn=sbe*(yc-tool(6))+cbe*(zc-tool(2))
c          check for disk cutter + edge contact
      if (asc(66).eq.1..and.zcn.gt.0.) goto 49
      if (zcn.gt.0.) goto 45
c             rcorn contact case
      si=(tool(2)-zc)/(tool(2)+sc(25)+r)
      if (si .gt. p99) then
         tn = t99
      else if (si .lt. -p99) then
         tn = -t99
      else
         co=sqrt(1.-si*si)
         tn=si/co
      endif
      h=tool(2)+tn*tool(6)
      goto 10
c             side or top case
45    IF (IFL(282).EQ.0) GOTO 46
C             ZCN TOP BARREL
      ZCN=SFI*(YC-hrad)+CFI*(ZC-TOOL(3))
      GOTO 47
C             ZCN TOP NON-BARREL
46    ZCN=SBE*(YC-hrad)+CBE*(ZC-TOOL(3))
47    IF (ZCN.GT.0.) GOTO 48
      IF (IFL(282).EQ.1) GOTO 472
C              SIDE CASE  NON-BARREL
      H=ZC+YC*SBE/CBE
      GOTO 10
C              CHOOSE BET R2 AND SIDEFLAT OF BARREL TOOL
472   ZCN=SFI*(YC-ASC(67))+CFI*(ZC-ASC(68))
      IF (ZCN.LT.0.) GOTO 474
C               SIDFLAT CASE BARREL
      H=ZC+YC*SFI/CFI
      GOTO 10
C               R2 CASE  BARREL
474   DEN=YC-ASC(67)
      TN=0.
      IF(DEN.NE.0.)TN=(ZC-ASC(68))/DEN
      H=ZC-TN*YC
      goto 10
c              top case
48    si=(zc-tool(3))/(r+sc(25))
      if (si .gt. p99) then
         tn = t99
      else if (si .lt. -p99) then
         tn = -t99
      else
         co=sqrt(1.-si**2)
         tn=si/co
      endif
      h=tool(3)-tn*tool(1)*.5
      goto 10
c              disk cutter, edge contact
49    tnd=0.
      den=yc-hrad
      if (den.ne.0.) tnd=(zc-tool(3))/den
      h=tool(3)-hrad*tnd
      goto 10
c             concave cs.  decide if corn or top
50    r=r-sc(25)
      hyp=r-tool(2)
      dz=zc-tool(2)
c            if barrel tool, goto 70 for tool-to-cs apply   4-feb-88
      if (ifl(282).eq.1) goto 70
      zcn=sbe*(yc-tool(6))+cbe*(zc-tool(2))
      if (zcn.lt.0.) goto 60
      dysq=hyp*hyp-dz*dz
      if (dysq.le.0.) goto 372
      dy=sqrt(dysq)
      yc=tool(6)-dy
      tgap=r-sqrt((tool(3)-zc)*(tool(3)-zc)+(hrad-yc)*(hrad-yc))
c             if tgap neg, go top route
c                rcorn contact
      if (tgap.lt.0.) goto 60
      tn=dz/dy
      h=tool(2)+tn*tool(6)
      goto 10
c             top contact
60    si=(tool(3)-zc)/r
      if (si .gt. p99) then
         tn = t99
      else if (si .lt. -p99) then
         tn = -t99
      else
         co=sqrt(1.-si*si)
         tn=si/co
      endif

      h=tool(3)-tn*hrad
      goto 10
c
C              CONCAVE CS AND BARREL CUTTER
C               CALC YC FOR RCORN. IF NOT REAL, SET YCR = BIGNEG.
70    YCR=BIGNEG
      DYSQ=HYP*HYP-DZ*DZ
C         ABANDON THIS CASE IF  R.LT.RCORN  OR   DY NEG
      IF(TOOL(2).GT.R.OR.DYSQ.LE.0.)GOTO 72
      DY=SQRT(DYSQ)
      YCR=TOOL(6)-DY
      ZCN=SBE*(YCR-TOOL(6))+CBE*(ZC-TOOL(2))
      IF(ZCN.LT.0.)YCR=BIGNEG
C                YCS FOR SIDE CONTACT
72    YCS=BIGNEG
      HYP=R-ASC(307)
      DZ=ZC-ASC(68)
      DYSQ=HYP*HYP-DZ*DZ
      IF(HYP.LE.0..OR.DYSQ.LE.0.)GOTO 74
      DY=SQRT(DYSQ)
      YCS=ASC(67)-DY
      ZCN=SFI*(YCS-ASC(67))+CFI*(ZC-ASC(68))
      ZCNB=SBE*(YCR-TOOL(6))+CBE*(ZC-TOOL(2))
      IF(ZCN.LT.0..OR.ZCNB.GT.0.)YCS=BIGNEG
C                YCT FOR TOP CONTACT
74    YCT=BIGNEG
      DZ=ZC-TOOL(3)
      DYSQ=R*R-DZ*DZ
      IF(DYSQ.LT.0.)GOTO 76
      DY=SQRT(DYSQ)
      YCT=hrad-DY
76    CONTINUE
C                MOST POSITIVE YC SETS CONTACT MODE
      IF(YCR-YCS)78,80,82
78    IF(YCT.GE.YCS)GOTO 84
C                 R2 SIDE CASE
      TN=0.
      DY=ASC(67)-YCS
      IF(DY.NE.0.)TN=(ZC-ASC(68))/DY
      H=ASC(68)+TN*ASC(67)
      GOTO 88
C                  YCR SAME AS YCS.  PROBABLY BOTH UNREAL.
C                    GOTO TOP CASE IF YCT REAL, OTHERWISE RCORN
80    IF(YCT.GT.BIGNEG)GOTO 84
      GOTO 83
82    IF(YCT.GT.YCR)GOTO 84
C                  RCORN CASE.
83    TN=0.
      DY=TOOL(6)-YCR
      IF(DY.NE.0.)TN=(ZC-TOOL(2))/DY
      H=TOOL(2)+TN*TOOL(6)
      GOTO 88
C                  TOP CASE
84    DY=hrad-YCT
      TN=0.
      IF(DY.NE.0.) TN=(TOOL(3)-ZC)/DY
      H=TOOL(3)-TN*hrad
88    CONTINUE
      GOTO 10
c          sto last used params and exit
99    continue
      if (h .lt. 0) h = 0
      if (h .gt. hgt) h = hgt

      s(8,3)=x+h*a
      s(9,3)=y+h*b
      s(10,3)=z+h*c

      t(27,ia)=h
      t(25,ia)=u
      t(26,ia)=v

999   return
      end

C***********************************************************************
C   PROGRAM NAME: csrfwd (tol)
C
C   PURPOSE OF PROGRAM: project a tool ring forward point on CS surface.
C
C***********************************************************************
 
      subroutine csrfwd
 
      include 'com4a.com'
      include 'mocom.com'

      integer*2 jd(600)
      real*4 ad(300)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)

      real*4 cute,u,v,h
      real*4 xv(3),fv(3)

      cute = tool(6)

      u = t(25,ia)
      v = t(26,ia)
      h = t(27,ia)
c
c..... forward look point on the tool ring
c
         call uvcplvc4(t(1,ia),t(4,ia),xv,h)
         call triple_cross4(t(4,ia),t(7,ia),t(4,ia),fv)
         call unitvc4(fv,fv)
         call uvcplvc4(xv,fv,s(8,3),cute)
         call surfpn(u,v,1)
         if (ifl(2) .gt. 0) goto 999

         t(25,ia) = u
         t(26,ia) = v

999   return
      end

C***********************************************************************
C   PROGRAM NAME: cssrf1 (itsk,ktyp,dis,tol)
C
C   PURPOSE OF PROGRAM: set projection parameters - to use in cselect
C
C***********************************************************************
 
      subroutine cssrf1 (itsk,ktyp,dis,tol)
 
      include 'com4a.com'
      include 'mocom.com'
      include 'csrel.com'

      integer*2 itsk,ktyp
      real*4 dis,tol

      integer*2 jd(600)
      real*4 ad(300),xv(3),fv(3),vpos(3),spt(3)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      real*4 x1,y1,cute,crad,hrad,hgt,dot1
      real*4 f_dot4,f_mag4

      hrad=tool(1)/2.
      crad = tool(2)
      cute = tool(6)
      hgt = tool(3)

      u=t(25,ia)
      v=t(26,ia)
      h = t(27,ia)
c
c..... spt is the look point projection onto the surface plane
c
         call point_on_plane4(s(8,3),s(5,3),s(1,3),spt)
         eps = 5.*tol
c
c..... if spt is inside the cutter (or almost), exit with this surface plane.
c
         call vcmnvc4(spt,t(1,ia),fv)
c
c..... if spt is behind, the type is IGNOR
c
         dot1 = f_dot4(t(7,ia),fv)
         if (dot1 .lt. tol) goto 999

         y1 = f_dot4(t(4,ia),fv)
         x1 = sqrt (f_dot4(fv,fv) - y1*y1)
c
c..... x1,y1 are spt coordinates in the plane through the tool axis and spt.
c
         if (y1 .lt. -tol .and. x1 .lt. cute - tol) then
           ktyp = BOTTM
           dis = -y1
           goto 999
         endif

         if (itsk .eq. 0) then
           call vcmnvc4(spt,s(8,3),fv)
           call unitvc4(fv,fv)
           dot1 = f_dot4(s(1,3),fv)
           if (abs(dot1) .gt. 0.9962) then
             ktyp = AHEAD
           else
             ktyp = EXTEN
           endif
         else
           if (y1 .gt. crad + eps .or. y1 .lt. -eps) then
             ktyp = EXTEN
           else
             ktyp = AHEAD
           endif
         endif

         if (y1 .ge. crad - tol) then
           dis = x1 - hrad
         else if (x1 .gt. cute - tol) then
           call uvcplvc4(fv,t(4,ia),vpos,-y1)
           call unitvc4(vpos,vpos)
           call avcplbvc4 (crad,t(4,ia),cute,vpos,fv)
           call vcplvc4(t(1,ia),fv,xv)
           if (itsk .eq. 1) then
             call vctovc4(xv,s(8,3))
           endif
c
c..... xv is the tool ring point closest to spt
c
           call vcmnvc4(spt,xv,fv)
           dis = f_mag4(fv) - crad
         else
           dis = -y1
         endif

999   return
      end

C***********************************************************************
C   PROGRAM NAME: cssrf2 (dis)
C
C   PURPOSE OF PROGRAM:  estimate distance from the current CS to
C                        the cutter; to use in csavoid
C
C***********************************************************************
 
      subroutine cssrf2 (dis)
 
      include 'com4a.com'
      include 'mocom.com'
      include 'csrel.com'

      real*8 dis

      real*4 asc(100),sbe,cbe,cutn
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
      integer*2 ia
      equivalence (ifl(51),ia)


      real*4 xv(3),fv(3),vpos(3),spt(3)

      real*4 f_dot4,f_mag4
      real*4 hsidtn,x1,y1,xsec,ysec,crad,cute,tol,del

      tol = sc(27)

      crad = tool(2)
      cute = tool(6)

c
c..... spt is the look point projection onto the surface plane
c
      call point_on_plane4(s(8,3),s(5,3),s(1,3),spt)
c
c... If spt is inside the cutter (or almost), exit with this surface plane.
c
      call vcmnvc4(spt,t(1,ia),fv)
      y1 = f_dot4(t(4,ia),fv)
      x1 = sqrt (f_dot4(fv,fv) - y1*y1)
      hsidtn = crad*(1.-sbe)

      xsec = cutn*cbe + x1*sbe*sbe + y1*cbe*sbe
      ysec = -cutn*sbe + x1*cbe*sbe + y1*cbe*cbe
c
c..... x1,y1 are spt coordinates in the plane through the tool axis and spt.
c..... xsec,ysec is the intersection of the line from spt perpendicular to the
c..... cutter side with the line through the cutter side
c
      if (ysec .ge. hsidtn - tol) then
        fv(1) = x1-xsec
        fv(2) = y1-ysec
        fv(3) = 0
        del = f_mag4 (fv)
        if (x1 .lt. xsec) del = -del
      else if (y1 + tol .gt. 0) then
        call uvcplvc4(fv,t(4,ia),vpos,-y1)
        call unitvc4(vpos,vpos)
        call avcplbvc4 (crad,t(4,ia),cute,vpos,fv)
        call vcplvc4(t(1,ia),fv,xv)
c
c..... xv is the tool ring point closest to spt
c
        call vcmnvc4(spt,xv,fv)
        dot1 = f_dot4 (fv,vpos)
        del = f_mag4 (fv)
        if (dot1 .lt. 0) del = -del
        del = del - crad
      endif

      dis = del - sc(25)

      return
      end

C **********************************************************************
C **  PROGRAM NAME: csrfpl
C **
C **  PURPOSE OF PROGRAM: project a tool point on CS surface.
C **
C **********************************************************************
C **********************************************************************
 
      subroutine csrfpl(itsk,ktyp,hpl,par,dis,tol)

      include 'com4a.com'
      include 'mocom.com'
      include 'csrel.com'

      real*4 tol,dis
      real*4 hpl(10),par(3)
      integer*2 itsk,ktyp

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

      ktyp = IGNOR
      if (itsk .eq. 0) then
        call cssrf0 (tol)
      else
        call csrfwd
      endif
      if (ifl(2) .le. 0) then
        call cssrf1 (itsk,ktyp,dis,tol)
        call conv4_4 (t(25,ia),par,3)
        call conv4_4 (s(1,3),hpl,10)
      endif

      return
      end
