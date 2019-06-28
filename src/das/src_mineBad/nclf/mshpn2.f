c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       mshpn2.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:19
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **
C **  SUBROUTINE NAME: MSHPN2
C **
C **  PURPOSE OF PROGRAM:
C **       THIS ROUTINE IS CALLED BY SMPRE & SSRFPN TO HANDLE LOADING 
C **       OF PATCHES OF MESH SURFACE IN A SUPER SURFACE AND TO CALL
C **       IPTCH2 TO CALCULATE POINT & VECTOR TO SURFACE.
C **
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE MSHPN2 (U,V,IRET)
 
      INCLUDE 'com4a.com'
      include 'mocom.com'
 
c          motion common equivalences
      real*4 SRF(10,3)
      equivalence (srf,s)
 
      REAL*8 E(14)
      REAL*4 AD(300),AE(28)
      INTEGER*2 KD(600)
      EQUIVALENCE (D,AD,KD),(E,AE),(IFL(54),ISRF)
      INTEGER*4 NCLKEY
 
C          SET PTRS TO SRF IN D-TBL
      IDX=50*(ISRF-1)
      JDX=2*IDX
      KDX=4*IDX
C          SAFETY COUNTER FOR IPATCH CALLS
      IPATK=0
      NPATS=KD(KDX+2)
      NMAX=KD(KDX+37)
      MMAX=KD(KDX+38)
      IPAT=KD(KDX+39)
      CALL GTDESC(SC(143+ISRF),NCLKEY,M,N)
      IF (IPAT.EQ.0) GOTO 5

      M=KD(KDX+41)
      N=KD(KDX+42)
C      IPG=KD(KDX+43)
C      IEL=KD(KDX+44)
      GOTO 10
C                        NO PATCH IN D-TABLE - SET UP TO LOAD MIDDLE PATCH
5     CONTINUE
      M=MMAX/2+1
      N=NMAX/2+1
      KD(KDX+41)=M
      KD(KDX+42)=N
      IPAT=(N-1)*MMAX+M
      GOTO 80

10    IPATK=IPATK+1
      IF (IPATK.LE.NPATS) GOTO 20
      IFL(2)=138
      GOTO 999
20    CALL IPTCH2(U,V,IRET)
      GOTO (25,50,999,60,30,999) IRET+3
25    IF (N.EQ.1) GOTO 999
      N=N-1
      KD(KDX+42)=N
      IPAT=IPAT-MMAX
      GOTO 80
30    IF (N.EQ.NMAX) GOTO 999
      N=N+1
      KD(KDX+42)=N
      IPAT=IPAT+MMAX
      GOTO 80
50    IF (M.EQ.1) GOTO 999
      M=M-1
      KD(KDX+41)=M
      IPAT=IPAT-1
      GOTO 80
60    IF (M.EQ.MMAX) GOTO 999
      M=M+1
      KD(KDX+41)=M
      IPAT=IPAT+1

C80    ITMP=(IPAT-1)*26
C      IPG=KD(KDX+5)+ITMP/35
C      IEL=KD(KDX+6)+ITMP-(ITMP/35*35)+1
C90    IF (IEL.LT.36) GOTO 100
C      IEL=IEL-35
C      IPG=IPG+1
C      GOTO 90
C100   CALL GETENT(D(IDX+25),26,IPG,IEL,0)

80    CONTINUE
      CALL GTMPTT(NCLKEY,IPAT,D(IDX+25))
      D(IDX+23)=D(IDX+25)
      D(IDX+24)=D(IDX+26)
      D(IDX+25)=D(IDX+27)
      AD(JDX+52)=0.
      AD(JDX+53)=0.
      AD(JDX+54)=0.
      XMM=MMAX
      XM=M
      XNM=NMAX
      XN=N
      AD(JDX+15)=(XM-1.)/XMM
      AD(JDX+16)=XM/XMM
      AD(JDX+17)=(XN-1.)/XNM
      AD(JDX+18)=XN/XNM
      KD(KDX+39)=IPAT
C      KD(KDX+43)=IPG
C      KD(KDX+44)=IEL
      U=.5
      V=.5
      GOTO 10
999   RETURN
      END
