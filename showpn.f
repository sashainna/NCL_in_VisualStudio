c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       showpn.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:43
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  SUBROUTINE NAME: SHOWPN                                         **
C **                                                                  **
C **  PURPOSE OF SUBROUTINE: THIS ROUTINE DISPLAYS THE CANONICAL      **
C **    FORM OF A PATERN IN WINDOW 3                                  **
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE SHOWPN (L1)
 
      include 'com8a.com'
 
      INTEGER*2 PG,EL,NW,PEQ(4),ISTOP
      REAL*8 PGELNW,RBUFF(6)
      EQUIVALENCE (PEQ(1),PGELNW)
      EQUIVALENCE (PG,PEQ(1)),(EL,PEQ(2)),(NW,PEQ(3))
      INTEGER*2 L1,j,ine
      integer*4 ipntyp,strlen1
      LOGICAL TRANS

      integer*4 knc, kmxc, kcol, klin

      TRANS = .TRUE. 
C      PGELNW=TV
      CALL GTPNNP(TV,NW,ipntyp)
      WRITE (COUT,1010) NW
1010  FORMAT (16X,'NUMBER OF POINTS: ',I5)
1015  FORMAT (16X,'X',11x,'Y',11x,'Z')
1020  FORMAT ('PT  ',I4,3F12.4)
1025  FORMAT ('PV  ',I4,3F12.4,3f11.4)
      CALL PUTMSG(COUT,80,17,0)
      write (COUT,1015)
      if (ipntyp .eq. 2) cout = cout(1:41) //
     -    '         I          J          K'
      CALL PUTMSG(COUT,78,18,0)
      II  = 0
      ine = 5
      if (ifl(35) .eq. 2) then
             ISTOP=(NW+5)/5 + 1
      else
             ISTOP=(NW+9)/9 + 1
      endif
      DO 500 J=1,ISTOP
         L1=19
         DO 100 I=II+1,II+ine 
c     changed the routine name from gtpnpt to gpnptt on: 3/4/88 by: kathy 
            CALL gpnptt(RBUFF,TV,I,TRANS)
            if (ipntyp .eq. 1) then
                WRITE (COUT,1020)I,RBUFF(1),RBUFF(2),RBUFF(3)
            else
                WRITE (COUT,1025)I,RBUFF
            end if
            CALL PUTMSG(COUT,78,L1,0)
            L1=L1+1
            IF (I.GE.NW) GO TO 99999
100      CONTINUE
c
c...Added check for NCL-VT mode
c...Paul  -  10/15/91
c...Old version was:
c   IF (IFL(35).EQ.0) THEN
c
c...VX
c
         IF (IFL(35).EQ.0 .or. (ifl(35) .eq. 2 .and. ifl(322) .ne. 1))
     1           THEN
c
             WRITE (COUT,1030)
1030         FORMAT ('HIT ENTER TO CONTINUE DISPLAY OF POINT VALUES,',
     1               ' OR Q TO QUIT SHOWING POINT DATA')
c
c...Added check for NCL-VT mode
c...Paul  -  10/15/91
c...Old version was:
c   call nclpmt (cout, cin)
c
          if (ifl(35) .eq. 2) then
             call putmsg(cout,78,3,2)
             nccin  = 0
             kmxc = 1
             kcol = 9
             klin = 1
             call gvtlin(cin,nccin,kmxc,klin,kcol)
          else    
             nccout = strlen1(cout)
             call nclpmt (cout, nccout, cin, nccin)
          endif 
c
             IF (AIN(1).EQ.'Q'.OR.AIN(1).EQ.'q') GO TO 99999
         ELSE
             COUT=' '
             CALL PUTMSG (COUT,80,2,0)
         ENDIF
         II=II+ine
         if (ifl(35) .eq. 2) then
            ine = 6
         else
            ine = 9
         endif
500   CONTINUE
99999 COUT=' '
c
c...Added check for NCL-VT mode
c...Paul  -  10/17/91
c...Old version was:
c   CALL PUTMSG (COUT,80,2,0)
c   (in the VT mode with this CALL after "*s/pn1" comm all screen is cleaned)
c
          if (ifl(35) .ne. 2) CALL PUTMSG (COUT,80,2,0)
      RETURN
      END
