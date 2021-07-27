c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       gotopn.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:09
c**
c*****************************************************
C*
C* COPYRIGHT (C) 1987 MILLS DATA SYSTEMS
C*
C **********************************************************************
C **********************************************************************
C     SUBROUTINE NAME: GOTOPN
C
C     PURPOSE OF SUBROUTINE:
C            THIS SUBROUTINE PARSES THE GOTO/PATERN STATEMENT.  THE
C         RBUFF ARRAY IS A REAL*8 ARRAY, WITH EACH REAL*8 REPRESENTING
C         A MASK FOR IT'S CORRESPONDING POINT IN THE PATTERN. THE
C         LAYOUT OF THE REAL*8 MASK FOR EACH POINT IS:
C
C            FIRST I*2   0 = NOT OMITTED, 1 = OMITTED
C            SECOND I*2  0 = NOT AVOIDED, 1 = AVOIDED
C            LAST R*4    IF THIS POINT HAS BEEN AVOIDED, THIS IS
C                        THE AVOID HEIGHT.
C
C         ONCE THE RBUFF ARRAY HAS BEEN ASSEMBLED, IT IS PASSED OFF
C         TO MOCNTL FOR THE SUBSEQUENT CALLS TO MOTIMM TO OUTPUT
C         THE MOTION.
C
C***********************************************************************
      SUBROUTINE GOTOPN
 
      INCLUDE 'com8a.com'

      REAL*8 RBUFF(36),TWD
      REAL*4 R4TWD(2),TAVOID
      INTEGER*2 KTV(4),I2TWD(2),IX,IBEGIN,IEND,UPDOWN
      INTEGER*2 RIX,NPTS
      integer*4 ntyp
      EQUIVALENCE (TWD,R4TWD,I2TWD)
      EQUIVALENCE (TV,KTV)
      LOGICAL OMIT,RETAIN,CONST,THRU,AVOID,FIRST

      OMIT = .FALSE.
      RETAIN = .FALSE.
      CONST = .FALSE.
      THRU = .FALSE.
      AVOID = .FALSE.
      FIRST = .TRUE.
      TAVOID = 0
      
      SC(11)=TV
      CALL GTPNNP(SC(11),NPTS,ntyp)
      ISC10(3)=NPTS
      ISC10(2)=3
      ISC10(4)=0
      RIX = 0
      TWD = 0
C          INITIALIZE RBUFF TO ALL ZERO
      DO 5 I=1,NPTS
5         CALL PTRANW(RBUFF,TWD,I,RIX)
      IF (NEXTYP.EQ.11) GO TO 88888
      CALL PARSIT
      DO 10 I=1,2
          IF (ITYP.EQ.1.AND.IST.EQ.822) THEN
C                                             **** INVERS
              ISC10(4)=1 
          ELSE IF (ITYP.EQ.1.AND.IST.EQ.328.) THEN
C                                             **** CONST
              CONST = .TRUE.
          ELSE
              GO TO 15
          ENDIF
          CALL PARSIT
10    CONTINUE
15    CONTINUE
      IF (ITYP.EQ.7) GO TO 88888
      DO 200 I=1,1000
          AVOID = .FALSE.    
          IF (ITYP.EQ.1.AND.(IST.EQ.172.OR.IST.EQ.327.OR.
     1              IST.EQ.329)) THEN
C                                              **** OMIT OR RETAIN
               IF (IST.EQ.329) THEN
                   RETAIN = .TRUE.
               ELSE IF (IST.EQ.327) THEN
                   AVOID = .TRUE.
                   CALL PARSIT
                   IF (ITYP.EQ.3.OR.ITYP.EQ.4.OR.(ITYP.EQ.2
     1                  .AND.IST.EQ.2)) THEN
                       TAVOID = TV
                   ENDIF
               ELSE
                   OMIT = .TRUE.
               ENDIF
               IF (OMIT.AND.RETAIN) THEN
                  CALL ERROR(337)
                  GO TO 99999
               ENDIF
               IBEGIN = 0
               DO 110 II=1,1000
                   IF (THRU) THEN
                       IBEGIN = IBEGIN + UPDOWN
                       IF (IBEGIN.EQ.IEND) THEN 
                           THRU = .FALSE.
                           IEND = 0
                       ENDIF
                   ELSE
                       idtype = -1
                       CALL PARSIT
                       IF (ITYP.EQ.3.OR.ITYP.EQ.2.AND.IST
     1                     .EQ.2)THEN
                           IBEGIN=ITV
                       ELSE IF (ITYP.EQ.1.AND.IST.EQ.152) THEN
C                                             **** THRU
                           IF (IBEGIN.EQ.0) THEN
                               CALL ERROR(339)
                               GO TO 99999
                           ENDIF
                           THRU = .TRUE.
                           CALL PARSIT
                           IF (ITYP.EQ.3.OR.ITYP.EQ.2.AND.
     1                         IST.EQ.2) THEN
                               IEND = ITV
                               IF (IEND.LT.IBEGIN) THEN
                                   UPDOWN = -1
                               ELSE IF (IEND.GT.IBEGIN) THEN
                                   UPDOWN = 1
                               ELSE
                                   UPDOWN = 0
                               ENDIF
                           ELSE
                               CALL ERROR(53)
                               GO TO 99999
                           ENDIF
                       ELSE
						   if (ii .eq. 1 .and. avoid) then
							   call error(355)
							   go to 99999
						   endif
                           GO TO 120
                       ENDIF
                   ENDIF
C                        NOW UPDATE THE WORD IN RBUFF.
                   IF (FIRST.AND.RETAIN) THEN
C                        THIS IS THE FIRST WORD TO BE RETAINED.  RBUFF
C                        MUST BE INITIALIZED TO BE ALL 'OMITTED'.
                       DO 105,III=1,NPTS
                          CALL GTRANW(RBUFF,TWD,III,RIX)
                          I2TWD(1) = 1
                          CALL PTRANW(RBUFF,TWD,III,RIX)
105                    CONTINUE
                       FIRST = .FALSE.
                   ENDIF
                   IF (IBEGIN.LE.0.OR.IBEGIN.GT.NPTS) THEN
                       CALL ERROR(340)
                       GOTO 99999
                   ENDIF
C                      IF CONST AND INVERS, THEN RECIPRICATE THE INDEX
                   IF (.NOT.CONST.AND.ISC10(4).EQ.1) THEN
                       IX = NPTS+1-IBEGIN
                   ELSE
                       IX = IBEGIN
                   ENDIF
                   CALL GTRANW(RBUFF,TWD,IX,RIX)
                   IF (AVOID) THEN
                       R4TWD(2) = TAVOID
                       I2TWD(2) = 1
                   ELSE IF (RETAIN) THEN
                       I2TWD(1) = 0
                   ELSE IF (OMIT) THEN
                       I2TWD(1) = 1
                   ENDIF
                   CALL PTRANW(RBUFF,TWD,IX,RIX)
110            CONTINUE
120            CONTINUE
          ELSE IF (ITYP.EQ.7) THEN
               GO TO 88888
          ELSE
              CALL ERROR(336)
              GO TO 99999
          ENDIF
200   CONTINUE
88888 CONTINUE
C            PUT THE LAST RANFIL RECORD AWAY
      CALL PUTRAN(RBUFF,IFL(4)+2+RIX)
      IF (DEBUG) THEN
          DO 99998 I=1,10
          TWD = RBUFF(I)
          WRITE (COUT,1010)I,I2TWD(1),I2TWD(2),R4TWD(2)
1010      FORMAT ('GOTOPN: PT',I2,I5,I5,F13.5)
          CALL PUTMSG(COUT,80,I+15,0)
99998     CONTINUE
      ENDIF
99999 RETURN
      END
