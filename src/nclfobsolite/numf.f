C ***  FILE NAME: NUMF
C**
C** COPYRIGHT (C) 1989 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **  SUBROUTINE NAME: NUMF
C **
C **     MODULE NAME AND RELEASE LEVEL 
C **       numf.f , 25.1
C **    DATE AND TIME OF LAST  MODIFICATION
C **       04/29/15 , 15:10:21
C **
C **  PURPOSE OF SUBROUTINE: TO PARSE AND HANDLE THE FOLLOWING CASES:
C **   NUM(PN1)  -  THIS RETURNS THE NUMBER OF POINTS IN A PATERN
C **   NUM(X)    -  X IS A RESERV VARIABLE; THIS RETURNS THE MAXIMUM
C **                   VALUE OF THE SUBSCRIPT FOR THIS VARIABLE.
C **
C **********************************************************************
 
      SUBROUTINE NUMF
 
      include 'com8a.com'
      
      INTEGER*2 KTV(4),ITEMP
      integer*4 pntyp,nclkey, ipg, iel, I4TEMP
      integer*2 nwds,jcomp,ietype

      EQUIVALENCE (TV,KTV)
      

      CALL PARSER
      IF (ITYP.EQ.5.AND.IST.EQ.6) THEN
          CALL PARSER
          IF (ITYP.EQ.2.AND.IST.EQ.14) THEN
C                      ITS A RESERV VARIABLE.  IF THE NEXT TOKEN IS
C                      A '(' THEN IT MUST BE A SUBSCRIPTED PATERN NAME.
C                      IF NOT, THEN RETURN THE MAX NUMBER OF SUBSCRIPTS 
C                      USED.
             IF (NEXTYP.EQ.6) THEN
                CALL EXPRS2
                IF (IFL(2).NE.0) THEN
                   GO TO 99999
                ENDIF
             ENDIF
          ENDIF
      ELSE
          ifl(2)=309
          GO TO 99999
      ENDIF
      IF (ITYP.EQ.2.AND.IST.EQ.8) THEN
c
c..... return number of composite components, (1 if not composite)
c
        itemp = 1
        I4TEMP = 1
        call gtdesc(tv, nclkey, nwds, ietype)
        call iscmpc (nclkey, jcomp)
        if (jcomp .eq. 1) call gtccnm (nclkey, itemp)
        I4TEMP = ITEMP
      else IF (ITYP.EQ.2.AND.IST.EQ.20) THEN
C                        ***** ITS A PATERN, RETURN THE NUMBER OF PTS
           CALL GTPNNP(TV,ITEMP,pntyp)
           I4TEMP = ITEMP
      ELSE IF (ITYP.EQ.2.AND.IST.EQ.14) THEN
C                        ****** ITS A RESERV VARIABLE.  RETURN THE MAX
C                               NUMBER OF SUBSCRIPTS USED.
           call vxchk (token2, ivxsub, nclkey, ipg, iel, nwds, ist)
           I4TEMP = ipg
      else if (ityp.eq.2.and.ist.eq.DATAST) then
C                        ***** its a data statement, return number of elements
           I4TEMP = ktv(3)
      else
          ifl(2)=374
          go to 99999
      ENDIF
      CALL PARSER
      IF (ITYP.NE.5.OR.IST.NE.7) THEN
          CALL ERROR(310)
          GO TO 99999
      ENDIF
      TV=I4TEMP
99999 RETURN
      END
