C*********************************************************************
C*    NAME         :  jmpchk.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       jmpchk.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:12
C********************************************************************/
C
C **********************************************************************
C **  PROGRAM NAME: JMPCHK
C **
C **  PURPOSE OF PROGRAM: CHECK IF JUMPTO A LABEL IS VALID.
C **                      LABEL MUST BE IN TOKEN, PG & EL OF LABEL REC
C **                      MUST BE IN TV
C **
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE JMPCHK
 
      include 'com4a.com'

      CHARACTER*1536 SVCIN
      REAL*8 SAVETV
      CHARACTER*64 SVTKN
      INTEGER*2 INXSV
      INTEGER*2 KTV(4)
      EQUIVALENCE (TV,KTV)
      integer*4 i,ilin5,ilin6,jlin,svsub

      IF (IFL(38).EQ.0 .AND. IFL(45).EQ.0) THEN
        IFL(2)=80
        GOTO 99999
      ENDIF
      CALL GETRAN(JB,KTV(1))
      IF (JB4(KTV(2)).EQ.-1) THEN
        IFL(2)=111
        GOTO 99999
      ENDIF
      call nclf_src_rec_to_line (jb4(ktv(2)),jlin)
      call nclf_src_rec_to_line (ifl4(5),ilin5)
      call nclf_src_rec_to_line (ifl4(6),ilin6)
      IF (jlin .GT. ilin5 .AND. jlin-1 .LE. ilin6) 
     1      GO TO 99999
C                   THIS MAY BE A FORWARD JUMPTO TO A LABEL THAT
C                   HASN'T BEEN SEEN YET.  CHECK IT OUT.  EPM  4-22-88
      SAVETV=TV
      SVTKN=TOKEN2
      SVSUB=IVXSUB
      SVCIN=CIN
      INXSV=INX
c
c...Find the JUMPTO label
c...in the source file
c...6/26/91  -  Bobby
c
      call labchk (ilin5,ilin6,svtkn,svsub,i)
      if (i .ne. 0) then
          tv     = savetv
          call nclf_src_line_to_rec (i,jb4(ktv(2)))
          call putran(jb,ktv(1))
      else
          ifl(2) = 111
      endif
c
c...Restore current input line
c
      CIN=SVCIN
      INX=INXSV
      TV=SAVETV
      TOKEN2=SVTKN
      IVXSUB=SVSUB

99999 RETURN
      END
