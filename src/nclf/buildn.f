C*********************************************************************
C*    NAME         :  buildn.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       buildn.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:38
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine buildn (toktyp)
C*       purpose of subroutine: this subroutine builds a name and puts
C*       it in token.  toktyp determines whether its a subscripted
C*       identifier or an auto generated identifier.  if its an auto
C*       generated one, the number is in itv.  if its subscripted,
C*       sc(116) contains the identifier.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine buildn (toktyp)

      include 'com8a.com'

      integer*2 toktyp
      character*2 styp
      character*64 csc116
      integer*2 i,k
      integer*4 strlen1, nc
      equivalence (sc116,csc116)

      do 50 i = 1, 64, 1
               token2(i:i) = ' '
 50   continue
      if (toktyp.eq.1) then
           token2=csc116
           ivxsub=ifl4(106)
      else if (toktyp.eq.3) then
           nc = strlen1(thrlbl)
           if (itv.lt.10) then
               write (token2,501) thrlbl(1:nc),itv
               k = 3
           else if (itv.lt.100) then
               write (token2,502) thrlbl(1:nc),itv
               k = 4
           else if (itv.lt.1000) then
               write (token2,503) thrlbl(1:nc),itv
               k = 5
           else if (itv.lt.10000) then
               write (token2,504) thrlbl(1:nc),itv
               k = 6
           else if (itv.lt.100000) then
               write (token2,505) thrlbl(1:nc),itv
               k = 7
           else if (itv.lt.1000000) then
               write (token2,506) thrlbl(1:nc),itv
               k = 8
           else
               call error (14)
               go to 99999
           endif
           ivxsub=0
501        format(a,i1)
502        format(a,i2)
503        format(a,i3)
504        format(a,i4)
505        format(a,i5)
506        format(a,i6)
      else
           call idtonm (ist,styp)
c              create token from two characters and itv value
           if (itv.lt.10) then
               write (token2,401) styp,itv
               k = 3
           else if (itv.lt.100) then
               write (token2,402) styp,itv
               k = 4
           else if (itv.lt.1000) then
               write (token2,403) styp,itv
               k = 5
           else if (itv.lt.10000) then
               write (token2,404) styp,itv
               k = 6
           else if (itv.lt.100000) then
               write (token2,405) styp,itv
               k = 7
           else if (itv.lt.1000000) then
               write (token2,406) styp,itv
               k = 8
           else
               call error (14)
               go to 99999
           endif
           ivxsub=0
401        format(a2,i1)
402        format(a2,i2)
403        format(a2,i3)
404        format(a2,i4)
405        format(a2,i5)
406        format(a2,i6)
           do 100 i = k+1, 64, 1
               token2(i:i) = ' '
  100      continue
      endif

      if (toktyp.ne.0) then
           call vstchk
           if (ist.eq.1) then
c                  unknown identifier
               nc = strlen1 (token2)
               if (toktyp.eq.1) then
                   write(errcom,149)token2,ivxsub
149                format('IDENTIFIER = ',a<nc>,'(',i6,')')
               else
                   write(errcom,150)token2
150                format('IDENTIFIER = ',a<nc>)
               endif
           endif
      endif

c     if (debug) then
c          write (cout,9010) toktyp,token,styp,ist
c9010      format ('buildn toktyp='i4' token='a6' styp='a2' ist='i4)
c          call putmsg (cout,80,19,0)
c     endif

99999 return
      end
