C*********************************************************************
C*    NAME         :  prtput
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       prtput.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:29
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine prtput
c*       write only motion and postprocessor statements to the batch
c*       listing file                          
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
      subroutine prtput

      include 'com8a.com'

      character*1 aout(MAX_LEN)
      equivalence (aout,cout)
      integer*4 isvll, nc, strlen1

      ncsfl(2)=0

      if (ifl(35).eq.1) then
          if (ncsfl(1).eq.1 .and.
     x        (ifl(38).eq.1 .or. ifl(45).eq.1) .or.
     x        .not.echo) go to 99999

c            check for major word in front of statement
          if (((ityp.eq.5 .and. ist.eq.4) .or.
     1         (ityp.eq.5 .and. ist.eq.10) .and.
     1         ifl(38).ne.1 .and. ifl(45).ne.1) .or.
     2        (ifl(38).ne.2 .and. ifl(45).ne.2 .and.
     3         ncsfl(1).eq.0) .or.
     4        err .or.nextyp.eq.13.or.
     5        ((ityp.eq.1) .and.
     6         (ist.eq.824 .or. (ist.ge.700 .and. ist.le.741) .or.
     x         (ist.ge.745 .and. ist.le.750) .or. ist.eq.873 .or.
     7          ist.ge.1000 .or. ist.eq.801 .or. ist.eq.802 .or.
     8          ist.eq.803 .or. ist.eq.808.or.ist.eq.809 .or.
     x          ist .eq. 761 .or.
     9          ist.lt.500 .or. (ist.eq.807 .and. ifl(38).eq.2)))) then
              ncsfl(2)=1
              do 100 i=1,ifl(123)
                  isvll = svll -1 +i
                  call nclf_getw2 (i,w2,ncw2,it)
                  if (it .eq. 0) then
                      write (cout,1010) isvll,w2(1:ncw2)
                  else
                      write (cout,1011) isvll,w2(1:ncw2)
                  endif
1010              format (i6,': ',a)
1011              format (i6,'# ',a)
90                ncw1 = ncw2 + 8
                  call putprt (cout,ncw1,1)

100           continue
          endif
      endif

99999 return
      end
