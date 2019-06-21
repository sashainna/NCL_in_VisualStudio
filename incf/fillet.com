c**********************************************************************
c**
c**    NAME         :  fillet.com
c**
c**    CONTAINS:
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       fillet.com , 25.2
c**    DATE AND TIME OF LAST MODIFICATION
c**       05/27/15 , 17:13:35
c**
c*********************************************************************
c
      integer*2 N_IFILET,N_KFILET,N_GFILET
c
      parameter (N_IFILET = 20)
      parameter (N_KFILET = 50)
      parameter (N_GFILET = 80)
c
      common /ifilet/ IFILET
c
      integer*2 IFILET(N_IFILET)
c
      equivalence (IMLTSV,IFILET(001)), (ICRSTO,IFILET(002))
      equivalence (IROTPT,IFILET(003)), (IFEDCO,IFILET(004))
      equivalence (FILOUT,IFILET(005)), (ICROUT,IFILET(006))
      equivalence (IFILTA,IFILET(007)), (MULTAX,IFILET(008))
      equivalence (MXCL  ,IFILET(009)), (LCOUNT,IFILET(010))
      equivalence (ISTRPT,IFILET(011)), (IFADJ2,IFILET(013))
      equivalence (LCIRVC,IFILET(014)), (SFLWRN,IFILET(015))
      equivalence (SFLCOM,IFILET(016)), (SFLSAM,IFILET(017))
      equivalence (SFLDCO,IFILET(018)), (SFL347,IFILET(019))
C
      integer*2 IMLTSV, ICRSTO, IROTPT, IFEDCO, FILOUT, ICROUT,
     1          IFILTA, MULTAX, MXCL  , LCOUNT, ISTRPT(2), IFADJ2,
     2          LCIRVC, SFLDCO, SFL347
c
      logical*2 SFLWRN, SFLCOM, SFLSAM
C
      common /kfilet/ KFILET
c
      integer*4 KFILET(N_KFILET)
c
      equivalence (FILFLG,KFILET(001)), (FILPTR,KFILET(011))
C
      integer*4 FILFLG(10),FILPTR(32)
C
      common /gfilet/ GFILET
c
      real*8 GFILET(N_GFILET)
c
      equivalence (FILRAD,GFILET(001)), (FILTOL,GFILET(002))
      equivalence (OLDPTS,GFILET(003)), (TVX   ,GFILET(009))
      equivalence (TVZ   ,GFILET(012)), (FEDLOW,GFILET(015))
      equivalence (FEDHIG,GFILET(016)), (FEDOUT,GFILET(017))
      equivalence (FILTAX,GFILET(018)), (FILANG,GFILET(021))
      equivalence (FILAXS,GFILET(022)), (SPTSAV,GFILET(025))
      equivalence (CIRVC0,GFILET(067)), (SFLRAD,GFILET(070))
      equivalence (SFLTOL,GFILET(071)), (SFLANG,GFILET(072))
      equivalence (SFLDLO,GFILET(073)), (SFLDHI,GFILET(074))
      equivalence (SFLFED,GFILET(075)), (SFLFMX,GFILET(076))
      equivalence (SFLDIA,GFILET(077))
c
      real*8 FEDOUT,OLDPTS(6),FILTOL,TVX(3),TVZ(3),FEDHIG,FEDLOW,FILRAD,
     1       FILTAX(3),FILANG,FILAXS(3),SPTSAV(42),CIRVC0(3),SFLRAD,
     2       SFLTOL,SFLANG,SFLDLO,SFLDHI,SFLFED,SFLFMX,SFLDIA
c
      common /rfilet/ DCLBUF, CIRBUF, CLPT  , DNXBUF
c
      real*8 DCLBUF(420),CIRBUF(7),CLPT(6,80),DNXBUF(840)
c
