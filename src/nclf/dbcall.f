c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbcall.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:49
c**
c*****************************************************
c ***  file name: dbcall
c**
c** copyright (c) 1981,1982 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbcall                                         **
c **                                                                  **
c **  last revision:                                                  **
c **                                                                  **
c **  purpose of subroutine: to call one of the four main data base   **
c **   subroutines. (dbfn, dbget, dbput and dbshow)                   **
c **                                                                  **
c **********************************************************************
c **********************************************************************
 
      subroutine dbcall (icall)
 
      include 'com4a.com'
      integer*2 icall

      if (ifl(316) .eq. 0) then
          ifl(2) = 102
      else
          if ( ifl (316) .eq. 1 ) then
             if (icall.eq.1) call dbfn
             if (icall.eq.4) call dbshow
          else if ( ifl (316) .eq. 2 ) then
             if (icall.eq.1) call ubfn
             if (icall.eq.4) call ubshow
          endif
          if (icall.eq.2) call ubio (1)
          if (icall.eq.3) call ubio (2)
      endif
c
      if (ifl(2).ne.0) then
          ist=2
          call error (ifl(2))
      endif
 
      return
      end
