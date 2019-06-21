c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       ubshow.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:49
c**
c*****************************************************
c**
c** copyright (c) 1994 NCCS.
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: ubshow                                         **
c **                                                                  **
c **  purpose of subroutine: to display canonical data of geometry    **
c **    items from a unibase file                                     **
c **********************************************************************
c **********************************************************************
 
      subroutine ubshow
 
      include 'com8a.com'
      include 'comdb.com'
 
      ifl(1)=0
c          get minor word; can be LIST(863) or STAT(864)
      ifl(44)=9
      call parsit
      if (ityp.eq.1.and.(ist.eq.863.or.ist.eq.864)) go to 10
          call error(453)
          go to 99999
 
10    if (ist .eq. 863) call ur_st02c(1)
      if (ist .eq. 864) call ur_st02c(2)
 
99999 return
      end

