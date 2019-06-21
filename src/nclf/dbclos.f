c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbclos.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:49
c**
c*****************************************************
c **********************************************************************
c      notice: this source requires the filter program to be used before
c              compilation.
c **********************************************************************

c**
c** copyright (c) 1981,1982,1983,1984 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbclos                                         **
c **                                                                  **
c **  purpose of subroutine: to close the data base file pointed to   **
c **     by ifl(63).                                                  **
c **********************************************************************
c **********************************************************************
 
      subroutine dbclos
 
      include 'com4a.com'
      include 'comdb.com'
 
c      integer*2 access
 
c      access=cdbio-9
c      if (ifl(access+63).eq.0) go to 99999
      if (ifl(64).eq.0) go to 99999
 
c          set data base file name to blank in mapping block
c=rsx
c      if (ncsfl(7).ne.0) then
c        is=(ncsfl(7)-1)*82+54
c        vwdb(5)=0
c        call craw (vwdb,idsw)
c        if (idsw.ne.1) call crawer ('dbclos',idsw)
c        do 100 i=is,is+26,1
c100       lncrb(i)=' '
c        call craw (icwdb,idsw)
c        if (idsw.ne.1) call crawer ('dbclos',idsw)
c      endif
c=all
 
c      close (unit=cdbio)
      close (unit=dblun)
c      ifl(access+63)=0
      ifl(64)=0
      if (wdbio.eq.cdbio) then
          wdbio=0
          wdbnpg=0
          wdbgpg=0
          wdbel=0
      endif
 
99999 return
      end
