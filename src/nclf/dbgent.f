c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbgent.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:49
c**
c*****************************************************
c**
c** copyright (c) 1981,1982 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbgent                                         **
c **                                                                  **
c **  purpose of subroutine: to move a maximum of 35 real*8 words     **
c **     from a data base file to a buffer.                           **
c **********************************************************************
c **********************************************************************
 
      subroutine dbgent (buf,dbnw,dbpg,dbel)
 
      include 'com8a.com'
      include 'comdb.com'
 
      integer*2 dbnw,dbpg,dbel,nwds,page,elem
      real*8 buf(36),jb8(36)
 
      j=0
      nwds=dbnw
      page=dbpg
      elem=dbel
100   max=elem+nwds-1
      if (max.gt.35) max=35
      call getdb (jb8,page)
      do 200 i=elem,max
          j=j+1
          buf(j)=jb8(i)
200   continue
      nwds=nwds-(max-elem+1)
      if (nwds.le.0) go to 99999
      page=page+1
      elem=1
      go to 100
 
99999 return
      end
