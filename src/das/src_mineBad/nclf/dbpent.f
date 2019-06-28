c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbpent.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:50
c**
c*****************************************************
c ***  file name: dbpent
c**
c** copyright (c) 1981,1982 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbpent                                         **
c **                                                                  **
c **  last revision: to add code to return dbpg and dbel to calling   **
c **    routine.                                                      **
c **                                                                  **
c **  purpose of subroutine: to move a maximum of 35 real*8 words     **
c **     from a buffer to a data base file.                           **
c **                                                                  **
c **********************************************************************
c **********************************************************************

      subroutine dbpent (buf,dbnw,dbpg,dbel,redef)

      include 'com8a.com'
      include 'comdb.com'

       integer*2 dbnw,dbmax,dbpg,dbel
       integer*2 sdbnpg,sdbgpg,sdbel
       real*8 buf(36),jb8(36)
       logical redef,first
       first=.true.
       j=0
       nwds=dbnw
       if (.not.redef) go to 50
           sdbnpg=wdbnpg
           sdbgpg=wdbgpg
           sdbel=wdbel
           wdbnpg=dbpg+1
           wdbgpg=dbpg
           wdbel=dbel
50     if (wdbel+nwds.gt.36) go to 100
           dbmax=wdbel+nwds-1
           nwds=0
           go to 200
100    if (wdbnpg.ne.wdbgpg+1) go to 600
       dbmax=35
       nwds=nwds-(36-wdbel)
200    if (wdbel .eq. 1 .and. .not. redef) go to 400
       call getdb (jb8,wdbgpg)
400    do 500 i=wdbel,dbmax
           j=j+1
           jb8(i)=buf(j)
500    continue
       call putdb (jb8,wdbgpg)
      if (ifl(2).ne.0) goto 99999
       if (.not.first) go to 550
           dbpg=wdbgpg
           dbel=wdbel
           first=.false.
550    wdbel=dbmax+1
       if (nwds.eq.0.and.wdbel.le.35) go to 700
600        wdbgpg=wdbnpg
           wdbnpg=wdbnpg+1
           wdbel=1
           if (nwds.lt.1) go to 700
           go to 50
700    if (.not.redef) go to 99999
           wdbnpg=sdbnpg
           wdbgpg=sdbgpg
           wdbel=sdbel
99999  return
       end

