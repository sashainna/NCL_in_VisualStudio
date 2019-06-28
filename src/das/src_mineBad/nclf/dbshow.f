c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbshow.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:51
c**
c*****************************************************
c**
c** copyright (c) 1981,1982 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbshow                                         **
c **                                                                  **
c **  purpose of subroutine: to display canonical data of geometry    **
c **    items from a data base fil                                    **
c **********************************************************************
c **********************************************************************
 
      subroutine dbshow
 
      include 'com8a.com'
      include 'comdb.com'
 
      integer*2 pg,el,nw,peq(4)
      real*8 pgelnw
      equivalence (peq(1),pgelnw)
      equivalence (pg,peq(1)),(el,peq(2)),(nw,peq(3))
      equivalence (token2,tkn)
      character*71 type(4)
      equivalence (cout,type)
      character*1 in(64),out(71),sscp(5)
      character*5 subscr
      character*64 tkn
      character*64 dbgeo
      equivalence (sscp(1),subscr),(in(1),token2),(out(1),type(1))
      character*14 dbnum
 
      ifl(1)=0
c          get geometry name and optionally data base file number
      ifl(44)=9
      call parsit
      if (ityp.eq.2) go to 10
          call error(101)
          go to 99999
 
10    dbgeo=token2
      if (nextyp.eq.11) go to 45
          call parsit
          if (ityp.eq.3.and.itv.gt.0.and.itv.lt.4) go to 30
              call error(103)
              go to 99999
c30        if (ifl(63+itv).ne.0) go to 40
30        if (ifl(64).ne.0) go to 40
31            call error(102)
              go to 99999
c40        cdbio=ifl(63+itv)
40        cdbio=ifl(64)
          i=99
          token2=dbgeo
          go to 55
 
c              search all open data base files for geometry name
45    if (dbfn1.eq.0.and.dbfn2.eq.0.and.dbfn3.eq.0) go to 31
      i=1
c50    if (ifl(63+i).eq.0) go to 60
c          cdbio=ifl(63+i)
50    if (ifl(64).eq.0) go to 60
          cdbio=ifl(64)
55        call dbchk(pg,el)
          if (ityp.eq.2.and.ist.gt.1.and.ist.lt.11) go to 70
60            i=i+1
              if (i.lt.4) go to 50
                  call error(113)
                  go to 99999
 
70    cout=' '
      if (ivxsub .ne. 0) then
          write (subscr,1) ivxsub
1         format(i5)
          j=1
          do 90 i=1,64
              if (in(i) .ne. ' ') then
                  out(j)=in(i)
                  j=j+1
              endif
90        continue
          out(j)='('
          j=j+1
          do 100 i=1,5
              if (sscp(i) .ne. ' ') then
                  out(j)=sscp(i)
                  j=j+1
              endif
100       continue
          out(j)=')'
      else
          type(1)=tkn
      endif
      if (ist.eq.2) then
          type(2)='  scalar'
      else if (ist.eq.3) then
          type(2)='  point'
      else if (ist.eq.4) then
          type(2)='  vector'
      else if (ist.eq.5) then
          type(2)='  line'
      else if (ist.eq.6) then
          type(2)='  plane'
      else if (ist.eq.7) then
          type(2)='  circle'
      else if (ist.eq.8) then
          type(2)='  curve'
      else if (ist.eq.9) then
          type(2)='  surf'
      else if (ist.eq.10) then
          type(2)='  matrix'
      endif
      j=17
      call putmsg(cout,80,15,0)
      i=cdbio-9
      write (dbnum,2010) i
2010  format (' data base:',i2)
c                                                    **** scalar
      if (ist.eq.2) then
          write(cout,130) dbnum,tv
          call putmsg(cout,80,16,0)
c                                      **** circle, curve or surface
      else if (ist.eq.7.or.ist.eq.8.or.ist.eq.9) then
          write(cout,130) dbnum
          call putmsg(cout,80,16,0)
      else
        pgelnw=tv
        call dbgent (sc(10),nw,pg,el)
c                                                    **** point/vector
        if (ist.eq.3.or.ist.eq.4) then
          write(cout,130) dbnum,sc(10),sc(11),sc(12)
          call putmsg(cout,80,16,0)
c                                                    ***** line
        else if (ist.eq.5) then
          write(cout,130) dbnum,sc(10),sc(11),sc(12)
          call putmsg(cout,80,16,0)
          dbnum=' '
          write(cout,130) dbnum,sc(13),sc(14),sc(15)
          call putmsg(cout,80,17,0)
          j=18
c                                                    ***** plane
        else if (ist.eq.6) then
          write(cout,130) dbnum,sc(10),sc(11),sc(12),sc(13)
130       format(a14,4(f12.6,4x))
          call putmsg(cout,80,16,0)
c                                                    **** matrix
        else if (ist.eq.10) then
          write(cout,130) dbnum,sc(10),sc(11),sc(12),sc(13)
          call putmsg(cout,80,16,0)
          dbnum=' '
          write(cout,130) dbnum,sc(14),sc(15),sc(16),sc(17)
          call putmsg(cout,80,17,0)
          write(cout,130) dbnum,sc(18),sc(19),sc(20),sc(21)
          call putmsg(cout,80,18,0)
          j=19
        endif
      endif
c
c...Added check for NCL-VT mode
c...Paul  -  10/2/91
c...Old variant was:
c   890   if (.not.ksr .and. ifl(35).eq.0) call ersw3 (j,1)
c

890   if ((.not.ksr .and. ifl(35).eq.0).or.
     *(.not.ksr .and. ifl(35).eq.0)) call ersw3 (j,1)
 
99999 return
      end

