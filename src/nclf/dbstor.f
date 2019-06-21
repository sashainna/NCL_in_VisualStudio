c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbstor.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:51
c**
c*****************************************************
c**
c** copyright (c) 1981,1982,1983 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbstor                                         **
c **                                                                  **
c **  purpose of subroutine: to get the canonical form of a geometry  **
c **     entity from the unibase and put it in the appropriate data   **
c **     base file.                                                   **
c **                                                                  **
c **********************************************************************
c **********************************************************************
c
c...note: I haven't change character to 64 from 6 because I think the data file have that
c...format
c
      subroutine dbstor (dbnam,rfname,asw,found)

      include 'com8a.com'
      include 'comdb.com'

      integer*4 nclkey, i4rtv, pntyp
      integer*2 rfnw,dbnw,rist,ietype
      integer*2 ktv(4),irtv(4),ijb8(144),dbpg,dbel,aswel
      integer*2 npg,gpg,irfbuf(160),idbel
      integer*2 dbpgsv,dbelsv,rfsbnm(4),itok(4)
      real*4 rfbuf4(80)
      real*8 rfbuf(40),jb8(36),rtv,rdbnam
      character*8  rfname,dbname,dbnam
      character*8 rname, tok
      equivalence (dbname,rdbnam)
      equivalence (tok,itok),(ktv,tv)
      equivalence (ijb8,jb8)
      equivalence (irtv,rtv,i4rtv)
      equivalence (rfnw,irtv(3)),(rist,irtv(4))
      equivalence (rfbuf,irfbuf,rfbuf4)
      equivalence (npg,ijb8(5)),(gpg,ijb8(6)),(idbel,ijb8(7))
      equivalence (rname,rfsbnm)
      real*8 dbver,dbver1
      equivalence (dbver,sc(166))
      parameter (dbver1=8.1)
      logical redef,found,trflg,ilog
c 
c     added on: 4/1/88 by: kathy
      real*8     rbuff(6)
      integer*2  itemp(4)
      logical    trans
      equivalence (itemp,rbuff(1))
      
      redef=.true.
      ilog=.false.
      trflg = .true. 
 
      rtv=asw
      rist=irtv(4)
      rname=rfname
      if (rist.lt.2 .or. rist.gt.10 .and. rist .ne. 20 .or.
     x    rfsbnm(4).eq.xffff) go to 8888 
c                               Don't store wf entities.
      if (rist.eq.8 .or. rist.eq.9) then
        call isitwf(i4rtv,i)
        if (i.eq.1) go to 8888 
        if (rist.eq.9) then
          call sftype (i4rtv,isftyp)
          if (isftyp.eq.27) then
            call isnswf (i4rtv, i)
            if (i.eq.1) goto 8888
          endif
        endif
      endif
      if (rist .eq. 20) then
          call gtpnnp (rtv,nw,pntyp)
          if (pntyp .eq. 2) go to 8888
      end if 
      found=.true.
      dbname=dbnam
      token2(1:6) = dbname(1:6)
      tok = dbname
	ivxsub = itok(4)
      call dbchk (ipg,iel)
      if (ist.ne.1) go to 90
85        ktv(1)=wdbgpg
          ktv(2)=wdbel
          ktv(3)=rfnw
          ktv(4)=rist
          redef=.false.
          go to 200
90        dbnw=ktv(3)
          redef=.true.
c              check if it is a scalar
100       if (rist.eq.2) go to 210
          if (rist.eq.9.or.(rfnw.gt.dbnw.and.ist.ne.1)) then
              ilog=.true.
              go to 85
          endif
          dbpg=ktv(1)
          dbel=ktv(2)
          ktv(3)=rfnw
          ktv(4)=rist
200   if (rist.ne.2) go to 250

c          this is scalar so get value
210   continue
      call gtgeo(i4rtv,tv)
      ktv(4)=ktv(4)/32*32
250   if (redef) go to 270
      call getdb (jb8,ipg)
      jb8(iel)=rdbnam
      if (.not.ilog) ijb8(141)=iel
      call putdb (jb8,ipg)
270   jpg=ipg+1
      aswel=iel
      if (rist.eq.9) go to 275
      call getdb (jb8,jpg)
      jb8(iel)=tv
      call putdb (jb8,jpg)
      if (rist.eq.2) go to 750
      if (rist.eq.20) go to 295
      if (rist.ne.8) go to 500
      go to 290

275   continue
c                     surfaces - allocate new page if current geo page not
c                                contiguous with next geo page
      if (wdbnpg.gt.wdbgpg+1) then
        wdbgpg=wdbnpg
        wdbnpg=wdbnpg+1
        wdbel=1
      endif
c              determine sf type & call appropriate routine.
      call sftype (i4rtv,isftyp)
      if (isftyp.eq.27) then
        call dbpssf(rtv,ktv(1),ktv(2),ktv(3))
      else if (isftyp.eq.26) then
c                                a mesh surface
        call dbpmsh(rtv,ktv(1),ktv(2))
        if (ktv(1).eq.0) go to 8888 
      else if (isftyp.eq.25) then
c                                a rockwell surface
        call dbpqlt(rtv,ktv(1),ktv(2))
      else if (isftyp.eq.91) then
c                                a normal ncl surface
        call dbpnsf(rtv,ktv(1),ktv(2))
      endif
      goto 710
c                                    curve
290   continue
      iclos=1
      if (dbver.lt.dbver1) iclos=0
      call gtcvhd (i4rtv,rfbuf(iclos+1))
      numseg=rfbuf4(iclos*2+1)
      icnt=(numseg+1)/2+iclos
      icvcnt=icnt+6*numseg
      if (iclos.eq.1) then
        rfbuf(1)=0.d0
        irfbuf(1)=numseg
        call gtclsd(i4rtv,0,irfbuf(2))
      endif
      rfnw=6
      rist=81
c          if the next available data base record
c          is not contiguous to the current geometry record and there
c          is not enough room to fit all of the curve data on the current
c          data base record, force the curve to start on the next 
c          available data base record
      if (.not.redef.and.wdbnpg.gt.wdbgpg+1) then
c               check for enough room for curve data
         if (wdbel+icvcnt.gt.36) then
           wdbgpg=wdbnpg
           wdbnpg=wdbnpg+1
           wdbel=1
         endif
      endif
      call dbpent (rfbuf,icnt,dbpg,dbel,redef)
      dbpgsv=dbpg
      dbelsv=dbel
      dbel=dbel+icnt
      do 300 iseg=1,numseg
      if (dbel.gt.35) then
        dbel=dbel-35
        dbpg=dbpg+1
      endif
      call gtcseg(i4rtv,iseg,rfbuf)
      if (ifl(72).eq.1) call transf(rfbuf,sc(68),rfnw,rist)
      call dbpent (rfbuf,rfnw,dbpg,dbel,redef)
      dbel=dbel+rfnw
300   continue
      ktv(1)=dbpgsv
      ktv(2)=dbelsv
      ktv(3)=icvcnt
      ktv(4)=8
      goto 710

c 
c     implemented patern on: 4/1/88 by: kathy
c
295   trans = .true. 
c 
c          check if enough room is available for data if next available
c          data base record is not contiguous with current data base 
      if (.not.redef.and.wdbnpg.gt.wdbgpg+1) then
          if (wdbel+rfnw.gt.36) then
              wdbgpg=wdbnpg
              wdbnpg=wdbnpg+1
              wdbel=1
          endif
      endif
      call gtpnnp(rtv,nw,pntyp)
      itemp(1) = nw
      call dbpent (rbuff,3,dbpg,dbel,redef)
      dbpgsv=dbpg
      dbelsv=dbel
      dbel=dbel+3
      if (dbel.gt.35) then
        dbel=dbel-35
        dbpg=dbpg+1
      endif
      do 550 i=1,nw
            call gpnptt(rbuff,rtv,i,trans)
            call dbpent(rbuff,3,dbpg,dbel,redef)
            dbel=dbel+3
            if (dbel.gt.35) then
                dbel=dbel-35
                dbpg=dbpg+1
            endif
550   continue
      ktv(1)=dbpgsv
      ktv(2)=dbelsv
      ktv(3)=rfnw
      ktv(4)=rist
      goto 710
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c    m o v e   d a t a   f r o m   r a n f i l   t o   d a t a   b a s e
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c500   call gtgeo (i4rtv,rfbuf)
c      if (ifl(72).eq.1) call transf(rfbuf,sc(68),rfnw,rist)
c
500   call gtentt(rtv,trflg,nclkey,ietype,rfbuf)
c 
c          check if enough room is available for data if next available
c          data base record is not contiguous with current data base 
      if (.not.redef.and.wdbnpg.gt.wdbgpg+1) then
          if (wdbel+rfnw.gt.36) then
              wdbgpg=wdbnpg
              wdbnpg=wdbnpg+1
              wdbel=1
          endif
      endif
      call dbpent (rfbuf,rfnw,dbpg,dbel,redef)
      ktv(1)=dbpg
      ktv(2)=dbel
      ktv(3)=rfnw
      ktv(4)=rist

710   call getdb (jb8,jpg)
      jb8(aswel)=tv
      call putdb (jb8,jpg)
 
c              update data base record zero pointers
750   call getdb (jb8,0)
      npg=wdbnpg
      gpg=wdbgpg
      idbel=wdbel
      call putdb (jb8,0)
      go to 99999
c
c...entity ignored
c
 8888 found = .false.
c
99999 return
      end
