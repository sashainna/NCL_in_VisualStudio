c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbfind.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:49
c**
c*****************************************************
c*****************************************************
c**
c** copyright (c) 1981,1982 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbfind                                         **
c **                                                                  **
c **  purpose: to find an entry on a data base file and move it to    **
c **     the ranfil.                                                  **
c **                                                                  **
c **********************************************************************
c **********************************************************************
c
c...note: I haven't change character to 64 from 6 because I think the data file have that
c...format
c
      subroutine dbfind (dbname,rname,found,errok,copied)

      include 'com8a.com'
      include 'comdb.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      parameter (maxpt=50)
      parameter (maxwd=(maxpt*6+(maxpt+1)/2+1))

      integer*2 rktv(4),kdbtv(4),dipg,diel
      integer*2 dist,idbbuf(4),ktv(4)
      integer*2 dbnw,i2tok(4),subsv,irest(4)
      integer*4 i4rest(2),pntyp
      character*8 dbname,rname, tok
      equivalence (tok,i2tok),(rest,irest,i4rest)
      real*4 rdbbuf(80)
      real*8 dbtv,dbbuf(maxwd)
      equivalence (rktv,rest),(kdbtv,dbtv),(dbbuf,idbbuf,rdbbuf)
      equivalence (ktv,tv)
      real*8 dbver,dbver1
      equivalence (dbver,sc(166))
      parameter (dbver1=8.1)
      logical errok,copied
      logical found
c 
c     added for patern on: 4/1/88 by: kathy
      real*8     dbuff(3)
      integer*2  itemp(4)
      integer*4  ipnkey
      logical    trans
      equivalence (dbuff(1),itemp)
      integer*4 nclkey
      integer*2 nwds, ietype

      copied=.false.

c          get location of it on the data base file
      tok = dbname
      token2(1:6) = dbname(1:6)
	ivxsub = i2tok(4)
      call dbchk (ipg,iel)
      if (ist.ne.1) go to 200
           found=.false.
           go to 99999
200   found=.true.
      dbtv=tv
      dist=ist
  
c          check if it's on ranfil
      tok = rname
      token2(1:6) = rname(1:6)
	ivxsub = i2tok(4)
      call vstchk
c          it's unknown and not subscripted
      if (ityp.eq.2 .and. ist.eq.1 .and. ivxsub.eq.0) then
       
      else if (ityp.eq.2 .and. ist.eq.1 .and. ivxsub.ne.0) then
c              the token is subscripted
          subsv=ivxsub
          ivxsub=0
          call vstchk
 
c              is there variable reserved?
          if (ityp.eq.2 .and. ist.eq.14) then
 
c                  yes - is the subscript within the valid range?
c...yurong              if (subsv.le.ktv(3)) then
              if (subsv.le.32767) then
                  ivxsub=subsv
                  call vstchk
              else
 
c                      give subscript out of range error
                  if (.not.errok) then
                      ist=99
                      call error (85)
                  endif
                  go to 99999
              endif
          else
c                               do automatic reserv
c102706
            savid2=token2
            isvsub = ivxsub
            ifl(9)=ifl(11)
            ifl(10)=ifl(12)
            idst=14
            rest=0.
            irest(3)=32767
            irest(4)=14
            call vstore
            ivxsub=subsv
            call vstchk
          endif
                  
c          known identifier and canon/on or scalar
      else if (ityp.eq.2 .and.
     x         (((ist.gt.2.and.ist.le.10) .or. ist .eq. 20) .and.
     x          ifl(41).eq.1) .or. ist.eq.2) then
      else
c          if (.not.errok) call error (8)
          go to 99999
      endif
 
300   savid2(1:6) = rname(1:6) 
      tok = rname
      isvsub = i2tok(4)
      idst=dist
      rest=tv
      keyold = keyhld
      istold = ist
 
c          if it's a scalar, move the value in and go store it
      if (dist.eq.2) then
          rest=dbtv
          go to 995
      endif
310   rktv(3)=kdbtv(3)
      rktv(4)=kdbtv(4)
  
c              read from data base file and put on ranfil
350   dipg=kdbtv(1)
      diel=kdbtv(2)
      dbnw=kdbtv(3)
      if (dist.eq.8) go to 370
      if (dist.eq.9) go to 355
      if (dist.eq.20) go to 365
      call dbgent (dbbuf,dbnw,dipg,diel)
c
c...  Zero out plane display point
c
      if (dist.eq.6) then
        dbbuf(5) = 0.0
        dbbuf(6) = 0.0
        dbbuf(7) = 0.0
      else if (dist .eq. 10) then
        dbbuf(13) = 1.0
        if (ifl(264) .eq. 1) dbbuf(13) = 25.0
        dbbuf(14) = .75 * dbbuf(13)
        dbbuf(15) = .75 * dbbuf(13)
        dbbuf(16) = .75 * dbbuf(13)
      endif
      call ptentt(dist,dbbuf,i4rest(1),tv)
      goto 900

355   call dbgent (dbbuf,1,dipg,diel)

      rktv(3)=1
c                                a super surface
      if (idbbuf(1).eq.27) then
        call dbgssf(dbtv,i4rest(1))
c                                a mesh surface
      else if (idbbuf(1).eq.26) then
        call dbgmsh(dbtv,i4rest(1))
c                                a rockwell surface
      else if (idbbuf(1).eq.25) then
        call dbgqlt(dbtv,i4rest(1))
c                                ncl surface
      else
        call dbgnsf(dbtv,i4rest(1))
      endif
      goto 995
c 
c         implemented patern on: 4/1/88 by: kathy
c
365   trans = .true.
      call dbgent(dbuff,3,dipg,diel)
      numpts = itemp(1) 
      pntyp = 1
      call ptpnhd (dbuff,pntyp,ipnkey,1)
      i4rest(1) = ipnkey
      diel=diel+3
      if (diel.gt.35) then
        diel=diel-35
        dipg=dipg+1
      endif
      do 375 i=1,numpts
            call dbgent (dbuff,3,dipg,diel)
            call ppnptt (dbuff,pntyp,ipnkey,i,trans)
            diel=diel+3
            if (diel.gt.35) then
                diel=diel-35
                dipg=dipg+1
            endif
375   continue
      go to 995

c          curve - read in curve header
370   continue
      call dbgent (dbbuf,1,dipg,diel)
      rktv(3)=12
      if (dbver.lt.dbver1) then
        iclos=0
        numseg=rdbbuf(1)
      else
        iclos=1
        numseg=idbbuf(1)
      endif
      iwds=(numseg+1)/2+iclos
      call dbgent (dbbuf,iwds,dipg,diel)
      mval=6
c          bump pointer to 1st curve segment
      diel=diel+iwds
      iwds=iwds+1
      do 600 i=1,numseg
      if (diel.gt.35) then
        diel=diel-35
        dipg=dipg+1
      endif
      call dbgent (dbbuf(iwds),mval,dipg,diel)
      if (i.eq.numseg) go to 600
      iwds=iwds+mval
      diel=diel+mval
600   continue
      if (iclos.eq.1) rdbbuf(3)=idbbuf(1)
      call ptentt(dist,dbbuf(iclos+1),i4rest(1),tv)
      if (iclos.eq.1.and.idbbuf(2).eq.1) then
        call gtdesc(tv,nclkey,nwds,ietype)
        call ptclsd(nclkey,0,1)
      endif
c      goto 900
900   continue
995   copied=.true.
      ifl(9)=ifl(11)
      ifl(10)=ifl(12)
      call vstore
c
c      if (ifl(35).eq.0) call dspent(i4rest(1),ist,rktv(3))
c.....
c.....Changed for NCL501+. Paul. 06/22/93
c.....Previous version: if (ifl(35).eq.0) call dspent(i4rest(1),ist)
c.....
      if (ifl(35).eq.0 .or. ifl(35).eq.2.and.ifl(350).eq.1)
     x call dspent(i4rest(1),ist)

99999 return
      end
