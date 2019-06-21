c
c***********************************************************************
c
c   FILE NAME: gincip.for
c   CONTAINS:
c               gencip  genci1  genci2  gencdr  chkcir  outcir
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        gencip.f , 25.3
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 08:37:36
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  gencip (gcir,gpas,gplon,gpltan,gind,gpts,kmul,kseq,
c                        krec,kcpt,gtol,kerr)
c
c   FUNCTION:  This routine creates points on a circular arc using
c              APT source circular motion syntax.
c
c   INPUT:  gcir    R*8  D7  Circle canonical data.
c
c           gpas    R*8  D4  Part SF plane definition.
c
c           gplon   R*8  D4  Check plane definition.
c
c           gpltan  R*8  D4  Tanto check plane definition when 'kion' = -1.
c
c           gind    R*8  D3  Indirection vector definition.
c
c           gpts    R*8  D6  Start point & tool vector.
c
c           kion    I*4  D1  Which intersection number.  -1 = Tanto.
c
c           kmul    I*4  D1  Multax flag.
c
c           kseq    I*4  D1  Sequence number of motion statement.
c
c           krec    I*4  D1  Output Clfile record number.
c
c           kcpt    I*4  D1  Output Clfile record pointer.
c
c           gtol    R*8  D1  Tolerance for selecting close end points.
c
c   OUTPUT: kerr    I*4  D1  Returns internal error number if any.
c
c***********************************************************************
c
      subroutine gencip (gcir,gpas,gplon,gpltan,gind,gpts,kion,kmul,
     1                   kseq,krec,kcpt,gtol,kerr)
c
      include 'post.inc'
      include 'menu.inc'
      include 'clnrd.inc'
c
      real*8 gcir(7),gpas(4),gplon(4),gind(3),gpts(6),gpltan(4),gtol
c
      integer*4 kmul,kseq,kerr,krec,kcpt,kion
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
c
      real*8 RAD,PI
c
      real*8 rmat(4,3),rv(3),an(2),rd,a,b,d,cent(3),pts(3),as,
     -       plon(4),div(3),psis(4),pte(3),pt(6),ptz(3),vec(3),
     -       zax(3),perp(4),v(4,3),pin(6),pe(6),ae,dis,rdis,
     -       ang,adel,rnum,toler,robuf(240),svd(2)
c
      equivalence (v(1,1),plon), (v(1,2),psis), (v(1,3),perp)
c
      character*80 cmsg
c
      integer*4 i,j,npt,idir,nus,jbuf(481),icr,icpt,nwds,ierr
      integer*2 inbuf(964),mxc
c
      equivalence (inbuf(5),robuf), (inbuf(1),mxc,jbuf)
c
      data zax /0,0,1.0/, ptz /0,0,0/, toler /.001/
c
      kerr   = 0
      icr    = krec
      icpt   = kcpt
c
c...Check if circle plane is perpto tool axis
c
      a      = gpts(4)*gcir(4) + gpts(5)*gcir(5) + gpts(6)*gcir(6)
      if (dabs(a) .lt. .9999) go to 9100
c
c...Get conversion matrix
c
      d      = dsqrt (gcir(4)**2 + gcir(5)**2 + gcir(6)**2)
      gcir(4) = gcir(4) / d
      gcir(5) = gcir(5) / d
      gcir(6) = gcir(6) / d
      call gtpola (gcir(4),an,rmat)
c
c...Convert circular motion data to circular plane
c
      call ptmatr (gpts,pts,rmat,2)
      call ptmatr (gplon,plon,rmat,2)
      plon(4) = gplon(4)
      call ptmatr (gcir,cent,rmat,2)
      d      = dsqrt (gind(1)**2 + gind(2)**2 + gind(3)**2)
      if (d .lt. 1.d-6) go to 9200
      gind(1) = gind(1) / d
      gind(2) = gind(2) / d
      gind(3) = gind(3) / d
      call ptmatr (gind,div,rmat,2)
c
c...Set part plane
c
      psis(1) = 0.d0
      psis(2) = 0.d0
      psis(3) = 1.d0
      psis(4) = 0.0 - pts(3)
      cent(3) = pts(3)
c
c...Get actual circle radius
c...and starting point angle
c
      rd      = dsqrt((pts(1)-cent(1))**2 + (pts(2)-cent(2))**2)
      if (rd .lt. 1.d-6) go to 9300
      vec(1)  = (pts(1) - cent(1)) / rd
      vec(2)  = (pts(2) - cent(2)) / rd
      vec(3)  = 0.d0
      call vecang (vec,3,as)
c
c...Get positive direction vector (perp to radius)
c
      rv(1) = cent(2) - pts(2)
      rv(2) = pts(1) - cent(1)
      d     = dsqrt(rv(1)**2 + rv(2)**2)
      if (d .eq. 0.d0) go to 9200
      rv(1) = rv(1) / d
      rv(2) = rv(2) / d
      rv(3) =  0.d0
c
c...Get direction of circular motion
c
      a     = rv(1) * div(1) + rv(2) * div(2)
      idir  = 1
      if (a .lt. 0.d0) idir = 0 - 1
c
c...Get plane perpto check plane, || to Z-axis
c...& thru circul center point
c...then intersection point of 3 planes
c
      call plnlnp (plon,zax,cent,perp,kerr)
      call determ (v,0,rnum)
      if (dabs(rnum) .lt. 1.d-6) go to 9400
      rnum   = 0.d0 - rnum
      call determ (v,1,pin(1))
      call determ (v,2,pin(2))
      call determ (v,3,pin(3))
      pin(1) = pin(1) / rnum
      pin(2) = pin(2) / rnum
      pin(3) = pin(3) / rnum
c
c...Get IO line of psis & plon (check plane) thru point
c
      pin(4) = psis(2) * plon(3) - psis(3) * plon(2)
      pin(5) = psis(3) * plon(1) - psis(1) * plon(3)
      pin(6) = 0.d0
c
c...Get dist from CI center to IO line
c...and intersection points on circle
c
      rnum   = dsqrt ((pin(1)-cent(1))**2 + (pin(2)-cent(2))**2)
      if (rnum-rd .ge. .001) go to 9400
      if (rnum .gt. rd) rnum = rd
cc      pe(1) = pin(1)
cc      pe(2) = pin(2)
cc      pe(4) = pin(4)
cc      pe(5) = pin(5)
        b      = rnum / rd
        b      = rd * dsqrt (1.d0 - b**2)
        pe(1)  = pin(1) + b * pin(4)
        pe(2)  = pin(2) + b * pin(5)
        pe(4)  = pin(1) - b * pin(4)
        pe(5)  = pin(2) - b * pin(5)
c
c...Select the closest point at defined direction
c
      ang    = 400.0
      rnum   = gtol * RAD / rd
      rdis   = 100000.
      do 305 i=0,3,3
         vec(1) = (pe(1+i) - cent(1)) / rd
         vec(2) = (pe(2+i) - cent(2)) / rd
         call vecang (vec,3,ae)
         adel   = (ae - as) * idir
         if (adel .lt. 0.0) adel = 360.0 + adel
c
c...For second intersection, if delta angle is very small
c...assume 360 degrees. IJD 30-Nov-98
c
         if ((kion .eq. 2 .or. kion .eq. -1) .and. adel .le. rnum)
     1       adel = 360.0
         svd(i/3+1) = adel
         if (kion .eq. -1) then
             call plndis (gpltan,pe(1+i),dis)
             dis = dabs(dis)
             if (dis .lt. rdis) then
                 rdis = dis
                 ang = adel
                 nus = i
             endif
         else
             if (adel .gt. rnum .and. adel .lt. ang) then
                 ang  = adel
                 nus  = i
             endif
         endif
  305 continue
c
c...Adjust if second IO is selected
c
      if (kion .eq. 2) nus = 3 - nus
      adel   = svd(nus/3+1)
      ang    = adel
c
      pte(1) = pe(1+nus)
      pte(2) = pe(2+nus)
      pte(3) = pts(3)
c
c...Get number of points to create
c
      rnum   = (rd - toler) / rd
      if (rnum .lt. 0.0) rnum = 0.d0
      d      = 2. * dacos(rnum) * RAD
      npt    = adel / d + 1
      if (npt .lt. 3) npt = 3
      ang    = ang / npt * idir
c
c...First output Circul record to cl file
c
      mxc    = 7
      inbuf(2) = kseq
      inbuf(3) = 3000
      inbuf(4) = 5
      do 355 i=1,7
          robuf(i) = gcir(i)
  355 continue
      call wrclrc (jbuf,mxc,icr,icpt,cmsg,ierr)
c
c...Create points on circle
c
      vec(3) = pts(3)
      i      = 0
      mxc    = 0
      nwds   = (kmul + 1) * 3
      pt(4)  = gpts(4)
      pt(5)  = gpts(5)
      pt(6)  = gpts(6)
      inbuf(2) = kseq
      inbuf(3) = 5000
      inbuf(4) = 5
c
  500 i      = i + 1
      if (i .lt. npt) then
          a      = (as + ang * i) / RAD
          vec(1) = cent(1) + rd * dcos(a)
          vec(2) = cent(2) + rd * dsin(a)
          call ptmatb (vec,pt,rmat,2)
      else
          call ptmatb (pte,pt,rmat,2)
      end if
      do 505 j=1,nwds,1
          mxc    = mxc + 1
          robuf(mxc) = pt(j)
  505 continue
      if (mxc .eq. 240) then
         call wrclrc (jbuf,mxc,icr,icpt,cmsg,ierr)
         if (ierr .ne. 0) go to 8000
         inbuf(4) = 6
         mxc   = 0
      end if
      if (i .lt. npt) go to 500
      if (mxc .ne. 0) call wrclrc (jbuf,mxc,icr,icpt,cmsg,ierr)
c
      do 515 j=1,nwds
         gpts(j) = pt(j)
  515 continue
      krec   = icr
      kcpt   = icpt
c
c...End of routine
c
 8000 return
c
c...Error in circul
c
 9100 kerr   = 6
      go to 8000
 9200 kerr   = 4
      go to 8000
 9300 kerr   = 5
      go to 8000
 9400 kerr   = 2
      go to 8000
c
      end
c
c***********************************************************************
c
c   SUBROUTINE:  genci1 (gcir,gpts,gpt,gang,kmul,kseq,krec,kcpt,kerr)
c
c   FUNCTION:  This routine creates points on a circular arc using
c              APT source ARCDAT & ARCMOV syntax.
c
c   INPUT:  gcir    R*8  D7  Circle canonical data.
c
c           gpts    R*8  D6  Start point & tool vector.
c
c           gpt     R*8  D3  End point.
c
c           gang    R*8  D1  Arc angle with sign (CCLW = +).
c
c           kmul    I*4  D1  Multax flag.
c
c           kseq    I*4  D1  Sequence number of motion statement.
c
c           krec    I*4  D1  Output Clfile record number.
c
c           kcpt    I*4  D1  Output Clfile record pointer.
c
c   OUTPUT: kerr    I*4  D1  Returns internal error number if any.
c
c***********************************************************************
c
      subroutine genci1 (gcir,gpts,gpt,gang,kmul,kseq,krec,kcpt,kerr)
c
      include 'post.inc'
      include 'menu.inc'
      include 'clnrd.inc'
c
      real*8 gcir(7),gpts(6),gang,gpt(3)
      integer*4 kmul,kseq,kerr,krec,kcpt
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
c
      real*8 RAD,PI
c
      real*8 rmat(4,3),an(2),rd,a,d,cent(3),pts(3),as,pte(3),pt(6),
     -       lpt(3),vec(3),ang,rnum,aend,robuf(240),ndot,ndist
c
      character*80 cmsg
c
      integer*4 i,j,npt,jbuf(481),icr,icpt,nwds,ierr
      integer*2 inbuf(964),mxc
c
      equivalence (inbuf(5),robuf), (inbuf(1),mxc,jbuf)
c
      kerr   = 0
      icr    = krec
      icpt   = kcpt
c
c...Check if circle plane is perpto tool axis
c
      a      = ndot (gcir(4),gpts(4))
      if (dabs(a) .lt. .9999) go to 9100
c
c...Get conversion matrix
c
      d      = dsqrt (a)
      gcir(4) = gcir(4) / d
      gcir(5) = gcir(5) / d
      gcir(6) = gcir(6) / d
      call gtpola (gcir(4),an,rmat)
c
c...Convert circular motion data to circular plane
c
      call ptmatr (gpts,pts,rmat,2)
      call ptmatr (gcir,cent,rmat,2)
      call ptmatr (gpt,lpt,rmat,2)
c
c...Set part plane
c
      cent(3) = pts(3)
c
c...Get actual circle radius
c...and starting point angle
c
      rd      = dsqrt((pts(1)-cent(1))**2 + (pts(2)-cent(2))**2)
      if (rd .lt. 1.d-3) go to 9300
      vec(1)  = (pts(1) - cent(1)) / rd
      vec(2)  = (pts(2) - cent(2)) / rd
      vec(3)  = 0.d0
      call vecang (vec,3,as)
c
c...Get end point
c
      aend   = as + gang
      pte(1) = cent(1) + rd * dcos(aend/RAD)
      pte(2) = cent(2) + rd * dsin(aend/RAD)
      pte(3) = pts(3)
c
c...End point should match point in ARCMOV statement, unless
c...direction modifiers (CLW,CCLW) are meaningless. Need other
c...example of source file to tune it (vp931116).
c
c...Set tolerance for difference between calculated end point
c...and given endpoint to be 0.05 mm.  We are assuming
c...metric.  This will allow some bad points in standard
c...inches to slip by, but there is no way to know at this
c...stage in the processing if the numbers are metric or
c...English (inches).  (Ed Ames 2 May 2002)
c
      rnum   = ndist (pte,lpt)
      if (rnum .gt. 0.05) go to 9400
c
c...Get number of points to create
c
      rnum   = (rd - .001) / rd
      if (rnum .lt. 0.0) rnum = 0.d0
      d      = 2. * dacos(rnum) * RAD
      npt    = gang / d + 1
      if (npt .lt. 0) npt = 0 - npt
      if (npt .lt. 3) npt = 3
      ang    = gang / npt
c
c...Set GOTO record data
c
      vec(3) = pts(3)
      i      = 0
      mxc    = 0
      nwds   = (kmul + 1) * 3
      pt(4)  = gpts(4)
      pt(5)  = gpts(5)
      pt(6)  = gpts(6)
      inbuf(2) = kseq
      inbuf(3) = 5000
      inbuf(4) = 5
c
c...Create points on circle
c
  500 i      = i + 1
      if (i .lt. npt) then
          a      = (as + ang * i) / RAD
          vec(1) = cent(1) + rd * dcos(a)
          vec(2) = cent(2) + rd * dsin(a)
          call ptmatb (vec,pt,rmat,2)
      else
          call ptmatb (pte,pt,rmat,2)
      end if
      do 505 j=1,nwds,1
          mxc    = mxc + 1
          robuf(mxc) = pt(j)
  505 continue
c
c...Store record
c
      if (mxc .eq. 240) then
         call wrclrc (jbuf,mxc,icr,icpt,cmsg,ierr)
         if (ierr .ne. 0) go to 8000
         inbuf(4) = 6
         mxc   = 0
      end if
      if (i .lt. npt) go to 500
      if (mxc .ne. 0) call wrclrc (jbuf,mxc,icr,icpt,cmsg,ierr)
c
c...Save end point
c
      do 515 j=1,nwds
         gpts(j) = pt(j)
  515 continue
      krec   = icr
      kcpt   = icpt
c
c...End of routine
c
 8000 return
c
c...Error in circul
c
 9100 kerr   = 6
      go to 8000
 9200 kerr   = 4
      go to 8000
 9300 kerr   = 5
      go to 8000
 9400 kerr   = 2
      go to 8000
c
      end
c
c***********************************************************************
c
c   SUBROUTINE:  genci2 (gcir,gpts,gpt,gang,kmul,kseq,krec,kcpt,ktim,kerr)
c
c   FUNCTION:  This routine creates points on a circular arc using
c              APT source CW or CCW statement syntax.
c
c   INPUT:  gcir    R*8  D7  Circle canonical data.
c
c           gpts    R*8  D6  Start point & tool vector.
c
c           gpt     R*8  D3  End point.
c
c           gang    R*8  D1  Arc angle sign (CCLW = +1).
c
c           kmul    I*4  D1  Multax flag.
c
c           kseq    I*4  D1  Sequence number of motion statement.
c
c           krec    I*4  D1  Output Clfile record number.
c
c           kcpt    I*4  D1  Output Clfile record pointer.
c
c           ktim    I*4  D1  Number of full circles to output prior to the
c                            final arc.
c
c   OUTPUT: kerr    I*4  D1  Returns internal error number if any.
c
c***********************************************************************
c
      subroutine genci2 (gcir,gpts,gpt,gang,kmul,kseq,krec,kcpt,ktim,
     1                   kerr)
c
      include 'post.inc'
      include 'menu.inc'
      include 'clnrd.inc'
c
      real*8 gcir(7),gpts(6),gang,gpt(3)
      integer*4 kmul,kseq,kerr,krec,kcpt,ktim
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
c
      real*8 RAD,PI
c
      real*8 rmat(4,3),an(2),rd,a,d,cent(3),pts(3),as,pte(3),pt(6),
     -       lpt(3),ang,aend,robuf(240),ndot
      real*8 vecl(3),vec(3),rnum,tvec(3)
c
      character*80 msg
c
      integer*4 i,j,npt,jbuf(481),icr,icpt,nwds,ierr,ihelx,is1,is4
      integer*2 inbuf(964),iobuf(960),mxc
c
      equivalence (inbuf(5),robuf,iobuf), (inbuf(1),mxc,jbuf)
c
C VAX-SUN-SGI-IBM-HPX-START
C      data is1 /0/, is4 /3/
C VAX-SUN-SGI-IBM-HPX-END
C WNT-DOS-DEC-START
      data is1 /3/, is4 /0/
C WNT-DOS-DEC-END
c
      kerr   = 0
      icr    = krec
      icpt   = kcpt
c
c...Check if circle plane is perpto tool axis
c
      tvec(1) = gpts(4)
      tvec(2) = gpts(5)
      tvec(3) = gpts(6)
      a      = ndot (gcir(4),gpts(4))
      if (dabs(a) .lt. .9999) then
          tvec(1) = gcir(4)
          tvec(2) = gcir(5)
          tvec(3) = gcir(6)
          inc    = 1
          if (dabs(tvec(2)) .gt. dabs(tvec(inc))) inc = 2
          if (dabs(tvec(3)) .gt. dabs(tvec(inc))) inc = 3
          if (tvec(inc) .lt. 0.) then
              tvec(1) = tvec(1) * -1.
              tvec(2) = tvec(2) * -1.
              tvec(3) = tvec(3) * -1.
          endif
      endif
c
c...Get conversion matrix
c
cc      d      = dsqrt (gcir(4)**2 + gcir(5)**2 + gcir(6)**2)
cc      gcir(4) = gcir(4) / d
cc      gcir(5) = gcir(5) / d
cc      gcir(6) = gcir(6) / d
      call gtpola (tvec,an,rmat)
c
c...Convert circular motion data to circular plane
c
      call ptmatr (gpts,pts,rmat,2)
      call ptmatr (gcir,cent,rmat,2)
      call ptmatr (gpt,lpt,rmat,2)
c
c...Set part plane
c
      cent(3) = pts(3)
c
c...Get actual circle radius
c...and starting point angle
c
      rd      = dsqrt((pts(1)-cent(1))**2 + (pts(2)-cent(2))**2)
      if (rd .lt. 5.d-6) go to 9300
      vec(1)  = (pts(1) - cent(1)) / rd
      vec(2)  = (pts(2) - cent(2)) / rd
      vec(3)  = 0.d0
      call vecang (vec,3,as)
c
      vecl(1)  = (lpt(1) - cent(1)) / rd
      vecl(2)  = (lpt(2) - cent(2)) / rd
      call vecang (vecl,3,aend)
      ang    = aend - as
      if (ang .lt. 0.) ang = 360. + ang
      if (gang .lt. 0.) ang = ang - 360.
      call dpoint (ang,ang,5)
      if (ang .eq. 0.) ang = ang + (360.*gang)
c
c...Calculate number of rotations
c...based on pitch if 'ktim' = -1
c
      if (ktim .eq. -1) then
          d =  dabs(lpt(3)-pts(3)) - .001
          ktim = d / gpt(4)
      endif
      ang   = ang   + (360.*ktim*gang)
c
c...Determine if end point is on circular plane
c...If not, then its helical and a COUPLE command
c...needs to be output
c
      ihelx  = 0
      if (dabs(lpt(3)-pts(3)) .gt. .0001) then
          ihelx  = 1
          inbuf(2) = kseq
          inbuf(3) = 2000
          inbuf(4) = 1049
          robuf(1) = -(lpt(3)-pts(3))
          iobuf(2*4-is4) = 0
          iobuf(2*4-is1) = 1
          robuf(3) = 0.
          mxc   = 3
          call wrclrc (jbuf,mxc,icr,icpt,msg,kerr)
          if (kerr .ne. 0) go to 9400
      endif
c
c...Output circular record
c
      call outcir (gcir,kseq,icr,icpt,msg,kerr)
c
c...Get end point
c
      call copyn (lpt,pte,3)
      if (ihelx .eq. 1) pte(3) = pts(3)
c
c...Get number of points to create
c
      rnum   = (rd - .001) / rd
      if (rnum .lt. 0.0) rnum = 0.d0
      d      = 2. * dacos(rnum) * RAD
      npt    = ang / d + 1
      if (npt .lt. 0) npt = 0 - npt
      if (rd .lt. 5.d-4 .and. npt .lt. 2) then
          npt = 2
      else if (npt .lt. 3) then
          npt = 3
      endif
      ang    = ang / npt
c
c...Set GOTO record data
c
      vec(3) = pts(3)
      i      = 0
      mxc    = 0
      nwds   = (kmul + 1) * 3
      pt(4)  = gpts(4)
      pt(5)  = gpts(5)
      pt(6)  = gpts(6)
      inbuf(2) = kseq
      inbuf(3) = 5000
      inbuf(4) = 5
c
c...Create points on circle
c
  500 i      = i + 1
      if (i .lt. npt) then
          a      = (as + ang * i) / RAD
          vec(1) = cent(1) + rd * dcos(a)
          vec(2) = cent(2) + rd * dsin(a)
          call ptmatb (vec,pt,rmat,2)
      else
          call ptmatb (pte,pt,rmat,2)
      end if
      do 505 j=1,nwds,1
          mxc    = mxc + 1
          robuf(mxc) = pt(j)
  505 continue
c
c...Store record
c
      if (mxc .eq. 240) then
         call wrclrc (jbuf,mxc,icr,icpt,msg,ierr)
         if (ierr .ne. 0) go to 9400
         inbuf(4) = 6
         mxc   = 0
      end if
      if (i .lt. npt) go to 500
      if (mxc .ne. 0) call wrclrc (jbuf,mxc,icr,icpt,msg,ierr)
      if (ierr .ne. 0) go to 9400
c
c...Helical interpolation is in effect
c...Output final point
c
      if (ihelx .eq. 1) then
          inbuf(4) = 5
          mxc    = nwds
          call copyn (gpt,pt,3)
          pt(4)  = gpts(4)
          pt(5)  = gpts(5)
          pt(6)  = gpts(6)
          call copyn (pt,robuf,nwds)
          call wrclrc (jbuf,mxc,icr,icpt,msg,ierr)
          if (ierr .ne. 0) go to 9400
      endif
c
c...Save end point
c
      do 515 j=1,nwds
         gpts(j) = pt(j)
  515 continue
      krec   = icr
      kcpt   = icpt
c
c...End of routine
c
 8000 return
c
c...Error in circul
c
 9100 kerr   = 6
      go to 8000
 9200 kerr   = 4
      go to 8000
 9300 kerr   = 5
      go to 8000
 9400 kerr   = 2
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gencdr (gcir,gpts,gvec,gdir)
c
c   FUNCTION:  This routine calculates the direction of the circular arc
c              based on the starting point/axis, circle center/axis, and
c              initial directional vector.
c
c   INPUT:  gcir    R*8  D7  Circle canonical data.
c
c           gpts    R*8  D6  Start point & tool vector.
c
c           gvec    R*8  D3  Tangent vector at start of circle.
c
c   OUTPUT: gdir    R*8  D1  Arc angle sign (CCLW = +1).
c
c***********************************************************************
c
      subroutine gencdr (gcir,gpts,gvec,gdir)
c
      real*8 gcir(7),gpts(6),gvec(3),gdir
c
      real*8 pts(3),cent(3),div(3),rv(3),rmat(12),an(2)
c
c...Get conversion matrix
c
      call unitvc (gpts(4),rv)
      call gtpola (rv,an,rmat)
c
c...Convert circular motion data to circular plane
c
      call ptmatr (gpts,pts,rmat,2)
      call ptmatr (gcir,cent,rmat,2)
      call ptmatr (gvec,div,rmat,2)
      call unitvc (div,div)
c
c...Get positive direction vector (perp to radius)
c
      rv(1) = cent(2) - pts(2)
      rv(2) = pts(1) - cent(1)
      rv(3) =  0.d0
      call unitvc (rv,rv)
c
c...Get direction of circular motion
c
      gdir   = rv(1) * div(1) + rv(2) * div(2)
      gdir   = gdir / dabs(gdir)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  chkcir (gpts,gcir,gccen,grad,kerr)
c
c   FUNCTION:  This routine checks if point is on the circle arc
c              and inthe circle plane.
c
c   INPUT:  gpts    R*8  D3  Point to check
c
c           gcir    R*8  D7  Circle canonical data.
c
c           gccen   R*8  D3  Circle center point in circle plane
c
c           grad    R*8  D3  Circle true radius (at the start point)
c
c
c   OUTPUT: kerr    I*4  D1  Returns 1 if point is not in tolerance.
c
c***********************************************************************
      subroutine chkcir (gpts,gcir,gccen,grad,kerr)
c
      include 'post.inc'
c
      real*8 gpts(3), gcir(7), gccen(3), grad
      integer*4 kerr
c
c     equivalence (PPTOLR,POSMAP(1274)), (CIRTOL,POSMAP(2201))
c
c     real*8 CIRTOL(5), PPTOLR(3)
c
      real*8 pt(3), d, ndist
c
      kerr  = 0
      d     = ndist (gpts,gccen)
      if (dabs(d-grad) .gt. .005) go to 9000
      call nptln (gpts,gcir,pt)
      if (ndist(pt,gccen) .lt. .001) go to 8000
c
 9000 kerr  = 1
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  outcir (gcir,kseq,krec,kcpt,cmsg,kerr)
c
c   FUNCTION:  This routine outputs circular record to cl file.
c
c   INPUT:  gcir    R*8  D7  Circle canonical data.
c
c           kseq    I*4  D1  Sequence number of motion statement.
c
c           krec    I*4  D1  Output Clfile record number.
c
c           kcpt    I*4  D1  Output Clfile record pointer.
c
c   OUTPUT: kerr    I*4  D1  Returns internal error number if any.
c
c           cmsg    C*n  D1  Text of error message.
c
c***********************************************************************
c
      subroutine outcir (gcir,kseq,krec,kcpt,cmsg,kerr)
c
      include 'post.inc'
      include 'menu.inc'
      include 'clnrd.inc'
c
      real*8 gcir(7)
      integer*4 kseq,kerr,krec,kcpt
      character*80 cmsg
c
      real*8 robuf(240)
c
      integer*4 jbuf(481),icr,icpt
      integer*2 inbuf(964),mxc
c
      equivalence (inbuf(5),robuf), (inbuf(1),mxc,jbuf)
c
      icr    = krec
      icpt   = kcpt
c
c...create circular CL record
c
      mxc    = 7
      inbuf(2) = kseq
      inbuf(3) = 3000
      inbuf(4) = 5
      call copyn (gcir,robuf(1),7)
      call wrclrc (jbuf,mxc,icr,icpt,cmsg,kerr)
      if (kerr .eq. 0) then
         krec = icr
         kcpt = icpt
      end if
c
      return
      end
