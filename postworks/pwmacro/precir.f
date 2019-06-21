c
c***********************************************************************
c
c   FILE NAME: precir.f
c   CONTAINS:
c             pre_circk  pre_cirdel  pre_cirdir
c             cirpar  cir3pt
c
c     COPYRIGHT 1998 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        precir.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:52
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  pre_circk (ist,iend,ptl,gpcl,cirr,ifl)
c
c   FUNCTION:  This routine processes (CLPT) points in a single record
c              to determine if points are on a circular arc.
c
c   INPUT:  ist     I*4  D1  -  First point index in gpcl array
c
c           iend    I*4  D1  -  Last point index in gpcl array
c
c           ptl     R*8  D6  -  Previous point (start point)
c
c           gpcl    R*8  D6  -  Array of points (CLPT)
c
c   OUTPUT: cirr    R*8  D7  -  Circle record data
c
c           ifl     I*4  D1  -  Returns 1 when circle is created.
c
c***********************************************************************
c
      subroutine pre_circk (ist,iend,ptl,gpcl,cirr,ifl)
c
      include 'post.inc'
      integer*4 ist,iend,ifl
      real*8 ptl(6),gpcl(240),cirr(7)
c
      equivalence (NPT   ,KPOSMP(0059))
c
      integer*4 NPT
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
      equivalence (VECSAV,POSMAP(1372))
      equivalence (CIRTOS,POSMAP(2241))
c
      real*8 PI,RAD,CIRTOS,VECSAV(3)
c
      real*8 ndot,amax,ptm(3),pte(3),anst,aarc,ang1,rmat(4,3),
     1       dan,ts(3),tl(3),qnum,pts(3),pta(3),an(2), temp,dansv,
     2       a(3), b(3), c(3), nothing,rang
c
      integer*4 i,imid,ier
c         character*80 cmsg
c
      ifl   = 0
      do 100 i=1,3,1
          cirr(i+3) = 0.0
          ts(i) = ptl(i+3)
  100 continue
c
c...Check for tool vector & 3-axis move
c
      do 230 i=ist,iend
c
c...Check for tool vector change
c
         if (NPT .gt. 3) then
            if (ndot (ts,gpcl(i*NPT-2)) .lt. .999999d0) go to 8000
         end if
c
c...Check for 3-axis move
c
         call vcplvc (gpcl((i-1)*NPT+1),ptl,pta,-1.d0)
         call unitvc (pta,pta)
c         if (ndot (ts,pta) .gt. 1.d-5) go to 8000
          temp = ndot (ts,pta)
  230 continue
c
c...get intermediate point on arc
c
      imid   = ist   + (iend-ist+1) / 2
      if (imid .eq. 1) imid = 2
      if (NPT .eq. 3) then
         call copyn (VECSAV,tl,3)
      else
         call copyn (gpcl(imid*NPT-2),tl,3)
      end if
c
c...Get vector normal to the plane of the circle.
c... a = vector from starting point to midpoint of arc.
c... b = vector from starting point to endpoint of arc.
c... c = vector normal to plane of circle
c
      call vcmnvc(gpcl((imid-1)*NPT + 1) , gpcl((ist-1)*NPT+1), a)
      call vcmnvc(gpcl((iend-1)*NPT + 1) , gpcl((ist-1)*NPT+1), b)
      call crosvc(a,b,c)
      call unitvc(c,c)
cc      call unitvc(a,a)
cc      call unitvc(b,b)
cc      call getlan(a, b, nothing, nothing, nothing, nothing, nothing, c)
c
c...get circle plane transformation and translate all points
c
      call gtpola (c,an,rmat)
      call ptmatr (ptl,pts,rmat,2)
      call ptmatr (gpcl((ist-1)*NPT+1),pta,rmat,2)
      call ptmatr (gpcl((imid-1)*NPT+1),ptm,rmat,2)
      call ptmatr (gpcl((iend-1)*NPT+1),pte,rmat,2)
c
c...Get circle parameters
c
      call cirpar (pts,pta,ptm,pte,cirr,anst,aarc,ier)
      if (ier .eq. 1) go to 8000
c
c...Set circle axis
c
      call betvec (tl,c,rang)
      if ((aarc .ge. 0. .and. rang .gt. 90.) .or.
     1    (aarc .lt. 0. .and. rang .lt. 90.)) then
          call vctmsc (c,-1.d0,cirr(4))
      else
          call copyn (c,cirr(4),3)
      endif
c
c...get maximum angular move for tolerance
c
      amax   = dabs ((cirr(7) - CIRTOS) / cirr(7))
      amax   = dacos (amax) * RAD * 2.
      if (CIRTOS .GT. cirr(7)) amax = 360.d0
      if (cirr(7) .gt. .9999999999d5) go to 8000
c
c...check points to radius and chord tolerance
c
      do 1000 i=ist,iend,1
          call ptmatr (gpcl((i-1)*NPT+1),ptm,rmat,2)
          call pre_cirdel (pts,ptm,ptm,cirr,ang1,dan)
          qnum   = dsqrt ((ptm(1) - cirr(1))**2 +
     1                    (ptm(2) - cirr(2))**2 +
     2                    (ptm(3) - cirr(3))**2)
          if (dabs(cirr(7)-qnum) .gt. CIRTOS) go to 8000
c
c......check chord height
c
          if (dabs(dan) .gt. amax) go to 8000
          call copyn (ptm,pts,3)
c
c......Make sure direction doesn't change
c
          if (i .eq. ist) then
              dansv = dan
          else
              if ((dan .lt. 0. .and. dansv .gt. 0) .or.
     1            (dan .gt. 0. .and. dansv .lt. 0)) go to 8000
          endif
 1000 continue
c
c...revers circul center back to original plane
c
      call ptmatb (cirr,cirr,rmat,2)
      ifl    = 1
c
c...invalid circle
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirpar (gpt1,gpt2,gptm,gptn,gcir,gangs,gdelt,kerr)
c
c   FUNCTION:  This routine defines the circle thru 3 points and sets
c              all circular parameters in circle plane.
c
c   INPUT:  gpt1    R*8  D3  -  starting point of circle.
c
c           gpt2    R*8  D3  -  second point of circle.
c
c           gptm    R*8  D3  -  intermediate point of circle.
c
c           gptn    R*8  D3  -  the last point of circle.
c
c   OUTPUT: gcir    R*8  D3  -  Circle canonical data.
c
c           gangs   R*8  D1  -  angle of the first point
c
c           gdelt   R*8  D1  -  total arc angle
c
c           kerr    I*4  D1  -  Error status (0 = circle OK)
c
c***********************************************************************
c
      subroutine cirpar (gpt1,gpt2,gptm,gptn,gcir,gangs,gdelt,
     -                   kerr)
c
      real*8 gpt1(3),gpt2(3),gptn(3),gptm(3),gcir(7),gangs,gdelt
      integer*4 kerr
c
      real*8 rdus,ndist
c
      integer*4 idir
c
      kerr = 0
c
c...Get circular parameters
c
      if (ndist(gpt1,gptn) .gt. .0001) then
          call cir3pt (gpt1,gptm,gptn,rdus,gcir,kerr)
      else
          call cir3pt (gpt2,gptm,gptn,rdus,gcir,kerr)
      end if
      if (kerr .ne. 0) go to 9000
      gcir(7) = rdus
c
c...define circular direction & starting angle
c
      call pre_cirdir (gpt1,gpt2,gcir,idir)
      call pre_cirdel (gpt1,gpt2,gptn,gcir,gangs,gdelt)
c
 9000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pre_cirdel (gpt1,gpt2,gptn,gcir,gangs,gdelt)
c
c   FUNCTION:  This routine defines starting angle of the circle lacated
c              in XY plane and delta angle of its arc.
c
c   INPUT:  gpt1    R*8  D3  -  starting point of circle.
c
c           gpt2    R*8  D3  -  second point of circle.
c
c           gptn    R*8  D3  -  the last point of circle.
c
c           gcir    R*8  D3  -  center of circul
c
c   OUTPUT: gangs   R*8  D1  -  angle of the first point
c
c           gdelt   R*8  D1  -  total arc angle
c
c***********************************************************************
c
      subroutine pre_cirdel (gpt1,gpt2,gptn,gcir,gangs,gdelt)
c
      include 'post.inc'
      real*8 gpt1(3),gpt2(3),gptn(3),gcir(7),gangs,gdelt
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
c
      real*8 RAD,PI
c
      real*8 a,b,rdus,cosa,cosb,one,fula,nula
c
      integer*4 idir
c
      data one /1.d0/, fula /360.d0/, nula /0.d0/
c
      rdus   = gcir(7)
c
c...define circular direction & starting angle
c
      call pre_cirdir (gpt1,gpt2,gcir,idir)
      a      = gpt1(1) - gcir(1)
      b      = gpt1(2) - gcir(2)
      cosa   = a / rdus
      if (cosa .lt. -one) cosa = -one
      if (cosa .gt.  one) cosa =  one
      cosa   = dacos (cosa) * rad
      if (b .lt. nula) cosa = fula - cosa
      gangs  = cosa
c
c...Get end point angle
c
      a      = gptn(1) - gcir(1)
      b      = gptn(2) - gcir(2)
      cosb   = a / rdus
      if (cosb .lt. -one) cosb = -one
      if (cosb .gt.  one) cosb =  one
      cosb   = dacos (cosb) * rad
      if (b .lt. nula) cosb = fula - cosb
c
c...Get delta angle (negative - CLW, positive - CCLW)
c
      if (idir .eq. 2) then
          gdelt = cosb - cosa
          if (gdelt .lt. nula) gdelt = fula + gdelt
      else
          gdelt = cosa - cosb
          if (gdelt .lt. nula) gdelt = fula + gdelt
          gdelt = -gdelt
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cir3pt (gpt1,gpt2,gpt3,grad,gcir,kerr)
c
c   FUNCTION:  This routine defines circle thru 3 points (from ncl/model)
c
c   INPUT:  gpt1    R*8  D3  -  starting point of circle.
c
c           gpt2    R*8  D3  -  second point of circle.
c
c           gpt3    R*8  D3  -  the last point of circle.
c
c
c   OUTPUT: grad    R*8  D1  -  angle of the first point
c
c           gcir    R*8  D3  -  center of circul
c
c           kerr    I*4  D1  -  Error status
c
c***********************************************************************
c
      subroutine cir3pt (gpt1,gpt2,gpt3,grad,gcir,kerr)
c
      real*8 gpt1(3),gpt2(3),gpt3(3),gcir(3),grad
      integer*4 kerr
c
      real*8 a(3),b(3),c(3),co,afc,bfc,cfc,v1(3),temp(3),pta(3),
     -       aa,bb,vv1,vv2,v2(3),ua(3),ub(3),uc(3),center(3),
     -       ptb(3),ndot,nmag,zer,one
c
      data zer /1.d-4/, one /1.d0/
c
      call vcplvc(gpt2,gpt1,a,-one)
      call vcplvc(gpt3,gpt1,b,-one)
      call vcplvc(gpt3,gpt2,c,-one)
c
      if (nmag(a) .lt. zer .or. nmag(b) .lt. zer .or.
     -    nmag(c) .lt. zer) then
          kerr = 1
      else
          call unitvc(a,ua)
          call unitvc(b,ub)
          co = dabs(ndot (ua,ub))
          if (1.d0 - co .lt. 1.d-6) then
              kerr = 1
          else
              call vcplvc (a,b,v1,-one)
              aa = nmag(a)
              bb = nmag(b)
              vv1 = nmag(v1)
              call crosvc (a,b,v2)
              vv2 = nmag(v2)
              grad = aa * bb * vv1 / (2.d0 * vv2)
              call vcplvc (gpt1,gpt2,pta,one)
              call vcplvc (gpt1,gpt3,ptb,one)
              call vctmsc (pta,.5d0,pta)
              call vctmsc (ptb,.5d0,ptb)
              call crosvc (ua,ub,uc)
              call unitvc (uc,uc)
              call crosvc (ub,uc,temp)
              co = ndot (ua,temp)
              afc   = ndot (ua,pta)
              call vctmsc (temp,afc,center)
              bfc   = ndot (ub,ptb)
              call crosvc (uc,ua,temp)
              call vcplvc (center,temp,center,bfc)
              cfc   = ndot (uc,gpt1)
              call crosvc (ua,ub,temp)
              call vcplvc (center,temp,center,cfc)
              call vctmsc (center,one/co,gcir)
          endif
      endif
c
      return
      end
c
c***************************************************************
c
c   SUBROUTINE:  pre_cirdir (gsta,gnext,center,idir)
c
c   FUNCTION:  From prepst: this routine checks direction of the
c              circular motion.  Points must be in XY plane.
c
c   INPUT:  gsta    R*8  D3  -  The first point on circle.
c           gnext   R*8  D3  -  The next point on circle.
c           center  R*8  D3  -  circle center coordinates.
c
c   OUTPUT: idir    I*2  D1  -  Direction of motion: 1 - CLW,
c                               2 - CCLW.
c
c***************************************************************
      subroutine pre_cirdir (gsta,gnext,center,idir)
c
      include 'post.inc'
      real*8 gsta(3),gnext(3),center(3)
      integer*4 idir
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
c
      real*8 RAD,PI
c
      integer*4 i
      real*8 rnum,dnum,side1,sig,side3,cosa,cangl,vec1(2),vec3(2),
     -       dvec(2)
c
c...get vectors and radius
c
      dnum   = 0.
      rnum   = 0.
      side3  = 0.
      do 120 i=1,2
         vec1(i) = gsta(i) - center(i)
         vec3(i) = gnext(i) - gsta(i)
         dnum = dnum + vec1(i)**2
         side3 = side3 + vec3(i)**2
  120 continue
      side1  = dsqrt (dnum)
      side3  = dsqrt (side3)
c
c...unitize vectors
c
      vec1(1) = vec1(1) / side1
      vec1(2) = vec1(2) / side1
      vec3(1) = vec3(1) / side3
      vec3(2) = vec3(2) / side3
c
c...default clw
c
  200 idir   = 1
      sig    = -1.d0
      dvec(1) = -(vec1(2)*sig)
      dvec(2) =  (vec1(1)*sig)
c
c...get angle between indirection vector and chord
c...if < 90 then  clw  else  cclw
c
      cosa   = dvec(1)*vec3(1) + dvec(2)*vec3(2)
      cangl  = dacos (cosa) * 180.d0/PI
      if (cangl .gt. 90.d0) idir   = 2
c
      return
      end
