C*********************************************************************
C*    NAME         :  revers.f
C*       CONTAINS:
C*    COPYRIGHT 1986 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       revers.for , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:09:35
C*********************************************************************
C **********************************************************************
C **  PROGRAM NAME: revers
C **
C **  PURPOSE OF PROGRAM: Reverse geometric entity.
C **
C **********************************************************************
C **********************************************************************
 
      subroutine revers (itsk)
 
c     include 'com4a.com'
c     implicit undefined (a-z)
      include 'com.com'
 
      integer maxpt, maxwd
      parameter (maxpt = 50)
      parameter (maxptx = 1000)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))
c
      common/pblok/x,y,z,a,b,c,dx,dy,dz,ch
      real*8 x(maxptx),y(maxptx),z(maxptx)
      real*4 a(maxptx),b(maxptx),c(maxptx),dx(maxptx)
      real*4 dy(maxptx),dz(maxptx),ch(maxptx)
 
      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld,sub,sub1
      integer*2 istold
      character*64 label,lab1
 
      real*8 buf(maxwd), asn
 
      integer*2 i, j, k, itemp(4), isvst, nsegs, modix, iwf, irev
      integer*2 iwx, jwx, ifrst, numr8s,inogo, isep, ivoc, itmp
      real*8 temp, p(3), v(3)
      equivalence (temp,itemp)
      integer*2 itsk,nwds,ietype, isv72,ibuf(600)
      integer*4 nclkey, nkey
      real*4 abuf(300), dir, a1,b1,c1,d1, u4
      equivalence (buf,abuf,ibuf)
      logical trflg, leq1
      integer*2 is1,is4
      real*8 newt0, newt1, tbuf(3), r1, r2
c
      data is1 /4/, is4 /1/
 
c          set revers statement type switch to revers/geo type.
      ifl(300) = 0
      if (nextyp .ne. 5) goto 9022
      ifl(44) = 9
 
c     save tv in sc(11) in case its a revers postprocessor word. kathy
      sc(11) = tv
      isvst = ist
c          parse statement for name of geometry to reverse
      ldtflg = .true.
      call parsit
      ldtflg = .false.
c          if type of token parsed is not valid geometry, report error
      if (ityp.ne.2) goto 9244
      if (ist.eq.1) goto 9009
      if (ist.ne.4 .and. ist.ne.5 .and. ist.ne.6 .and. ist.ne.7 .and.
     1    ist.ne.8 .and. ist .ne. 21 .and. ist.ne.DATAST) goto 9244
 
      asn = tv
      nclkey = keyhld
 
      if (itsk.eq.2) goto 10
c                              *****  name was supplied
5     if (idst .eq. 1) then
        idst=ist
        goto 25
      endif
      if (ifl(41) .eq. 0) goto 9008
      if (idst .ne. ist) goto 9089
      goto 25
 
10    continue
c                              *****  no name  - use name in statement
      savid2 = token2
      isvsub = ivxsub
      ifl(9) = ifl(11)
      ifl(10) = ifl(12)
      idst = ist
      keyold = keyhld
      istold = ist
 
25    continue
 
      modix = 0
      if (nextyp .ne. 11 .and.
     x   (ifl(111) .eq. 0 .or. nextyp .ne. 7)) then
c                          get optional modifier posx, negx, etc.
c        if (itsk.eq.1) goto 9004
        call parsit
        if (ityp.ne.1) goto 9018
        if (ist.lt.654 .or. ist.gt.659) goto 9018
        if (ist.lt.657) then
          modix=ist-653
          dir=1.
        else
          modix=ist-656
          dir=-1.
        endif
        if (nextyp .ne. 11 .and.
     x     (ifl(111) .eq. 0 .or. nextyp .ne. 7)) goto 9004
      endif
 
      trflg = .false.
 
c           get geom canonical data if not wf curve
      call isitwf(nclkey,iwf)
      if (iwf.eq.0) then
          call gtentt (asn, trflg, nclkey, ietype, buf)
      else
          call gtdesc (asn, nclkey, nwds, ietype)
      endif
 
c **********************************
c                             vector
      if (idst.eq.4) then
         if (modix.gt.0) then
           if (dabs(buf(modix)).lt.sc(27)) goto 9213
           if (buf(modix)*dir.gt.0.) goto 100
         endif
         buf(1)=-buf(1)
         buf(2)=-buf(2)
         buf(3)=-buf(3)
 
c **********************************
c                             line
      else if (idst .eq. 5) then
         if (modix.gt.0) then
           if (dabs(buf(modix+3)).lt.sc(27)) goto 9213
           if (buf(modix+3)*dir.gt.0.) goto 100
         endif
         buf(1)=buf(1)+buf(4)
         buf(2)=buf(2)+buf(5)
         buf(3)=buf(3)+buf(6)
         buf(4)=-buf(4)
         buf(5)=-buf(5)
         buf(6)=-buf(6)
 
c **********************************
c                             pntvec
      else if (idst .eq. 21) then
         if (modix.gt.0) then
           if (dabs(buf(modix+3)).lt.sc(27)) goto 9213
           if (buf(modix+3)*dir.gt.0.) goto 100
         endif
         buf(4)=-buf(4)
         buf(5)=-buf(5)
         buf(6)=-buf(6)
 
c **********************************
c                             circle
      else if (idst .eq. 7) then
         if (buf(8)**2+buf(9)**2+buf(10)**2.lt..9) goto 9323
         if (modix.gt.0) then
           v(1)=buf(5)*buf(10)-buf(6)*buf(9)
           v(2)=buf(6)*buf(8)-buf(4)*buf(10)
           v(3)=buf(4)*buf(9)-buf(5)*buf(8)
           if (dabs(v(modix)).lt.sc(27)) goto 9213
           if (v(modix)*dir.gt.0.) goto 100
         endif
         buf(4)=-buf(4)
         buf(5)=-buf(5)
         buf(6)=-buf(6)
 
c **********************************
c                             plane
      else if (idst.eq.6) then
         if (modix.gt.0) then
           if (dabs(buf(modix)).lt.sc(27)) goto 9213
           if (buf(modix)*dir.gt.0.) goto 100
         endif
         buf(1)=-buf(1)
         buf(2)=-buf(2)
         buf(3)=-buf(3)
         buf(4)=-buf(4)
 
c **********************************
c                             curve
      else if (idst .eq. 8) then
C
C...If it is a curve, get the t-parameters.  If they aren't 0 and
C...1, then adjustments are going to be made so that the right portion
C...of the curve is displayed.  JLS 4/23/99
C
         if (iwf.eq.0) then
           call ncl_get1_tpar(nclkey,tbuf)
           newt1 = 1-tbuf(1)
           newt0 = 1-tbuf(2)
         endif
         inogo = 0
         irev = 1
c
c...vp 3/24/98 do not copy spline if it same name
c...prevents changing color to default when reversed
c
         if (itsk .eq. 2) irev = 2
         if (iwf.eq.0) nsegs = abuf(1)
         if (modix.gt.0) then
           u4 = .5
           call crvevl(nclkey, u4, p, v)
 
           if (dabs(v(modix)).lt.sc(27)) goto 9213
c              if direction of curve is same as requested direction,
c              don't reverse it
           if (v(modix)*dir .gt. 0.) then
             if (iwf.eq.0) then
               inogo = 1
               abuf(1) = nsegs
               goto 100
             endif
             if (irev .eq. 2) then
                 ifl(262) = 1
c
c..... defwf is set to avoid the vstore call from driver; the driver then
c..... resets everything. QAR 9461
c
                 defwf = .true.
                 go to 99999
             endif
             irev = 0
           endif
         endif
c
c...when revers sspline copy is not allowed (irev=2)
c
         if (iwf.eq.1) then
           call cvtype1(nclkey,nss)
           if (nss .eq. 13) then
             if (irev .eq. 0) then
                call ptdesc (nclkey,ietype,asn)
                goto 120
             else
                irev = 2
             end if
           else if (nss.eq.5 .and. irev.eq.0 .and. itsk.eq.2) then
             goto 120
           end if
           defwf = .true.
           call revwf (nclkey,irev)
           if (nclkey.eq.0) goto 9244
           call ptdesc (nclkey,ietype,asn)
           goto 120
         endif
         iwx=(nsegs+1)/2+6*(nsegs-1)
         do 30 i=1,nsegs
           jwx=iwx*2
c              load x, y and z values
           x(i)=buf(iwx+1)
           y(i)=buf(iwx+2)
           z(i)=buf(iwx+3)
c              load negative of slope vector values
           a1=-abuf(jwx+7)
           b1=-abuf(jwx+8)
           c1=-abuf(jwx+9)
c              get value to normalize slope values with
           d1=sqrt(a1**2+b1**2+c1**2)
c              normalize slope values
           a(i)=a1/d1
           b(i)=b1/d1
           c(i)=c1/d1
c              load up dx, dy, dz and ch array for crvpre to use
           if (i.gt.1) then
             dx(i-1)=x(i)-x(i-1)
             dy(i-1)=y(i)-y(i-1)
             dz(i-1)=z(i)-z(i-1)
             ch(i-1)=sqrt(dx(i-1)**2+dy(i-1)**2+dz(i-1)**2)
           endif
           iwx=iwx-6
30       continue
         call crvpre(ibuf,nsegs,i)
         iwx=(nsegs+1)/2
 
c ***********************
c          load all cuve canonical values in reversed order
         do 40 i=1,nsegs
           jwx=iwx*2
c              load 's' values (stored by crvpre)
           abuf(i)=dx(i)
c              load x, y & z values
           buf(iwx+1)=x(i)
           buf(iwx+2)=y(i)
           buf(iwx+3)=z(i)
c              load slope vector values
           abuf(jwx+7)=a(i)
           abuf(jwx+8)=b(i)
           abuf(jwx+9)=c(i)
c              load ds/du and du/ds values
           abuf(jwx+10)=dy(i)
           abuf(jwx+11)=dz(i)
c              load rho values
           abuf(jwx+12)=ch(i)
           iwx=iwx+6
40       continue
c
c... ********************************** Data statement
c
      else if (idst .eq. DATAST) then
        nkey = 0
        call dtinit(nkey)
        call gtdesc(asn,nclkey,nwds,ietype)
        k = nwds
        do j=1,nwds
          call dtgetv(nclkey,k,r1,ivoc,isep,label, sub)
          k = k-1
          if (k.gt.0) then
            call dtgetv(nclkey,k,r2,itmp,isep,lab1, sub1)
          else
            isep = 11
          endif
          ietype = 2
          if (ivoc .eq. 0) then
            ietype = 3
          else if (ivoc.eq.1) then
            ietype = 1
            ivoc = r1
            r1 = 0.0d0
          endif
          call dtstor(nkey,j,ietype,ivoc,r1,isep,label, sub)
        enddo
        if (keyold.gt.0) call dlgeom(keyold)
        call ptdsc3(nkey,nwds,idst,asn)
        goto 120
      endif
 
c **********************************************************************
c              store generated canonical data to unibase
100   continue
      isv72 = ifl(72)
      ifl(72) = 0
      call ptentt (ietype, buf, nclkey, asn)
C
C...Adjust for the new t-parameters. JLS 4/23/99
C...If inogo is equal to 1, then nothing was changed,so we need to
C...put the original parameters back because they were changed
C...to automatically be 0 to 1 when the curve was setup.
C
      if ((ietype.eq.8)) then
         if (inogo.ne.1) then
            tbuf(1) = newt0
            tbuf(2) = newt1
         endif
         call ncl_put_tpar(nclkey,tbuf)
      endif
      ifl(72) = isv72
120   rest = asn
      ifl(262) = 1
      goto 99999
 
c **********************************************************************
c                  error conditions code
c **********************************************************************
c                            error - end of statement expected
9004  ifl(2) = 4
      goto 9999
c                            error - identifier previously defined
9008  ifl(2) = 8
      goto 9999
c                            error - identifier not previously defined
9009  ifl(2) = 9
      goto 9999
c                            error - direction modifier expected
9018  ifl(2) = 18
      goto 9999
c                            error - '/' expected
9022  isvinx = inx
      ifl(2) = 22
      goto 9999
c                             error - identifier currently has different type
9089  ifl(2) = 89
      goto 9999
c                             error - illogical direction modifier
9213  ifl(2) = 213
      isvinx = 0
      goto 9999
c
c                  if not recognizable disply syntax send it to clfile as
c                  post command. kathy
c
9244  continue
      if (ist .eq. 71 .or. ist .eq. 72) then
         idst = isvst
         ifl(44)=9
         ifrst=1
         do 2500 i=11,22
           if (nextyp.eq.11 .and. ifrst.eq.0) then
                 numr8s=i-10
                 call putcl (2000,1008,numr8s,sc(11))
c          set revers statement type switch to clfile output
                 ifl(300) = 1
                 if (ifl(351) .eq. 1) ifl(367) = 1
                 go to 9999
            endif
            if (ifrst.eq.0) then
                 if (nextyp.eq.9) then
                      call parsit
                 else
                      isvinx=inx
                      ifl(2)=57
                      go to 9999
                 endif
            endif
            ifrst=0
c
c                    if it is a vocabulary word store it in the low order
c                    integer*2 for the post to be able to distinguish it
c                    from a scalar value
            if (ityp.eq.1) then
               temp=0.
               itemp(is4)=ist
               sc(i)=temp
            else if ((ityp.eq.2 .and. ist.eq.2) .or.
     x            ityp.eq.3. .or. ityp.eq.4) then
                sc(i)=tv
             else
                 ifl(2)=7
                 go to 9999
              endif
2500   continue
      endif
c                             error - valid geometry name expected
      ifl(2) = 244
      ityp = 1
      goto 9999
c                             error - full 360 circle invalid
9323  ifl(2) = 323
      isvinx = 0
      goto 9999
 
9999  continue
99999 return
      end
