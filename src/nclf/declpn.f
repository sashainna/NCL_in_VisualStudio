c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       declpn.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:53
c**
c*****************************************************
c**
c** COPYRIGHT (c) 1987 MILLS DATA SYSTEMS CORPORATION
c**
c*************************************************************************
c                          declpn
c                          ******
c      this routine parses patern declaration. the valid syntax
c      constructs are:
c          1.  (name =) patern/linear,pt1,pt2,pn1,...ptn
c          2.           patern/arc
c          3.           patern/random
c          4.           patern/parlel
c
c       the canonical form of a patern consists of a header followed by
c       the points.  the header is 3 real*8s.  the first i*2 of the 
c       first real*8 contains the number of points in the patern.  the
c       rest of the header is not currently used.
c*************************************************************************
c
      subroutine declpn
c
      include 'com8a.com'
      include 'const.com'
c
      real*8 rbuff(36),p1(11),p2(6),dx,dy,dz,u,a1,a2,rads
      real*8 svinc,tl,pkey,temp,dl,dl1,di,dj,dk,uv,an1,an2,an3
      real*8 ptv(6),pval(6)
      real*8 d1,fi,thet,q2(6)
      integer*4 ipnkey, i4rest(2), ipntyp, ictyp,ndev, nclkey
      integer*4 patt_key, surf_key, vec_key, atpt_key, nrpt_key,sfa_key
      integer*2 inter_fl,ipfl(3)
      integer*2 irest(4),nsp,numpts,iclw,itemp(4),npts, nwds
      equivalence (rest,i4rest,irest),(temp,itemp)
      equivalence (q2(4),p2)
      logical redef,first,done,trans,fstpt
      data rads /57.29577951/
      integer*2 iblnk /1/

      first = .true.
      redef = .false.
      done = .false.
      trans = .true.
      numpts=0
      ipntyp = 1
      j = 1
      i = 0
c
c...This part removed vp3.31.93 and replaced by single calls
c...since type of patern must be defined before initializing
c...patern record
c
c               write the header
c     call ptpnhd(rbuff,ipnkey,1)
c
c      added for reset/disply,pn. kathy
c
c     if (ldsppn) then
c     else
c         call blkgeo (ipnkey, 1)
c     endif

c     i4rest(1) = ipnkey
      ipnkey = 0
      if (ityp.eq.1.and.ist.eq.326) then
c                                                 ***** random
         idtype = 0
         call parsit
c
c...allow more than 1000
c...         do 200 i=1,1000
         do 200 i=1,32767
            ictyp = 1
            if (ityp.eq.2.and.(ist.eq.3 .or. ist.eq.21)) then
c                                         **** point
               call gtentt(tv, trans, nclkey, ietype, rbuff)
               if (ietype .eq. 21) ictyp = 2
               if (first) then
                   ipntyp = ictyp
                   first = .false.
                   call ptpnhd(rbuff,ipntyp,ipnkey,1)
                   i4rest(1) = ipnkey
                   if (.not.ldsppn) call blkgeo (ipnkey, 1)
               else if (ipntyp .ne. ictyp) then
                   go to 8887 
               end if      
               numpts=numpts+1
               call chkzsf(rbuff)
               call ppnptt(rbuff,ipntyp,ipnkey,numpts,trans)
            else if (ityp.eq.2.and.ist.eq.20) then
c                                         **** patern
               call gtpnnp(tv,npts,ictyp)
               if (first) then
                   ipntyp = ictyp
                   first = .false.
                   call ptpnhd(rbuff,ipntyp,ipnkey,1)
                   i4rest(1) = ipnkey
                   if (.not.ldsppn) call blkgeo (ipnkey, 1)
               else if (ipntyp .ne. ictyp) then
                   go to 8887 
               end if      
               do 150,j=1,npts
                   call gpnptt(rbuff,tv,j,trans)
                   numpts = numpts+1
                   call chkzsf(rbuff)
                   call ppnptt(rbuff,ipntyp,ipnkey,numpts,trans)
150            continue
            else if (ityp.eq.7.or.(ifl(111).ne.0.and.ityp.eq.5.and.ist
     1            .eq.7))  then
               go to 88888
            else
               call error(2)
               go to 99998
            endif
         idtype = 0
         call parsit
         if (err) go to 99998
200      continue
         call error(15)
         go to 99998
      else if (ityp.eq.1.and.ist.eq.76) then
c
c......***** linear
c
         call parsit
         if (ityp.eq.2.and.(ist.eq.3 .or. ist.eq.21)) then
            call gtentt(tv, trans, nclkey, ietype, p1)
c                  ** put the first point out.  first move it to
c                     rbuff since putent will modify p1 if refsys
            do 210 i=1,6
210            rbuff(i) = p1(i)
            numpts = numpts+1
            if (ist .eq. 21) ipntyp = 2
            call ptpnhd(rbuff,ipntyp,ipnkey,1)
            i4rest(1) = ipnkey
            if (.not.ldsppn) call blkgeo (ipnkey, 1)
            call chkzsf(rbuff)
            call ppnptt(rbuff,ipntyp,ipnkey,numpts,trans)
            call parsit
            if (ityp.eq.2.and.(ist.eq.3 .or. ist.eq.21)) then
c
c......**** point/pointvector
c
                ictyp = 1
                if (ist .eq. 21) ictyp = 2
                if (ictyp .ne. ipntyp) go to 8887
                call gtentt(tv, trans, nclkey, ietype, p2)
                call parsit
                if (ityp.eq.3.or.ityp.eq.4.or.
     1              (ityp.eq.2.and.ist.eq.2)) then
                   if (itv.lt.2.or.itv.gt.32767) then
                        call error(341)
                        goto 99998
                   endif
c
c...                         it's a patern between two pts/pvs with
c...                         a number of equal spaces.
c
                   dx = p2(1)-p1(1)
                   dy = p2(2)-p1(2)
                   dz = p2(3)-p1(3)
                   nsp = itv-1
                   u = nsp
                   u = 1./u
                   ndev = 0
c
c......for PVs check angle between vectors
c
                   if (ictyp .eq. 2) then
                       ndev = 2
                       dl = dsqrt(p1(4)**2+p1(5)**2+p1(6)**2) 
                       dl1 = dsqrt(p2(4)**2+p2(5)**2+p2(6)**2) - dl 
                       uv = dl1 * u
                       call chkvan (p1,p2,an1,an2,d1,fi,thet)
                       if (d1 .lt. .01) then
                           di = uv * p1(4) / dl
                           dj = uv * p1(5) / dl
                           dk = uv * p1(6) / dl
                           ndev = 1
                       end if
                       d1 = d1 * u
                   end if
                   do 100,i=1,nsp
                      rbuff(1) = p1(1)+u*i*dx
                      rbuff(2) = p1(2)+u*i*dy
                      rbuff(3) = p1(3)+u*i*dz
                      numpts = numpts+1
                      call chkzsf(rbuff)
c
c...for PVs linerize vector's length only if same angle
c
                      if (ndev .eq. 1) then
                          rbuff(4) = p1(4)+i*di
                          rbuff(5) = p1(5)+i*dj
                          rbuff(6) = p1(6)+i*dk
c
c...or linerize vector's length and angle
c
                      else if (ndev .eq. 2) then
                          dl1 = dl + i*uv
                          an3 = an1 + i*d1
                          call gtwijk (dl1,an3,fi,thet,rbuff(4))
                      end if
                      call ppnptt(rbuff,ipntyp,ipnkey,numpts,trans)
100                continue
                else
                   call error(53)
                   go to 99998
                endif
            else if (ityp.eq.2.and.ist.eq.4) then
c
c.......**** vector     
c
                call gtentt(tv, trans, nclkey, ietype, p2)
                tl=dsqrt(p2(1)**2+p2(2)**2+p2(3)**2)
                call parsit
c
c...allow more than 1000
c...                do 308 j=1,1000
                do 308 j=1,32767
                 if (ityp.eq.3.or.ityp.eq.4.or.
     1              (ityp.eq.2.and.ist.eq.2)) then
                   if (.not.first) then
                       call error(335)
                       goto 99998
                   endif
c                          it's a patern from a point along a vector
c                          where the magnitude of the vector determines
c                          the distance between the points.
                   nsp=itv-1
                   do 300,i=1,nsp
                       p1(1)=p1(1)+p2(1)
                       p1(2)=p1(2)+p2(2)
                       p1(3)=p1(3)+p2(3)
                       do 295 ii=1,3
295                        rbuff(ii)=p1(ii)
                       numpts=numpts+1
                       call chkzsf(rbuff)
                       call ppnptt(rbuff,ipntyp,ipnkey,numpts,trans)
300                continue
                   goto 88888
                 else if (ityp.eq.1.and.ist.eq.66) then
c                                                       **** incr
c                            its a patern from a point
c                            along a vector where the distance
c                            between the points is specified by
c                            increments.  unitize the vector and 
c                            calculate the distances.
                   call parsit
                   if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3
     1                    .or.ityp.eq.4) then
                      svinc=tv
                      call parsit
                      if (ityp.eq.1.and.ist.eq.189) then
c                                                  **** at
                         call parsit
                         if (ityp.eq.3.or.ityp.eq.4.or.
     1                         (ityp.eq.2.and.ist.eq.2)) then
                            isvtot = svinc
                            svinc = tv
                            do 302 i=1,isvtot
                               p1(1)=p1(1)+svinc*p2(1)/tl
                               p1(2)=p1(2)+svinc*p2(2)/tl
                               p1(3)=p1(3)+svinc*p2(3)/tl
                               do 301 ii=1,3
301                                rbuff(ii)=p1(ii)
                               call chkzsf(rbuff)
                               numpts=numpts+1
                               call ppnptt(rbuff,ipntyp,ipnkey,
     1                                     numpts,trans)
302                         continue
                            call parsit
                         else
                            call error(282)
                            go to 99998
                         endif
                      else if (ityp.eq.3.or.ityp.eq.4.or.
     1                        (ityp.eq.2.and.ist.eq.2)) then
c
c...allow more than 1000
c...                         do 304 i=1,1000
                         do 304 i=1,32767
                            p1(1)=p1(1)+svinc*p2(1)/tl
                            p1(2)=p1(2)+svinc*p2(2)/tl
                            p1(3)=p1(3)+svinc*p2(3)/tl
                            do 303 ii=1,3
303                             rbuff(ii)=p1(ii)
                            numpts=numpts+1
                            call chkzsf(rbuff)
                            call ppnptt(rbuff,ipntyp,ipnkey,
     1                                  numpts,trans)
                            if (done) then
                                done = .false.
                                go to 308
                            endif
                            svinc=tv
                            call parsit
                            if (ityp.eq.3.or.ityp.eq.4.or.
     1                          (ityp.eq.2.and.ist.eq.2))then
                            else
                               done=.true.
                            endif
304                      continue
                      else if (ityp.eq.7 .or.
     1                        (ityp .eq. 1 .and. ist .eq. 66)) then
                         p1(1)=p1(1)+svinc*p2(1)/tl
                         p1(2)=p1(2)+svinc*p2(2)/tl
                         p1(3)=p1(3)+svinc*p2(3)/tl
                         do 305 ii=1,3
305                          rbuff(ii)=p1(ii)
                         numpts=numpts+1
                         call chkzsf(rbuff)
                         call ppnptt(rbuff,ipntyp,ipnkey,numpts,trans)
                      else
                         call error(282)
                         go to 99998
                      endif
                   else 
                      call error(282)
                      go to 99998
                   endif
                 else if (ityp.eq.7) then
                   go to 88888
                 else
                   call error(53)
                   go to 99998
                 endif
308            continue                   
            else 
                call error(334)
                goto 99998
            endif
         else
            call error(20)
            go to 99999
         endif
c
c...pn/pn1. Paul 11/11/93
c
      else if (ityp .eq. 2 .and. ist .eq. 20 .and. NXTEOS) then
c                                                     ***** pn/pn1
            pkey = tv
            call gtpnnp(pkey,nsp,ipntyp)
c                           get the patern and load it first
            call ptpnhd(rbuff,ipntyp,ipnkey,1)
            i4rest(1) = ipnkey
            if (.not.ldsppn) call blkgeo (ipnkey, 1)
            do 515,i=1,nsp
c  changed the routine name gtpnpt to gpnptt on: 3/4/88 by: kathy
                call gpnptt(rbuff,pkey,i,trans)
                call gtentt(tv, trans, nclkey, ietype, p2)
                numpts=numpts+1
                call chkzsf(rbuff)
                call ppnptt(rbuff,ipntyp,ipnkey,numpts,trans)
                if (first) first = .false.
515         continue
            goto 99999
c
      else if (ityp.eq.1.and.ist.eq.637) then
c                                                     ***** parlel
         call parsit
         if (ityp.eq.2.and.ist.eq.20) then
            pkey = tv
            call gtpnnp(pkey,nsp,ipntyp)
c                           get the patern and load it first
            call ptpnhd(rbuff,ipntyp,ipnkey,1)
            i4rest(1) = ipnkey
            if (.not.ldsppn) call blkgeo (ipnkey, 1)
            do 510,i=1,nsp 
c  changed the routine name gtpnpt to gpnptt on: 3/4/88 by: kathy 
                call gpnptt(rbuff,pkey,i,trans)
                call gtentt(tv, trans, nclkey, ietype, p2)
                numpts=numpts+1
                call chkzsf(rbuff)
                call ppnptt(rbuff,ipntyp,ipnkey,numpts,trans)
                if (first) first = .false.
510         continue
            call parsit
            if (ityp.eq.2.and.(ist.eq.4 .or. ist.eq.21)) then
                if (ist.eq.4) then
                    call gtentt(tv, trans, nclkey, ietype, p2)
                else
                    call gtentt(tv, trans, nclkey, ietype, q2)
                end if
                inc=1
                p1(1)=0.
                p1(2)=0.
                p1(3)=0.
                call parsit
c
c...allow more than 1000
c...                do 550,ii=1,1000
                do 550,ii=1,32767
                   if (ityp.eq.1.and.ist.eq.66) then
c                                      *** incr
                      tl=dsqrt(p2(1)**2+p2(2)**2+p2(3)**2)
                      call parsit
                      if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3
     1                       .or.ityp.eq.4) then
                          svinc=tv
                      else
                          call error(217)
                          go to 99998
                      endif
                      call parsit
                      if (ityp.eq.1.and.ist.eq.189) then
                          call parsit
                          if ((ityp.eq.2.and.ist.eq.2).or.
     1                         ityp.eq.3.or.ityp.eq.4) then
c                                   make the vector the correct length
                             dx=p2(1)*tv/tl
                             dy=p2(2)*tv/tl
                             dz=p2(3)*tv/tl
                          else
                             call error(217)
                             go to 99998
                          endif
                          do 505,j=1,svinc
                             if (inc.eq.1) then
                                inc=-1
                                istart=nsp
                                istop=1
                             else
                                inc=1
                                istart=1
                                istop=nsp
                             endif
                             p1(1)=p1(1)+dx
                             p1(2)=p1(2)+dy
                             p1(3)=p1(3)+dz
                             do 500,i=istart,istop,inc 
c      changed the routine name from gtpnpt to gpnptt on: 3/4/88 by: kathy 
                                call gpnptt(rbuff,pkey,i,trans)
                                rbuff(1)=rbuff(1)+p1(1)
                                rbuff(2)=rbuff(2)+p1(2)
                                rbuff(3)=rbuff(3)+p1(3)
                                numpts = numpts + 1
                                call chkzsf(rbuff)
                                call ppnptt(rbuff,ipntyp,ipnkey,
     1                                      numpts,trans)
500                          continue
505                       continue
                          call parsit
                       else if ((ityp.eq.2 .and. ist.eq.2 .or.
     1                          ityp.eq.3 .or. ityp.eq.4) .or.
     2                          (ityp .eq. 1 .and. ist .eq. 66) .or.
     3                          ityp .eq. 7) then
                           dx=p2(1)/tl
                           dy=p2(2)/tl
                           dz=p2(3)/tl
c
c...allow more than 1000
c...                           do 506,j=1,1000
                           do 506,j=1,32767
                              if (inc.eq.1) then
                                inc=-1
                                istart=nsp
                                istop=1
                              else
                                inc=1
                                istart=1
                                istop=nsp
                              endif
                              p1(1)=p1(1)+dx*svinc
                              p1(2)=p1(2)+dy*svinc
                              p1(3)=p1(3)+dz*svinc
                              do 501,i=istart,istop,inc 
c      changed the routine name from gtpnpt to gpnptt on: 3/4/88 by: kathy
                                call gpnptt(rbuff,pkey,i,trans)
                                rbuff(1)=rbuff(1)+p1(1)
                                rbuff(2)=rbuff(2)+p1(2)
                                rbuff(3)=rbuff(3)+p1(3)
                                numpts = numpts + 1
                                call chkzsf(rbuff)
                                call ppnptt(rbuff,ipntyp,ipnkey,
     1                                      numpts,trans)
501                          continue
                             if (ityp .eq. 1 .or. ityp .eq. 7) then
                                 done = .true.
                                 goto 507
                             endif
                             if (done) then
                                done=.false.
                                goto 550
                             endif
                             svinc=tv
                             call parsit
                             if ((ityp.eq.2.and.ist.eq.2).or.ityp
     1                            .eq.3.or.ityp.eq.4) then
c   
                             else
                                done=.true.
                                goto 506
                             endif
506                       continue
507                   continue
                       else
                          call error(343)
                          goto 99998
                       endif
                    else if ((ityp.eq.2.and.ist.eq.2).or.
     1                         ityp.eq.3.or.ityp.eq.4) then
                        do 525,j=1,itv-1
                            if (inc.eq.1) then
                                inc=-1
                                istart=nsp
                                istop=1
                            else
                                inc=1
                                istart=1
                                istop=nsp
                            endif
                            p1(1)=p1(1)+p2(1)
                            p1(2)=p1(2)+p2(2)
                            p1(3)=p1(3)+p2(3)
                            do 520,i=istart,istop,inc 
c     changed the routine name from gtpnpt to gpnptt on: 3/4/88 by: kathy 
                                call gpnptt(rbuff,pkey,i,trans)
                                rbuff(1)=rbuff(1)+p1(1)
                                rbuff(2)=rbuff(2)+p1(2)
                                rbuff(3)=rbuff(3)+p1(3)
                                numpts = numpts + 1
                                call chkzsf(rbuff)
                                call ppnptt(rbuff,ipntyp,ipnkey,
     1                                      numpts,trans)
520                         continue
525                     continue
                        go to 88888
                    else if (ityp.eq.7.or.(ifl(111).ne.0.and.
     1                    ityp.eq.5.and.ist.eq.7))  then
                         go to 88888
                    else
                        call error(335)
                        go to 99998
                    endif
550              continue
             else
                 call error(11)
                 go to 99998
             endif
         else
             call error(342)
             go to 99999
         endif 
      else if (ityp.eq.1.and.ist.eq.182) then
c                                                     ***** arc
         call parsit
         if (ityp.ne.2.or.ist.ne.7) then
            call error(159)
            goto 99999
         endif
         call gtentt(tv, trans, nclkey, ietype, p1)
c                              check for tipped
         if (dabs(p1(6)).lt..999) then
            call error(161)
            goto 99999
         endif
         call ptpnhd(rbuff,ipntyp,ipnkey,1)
         i4rest(1) = ipnkey
         if (.not.ldsppn) call blkgeo (ipnkey, 1)
c                              get angles
         call parsit
         if (ityp.ne.3.and.ityp.ne.4.and.
     1              (ityp.ne.2.or.ist.ne.2)) then
            call error(7)
            goto 99998
         endif
         a1=tv
310      if (a1.lt.0) then
           a1=a1+360
           goto 310
         endif
         call parsit
         if (ityp.eq.3.or.ityp.eq.4.or.
     1              (ityp.eq.2.and.ist.eq.2)) then
           a2=tv
320        if (a2.lt.0) then
             a2=a2+360
             goto 320
           endif
c                              get cclw or clw
           call parsit
           if (ityp.ne.1.or.(ist.ne.59.and.ist.ne.60)) then
              call error(316)
              goto 99998
           endif
           iclw=0
           if (ist.eq.60) iclw=1
           call parsit
c                                        get number of points
           if (ityp.ne.3.and.ityp.ne.4.and.
     1              (ityp.ne.2.or.ist.ne.2)) then
              call error(7)
              goto 99998
           endif
           if (nextyp.ne.11) then
              call error(4)
              goto 99998
           endif
           nsp=itv-1
           u=nsp
           a2=a2-a1
           if (iclw.eq.1) goto 380
360        if (a2.gt.0.) goto 390
           a2=a2+360
           goto 360
380        if (a2.lt.0.) goto 390
           a2=a2-360
           goto 380
390        continue
           if (u.le.0) u=1.
           u=a2/u
           a1=a1/rads
           u=u/rads
           do 400,i=0,nsp
              rbuff(1)=p1(1)+p1(7)*dcos(a1+u*i)
              rbuff(2)=p1(2)+p1(7)*dsin(a1+u*i)
              rbuff(3)=p1(3)
              numpts=numpts+1
              call chkzsf(rbuff)
              call ppnptt(rbuff,ipntyp,ipnkey,numpts,trans)
              if (.not.first) first=.false.
400        continue
         else if (ityp.eq.1.and.(ist.eq.59.or.ist.eq.60)) then
c                                      cclw or clw - incr next
           fstpt = .true.
           iclw=0
           if (ist.eq.60) iclw=1
           a1=a1/rads
           call parsit
           if (ityp.eq.7) then
             call error(61)
             goto 99998
           endif
c
c...allow more than 1000
c...           do 480 j=1,1000
           do 480 j=1,32767
             if (ityp.eq.7) goto 88888
             if (ityp.ne.1.or.ist.ne.66) then
                call error(12)
                goto 99998
             endif
             call parsit
             if (ityp.ne.3.and.ityp.ne.4.and.
     1              (ityp.ne.2.or.ist.ne.2)) then
                call error(7)
                goto 99998
             endif
             svinc=tv
             call parsit
             if (ityp.eq.1.and.ist.eq.189) then
c                                                ** at
               call parsit
               if (ityp.ne.3.and.ityp.ne.4.and.
     1              (ityp.ne.2.or.ist.ne.2)) then
                  call error(7)
                  goto 99998
               endif
               isvtot=svinc
               svinc=tv/rads
               if (iclw.eq.1) svinc=-svinc
               n = 2
               if (fstpt) then
                   n = 1
                   fstpt = .false.
                   a1 = a1 - svinc
               endif
               do 420 i=n,isvtot+1
                 a1=a1+svinc
                 rbuff(1)=p1(1)+p1(7)*dcos(a1)
                 rbuff(2)=p1(2)+p1(7)*dsin(a1)
                 rbuff(3)=p1(3)
                 numpts=numpts+1
                 call chkzsf(rbuff)
                 call ppnptt(rbuff,ipntyp,ipnkey,numpts,trans)
420            continue
               call parsit
             else if (ityp.eq.3 .or. ityp.eq.4 .or.
     1                  (ityp.eq.2.and.ist.eq.2)) then
c                                                    list of angle incs
               first = .true.
               if (fstpt) then
                  rbuff(1)=p1(1)+p1(7)*dcos(a1)
                  rbuff(2)=p1(2)+p1(7)*dsin(a1)
                  rbuff(3)=p1(3)
                  numpts=numpts+1
                  call chkzsf(rbuff)
                  call ppnptt(rbuff,ipntyp,ipnkey,numpts,trans)
               endif
               fstpt = .false.
               svinc=svinc/rads
c
c...allow more than 1000
c...               do 440 i=1,1000
               do 440 i=1,32767
                 if (iclw.eq.1) then
                   a1=a1-svinc
                 else
                   a1=a1+svinc
                 endif
                 rbuff(1)=p1(1)+p1(7)*dcos(a1)
                 rbuff(2)=p1(2)+p1(7)*dsin(a1)
                 rbuff(3)=p1(3)
                 numpts=numpts+1
                 call chkzsf(rbuff)
                 call ppnptt(rbuff,ipntyp,ipnkey,numpts,trans)
                 if (.not.first) then
                   call parsit
                 else
                   first=.false.
                 endif
                 if (ityp.ne.3 .and. ityp.ne.4 .and.
     1                 (ityp.ne.2.or.ist.ne.2)) goto 480
                 svinc=tv/rads
440            continue
             else
               call error(7)
               goto 99998
             endif
480       continue
         else
            call error(7)
            goto 99998
         endif
c
c...PATERN/PROJCT
c
      else if (ityp.eq.1 .and. ist.eq.888) then
         call prjprs (2,ipfl,ptv,pval,ierr)
         if (ierr .ne. 0) then
            call error (ierr)
            goto 99999
         endif
c
         atpt_key = 0
         vec_key = 0
         sfa_key = 0
         nrpt_key = 0
         call gtdesc (ptv(1),patt_key,nwds,ietype)
         call gtdesc (ptv(2),surf_key,nwds,ietype)
         if (ipfl(1) .eq. 1) call gtdesc (ptv(3),vec_key,nwds,ietype)
         if (ipfl(1) .eq. 2) call gtdesc (ptv(4),sfa_key,nwds,ietype)
         if (ipfl(3) .eq. 5) call gtdesc (ptv(5),atpt_key,nwds,ietype)
         if (ptv(6) .ne. 0.) call gtdesc (ptv(6),nrpt_key,nwds,ietype)
c
         inter_fl = 0
         if (ipfl(1) .eq. 2) inter_fl = 1
c
         pval(1) = pval(1) / RADIAN
         pval(2) = pval(2) / RADIAN
         call nclf_pn_project_sf(patt_key,surf_key,pval(3),ipfl(2),
     1       ipfl(3),ipfl(1),pval(1),vec_key,atpt_key,sfa_key,pval(5),
     2       nrpt_key,nclkey,numpts,err)
         if (err) then
            call error (err)
            goto 99999
         endif
         call ptdsc3 (nclkey, numpts, 20, rest)
         if (.not.ldsppn) call blkgeo (nclkey, iblnk)
         goto 99999
      else
         call error(333)
         go to 99998
      endif
88888 continue
      if (numpts.lt.1.or.numpts.gt.32767) then
          call error(341)
          go to 99998
      endif
      if (ityp.eq.7.or..not.(ifl(111).eq.0.and.nextyp.ne.11))then
c                    put numpts in the header (first i*2) if 501
          itemp(1) = numpts
          call ptpnhd(temp,ipntyp,ipnkey,0)
          irest(3) = numpts
          irest(4) = idst
          tv = rest
          goto 99999
      else
          call error(4)
          go to 99998
      endif
c
c...error of incompatibility of patern types
c
8887  call error (429)
99998 if (ipnkey .ne. 0) call dlgeom (ipnkey)
c
99999 return
      end
c
c*****************************************************************
c  SUBROUTINE: chkvan (p1,p2,an1,an2,dlta,fi,thet)
c
c  FUNCTION:  This routine defines angle between two pointvectors.
c
c     INPUT:  p1     R*8  D6  -  PoinVector 1
c
c             p2     R*8  D6  -  PoinVector 2
c
c     OUTPUT: an1    R*8  D1  -  Angle of PV1 on tvmp 
c
c             an2    R*8  D1  -  Angle of PV2 on tvmp 
c
c             dlta   R*8  D1  -  Angle between PV1 & PV2
c
c             fi     R*8  D1  -  polar angle of tvmp vector
c
c             thet   R*8  D1  -  polar longtitude angle of tvmp vector
c
c*****************************************************************
      subroutine chkvan (p1,p2,an1,an2,dlta,fi,thet)
c
      real*8 p1(6),p2(6),an1,an2,dlta,fi,thet
c
      real*8 rd1,rd2,vin(3),vot(3),tvmp(3)
c
      integer*4 i,ifl
c
c...unitize vectors
c
      rd1   = dsqrt (p1(4)**2 + p1(5)**2 + p1(6)**2)
      rd2   = dsqrt (p2(4)**2 + p2(5)**2 + p2(6)**2)
      do 105 i=1,3
          vin(i) = p1(i+3) / rd1
          vot(i) = p2(i+3) / rd2
  105 continue
c 
c...get tool motion plane
c 
      call getvan (vin,vot,an1,an2,dlta,fi,thet,tvmp,ifl)
      if (ifl .eq. 1 .and. dlta .gt. 179.99) then
c
c...vectors at 180 deg, try to use line between points
c...to obtain motion plane
c
          vot(1) = p2(1) - p1(1)
          vot(2) = p2(2) - p1(2)
          vot(3) = p2(3) - p1(3)
          rd1   = dsqrt (vot(1)**2 + vot(2)**2 + vot(3)**2)
          if (rd1 .lt. 1.d-6) go to 900
          vot(1) = vot(1) / rd1 
          vot(2) = vot(2) / rd1 
          vot(3) = vot(3) / rd1 
          call getvan (vin,vot,an1,an2,dlta,fi,thet,tvmp,ifl)
          if (ifl .eq. 0) then
              dlta = 180.0
              an2  = an1 + dlta 
          end if
      end if
c
  900 return
      end
c*****************************************************************
c  SUBROUTINE: gtwijk (dl,ang,fi,thet,vec) 
c
c  FUNCTION:  This routine defines vector's cartesian coordinates
c             using its polar coordinates.
c
c     INPUT:  dl     R*8  D1  -  Vector's length
c
c             ang,   R*8  D1
c             fi,    R*8  D1  -  Polar coordinates of vector
c             thet   R*8  D1  
c
c     OUTPUT: vec    R*8  D3  -  Catesian vector coordinates 
c
c*****************************************************************
      subroutine gtwijk (dl,ang,fi,thet,vec)
c
      real*8 dl,ang,fi,thet,vec(3)
c
      real*8 v(3)
c
c...get unit vector
c
      call setijk (ang,v,fi,thet)
c
c...adjust vector's length
c
      vec(1) = v(1) * dl
      vec(2) = v(2) * dl
      vec(3) = v(3) * dl
c
      return
      end
