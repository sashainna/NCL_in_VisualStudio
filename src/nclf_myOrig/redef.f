c*****************************************************
c**    NAME         :redef.f
c**       CONTAINS:
c**                 trminn
c**                 trminc
c**                 trimsf0
c**                 redef
c**                 redefn
c**
c**    MODULE NAME AND RELEASE LEVEL 
c**       redef.f , 25.3
c**    DATE AND TIME OF LAST  MODIFICATION
c**       02/28/17 , 15:51:31
c**
c*****************************************************
c**
c** copyright (c) 1985 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  SUBROUTINE: trminn
c **                                                                  **
c **  purpose of subroutine: read in and store a sequence of scalars. **
c **                                                                  **
c **********************************************************************
c **********************************************************************
      subroutine trminn

      include 'com8a.com'

      integer*4 inum1, inum2, j, num

      inum1 = -10
      call parsit
      do 100 i=1,20000,1
c
c...REMOVE, n1, n2
c
          if (scalar) then
              inum1 = tv
              call rmvput (inum1)
c
c...REMOVE, n1, THRU, n2
c
          else if (ityp .eq. 1 .and. ist .eq. 152) then
              if (nextyp.ne.9) then
                  ifl(2)=57
                  goto 8000
              endif
              call parsit
              if (.not.scalar) then
                  ifl(2) = 53
                  goto 8000
              endif
              inum2 = tv
              if (inum1 .eq. -10 .or. inum2 .lt. inum1) then
                  ifl(2) = 114
                  go to 8000
              endif
              do 50 j=inum1+1,inum2,1
                  call rmvput (j)
   50         continue
c
c...REMOVE, ALL
c
          else if (ityp .eq. 1 .and. ist .eq. 816) then
              num = -1
              call rmvput (num)
c
c...Scalar expected
c
          else
              ifl(2) = 53
              go to 8000
          endif
          if(nextyp .eq. 11) goto 8000
          if (nextyp.ne.9) then
              ifl(2)=57
              goto 8000
          endif
          idtype = -1
          call parsit
  100 continue
c
c...End of routine
c
 8000 if (ifl(2).ne.0) then
          call rmvfre
          call error(ifl(2))
      endif
      return
      end

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE: trminc
c **                                                                  **
c **  purpose of subroutine: read in and store a sequence of curves.  **
c **                                                                  **
c **********************************************************************
c **********************************************************************
      subroutine trminc

      include 'com8a.com'

      integer*4 nclkey
      integer*2 nwds

         idtype = CURVE
         call parsit
         do while (geom .and. (ist.eq.CURVE.or.ist.eq.CIRCLE))
           isc10(3) = isc10(3)+1
           call gtdesc (tv, nclkey, nwds, itype)
           call ncl_put_key (nclkey)
           idtype = CURVE
           call parsit
         enddo

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine name: trimsf0
c **********************************************************************
c **********************************************************************
      subroutine trimsf0(iwhich)
 
      include 'com8a.com'

          call trimsf1
          if (ifl(2).ne.0) then
            call error(ifl(2))
            iwhich = 0
          endif

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine name: redef
c **                                                                  **
c **  purpose of subroutine: to parse the redef statement.            **
c **                                                                  **
c **********************************************************************
c **********************************************************************
 
      subroutine redef(iwhich)
 
      include 'com8a.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      real*8 lorc,p1desc,p2desc
      integer*2 ktv(4),iwhich,imod1,imod2,lorct(4),p1t(4),p2t(4)
      equivalence (lorc,lorct),(p1desc,p1t),(p2desc,p2t)
      equivalence (tv,ktv)
      equivalence (sc(14),uv0),(sc(19),uv),(u1,uv(1)),(v1,uv(2))
      real*4 u1,v1,uv(2),uv0(2)
      logical lflg
      integer*4 nclkey
      integer*2 nwds, ietype, iflg, iclos, irld, ist0, iswap
      integer*2 PARELM, OUT, KEEP, VOCSF
      parameter (PARELM = 833, OUT = 653, KEEP = 761, VOCSF = 609)
      integer*2 BASE, FACE, REMOVE, IN
      parameter (BASE = 868, FACE = 81, REMOVE = 843, IN = 652)
      integer*2 NORMAL,PARAMS,REVERS,SWAP
      parameter (NORMAL = 820, PARAMS = 934, REVERS = 1008, SWAP = 111)

      p1desc = 0
      p2desc = 0
      lorc  = 0
      imod1 = 0
      imod2 = 0
      if (nextyp.ne.5) then
          isvinx=inx
          call error(22)
          go to 99999
      endif
      ifl(44)=9
      isv   = INX
      call parsit
      keyold = keyhld
      istold = ist

      if (vocab .and. (ist.eq.OUT .or. ist.eq.VOCSF)) then
        if (ist .eq. VOCSF) then
          call parsit
          if (.not. (vocab .and. ist.eq.OUT)) then
            call error (186)
            goto 99999
          endif
        endif
        if (ifl(296) .eq. 0) call assgn0
        sc(10) = 0
        isc10(1) = 607
        isc10(2) = 10
        sc(11) = 0
        call parsit
        if (ityp.eq.2 .and. (ist.eq.CURVE .or. ist.eq.CIRCLE)) then
          isc10(4) = 2
          sc(12) = tv
          isc10(3) = 1
          i = 1
c
c...added parsing logic for iner boundary curves.
c
          if (nxteos) goto 100
          call parsit
          if (.not. vocab .and. ist.ne.IN) then
            call error(479)
            goto 99999
          endif
          idtype = CURVE
          call parsit
          do while (geom .and. (ist.eq.CURVE.or.ist.eq.CIRCLE))
            i = i+1
            call gtdesc (tv, nclkey, nwds, itype)
            call ncl_put_key (nclkey)
            idtype = CURVE
            call parsit
          enddo
          if (ityp.ne.7) then
          call error(21)
          goto 99999
          endif
          isc10(3) = i
100       call trimsf0(iwhich)
        else
          call error(21) ! surf expected
        endif
        goto 99999
      endif

      if (ityp.eq.2 .and. (ist.eq.CURVE .or. 
     x                     ist.eq.PLANE .or. ist.eq.SURF)) then
        ist0 = ist
        call gtdesc(tv,nclkey,nwds,ietype)
        p1desc = tv

        if (ist0 .eq. SURF) then
          u1 = 0.5
          v1 = 0.5
          iret = 9
          call parsuv(iret, lflg, u1, v1)
          if (iret.gt.0) then
            call error(iret)
            goto 99999
          endif
        endif

        if (nxteos .and. ist0.eq.SURF) then
          sc(10) = 0
          isc10(1) = 607
          isc10(2) = 10
          isc10(4) = 1
          sc(11) = p1desc
          sc(12) = 0
          call trimsf0(iwhich)
          goto 99999
        endif

        call parsit
              
        if ((ist0.eq.PLANE .or. ist0.eq.SURF) .and. vocab .and.
     x      (ist.eq.OUT .or. ist.eq.IN .or. ist.eq.REMOVE)) then

          sc(10) = 0
          isc10(1) = 607
          isc10(2) = 10
          isc10(4) = 1

          sc(11) = p1desc
          sc(12) = 0
          sc(13) = 0
          sc(14) = 0

          if (ist .eq. REMOVE) then
            if (ist0 .ne. SURF) then
              call error(21) ! surf expected
              iwhich = 0
            else
              call trminn
              isc10(4) = 3
              call trimsf0(iwhich)
            endif

            goto 99999
          endif

          if (ist .eq. IN) then
            if (ist0 .ne. SURF) then
              call error(21) ! surf expected
              iwhich = 0
            else
              call trminc
              call trimsf0(iwhich)
            endif

            goto 99999
          endif

c          if (ist0 .eq. SURF) isc10(3) = 1
          call parsit
          if (ityp.eq.2) then
            if (ist.ne.SURF .and. (ist0.ne.SURF.or.ist.ne.PLANE) .and.
     x          ist.ne.CURVE .and. ist.ne.LINE .and. ist.ne.CIRCLE) then
              call error(640) ! curve or surface expected
              goto 99999
            endif
              sc(12) = tv
              isc10(3) = isc10(3)+1
              if (.not.nxteos) then
                call parsit
                if (vocab .and. voc.eq.IN) then
                  call trminc
                  call trimsf0(iwhich)
                  goto 99999
                endif
               
                if (vocab .and. voc.eq.KEEP) call parsit
                if (scalar) then
                  sc(13) = 1
                  uv0(1) = tv
                  if (.not.nxteos) then
                    call parsit
                    if (scalar) uv0(2) = tv
                  endif
                else if (vocab .and. (voc.ge.638 .and. voc.le.643)) then
                  sc(13) = 2
                  sc(14) = voc
                else if (vocab .and. (voc.eq.662 .or. voc.eq.663)) then
                  sc(13) = 3
                  sc(14) = voc
                else if (ityp.eq.2 .and.
     x                   (ist.eq.POINT .or. ist.eq.PNTVEC)) then
                  sc(13) = 4
                  sc(14) = tv
                else if (ityp.eq.2 .and. ist.eq.VECTOR) then
                  sc(13) = 5
                  sc(14) = tv
                else if (.not. (vocab .and. voc.eq.IN)) then
                  call error(18) ! modifier expected
                  goto 99999
                endif
              endif
              if (.not.nxteos) call parsit
              if (vocab .and. voc.eq.IN) call trminc
              call trimsf0(iwhich)
          else
            call error(170)
          endif
          goto 99999
        endif
       
        if (ietype.eq.SURF .and. vocab) then
c
c...   Change PARAMS: swap and/or reverse U,V directions of a surface.
c
          if (voc .eq. PARAMS .and. .not.nxteos) then
            iswap = 0
            lflg = .false.
            call parsit

            if (vocab .and. voc.eq.SWAP) then
              iswap = 1
              if (nxteos) goto 106 
              call parsit
            endif

            if (vocab .and. voc.eq.REVERS) then
105           continue
              call parsit
              if (scalar .and. itv.eq.0) then
                iswap = iswap + 4
              else if (scalar .and. itv.eq.1) then
                iswap = iswap + 2
              else if (vocab .and. voc.eq.NORMAL) then
                lflg = .true.
              else
                goto 106
              endif
              if (.not.nxteos) then
                goto 105
              endif
            endif
         
106         continue
            if (iswap .gt. 0) call nclf_swapuv (nclkey,iswap)
            if (lflg) call nclf_revnorm (nclkey)
            if (nxteos) goto 99999
          endif
c
c...   Change PARELM ruling direction of a surface.
c
          if (vocab .and. voc.eq.PARELM) then
            call parsit
            if (.not.scalar .or. itv.lt.-1 .and. itv.gt.1) then
              call error(170)
              goto 99999
            endif
            irld = itv
            call ptrld (nclkey, irld)
            if (nxteos) goto 99999
            call parsit
          endif
c
c...Set drive trimmed surface as trimmed flag
c
          if (ist.eq.BASE .or. ist.eq.FACE) then
            iflg = 0
            if (ist.eq.FACE) iflg = 1
            call pttrim (nclkey, iflg)
            if (nxteos) goto 99999
            call parsit
          endif
        endif
c
c...   Change OPEN or CLOSED for a curve or surface.
c
        if (ityp.ne.1 .or. ist.ne.50 .and. ist.ne.831) then
          if (ist0 .eq. 8) then
              INX  = isv
              call parsit
              go to 110
          end if
          call error(380)
          goto 99999
        endif
        iclos = 1
        if (ist.eq.50) iclos = 0
        iflg = 0
        if (ietype.eq.9) then
          call parsit
          if (.not. scalar) then
            call error(53)
            goto 99999
          endif
          if (itv.ne.0 .and.itv.ne.1) then
            call error(170)
            goto 99999
          endif
          iflg = itv
        endif
        if (.not.nxteos) then
          call error(4)
          goto 99999
        endif
        call ptclsd(nclkey,iflg,iclos)
        goto 99999
      endif
c
c...try new stuff
c
  110 if (ityp.eq.2.and.(ist.eq.5.or.ist.eq.7.or.ist.eq.8)) then
c                                                    **** line or circle
           call redefn (ierr)
           if (ierr .eq. 0) go to 99999 
           savid2 = token2
           isvsub = ivxsub
           ifl(9)=ifl(11)
           ifl(10)=ifl(12)
           idst = ist
           lorc=tv
           rest=tv
           call parsit
      else
           call error (192)
           go to 99999
      endif
      if (ityp.eq.2.and.(ist.eq.3.or.ist.eq.5.or.ist.
     1       eq.7.or.ist.eq.8)) then
c                       ***** its a point,line,circle or curve
         p1desc = tv
         call parsit
      else 
         call error (20)
         go to 99999
      endif
      if (ityp.eq.2.and.(ist.eq.3.or.ist.eq.5)) then
c                                                    **** point or line
         p2desc = tv
      else if (ityp.eq.1.and.(ist.gt.637.and.ist.lt.644)) then
         if (ist.eq.640.or.ist.eq.643) then
              call error(213)
              go to 99999
         endif
c                    *****  xs,ys,xl,yl
         imod1 = ist
       else if (ityp.eq.1.and.(ist.eq.59.or.ist.eq.60)) then
             imod1 = ist
       else if (nextyp.ne.11) then
         call error (20)
         go to 99999
      endif
      if (nextyp.ne.11) then
         call parsit
         if (ityp.eq.1.and.(ist.gt.637.and.ist.lt.644)) then
             if (ist.eq.640.or.ist.eq.643) then
                  call error(213)
                  go to 99999
             endif
c                    *****  xs,ys,xl,yl
             if (imod1.eq.0) then
                 imod1 = ist
             else
                 imod2 = ist
             endif
         else if (ityp.eq.1.and.(ist.eq.59.or.ist.eq.60)) then
             if (imod1.eq.0) then
                 imod1 = ist
             else
                 imod2 = ist
             endif
         else 
             call error (20)
             go to 99999
         endif
      endif

      if (idst.eq.5) then
c                                 ***** its a line ******
          if (p1t(4).eq.3.and.p2t(4).eq.3) then
c                  its a line between two points
c                  call ptonln to compute the first and second points
              call ptonln(lorc,p1desc,sc(11),sc(12),sc(13))
              call ptonln(lorc,p2desc,sc(14),sc(15),sc(16))
c                  now make it look like a line defined by 6 scalars
c                  for geogna
          else
c                  its a line trimmed to an entity
              call lnrdef(lorc,p1desc,p2desc,imod1,imod2)
              if (debug) then
               write (cout,123)lorct,p1t,p2t,imod1
123            format ('lnrdef: ',13i4)
               call putmsg(cout,80,18,0)
              endif               
          endif
c
c... call error now since it is no longer called from driver -raz
c
          if (ifl(2).ne.0) then
              call error(ifl(2))
              iwhich = 0
          else
              isc10(1) = 602
              isc10(2) = 11
              isc10(3) = 6
              iwhich = 1
          endif
      else if (idst.eq.7) then
c                                  ***** its a circle *****
          if (p1t(4).eq.3.and.p2t(4).eq.3) then
c                  its a circle between two points
c                  call trimci to compute the first and second points
c                  and set up the sc table for geogn2 for ci/pt1,pt2,pt3
              call trimci(lorc,p1desc,p2desc,imod2)
          else
c                  its a circle trimmed to an entity
             call cirdef(lorc,p1desc,p2desc,imod1,imod2)
             if (debug) then
              write(cout,1234)lorct,p1t,p2t,imod1,imod2
1234          format ('cirdef: ',14i4)
              call putmsg (cout,80,19,0)
             endif
          endif
          if (ifl(2).ne.0) then
              call error(ifl(2))
              iwhich = 0
          else
              iwhich = 2 
          endif
      endif
99999 return
      end
c
c **********************************************************************
c **********************************************************************
c **  SUBROUTINE: redefn
c **                                                                  **
c **  FUNCTION: to parse modified REDEF statement.                    **
c **                                                                  **
c **********************************************************************
c **********************************************************************
      subroutine redefn (ierr)
c
      include 'com8a.com'
c
      integer*2 ierr
c
      real*8 v(3),r,stv
      integer*4 keys(8),idir(8),kerr,ksub, sinx
      integer*2 mod(8),nwds,itype,n,nw,i
      integer*2 sist,is11,is12,savinx
      character*64 cnnam, stok
      character*64 sav
c
      data mod /641,642,643,638,639,640,634,653/
c
      cnnam  = '      '
      ierr   = 0
c
c...Save vital variables in case of error to
c...reprocess REDEF by old routine
c
      is11   = ifl(11)
      is12   = ifl(12)
      stv    = TV
      stok = token2
      sinx = ivxsub
      isv    = INX
      do 55 i=1,8,1
         keys(i) = 0
         idir(i) = 0
   55 continue
      i      = 0
      nw     = 3
      iedge  = 0
c
c...Get geo (CV0, (CV1), TR1, (TR2) and near PT if
c...modifier is not used 
c
  100 if (ityp .eq. 2) then 
         if (ist.eq.3 .or. ist.eq.5 .or. ist.eq.7 .or. ist.eq.8) then
            i = i + 1
            if (i .eq. 1) sist = ist
            if (i .eq. 2 .and. ist .eq. 3) i = 3
            if (i .eq. 2 .and. ist .ne. sist) go to 9000
            if ((i .eq. 3 .or. i .eq. 5 .or. i .eq. 7) .and. 
     -         ist .ne. 3) go to 9000
            call gtdesc(tv, keys(i), nwds, itype) 
c
c...Split CV, get new name for 2nd item
c
         else if (ist .eq. 1) then
            if (i .ne. 1) go to 9100
            i = i + 1
         end if
         if (i .eq. 2) then
            cnnam = token2
            ksub  = ivxsub
            if (nxteos) go to 9000
         end if
         if (nxteos) go to 800
c
c...Modifier
c
      else if (ityp .eq. 1) then
         if (ist .eq. 860) then
            if (i .lt. 1) go to 9000
            nw = 3 + 2*iedge 
            if (i .lt. nw) i = nw 
            iedge = iedge + 1
         else if (ist .ne. 1) then
            if (i .eq. 1) i = 2
            i = i + 1
            if (i.ne.3 .and. i.ne.5 .and. i.ne.7) go to 9000
            n   = mindex(mod,ist,8)
            if (n .eq. 0) go to 9000
            if (n .gt. 6 .and. i .ne. 3) go to 9000
            if (n .lt. 7) then
               idir(i) = n
            else
               idir(i) = 7 - n 
            end if
         else
            go to 9000
         end if
         if (nxteos) go to 800
      end if 
c
c...get next token
c
      if (i .lt. 7) then
          if (nxteos) go to 800
          call parsit
          go to 100
      end if
c
  800 if (.not.nxteos) go to 9000
      if (i .lt. 4 .and. i .ne. 1) go to 9000
      if (i .gt. 1) then
          if (nw .eq. 3 .and. iedge .ne. 1) go to 9000 
          if (nw .eq. 5 .and. iedge .ne. 2) go to 9000 
      end if
c
c...make supporting vectors
c
      do 855 j=1,8
         n    = idir(j)
         if (n .le. 0) go to 855
         v(1) = 0.d0
         v(2) = 0.d0
         v(3) = 0.d0
         if (n .gt. 3) then 
            v(n-3) = 1.d0
         else
            v(n) = -1.d0
         end if
         sav    = savid2
         savinx = isvsub 
         savid2 = '@UN   '
         isvsub = 0
         r = 0.
         call ptentt (4,v,keys(j),r)
         savid2 = sav
         isvsub = savinx
  855 continue
      call predef (keys,idir,cnnam,ksub,kerr)
c
c...delete vectors
c
      do 865 j=1,8
         if (idir(j)*keys(j) .gt. 0) call dlgeom (keys(j))
  865 continue
      if (kerr .eq. 383) then
         call error(383) 
      else if (kerr .ne. 0) then
         call error(465) 
      endif
      go to 9999
c
c...restore all parameters for parsing
c...and try old syntax
c
 9000 INX  = isv 
      TV   = stv
      token2 = stok
      ivxsub = sinx
      ist  = sist
      ifl(11) = is11
      ifl(12) = is12
      ierr = 1
      go to 9999
c
 9100 call error (9)
c
 9999 return
      end
      
c****************************************************************
c   FUNCTION: mindex (kary,knum,ksiz)
c
c   Function: integer*2 version of index()
c
c****************************************************************
      integer*2 function mindex (kary,knum,ksiz)
c
      integer*2 kary(*),knum,ksiz
c
      integer*2 i
c
      mindex = 0
c
c...Search for 1st occurrence of 'knum' in 'kary'
c
      do 100 i=1,ksiz,1
          if (knum .eq. kary(i)) go to 200
  100 continue
      go to 8000
c
c...Found value
c
  200 mindex = i
c
c...End of routine
c
 8000 return
      end 


