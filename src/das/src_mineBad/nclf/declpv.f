C*********************************************************************
C*    NAME         :  declpv.f
C*       CONTAINS:
C*      subroutine declpv
C*      subroutine geogpv
C*      subroutine determ (gmat,kol,gval)
C*      subroutine trunrf (gbuf)
C*      subroutine vettcv (pto,vco,ierr)
C*      subroutine gtnruv (aswpt,aswsf,isf,u4,v4)
C*      subroutine dclpv1
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       declpv.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:54
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine declpv
C*      this subroutine parses the point declaration statement            *
C*      the valid syntax combinations are:                                *
C*          1. (name)=pntvec/x,y,z,i,j,k                                  *
C*          2. (name)=pntvec/x,y,i,j                                      *
C*          3. (name)=pntvec/pt1,ve1                                      *
C*          4. (name)=pntvec/projct,pv1(,ve1),sf1(,pt1)                   *
C*
C*      the following represents the info passed to geogn1 in syscom:
C*             sc(10)             !       sc(11) thru sc(nn)
C*--------------------------------!----------------------------------------
C* type  subtype  numwds  notused !   sc(11)       sc(12)       sc(13)
C* ----  -------  ------  ------- !   ------       ------       ------
C*   *      1       *        *    !   x val        y val        z val
C*   *      1       *        *    !   x val        y val          *
C*   *      2       *        *    !   pt1 loc      ve1 loc        * 
C*   *      3       *        *    !   pt1 loc      pt2 loc        * 
C*   *      4       *        *    !   ln1 loc         *           * 
C*  mv2     5       *        *    !      *            *           * 
C*   *      6       *        *    !   ci1 loc         *           * 
C*  mv5     7       *        *    !   pl1 loc      pl2 loc      pl3 loc 
C*   *      8       *        *    !   cv1 loc      dis val      pt2 opt 
C*  mv2     9       *        *    !   pv1 loc      ve1 loc        * 
C*  mv2    10       *        *    !   pv1 loc      ve2 loc        * 
C*  mv1    11       *        *    !   pv1 loc         *           * 
C*  mv1    12       *        *    !   pl1 loc         *           * 
C*   *     13       *        *    !   pt1 loc      sf1 loc        * 
C*  mv2    14       *        *    !   pv1 loc        val          * 
C*  mv2    15       *        *    !   pv1 loc        val          * 
C*   *     16       *        *    !   pt1 loc      cv1 loc        * 
C*  mv2    17       *        *    !   ve1 loc      pv1 loc        * 
C*  mv2    18       *        *    !   ve1 loc      pv1 loc        * 
C*   *     19       *        *    !   cv1 loc      pt1 loc        *   
C*   *     20       *        *    !   pv1 loc         *           *   
C*   *     21       *        *    !   pn1 loc        val          *   
C*   *     22       *        *    !   pv1 loc      ve1 opt      sf1 loc
c
C*
C**************************************************************************
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C**************************************************************************
C
      subroutine declpv
c
      include 'com8a.com'
      include 'suvcom.com'
c
      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      integer*2 irest(4)
      real*8 rtok
      equivalence (token2,leq1),(rtok,leq1),(rest,irest)

      real*8 tv_save
      integer*4 inum,nclkey
      real*4 dsu1,dsv1
      integer*2 ietype,primtyp,istsv
      logical luv1
c
      integer*2 ipro(6,25),ispe(6,6),ie,i8, iret, ntmp
      integer*2 i, j, n, m, nwds, iscl, ics, icw, nvs(6), lln(25)
c
      data ipro /  2,  2,  2,  2,  2,  2,   3,  -4,  0,  0,  0,  0,
     -             3,  3,  0,  0,  0,  0,   5,  0,  0,  0,  0,  0,
     -           650, -1,  0,  0,  0,  0, 634,  7,  0,  0,  0,  0,
     -           727,  6,  6,  6, -3,  0,   8,  2,  3,  0,  0,  0,
     -            21, -2,  -4,  0,  0,  0,  21,819,  -4,  0,  0,  0,
     -            30, 21,  0,  0,  0,  0, 630,  6,  0,  0,  0,  0,
     -             -5,  9,  0,  0,  0,  0,  21, 28,  2,  0,  0,  0,
     -            21,666,  2,  0,  0,  0,   3,  8,  0,  0,  0,  0,
     -             4, -2, 21,  0,  0,  0,   4,819, 21,  0,  0,  0,
     -             8,  3,  0,  0,  0,  0,  21,  0,  0,  0,  0,  0,
     -            20,  2,  0,  0,  0,  0,   3, -6,  6,  0,  0,  0,
     -            21, -6,  6,  0,  0,  0,   0,  0,  0,  0,  0,  0,
     -             9,  0,  0,  0,  0,  0 /
c
      data ispe /721,651,  0,  0,  0,  0,  19, 10,  0,  0,  0,  0,
     -           654,655,656,657,658,659,   4, 21,  0,  0,  0,  0,
     -           3,21,0,0,0,0,            630,646,  0,  0,  0,  0 /
      data lln /6,2,2,1,2,3,5,3,3,3,2,2,2,3,3,2,3,3,2,1,2,5,5,5,1/
c
      integer*2 PERPTOV, TANTOV, ONV, OFFV
      parameter(PERPTOV=630,TANTOV=646,ONV=71,OFFV=72)

      character*64 svtkn
      integer*2 idst0

      idst0 = idst
      svtkn = savid2
      sc(199) = 0
c 
c...PNTVEC encountered (subtype 21)
c
      if (vocab .and. ist.eq.ONV) then
        call dclpv1
        return
      endif
      ie     = 0
      if (IDST .ne. 21)  go to 8000
      nwds   = 0
      ics    = 0
      icw    = 0
      iscl   = 1
      i8     = 8
      luv1   = .false.
      dsu1 = .5
      dsv1 = .5
      isc10(1) = 0
c
c...Set to zero so near point is reset - Andrew 12/17/12
c
      sc(15) = 0
      sc(16) = 0
      do 6900 i=1,100
         if (ITYP .eq. 7) go to 7000
         if (iscl .eq. 4) go to 8800
         nwds = nwds + 1
         if (nwds .gt. 2 .and. lln(iscl) .lt. nwds) go to 8800
         if (ITYP .eq. 2 .or. ITYP .eq. 1) then
c
c...Get first parameter and try to set syntax type
c
            if (nwds .eq. 1) then
               if (ITYP .eq. 2) then
                  if (IST .eq. 3) iscl = 2 
                  if (IST .eq. 4) iscl = 17 
                  if (IST .eq. 5) iscl = 4 
                  if (IST .eq. 8) iscl = 8 
                  if (IST .eq. 20) iscl = 21 
                  if (IST .eq. 21) iscl = 9 
                  if (IST .eq. 9) iscl = 25
               else 
                  if (IST .eq. 650) iscl = 5 
                  if (IST .eq. 634) iscl = 6 
                  if (IST .eq. 727) iscl = 7 
                  if (IST .eq.  30) iscl = 11 
                  if (IST .eq. 630) iscl = 12 
                  if (IST .eq. 888) then
                     if (NXTEOS) then
                        call error (20)
                        goto 9000
                     endif
                     iscl = 24
                  endif
               end if
               if (iscl .eq. 8) then
                  tv_save = TV
                  call parsuv (i8,luv1,dsu1,dsv1)
c
c... aak Oct-13-97: restore IST, ITYP and TV changed by parsuv
c
                  IST = 8
                  ITYP = 2
                  TV = tv_save
               endif

               if (NXTEOS .and. iscl .eq. 9) iscl = 20
            end if
c
c...Get second parameter and correct syntax type
c
            if (nwds .eq. 2) then
               if (ITYP .eq. 2) then
                  if (iscl .eq. 2 .or. iscl .eq. 9) then
                     if (IST .eq. 3) iscl = 3 
                     if (IST .eq. 8) then
                         iscl = 16 
                         tv_save = TV
                         call parsuv (i8,luv1,dsu1,dsv1)
c
c... aak Oct-13-97: restore IST, ITYP and TV changed by parsuv
c
                         IST  = 8
                         ITYP = 2
                         TV   = tv_save
                     end if
                     if (IST .eq. 9) then
                         iscl = 13 
                         tv_save = TV
                         iret = 9
                         call parsuv (iret,luv1,dsu1,dsv1)
                         IST  = 9
                         ITYP = 2
                         TV   = tv_save
                         if (iret.ne.0) then
                           ie = iret
                           goto 8900
                         endif
                     end if
                  else if (iscl .eq. 8) then
                     if (IST .eq. 3) iscl = 19
                  end if 
c
c... Get the point for PV/PROJCT,PT,SF,...
c... Check to see if user did not include anything else,
c... for example, pv/projct,pt8   (This needs to be an error.)
c
                  if (iscl.eq.24) then
                     if (ist.ne.3 .and. ist.ne.21) then
                        call error (20)
                        go to 9000
                     endif
                     if (NXTEOS) then
                        call error (186)
                        go to 9000
                     endif
                  endif
               else if (ITYP .eq. 1) then
                  if (iscl .eq. 9) then
                     if (IST .eq. 819) iscl = 10 
                     if (IST .eq.  28) iscl = 14 
                     if (IST .eq. 666) iscl = 15
C
C...Added for the command PV/(pt/pv),perpto,pl.  JLS 4/15/99
C
                     if (IST.eq.PERPTOV .or. IST.eq.TANTOV) iscl = 23
                  else if (iscl .eq. 2) then
                     if (IST.eq.PERPTOV .or. IST.eq.TANTOV) iscl = 22
                  end if
                  if (iscl .eq. 17) then
                     if (IST .eq. 819) iscl = 18 
                  end if
               end if
            end if
            if (iscl.eq.22 .or. iscl.eq.23) then
              if (nwds.ge.3.and. ITYP.eq.2) then
                if (IST.eq.CURVE .or. IST.eq.SURF) then
                  tv_save = TV
                  istsv   = IST
                  iret    = IST
                  luv1    = .false.
                  call parsuv (iret, luv1, dsu1, dsv1)
                  if (iret.gt.0) then
                     call error (iret)
                     goto 9000
                  endif
                  ITYP = 2
                  IST  = istsv
                  TV   = tv_save
                endif
              endif
            endif
c
c... PNTVEC/PROJCT,PT(,VE),SF(,PT)
c... Make a point vector by projecting a point onto a surface and having
c... the normal of that surface be the vector.  Take the normal in the
c... direction of the projecting point.
c... Let the NXTEOS flag handle stopping routine when all input parsed.
c
            if (nwds.eq.3 .and. iscl.eq.24) then
               sc(16) = 0
               if (geom .and. (ist.eq.4.or.ist.eq.21)) then
                  icw = icw+1
                  sc(10+icw) = tv
                  nwds = nwds +1
                  call parsit
               endif
               if (.not.geom.or.
     x            (IST.ne.6.and.IST.ne.9.and.IST.ne.33)) then
                  call error (186)
                  goto 9000
               endif
               sc(icw+11) = tv
               if (IST.eq.9) then
                  iret = 9
                  luv1 = .FALSE.
                  call parsuv (iret, luv1, dsu1, dsv1)
c
c... In case u, v parameters included with surface (ex. sf1[0.2,0.4])
c... luv1 will be true.  Otherwise, luv1 = false, and u = v = 0.5.
c... parsuv will modify the global variable tv, losing all info of surf
c... If no u, v given, tv won't be changed, but if given tv -> 0.0.
c... Just skip everything in the loop; take care of it here.
c
                  if (iret.gt.0) then
                     call error (iret)
                     goto 9000
                  endif
                  if (luv1) then
                     sc(icw+12) = dsu1
                     sc(icw+13) = dsv1
                  else
                     sc(icw+12) = 0.5
                     sc(icw+13) = 0.5
                  endif
               endif
               icw = icw + 3
               if (nxteos) goto 7900
               call parsit
c
c..... possible near point
c
               if (.not. (geom. and. (IST.eq.3 .or. IST.eq.21))) then
                  call error (20)
                  goto 9000
               endif
               sc(icw+11) = tv
c
c..... end of statement.
c
               if (.not. nxteos) then
                  call error (4)
                  goto 9000
               endif
               goto 7900
            endif      
c
c...  End PNTVEC/PROJCT,PT(,VE),SF(,PT)
c
         end if
         ics   = ics + 1
         nvs(ics) = IST
c
c...vp 13-aug-97 process scalar first so ITYP=2 will not
c...take control over
c...store scalar value
c
         if (SCALAR) then
            icw = icw + 1
            if (icw .gt. 6) go to 8800
            sc(10+icw) = TV
            nvs(ics) = 2
            inum = itv
c
c...Store token value (reference)
c
         else if (ITYP .eq. 2) then
            icw = icw + 1
            SC(10+icw) = TV
         else
            if (ITYP .ne. 1) go to 8000
            if (isc10(1).eq.0 .or. isc10(1).eq.727) then
              isc10(1) = nvs(ics)
            else
              isc10(4) = nvs(ics)
            end if
         end if
 6890    isc10(2) = iscl
         if (NXTEOS) go to 7000
         call parsit
         if (geom.and.IST.eq.SURF) then
           if (iscl.eq.7.or.iscl.eq.12) then
             call gtdesc(tv,nclkey,ntmp,ietype)
             call ncl_get_sf_primtyp(nclkey,primtyp)
             if (primtyp.ne.3) then
               ie = 19
               go to 8900
             endif
             IST = PLANE
           endif
         endif
 6900 continue
c
c...End of statement
c...check syntax errors
c
 7000 continue
      if (iscl.eq.24) goto 7900
      if (iscl.ne.1.and.(iscl.ne.6.or.nwds.ne.2).and. iscl .ne. 8 .and.
     -    iscl .ne. 22 .and. iscl .ne. 23 .and. nwds .lt. lln(iscl))
     -       go to 8000 

      if (iscl .eq. 8) isc10(3) = nwds - 1
      if (iscl .eq. 21) isc10(3) = inum 
      if (iscl .eq. 1 .and. nwds .ne. 5 .and.
     -    nwds .ne. 6) go to 8000
      if ((iscl.eq.22 .or. iscl.eq.23) .and.
     -    (nwds .lt. 3 .or. nwds .gt. 5)) go to 8000
c
c...If PT1,CV1 store u value if specified
c
      if (iscl .eq. 16 .or. iscl .eq. 19) then
         if (i8 .eq. 0) then
            if (luv1) then 
               dsu  = dsu1
               dsuv = luv1
            end if
         else
            ie   = i8
            go to 8900
         end if
      end if
      if (iscl .eq. 13 .or. iscl.eq.22 .or. iscl.eq.23) then
         if (luv1) then 
            dsu  = dsu1
            dsv  = dsv1
            dsuv = luv1
         end if
      end if
c
      if (iscl.eq.22.or.iscl.eq.23) then
        j = 3
        if (nvs(3).eq.ONV.or.nvs(3).eq.OFFV) then
          if (nwds.lt.4) then
            ie = 56
            goto 8900
          endif
          j = 4
        endif
        if (nvs(j).ne.LINE   .and. nvs(j).ne.PLANE .and.
     x      nvs(j).ne.CIRCLE .and. nvs(j).ne.CURVE .and.
     x      nvs(j).ne.SURF) then
          ie = 244
          goto 8900
        endif
        if (nwds.eq.j+1) then
          if (nvs(j+1).ne.POINT) then
            ie = 20
            goto 8900
          endif
          isc10(3) = 1
        endif
        goto 7900
      endif
c
c...Added for PV/CENTER,CI[,planar]
c...Added for PV/CENTER,CV[,planar]
c...Added for PV/CENTER,SF[,planar]
c
      if (nvs(1) .eq. 634) then
        if (nvs(2) .eq. 7 .or. nvs(2) .eq. 8 .or. nvs(2) .eq. 9) then
          if (ist.eq.7 .or. ist .eq. 8 .or. ist .eq. 9) then
            goto 7900
          else
            ie = 420
          endif
c
c...Get name of var to store if curve/surface is planar
c
          if (ityp.eq.2.and.(ist.eq.1.or.ist.eq.2)) then
c
c...Scalar (or unknown - make it a scalar)
c
            idst = 2
            ist = 2
            savid2 = token2
            ifl(9)  = ifl(11)
            ifl(10) = ifl(12)
            rest = 0.
            keyold = keyhld
            istold = ist
            call vstore
            sc164 = token2
            sc(199) = 1
            idst = idst0
            savid2 = svtkn
c
c...Simple variable expected
c
          else
            call error (282)
            goto 9000
          endif
          goto 7900 
        endif
      endif
c
c...Check minor words and allowed 
c...types of parameters 
c
      do 7590 i=1,nwds,1
         if (ipro(i,iscl) .lt. 0) go to 7200
         if (ipro(i,iscl) .ne. nvs(i)) go to 7600
         go to 7590
 7200    n  = 0 - ipro(i,iscl)
         do 7250 j=1,6,1
             if (ispe(j,n) .eq. 0) go to 7650
             if (ispe(j,n) .eq. nvs(i)) go to 7590
 7250    continue
         go to 7650
 7590 continue
      go to 7700
c
c...Syntax error,
c...specify what is expected
c
 7600 m     = ipro(i,iscl)
      ie    = 12
      if (m .eq. 2) ie = 7 
      if (m .eq. 4) ie = 11 
      if (m .eq. 7) ie = 159 
      if (m .eq. 6) ie = 19 
      if (m .eq. 21) ie = 443
      go to 8900
 7650 m     = ispe(j-1,n)
      ie    = 12
      if (m .eq. 659) ie = 18
      if (m .eq. 10)  ie = 461
      if (m .eq. 651) ie = 433
      go to 8900
c
c...Canonical format
c
 7700 if (iscl .ne. 1) go to 7900
      if (nwds .eq. 5) then
         sc(16) = sc(15) 
         sc(15) = sc(14)
         sc(14) = sc(13)
         sc(13) = 0.0
         call chkzsf (sc(11))
         if (ifl(2) .ne. 0) go to 8300
      else if (nwds .ne. 6) then   
         go to 8000
      end if
      if (iscl .eq. 2) go to 7900
      if (sc(14)**2+sc(15)**2+sc(16)**2 .lt. 1.d-6)
     -    go to 8500
c
c...Store definition
c
 7900 call geogpv
      go to 9000
c
c...Errors
c
 8000 ie     = 12 
      go to 8900
c
 8300 call error (ifl(2))
      go to 9000
c
 8500 ie     = 49 
      go to 8900
c
 8800 ie     = 4
 8900 call error (ie)
c
 9000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine geogpv
C*       prepare and store pointvector canon lists.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine geogpv

      include 'com8a.com'
      include 'mocom.com'
      include 'suvcom.com'
c
      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld,key0
      integer*2 istold, idstsv,ist0
      character*64 labsav 
      integer*2 isc(10),ktv(4),prjvfl,nerptf,normfl,isrf,plnr,iflg
      real*8 w(16),vln,v(4,3),rnum,v1(3),ptvect(6), scale
      real*8 spacpt(6), nearpt(6), projvc(3), surfu, surfv
      real*8 plt(4), plpt(3), f_dot, f_mag
      equivalence (sc(10),isc),(tv,ktv)
      equivalence (ifl(54),isrf)
      real*4 u4, v4
      integer*4 jsb,nclkey,nkey1,nkey2,nkey3,nkey4,ipnt,srfkey
      integer*4 kerr
      integer*2 i,ietype,ier,nwds,nw,n,primtyp
      logical trflg
      integer*2 izero/0/,i2v3/3/
      integer*2 PERPTOV, TANTOV, ONV, OFFV
      parameter(PERPTOV=630,TANTOV=646,ONV=71,OFFV=72)

c
      trflg = .true.
      jsb   = isc(2)
      ier   = 0

      go to (100,200,200,400,500,600,700,800,900,
     -       1000,1100,1200,1300,1400,1500,1600,
     -       1700,1800,1900,2000,2100,2200,2200,2400,2500) jsb
c
c...x,y,z,i,j,k data in sc() 
c
  100 do 125 i=1,6
          w(i)=sc(i+10)
  125 continue
      go to 7000
c
c...pt,ve in data base
c
  200 call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call gtentt(sc(12), trflg, nclkey, ietype, w(4))
      if (ietype .eq. 21) then
          w(4) = w(7)
          w(5) = w(8)
          w(6) = w(9)
      endif
      if (jsb .eq. 2) go to 7000
c
c...pt1,pt2
c
  300 w(4)  = w(4) - w(1)
      w(5)  = w(5) - w(2)
      w(6)  = w(6) - w(3)
      vln   = w(4)**2 + w(5)**2 + w(6)**2
      if (vln .lt. 1.d-4) ier = 317
      go to 7000
c
c...ln
c
  400 call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      go to 7000
c
c...TE,TA or FWD
c
  500 if (isc(4) .eq. 721) then
          w(4) = sc(4)
          w(5) = sc(5)
          w(6) = sc(6)
      else if (isc(4) .eq. 651) then
          w(4) = sc(7)
          w(5) = sc(8)
          w(6) = sc(9)
      end if
      w(1)  = sc(1)
      w(2)  = sc(2)
      w(3)  = sc(3)
      go to 7000
c
c...CENTER,CI
c
  600 call gtdesc(sc(11),nclkey,nwds,ietype)
      if  (ietype .eq. 7) then
        call gtentt(sc(11), trflg, nclkey, ietype, w(1))
        plnr = 1
        go to 7000
      endif
c
c...Added PV/CENTER,CV[,planar]
c...Added PV/CENTER,SF[,planar]
c
      if  (ietype .eq. 8) then
          call plcvsf (nclkey,1,w,plnr,iflg)
          if (iflg .eq. 1) then
              ier = 163
          endif
      else 
          call plcvsf (nclkey,0,w,plnr,iflg)
          if (iflg .eq. 1) then
              ier = 163
          endif
      endif
      go to 7000
c
c...INTOF,pl1,pl2,pl3,modifier
c
c  700 call gtentt(sc(11), trflg, nclkey, ietype, v(1,1))
c      call gtentt(sc(12), trflg, nclkey, ietype, v(1,2))
c      call gtentt(sc(13), trflg, nclkey, ietype, v(1,3))
  700 continue
      call gtplt (sc(11), ifl(72), v(1,1))
      call gtplt (sc(12), ifl(72), v(1,2))
      call gtplt (sc(13), ifl(72), v(1,3))
c
c......get vector of intersection line of pl1 and pl2 
c
      w(4) = v(2,1)*v(3,2) - v(3,1)*v(2,2)
      w(5) = v(3,1)*v(1,2) - v(1,1)*v(3,2)
      w(6) = v(1,1)*v(2,2) - v(2,1)*v(1,2)
c
c......apply modifier and reverse if necessary
c
      n    = isc(1) - 653
      if (n .gt. 3) then
          n   = n - 3
          rnum = -1.d0
      else
          rnum = 1.d0
      end if
c
c.....zero value causes error
c
      if (w(n+3) .eq. 0) then
        ier = 213
        go to 8000
      endif
      rnum   = w(n+3)*rnum
      if (dabs(rnum) .lt. 1.d-6) go to 750
      if (rnum .lt. 0.d0) then
         w(4) = -w(4)
         w(5) = -w(5)
         w(6) = -w(6)
      end if
c
c......get intersecting point
c
      call determ (v,0,rnum)
      if (dabs(rnum) .lt. 1.d-6) go to 750 
      call determ (v,1,w(1))
      call determ (v,2,w(2))
      call determ (v,3,w(3))
      w(1) = w(1) / rnum
      w(2) = w(2) / rnum
      w(3) = w(3) / rnum
      go to 7000
c
  750 ier    = 444
      go to 8000
c
c...CV,distance
c......use PT/CV,dist logic to get point
c
  800 call geogne (w)
      ier   = ifl(2)
      go to 7000
c
c...PV,plus/minus,VE
c
  900 call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call gtentt(sc(12), trflg, nclkey, ietype, w(7))
      if (ietype .eq. 21) then
          w(7) = w(10)
          w(8) = w(11)
          w(9) = w(12)
      endif
      rnum  = 1.d0
      if (isc(1) .eq. 10) rnum = -1.d0
      w(4)  = w(4) + rnum*w(7)
      w(5)  = w(5) + rnum*w(8)
      w(6)  = w(6) + rnum*w(9)
  990 rnum  = w(4)**2 + w(5)**2 + w(6)**2
      if (rnum .gt. 1.d-10) go to 7000
      ier   = 317
      go to 8000
c
c...PV,cross,VE
c
 1000 call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call gtentt(sc(12), trflg, nclkey, ietype, w(7))
      if (ietype .eq. 21) then
          w(7) = w(10)
          w(8) = w(11)
          w(9) = w(12)
      endif
      v1(1) = w(4)
      v1(2) = w(5)
      v1(3) = w(6)
      w(4)  = v1(2) * w(9) - v1(3) * w(8)
      w(5)  = v1(3) * w(7) - v1(1) * w(9)
      w(6)  = v1(1) * w(8) - v1(2) * w(7)
      go to 990 
c
c...unit,PV
c
 1100 call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      rnum   = dsqrt(w(4)**2 + w(5)**2 + w(6)**2)
      w(4)   = w(4) / rnum
      w(5)   = w(5) / rnum
      w(6)   = w(6) / rnum
      go to 7000
c
c...perpto,PL
c
c 1200 call gtentt(sc(11), trflg, nclkey, ietype, w(4))
 1200 continue
      call gtplt (sc(11), ifl(72), w(4))
      call gtdesc(sc(11), nclkey, nwds, ietype)
      if (ietype.eq.PLANE) then
        call gtdpt (nclkey,w(1))
      else
        call vctmsc(w(4),w(1),w(7))
      endif
      call trunrf (w(1))
      go to 7000
c
c...PT,SF
c
 1300 call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call sfptdo (w(4))
      if (ifl(72) .eq. 1) call transf (w(4),sc(68),3,4)
      ier   = ifl(2)
      go to 7000
c
c...PV,times,n
c
 1400 call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      if (sc(12) .eq. 0.d0) go to 990
      w(4)  = w(4) * sc(12)
      w(5)  = w(5) * sc(12)
      w(6)  = w(6) * sc(12)
      go to 7000
c
c...PV,offset,n
c
 1500 call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      rnum   = dsqrt (w(4)**2 + w(5)**2 + w(6)**2)
      w(1)  = w(1) + sc(12) * w(4) / rnum
      w(2)  = w(2) + sc(12) * w(5) / rnum
      w(3)  = w(3) + sc(12) * w(6) / rnum
      go to 7000
c
c...PT,CV store without refsys
c
 1600 trflg  = .false.
      call gtentt (sc(11), trflg, nclkey, ietype, w(1))
      call vettcv (w(7),w(4),ier) 
      if (ier .ne. 0) go to 8000
      go to 7100
c
c...VE,plus/minus,PV
c
 1700 call gtentt(sc(12), trflg, nclkey, ietype, w(1))
      call gtentt(sc(11), trflg, nclkey, ietype, w(7))
      rnum   = 1.d0
      if (isc(1) .eq. 10) rnum = -1.d0
      w(4)  = w(7) + rnum*w(4)
      w(5)  = w(8) + rnum*w(5)
      w(6)  = w(9) + rnum*w(6)
      go to 990
c
c...VE,cross,PV
c
 1800 call gtentt(sc(12), trflg, nclkey, ietype, w(1))
      call gtentt(sc(11), trflg, nclkey, ietype, w(7))
      v1(1) = w(4)
      v1(2) = w(5)
      v1(3) = w(6)
      w(4)  = v1(3) * w(8) - v1(2) * w(9)
      w(5)  = v1(1) * w(9) - v1(3) * w(7)
      w(6)  = v1(2) * w(7) - v1(1) * w(8)
      go to 990 
c
c...CV,PT
c
 1900 rnum   = sc(11)
      sc(11) = sc(12)
      sc(12) = rnum 
      go to 1600
c
c...PV
c
 2000 sc(12) = 1.0
      go to 1400 
c
c...PN,num
c
 2100 call gtpnnp (sc(11),nw,ipnt)
      if (ipnt .ne. 2) ier = 426
      n     = isc10(3)
      if (n .lt. 1 .or. n .gt. nw) ier = 340
      if (ier .ne. 0) go to 8000 
      call gpnptt (w,sc(11),n,trflg)
      go to 7000
C
C...(PT|PV),(PERPTO|TANTO),(ON|OFF),(LN|PL|CI|CV|SF)
C
 2200 continue
      trflg = .false.
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call gtdesc(sc(12), nclkey, nwds, ietype)
c
c...Plane or surface
c
      if (ietype.eq.PLANE .or. ietype.eq.SURF) then
        if (isc10(1).eq.TANTOV) goto 9005
c
c...get plane or sf normal plane
c
        call ncl_get_sf_primtyp(nclkey,primtyp)
        if (ietype.eq.PLANE .or. primtyp.eq.3) then
          call gtplt (sc(12), izero, w(4))
        else
          isrf = 2
          if (dsuv .or. isc10(3).eq.0) then
            call getsuv (sc(12),isrf,u4,v4)
          else
            call gtnruv (sc(13),sc(12),isrf,u4,v4)
          endif
          call conv8_4(w(1),s(8,isrf),3)
          call sfinit(sc(12),isrf,u4,v4)
          call surfpn(u4,v4,1)
          call conv4_8(s(1,isrf),w(4),4)
        endif
c
c...project external point onto plane and direct vector towards plane.
c
        call plnint(w(1),w(4),w(4),plpt,kerr)
        if (kerr.gt.0) goto 9005
        if (ietype.eq.SURF) then
          call vcmnvc(plpt,w(1),w(10))
          if (f_dot(w(4),w(10)).lt.0) then
            rnum = -1.d0
            call vctmsc(w(4),w(4),rnum)
          endif
        endif
c
c...Curve, circle or line
c
      else
        if (ietype.eq.LINE) then
          call gtentt(sc(12), trflg, nclkey, ietype, w(7))
          call unitvc(w(10),plt)
          plt(4) = f_dot(w(1),plt)
          call plnint(w(7),plt,plt,plpt,kerr)
          if (kerr.gt.0) goto 9163
        else
          call vettcv (plpt,plt,ier)
          if (ier .ne. 0) go to 8000
        endif
        if (isc10(1).eq.PERPTOV) then
          call vcmnvc(plpt,w(1),w(10))
          if (f_mag(w(10)).lt.sc(27)) goto 9317
          call unitvc(w(10),w(4))
          if (ietype .ne. LINE) then
            plt(4) = f_dot (w(4),plt)
            if (dabs(plt(4)) .gt. 0.0087265) then
              call uvcplvc(w(4),plt,w(4),-plt(4))
              call unitvc(w(4),w(4))
              if (isc10(4).eq.ONV) then
                plt(4) = f_dot(w(1),plt)
                call plnint(plpt,plt,plt,plpt,kerr)
                if (kerr.gt.0) goto 9163
              endif
            endif
          endif
        else
          call unitvc(plt,w(4))
        endif
      endif
      if (isc10(4).eq.ONV) then
        call vctovc(plpt,w(1))
      endif
      go to 7100

c
c... PNTVEC/PROJCT,PT(,VE),SF(,PT)    Ed Ames 4 Aug 00
c... Make a point vector by projecting a point onto a surface.
c... The vector is the normal to the surface at that point in direction
c... of projecting point.
c
 2400 call fill_array(spacpt, 3, 0.0)
      call fill_array(projvc, 3, 0.0)
      call fill_array(nearpt, 3, 0.0)
      call fill_array(ptvect, 6, 0.0)
      prjvfl = 0
      nerptf = 0
c
      call gtdesc (sc(11),nkey1,nwds,ietype)
      call gtgeo (nkey1, spacpt)
      call gtdesc (sc(12),nkey2,nwds,ietype)
c
c...  Using the optional projection vector, set the projvec flag to 1.
c
      if ((ietype .eq. VECTOR) .or. (ietype .eq. PNTVEC)) then
         call gtgeo (nkey2, ptvect)
         if (ietype .eq. VECTOR) then
            call vctovc(ptvect, projvc)
         else
            call vctovc(ptvect(4), projvc)
         endif
         prjvfl = 1
c
         call gtdesc (sc(13),nkey3,nwds,ietype)
         call gtdesc (sc(16),nkey4,nwds,ietype)
         srfkey = nkey3
         surfu = sc(14)
         surfv = sc(15)
c
c...  If there is an optional near point, set the nearpt flag to 1 
c...  sc(14) = u and sc(15) = v , the parametric surface coordinates
c
         if ((ietype .eq. POINT) .or. (ietype .eq. PNTVEC)) then
            call gtgeo (nkey4,nearpt)
            nerptf = 1
         endif
      else
c
c...  No projection vector. The nkey2 variable is from the surface.
c
         srfkey = nkey2
         surfu = sc(13)
         surfv = sc(14)
         call gtdesc (sc(15),nkey4,nwds,ietype)
c
c...  If the near point is used, without the projection vec
c...  sc(13) = u, sc(14) = v, parametric surface coordinates.
c
         if ((ietype .eq. POINT) .or. (ietype .eq. PNTVEC)) then
            call gtgeo (nkey4,nearpt)
            nerptf = 1
         endif
      endif
      normfl = 1
      call nclf_pt_project_sf(spacpt, srfkey, surfu, surfv,
     1              prjvfl, projvc, nerptf, nearpt, normfl, w, ier)
c
c...  Transform the point ( w(1:3) ) and the vector ( w(4:6) )
c...  from world coordinate system (used in C code) to 
c...  model coordinate system (used in Fortran code).
c...  Be sure to add units ( ifl(264) ) before adding
c...  refsys ( ifl(72) ).
c
      call wcsmcs(0,w)
      call wcsmcs(1,w(4))
      if (ifl(264) .eq. 1) then
         scale = 25.4
         call vctmsc(w,w, scale)
      endif
      if (ifl(72) .eq. 1) then
         call transf(w, sc(68), 3, POINT)
         call transf(w(4), sc(68), 3, VECTOR)
      endif
      go to 7000
c
c...  End of PNTVEC/PROJCT,PT1(,VE1),SF1[U,V](,PT2)
c
c
c...pntvec/sf
c
 2500 call gtdesc(sc(11), nclkey, nwds, ietype)
      iflg =ncl_pntvec_revsf (nclkey,w)
      if (iflg.ne.0) then
        ifl(2) = 163
      endif
      go to 7000
c
c...Store PV in data base
c
 7000 if (ier .ne. 0) go to 8000
      ktv(3) = 6
      ietype = idst
      if (ietype .eq. 21 .and. sc(199) .ne. 0) then
c
c.... update the planar scalar
c
        idstsv = idst
        labsav = savid2
        ist0 = istold
        key0 = keyold 
c
c...still keep the logic, just fill token2 9-64 with spaces
c
        token2 = sc164
        call vstchk
        rest = tv + plnr
        idst = 2
        keyold = keyhld
        istold = ist
        ifl(9) = ifl(11)
        ifl(10) = ifl(12)
        call vstore
        idst = idstsv
        savid2 = labsav
        istold = ist0
        keyold = key0
      endif
      call ptentt (ietype,w,nclkey,tv)
      rest   = tv
      go to 9000
c
c...Store without refsys
c
 7100 nwds   = 6
      ietype = idst
      call ptgeom (ietype,w,nclkey,nwds)
      call ptdsc3 (nclkey,nwds,ietype,tv)
      if (.not.ldsppv) call blkgeo (nclkey,1)
      rest   = tv
      go to 9000
c
c...Errors
c
 9005 ier = 5
      goto 8000
c                 Impossible geometry case
 9163 ier = 163
      goto 8000
c                 Vector must have real length
 9317 ier = 317
      goto 8000
c
 8000 call error (ier)
c
 9000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine determ (gmat,kol,gval)
C*       calculates value of determinant generated from the 3 X 4
C*       matrix.
C*    PARAMETERS   
C*       INPUT  :  gmat  R*8  D4,3 - input matrix 
C*                 kol   I*2  D1   - column selector: 0 = 1,2,3
C*                                   1 = 4,2,3; 2 = 1,4,3; 3 = 1,2,4.
C*
C*       OUTPUT :  gval  R*8  D1   - determinant value. 
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine determ (gmat,kol,gval)
c
      real*8 gmat(4,3),gval
      integer*2 kol
c
      integer*2 is(3)
c
      is(1) = 1 
      is(2) = 2 
      is(3) = 3 
      if (kol .ne. 0) is(kol) = 4 
c
      gval  = gmat(is(1),1)*gmat(is(2),2)*gmat(is(3),3) +
     -        gmat(is(2),1)*gmat(is(3),2)*gmat(is(1),3) +
     -        gmat(is(3),1)*gmat(is(1),2)*gmat(is(2),3) -
     -        gmat(is(3),1)*gmat(is(2),2)*gmat(is(1),3) -
     -        gmat(is(2),1)*gmat(is(1),2)*gmat(is(3),3) -
     -        gmat(is(1),1)*gmat(is(3),2)*gmat(is(2),3)
      gval  = -gval
c
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine trunrf (gbuf)
C*       Transforms point thru refsys matrix and converts units if appl.
C*    PARAMETERS   
C*       INPUT  : 
C*          gbuf    R*8   D3  -  input point coordinates
C*       OUTPUT :  
C*          gbuf    R*8   D3  -  converted point coordinates
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine  trunrf (gbuf)
c
      include 'com8a.com'
      include 'wrksys.com'
c
      real*8 gbuf(3)
c
      real*8 tm
c
      data tm /25.4/
c
      if (ifl(264) .eq. 1) then
          gbuf(1) = gbuf(1) * tm
          gbuf(2) = gbuf(2) * tm
          gbuf(3) = gbuf(3) * tm
      end if
      if (lwrk) call transf(gbuf,invwrk,3,3)
      if (ifl(72) .eq. 1) call transf (gbuf,sc(68),3,3)
c
      return
      end
c
C*********************************************************************
C*    SUBROUTINE     : subroutine vettcv (pto, vco,ierr)
C*       Generates vector tangent to the curve.
C*    PARAMETERS   
C*       INPUT :
C*          none  
C*       OUTPUT :  
C*          pto     R*8   D3  - output vector components 
C*          vco     R*8   D3  - output vector components 
C*          ierr    I*2   D1  - error status
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine vettcv (pto,vco,ierr)
c
      include 'com8a.com'
      include 'mocom.com'
      include 'suvcom.com'
c
      real*8 pto(3), vco(3)
      integer*2 ierr
c
c          MOTION COMMON EQUIVALENCES
      EQUIVALENCE(IFL(54),ISRF),(IFL(51),IA)
c
      logical trflg
      integer*4 nclkey, kpt
      integer*2 ietype,ipttyp,ksn(4),j,iref
      real*4 ad(300), px,py,pz, vx,vy,vz
      real*8 u(12), uu, vv, dis, adis, du, aa(3), bb(3)
      equivalence (asn,ksn), (d,ad)
c
      ierr   = 0
c
c...Retrieve point without refsys because curvpv ignores refsys.
c
      trflg  = .false.
      call gtdesc(sc(12),nclkey,nwds,ietype)
      if (ietype.eq.CURVE.and.sc(169).gt.9.1499d0 .and. .not. dsuv) then
        isrf = 2
        call evstup(nclkey, isrf)
        j = 11
        if (isc10(3).eq.1) j = 13
        call gtentt(sc(j),trflg,kpt,ipttyp,u)
c
c... find a rough value for the curve parameter near the external point.
c
        dis=1.d12
        du = 1.0d0 / 9.0d0
        vv = 0.d0
        uu = 0.d0
        iref = 0
        do j=1,10
          if (vv.gt.1.0d0) vv = 1.0d0
          call uevcvt (vv,isrf,iref,aa,bb,ifl(2))
          if (ifl(2) .gt. 0) goto 10
          adis = f_dist(u,aa)
          if (adis.lt.dis) then
            dis = adis
            uu = vv
          endif
          vv=vv+du
        enddo
        dsu = uu
        dsuv = .true.
      endif

10    IA     = 1
      call gtentt(sc(11),trflg,kpt,ipttyp,u)
      t(1,IA) = U(1)
      t(2,IA) = U(2)
      t(3,IA) = U(3)
      t(4,IA) = 0.
      t(5,IA) = 0.
      t(6,IA) = 0.
      if (ietype.eq.CURVE) call gtcvhd (nclkey,d(53))
      ASN    = 0.
      KSN(1) = 8
      KSN(2) = ad(105)
      D(51)  = ASN
      D(52)  = SC(12)
      ISRF   = 2
      if (ietype.eq.CURVE) then
        call curvpv (px,py,pz,vx,vy,vz)
        if (ifl(2) .gt. 0) goto 990
        if (auvset) then
           hu(isrf)  = t(19,ia)
           kuv(isrf) = nclkey
        endif
        dsuv   = .false.
      else if (ietype.eq.CIRCLE) then
        call gtgeom (sc(12),d(53),nclkey,nwds,ietype)
        call circpv (px,py,pz,vx,vy,vz)
        if (ifl(2) .gt. 0) goto 990
      else
        ifl(2) = 5
        goto 990
      endif
      pto(1) = px
      pto(2) = py
      pto(3) = pz
      vco(1) = vx
      vco(2) = vy
      vco(3) = vz
      go to 9000
c
c...Error
c
  990 ierr   = ifl(2)
c
 9000 return
      end
c
C*********************************************************************
C*    SUBROUTINE     : subroutine gtnruv (aswpt,aswsf,isf,u4,v4)
C*       Find surface parameters near external point.
C*    PARAMETERS
C*       INPUT :
C*          aswpt   - Near point asw.
C*          aswsf   - Surface asw.
C*          isf     - Surface index.
C*       OUTPUT :
C*          u4      - U value.
C*          v4      - V value.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine gtnruv (aswpt,aswsf,isf,u4,v4)
c
      include 'com8a.com'
      include 'mocom.com'
c
      real*8 aswpt, aswsf
      real*4 u4, v4
      integer*2 isf
c
c          MOTION COMMON EQUIVALENCES
      integer*2 isrf
      EQUIVALENCE(IFL(54),ISRF)
c
      integer*4 nclkey
      integer*2 nwds,ietype,j,k
      real*4 du
      real*8 u8, v8, dis, adis, ptn(6), pvs(12)
      logical trflg
c
      trflg = .false.
      isrf = isf
      call gtentt(aswpt, trflg, nclkey, ietype, ptn)
c
c... find an approximate value for the surface parameters near 
c... the external point.
c
      dis=1.d12
      du = 1.0d0 / 9.0d0
      u8 = 0.d0
      v8 = 0.d0
      call gtdesc(aswsf,nclkey,nwds,ietype)
      call evstup(nclkey, isrf)
      do j=1,10
        if (u8.gt.1.0d0) u8 = 1.0d0
        do k=1,10
          if (v8.gt.1.0d0) v8 = 1.0d0
          call uevsft (u8, v8, isrf, pvs, ifl(2))
          if (ifl(2) .gt. 0) then
            call getsuv(aswsf,isrf,u4,v4)
            goto 10
          endif
          adis = f_dist(ptn,pvs)
          if (adis.lt.dis) then
            dis = adis
            v4 = v8
            u4 = u8
          endif
          v8=v8+du
        enddo
        u8=u8+du
      enddo

10    continue
c
 9000 return
      end

C**************************************************************************
C*        Parse 'PV/ON,sf,u,v' and 'PV/ON,cv,u' commands
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine dclpv1

      include 'com8a.com'

      real*8 w(12)
      real*8 W0,W1
      integer*4 nclkey,nkey1
      integer*2 ietype,ier,nwds,it,lprcnt,ktv(4)

      integer*2 PERCNT
      parameter (PERCNT = 763)
c
      equivalence (tv,ktv)


           W0 = -.0001
           W1 = 1.0001
           lprcnt = 0
           call parsit
           if (ityp .eq. 2 .and. (ist.eq.8 .or. ist .eq. 5 .or.
     1         ist .eq. 7 .or. ist .eq. 9)) then
               it = ist
               sc(11)=tv
               isub = 20
               call parsit
               if (vocab .and. voc.eq.PERCNT) then
                 if (it.eq.5 .or. it.eq.7) then
                   call error (21)
                   goto 99999
                 endif
                 lprcnt = 1
                 W1 = 100.0001
                 call parsit
               endif
               if (.not.scalar .or. tv.lt.W0 .or. tv.gt.W1) then
                   call error (463)
                   go to 99999
               endif
               sc(12) = tv
               if (it .eq. 9) then
                   call parsit
                   if (.not.scalar .or. tv.lt.W0 .or. tv.gt.W1) then
                       call error (463)
                       go to 99999
                   endif
                   isub = 21
                   sc(13) = tv
               endif
           else
               call error(21)
               go to 99999
           endif

      if (ifl(111).eq.0) then
           if (nextyp.ne.11) then
               call error(4)
               go to 99999
           endif
       else if (nextyp.ne.7) then
           call error(4)
           go to 99999
       endif

       call gtdesc (sc(11),nkey1,nwds,ietype)
       it = 1
       if (isub .eq. 20) then
c
c...pv/on,cv1,u
c
         call ncl_pt_on_cv (it,nkey1,lprcnt,sc(12),w(1),ier)
       else
c
c...pv/on,sf1,u,v
c
         call ncl_pt_on_sf (it,nkey1,lprcnt,sc(12),sc(13),w(1),ier)
       endif
       if (ier .ne. 0) call error (466)
       ktv(3) = 6
       call ptnomd(PNTVEC, w, nclkey, tv)

99999  return
       end
