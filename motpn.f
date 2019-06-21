c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
C*       motpn.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:33
c**
c*****************************************************
c*
c* COPYRIGHT (c) 1987 MILLS DATA SYSTEMS
c*
c **********************************************************************
c **********************************************************************
c     subroutine name: motpn
c
c     purpose of subroutine:
c            goto a patern
c
c***********************************************************************
      subroutine motpn
 
      include 'com8a.com'
 
      integer*2 ksn(4),i2twd(4),icycle(4),rix,ix
      integer*4 pntyp
      real*8 rbuff(36),twd,tavoid,rapid,cycle,asn,rn
      integer*4 pnkey,jsn(2)
      real*4 r4twd(2)
      equivalence (twd,r4twd,i2twd),(cycle,icycle)
      equivalence (asn,jsn,ksn)
      logical trans
      integer*2 is1,is4
      data is1 /4/, is4 /1/
 
c          its goto/patern....  the ranfil holds the mask of points
c          specifying the status of each point (whether or not it's
c          been omitted or retained and if it has an avoid value.
 
      trans = .false.
      rix = 0
      asn=sc(11)
      pnkey=jsn(1)
c          set up rapid for avoided points
      rapid = 0.
      cycle = 0.
      if (isc10(4).eq.0) then
          istart=1
          call gtpnnp(pnkey,istop,pntyp)
          incr=1
      else if (isc10(4).eq.1) then
c                               ***** invers
          call gtpnnp(pnkey,istart,pntyp)
          istop=1
          incr=-1
      endif
c
c...vp 19-apr-93 set isc10(2) according to patern type
c...so motimm will think its a norm goto (1 - pts, 2 - pt & ve)
c
      isc10(2) = pntyp
      ix     = 0
      if (pntyp .eq. 2) ix = 10
c                 read first ranfil record containing mask
      call getran(rbuff,ifl(4)+2)
      do 100 i=istart,istop,incr
c                    check mask to see if point should be output
         call gtranw(rbuff,twd,i,rix)
         if (i2twd(1).eq.0) then
c     changed routine name from gtpnpt to gpnptt on: 3/4/88 by: kathy
             call gpnptt(sc(11),pnkey,i,trans)
             call motimm
             ifl(130)=0
c RAH: added call to set intr flag - effective on SGI ONLY
             call ckintr(ifl(86),ifl(35))
             if (ifl(86).ne.0) then
                  ifl(2) = 149
                  go to 99999
             endif
             if (i2twd(2).eq.1) then
c                         *** this point has been avoided.  calculate
c                             the retract point, then the stepover point.
                 tavoid = r4twd(2)
                 do 40 j=i+incr,istop,incr
                    call gtranw(rbuff,twd,j,rix)
                    if (i2twd(1).eq.0) then
c                         *** this is the next point from which to calculate
c                             the stepover point.  first, output a
c                             cycle/off,rapid.
c
c...not use icycle(4) but use icycle(is4) because
c...WNT will act different
c...Yurong 3/28/00
c
c                        icycle(4) = 72
                         icycle(is1) = 0
                         icycle(is4) = 72
                        call putcl(2000,1054,2,cycle)
                        call putcl(2000,5,1,rapid)
                        sc(11) = sc(11) + tavoid * sc(4)
                        sc(12) = sc(12) + tavoid * sc(5)
                        sc(13) = sc(13) + tavoid * sc(6)
                        call motimm
                        ifl(130)=0
c
c                          **** now calculate stepover point.
c   changed the routine name from gtpnpt to gpnptt on: 3/4/88 by: kathy
c
                        call gpnptt(sc(11),pnkey,j,trans)
c
c...vp 20-apr-93 unitize vector since PV in patern is not unit vector
c
                        rn = dsqrt(sc(ix+4)**2+sc(ix+5)**2+sc(ix+6)**2)
                        rn = tavoid / rn
                        sc(11) = sc(11) + rn * sc(ix+4)
                        sc(12) = sc(12) + rn * sc(ix+5)
                        sc(13) = sc(13) + rn * sc(ix+6)
                        call putcl(2000,5,1,rapid)
                        call motimm
                        ifl(130)=0
c
c...not use icycle(4) but use icycle(is4) because
c...WNT will act different
c...Yurong 3/28/00
c
c                        icycle(4) = 71
                        icycle(is1) = 0
                        icycle(is4) = 71
                        call putcl(2000,1054,2,cycle)
                        go to 50
                    endif
40              continue
50              continue
             endif
         endif
100   continue
 
99999 return
      end
