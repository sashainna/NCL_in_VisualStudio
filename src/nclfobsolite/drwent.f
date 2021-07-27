C*********************************************************************
C*    NAME         :  drwent.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       drwent.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:58
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine drwent(nclkey,dtype)
C*      call appropriate geometry draw subroutine
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
      subroutine drwent(nclkey,dtype, tfmat)

c  dtype   type of display

c    3      disply/pt
c    4      disply/ve
c    5      disply/ln
c    6      disply/pl
c    7      disply/ci
c    8      disply/cv
c    9      disply/sf1,m,n                 (m*n pts + stln segs full srf)
c
c   18      disply/shape                   (use toler for pt spacing)
c
c   19      disply/cutter                   disply cutter
c
c   20      disply/patern
c
c   21      disply/pv                      (disply pointvector)
c   23      disply/pt                      all points
c   24      disply/ve                      all vectors
c   25      disply/ln                      all lines
c   27      disply/ci,n                    all circles using 'n' segments
C
c   28      disply/cv,n                    all curves using 'n' segments
c                                            for each curves displayed
c   29      disply/sf,m,n                  all surfaces using 'm' by 'n'
c                                            segments for each surface
c                                            displayed
c   30      disply/all,cn,sm,sn            all geometry defined using 'cn'
c                                            segments for each circle or
c                                            curve and 'sm' by 'sn'
c                                            segments for each surface    is
c                                            displayed
c   38      disply/shape                   all shapes (per toler pt-spacing)

      include 'com8a.com'

      integer*4 nclkey
      integer*2 dtype
		real*8 tfmat(12)
      real*8 dbuf(3)

cunic          if not plotting then exit
cuni      if (ifl(100).ne.1) go to 99999
c           geonam and lnam are invalid at this time.  eventually they
c           need to hold the 'name' string and the length to display the
c           label.
c     write(cout,1010) dtype
c1010  format('dspent type = ', i4)
c     call putmsg(cout,80,1,1)
      if ((dtype.gt.2.and.dtype.lt.6) .or. dtype .eq. 10 .or.
     x       dtype .eq. 19 .or. dtype .eq. 21) then
c
c...Added tranformation
c...Yurong 9/30/97
c
          call dsplge (nclkey,dtype, tfmat)
      else if (dtype.ge.6.and.dtype.le.8) then
          sc(12) = ifl(136)
c
c...Added tranformation
c...Yurong 9/30/97
c
          call dsplcc (nclkey,dtype, tfmat)
      else if (dtype.eq.9) then
c          sc(12) = ifl(137)
c          sc(13) = ifl(138)
           call sftype (nclkey, itype)
           if (itype .eq. 91) then
             call dsplsf (nclkey,dbuf)
           else if (itype .eq. 25) then
             call displq (nclkey,dtype)
           else if (itype .eq. 26) then
             call dspmsh (nclkey,dbuf)
           else if (itype .eq. 27) then
             call dspssf (nclkey)
           endif
      else if (dtype.eq.18) then
c
c...Added tranformation
c...Yurong 9/30/97
c
          call dsplsh (nclkey,dtype, tfmat)
      else if (dtype.eq.20) then
c
c...Added tranformation
c...Yurong 9/30/97
c
          call dsplpn (nclkey, tfmat)
c      else if (dtype.eq.19) then
c          call dsplgr
      endif

99999 return
      end
