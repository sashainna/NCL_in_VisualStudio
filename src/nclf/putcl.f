C*********************************************************************
C*    NAME         :  putcl.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       putcl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:32
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine putcl(icl,isubcl,numitm,rdata)
C*       description
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
      subroutine putcl(icl,isubcl,numitm,rdata)

      include 'com8a.com'
      include 'cutter.com'
      include 'mocom.com'

      integer*2 icl,isubcl,numitm,iclf,itr
c
      real*8 rdata(640),sec,cnv,rcldat(640),rval
c
      integer*4 iclw(6),jcldat(240),jval(2),iline,iltyp,inc,nlin
      integer*4 istrt,ien,jinc,k,n,idir,ipx,it,im,id
c
      equivalence (rcldat,jcldat), (rval,jval)
c
c...Just previewing motion command
c...Don't output to clfile
c
cc      if (ifl(216) .eq. 1) go to 99999
c
c...vp 12/19/96
c...set translation flag for position record types 
c
      itr    = 0
      iclf   = 0
c
c...DNTCUT is in effect
c...Change motion type
c...Used to ignore DNTCUT records
c...Bobby  -  11/30/92
c
      ictyp  = icl
      if (icl .eq. 3000 .or. icl .eq. 5000 .or. icl .eq. 5200 .or.
     1    icl .eq. 5210) itr = 1
      if (ifl(42) .eq. 1 .and. (icl .eq. 5000 .or. icl .eq. 3000 .or.
     1    icl .eq. 5200)) ictyp = ictyp + 100
c
c...Set up clfile record
c......ISN
c......Implicit check surface is in effect
c......Store saved ISN's from previous motion command
c
      if (ifl(215) .gt. 0) then
          call nclf_src_rec_to_line (ifl4(13),iclw(1))
          iclw(2) = nline - ifl(123)
c
c.........Beginning ISN is current ISN minus
c.........number of continuation lines
c
      else
          iclw(1) = nline-ifl(123)+1
          iclw(2) = nline
      endif
c
c......First record of a motion command
c......Output a type 1000 ISN record
c
      if (ifl(388) .eq. 1 .and. (ictyp .eq. 5000 .or. ictyp .eq. 5200))
     1        then
          call getxln_count (nlin)
          if (nlin .gt. 0) then
              iclw(3) = 1000
              iclw(4) = 0
              iclw(5) = 0
              inc    = 0
              ipx = 1
              do 100 i=1,nlin,1
                  call getxln (iline,rval,iltyp,inc)
                  if (inc .ne. -1) then
                      ipx = ipx + 1
                      jcldat(ipx) = iline
                      if (iltyp .eq. 2) then
                          jcldat(ipx) = -iline
                          ipx = ipx + 1
                          jcldat(ipx) = jval(1)
                          ipx = ipx + 1
                          jcldat(ipx) = jval(2)
                          jcldat(1) = jcldat(1) + 2
                      endif
                  endif
  100         continue
              jcldat(1) = ipx - 1
              iclw(5) = (ipx+1) / 2
              call clstor(iclf,imotp,iclw,rcldat)
          endif
          ifl(388) = 2
      endif
c
c......Cl record type & subtype
c
      iclw(3) = ictyp
      iclw(4) = isubcl
c
c......Number of reals
c......Motion records must convert number of points
c......to number of reals
c......Remove 1 real for the 4 I*2 header values
c
      if (ictyp .eq. 5000 .or. ictyp .eq. 5100 .or. ictyp .eq. 2500)
     1        then
          npt = 3
          if (ifl(82) .eq. 1) npt = 6
          iclw(5) = numitm * npt
      else if (ictyp .eq. 5200 .or. ictyp .eq. 5210 .or.
     1         ictyp .eq. 5300) then
          iclw(5) = numitm * 21
      else
          iclw(5) = numitm - 1
      endif
c
c...Put Motion and Circle records
c...through TRACUT Matrix
c...vp 12/19/96 translate all records if 'itr' is set 
c
c     if ((icl .eq. 3000 .or. icl .eq. 5000) .and.
c    1    ifl(73) .eq. 1) then
      if (itr .eq. 1 .and. ifl(73) .eq. 1) then
          if (icl .eq. 3000) then
              jinc   = 6
              if (ifl(144) .eq. 1) then
                  istrt  = 6
                  ien    = 11
              else
                  istrt  = 2
                  ien    = 7
              endif
          else 
              jinc   = 3
              if (ifl(82) .eq. 1) jinc = 6
              if (icl .ge. 5200) jinc  = 21
              istrt  = 1
              ien    = iclw(5)
          endif
c
c...vp 12/19/96 translate expanded records
c
          if (icl .ge. 5200) then
              do 120 i=istrt,ien,jinc
                  call conent (rdata(i),sc(41),3)
                  call conent (rdata(i+3),sc(41),4)
                  call unitvc (rdata(i+3),rdata(i+3))
                  call conent (rdata(i+6),sc(41),4)
                  call conent (rdata(i+9),sc(41),3)
                  call conent (rdata(i+12),sc(41),4)
                  call conent (rdata(i+15),sc(41),3)
                  call conent (rdata(i+18),sc(41),4)
  120         continue
c
c...translate regular motion and circul record data
c
          else
              do 150 i=istrt,ien,jinc
                  call conent (rdata(i),sc(41),3)
                  if (jinc .ge. 6) then
                      call conent (rdata(i+3),sc(41),4)
                      sec = dsqrt(rdata(i+3)**2 + rdata(i+4)**2 +
     1                            rdata(i+5)**2)
                      rdata(i+3) = rdata(i+3) / sec
                      rdata(i+4) = rdata(i+4) / sec
                      rdata(i+5) = rdata(i+5) / sec
                  endif
  150         continue
          end if
      endif
c
c...Post-processor variable
c
      if (iclw(3) .eq. 2000) then
          RPFRON = .false.
c
c......COOLNT
c
          if (iclw(4) .eq. 1030) then
              call clcool (rdata,iclw(5),CCOOL)
              call motcln (CCOOL)
c
c......CUTCOM
c
          else if (iclw(4) .eq. 1007) then
              call clcutc (rdata,iclw(5),it,im,id)
              if (it .eq. 1) then
                  ICCMOD = im
                  ICCDIR = id
                  call motccm (ICCMOD,ICCDIR)
              endif
c
c......CYCLE
c
          else if (iclw(4) .eq. 1054) then
              cnv    = 1.
              if (ifl(264) .eq. 1) cnv = 25.4
              call clcyc (rdata,iclw(5),cnv,idid,ICIPRM,RCIPRM)
c
c......FEDRAT
c
          else if (iclw(4) .eq. 1009) then
              fed    = CFEED(FITYP)
              call clfed (rdata,iclw(5),FITYP,fed)
              CFEED(FITYP) = fed
c
c......LOADTL
c
          else if (iclw(4) .eq. 1055) then
              call clldtl (rdata,iclw(5),CTLNO,CTLEN)
              call mottln (CTLNO,CTLEN)
c
c......RAPID
c
           else if (iclw(4) .eq. 5) then
               RPFRON = .true.
c
c......RETRCT
c
          else if (iclw(4) .eq. 7) then
              call clret (rdata,iclw(5),CYCRET)
c
c......SPINDL
c
          else if (iclw(4) .eq. 1031) then
              call clspn (rdata,iclw(5),CRPM,CSPDIR)
              call motspn (CSPDIR,CRPM)
          endif
      endif
c
c...Save clfile record
c
      call clstor(iclf,imotp,iclw,rdata)
99999 return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine putcl5(icl,isubcl,numitm,rdata)
C*       description
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
      subroutine putcl5(icl,isubcl,numitm,rdata)

      include 'com8a.com'
c
      integer*2 icl,isubcl,numitm
      real*8 rdata(640)

      integer*4 mupt,ncp,max
c
      if (sc(169) .lt. 8.229) then
         call putcl (icl,isubcl,numitm,rdata)
      else
         if (icl.eq.5200 .or. icl .eq. 5300) then
           mupt = 21
           max = 640
         else if (icl .eq. 5210) then
           mupt = 21
           max = mupt
         else
           mupt = 3 * (ifl(82) + 1)
           max = 120
         endif
         ncp  = numitm
         call ptput1 (nrcn,ncp,rdata,mupt,max)
      end if
c
      return
      end
