C***********************************************************************
C*    NAME         :  pfillet.f
C*       CONTAINS:
C*          pfillet  pfildo  cmplen  chkdir  gtfilrad  pfilpt
C*
C*    COPYRIGHT 2000 (c) Numerical Control Computer Sciences Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       pfillet.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:25
C***********************************************************************
c
c***********************************************************************
c
c  SUBROUTINE:  pfillet (kst,ken,kold,kerr)
c
c  FUNCTION:  This is the controlling routine for creating fillets
c             between motions within NCL.
c
c     INPUT:  kst     I*4  D2  -  First clfile motion record number to
c                                 process.
c
c             ken     I*4  D2  -  Final clfile motion record number to
c                                 process.
c
c             kold    I*4  D2  -  Previous clfile motion record number to
c                                 process.
c
c     OUTPUT: kerr    I*2  D1  -  Non-zero when a fillet was not generated.
c
c***********************************************************************
c
      subroutine pfillet (kst,ken,kold,kerr)
c
      include 'com8a.com'
      include 'fillet.com'
      include 'cutter.com'
      include 'wrksys.com'
c
      integer*2 kerr
      integer*4 kst(2),ken(2),kold(2)
c
      integer*2 iclf,ifil,npt,ierr,icnt,imiss,i487,iserr,iferr,
     1          ifl129,icfl3, iflg, jerr,isfl
      integer*4 irec(2),svrec(2),iold(2),jst(2),ien(2),istsv(2),
     1          iensv(2),ioldsv(2),jrec(2),lst(2),lold(2),irecst(2),
     2          svrec1(2),i,intnxt,iclw(6)
c
      logical idone,iq,ibeg,jdone,lsterr,imbeg,ismrec,lrap
      integer*4 ifnd
c
      real*8 dcltmp(640),pbuf(6),sptt(21),sc123,spttra(6)
c
      integer*4 DEBUGX
      parameter (DEBUGX=0)
      character*80 dbuf
      byte dout(80)
c
c...Initialize routine
c
      iclf  = 0
      ifil  = 0
      icnt  = 0
      imiss = 0
      lsterr = .false.
      kerr  = 0
      iserr = 0
      iferr = 0
      ierr   = 0
      i487  = 0
      intnxt = 0
      FILFLG(10) = 0
      sc123 = sc(123)
      MULTAX = ifl(82)
      ifnd = 1
      call ncl_zroptr(iensv)
c
c...Find record coming from
c
      call ncl_setptr(kold, irec)
      iclw(3) = 0
      do while (.not. ismrec(iclw))
          call ncl_setptr(irec, svrec)
          call clread (iclf,irec,iclw,dcltmp,jerr)
          call ncl_tstptr(irec, iflg)
          if (iflg .eq. 0) go to 8000
c
c......ARCSLP/FILLET interrupt record
c
          if (iclw(3) .eq. 13000) then
              if (iclw(4) .eq. 1) then
                  intnxt = 1
              else
                  intnxt = 0
              endif
              call cldel (iclf,svrec,irec,svrec1)
          endif
c
c.........Allow for FEDRAT/OUT
c
          if (iclw(3) .eq. 2000 .and. iclw(4) .eq. 1009 .and.
     1        iclw(5) .eq. 1) sc(123) = dcltmp(1)
      enddo
      call ncl_setptr(svrec, iold)
      call setnpt (iclw,npt)
      do 100 i=1,npt,1
          sptt(i) = dcltmp(iclw(5)-npt+i)
  100 continue
      if (ifl(82) .eq. 0) then
          sptt(4) = sc(4)
          sptt(5) = sc(5)
          sptt(6) = sc(6)
          if (ifl(73) .eq. 1) call conent (sptt(4),sc(41),4)
      endif
      if (DEBUGX .eq. 1) then
          write (dbuf,7001) iold,sptt(1),sptt(2),sptt(3)
 7001     format ('iold = ',i8,'   Sptt = ',3f12.6)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
c
c...Find record coming from
c
      iclw(3) = 0
      ibeg = .true.
      do while (.not. ismrec(iclw) .or. iclw(4) .eq. 6)
          call ncl_setptr(irec, svrec)
          call clread (iclf,irec,iclw,dcltmp,jerr)
          call ncl_tstptr(irec, iflg)
          if (iflg .eq. 0) go to 8000
          if (ismrec(iclw) .and. iclw(4) .eq. 6 .and. ibeg) then
              call setnpt (iclw,npt)
              do 200 i=1,npt,1
                  sptt(i) = dcltmp(iclw(5)-npt+i)
  200         continue
          endif
c
c......ARCSLP/FILLET interrupt record
c
          if (iclw(3) .eq. 13000) then
              if (iclw(4) .eq. 1) then
                  intnxt = 1
              else
                  intnxt = 0
              endif
              call cldel (iclf,svrec,irec,svrec1)
          endif
c
c.........Allow for FEDRAT/OUT
c
          if (iclw(3) .eq. 2000 .and. iclw(4) .eq. 1009 .and.
     1        iclw(5) .eq. 1) sc(123) = dcltmp(1)
          ibeg = .false.
      enddo
      call ncl_setptr(svrec, jst)
      call ncl_setptr(jst, irecst)
      if (DEBUGX .eq. 1) then
          write (dbuf,7002) jst,dcltmp(1),dcltmp(2),dcltmp(3)
 7002     format ('jst = ',i8,'   Ptt = ',3f12.6)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
c
c...Main loop to fillet all corners
c...in clfile range
c
      idone = .false.
      do while (.not. idone)
c
c......Find third and final point
c
          iclw(3) = 0
          FILFLG(10) = intnxt
          do while (.not. ismrec(iclw) .or. iclw(4) .eq. 6)
              call ncl_tstptr(irec, iflg)
              if (iflg .eq. 0) go to 8000
              call ncl_setptr(irec, svrec)
              call clread (iclf,irec,iclw,dcltmp,jerr)
c
c.........Allow for FEDRAT/OUT
c
              if (iclw(3) .eq. 2000 .and. iclw(4) .eq. 1009 .and.
     1            iclw(5) .eq. 1) sc(123) = dcltmp(1)
c
c......ARCSLP/FILLET interrupt record
c
              if (iclw(3) .eq. 13000) then
                  if (iclw(4) .eq. 1) then
                      intnxt = 1
                      FILFLG(10) = 1
                  else
                      intnxt = 0
                  endif
                  call cldel (iclf,svrec,irec,svrec1)
              endif
          enddo
          call ncl_setptr(svrec, ien)
c
c......Fillet corner
c
          if (DEBUGX .eq. 1) then
              dbuf = ' '
              call ctob (dbuf,dout)
              call nclxostr(dout)
              write (dbuf,7003) iold,jst
 7003         format ('iold = ',i8,'   jst = ',i8)
              call ctob (dbuf,dout)
              call nclxostr(dout)
              write (dbuf,7004) ien,dcltmp(1),dcltmp(2),dcltmp(3)
 7004         format ('ien = ',i8,'   Eptt = ',3f12.6)
              call ctob (dbuf,dout)
              call nclxostr(dout)
          endif
          call ncl_setptr(jst, istsv)
          call ncl_setptr(ien, iensv)
          call ncl_setptr(iold, ioldsv)
c
c......Previous fillet failed in combine mode
c......Try with 1st and 3rd motions, ignoring middle motion
c......prior to two motion attempt
c
          filout = 0
          if (FILFLG(1) .eq. 2 .and. lflcom) then
              call ncl_setptr(FILPTR(1), iold)
              call ncl_setptr(FILPTR(3), jst)
              call ncl_setptr(FILPTR(5), ien)
              call ncl_setptr(iold, lold)
              call ncl_setptr(jst, lst)
              FILFLG(4) = 1
              if (DEBUGX .eq. 1) then
                  write (dbuf,7011) iold,jst,ien
 7011             format ('Pre iold = ',i8,'   jst = ',i8,
     1                    '   ien = ',i8)
                  call ctob (dbuf,dout)
                  call nclxostr(dout)
              endif
              call pfildo (jst,ien,iold,isfl,ierr)
              FILFLG(4) = 0
              if (filout .eq. 1) then
                  ifil = 1
                  if (lsterr) imiss = imiss - 1
                  call ncl_setptr(lold, ioldsv)
                  call ncl_eqlptr(irecst, istsv, iflg)
                  if (iflg .eq. 1) call ncl_setptr(lst, irecst)
                  call ncl_setptr(lst, istsv)
                  iferr = 0
                  do 250 i=1,npt,1
                      sptt(i) = sptsav(i)
  250             continue
                  if (ifnd .eq. 1) ifnd = 0
                  call filmrk
              else
                  call ncl_setptr(istsv, jst)
                  call ncl_setptr(iensv, ien)
                  call ncl_setptr(ioldsv, iold)
              endif
          endif
c
c......Attempt normal fillet
c
          if (filout .eq. 0) then
              call pfildo (jst,ien,iold,isfl,iferr)
              if (DEBUGX .eq. 1) then
                  write (dbuf,7010) iferr
 7010             format ('iferr = ',i4)
                  call ctob (dbuf,dout)
                  call nclxostr(dout)
              endif
              if (filout .eq. 1) ifil = 1
c
c........Fillet created
c........If continuation record used for second leg
c........then set pointers.
c........'iensv' must point to beginning of second leg
c........not to last continuation record read
c
              if (filout .eq. 1) then
                  ifil = 1
                  if (isfl .eq. 1) then
                      call ncl_setptr(ien,irec)
                      call ncl_setptr(ien,iensv)
                      iclw(4) = 6
                      do while (iclw(4) .eq. 6)
                        call clprev (iclf,iensv,iclw,dcltmp,jerr)
                      enddo
                  endif
              endif
c
c......Fillet failed
c......If COMBIN in effect, then attempt
c......fillet with previous location
c
              if (filout .eq. 0 .and. FILFLG(1) .ne. 0 .and. lflcom)
     1               then
                  ifl(348) = 0
                  call ncl_setptr(FILPTR(1), iold)
                  call ncl_setptr(FILPTR(3), jst)
                  call ncl_setptr(FILPTR(5), ien)
                  call ncl_setptr(iold, lold)
                  call ncl_setptr(jst, lst)
                  FILFLG(4) = 1
                  if (DEBUGX .eq. 1) then
                      write (dbuf,7005) iold,jst,ien
 7005                 format ('2nd iold = ',i8,'   jst = ',i8,
     1                        '   ien = ',i8)
                      call ctob (dbuf,dout)
                      call nclxostr(dout)
                  endif
                  call pfildo (jst,ien,iold,isfl,ierr)
                  FILFLG(4) = 0
                  if (filout .eq. 1) then
                      ifil = 1
                      if (lsterr) imiss = imiss - 1
                      call ncl_setptr(lold, ioldsv)
                      call ncl_eqlptr(irecst, istsv, iflg)
                      if (iflg .eq. 1) call ncl_setptr(lst, irecst)
                      call ncl_setptr(lst, istsv)
                      iferr = 0
                      do 400 i=1,npt,1
                          sptt(i) = sptsav(i)
  400                 continue
                      if (ifnd .eq. 1) ifnd = 0
                      call filmrk
c
c......Second fillet failed
c......Try for another time if previous fillet was generated
c......Try with 1st and 3rd motions, ignoring previous fillet
c
                  else if (FILFLG(1) .eq. 1) then
                      call ncl_setptr(FILPTR(7), iold)
                      call ncl_setptr(FILPTR(9), jst)
                      call ncl_setptr(FILPTR(5), ien)
                      call ncl_setptr(iold, lold)
                      call ncl_setptr(jst, lst)
                      FILFLG(4) = 1
                      if (DEBUGX .eq. 1) then
                          write (dbuf,7006) iold,jst,ien
 7006                     format ('3rd iold = ',i8,'   jst = ',i8,
     1                            '   ien = ',i8)
                          call ctob (dbuf,dout)
                          call nclxostr(dout)
                      endif
                      call pfildo (jst,ien,iold,isfl,ierr)
                      FILFLG(4) = 0
                      if (filout .eq. 1) then
                          ifil = 1
                          if (lsterr) imiss = imiss - 1
                          call ncl_setptr(lold, ioldsv)
                          call ncl_eqlptr(irecst, istsv, iflg)
                          if (iflg .eq. 1) call ncl_setptr(lst, irecst)
                          call ncl_setptr(lst, istsv)
                          iferr = 0
                          do 450 i=1,npt,1
cc                              sptt(i) = sptsav(i+21)
                              sptt(i) = sptsav(i)
  450                     continue
                          if (ifnd .eq. 1) ifnd = 0
                          call filmrk
                      endif
                  endif
              endif
          endif
c
c......Save clfile pointers
c......for when ARCSLP/COMBIN is in effect
c
          if (lflcom) then
              call ncl_setptr(ioldsv, FILPTR(1))
              call ncl_setptr(istsv, FILPTR(3))
              call ncl_setptr(iensv, FILPTR(5))
              n = npt
              if (n .lt. 6) n = 6
              do 500 i=1,n,1
                  sptsav(i+21) = sptsav(i)
                  sptsav(i) = sptt(i)
  500         continue
          endif
c
c.........Count fillets generated
c
          lsterr = .false.
          if (filout .eq. 1) then
              icnt = icnt + 1
          else if (iferr .ne. 0) then
              lsterr = .true.
              imiss = imiss + 1
          endif
          if (ierr .eq. -487 .or. iferr .eq. -487) i487 = ierr
          if (iserr .eq. 0) iserr = iferr
          if (filout .eq. 0) ifnd = 2
c
c.........Fillet was output
c.........Save fillet as previous
c.........record for ARCSLP/COMBIN
c
          if (filout .eq. 1 .and. lflcom) then
              FILFLG(1) = 1
              jdone = .false.
              call ncl_setptr(FILPTR(5), jrec)
              do while (.not. jdone)
                  call clprev (iclf,jrec,iclw,dcltmp,kerr)
                  call ncl_eqlptr(jrec,FILPTR(1),iflg)
                  if (iflg .eq. 1 .or. (ismrec(iclw) .and.
     1                iclw(4) .ne. 6)) jdone = .true.
              enddo
              call ncl_setptr(FILPTR(1), FILPTR(7))
              call ncl_setptr(FILPTR(3), FILPTR(9))
              call ncl_setptr(FILPTR(5), FILPTR(11))
              call ncl_setptr(FILPTR(3), FILPTR(1))
              call ncl_setptr(jrec, FILPTR(3))
CC TEST1
              call ncl_setptr(jrec, istsv)
              if (DEBUGX .eq. 1) then
                  write (dbuf,7007) FILFLG(2),FILFLG(3),FILFLG(5)
 7007             format ('Fillet FLG(1) = ',i8,'   FLG(2) = ',i8,
     1                    '   FLG(3) = ',i8)
                  call ctob (dbuf,dout)
                  call nclxostr(dout)
              endif
c
c.........No fillet generated
c
          else
              FILFLG(1) = 2
          endif
c
c......Point to next corner
c
          call ncl_setptr(istsv, iold)
          call ncl_setptr(iensv, jst)
          call ncl_eqlptr(ien, ken, iflg)
          if (iflg .eq. 1) idone = .true.
          call ncl_eqlptr(iensv, ken, iflg)
          if (iflg .eq. 1) idone = .true.
      enddo
c
c...End of routine
c...Redisplay motion if necessary
c
 8000 sc(123) = sc123
      if (icnt .eq. 0 .and. iserr .ne. 0) then
          kerr = 474
      else if (imiss .gt. 0 .and. iserr .ne. 0) then
          kerr = -474
      endif
      if (kerr .eq. 0) kerr = i487
      if (ifil .eq. 1 .and. ifl(35) .ne. 1 .and. ifl(330) .ne. 1 .and.
     1    ifl(42) .eq. 0) then
          ifl129 = ifl(129)
          icfl3  = icutfl(3)
          lrap   = RPFRON
          if (icutfl(3) .eq. 1) then
              ifl(129) = 0
              icutfl(3) = -1
          endif
c
c......Erase the previous motion
c
          if (ifnd .ne. 0) then
              do 8020 i=1,6,1
                  spttra(i) = sptt(i)
 8020         continue
              if (ifl(73) .eq. 1 .and. itrafl .eq. 0) then
                  call conent (spttra,sc(93),3)
                  call conent (spttra(4),sc(93),4)
              endif
              if (ifl(264).eq.1) then
                  call vctmsc(spttra,spttra,(1.d0/25.4d0))
              endif
              if (lwrk) then
                  call conent (spttra,wrkmx,3)
                  call conent (spttra(4),wrkmx,4)
                  if (wrkscl .ne. 1.0d0) call unitizevc(spttra(4))
              endif

          endif
          call filers (spttra,ifnd)
c
c......Point to the first clfile record
c
          call ncl_setptr(irecst, irec)
c
c......Position to the start of the motion
c
          call plotm (sptt,.true.)
c
c......Loop through all motions
c
          idone = .false.
          iq = .false.
          imbeg = .false.
c
          call ncl_clfind_isn (iclf,irec)
c
c......See if previous record is RAPID
c
          call ncl_setptr(irec, irecst)
          call clprev (iclf,irec,iclw,dcltmp,jerr)
          if (iclw(3) .eq. 2000 .and. iclw(4) .eq. 5) RPFRON = .true.
          call ncl_setptr(irecst, irec)
c
          do while (.not. idone)
              call ncl_setptr(svrec, svrec1)
              call ncl_setptr(irec, svrec)
              call clread (iclf,irec,iclw,dcltmp,jerr)
              call ncl_eqlptr(svrec, iensv, iflg)
              if (iclw(3) .eq. 1000) then
                  call motisn (dcltmp)
              else if (ismrec(iclw)) then
                  if ((iq .and. (iclw(4) .eq. 6 .or. iflg .eq. 1))
     1                .or. .not. iq) then
                      if (iclw(4) .ne. 6) then
                          if (imbeg) call motend (svrec)
                          do 8050 i=1,6,1
                              spttra(i) = sptt(i)
 8050                     continue
                          if (ifl(73) .eq. 1) then
                              call conent (spttra,sc(93),3)
                              call conent (spttra(4),sc(93),4)
                          endif
                          call motbgn (svrec,spttra)
                          imbeg = .true.
                      endif
                      call setnpt (iclw,npt)
                      do 8100 i=1,iclw(5),npt
                          pbuf(1) = dcltmp(i)
                          pbuf(2) = dcltmp(i+1)
                          pbuf(3) = dcltmp(i+2)
                          if (multax .ne. 0) then
                              pbuf(4) = dcltmp(i+3)
                              pbuf(5) = dcltmp(i+4)
                              pbuf(6) = dcltmp(i+5)
                          else
                              pbuf(4) = sptt(4)
                              pbuf(5) = sptt(5)
                              pbuf(6) = sptt(6)
                          endif
                          isv    = icutfl(3)
                          if (i+npt .gt. iclw(5)) then
                              ifl(130) = 0
                              if (icutfl(3) .eq. -1) icutfl(3) = 1
                          endif
                          call plotm (pbuf,.false.)
                          icutfl(3) = isv
 8100                 continue
                      do 8200 i=1,6,1
                          sptt(i) = pbuf(i)
 8200                 continue
                      RPFRON = .false.
                  else if (iq) then
cc                      idone = .true.
                  endif
              else if (iq) then
cc                idone = .true.
              else if (iclw(3) .eq. 2000 .and. iclw(4) .eq. 5) then
                  RPFRON = .true.
              else if (iclw(3) .eq. 2000) then
                  RPFRON = .false.
              endif
              call ncl_tstptr(irec, iflg)
              if (iflg .eq. 0) idone = .true.
          enddo
          if (imbeg) call motend (svrec)
          ifl(129) = ifl129
          icutfl(3) = icfl3
          RPFRON = lrap
      endif
c
c...Set new clfile positions
c...Make sure 'kst' is just prior motion record
c...to 'kend'.  Otherwise next time through will
c...display incorrect motion
c
      call ncl_tstptr(iensv,iflg)
      if (iflg .eq. 0) then
          kerr = 474
      else
          idone = .false.
          call ncl_setptr(iensv, irec)
          do while (.not. idone)
              call ncl_setptr(irec, svrec)
              call clprev (iclf,irec,iclw,dcltmp,jerr)
              call ncl_eqlptr(irec, istsv, iflg)
              if (iflg .eq. 1 .or. ismrec(iclw)) idone = .true.
          enddo
          call ncl_setptr(irec, kst)
          call ncl_setptr(iensv, ken)
      endif
      return
      end
c
c***********************************************************************
c
c  SUBROUTINE:  pfildo (kst,ken,kold,ksfl,kerr)
c
c  FUNCTION:  Generates a single fillet between clfile motions.  Called from
c             the controlling routines 'pfillet' (NCL) and 'NclxMotFillet'
c             (OpenNCL).
c
c     INPUT:  kst     I*4  D1  -  First clfile motion record number to
c                                 process.
c
c             ken     I*4  D1  -  Final clfile motion record number to
c                                 process.
c
c             ksfl    I*2  D1  -  1 = A continuation record was processed
c                                 for the second motion (ken), so the main
c                                 routine will have to adjust the current
c                                 clfile record to take this into account,
c                                 since the clfile records in the second
c                                 leg of motion prior to the continuation
c                                 record used will be deleted.
c
c             kold    I*4  D1  -  Previous clfile motion record number to
c                                 process.
c
c     OUTPUT: kerr    I*2  D1  -  Non-zero when a fillet was not generated.
c
c***********************************************************************
c
      subroutine pfildo (kst,ken,kold,ksfl,kerr)
 
      include 'com8a.com'
      include 'fillet.com'
      include 'cutter.com'
      include 'wrksys.com'
c
      integer*2 kerr,ksfl
      integer*4 kst(2),ken(2),kold(2)
 
      real*8 wrkpts(6,4),vec(3,3),tl,RADS /57.29577 95d0/,angl(2),
     -       dist2,dist3,dist4,spoint(6),fpoint(6),r,f_dot,mintol,
     -       tangs,tangf,tflr,filsav(640),wk1(6),wk2(6),wkt(6),
     -       svec(2),fvec(2),cirsto(7),ratio(2),alph1,
     -       alph3,bet1,ddl,fi,thet,tvmp(4),iclpt(12),
     x       dclall(441), pbuf(6),
     x       dcll2(441),dcll3(441),dcll4(480),dcll5(441),dcll6(441),
     x       dcltmp(441),sc171,sc172,sc173,tcinv(12),fctmm,apex(6)

      integer*2 imod(2),icrnbf(5),iadj,
     -          mxtmp,mxcl1,mxcl2,iret,ifless,npts,
     x          iokay, npts1,npts2,npts3,ider,iptcnt,inopts,iacy,is,
     x          iclf,i,j,ix,mxclsv,inc,ierr,icnt,j1,k1,svmxcl,
     x          iisav,idcnt,iflg,iad2nd,itst,jerr
 
      integer*4 irec(2),nxrc(2),savkst(2),savken(2),svkold(2),iclwsv(6),
     1          iclw(6),iclw2(6),iclw3(6),iclw4(6),iclw5(6),iclw6(6),
     2          iclwtp(6),irectp(2),ihld
      logical found,swit,lnopts,lv93,ismrec
      integer*2 pointv /3/, vectrv /4/
c
      integer*4 DEBUGX
      parameter (DEBUGX=0)
      character*80 dbuf
      byte dout(80)
C
C...Assign kst and ken, clread returns the next record number
C...thus changing the value of the variable holding the record number.
C...kst will be returned as the same record number, but ken will be returned
C...with a newly created record number
C
      iflg   = 0
      kerr   = 0
      ifl(2) = 0
      ierr   = 0
      lnopts = .false.
      call ncl_setptr(kst, irec)
      call ncl_setptr(ken, nxrc)
      call ncl_setptr(ken, savken)
      call ncl_setptr(kold, svkold)
      filout = 0
      ider   = 1
      iokay  = 0
      iclf     = 0
      imltsv   = 1
      ifless   = 0
      iptcnt   = 0 
      idcnt    = 0
      inopts   = 0
      icrout   = 0
      iacy     = 5
      mxcl2    = 1
      svec(1)  = 0.
      svec(2)  = 0.
      fvec(1)  = 0.
      fvec(2)  = 0.
      npts     = 3
      FEDOUT   = 0.0d0
      mintol = .001
cc      if (ifl(264) .eq. 1 .or. ifl(362) .eq. 1) mintol = mintol * 25.4
      if (lflcom .and. filtol .lt. mintol) filtol = mintol
      tflr     = DABS(filrad) - filtol
      istrpt(1) = 1
      istrpt(2) = 1
      ioldfl = 0
      ksfl   = 0
      multax=ifl(82)
      if (multax.eq.1)  npts = 6 
      sc171 = sc(171)
      sc172 = sc(172)
      sc173 = sc(173)
      if (ifl(73).eq.1) call invmx (sc(41),tcinv)
      if (ifl(264).eq.1) fctmm = 1.0d0 / 25.4d0
      swit = .false.
      iadj = 0
      lv93 = sc(169).lt.9.349d0
c
c...ARCSLP/FILLET interrupt is in effect
c...Don't process this record
c
      if (FILFLG(10) .eq. 1) then
          ider = 0
          goto 9000
      endif
C
C...Get the record needed for the oldpts array
C...We want the last point in the motion so get the
C...last npts valid elements of dclbuf.
C
      if (ifl(348).eq.0) then
5        call clread (iclf,kold,iclw,dclbuf,jerr)
         if (jerr.eq.1) goto 9000
         if (.not. ismrec(iclw)) goto 5
C
C...Check to see if this is the just the first set of points.
C...In other words, is the next record a continuation record.
C...If there is a continuation record, we want to use the 
C...last one.  JLS 7/1/99
C
6        call clread (iclf,kold,iclwtp,dcltmp,jerr)
         if (jerr.eq.1) goto 9000
         if (ismrec(iclwtp) .and. iclwtp(4) .eq. 6) then
            do 7, i=1,640
7              dclbuf(i) = dcltmp(i)
            do 8, i=1,5
8              iclw(i) = iclwtp(i)
            goto 6
         endif
         
C
C...Determine the number of elements for each cl point, possiblities
C...3,6,or 21
C
         call setnpt(iclw,npts1)
         ix=0
         is=iclw(5)-(npts1-1)
         do 10 i=is,(is +(npts-1))
            ix=ix + 1
10          oldpts(ix)=dclbuf(i)
      endif
c
c...Read next clfile record
c
100   call ncl_setptr(irec,savkst)
      call clread (iclf,irec,iclw,dclbuf,jerr)
      if (jerr.eq.1) goto 9000
      call ncl_tstptr(irec,itst)
      if (.not. ismrec(iclw) .and. itst.ne.0)goto 100
C
C...Again we need to check to see if the next record is a 
C...continuation of this record.  If so we want to move the
C...last point of the current record into oldpts and then
C...use the the next record. (Do this until it is the last
C...record for this motion.) JLS 7/1/99
C
11    call ncl_setptr(irec,irectp)
      call clread (iclf,irec,iclwtp,dcltmp,jerr)
      if (jerr.eq.1) goto 9000
      if (ismrec(iclwtp) .and.  iclwtp(4) .eq. 6) then
         call setnpt(iclw,npts1)
         is=iclw(5)-(npts1-1)
         call conv8_8(dclbuf(is),oldpts,npts)
         i = iclwtp(5)
         call conv8_8(dcltmp,dclbuf,i)
         do 13, i=1,5
13          iclw(i) = iclwtp(i)
c         savkst = irectp
         call ncl_setptr(irectp,savkst)
         goto 11
      endif
C
C...Determine the number of elements for each cl point, possiblities are
C...3,6,or 21
C
      call setnpt (iclw, npts2)
      do 14 i=1,iclw(5)
14       dclall(i) = dclbuf(i)
C
C...Save the current iclw for future use.
C
      do 15 i=1,5
15       iclwsv(i) = iclw(i)
C
C...In most cases records of subtype 2 are not going to be used
C...and end up being deleted.  If this record is subtype 2, we
C...don't want to delete it so set the subtype to 5
C
      if (iclwsv(4).eq.2) iclwsv(4) = 5
      i = iclw(5)
C
C...sc(171) thru sc(173) holds the last motion point and is
C...used in the plotting routines.
C
      sc(171) = dclbuf(i-(npts2-1))
      sc(172) = dclbuf(i-(npts2-2))
      sc(173) = dclbuf(i-(npts2-3))
      apex(1) = sc(171)
      apex(2) = sc(172)
      apex(3) = sc(173)
c
c...Save the tool axis of the corner point
c...Used when fixed tool axis is request along fillet
c
      if (multax .eq. 1) then
          FILAXS(1) = dclbuf(i-(npts2-4))
          FILAXS(2) = dclbuf(i-(npts2-5))
          FILAXS(3) = dclbuf(i-(npts2-6))
      else
          FILAXS(1) = sc(4)
          FILAXS(2) = sc(5)
          FILAXS(3) = sc(6)
      endif
c
c...Apply working coordinate system matrix, units and tracut
c
      if (ifl(73) .eq. 1 .and. itrafl .eq. 0) then
        call conent(sc(171),tcinv,pointv)
      endif
      if (ifl(264).eq.1) then
        call vctmsc(sc(171),sc(171),fctmm)
      endif
      if (lwrk) then
        call conent(sc(171), wrkmx, pointv)
      endif
c
c...Calculate number of cl points in record
c
      call calmxc (iclw,mxcl)
      mxcl1    = mxcl
c
c...Reset circular buffer
c
      icrsto = 0
      lcirvc = 0
      do 150 i=1,7,1
          cirbuf(i) = 0.0
          cirsto(i) = 0.0
  150 continue
C
C...Save current array
C
      if (mxcl .eq. 1) istrpt(1) = 1
      call ftsave (1,iclw,dclbuf,filsav,mxcl1,npts)
      mxtmp  = mxcl1
C
C...Load wrkpts with the correct values - a combination of
C...oldpts,filsav and the data that corresponds with nxrc
C
2900  continue
      call filchk (nxrc,wrkpts,iret,ifless,mxtmp,
     -             mxcl1,mxcl2,filsav,npts,cirsto,
     -             icrnbf,npts3)
c
c...Save ending position of 1st line and
c...starting position of 2nd line
c...as these are needed to verify the circle tangency
c...lies within the motion
c...(We do not extend motion during filleting)
C
      do 2910 i=1,6,1
          wk1(i) = wrkpts(i,2)
          wk2(i) = wrkpts(i,3)
 2910 continue
      iisav = irotpt
      irotpt = 0
      call rotpts (wk1,ierr)
      call rotpts (wk2,ierr)
      irotpt = iisav
c      savken = nxrc
      call ncl_setptr(nxrc,savken)
      mxclsv = mxcl1
      if (iret .eq. 1 .or. iret .eq. 2) then
          ider = 0
          goto 9000
      endif
C
C...Main loop
C
 3000 continue
          iad2nd = 0
C
C...Calculate vectors and unitize then calculate the angles.
C
  250     inc = 0
          do 300 i=1,3,2
              inc = inc +1
              vec(inc,1)  = wrkpts(1,i+1)-wrkpts(1,i)
              vec(inc,2)  = wrkpts(2,i+1)-wrkpts(2,i)
              vec(inc,3)  = wrkpts(3,i+1)-wrkpts(3,I)
              tl     = DSQRT (vec(inc,1)**2 +
     -                        vec(inc,2)**2 +
     -                        vec(inc,3)**2)
C
C...Does the vector have real length
C
              IF (TL .LE. 0) GO TO 980
              VEC(INC,1)  =  VEC(INC,1) / TL
              VEC(INC,2)  =  VEC(INC,2) / TL
              VEC(INC,3)  =  VEC(INC,3) / TL
              CALL dpont2 (VEC(INC,1),6)
              CALL dpont2 (VEC(INC,2),6)
              CALL dpont2 (VEC(INC,3),6)
              ANGL(INC)   =  DACOS (VEC(INC,1))*RADS
  300     CONTINUE
C
          IF (ANGL(1) .GT. -.00005 .AND. ANGL(1) .LT. .00005)
     -        ANGL(1) = 0.0
          IF (ANGL(2) .GT. -.00005 .AND. ANGL(2) .LT. .00005)
     -        ANGL(2) = 0.0
          IF (VEC(1,2) .LT. 0) ANGL(1) = 360. -  ANGL(1)
          IF (VEC(2,2) .LT. 0) ANGL(2) = 360. -  ANGL(2)
C
C...Calculate cirular center point
C...Determine XL,XS for circle calcs
C...Find quadrants
C
          call filmod (VEC,ANGL,SVEC,FVEC,IMOD)
          tl = DSQRT (svec(1)**2 + svec(2)**2)
          IF (TL .LE. 0) GO TO 980
          SVEC(1)  =  SVEC(1) / TL
          SVEC(2)  =  SVEC(2) / TL
          tl = DSQRT (fvec(1)**2 + fvec(2)**2)
          IF (TL .LE. 0) GO TO 980
          FVEC(1)  =  FVEC(1) / TL
          FVEC(2)  =  FVEC(2) / TL
C
C... X,Y,Z = ARE THE LINE ENDPOINTS.
C... VEC   = THE UNITIZED VECTORS OF THE LINES CREATED
C...         FROM THE MOTION CLPTS.
C... IERR  = RETURNS 1 IF AN ERROR IN BUILDING THE CIRCLE
C... IMOD  = IS THE MODIFIER 1=XL,2=YL,3=XS,4=YS
C
  900     call tancir (wrkpts,vec,ierr,imod)
          iout2 = 0
          if (ierr .eq. 1) then
              ider = 0
              goto 9000
          endif
C
C... ADJUST FEDRAT
C
          IF (IFEDCO .NE. 0) CALL FEDADJ (VEC,IERR)
          IF (TFLR .LE. 0.) FEDOUT = 0.
C
C... CALCULATE START AND STOP POINTS
C
          TANGS     = DACOS (SVEC(1)) * RADS
          IF (SVEC(2) .LT. 0.) TANGS = 360. - TANGS
C
          SPOINT(1) = CIRBUF(1) + FILRAD*SVEC(1)
          SPOINT(2) = CIRBUF(2) + FILRAD*SVEC(2)
          SPOINT(3) = WRKPTS(3,1)
C
C...CHECK IF LAST POINT IN ARRAY SHOULD BE OUTPUT
C...First CALCULATE THE NEW TOOL AXIS VECTOR
C
          call rotpt (wk1,wkt)
          DIST3 = DSQRT ((WRKPTS(1,2)-wkt(1))**2 +
     -                   (WRKPTS(2,2)-wkt(2))**2 +
     -                   (WRKPTS(3,2)-wkt(3))**2)
          DIST4 = DSQRT ((WRKPTS(1,2)-WRKPTS(1,3))**2 +
     -                   (WRKPTS(2,2)-WRKPTS(2,3))**2 +
     -                   (WRKPTS(3,2)-WRKPTS(3,3))**2)

c
c.....Dassault's NCL343.cpp
c.....See note attached to NCL343.cpp below
c
          if (dist3 .gt. cirbuf(7)*2.5d0) then
            call filseg (wrkpts(1,2),wrkpts(1,1),spoint,iout1)
            if (iout1 .eq. 1) then
              if (dist4.gt.cirbuf(7)*15.0d0 .or. lcirvc.ne.1) iout2 = 1
              dist3 = f_dot (cirvc0,cirbuf(4))
              if (.not.lv93 .and. dist3.lt.0.9848) iout2 = 1
             endif
          endif

          IF (MULTAX .EQ. 1) THEN
              DIST2  = DSQRT ((SPOINT(1)-WRKPTS(1,2))**2 +
     -                        (SPOINT(2)-WRKPTS(2,2))**2 +
     -                        (SPOINT(3)-WRKPTS(3,2))**2)
              DIST3  = DSQRT ((WRKPTS(1,2)-WRKPTS(1,1))**2 +
     -                        (WRKPTS(2,2)-WRKPTS(2,1))**2 +
     -                        (WRKPTS(3,2)-WRKPTS(3,1))**2)
              RATIO(1) = 1.0 - (DIST2/DIST3)
              if (dabs(ratio(1)).gt.2.0d0) then
                DIST2 = DSQRT ((SPOINT(1)-WRKPTS(1,1))**2 +
     -                         (SPOINT(2)-WRKPTS(2,1))**2 +
     -                         (SPOINT(3)-WRKPTS(3,1))**2)
                DIST3 = DSQRT ((WRKPTS(1,1)-wkt(1))**2 +
     -                         (WRKPTS(2,1)-wkt(2))**2 +
     -                         (WRKPTS(3,1)-wkt(3))**2)
                RATIO(1) = 0.0
                if (dist3.gt.1.d-6) RATIO(1) = 1.0 - (DIST2/DIST3)
                call pgtlan (wkt(4),WRKPTS(4,2),ALPH1,BET1,ddl,fi,
     -                     thet,tvmp)

              else
                call pgtlan (WRKPTS(4,1),WRKPTS(4,2),ALPH1,BET1,ddl,fi,
     -                     thet,tvmp)
              endif
              ALPH3  = ALPH1 + RATIO(1) * ddl
              call setijk (ALPH3,SPOINT(4),fi,thet)
C
C... CALC TOOL AXIS VECTORS
C
          ELSE
              SPOINT(4) = 0.
              SPOINT(5) = 0.
              SPOINT(6) = 1.
          ENDIF
c
c...Do the actual comparison
c
          if (DEBUGX .eq. 1) then
              write (dbuf,7001) spoint(1),spoint(2),spoint(3)
 7001         format ('Spoint = ',f12.4,f12.4,f12.4)
              call ctob (dbuf,dout)
              call nclxostr(dout)
          endif
          call filseg (wrkpts(1,1),wkt,spoint,iout)
          IF (iout .eq. 0) GO TO 985
C
C... CHECK FOR distances on the other leg; if both legs need adjustment
c... do them in turn (each time both legs are bad the other one is shifted)
C
          FPOINT(1) = CIRBUF(1) + FILRAD*FVEC(1)
          FPOINT(2) = CIRBUF(2) + FILRAD*FVEC(2)
          FPOINT(3)  = WRKPTS(3,4)
          if (DEBUGX .eq. 1) then
              write (dbuf,7004) wrkpts(1,3),wrkpts(2,3),wrkpts(3,3)
 7004         format ('Wrkpts3 = ',f12.4,f12.4,f12.4)
              call ctob (dbuf,dout)
              call nclxostr(dout)
              write (dbuf,7005) wrkpts(1,4),wrkpts(2,4),wrkpts(3,4)
 7005         format ('Wrkpts4 = ',f12.4,f12.4,f12.4)
              call ctob (dbuf,dout)
              call nclxostr(dout)
          endif
          call rotpt (wk2,wkt)
          call filseg (wrkpts(1,4),wkt,fpoint,iout)
          call filsp2 (ISTRPT(2),mxcl2,nxrc,1,isfl)
          IF (iout .eq. 1 .and. isfl .ne. 0) then
             swit = .not.swit
             if (swit) goto 1101
          endif

 
  980     IF (MXCL1 .GT. 2) GOTO 982
          if (iclw(4) .ne. 6) go to 982
          ihld   = iclw(1)
          call ncl_setptr(savkst, irectp)
          call ncl_tstptr(irectp, itst)
          found  = itst.gt.0
          call clprev (iclf,irectp,iclwtp,dcltmp,jerr)       
          call ncl_tstptr(irectp, itst)
          if (itst.eq.0 .or. jerr.eq.1 .or.
     1        iclwtp(1).ne.ihld .or. .not. ismrec(iclwtp)) goto 2800
          do 981 i=1,4
            iclw(i)   = iclwtp(i)
            iclwsv(i) = iclw(i)
  981     continue
          j1 = iclwtp(5)
          call conv8_8 (dcltmp, dclbuf, j1)
          call conv8_8 (dclall, dclbuf(j1+1), npts)
          iclw(5)   = j1+npts
          iclwsv(5) = iclw(5)
          iadj = npts
          i = iclw(5)
          call conv8_8 (dclbuf, dclall, i)
          call ncl_setptr(irectp, savkst)
          istrpt(1) = 1
          call ftsave (1,iclw,dclbuf,filsav,mxcl1,npts)
          mxtmp  = mxcl1
          mxcl1  = mxcl1+1
          iptcnt = -1
          if (IMLTSV .eq. 1) IMLTSV = 4
          if (IMLTSV .eq. 3) IMLTSV = 2
c
c...Move back one point in first cl point array
c
  982     MXCL1  = MXCL1 -1
          if (mxcl1.le.0 .or. (mxcl1 .eq. 1 .and. iflg .eq. 1))
     1            goto 2800
          IFLESS  = 1
          IF(IROTPT .EQ. 1) IROTPT  = 3
c
c...If cl points are exhausted, insert previous pt into start of array.
c
          if (mxcl1.eq.1) then
            iflg = 1
            call conv8_8 (OLDPTS, dcltmp, npts)
            j1 = mxtmp*npts
            call conv8_8 (filsav, dcltmp(npts+1), j1)
            mxtmp = mxtmp+1
            j1 = 2
            CALL FILCHK (NXRC,WRKPTS,IRET,IFLESS,MXTMP,
     -                   j1,MXCL2,dcltmp,NPTS,CIRSTO,
     -                   ICRNBF,npts3)
          else
            CALL FILCHK (NXRC,WRKPTS,IRET,IFLESS,MXTMP,
     -                   MXCL1,MXCL2,FILSAV,NPTS,CIRSTO,
     -                   ICRNBF,npts3)
          endif
C
          call ncl_setptr(nxrc, savken)
          iptcnt = iptcnt +1
          idcnt  = idcnt + 1
          MXCLSV = MXCL1
          if (iret .eq. 1 .or. iret .eq. 2) then
              ider = 0
              GO TO 9000
          endif
          IF (IRET .EQ. 3) then
            iret = 0
            GOTO 980
          endif
          GO TO 250
C
C...Check for End Point
c...outside of line segment
C
  985     TANGF     = DACOS (FVEC(1)) * RADS
          IF (FVEC(2) .LT. 0.) TANGF = 360. - TANGF
          FPOINT(1) = CIRBUF(1) + FILRAD*FVEC(1)
          FPOINT(2) = CIRBUF(2) + FILRAD*FVEC(2)
          FPOINT(3)  = WRKPTS(3,4)
C
C... GET ANGLES
C
          call rotpt (wk2,wkt)
          DIST3 = DSQRT ((WRKPTS(1,3)-wkt(1))**2 +
     -                   (WRKPTS(2,3)-wkt(2))**2 +
     -                   (WRKPTS(3,3)-wkt(3))**2)
c
c.....Dassault's NCL343.cpp - if the leg is over 2.5*rad and
c.....the circle plane is very different from the original 'corner'
c.....plane - do not accept this fillet
c
c.....An addition to this logic is to test if the circle
c.....is actually touching the two motion lines and not
c.....just between the end of the line and the corner point
c.....If so, then this fillet is valid
c.....Test cases: NCL343.cpp
c.....            TestFlank_NoCornerization_01.cpp
c.....            TestFlank_D4_Filleting.cpp
c.....            TestFlank_D8_Filleting.cpp
c
          if (dist3 .gt. cirbuf(7)*2.5d0) then
            call filseg (wrkpts(1,4),wrkpts(1,3),fpoint,iout1)
            if (iout1 .eq. 1) then
              if (dist4.gt.cirbuf(7)*15.0d0 .or. lcirvc.ne.1) iout2 = 1
              dist3 = f_dot (cirvc0,cirbuf(4))
              if (.not.lv93 .and. dist3.lt.0.9848) iout2 = 1
             endif
          endif

          IF (MULTAX .EQ. 1) THEN
              DIST2      = DSQRT ((FPOINT(1)-WRKPTS(1,3))**2 +
     -                            (FPOINT(2)-WRKPTS(2,3))**2 +
     -                            (FPOINT(3)-WRKPTS(3,3))**2)
              DIST3      = DSQRT ((WRKPTS(1,4)-WRKPTS(1,3))**2 +
     -                            (WRKPTS(2,4)-WRKPTS(2,3))**2 +
     -                            (WRKPTS(3,4)-WRKPTS(3,3))**2)
C
C... CALC RATIO
C
              RATIO(2)  = (DIST2/DIST3)
              if (dabs(ratio(2)).gt.2.0d0) then
                DIST2    = DSQRT ((FPOINT(1)-wkt(1))**2 +
     -                            (FPOINT(2)-wkt(2))**2 +
     -                            (FPOINT(3)-wkt(3))**2)
                DIST3    = DSQRT ((WRKPTS(1,3)-wkt(1))**2 +
     -                            (WRKPTS(2,3)-wkt(2))**2 +
     -                            (WRKPTS(3,3)-wkt(3))**2)
                RATIO(2) = 0.0
                if (dist3.gt.1.d-6) RATIO(2) = DIST2/DIST3
                call pgtlan (wkt(4),WRKPTS(4,3),ALPH1,BET1,ddl,fi,
     -                       thet,tvmp)

              else
                call pgtlan (WRKPTS(4,3),WRKPTS(4,4),ALPH1,BET1,ddl,fi,
     -                       thet,tvmp)
              endif
              ALPH3  = ALPH1 + RATIO(2) * ddl
              call setijk (ALPH3,FPOINT(4),fi,thet)
          ELSE
              FPOINT(4) = 0.
              FPOINT(5) = 0.
              FPOINT(6) = 1.
          ENDIF
c
c...Do the actual comparison
c
          if (DEBUGX .eq. 1) then
              write (dbuf,7002) fpoint(1),fpoint(2),fpoint(3)
 7002         format ('Fpoint = ',f12.4,f12.4,f12.4)
              call ctob (dbuf,dout)
              call nclxostr(dout)
          endif
          call filseg (wrkpts(1,4),wkt,fpoint,iout)
          IF (iout .eq. 0 .and. iout2 .eq. 0) then
c
c..... Dassault MFGNC 305. No-points arc caused to abandon further filleting.
c
            call pchkpts (tangs,tangf,filrad,inopts)
            if (inopts.gt.0 .or. lv93) goto 1150
            lnopts = mxcl1 .gt. 0
          endif
C
C...The fillet final point was after the first motion point in the second
C...motion record, so get the next motion point after that and use it instead.
C...continue working through the motion points until the fillet final point
C...comes before the next motion point.
C
 
1101      IFLESS = 1
          iad2nd = 1
C
C...Set irotpt so that in filchk it will unrotate the points if necessary
C...Increase istrpt(2) so that in filchk, wrkpts will be loaded with
C...the point further along in the motion record.
C
          IF(IROTPT .EQ. 1) IROTPT  = 3
cc          ISTRPT(2) = ISTRPT(2) + 1
cc          if (istrpt(2) .gt. mxcl2) go to 9000
          call filsp2 (istrpt(2),mxcl2,nxrc,0,isfl)
          if (isfl .eq. 2) ksfl = 1
          if (isfl .eq. 0) go to 9000
C
C... SET UP XYZ POINTS FOR CHECKING
C
          CALL FILCHK (NXRC,WRKPTS,IRET,IFLESS,MXTMP,
     -                 MXCL1,MXCL2,FILSAV,NPTS,CIRSTO,
     -                 ICRNBF,npts3)
 
          call ncl_setptr(nxrc, savken)

          MXCLSV = MXCL1
          if (iret .eq. 1 .or. iret .eq. 2) then
              ider = 0
              GO TO 9000
          endif
          GO TO 250
C
C... RESET 1ST RECORD
C
 2800     if (iflg .eq. 1) mxtmp = mxtmp - 1
          iflg  = 0
          MXCL1 = MXTMP
          FILOUT  = 3
          IF (IMLTSV .EQ. 1 .OR. IMLTSV .EQ. 4 .or. iad2nd .eq. 0)
     1            GO TO 9000
          IFLESS = 2
cc          IF (ISTRPT(2) .EQ. MXCL2) GO TO 9000
          call filsp2 (istrpt(2),mxcl2,nxrc,1,isfl)
          if (isfl .eq. 0) go to 9000
          IF (IMLTSV .EQ. 2 .OR.
     -        IMLTSV .EQ. 4) GO TO 3000
C
C...Set irotpt so that in filchk it will unrotate the points if necessary
C...Increase istrpt(2) so that in filchk, wrkpts will be loaded with
C...the point further along in the motion record.
C
          IF(IROTPT .EQ. 1) IROTPT  = 3
          call filsp2 (istrpt(2),mxcl2,nxrc,0,isfl)
          if (isfl .eq. 2) ksfl = 1
cc          ISTRPT(2) = ISTRPT(2) + 1
 
          CALL FILCHK (NXRC,WRKPTS,IRET,IFLESS,MXTMP,
     -                 MXCL1,MXCL2,FILSAV,NPTS,CIRSTO,
     -                 ICRNBF,npts3)
 
          call ncl_setptr(nxrc, savken)
          MXCLSV = MXCL1

          if (iret .eq. 1 .or. iret .eq. 2) then
              ider = 0
              GO TO 9000
          endif
      go to 3000
C
C... IF NO MATCHES GO OUTPUT FILSAV
C
 1150 IF ((IMLTSV .EQ. 2 .OR. IMLTSV .EQ. 4) .AND.
     -    IFLESS .NE. 2 .and. iflg.eq.0) MXCL1  =  MXCL1 -1
      IF (MXCL1 .LT. 1) GO TO 9000
C
C...Are there going to be any points output
C
      call pchkpts (tangs,tangf,filrad,inopts)
      if (inopts.eq.0) then
          ider = 0
          goto 9000
      endif
C
C...Output first point on the circle
C
      filout  = 1
      if (mxtmp .gt. 1) then
          icnt = -npts + 1
          do 1195 j1=1,mxcl1,1
              icnt = icnt + npts
              DO 1190 k1=1,npts,1
                  clpt (k1,j1) = filsav (icnt+k1-1)
 1190         continue
 1195     continue
C
C... ADJUST AND OUTPUT POINTS
C
          SVMXCL = MXCL
          MXCL   = MXCL1
          IISAV  = IROTPT
          IROTPT = 99
          CALL FILADJ
          IROTPT = IISAV
          MXCL   = SVMXCL
      ENDIF
      DO 1300 J=1,6,1
          CLPT(J,1) = SPOINT(J)
 1300 CONTINUE
      MXCL   = 1
      IISAV  = IROTPT
      IF (IROTPT .NE. 99) IROTPT = 0
      CALL FILADJ
      IROTPT = IISAV
C
C...Save the first 6 elements of clpt, these will be used for the 
C...starting point of the new fillet.
C
      do 1310 J=1,6
         pbuf(j)=clpt(j,1)
1310     iclpt(j)=clpt(j,1)
c
c...ARCSLP/FILLET,LOCK
c...Adjust starting point of circle's tool axis
c...to be the same as the rest of the circle
c
      if (lflsam .and. multax .eq. 1) then
          r =  f_dot(iclpt(4),FILAXS)
          if (r .lt. FILANG .and. iret .ne. 3 .and. lflwrn)
     1        ifl(2) = -487
          iclpt(4) = FILAXS(1)
          iclpt(5) = FILAXS(2)
          iclpt(6) = FILAXS(3)
          pbuf(4) = FILAXS(1)
          pbuf(5) = FILAXS(2)
          pbuf(6) = FILAXS(3)
      endif
C
C...fillin will determine the number of fillet points
C...needed and put them in the clpt array.
C
 1900 call fillin (tangs,tangf,filrad,cirbuf,spoint,fpoint,
     -             wrkpts)
      ifl(348) = 1
c
c...Get rid of any continuation points appended to record
c
      iclwsv(5) = iclwsv(5) - iadj
C
C...If iptcnt is greater than 0 then not all of the points
C...in the original motion record were used, so decrease the number
C...in iclwsv(5) accordingly.  This can occur when the original record
C...was a multiple point record and the start of the fillet came before
C...the second to last point in the motion record.
C
      if (iptcnt.gt.0) then
         iclwsv(5) = iclwsv(5)-(iptcnt*npts2)
      endif
      if (iclwsv(5).le.0) iclwsv(5) = npts2
      ix=0
      is=iclwsv(5)-(npts2-1)
      do 1320 i= is, (is+npts-1)  
         ix = ix +1
1320     dclall(i)=iclpt(ix)

C
C...If there is a feed rate, set iclw2 accordingly, if not
C...set iclw2(3) to zero so that a feed rate record won't
C...be inserted into the list in clstor_insert. 
C
      if (fedout.ne.0) then
         iclw2(1) = 0
         iclw2(2) = 0
         iclw2(3) = 2000
         iclw2(4) = 1009
         iclw2(5) = 1
         dcll2(1) = fedout
      else
         iclw2(3) = 0
      endif
C
C...If icrout is not equal to 1, we can just output the newly created
C...points to the clfile, so set iclw3(3) to 0 so that it will be skipped
C...over in clstor_insert, lcount is the line number of the ARCSLP/FILLET command
C...in the part program file. If icrout is equal to 1 then we need to output
C...the circular record, the format for this is:
C...PSIS/ (PLANE/ i,j,k,d)
C...INDIRP/ x,y,z
C...TLON,GOFWD/(CIRCLE/CANON, x,y,z,i,j,k,r),on or tt,(PLANE/i,j,k,d)
C
      if (icrout.eq.1) then
         lcount = 0
         iclw3(1) = lcount
         iclw3(2) = lcount
         iclw3(3) = 3000
         iclw3(4) = 2
         iclw3(5) = 12
         dcll3(1) = 5
         do 1330 i = 2,5
1330        dcll3(i) = 0
c
c...Rotate circle record back to original plane.
c
         j1 = IROTPT
         k1 = MULTAX
         IROTPT = 0
         MULTAX = 1
         call rotpts(CIRBUF,ierr)
         IROTPT = j1
         MULTAX = k1
         if (ierr .eq. 1) goto 9200
         do 1340 i = 1,7
1340        dcll3(i+5) = cirbuf(i)
c
c......If MULTAX is on then
c......make sure circle vector matches tool axis vector
c
         if (MULTAX .eq. 1) then
             do 1343 i=1,inopts,1
                 r = f_dot(clpt(4,i),cirbuf(4))
                 if (1.0d0-dabs(r) .gt. .0001d0) iclw3(3) = 0
 1343        continue
         endif
      else
         iclw3(3) = 0
      endif
      iclw4(1) = lcount
      iclw4(2) = lcount
      iclw4(3) = 5000
      if (ifl(42) .eq. 1) iclw4(3) = 5100
      iclw4(4) = 5
      iclw4(5) = inopts*npts
C
C...If multax is off, we need to get rid of the tool axis portion of
C...clpt
c
      if (multax.ne.1) then
         ix = 0
         do 1345 i =1,inopts
            dcll4(1+(ix*3)) = clpt(1,i)
            dcll4(2+(ix*3)) = clpt(2,i)
            dcll4(3+(ix*3)) = clpt(3,i)
            ix = ix +1
1345     continue
      else
C
C...Multax is on, so keep the tool axis  portion of clpt
C
         ix = 0
         do 1347 i =1,inopts
            dcll4(1+(ix*6)) = clpt(1,i)
            dcll4(2+(ix*6)) = clpt(2,i)
            dcll4(3+(ix*6)) = clpt(3,i)
            dcll4(4+(ix*6)) = clpt(4,i)
            dcll4(5+(ix*6)) = clpt(5,i)
            dcll4(6+(ix*6)) = clpt(6,i)
            ix = ix +1
1347     continue
      endif
c
c...Save the last point of the fillet
c
      do 1351 i=1,6,1
          iclpt(i) = clpt(i,inopts)
 1351 continue
c      nxrc = savken
      call ncl_setptr(savken,nxrc)
C
C...If fedout is not equal to 0 set iclw5, otherwise
C...set iclw5(3) to 0 so that in clstor_insert, a feedrate
C...record will not be inserted.
C 
      if (fedout.ne.0) then
         iclw5(1) = 0
         iclw5(2) = 0
         iclw5(3) = 2000
         iclw5(4) = 1009
         iclw5(5) = 1
         dcll5(1) = sc(123)
      else
         iclw5(3) =0
      endif
c
c...Get past any non motion records.
c
      iclw6(3) = 0
      do while (.not. ismrec(iclw6))
        call clread (iclf,savken,iclw6,dcll6,jerr)
        if (jerr.eq.1) goto 9000
      enddo
c
c...Make adjustment for ARCSLP/FILLET,COMBIN
c
      if (FILFLG(4) .eq. 1) istrpt(2) = istrpt(2) - IFADJ2
C
C...Remove points at the start of the second record which
C...are before the end point of the fillet.
C 
      if (iclw6(5).gt.npts3) then
         j = (ISTRPT(2)-1)*npts3
         if (j.gt.0) then
           iclw6(5) = iclw6(5)-j
c
c......Check for tool axis change too large
c......when fillet is forced to have fixed tool axis
c
           if (lflwrn .and. lflsam .and. multax .eq. 1) then
             r =  f_dot(dcll6(iclw6(5)-2),FILAXS)
             if (r .lt. FILANG .and. iret .ne. 3) ifl(2) = -487
           endif
c
c......Transfer points
c
           do i=1,iclw6(5)
             dcll6(i) = dcll6(i+j)
           enddo
         endif
      endif
      lcount   = iclw6(1)
      iclw2(1) = lcount
      iclw2(2) = lcount
      iclw3(1) = lcount
      iclw3(2) = lcount
      iclw4(1) = lcount
      iclw4(2) = lcount
      i =0
C
C...Call clstor_insert to save the created fillet and to make any
C...necessary changes to the already saved records. 
C
      call clstor_insert(i,savkst,iclwsv,dclall,iclw2,dcll2,
     x                   iclw3,dcll3,iclw4,dcll4,iclw5,dcll5,
     x                   iclw6,dcll6,nxrc)
      call ncl_setptr(savkst, kst)
      call ncl_setptr(nxrc, ken)
      call ncl_setptr(ken, savken)
      call ncl_tstptr(savken, itst)
      if (itst.eq.1) call clread (iclf,savken,iclwtp,dcltmp,jerr)
1369  continue
      ix = iclw6(5)/npts3
C
C...Need to check to see if the next record is a continuation of this
C...one, if so, we need to finish ploting the motion. If there
C...is a continuation record, then we also want to save the record number
C...so it is returned to driver and can be used if there are more fillets
C...to be made. JLS 7/1/99
C
      call ncl_tstptr(savken,itst)
      if (jerr.ne.1 .and. itst .ne. 0) then
1390   call ncl_setptr(savken, irectp)
       call clread (iclf,savken,iclwtp,dcltmp,jerr)
       if (jerr.eq.1) goto 9000
       if (iclwtp(1).eq.iclw6(1)) then
         if (.not. ismrec(iclwtp)) then
           call ncl_setptr(irectp, ken)
           goto 1390
         endif
         if (iclwtp(4).eq.6) then
           do 1381 i=1,5
1381          iclw6(i) = iclwtp(i)
           do 1382 i = 1,640
1382          dcll6(i) = dcltmp(i)
           call setnpt(iclw6,npts3)
           call ncl_setptr(irectp, ken)
           goto 1369
         endif
       endif
      endif
C
C...Save into oldpts the last point in the fillet.
C...if there are multiple fillet, when we return to this
C...routine we will already have oldpts and not have to
C...get them from a record.
C
      if (inopts.gt.0) then
        call conv8_8(clpt(1,inopts),oldpts,npts)
      else
        ix = iclw(5)-npts+1
        if (ix.lt.1) ix = 1
        call conv8_8(dclbuf(ix),oldpts,npts)
      endif
 9000 CONTINUE
      if (FILFLG(10) .eq. 0) then
          IF (FILOUT .EQ. 1) THEN
              IF (IMLTSV .EQ. 2 .OR. IMLTSV .EQ. 3) THEN
                  ISTRPT(1) = ISTRPT(2)
              ENDIF
              RETURN
          endif
          sc(171) = sc171
          sc(172) = sc172
          sc(173) = sc173
          ifl(348) = 0
C
C...Reset records if no fillet is output.
C
          if (ider.eq.1 .and. iret.ne.3 .and. lflwrn .and. .not.lnopts)
     x      ifl(2) = -474
          istrpt(1) = 1
          istrpt(2) = 1
 
          MXCL1 = MXTMP
          ICNT = -NPTS + 1
C
          DO 9120 J1=1,MXCL1,1
              ICNT = ICNT + NPTS
              DO 9110 K1=1,NPTS,1
                  CLPT (K1,J1) = FILSAV (ICNT+K1-1)
 9110         CONTINUE
 9120     CONTINUE
C
          MXCL   = MXCL1
          kerr = ifl(2)
      else
          ifl(348) = 0
      endif
      FILOUT = 0
      RETURN
C
C... IF END OF PART PROGRAM
C... AND OUTPUT TO MOTOUT
C
9200  icnt = -npts + 1
      do 9220 j1=1,mxcl1,1
          icnt = icnt + npts
          do 9210 k1=1,npts,1
              clpt (k1,j1) = filsav (icnt+k1-1)
9210      continue
9220  continue
 
      mxcl   = mxcl1
      filout   = 0
 
      if (lflwrn) kerr = -474
      return
 
      end
c
c***********************************************************************
c
c  SUBROUTINE:  cmplen (point1,point2,point3,iokay)
c
c  FUNCTION:  Computes the distance betwwen two different points to a
c             third point and returns which point is closest.
c
c     INPUT:  point1  R*8  D3  -  First point for distance checking.
c
c             point2  R*8  D3  -  Second point for distance checking.
c
c             point3  R*8  D3  -  Point which is used for distance checking
c                                 against point1 and point2.
c
c     OUTPUT: iokay   I*2  D1  -  Returns 1 if point1 is closest to point3,
c                                 otherwise returns 0.
c
c***********************************************************************
c
      subroutine cmplen(point1,point2,point3,iokay)

      real*8 point1(3), point2(3), point3(3)
      integer*2 iokay

      real*8 x1,y1,z1,length1,x2,y2,z2,length2

      iokay = 0
      x1 = point1(1) - point3(1)
      y1 = point1(2) - point3(2)
      z1 = point1(3) - point3(3)
      length1 = sqrt((x1*x1) + (y1*y1) + (z1*z1))
      
      x2 = point2(1) - point3(1)
      y2 = point2(2) - point3(2)
      z2 = point2(3) - point3(3)
      length2 = sqrt((x2*x2) + (y2*y2) + (z2*z2))

      if (length1.lt.length2) iokay = 1

      return

      end
c
c***********************************************************************
c
c  SUBROUTINE:  chkdir (point1,point2,point3,iokay)
c
c  FUNCTION:  Checks if a point lies in front of another point in the
c             direction of a third point.
c
c     INPUT:  point1  R*8  D3  -  First point.
c
c             point2  R*8  D3  -  Second point.
c
c             point3  R*8  D3  -  Point which is used for reference.
c
c     OUTPUT: idel    I*2  D1  -  Returns 1 if p1 is in front of p2 with
c                                 reference to p3.
c
c***********************************************************************
c
      subroutine chkdir(p1,p2,p3,idel)
      
      include 'com.com'

      real*8 p1(3),p2(3),p3(3)
      integer*2 idel

      real*8 v1(3), d1, d2, f_dot

      idel = 0
      call vcmnvc(p3,p2,v1)
      call unitvc(v1,v1)
      d1 = f_dot(p1,v1)
      d2 = f_dot(p2,v1)
      if (d1.ge.d2) idel = 1
      return
      end
c
c***********************************************************************
c
c  SUBROUTINE:  gtfilrad (frad,lwarn,lcomb,lsame)
c
c  FUNCTION:  Returns the current ARCSLP/FILLET radius.
c             direction of a third point.
c
c     INPUT:  none
c
c
c     OUTPUT: frad    R*8  D1  -  The current ARCSLP/FILLET radius.
c
c             lwarn   L*2  D1  -  The current ARCSLP/FILLET,WARN setting.
c
c             lcomb   L*2  D1  -  The current ARCSLP/FILLET,COMBIN setting.
c
c             lsame   L*2  D1  -  The current ARCSLP/FILLET,LOCK setting.
c
c***********************************************************************
c
      subroutine gtfilrad(frad,lwarn,lcomb,lsame)
      
      include 'com8a.com'
      include 'fillet.com'
c
      logical*2 lwarn,lcomb,lsame
c
      real*8 frad

      frad = filrad
      lwarn = lflwrn
      lcomb = lflcom
      lsame = lflsam

      return
      end
c
c***********************************************************************
c
c  SUBROUTINE:  pfilpt (krec,krec1,krec2,krec3)
c
c  FUNCTION:  Stores the current clfile pointer in the correct fillet
c             clfile pointer.
c
c     INPUT:  krec    I*4  D1  -  Current clfile record.
c
c     OUTPUT: krec1   I*4  D1  -  Clfile record coming from.
c
c             krec2   I*4  D1  -  Clfile record currently at.
c
c             krec3   I*4  D1  -  Clfile record going to.
c
c***********************************************************************
c
      subroutine pfilpt (krec,krec1,krec2,krec3)
c
      include 'com8a.com'
c
      integer*4 krec(2),krec1(2),krec2(2),krec3(2)
      integer*2 iflg1, iflg2
c
c...Save the appropriate record number
c...used to make fillets
c
      if (ifl(42) .eq. 0 .or. ifl(372) .eq. 1) then
          call ncl_tstptr(krec1, iflg1)
          call ncl_tstptr(krec2, iflg2)
          if (ifl(347) .eq. 1) then
              if (iflg1 .eq. 0) then
                  call ncl_setptr(krec, krec1)
              else
                  call ncl_setptr(krec, krec2)
              endif
          else if (ifl(347) .eq. 2) then
              if (iflg1 .eq. 0) then
                  call ncl_setptr(krec, krec1)
                  ifl(347) = 1
              else if (iflg2 .eq. 0) then
                  call ncl_setptr(krec, krec2)
                  ifl(347) = 1
              else
                  call ncl_setptr(krec, krec3)
              endif
          else if ((isc10(1) .ge. 701 .and. isc10(1) .le. 710) .or.
     1             isc10(1) .eq. 803) then
              call ncl_setptr(krec, krec1)
          endif
      endif
c
c...End of routine
c
 8000 return
      end
